
# X_Label ordering.R ####

# Packages ####

library(tidyverse)

# Example input ####
# Data frame must contain:
# Id, Lon, Lat
# You can join the returned Side / LabelX / LabelY columns back onto your map data.

# Objective function ####

CountSegmentCrossings = function(SegmentsDf) {
  CrossCount = 0L
  
  if(nrow(SegmentsDf) < 2) {
    return(CrossCount)
  }
  
  for(I in 1:(nrow(SegmentsDf) - 1)) {
    for(J in (I + 1):nrow(SegmentsDf)) {
      X1A = SegmentsDf$XPoint[I]
      Y1A = SegmentsDf$YPoint[I]
      X1B = SegmentsDf$XLabel[I]
      Y1B = SegmentsDf$YLabel[I]
      
      X2A = SegmentsDf$XPoint[J]
      Y2A = SegmentsDf$YPoint[J]
      X2B = SegmentsDf$XLabel[J]
      Y2B = SegmentsDf$YLabel[J]
      
      Orient = function(XA, YA, XB, YB, XC, YC) {
        (XB - XA) * (YC - YA) - (YB - YA) * (XC - XA)
      }
      
      O1 = Orient(X1A, Y1A, X1B, Y1B, X2A, Y2A)
      O2 = Orient(X1A, Y1A, X1B, Y1B, X2B, Y2B)
      O3 = Orient(X2A, Y2A, X2B, Y2B, X1A, Y1A)
      O4 = Orient(X2A, Y2A, X2B, Y2B, X1B, Y1B)
      
      Intersects = (O1 * O2 < 0) & (O3 * O4 < 0)
      
      if(Intersects) {
        CrossCount = CrossCount + 1L
      }
    }
  }
  
  CrossCount
}

BuildLayout = function(PointsDf, SideAssignment, XPaddingFrac = 0.05, YPaddingFrac = 0.12) {
  StopIfNot = c("Id", "Lon", "Lat")
  
  if(!all(StopIfNot %in% names(PointsDf))) {
    stop("PointsDf must contain columns: Id, Lon, Lat")
  }
  
  LayoutDf = PointsDf %>%
    mutate(Side = SideAssignment)
  
  XRange = range(LayoutDf$Lon, na.rm = TRUE)
  YRange = range(LayoutDf$Lat, na.rm = TRUE)
  
  XPad = diff(XRange) * XPaddingFrac
  YPad = diff(YRange) * YPaddingFrac
  
  XMin = XRange[1] - XPad
  XMax = XRange[2] + XPad
  TopY = YRange[2] + YPad
  BottomY = YRange[1] - YPad
  
  TopDf = LayoutDf %>%
    filter(Side == "top") %>%
    arrange(Lon) %>%
    mutate(
      LabelX = if(n() == 1) mean(c(XMin, XMax)) else seq(XMin, XMax, length.out = n()),
      LabelY = TopY
    )
  
  BottomDf = LayoutDf %>%
    filter(Side == "bottom") %>%
    arrange(Lon) %>%
    mutate(
      LabelX = if(n() == 1) mean(c(XMin, XMax)) else seq(XMin, XMax, length.out = n()),
      LabelY = BottomY
    )
  
  bind_rows(TopDf, BottomDf) %>%
    arrange(match(Id, PointsDf$Id))
}

ScoreLayout = function(LayoutDf,
                       CrossingWeight = 10000,
                       LengthWeight = 1,
                       ImbalanceWeight = 200,
                       TopPenaltyWeight = 0) {
  SegmentsDf = LayoutDf %>%
    transmute(
      Id = Id,
      XPoint = Lon,
      YPoint = Lat,
      XLabel = LabelX,
      YLabel = LabelY
    )
  
  CrossingCount = CountSegmentCrossings(SegmentsDf)
  
  TotalLength = LayoutDf %>%
    mutate(
      ConnectorLength = sqrt((Lon - LabelX)^2 + (Lat - LabelY)^2)
    ) %>%
    summarise(Total = sum(ConnectorLength)) %>%
    pull(Total)
  
  NTop = sum(LayoutDf$Side == "top")
  NBottom = sum(LayoutDf$Side == "bottom")
  Imbalance = abs(NTop - NBottom)
  
  TopPenalty = sum(LayoutDf$Side == "top")
  
  Score =
    CrossingWeight * CrossingCount +
    LengthWeight * TotalLength +
    ImbalanceWeight * Imbalance +
    TopPenaltyWeight * TopPenalty
  
  tibble(
    Score = Score,
    CrossingCount = CrossingCount,
    TotalLength = TotalLength,
    Imbalance = Imbalance,
    NTop = NTop,
    NBottom = NBottom
  )
}

# Initial solution ####

BuildInitialSideAssignment = function(PointsDf) {
  PointsDf %>%
    arrange(Lon) %>%
    mutate(
      Side = rep(c("top", "bottom"), length.out = n())
    ) %>%
    arrange(match(Id, PointsDf$Id)) %>%
    pull(Side)
}

# Local search optimiser ####

OptimiseLabelSides = function(PointsDf,
                              MaxIter = 5000,
                              XPaddingFrac = 0.05,
                              YPaddingFrac = 0.12,
                              CrossingWeight = 10000,
                              LengthWeight = 1,
                              ImbalanceWeight = 200,
                              TopPenaltyWeight = 0,
                              Verbose = TRUE) {
  CurrentSides = BuildInitialSideAssignment(PointsDf)
  
  CurrentLayout = BuildLayout(
    PointsDf = PointsDf,
    SideAssignment = CurrentSides,
    XPaddingFrac = XPaddingFrac,
    YPaddingFrac = YPaddingFrac
  )
  
  CurrentScore = ScoreLayout(
    LayoutDf = CurrentLayout,
    CrossingWeight = CrossingWeight,
    LengthWeight = LengthWeight,
    ImbalanceWeight = ImbalanceWeight,
    TopPenaltyWeight = TopPenaltyWeight
  )
  
  BestSides = CurrentSides
  BestLayout = CurrentLayout
  BestScore = CurrentScore
  
  if(Verbose) {
    message(glue::glue(
      "Initial score = {round(BestScore$Score, 2)}, crossings = {BestScore$CrossingCount}, length = {round(BestScore$TotalLength, 2)}"
    ))
  }
  
  Improved = TRUE
  Iter = 0
  
  while(Improved && Iter < MaxIter) {
    Improved = FALSE
    Iter = Iter + 1
    
    for(I in seq_len(nrow(PointsDf))) {
      TrialSides = BestSides
      TrialSides[I] = ifelse(TrialSides[I] == "top", "bottom", "top")
      
      TrialLayout = BuildLayout(
        PointsDf = PointsDf,
        SideAssignment = TrialSides,
        XPaddingFrac = XPaddingFrac,
        YPaddingFrac = YPaddingFrac
      )
      
      TrialScore = ScoreLayout(
        LayoutDf = TrialLayout,
        CrossingWeight = CrossingWeight,
        LengthWeight = LengthWeight,
        ImbalanceWeight = ImbalanceWeight,
        TopPenaltyWeight = TopPenaltyWeight
      )
      
      if(TrialScore$Score < BestScore$Score) {
        BestSides = TrialSides
        BestLayout = TrialLayout
        BestScore = TrialScore
        Improved = TRUE
        
        if(Verbose) {
          message(glue::glue(
            "Iter {Iter}, flip {I}: score = {round(BestScore$Score, 2)}, crossings = {BestScore$CrossingCount}, length = {round(BestScore$TotalLength, 2)}"
          ))
        }
      }
    }
    
    for(I in 1:(nrow(PointsDf) - 1)) {
      for(J in (I + 1):nrow(PointsDf)) {
        TrialSides = BestSides
        Temp = TrialSides[I]
        TrialSides[I] = TrialSides[J]
        TrialSides[J] = Temp
        
        TrialLayout = BuildLayout(
          PointsDf = PointsDf,
          SideAssignment = TrialSides,
          XPaddingFrac = XPaddingFrac,
          YPaddingFrac = YPaddingFrac
        )
        
        TrialScore = ScoreLayout(
          LayoutDf = TrialLayout,
          CrossingWeight = CrossingWeight,
          LengthWeight = LengthWeight,
          ImbalanceWeight = ImbalanceWeight,
          TopPenaltyWeight = TopPenaltyWeight
        )
        
        if(TrialScore$Score < BestScore$Score) {
          BestSides = TrialSides
          BestLayout = TrialLayout
          BestScore = TrialScore
          Improved = TRUE
          
          if(Verbose) {
            message(glue::glue(
              "Iter {Iter}, swap {I}/{J}: score = {round(BestScore$Score, 2)}, crossings = {BestScore$CrossingCount}, length = {round(BestScore$TotalLength, 2)}"
            ))
          }
        }
      }
    }
  }
  
  list(
    LayoutDf = BestLayout,
    ScoreDf = BestScore,
    SideAssignment = BestSides
  )
}

# Example use ####

# PointsDf =
#   MapDf %>%
#   transmute(
#     Id = DisplayLabel,
#     Lon = Lon,
#     Lat = Lat
#   )

# Fit =
#   OptimiseLabelSides(
#     PointsDf = PointsDf,
#     MaxIter = 100,
#     CrossingWeight = 10000,
#     LengthWeight = 1,
#     ImbalanceWeight = 200,
#     Verbose = TRUE
#   )

# Fit$ScoreDf
# LabelDf = Fit$LayoutDf

# Suggested plotting data ####

# LabelDf will contain:
# Id, Lon, Lat, Side, LabelX, LabelY
#
# You can then use:
# - geom_path() or geom_segment() from (Lon, Lat) to (LabelX, LabelY)
# - geom_text() at (LabelX, LabelY)
