
# X_Collaborator map ####

rm(list = ls())

source("R/X_Label ordering.R")

library(tidyverse)
library(gsheet)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(cowplot)
library(colorspace)
library(ggregplot)
library(magrittr)
library(fs)

theme_set(
  theme_cowplot() +
    theme(strip.background = element_rect(fill = "white"))
)

dir_create("Figures")

if(!file.exists("Data/Systems.csv")){
  
  # Authenticate if the sheet is private.
  # If it is public, you can skip this.
  # gs4_auth(email = "your_email@domain.com")
  
  # Replace with your Google Sheet URL or sheet ID.
  SheetUrl <- "https://docs.google.com/spreadsheets/d/1NGtxJm5w2TEzAYptzvwhjZbLuTOGN6va4RUJZw4vCRQ/edit?usp=sharing"
  
  # If needed, replace `sheet = 1` with the tab name or index.
  BaseDf <-
    gsheet2tbl(SheetUrl, sheet = 1) %>%
    as_tibble()
  
  # Expecting columns named exactly Longitude and Latitude.
  # Optional: also keep Site / System / Species columns if they already exist.
  # Rename here only if your sheet uses slightly different names.
  BaseDf <-
    BaseDf %>%
    rename(
      Lon = Longitude,
      Lat = Latitude
    )
  
  # Euromammals reconstruction ----------------------------------------------
  
  EuromammalsDf <- tibble(
    Site = c(
      "Andalusia", "Central Spain", "Catalonia", "Alentejo",
      "Pyrenees", "Central France", "Alsace", "Ardennes", "Veluwe",
      "Switzerland", "Austria Alps", "Bavaria", "Central Germany",
      "Czech Republic", "Bialowieza", "Slovakia", "Hungary",
      "Denmark", "South Sweden", "Central Sweden", "North Sweden",
      "South Norway", "Central Norway", "South Finland", "North Finland",
      "Romania Carpathians", "Slovenia", "Croatia"
    ),
    Latitude = c(
      37.4, 39.9, 41.8, 38.0,
      43.0, 46.5, 48.3, 50.2, 52.1,
      46.8, 47.5, 48.5, 51.0,
      49.8, 52.7, 48.7, 47.2,
      56.0, 56.5, 60.0, 64.5,
      60.5, 63.5, 61.5, 67.5,
      45.5, 46.1, 45.2
    ),
    Longitude = c(
      -5.9, -4.0, 1.5, -7.9,
      -0.5, 2.5, 7.5, 5.7, 5.8,
      8.3, 13.0, 11.5, 10.0,
      15.5, 23.8, 19.7, 19.5,
      10.0, 13.5, 15.0, 19.0,
      9.5, 11.0, 25.0, 26.0,
      25.0, 14.9, 16.0
    ),
    Species = c(
      "Cervus elaphus", "Cervus elaphus", "Sus scrofa", "Sus scrofa",
      "Capreolus capreolus", "Capreolus capreolus", "Capreolus capreolus", "Capreolus capreolus", "Capreolus capreolus",
      "Capreolus capreolus", "Capreolus capreolus", "Capreolus capreolus", "Capreolus capreolus",
      "Lynx lynx", "Capreolus capreolus", "Cervus elaphus", "Sus scrofa",
      "Capreolus capreolus", "Alces alces", "Alces alces", "Alces alces",
      "Alces alces", "Alces alces", "Alces alces", "Alces alces",
      "Cervus elaphus", "Lynx lynx", "Ursus arctos"
    ),
    NStudies = c(
      2, 2, 2, 1,
      4, 5, 4, 3, 4,
      5, 5, 6, 5,
      3, 6, 3, 3,
      2, 4, 4, 3,
      3, 2, 3, 2,
      3, 3, 2
    )
  ) %>%
    mutate(
      System = paste0("Euromammals: ", Site),
      Source = "Euromammals",
      Lon = Longitude,
      Lat = Latitude
    ) %>% 
    mutate_at("Site", ~paste0("EM: ", .x))
  
  # Standardise Google Sheet data --------------------------------------
  
  # This tries to preserve your existing columns and only adds missing ones.
  # Adjust the fallbacks if the sheet uses different names.
  BaseDfStd <-
    BaseDf %>%
    mutate(
      # System = coalesce(.data[["System"]], .data[["Site"]], .data[["Name"]], "Unnamed system"),
      # Site = coalesce(.data[["Site"]], .data[["System"]], .data[["Name"]], "Unnamed site"),
      Species = coalesce(.data[["Species"]], NA_character_),
      # NStudies = coalesce(suppressWarnings(as.numeric(.data[["NStudies"]])), 1),
      Source = "Google Sheet"
    )
  
  # Combine -----------------------------------------------------------------
  
  MapDf <-
    bind_rows(
      BaseDfStd %>%
        mutate(
          Longitude = Lon,
          Latitude = Lat
        ) %>%
        select(any_of(c("System", "Site", "Species", "NStudies", "Source", "Longitude", "Latitude", "Lon", "Lat"))),
      EuromammalsDf %>%
        select(any_of(c("System", "Site", "Species", "NStudies", "Source", "Longitude", "Latitude", "Lon", "Lat")))
    ) %>%
    mutate(
      DisplayLabel = case_when(
        Source == "Euromammals" ~ Site,
        TRUE ~ System
      )
    ) %>%
    distinct()
  
  MapDf %>% write.csv("Data/Systems.csv")
  
}else{
  
  MapDf <- read.csv("Data/Systems.csv")
  
}

MapDf %<>% filter(!(is.na(Lat)|is.na(System))) %>% 
  filter(!is.na(Species))

# Species colours ####

SpeciesLevels <- 
  MapDf %>% 
  pull(Species) %>% 
  unique() %>% 
  sort()

SpeciesPalette <- 
  colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(length(SpeciesLevels)) %>% 
  setNames(SpeciesLevels)

MapDf <- 
  MapDf %>% 
  mutate(
    Colour = SpeciesPalette[Species]
  )

# World basemap -----------------------------------------------------------

World <-
  ne_countries(scale = "medium", returnclass = "sf")

# Build side-label positions in the same visual language ------------------

MapDfOrdered <-
  MapDf %>%
  arrange(Lon, desc(Source)) %>%
  mutate(
    RowId = row_number(),
    Side = rep(c("top", "bottom"), length.out = n()),
    LongSec = seq(-175, 175, length.out = n()),
    LatSec = if_else(Side == "top", 92, -62),
    LabelY = if_else(Side == "top", 112, -82)
  )

ConnectorDf <-
  MapDfOrdered %>%
  transmute(
    DisplayLabel,
    x = Lon,
    y = Lat,
    xend = LongSec,
    yend = LatSec,
    Source,
    Species,
    NStudies
  )

# Setting extents etc --------------------------------------------------------------------

MapDf =
  MapDf %>%
  arrange(Lon, desc(Source)) %>%
  mutate(
    RowId = row_number(),
    Side = rep(c("top", "bottom"), length.out = n())
  )

AllExtentDF <- ExtentGet(MapDf, "Lon", "Lat")

XRange = range(MapDfOrdered$Lon, na.rm = TRUE)
YRange = range(MapDfOrdered$Lat, na.rm = TRUE)

XPad = diff(XRange) * 0.1
YPad = diff(YRange) * 0.2

LabelXSeq =
  seq(
    from = XRange[1] - 0.02 * diff(XRange),
    to = XRange[2] + 0.02 * diff(XRange),
    length.out = nrow(MapDf)
  )

TopLabelY = YRange[2] + YPad * 0.25
BottomLabelY = YRange[1] - YPad * 0.25

TopAnchorY = YRange[2] + YPad * 0.15
BottomAnchorY = YRange[1] - YPad * 0.15

MapDf =
  MapDf %>%
  mutate(
    LongSec = LabelXSeq,
    LatSec = ifelse(Side == "top", TopAnchorY, BottomAnchorY),
    LabelY = ifelse(Side == "top", TopLabelY, BottomLabelY),
    HJust = ifelse(Side == "top", 0, 1)
  )

LongSummaryTable =
  MapDf %>%
  select(DisplayLabel, Species, Source, NStudies, Lon, Lat, LongSec, LatSec) %>%
  pivot_longer(
    cols = c(Lon, LongSec),
    names_to = "Begin",
    values_to = "X"
  ) %>%
  mutate(Begin = c("Begin", "End")[match(Begin, c("Lon", "LongSec"))]) %>%
  select(-Lat) %>%
  bind_cols(
    MapDf %>%
      select(Lat, LatSec) %>%
      pivot_longer(
        cols = c(Lat, LatSec),
        names_to = "End",
        values_to = "Y"
      ) %>%
      select(Y)
  )

World =
  ne_countries(scale = "medium", returnclass = "sf")

XLimits = c(XRange[1] - XPad*2, XRange[2] + XPad*2)
XCrop = c(XRange[1] - XPad, XRange[2] + XPad)

YLimits = c(YRange[1] - YPad*2, YRange[2] + YPad*2)
YCrop = c(YRange[1] - YPad, YRange[2] + YPad)

WorldCrop =
  st_crop(
    World,
    xmin = XCrop[1],
    xmax = XCrop[2],
    ymin = YCrop[1],
    ymax = YCrop[2]
  )

# PhyloPic lookup helpers ####

GetUuid = function(TaxonName, Nth = 1) {
  Uuid =
    tryCatch(
      {
        Res = rphylopic::get_uuid(name = TaxonName)
        if(length(Res) == 0) {
          return(NA_character_)
        }
        as.character(Res[Nth])
      },
      error = function(E) {
        NA_character_
      }
    )
  return(Uuid)
}

# Use robust higher taxa where species lookups are unreliable.
# Adjust any of these if you prefer a different silhouette.
PhyloTaxonLookup = c(
  "Alces alces" = "Alces",
  "Capreolus capreolus" = "Capreolus",
  "Cervus elaphus" = "Cervus",
  "Equus ferus caballus" = "Equus",
  "Lynx lynx" = "Lynx",
  "Rangifer tarandus" = "Rangifer",
  "Sus scrofa" = "Sus",
  "Ovis aries" = "Ovis",
  "Dama dama" = "Dama",
  "Ursus arctos" = "Ursidae"
)

PhyloTaxonLookup <- names(PhyloTaxonLookup)
names(PhyloTaxonLookup) <- PhyloTaxonLookup

NthLookup = c(
  "Alces" = 1,
  "Capreolus" = 1,
  "Cervus" = 1,
  "Equus" = 1,
  "Lynx" = 1,
  "Rangifer" = 1,
  "Sus" = 1,
  "Ovis" = 1,
  "Dama" = 1,
  "Ursidae" = 1
)

names(NthLookup) <- PhyloTaxonLookup

MapDf <-
  MapDf %>%
  dplyr::select(Species) %>% unique %>% 
  mutate(
    PhyloTaxon = unname(PhyloTaxonLookup[Species]),
    Uuid = map2_chr(
      PhyloTaxon,
      PhyloTaxon,
      ~{
        if(is.na(.x)) {
          return(NA_character_)
        }
        GetUuid(.x, Nth = NthLookup[[.y]])
      }
    ),
    IconWidth = case_when(
      Species == "Capreolus capreolus" ~ diff(XLimits) * 0.018,
      Species == "Dama dama" ~ diff(XLimits) * 0.022,
      Species == "Ovis aries" ~ diff(XLimits) * 0.022,
      Species == "Cervus elaphus" ~ diff(XLimits) * 0.022,
      Species == "Alces alces" ~ diff(XLimits) * 0.025,
      Species == "Rangifer tarandus" ~ diff(XLimits) * 0.022,
      Species == "Sus scrofa" ~ diff(XLimits) * 0.020,
      Species == "Equus ferus caballus" ~ diff(XLimits) * 0.022,
      Species == "Lynx lynx" ~ diff(XLimits) * 0.018,
      Species == "Ursus arctos" ~ diff(XLimits) * 0.022,
      TRUE ~ diff(XLimits) * 0.020
    )
  ) %>% 
  left_join(MapDf, ., by = "Species")

MapDf$Uuid

# Trying a new label order ####

PointsDf =
  MapDf %>%
  transmute(
    Id = DisplayLabel,
    Lon = Lon,
    Lat = Lat
  )

Fit =
  OptimiseLabelSides(
    PointsDf = PointsDf,
    MaxIter = 100,
    CrossingWeight = 10000,
    LengthWeight = 1,
    ImbalanceWeight = 200,
    Verbose = TRUE
  )

Fit$ScoreDf
LabelDf = Fit$LayoutDf

# Label layout join ####

LabelDf =
  Fit$LayoutDf %>%
  left_join(
    MapDf %>%
      dplyr::select(DisplayLabel, Species, Uuid, IconWidth),
    by = c("Id" = "DisplayLabel")
  ) %>%
  mutate(
    HJust = ifelse(Side == "top", 0, 1)
  )

LabelDf %<>%
  mutate(
    # XOffset = ifelse(Side == "top", 1, -1) * diff(XLimits) * 0.02,
    YOffset = diff(YLimits) * 0.01
  ) %>% 
  mutate_at("YOffset", ~ifelse(Side == "top", .x, -.x))

# Connector data ####

LongSummaryTable =
  bind_rows(
    LabelDf %>%
      transmute(
        DisplayLabel = Id,
        X = Lon,
        Y = Lat,
        Order = 1
      ),
    LabelDf %>%
      transmute(
        DisplayLabel = Id,
        X = LabelX,
        Y = LabelY,
        Order = 2
      )
  ) %>%
  arrange(DisplayLabel, Order)


# Base map ####

(P <-
   ggplot() +
   geom_sf(
     data = World,
     inherit.aes = FALSE,
     fill = "grey85",
     colour = "grey70",
     linewidth = 0.25
   ) +
   geom_path(
     data = LongSummaryTable,
     aes(x = X, y = Y, group = DisplayLabel),
     colour = "grey45",
     linewidth = 0.25,
     lineend = "round"
   ) +
   geom_text(
     data = LabelDf,
     aes(
       x = LabelX,
       y = LabelY + YOffset,
       label = Id,
       hjust = HJust
     ),
     size = 3.2,
     angle = 45,
     lineheight = 0.9
   ) +
   coord_sf(
     xlim = XLimits,
     ylim = YLimits,
     expand = FALSE
   ) +
   labs(x = NULL, y = NULL) +
   theme_void() +
   theme(
     legend.position = "none",
     plot.margin = margin(10, 10, 10, 10)
   ))

# Legend data ####

LegendDf <-
  MapDf %>%
  dplyr::select(Species, Colour, Uuid, IconWidth) %>%
  distinct() %>%
  arrange(Species) %>%
  mutate(
    Row = row_number()
  )

# Legend position ####

LegendX <- XLimits[2] - diff(XLimits) * 0.12
LegendYStart <- YLimits[1] + diff(YLimits) * 0.25
LegendSpacing <- diff(YLimits) * 0.045

LegendDf <-
  LegendDf %>%
  mutate(
    X = LegendX,
    Y = LegendYStart + (n() - Row) * LegendSpacing
  )

# Add legend phylopics ####

for(I in seq_len(nrow(LegendDf))) {
  
  if(!is.na(LegendDf$Uuid[I])) {
    
    P <-
      P +
      rphylopic::add_phylopic(
        uuid = LegendDf$Uuid[I],
        x = LegendDf$X[I],
        y = LegendDf$Y[I],
        width = LegendDf$IconWidth[I] * 0.8,
        color = LegendDf$Colour[I],
        fill = LegendDf$Colour[I],
        alpha = 1
      )
    
  } else {
    
    P <-
      P +
      geom_point(
        data = LegendDf[I, , drop = FALSE],
        aes(x = X, y = Y),
        inherit.aes = FALSE,
        shape = 21,
        size = 2,
        fill = LegendDf$Colour[I],
        colour = "white"
      )
  }
}

# Add legend labels ####

P <-
  P +
  geom_text(
    data = LegendDf,
    aes(
      x = X + diff(XLimits) * 0.02,
      y = Y,
      label = Species
    ),
    hjust = 0,
    size = 3
  )

# Add PhyloPics in place of points ####

for(I in seq_len(nrow(MapDf))) {
  
  print(I)
  
  if(!is.na(MapDf$Uuid[I])) {
    P =
      P +
      rphylopic::add_phylopic(
        uuid = MapDf$Uuid[I],
        x = MapDf$Lon[I],
        y = MapDf$Lat[I],
        width = MapDf$IconWidth[I],
        # color = "black",
        # fill = "black",
        color = MapDf$Colour[I],
        fill = MapDf$Colour[I],
        alpha = 1
      )
  } else {
    P =
      P +
      geom_point(
        data = MapDf[I, , drop = FALSE],
        aes(x = Lon, y = Lat),
        inherit.aes = FALSE,
        shape = 21,
        size = 2.2,
        stroke = 0.25,
        colour = "white",
        fill = "black"
      )
  }
}

P

ggsave(
  filename = "Figures/CollaboratorMap_PhyloPic.png",
  plot = P,
  width = 300,
  height = 220,
  units = "mm",
  dpi = 600
)

ggsave(
  filename = "Figures/CollaboratorMap_PhyloPic.jpeg",
  plot = P,
  width = 300,
  height = 220,
  units = "mm",
  dpi = 600
)
