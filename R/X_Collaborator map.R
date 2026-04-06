
# X_Collaborator map ---------------------------------------------------------

library(tidyverse)
library(gsheet)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(cowplot)
library(colorspace)

theme_set(
  theme_cowplot() +
    theme(strip.background = element_rect(fill = "white"))
)

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
  )

# Standardise your Google Sheet data --------------------------------------

# This tries to preserve your existing columns and only adds missing ones.
# Adjust the fallbacks if your sheet uses different names.
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

# Plot --------------------------------------------------------------------

(FullMap <-
  ggplot() +
  # geom_sf(
  #   data = World,
  #   inherit.aes = FALSE,
  #   fill = "grey85",
  #   colour = "grey65",
  #   linewidth = 0.25
  # ) +
  # geom_curve(
  #   data = ConnectorDf,
  #   aes(x = x, y = y, xend = xend, yend = yend),
  #   curvature = 0.08,
  #   linewidth = 0.3,
  #   colour = "grey45",
  #   alpha = 0.8
  # ) +
  geom_point(
    data = MapDfOrdered,
    aes(x = Lon, y = Lat, #size = NStudies, 
        #fill = Source
        ),
    shape = 21,
    colour = "black",
    stroke = 0.3,
    alpha = 0.95
  ) +
  geom_text(
    data = MapDfOrdered,
    aes(
      x = LongSec,
      y = LabelY,
      label = DisplayLabel,
      hjust = if_else(Side == "top", 0, 1)
    ),
    angle = 90,
    size = 3.2,
    lineheight = 0.9
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-120, 180),
    expand = FALSE
  ) +
  scale_size_continuous(range = c(2.2, 5.5)) +
  # scale_fill_manual(
  #   values = c(
  #     "Google Sheet" = "#1B9E77",
  #     "Euromammals" = "#D95F02"
  #   )
  # ) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 30, 5.5, 30)
  ))



ggsave(
  filename = "Figures/CollaboratorMap.jpeg",
  plot = FullMap,
  units = "mm",
  height = 300,
  width = 300,
  dpi = 600
)