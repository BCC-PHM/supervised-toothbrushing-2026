library(readxl)
library(dplyr)
library(janitor)
library(BSol.mapR)
library(tmap)

# Load and process data
school_data <- read_excel("data/supervised-toothbrushing-schools-may-2026.xlsx") %>%
  clean_names() %>%
  # Only include schools invited
  mutate(
    invited_y_n == "y" 
  ) %>%
  # Improve status readability
  mutate(
    status = case_when(
      status == "Participating" ~ "Agreed",
      status == "Does not wish to participate" ~ "Declined",
      is.na(status) ~ "Invited"
    ),
    status = factor(status, levels = c("Active", "Agreed", "Declined", "Invited"))
  ) %>%
  # Join postcode info
  left_join(
    # Tower Street Recreation Ground
    postcodes <- read.csv("data/West Midlands postcodes.csv") %>%
      clean_names() %>%
      filter(
        grepl("Birmingham", constituency) | constituency == "Sutton Coldfield",
      ) %>%
      select(c(postcode, latitude, longitude)),
    by = join_by("postcode")
  ) %>%
  rename(LAT = latitude, LONG = longitude, Postcode = postcode)

school_shape <- sf::st_as_sf(get_points_shape(school_data))

IMD <- read_excel(
  "data/File_1_IoD2025 Index of Multiple Deprivation.xlsx",
  sheet = "IMD25") %>%
  clean_names() %>%
  filter(
    local_authority_district_name == "Birmingham",
    imd_decile %in% c(1,2)
  )

LSOA21_shape <- sf::st_as_sf(LSOA21) %>%
  inner_join(
    IMD, 
    by = join_by("LSOA21" == "lsoa_code_2021")
  ) 

Brum_localities_shape <- sf::st_as_sf(Locality) %>%
  filter(Area == "Birmingham")

Brum_shape <- sf::st_union(Brum_localities_shape)

# Load LSOA IMD data
map <- tm_shape(Brum_shape) +
  tm_borders(lwd=0) + 
  tm_shape(LSOA21_shape) +
  tm_fill(
    "imd_decile",
    fill.scale = tm_scale_categorical(
      n.max = 2,
      values = c("#619BFF", "#A6C8FF")
    ),
    fill.legend = tm_legend(
      orientation = "landscape",
      title = "IMD Decile",
      position = tm_pos_in(0.05, 0.87),
      width = 8,
      title.size = 0.7,
      text.size = 0.6
    )
  ) + 
  tm_shape(
    Brum_localities_shape
  ) +
  tm_borders(
    lwd = 1,
    col = "gray40"
    ) +
  tm_text(
    "Locality",
    col = "gray20"
    ) +
  tm_shape(
    Brum_shape
  ) +
  tm_borders(
    lwd = 1.5,
    col = "gray20"
  ) +
  tm_shape(school_shape) +
  tm_dots(
    size = 0.3,
    shape = 21,
    fill = "status",
    fill.scale = tmap::tm_scale(
      values = c("green", "yellow", "darkred", "gray")
    ),
    fill.legend = tm_legend(
      title = "Participation Status",
      position = tm_pos_in(0.05, 0.75),
      width = 8,
      title.size = 0.7,
      text.size = 0.6
    )
  ) +
  tmap::tm_layout(
    legend.frame.alpha = 0,
    legend.frame.lwd = 0,
    legend.frame = FALSE,
    legend.bg.alpha = 0,
    inner.margins = c(0.08, 0.08, 0.13, 0.08),
    frame = FALSE,
    title = "Schools Participating in the Supervised Toothbrushing\nProgramme (2026)",
    title.size = 1
  ) +
  tm_credits(
    paste("Contains OS data \u00A9 Crown copyright and database right",
          # Get current year
          format(Sys.Date(), "%Y"),
          ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
    ), 
    size = 0.8,
    position = c(0, 0.05)
    )+
  tmap::tm_compass(
    type = "8star",
    size = 4,
    position = c(0.75, 0.25),
    color.light = "white"
  )

map

save_map(map, "output/supervised-toothbrushing-map-2026.png")