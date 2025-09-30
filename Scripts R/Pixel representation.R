# Burnt wood removal reduces snow persistence after wildfire –a remote-sensing assessment
# Authors: [P. Cazorla, Beatriz1,4*, Navarro, Carlos Javier 1*;  Martínez-López, Javier 1,2,4; Postma, Thedmer M. 1; Leverkus, Alexandro B. 1,2; Alcaraz-Segura, Domingo 1,3,4 ; Castro, Jorge 2]
# Affiliations: 
# 1 Andalusian Institute for Earth System Research IISTA-CEAMA, Spain 
# 2 Ecology Department, Faculty of Sciences, University of Granada, Spain 
# 3 Botany Department, Faculty of Sciences, University of Granada, Spain
# 4 Andalusian Center for Global Change - Hermelindo Castro (engloba), University of Almería, Spain

# Description: This script processes and visualizes snow cover data from Landsat imagery
#              in areas affected by different post-fire management treatments.
# Date: 2025-06-10
# Last update: 2025-09-30
###################################

# Load libraries
library(ggplot2)
library(sf)
library(dplyr)
library(scales)
library(ggspatial)
library(prettymapr)
library(terra)
library(tidyterra)
library(leaflet)


## Post fire values 

## Load database
df<- read.delim("NDSI_Values.csv", sep = ",")

# Count the number of unique polygons per treatment
df %>% 
  group_by(Trat_1) %>% 
  summarise(n = length(unique(polygon_id)))

### Option to filter by spacecraft
# df <- df %>% filter(SPACECRAFT_ID == "LANDSAT_5")


###################################
######1) Preprocesing #########
###################################

## Create binary variable for snow presence
df_y <-df %>% mutate(NDSI_bin = ifelse(NDSI_mean > 0.4, 1, 0 ))


# Separate the year into a new column
data <- df_y %>%
  mutate(Año_real = as.numeric(substr(DATE_ACQUIRED, 1, 4)),
         Mes = as.numeric(substr(DATE_ACQUIRED, 6, 7)))


### Create new column called year to assign the months of October, November and December to the following year (meteorological year that goes from October to May) for that I add to the year the value of 1

data <- data %>% 
  mutate(Año = ifelse(Mes == 12, (Año_real+1), Año_real))
data <- data %>% 
  mutate(Año = ifelse(Mes == 11, (Año_real+1), Año))
data <- data %>% 
  mutate(Año = ifelse(Mes == 10, (Año_real+1), Año))


# With the year column I create periods of interest
data <- data %>%
  mutate(PrePost = case_when(Año %in% c(1984:1999) ~ "Historical records",
                             Año %in% c(2000:2005) ~ "Pre-fire",
                             Año %in% c(2006:2006) ~ "Fire",
                             Año %in% c(2007:2025) ~ 'Post-fire'))

# Load the data
spatial_data <- st_read("Spatial analysis/plots_pixels.shp")  

# Join spatial data with the  data using ID of statial with poligon_id
spatial_data <- spatial_data %>%
  left_join(data, by = c("ID" = "polygon_id"))



# Excluir los años que no son de interés
spatial_data <- spatial_data %>%
  filter(Año %in% c(2007:2012))


# Sacar un valor promedio por poligon_id por periodo y luego solo filtrar post_fire
spatial_data <- spatial_data %>%
  group_by(ID, Trat_1.y, PrePost) %>%
  summarise(mean_NDSI = mean(NDSI_bin, na.rm = TRUE)) %>%
  ungroup()

spatial_data<- spatial_data %>%
  mutate(porcentaje = mean_NDSI * 100)


# Categorizing the differences
spatial_data <- spatial_data %>%
  mutate(
    dif_cat = cut(porcentaje,
                  breaks = c(-Inf, 6, 13, 20, 27, 34, 41, Inf),
                  labels = c("0 to 6", "6 to 13", "13 to 20", "20 to 27",
                             "27 to 34", "34 to 41", "41 to 48"),
                  right = FALSE),
    Trat_1 = factor(Trat_1.y, levels = c("NI", "SL", "PCL"))
  )


# Define the color palette for the differences
colors_dif <- c(
  "0 to 6" = "#ca0020",
  "6 to 13"  = "#f4a582",
  "13 to 20"   = "#fddbc7",
  "20 to 27"    = "#f7f7f7",
  "27 to 34"     = "#ccece6",  # Celeste claro en lugar del verde
  "34 to 41"    = "#92c5de",
  "41 to 48"   = "#0571b0"
)

# Create the countours for the treatments
contours <- spatial_data %>%
  group_by(Trat_1) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()



# Plot 
g <- ggplot() +
  geom_sf(data = spatial_data, aes(fill = dif_cat), color = NA) +
  geom_sf(data = contours, aes(color = Trat_1), fill = NA, linewidth = 0.8) +
  scale_fill_manual(values = colors_dif, name = "Snow ocurrence (%) \npost-fire period") +
  scale_color_manual(
    name = "Treatment",
    values = c("NI" = "darkgreen", "SL" = "#984EA3", "PCL" = "#000000"),
    labels = c("NI (151 pixels)", "SL (59 pixels)", "PCL (212 pixels)")
  ) +
  annotation_scale(location = "bl", width_hint = 0.18, bar_cols = c("black", "white")) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering(fill = c("black", "white")),
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "transparent"),
    panel.background = element_rect(fill = "#e6f2d3", color = NA),
    axis.ticks = element_line(color = "black"),  # Color de los ticks
    axis.ticks.length = unit(-0.15, "cm"),      # Longitud negativa para ticks internos
    # panel.background = element_rect(fill = "gray95", color = NA)
  ) +
  coord_sf() +
  # scale_x_continuous(labels = function(x) round(x, 2)) +
  # scale_y_continuous(labels = function(x) round(x, 2)) +
  labs(x = "", y = "")+
  scale_x_continuous(
    breaks = c(-3.486, -3.474),  # Más breaks para mayor detalle
    # name = "Longitud"
  ) +
  scale_y_continuous(
    breaks = c(36.970, 36.964),  # Más breaks para mayor detalle
    # name = "Latitud"
  )

g


# Save the plot as a PNG file
ggsave("Figures/PixelChart_postfire.png", g, units = "cm", width = 19, height = 10, dpi = 300)



#########################################
############## End of script#############
#########################################