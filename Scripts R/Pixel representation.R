# Cargar paquetes
library(ggplot2)
library(sf)
library(dplyr)
library(scales)
library(ggspatial)
library(prettymapr)
library(terra)
library(tidyterra)
library(leaflet)

# Load the data
data <- st_read("Spatial analysis/Difference_p3p4.shp")  


# Rename Pre_difere as "diference"
data <- data %>%
  rename(diference = Pre_difere)


# Categorizing the differences
data <- data %>%
  mutate(
    dif_cat = cut(diference,
                  breaks = c(-Inf, -15, -9, -3, 3, 9, 15, Inf),
                  labels = c("-21 to -15", "-15 to -9", "-9 to -3", "-3 to 3",
                             "3 to 9", "9 to 15", "15 to 21"),
                  right = FALSE),
    Trat_1 = factor(Trat_1, levels = c("NI", "SL", "PCL"))
  )


# Define the color palette for the differences
colors_dif <- c(
  "-21 to -15" = "#ca0020",
  "-15 to -9"  = "#f4a582",
  "-9 to -3"   = "#fddbc7",
  "-3 to 3"    = "#f7f7f7",
  "3 to 9"     = "#d9f0d3",
  "9 to 15"    = "#92c5de",
  "15 to 21"   = "#0571b0"
)

# Create the countours for the treatments
contours <- data %>%
  group_by(Trat_1) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

 

# Plot 
g <- ggplot() +
  geom_sf(data = data, aes(fill = dif_cat), color = NA) +
  geom_sf(data = contours, aes(color = Trat_1), fill = NA, linewidth = 0.8) +
  scale_fill_manual(values = colors_dif, name = "Difference post-pre fire") +
  scale_color_manual(
    name = "Treatment",
    values = c("NI" = "#67A9CF", "SL" = "#984EA3", "PCL" = "#000000"),
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
    panel.background = element_rect(fill = "#e6f2d3", color = NA)
    # panel.background = element_rect(fill = "gray95", color = NA)
  ) +
  coord_sf() +
  scale_x_continuous(labels = function(x) round(x, 2)) +
  scale_y_continuous(labels = function(x) round(x, 2)) +
  labs(x = "", y = "")

g

# Save the plot as a PNG file
ggsave("Figures/PixelChart.png", g, units = "cm", width = 19, height = 10, dpi = 300)


###################
## WITH LEAFLET####
###################


data <- st_transform(data, crs = 4326)

# Color palette for differences
pal <- colorFactor(
  palette = c("#ca0020", "#f4a582", "#fddbc7", "#f7f7f7",
              "#d9f0d3", "#92c5de", "#0571b0"),
  domain = levels(data$dif_cat)
)

# Paleta de colores para tratamientos
pal_trat <- colorFactor(
  palette = c("NI" = "#67A9CF", "SL" = "#984EA3", "PCL" = "#000000"),
  domain = levels(data$Trat_1)
)

# Mapa interactivo
leaflet(data) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%  # Fondo satelital
  addPolygons(
    fillColor = ~pal(dif_cat),
    color = ~pal_trat(Trat_1),
    weight = 2,
    opacity = 1,
    fillOpacity = 0.6,
    group = "Diferences",
    popup = ~paste("Tratamiento:", Trat_1, "<br>Diferencia:", diference)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~dif_cat,
            title = "Diferencia post-pre fuego") %>%
  addLegend("topright", pal = pal_trat, values = ~Trat_1,
            title = "Tratamiento") %>%
  addLayersControl(
    baseGroups = c("Satélite"),
    overlayGroups = c("Diferences"),
    options = layersControlOptions(collapsed = FALSE)
  )


# 
# g <- ggplot() +
#   geom_sf(data = datos, aes(fill = dif_cat), color = NA) +
#   geom_sf(data = contornos, aes(color = Trat_1), fill = NA, linewidth = 1) +
#   scale_fill_manual(values = colores_dif, name = "Difference post-pre fire") +
#   scale_color_manual(
#     name = "Treatment",
#     values = c("NI" = "#67A9CF", "SL" = "#984EA3", "PCL" = "#000000"),
#     labels = c("NI (151 pixels)", "SL (59 pixels)", "PCL (212 pixels)")
#   ) +
#   annotation_scale(location = "bl", width_hint = 0.18, bar_cols = c("black", "white")) +
#   annotation_north_arrow(
#     location = "tl", which_north = "true",
#     style = north_arrow_fancy_orienteering(fill = c("black", "white")),
#     height = unit(1.5, "cm"),
#     width = unit(1.5, "cm")
#   ) +
#   theme_minimal() +
#   theme(
#     # Posición de la leyenda (x, y) en coordenadas normalizadas (0-1)
#     legend.position = c(0.85, 0.75),
#     # Fondo de la leyenda (opcional)
#     legend.background = element_rect(fill = "white", color = "gray50"),
#     # Espacio alrededor de la leyenda (opcional)
#     legend.box.margin = margin(10, 10, 10, 10),
#     # Margen del gráfico (top, right, bottom, left)
#     plot.margin = unit(c(1, 5, 1, 1), "cm"),
#     panel.grid.major = element_line(color = "transparent"),
#     panel.background = element_rect(fill = "#e6f2d3", color = NA)
#   ) +
#   coord_sf() +
#   scale_x_continuous(labels = function(x) round(x, 2)) +
#   scale_y_continuous(labels = function(x) round(x, 2)) +
#   labs(x = "", y = "")
# 
# g
