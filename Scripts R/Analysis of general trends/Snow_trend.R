# Load libraries
library(tidyverse)
library(plotly)
library(ggpmisc)
library(sf)
library(ggspatial)
library(cowplot)
library(terra)
library(ggmap)


# Load dataframe from GEE
df<-read.delim2("Snow_cover.csv", sep = ";", dec = ".", fileEncoding = "Latin1")


## Load the ski resort data
data<-read.delim("Ski_resort_data.csv", sep = ";", stringsAsFactors=FALSE, fileEncoding="latin1")
# Filter the skiable tracks data
data<- data %>% filter(Source=="Kilometros Abiertos")

# Year to factor 
data$Year<-as.factor(data$Year)


year_summary<- data %>% 
  group_by(Year) %>% 
  summarise(year_mean=mean(value, na.rm=TRUE))

year_summary$Year<- c(2010:2025)

# Convert year to numeric
# year_summary$Year<-as.numeric(year_summary$Year)


#############################
##### Plots #################
#############################

## Combine snow cover and skiable tracks data in one plot 
g <- ggplot(df, aes(Year, SnowCover, group=1))+
  geom_point(year_summary, mapping = aes(year_summary$Year, (year_summary$year_mean/10)),  colour="red" )+
  geom_line(year_summary, mapping = aes(year_summary$Year, (year_summary$year_mean/10)),  colour="red" )+
  geom_smooth(method = "lm", se=F)+
  stat_poly_eq(formula= y ~ x , aes(label = paste(after_stat(eq.label),
                                                  sep = "*\", \"*")))+
  stat_poly_eq(formula= y ~ x , label.y = 0.85)+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(x="years", y="Snow cover %")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black"))+
  scale_y_continuous(name="Snow cover %",
                     sec.axis = sec_axis(~.*10,name = " Skiable tracks (km)"))

g


# Load the shapefiles for the area of study and Countries
PN_snev <- st_read("../Spatial analysis/WDPA_WDOECM_May2025_Public_555549110_shp_0/WDPA_WDOECM_May2025_Public_555549110_shp-polygons.shp")
Countries <- st_read("../Spatial analysis/CNTR_RG_01M_2016_3035.shp")


# Filter Spain and Portugal
Countries <- Countries %>% 
  filter(NAME_ENGL %in%  c("Spain", "Portugal", "France", "Italy") )

# Convert the CRS of the shapefiles to match 
Countries <- st_transform(Countries, crs = st_crs(PN_snev))



map_main <- ggplot() +
  geom_sf(data = PN_snev, fill = "purple", alpha = 0.3, color = "black", linewidth = 1) +
  geom_sf(data = Countries, alpha = 0.4, color = "black") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_sf(xlim = c(-3.6, -2.5), ylim = c(36.6, 37.4))+
  scale_x_continuous(
    breaks = seq(-3.6, -2.5, by = 0.4),  # Más breaks para mayor detalle
    # name = "Longitud"
  ) +
  scale_y_continuous(
    breaks = seq(36.6, 37.4, by = 0.4),  # Más breaks para mayor detalle
    # name = "Latitud"
  )



map_main

# Crear el inserto
bbox_area <- st_bbox(PN_snev)
inset <- ggplot() +
  geom_sf(data = Countries, fill= "grey90") +
  geom_sf(data = PN_snev, fill = "purple", alpha = 0.4, color = "black") +
  coord_sf(xlim = c(-11, 1), ylim = c(36, 44))+
  scale_x_continuous(
    breaks = c(-11, -5, 0))+
  scale_y_continuous(
    breaks = c(44, 40, 36))+
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        panel.grid = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))


inset
# Usar cowplot para insertar el mapa pequeño en el grande
map_combined <- ggdraw() +
  draw_plot(map_main) +
  draw_plot(inset, x = 0.68, y = 0.7, width = 0.3, height = 0.3)



map_combined_final <- ggdraw() +
  draw_plot(map_combined, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(g, x = 0.5, y = 0, width = 0.5, height = 1)

# Agregate labs a) and b)
map_combined_final <- map_combined_final +
  draw_label("a)", x = 0.016, y = 0.95, size = 14, fontface = "bold") +
  draw_label("b)", x = 0.516, y = 0.95, size = 14, fontface = "bold")

map_combined_final


# plot_grid(map_combined, g, labels = c("a)", "b)"), ncol = 2)
# Save the plot as a PNG file
ggsave("../Figures/SnowCover_Skiable_tracks.png", map_combined_final, units = "cm", width = 25, height = 10, dpi = 300)


#################################
####### END OF SCRIPT ###########
#################################