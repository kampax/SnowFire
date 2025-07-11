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

## Load required libraries
library(tidyverse)
library(car)
library(multcomp)
library(gamlss)
library(ggnewscale)
library(kableExtra)
library(agricolae)
library(broom)
library(olsrr)

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
df_y <-df %>% mutate(NDSI_bin = ifelse(NDSI_mean > 0.40, 1, 0 ))


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
                             Año %in% c(2007:2012) ~ 'Post-fire',
                             Año %in% c(2013:2025) ~ 'After Post-fire'))


###################################
######2) TEMPORAL ANALISIS########
###################################

#####################################
###2.a)SUMMARY PERCENTAGE PER PIXEL##
#####################################

# Load the ancillary data
datos <- read.delim("PlotPixels.csv", sep = ",")

# Filter the data to keep only the polygons of interest suit = 1 based on the first database
for_selection <- unique(df$polygon_id)

datos <- datos %>% filter(ID %in% for_selection)


## Summarize the binary NDSI values by treatment and period
df_y2 <- data %>%
  group_by(DATE_ACQUIRED, Trat_1) %>%
  mutate(snow_sum = sum(NDSI_bin, na.rm = TRUE)) %>%
  ungroup()  

## Exclude images without snow for all treatments/ OPTIONAL

df_y3 <- df_y2

## OPTIONAL 
# df_y3 <- df_y2 %>%
#   group_by(DATE_ACQUIRED) %>%
#   filter(sum(snow_sum[Trat_1 %in% c("SL", "PCL", "NI")]) != 0) %>%
#   ungroup()

### Join the dataframes
unidos <- merge(df_y3, datos, by.x ="polygon_id", by.y ="ID")


### Calculate the mean snow cover percentage per pixel 
tab <- unidos %>% 
  group_by(polygon_id, Trat_1.x, PrePost) %>% 
  summarise(porcent = (mean (NDSI_bin)*100),
            Parcela = unique(Parcela),
            Repl = unique(Repl), 
            ReplicaID = unique(ReplicaID),
            Trat_num = unique(Trat_num), 
            curvature = mean(curvature), 
            insolation = mean(insolation),
            orientation = mean(orientation),
            shadows = mean (shadows),
            slope = mean (slope), 
            suit = unique(suit),
            elevation = mean(height),
            trat_borde = unique(trat_borde),
            x = unique(x), 
            y = unique(y))

# Remove the "Historical records" period
tab<- tab %>% filter(PrePost %in% c('Pre-fire', 'Post-fire'))

# convert to factors
tab$PrePost <- factor(tab$PrePost, levels = c("Pre-fire", "Post-fire"))

# Modify the name of periods
tab$PrePost2 <- factor(tab$PrePost, levels = c("Pre-fire", "Post-fire"), 
                       labels = c("Pre-fire period", "Post-fire period"))


tabla <- data.frame(
  Trat_1 = c("NI", "SL", "PCL", "NI", "PCL", "SL"),
  y_pos = c( 65, 65, 65, 65, 40, 40),
  groups = c("a", "a", "a", "a*", "b", "b"),
  PrePost2 = c("Pre-fire period", "Pre-fire period", "Pre-fire period", 
               "Post-fire period", "Post-fire period", "Post-fire period")
)



tabla$PrePost2 <- factor(tabla$PrePost, levels = c("Pre-fire period", "Post-fire period"), 
                         labels = c("Pre-fire period", "Post-fire period"))


# Plot 
g <- tab %>% 
  filter(PrePost %in% c('Pre-fire', 'Post-fire')) %>%
  ggplot(tab, mapping = aes(x = Trat_1.x, y = porcent, fill = Trat_1.x)) +
  geom_boxplot(alpha = 0.75) +
  facet_wrap(~ PrePost2) +
  labs(title = "",
       x = "Treatment",
       y = "Percentage of snow occurrence (%)") +
  theme_bw()+
  scale_fill_manual(values = c("darkgreen", "#202021","#8C2A9F"))+
  # scale_fill_manual(values = c("gray80", "gray60", "gray40"))+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  # geom_text(
  #   data = tabla,
  #   aes(x = Trat_1, y = y_pos, label = groups),
  #   inherit.aes = FALSE,
  #   size = 5,
  #   fontface = "bold"
  # )

g


ggsave("Figures/Snow_Pixel_Level.jpg", g, units = "cm", width = 20, height = 10, dpi = 300)


#########################
### Prefire analysis ####
#########################

# Filter the period
pre_period<-tab %>% filter(PrePost=='Pre-fire')

# Select the relevant columns
pre<- pre_period %>%
  dplyr::select(porcent, Trat_1.x, curvature, insolation, orientation, shadows, slope, x, y, elevation)

# exclude the ID column
pre <- pre[,2:11]

### Stepwaise regresion 
full.model.pre <- lm(porcent ~., data = pre)

# Stepwise regression model
step_both_pre  <- ols_step_both_p(full.model.pre)

step_both_pre_model <- lm(porcent ~ Trat_1.x + elevation+ x + insolation+ shadows, data=pre)

summary(step_both_pre_model)
summary.aov(step_both_pre_model)

step_both_pre$metrics$variable

# Extract relevant columns from the stepwise selection object
stepwise_model_table <- step_both_pre$metrics %>%
  transmute(
    Step = step,
    Variable_Added_or_Removed = variable,
    Method = method,
    AIC = aic,
    R_squared = r2,
    Adjusted_R_squared = adj_r2
  )

# Add full model statistics for comparison

full_model_stats <- data.frame(
  Step = 0,
  Variable_Added_or_Removed = "Full model",
  Method = "full",
  AIC = AIC(full.model.pre),
  R_squared = summary(full.model.pre)$r.squared,
  Adjusted_R_squared = summary(full.model.pre)$adj.r.squared
)

# Combine stepwise and full model stats
model_comparison_table <- bind_rows(full_model_stats, stepwise_model_table) %>%
  arrange(AIC)

# Print the result
print(model_comparison_table)


##########################
###Post fire analysis ####
##########################

# Filter the period
post_period<-tab %>% filter(PrePost=='Post-fire')

# Select the relevant columns
post<- post_period %>%
  dplyr::select(porcent, Trat_1.x, curvature, insolation, orientation, shadows, slope, x, y, elevation)

# Exclude the ID column
post <- post[,2:11]
### Stepwaise regresion 
full.model.post <- lm(porcent ~., data = post)

# Stepwise regression model
# Stepwise regression model
step_both_post  <- ols_step_both_p(full.model.post)

step_both_post_model <- lm(porcent ~  elevation + x + Trat_1.x +  y +curvature + orientation + shadows + slope, data=post)

summary(step_both_post_model)
summary.aov(step_both_post_model)


step_both_post$metrics$variable

# Extract relevant columns from the stepwise selection object
stepwise_model_table <- step_both_post$metrics %>%
  transmute(
    Step = step,
    Variable_Added_or_Removed = variable,
    Method = method,
    AIC = aic,
    R_squared = r2,
    Adjusted_R_squared = adj_r2
  )

# Add full model statistics for comparison

full_model_stats <- data.frame(
  Step = 0,
  Variable_Added_or_Removed = "Full model",
  Method = "full",
  AIC = AIC(full.model.post),
  R_squared = summary(full.model.post)$r.squared,
  Adjusted_R_squared = summary(full.model.post)$adj.r.squared
)

# Combine stepwise and full model stats
model_comparison_table <- bind_rows(full_model_stats, stepwise_model_table) %>%
  arrange(AIC)

# Print the result
print(model_comparison_table)



########################################
#### Difference #########################
########################################

tab2 <- tab %>%
  group_by(polygon_id, Trat_1.x) %>%
  summarise(
    pre_fire = mean(porcent[PrePost == "Pre-fire"], na.rm = TRUE),
    post_fire = mean(porcent[PrePost == "Post-fire"], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(difference = post_fire - pre_fire)



g <- ggplot(tab2, aes(x = Trat_1.x, y = difference, fill = Trat_1.x)) +
  geom_boxplot(alpha = 0.75) +
  labs(title = "Differences in Snow Cover Percentage by Treatment",
       x = "Treatment",
       y = "Difference in Snow Cover Percentage (%)", fill="Treatment") +
  theme_bw() +
  scale_fill_manual(values = c("NI" = "darkgreen", "SL" = "#202021", "PCL" = "#8C2A9F")) +
  theme(legend.position = "right")
g

ggsave("Figures/Difference_pixel_level.jpg", g, units = "cm", width = 20, height = 10, dpi = 300)

######################################
###2) Summarise table ##################
######################################



stargazer(step_both_pre_model, step_both_post_model,
          title = "Best model",
          out = "regression_results.txt",
          type = "text",
          column.labels = c("Pre-fire", "Post-fire"),
          dep.var.labels = "Percentage of snow occurrence (%)",
          covariate.labels = c("PCL", "SL", "Curvature", "Insolation", "Slope", "Orientation", "Shadows", "x", "y", "Intercept"),
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          digits = 3)

######################################
###3) Scenes Summarize ###############
######################################



data_x <- data %>% 
  group_by(PrePost,Año, Mes, DATE_ACQUIRED, Trat_1) %>% 
  summarise(snow_sum=sum(NDSI_bin), total = 422)


data_x <- data_x %>%
  mutate(snow = ifelse(snow_sum == 0, 0, 1))


resumen1 <- data_x %>% 
  group_by(DATE_ACQUIRED, PrePost, Año, Mes ) %>% #Año_real, Mes, PrePost
  summarize(snow_presence = max(snow, na.rm = T))

resumen1 <- resumen1 %>% 
  group_by(Año) %>% 
  mutate(escenes = length(Año)) %>% 
  ungroup()


resumen1 <- resumen1 %>%
  mutate(snow_fact = ifelse(snow_presence == 0, "NoSnow", "Snow"))


r <- resumen1 %>% 
  group_by(PrePost, Año, snow_fact) %>% 
  summarise(count=length(snow_presence))

r <- r %>% 
  group_by( PrePost, Año) %>% 
  mutate(escenes = sum(count))

# Número de escenas con nubes por año
# Datos combinados por satélite
cloudy_l5 <- data.frame(
  Año = c(1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993,
          1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004,
          2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012),
  cloudy_scenes = c(
    1, 4, 3, 3, 4, 3, 3, 3, 3, 4,
    1, 4, 3, 2, 3, 3, 3, 3, 2, 3,
    2, 1, 2, 1, 2, 5, 3, 1
  )
)

cloudy_l7 <- data.frame(
  Año = c(1999, 2000, 2001, 2002, 2003),
  cloudy_scenes = c(2, 4, 5, 4, 3)
)

cloudy_l8 <- data.frame(
  Año = c(2013, 2014, 2015, 2016, 2017, 2018, 2019,
          2020, 2021, 2022, 2023, 2024, 2025),
  cloudy_scenes = c(5, 6, 3, 5, 6, 6, 5,
                    4, 4, 6, 0, 6, 3)
)

# Join
cloudy_scenes <- bind_rows(cloudy_l5, cloudy_l7, cloudy_l8) %>%
  group_by(Año) %>%
  summarise(cloudy_scenes = sum(cloudy_scenes), .groups = "drop") %>%
  arrange(Año)

# Agregar la columna de escenas nubladas
r <- merge(r, cloudy_scenes, by = "Año", all.x = TRUE)

# Rellenar los NA con 0 (si algún año no tuvo escenas nubladas)
r$cloudy_scenes[is.na(r$cloudy_scenes)] <- 0



library(ggpattern)

# Definir los colores
colores_barras <- c('Snow' = "lightblue4", 'NoSnow' = "orange4")
colores_fondo <- c("Pre-fire" = "grey5", "Post-fire" = "grey70", "Fire" = "red")

# Filtrar los datos
rgraph <- r %>% filter(PrePost %in% c('Pre-fire', 'Fire','Post-fire'))


# Crear resumen de períodos
periodos <- rgraph %>%
  group_by(PrePost) %>%
  summarise(xmin = min(Año) - 0.5,
            xmax = max(Año) + 0.5,
            .groups = "drop") %>%
  mutate(ymin = 0,
         ymax = max(rgraph$escenes) * 2.1)


g3 <- ggplot() +
  # Rectángulos para los períodos
  geom_rect(data = periodos,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = PrePost),
            color = "black", size = 0.3, alpha = 0.2) +
  scale_fill_manual(name = "Period", values = colores_fondo,
                    breaks = c("Pre-fire","Fire", "Post-fire"),
                    labels = c("Pre-fire period", "Fire period","Post-fire period")) +
  ggnewscale::new_scale_fill() +
  
  # Barras de datos apiladas para escenas con o sin nieve
  geom_col(data = rgraph, aes(x = Año, y = escenes, fill = snow_fact), width = 0.8) +
  scale_fill_manual(name = "Condition", values = colores_barras,
                    breaks = c("Snow", "NoSnow"),
                    labels = c("Scenes with snow", "Scenes without snow")) +
  ggnewscale::new_scale_fill() +
  
  # Barras más delgadas por número de escenas nubladas
  geom_col(data = rgraph, aes(x = Año, y = cloudy_scenes, fill = "Cloudy scenes"), 
           width = 0.3, position = position_nudge(x = 0.25), alpha = 0.6) +
  scale_fill_manual(name = "Cloud cover", values = c("Cloudy scenes" = "gray50")) +
  
  labs(x = "Year", y = "Number of scenes") +
  theme_bw() +
  theme(legend.position = "right", legend.box = "vertical")

# Mostrar
g3


ggsave("Figures/Number of scenes per period.jpg",g3, units = "cm", width = 20, height = 10, dpi = 300)


r2 <- r %>% 
  group_by(PrePost, snow_fact) %>% 
  summarise(count2 = sum(count))


kbl(r2, booktabs = TRUE, caption = "Summary of scenes by period") %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  kable_classic(full_width = F, html_font = "Cambria")

r2 %>% 
  kbl(caption = "Summary of scenes by period") %>%
  kable_classic(full_width = F, html_font = "Cambria")

######################################
# Sumarise the Satelital data #######
#####################################
sat <- data %>%
  mutate(Satellite = SPACECRAFT_ID,
         ID_Scene = stringr::str_extract(system.index, "L[ET]0[57]_[0-9]{6}_[0-9]{8}"),
         Date = as.Date(DATE_ACQUIRED, format = "%Y-%m-%d")) %>% 
  dplyr::select(Satellite, ID_Scene, Date, PrePost, NDSI_bin)

sat<- sat %>% 
  group_by(Satellite,ID_Scene, Date, PrePost) %>% 
  summarise(snow_sum=sum(NDSI_bin))

# Exclude the historical period and order 
sat <- sat %>% filter(PrePost %in% c('Pre-fire', 'Post-fire', 'Fire')) %>% arrange(Satellite, Date)

# Number of scenes per Satellite
sat %>%
  group_by(Satellite) %>%
  summarise(Scenes = n_distinct(ID_Scene))

sat<- sat %>% dplyr::select(Satellite, ID_Scene, Date)


# Save as csv
write.csv(sat, "Scenes_used.csv", row.names = FALSE)


#########################################
# Summarise the values of covariates ####
#########################################


# mean values per pixel
datos2 <- datos %>% 
  group_by(ID) %>% 
  summarise(
    Trat_1 = unique(Trat_1),
    Repl = unique(ReplicaID),
    height = mean(height, na.rm = TRUE),
    y = mean(y, na.rm = TRUE),
    x = mean(x, na.rm = TRUE),
    slope = mean(slope, na.rm = TRUE),
    orientation = mean(orientation, na.rm = TRUE)
  )




# Summarise the covariates by treatment and replication

covariates_summ <- datos2 %>% 
  group_by(Trat_1, Repl) %>% 
  summarise(
    elevation_mean = round(mean(height, na.rm = TRUE),2),
    latitude_mean = mean(y, na.rm = TRUE),
    longitude_mean = mean(x, na.rm = TRUE),
    number_of_pixels = n(),
    Area = round((n()* 30 * 30 / 10000), 2), 
    slope_mean = round(mean(slope, na.rm = TRUE), 2),
    aspect_mean = round(mean(orientation, na.rm = TRUE),2)
  )


mean(covariates_summ$slope_mean)

#########################################
############## End of script#############
#########################################

