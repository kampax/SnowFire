# Assessing postfire burned wood management effects on snow duration in a Mediterranean mountain using remote sensing
# Author: [Carlos Javier Navarro]
# Description: This script processes and visualizes snow cover data from Landsat imagery
#              in areas affected by different post-fire management treatments.


## Load required libraries
library(tidyverse)
library(car)
library(multcomp)
library(gamlss)
library(ggnewscale)
library(kableExtra)

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
df_y <-df %>% mutate(NDSI_bin = ifelse(NDSI_mean > 0.35, 1, 0 ))


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
                             Año %in% c(2007:2012) ~ 'Post-fire'))




# Exclude images without snow
## 
filtered_data <- data %>% 
  group_by(PrePost, DATE_ACQUIRED, Trat_1) %>% 
  summarise(snow_sum=sum(NDSI_bin), total = 422)


## Exclude images without snow for all treatments
filtered_data <- filtered_data %>%
  group_by(DATE_ACQUIRED) %>%
  filter(sum(snow_sum[Trat_1 %in% c("SL", "PCL", "NI")]) != 0) %>%
  ungroup()

## New column with the number of pixels per treatment
filtered_data <- filtered_data %>%
  mutate(pixel_number = case_when(
    Trat_1 == "NI" ~ 151,
    Trat_1 == "SL" ~ 59,
    Trat_1 == "PCL" ~ 212
  ))



# Calculates the percentage of snow cover per scene and treatment
filtered_data<-filtered_data %>% 
  mutate(porcentaje = (snow_sum/pixel_number)*100)


## Summarize the values by treatment and period
resumen = filtered_data %>% 
  group_by(Trat_1, PrePost) %>% 
  summarise(mean_trat = mean(porcentaje, na.rm=T),
            median_trat = median(porcentaje), 
            sd = sd(porcentaje, na.rm=T))

# Diference between NI and the other treatments this information its mentioned in the abstract
resumen %>% 
  filter(PrePost == "Post-fire") 


# Convert the variables to factors
filtered_data$Trat_1 <- as.factor(filtered_data$Trat_1)
filtered_data$PrePost <- as.factor(filtered_data$PrePost)




###################################
######2) Spatial analizes #########
###################################


###########################
###2.a)  PREFIRE ANALISIS###
###########################

# Filter the period
pre_period<-filtered_data %>% filter(PrePost=='Pre-fire')

## ANOVA
anova_result <- aov(sqrt(porcentaje + 1) ~ Trat_1, data = pre_period)

# Results
summary_pre <- summary(anova_result)

# Normality test
shapiro_test <- shapiro.test(residuals(anova_result))
shapiro_test

# Homogeneity of variance test
levene_test <- leveneTest(porcentaje ~ Trat_1, data = pre_period)
levene_test

# post hoc comparisons using Tukey's test
tukey_result_pre <- TukeyHSD(anova_result)
tukey_pre_df <- as.data.frame(tukey_result_pre$Trat_1)


############################
###2.b) POSTFIRE ANALIZES###
############################

# Filtro el periodo
post_period<-filtered_data %>% filter(PrePost=='Post-fire')

## ANOVA
anova_result <- aov(sqrt(porcentaje + 1) ~ Trat_1, data = post_period)

# Normality test
shapiro_test <- shapiro.test(residuals(anova_result))
shapiro_test

# Homogeneity of variance test
levene_test <- leveneTest(porcentaje ~ Trat_1, data = post_period)
levene_test


# Results
summary_post <- summary(anova_result)

# post hoc comparisons using Tukey's test
tukey_result_post <- TukeyHSD(anova_result)
tukey_result_post
tukey_post_df <- as.data.frame(tukey_result_post$Trat_1)


# Kruskall wallis
kruskal_test_post <- kruskal.test(porcentaje ~ Trat_1, data = post_period)
kruskal_test_post
#########################
###2.c) TABLES###########
#########################


# Extract the ANOVA results for pre-fire and post-fire periods
anova_results <- data.frame(
  Periodo = rep(c("Pre-fire", "Post-fire"), each = 2),
  Fuente = rep(c("Trat_1", "Residuals"), 2),
  `Sum Sq` = c(summary_pre[[1]][["Sum Sq"]], summary_post[[1]][["Sum Sq"]]),
  `Mean Sq` = c(summary_pre[[1]][["Mean Sq"]], summary_post[[1]][["Mean Sq"]]),
  `F value` = c(summary_pre[[1]][["F value"]], summary_post[[1]][["F value"]]),
  `Pr(>F)` = c(summary_pre[[1]][["Pr(>F)"]], summary_post[[1]][["Pr(>F)"]])
)


# Create the data frame for Tukey test results
tukey_results <- data.frame(
  Period = rep(c("Pre-fire", "Post-fire"), times = c(nrow(tukey_pre_df), nrow(tukey_post_df))),
  Comparison = c(rownames(tukey_pre_df), rownames(tukey_post_df)),
  `Mean Difference` = c(tukey_pre_df$diff, tukey_post_df$diff),
  `Lower CI` = c(tukey_pre_df$lwr, tukey_post_df$lwr),
  `Upper CI` = c(tukey_pre_df$upr, tukey_post_df$upr),
  `p-value` = c(tukey_pre_df$`p adj`, tukey_post_df$`p adj`)
)


anova_results %>% 
  kbl(caption = "ANOVA Results Before and After Fire") %>%
  kable_classic(full_width = F, html_font = "Cambria")


tukey_results %>% 
  kbl(caption = "Tukey Test Results Before and After Fire") %>%
  kable_classic(full_width = F, html_font = "Cambria")


###################################
######1) GRAFICAS #########
###################################


# Modify the data
filtered_data$PrePost2 <- factor(filtered_data$PrePost, levels = c("Pre-fire", "Post-fire"), 
                                 labels = c("Pre-fire period", "Post-fire period"))

# plot
g<- filtered_data %>% filter(PrePost2 %in% c('Pre-fire period', 'Post-fire period')) %>% 
  ggplot(filtered_data, mapping = aes(x = Trat_1, y = porcentaje, fill = Trat_1)) +
  geom_boxplot(alpha = 0.75) +
  facet_wrap(~ PrePost2) +
  labs(title = "",
       x = "Treatment",
       y = "Snow cover (%)") +
  theme_bw()+
  # scale_fill_manual(values = c("gray80", "gray60", "gray40"))+
  scale_fill_manual(values = c("#237BB4", "#202021","#8C2A9F"))+
  theme(legend.position = "none")

g


ggsave("Figures/1_Fire_Pre_Post.jpg",g, units = "cm", width = 15, height = 7, dpi = 600)


###################################
######3) TEMPORAL ANALISIS########
###################################

#####################################
###2.a)RESUMEN PORCENTAJE POR PIXEL##
#####################################

datos <- read.delim("PlotPixels.csv", sep = ",")

# Filter the data to keep only the polygons of interest suit = 1 based on the first database
for_selection <- unique(df$polygon_id)

datos <- datos %>% filter(ID %in% for_selection)



## Summarize the binary NDSI values by treatment and period
df_y2 <- data %>%
  group_by(DATE_ACQUIRED, Trat_1) %>%
  mutate(snow_sum = sum(NDSI_bin, na.rm = TRUE)) %>%
  ungroup()  

## Exclude images without snow for all treatments
df_y3 <- df_y2 %>%
  group_by(DATE_ACQUIRED) %>%
  filter(sum(snow_sum[Trat_1 %in% c("SL", "PCL", "NI")]) != 0) %>%
  ungroup()

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


# Plot 
g3 <- tab %>% filter(PrePost %in% c('Pre-fire', 'Post-fire')) %>% 
  ggplot(tab, mapping = aes(x = Trat_1.x, y = porcent, fill = Trat_1.x)) +
  geom_boxplot(alpha = 0.75) +
  facet_wrap(~ PrePost2) +
  labs(title = "",
       x = "Treatment",
       y = "Percentage of snow occurrence (%)") +
  theme_bw()+
  scale_fill_manual(values = c("#237BB4", "#202021","#8C2A9F"))+
  # scale_fill_manual(values = c("gray80", "gray60", "gray40"))+
  theme(legend.position = "none")

g3

ggsave("Figures/Snow_Pixel_Level.jpg", g3, units = "cm", width = 20, height = 10, dpi = 300)


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
full.model <- lm(porcent ~., data = pre)

# Stepwise regression model
step.model.pre <- MASS::stepAIC(full.model, direction = "both", 
                            trace = FALSE)
summary(step.model.pre)
summary.aov(step.model.pre)


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
full.model <- lm(porcent ~., data = post)

# Stepwise regression model
step.model.post <- MASS::stepAIC(full.model, direction = "both", 
                            trace = FALSE)
summary(step.model.post)
summary.aov(step.model.post)

######################################
###2) Summarise table ##################
######################################

library(stargazer)
# Crear una tabla bien formateada para la publicación científica
stargazer(step.model.pre, step.model.post,
          title = "Resultados de la Regresión Lineal Stepwise",
          out = "resultados_regresion.txt",
          type = "text",
          column.labels = c("Pre-fire", "Post-fire"),
          dep.var.labels = "Porcentaje",
          covariate.labels = c("PCL", "SL", "Curvature", "Insolation", "Slope", "Orientation", "Shadows", "x", "y", "Intercept"),
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          digits = 3)


######################################
###3) RESUMEN CANTIDAD DE IMAGENES ####
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

library(ggpattern)

# Definir los colores
colores_barras <- c('Snow' = "lightblue4", 'NoSnow' = "orange4")
colores_fondo <- c("Pre-fire" = "grey5", "Post-fire" = "grey70", "Fire" = "red")

# Filtrar los datos
rgraph <- r %>% filter(PrePost %in% c('Pre-fire', 'Fire','Post-fire'))

# Crear la gráfica
g3<- ggplot() +
  # Rectángulos para los períodos
  geom_rect(data = distinct(rgraph, PrePost), 
            aes(xmin = Año - 0.5, xmax = Año + 0.5, ymin = 0, ymax = max(rgraph$escenes) * 2.1, fill = PrePost), 
            color = "black", size = 0.3, alpha = 0.2) +
  scale_fill_manual(name = "Period", values = colores_fondo, 
                    breaks = c("Pre-fire","Fire", "Post-fire"), 
                    labels = c("Pre-fire period", "Fire period","Post-fire period")) +
  ggnewscale::new_scale_fill() + 
  # Barras de datos apiladas
  geom_col(data = rgraph, aes(x = Año, y = escenes, fill = snow_fact)) +
  scale_fill_manual(name = "Condition", values = colores_barras, 
                    breaks = c("Snow", "NoSnow"), 
                    labels = c("Scenes with snow", "Scenes without snow")) +
  labs(x = "Year", y = "Number of scenes") +
  theme_bw() +
  theme(legend.position = "right", legend.box = "vertical")

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



