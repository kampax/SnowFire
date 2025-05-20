# Load libraries
library(tidyverse)
library(plotly)
library(ggpmisc)


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


ggplotly(g)

# Save the plot as a PNG file
ggsave("../Figures/SnowCover_Skiable_tracks.png", g, units = "cm", width = 19, height = 10, dpi = 300)


#################################
####### END OF SCRIPT ###########
#################################