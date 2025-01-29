library(sf)
library(ggplot2)
library(readr)
library(tidyverse)
library(ggmap)
library(jpeg)
library(png)
library(patchwork)
library(cowplot)
library(usmap)

margin = 0.02
c_unflooded <- rgb(173,216,230,max = 255, alpha = 200, names = "lt.blue")
c_flooded <- rgb(255,192,203, max = 255, alpha = 200, names = "lt.pink")

img_unflooded = readJPEG("data/images/unflooded.jpg",native = TRUE)
img_flooded = readJPEG("data/images/flooded.jpg",native = TRUE)
img_bug = readPNG("data/images/bug.png",native = TRUE)

register_stadiamaps("e154c931-ba7a-4342-92ce-795c6c800d4a", write = FALSE)
site_data <- as.data.frame(read_csv("data/site_metadata.csv",show_col_types = FALSE))
huston_roads <- get_stadiamap(bbox= c(bottom = min(site_data$Lat)-margin,left = min(site_data$Lon)-margin,
                                  top= max(site_data$Lat)+margin, right = max(site_data$Lon)+margin),
                        zoom = 13, maptype = c("stamen_terrain_lines"),messaging = T)
huston_terrian <- get_stadiamap(bbox= c(bottom = min(site_data$Lat)-margin,left = min(site_data$Lon)-margin,
                                  top= max(site_data$Lat)+margin, right = max(site_data$Lon)+margin),
                        zoom = 10, maptype = c("stamen_terrain_background"),messaging = T)

dx = 0.11
dy = 0.2
x = -95.45
y = 29.37
sites = ggmap(huston_terrian)+ inset_ggmap(huston_roads)+
  geom_point(data =site_data, aes(x= Lon,y = Lat, fill = Condition),
             pch = 21, size = 5, col = "black")+
  annotate("text", x = -95.31, y = 29.750, label = "Houston", size = 5,fontface = "bold")+
  annotate("text", x = -95.61772, y = 29.78182, label = "*", size = 10)+
  geom_segment(aes(x = -95.65, y = 29.4, xend = -95.55, yend = 29.4))+
  geom_segment(aes(x = -95.65, y = 29.41, xend = -95.65, yend = 29.39))+
  geom_segment(aes(x = -95.55, y = 29.41, xend = -95.55, yend = 29.39))+
  geom_text(aes(y = 29.413, x = -95.605, label= "10 km"))+
  ylab("Latitude") + xlab("Longitude")+
  scale_fill_manual(values = as.character(c(c_unflooded, c_flooded)), 
                    name="Site Condition",
                    breaks=rev(c("Flooded", "Unflooded")),
                    labels=rev(c("Flooded", "Unflooded")))+
  annotation_raster(img_bug, ymin = y ,ymax= y + dy ,xmin = x-dx ,xmax = x+dx) +
  annotate("text", x = -95.515, y = 29.57, label = "Brachypterous \n form", size = 2.5)+
  annotate("text", x = -95.4, y = 29.57, label = "Macropterous  \n form", size = 2.5)+
  coord_cartesian() + theme(legend.position = c(0.8, 0.16), plot.margin = unit(c(0.2, 0.2, 0.2, 0.1), "cm"))


unflooded_site = ggplot() + geom_blank()+
  theme(panel.background = element_blank(),plot.margin = unit(c(0, 0, 0, 0), "cm"))+
 inset_element(p = img_unflooded,left = 0, bottom = 0, right = 1, top = 1)

flooded_site = ggplot() + geom_blank()+
  theme(panel.background = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  inset_element(p = img_flooded,left = 0, bottom = 0, right = 1, top = 1)

comparision = plot_grid(unflooded_site,flooded_site, nrow = 2, labels = c("B","C"),
                        hjust = -1.75, vjust = 2.75)
Fig1 = plot_grid(sites, comparision, ncol = 2, labels = c("A",""), rel_widths  = c(10,6),
                 hjust = -5.25, vjust = 2.75)
 plot(Fig1)

ggsave("Figures/Fig1/sites.pdf",Fig1, width = 18, height = 10.9 , units = c("cm"))






