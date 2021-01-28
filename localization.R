
library(tidyverse)
library(grid)
library(maptools)
library(maps)
library(ggsn)
library(rgdal)
library(raster)

##### LOCALIZATION IN THE CONTEXT ####

# Background map
# See all regions available
sort(unique(ggplot2::map_data("world")$region))
europe <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany", "Andorra",
  "Austria", "Belgium", "UK", "Netherlands", "Albania", "Armenia",
  "Azerbaijan", "Belarus", "Bosnia and Herzegovina", "Bulgaria",
  "Croatia", "Cyprus", "Czechia", "Estonia", "Finland", "Georgia", "Greece",
  "Iceland", "Ireland", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein",
  "Lithuania", "Malta", "Moldova", "Monaco", "Montenegro", "Macedonia",
  "Norway", "Romania", "Russia", "San Marino", "Serbia", "Slovakia",
  "Slovenia", "Sweden", "Turkey", "Ukraine", "United Kingdom",
  "Vatican City",
  "Denmark", "Poland", "Italy", "Luxembourg",
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic",
  "Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia", "Western Sahara"
)
# Retrieve the map data
europe <- map_data("world", region = europe)

# Selected region to highlight in the background map
# A country
pi <- map_data("world", region = c("Spain","Portugal"))
pi <- pi[is.na(pi$subregion),] #Removing the islands (subregions)
ggplot(data = pi) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

# A subregion within a country
provsp <- readOGR("Examples_data/Provincias_ETRS89_30N.shp")  
barcelona <- provsp[provsp$Texto=="Barcelona",]

proj4string(barcelona) 
barna2 <- spTransform(barcelona,
                      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

barna2_forti <- fortify(barna2)

# A point
# 
# To do

# Complete map
map_back <- ggplot(europe, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "gray", color = "white") +
  coord_fixed(1.3) + 
  theme(legend.position = "none") +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.title = element_text(colour="black", face="bold", size = 12),
        legend.position = c(0.9,0.2), legend.text = element_text(size=10),
        axis.title = element_blank(),
        axis.text.x = element_text(colour="grey50", size=16),
        axis.text.y = element_text(colour="grey50", size=16),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour="grey90", size=0.5), 
        axis.line = element_blank(),
        panel.background = element_blank()) 

map_marked <- map_back + 
  geom_polygon(data = barna2_forti, aes(x = long, y = lat, group = group), color = "white", fill = "grey10") + 
  coord_fixed(xlim = c(-11, 30),  ylim = c(35, 60), ratio = 1.3) +
  theme(panel.border = element_rect(size=1, color="black", fill=NA))

#Additional mark
rectangle <- data.frame(long=c(1.5, 2.4, 2.4, 1.5, 1.5),
                        lat=c(41.72, 41.72, 41.3, 41.3, 41.72))
map_marked2 <-  map_marked + 
  geom_path(data=rectangle, aes(x = long,  y = lat),
            size=1, color="darkred", inherit.aes = TRUE)

ggsave(filename = "Result_figures/loc1.png", plot = map_marked2, 
       width = 130, height = 100, units = "mm", dpi = 300)


##### ESPECIFIC MAP OF THE STUDY SITE ####

# Load the data we want to represent (plots, raster, aerial photography, etc)
plots <- shapefile("Examples_data/ParcelasETRS8931.shp")
plots$'Forest type' <- factor(plots$CATEGORIA, levels=c("BP","BN","BMN"),
                              labels = c("Long-established", "Post-50", "Post-80"))
load("Examples_data/prec.Rdata")

proj4string(plots) 
plots <- spTransform(plots,
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plots_forti <- as.data.frame(plots)
plots_forti <- plots_forti %>% rename(long=coords.x1,
                      lat=coords.x2)

prec_barna <- mask(prec_wc.penin.masked, barna2) #Cut the background raster to the defined area extension
prec_spdf <- as(prec_barna, "SpatialPixelsDataFrame")
prec_forti <- as.data.frame(prec_spdf)
colnames(prec_forti) <- c("value", "long", "lat")


#Create the map
mymap <- ggplot() +
  geom_tile(data=prec_forti, aes(x=long, y=lat, fill=value), alpha=0.8) + 
  geom_point(data = plots_forti, #Delete color in aes if you want all points in the same color
             aes(x = long, y =lat, color=Forest.type),
             size = 1, alpha=0.8) +
  geom_polygon(data = barna2_forti, #Same region than in map_marked
               aes(x = long, y = lat, group = piece, fill = NA),
               color = "grey30", size = 1, fill = NA) +
  geom_path(data=rectangle, aes(x = long,  y = lat), #Sme than in map_marked2
            size=1, color="darkred", inherit.aes = TRUE) +
  coord_fixed(xlim = c(1.5, 2.4),  ylim = c(41.3, 41.72), ratio = 1.3) +
  scale_fill_gradient(name="Annual\nprecipitation\n(mm)",
                      low = 'lightcyan', high = 'steelblue4',
                      na.value = NA, limits=c(550,920)) +
  scale_color_manual(name="Forest type",
                     values = c("grey50","chartreuse3","darkolivegreen1")) +
  xlab("Longitude") + ylab("Latitude") + 
  theme(legend.title = element_text(colour="black", face="bold", size = 12),
        # legend.position="none", #To remove the legends
        # legend.position = c(0.85,0.15), legend.text = element_text(size=10),
        axis.title.x = element_text(colour="grey50", size = 10,
                                    margin = margin(t = 15, r = 0, b = 0, l = 0),
                                    hjust = 1),
        axis.text.x = element_text(colour="grey50", size=8),
        axis.title.y = element_text(colour="grey50", size=10, angle = 90,
                                    margin = margin(t = 0, r = 15, b = 0, l = 0),
                                    hjust=0),
        axis.text.y = element_text(colour="grey50", size=8),
        panel.grid.major = element_line(colour="grey90", size=0.5), 
        axis.line = element_blank(),
        panel.background = element_blank()) 

northSymbols()
mymap2 <- mymap + #Adding scalebar and North arrow
  ggsn::scalebar(x.min=2.15, x.max = 2.35,
                 y.min = 41.31, y.max = 41.327,
                 dist_unit = "km", dist = 10, height = 0.7,
                 st.bottom = FALSE, st.dist = 0.5, st.size = 3,
                 transform = TRUE, model = "WGS84",
                 st.color = "black", box.fill = c("black","white"), 
                 box.color = "black", border.size = 0.6) +
  north(plots_forti, location = "bottomright", scale = 0.2, symbol = 1) 
  # annotate("segment", x = 2.3, xend = 2.3, y = 41.4, yend= 41.45, size=3, #Home-made north symbol
  #          arrow = arrow(ends = "last", angle = 15, length = unit(.3,"cm"), type = "open")) +
  # annotate("text", x = 2.3, y= 41.48, hjust=0.5,  
  #          label = "N", size=4) 

ggsave(filename = "Result_figures/study_area.png", plot = mymap2, width = 200, height = 170, 
       units = "mm", dpi = 300)

#### COMPOSTION ####

library(magick)

loc <- image_read("Result_figures/loc1.png")
loc <- image_border(loc, "black", "10x10")

studyarea <- image_read("Result_figures/study_area.png") 



plot <- image_composite(studyarea, image_scale(loc, "x400"), 
                        offset = "+295+475")
plot_crop <- image_trim(plot)

image_write(plot_crop, "Result_figures/map1.png", quality = 100)

