#Figure2. Spatial distribution of permanent plots####
library(tidyverse)
library(ggplot2) 
library(cowplot)
library(gridExtra)
library(grid)
library(reshape2)
library(rgdal)
library(viridis)
library(viridisLite)
library(magick)
library(ggalt)
library(ggsignif)
library(sp) 
library(rgeos)
library(FRK)
library(ggsn)

plot_clima <- read.csv("Examples_data/plot_clima.csv")

names(plot_clima)
plots <- dplyr::select(plot_clima, plotcode, tipo,
                CX, CY)

# I delete 5 plots with incorrect coordinates
plots <- filter(plots, CY > "0")
View(plots)

# Iberian peninsula contour
pib <- c("Portugal", "Spain")
pib <- map_data("world", region = pib) #Coords en WGS84

pib_contour <- pib[!pib$subregion %in%
                     c("Formentera", "Ibiza", "Majorca", "Minorca"),] # I delete Baleares

# I dissolve the borders between Spain and Portugal
# I use FRK package to change from dataframe to spatialpolygon
pib_contour <- df_to_SpatialPolygons(pib_contour, "group", c("long","lat"), CRS())

library(raster) # Open raster package here because some objects are masked with other packages
crs(pib_contour) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# I use rgeos to dissolve the borders
pib_contour2 <- gUnaryUnion(pib_contour)
plot(pib_contour2, border = 'black')

pib_contour2 <- fortify(pib_contour2)

# I create the base map of Spain and Portugal
base_map <- ggplot() + geom_polygon(data = pib_contour2,
                                    aes(x = long, y = lat, fill = NA, group=group),
                                    color = "black", size = 0.5, fill = NA) + 
  coord_fixed(1.3)

# I add the points to the base map
coords.ifn <- CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs") #ETRS89 30T
coordinates(plots) <- c("CX", "CY")
crs(plots) <- coords.ifn
plots <- spTransform(plots, "+proj=longlat +ellps=WGS84 +datum=WGS84")
plots <- as.data.frame(plots)
plot(plots$CX, plots$CY) 

base_plots <- base_map + geom_point(data = plots, aes(x = CX, y = CY),
                                    color = alpha("black",0.1), size = 0.5)

# I change the axis and the background
base_plots2 <- base_plots +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.title = element_text(colour="black", face="bold", size = 14),
        axis.title.x = element_text(colour="black", size = 12,
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(colour="black"),
        axis.title.y = element_text(colour="black", size=12, angle = 90,
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(colour="grey90", size=0.5), 
        axis.line = element_blank(),
        panel.background = element_blank()) 

base_plots2 

# I divide plots into regions
# I filter plots starting with a certain value
# Araba
start_1 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 1) %>%
  filter(plotcode %in% (10009:11481))
start_1_plots <- base_map + geom_point(data = start_1, aes(x = CX, y = CY),
                                       color = "red", size = 0.5)

# Badajoz
start_10 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 1) %>%
  filter(plotcode %in% (100004:103523))
start_10_plots <- base_map + geom_point(data = start_10, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# La Coruna
start_15 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 1) %>%
  filter(plotcode %in% (150014:153361))
start_15_plots <- base_map + geom_point(data = start_15, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Girona
start_17 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 1) %>%
  filter(plotcode %in% (170004:172895))
start_17_plots <- base_map + geom_point(data = start_17, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Gipuzkoa
start_20 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 2) %>%
  filter(plotcode %in% (200003:201195))
start_20_plots <- base_map + geom_point(data = start_20, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Lleida
start_25 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 2) %>%
  filter(plotcode %in% (250001:253347))
start_25_plots <- base_map + geom_point(data = start_25, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Rioja
start_26 <- plots %>% dplyr::filter(substr(plotcode,1,1) == 2) %>%
  filter(plotcode %in% (260001:261272))
start_26_plots <- base_map + geom_point(data = start_26, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Lugo
start_27 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 2) %>%
  filter(plotcode %in% (270001:272930))
start_27_plots <- base_map + geom_point(data = start_27, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Madrid
start_28 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 2) %>%
  filter(plotcode %in% (280004:282561))
start_28_plots <- base_map + geom_point(data = start_28, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Murcia
start_30 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 3) %>%
  filter(plotcode %in% (300003:302104))
start_30_plots <- base_map + geom_point(data = start_30, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Orense
start_32 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 3) %>%
  filter(plotcode %in% (320010:322037))
start_32_plots <- base_map + geom_point(data = start_32, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Pontevedra
start_36 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 3) %>%
  filter(plotcode %in% (360003:361678))
start_36_plots <- base_map + geom_point(data = start_36, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Tarragona
start_43 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 4) %>%
  filter(plotcode %in% (430003:431523))
start_43_plots <- base_map + geom_point(data = start_43, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Bizkaia
start_48 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 4) %>%
  filter(plotcode %in% (480001:481279))
start_48_plots <- base_map + geom_point(data = start_48, aes(x = CX, y = CY),
                                        color = "red", size = 0.5)

# Caceres
start_6 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 6) %>%
  filter(plotcode %in% (60002:62698))
start_6_plots <- base_map + geom_point(data = start_6, aes(x = CX, y = CY),
                                       color = "red", size = 0.5)

# Barcelona
start_8 <- plots %>% 
  dplyr::filter(substr(plotcode,1,1) == 8) %>%
  filter(plotcode %in% (80001:83576))
start_8_plots <- base_map + geom_point(data = start_8, aes(x = CX, y = CY),
                                       color = "red", size = 0.5)

# All
# all <- base_map + 
#   geom_point(data = start_1, aes(x = CX, y = CY),
#              color = "red", size = 0.5) + 
#   geom_point(data = start_10, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_15, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_17, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_20, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_25, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_26, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_27, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_28, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_30, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_32, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_36, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_43, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_48, aes(x = CX, y = CY),
#            color = "red", size = 0.5) + 
#   geom_point(data = start_6, aes(x = CX, y = CY),
#                       color = "red", size = 0.5) + 
#   geom_point(data = start_8, aes(x = CX, y = CY),
#                       color = "red", size = 0.5)

# devtools::install_github("hrbrmstr/ggalt")
# I bind them by regions
PVRI <- bind_rows(start_1,start_20, start_48, start_26)
PVRI$region <- "PVRI"

Cat <- bind_rows(start_8,start_43, start_17, start_25)
Cat$region <- "Cat"

Ext <- bind_rows(start_6,start_10)
Ext$region <- "Ext"

Gal <- bind_rows(start_36,start_32, start_27, start_15)
Gal$region <- "Gal"

Mur <- bind_rows(start_30)
Mur$region <- "Mur"

Mad <- bind_rows(start_28)
Mad$region <- "Mad"

all_regions <- bind_rows(PVRI,Cat, Ext, Gal, Mur, Mad)
all_regions$region <- as.factor(all_regions$region)

# I load precipitation raster (see prec_Elena.R)
# and convert it to dataframe
load("Examples_data/prec_wc.penin.masked.Rdata")
# prec_spain <- mask(prec_wc.penin.masked, Spain)
prec_spain <- prec_wc.penin.masked
prec_spdf <- as(prec_spain, "SpatialPixelsDataFrame")
prec_df <- as.data.frame(prec_spdf)
colnames(prec_df) <- c("value", "x", "y")

# base map
library(ggsn)
regions_circled <- ggplot() +
  geom_tile(data=prec_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data = pib_contour2,
               aes(x = long, y = lat, fill = NA, group=group),
               color = "black", size = 1, fill = NA) + 
  geom_point(data = all_regions, aes(x = CX, y = CY),
             size = 0.1, colour = "black", alpha = 0.2) +
  geom_encircle(data=all_regions, aes(x=CX, y=CY, group = region), 
                size=1, 
                expand=0.01, linetype = 2) +
  scale_fill_gradient("Annual\nprecipitation\n(mm)",
                      low = 'lightcyan', high = 'steelblue3',
                      na.value = NA) +
  xlab("Longitude") + ylab("Latitude") + 
  theme(legend.title = element_text(colour="black", face="bold", size = 12),
        legend.position = c(0.92,0.15), legend.text = element_text(size=10),
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
 + ggsn::scalebar(x.min = 1.2, x.max = 3.2,
         y.min = 38.3, y.max = 39.6,
         dist = 100, dist_unit = "km", height = 0.1,
         st.bottom = FALSE, st.dist = 0.15, st.size = 4.3,
         transform = TRUE, model = "WGS84",
         st.color = "black", box.fill = c("black","white"), box.color = #"black")



regions_circled2 <- regions_circled + 
  annotate("text", x = c(-9.1,-9.1,-9.1), y= c(36.5, 35.9, 35.4), hjust=0,  
           label = c("Broad-leaved\ndeciduous",
                     "Broad-leaved evergreen", "Needle-leaved evergreen")) +
  annotate("segment", x = 3, xend = 3, y = 39.5, yend= 40, size=3.5,
           arrow = arrow(ends = "last", angle = 30, length = unit(.3,"cm"), type = "open")) +
  annotate("text", x = 3, y= 39.2, hjust=0.5,  
           label = "N", size=4) 

ggsave(filename = "./Plots/base_map.png", plot = regions_circled2, width = 200, height = 170, 
       units = "mm", dpi = 300)

# I create the donut charts
# Percentage of each type of forest in each region
BLDEC <- all_regions %>% group_by(region) %>%
  summarize(BLDEC = mean(tipo == "BLDEC")*100) %>% 
  mutate(BLDECr = round(BLDEC, 0))

BLEVE <- all_regions %>% group_by(region) %>%
  summarize(BLEVE = mean(tipo == "BLEVE")*100) %>% 
  mutate(BLEVEr = round(BLEVE, 0))

NLEVE <- all_regions %>% group_by(region) %>%
  summarize(NLEVE = mean(tipo == "NLEVE")*100) %>% 
  mutate(NLEVEr = round(NLEVE, 0))

percent <- inner_join(BLDEC, BLEVE, by = "region") %>%
  inner_join(NLEVE, by = "region")

percent_gather <- dplyr::select(percent, region, BLDECr, BLEVEr, NLEVEr) %>%
  gather(key = "TIPO", value = "value",
         BLDECr, BLEVEr, NLEVEr)

levels(percent$region)
Cat_per <- subset(percent_gather, region == "Cat")
Ext_per <- subset(percent_gather, region == "Ext")
Gal_per <- subset(percent_gather, region == "Gal")
Mad_per <- subset(percent_gather, region == "Mad")
Mur_per <- subset(percent_gather, region == "Mur")
PVRI_per <- subset(percent_gather, region == "PVRI")

# Cataluna
# Add label position
names(Cat_per)
Cat_per <- Cat_per %>%
  arrange(desc(TIPO)) %>%
  mutate(lab.ypos = cumsum(value) - 0.5*value)
Cat_per

# Pie chart
Pie_Cat <- ggplot(Cat_per, aes(x = "", y = value, fill = TIPO)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = value), color = "black")+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank())

# Donut chart
Don_Cat <- ggplot(Cat_per, aes(x = 2, y = value, fill = TIPO)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = value), color = "black", size=18)+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) +
  xlim(0.5, 2.5)


# Extremadura
# Add label position
names(Ext_per)
Ext_per <- Ext_per %>%
  arrange(desc(TIPO)) %>%
  mutate(lab.ypos = cumsum(value) - 0.5*value)
Ext_per

# Pie chart
Pie_Ext <- ggplot(Ext_per, aes(x = "", y = value, fill = TIPO)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = value), color = "black")+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank())

# Donut chart
Don_Ext <- ggplot(Ext_per, aes(x = 2, y = value, fill = TIPO)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = value), color = "black", size=18)+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) +
  xlim(0.5, 2.5)

# Galicia
# Add label position
names(Gal_per)
Gal_per <- Gal_per %>%
  arrange(desc(TIPO)) %>%
  mutate(lab.ypos = cumsum(value) - 0.5*value)
Gal_per

# Pie chart
Pie_Gal <- ggplot(Gal_per, aes(x = "", y = value, fill = TIPO)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = value), color = "black")+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank())

# Donut chart
Don_Gal <- ggplot(Gal_per, aes(x = 2, y = value, fill = TIPO)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = value), color = "black", size=18)+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) +
  xlim(0.5, 2.5)

# Madrid  
# Add label position
names(Mad_per)
Mad_per <- Mad_per %>%
  arrange(desc(TIPO)) %>%
  mutate(lab.ypos = cumsum(value) - 0.5*value)
Mad_per

# Pie chart
Pie_Mad <- ggplot(Mad_per, aes(x = "", y = value, fill = TIPO)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = value), color = "black")+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank())

# Donut chart
Don_Mad <- ggplot(Mad_per, aes(x = 2, y = value, fill = TIPO)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = value), color = "black", size=18)+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) +
  xlim(0.5, 2.5)


# Murcia
# Add label position
names(Mur_per)
Mur_per <- Mur_per %>%
  arrange(desc(TIPO)) %>%
  mutate(lab.ypos = cumsum(value) - 0.5*value)
Mur_per

# Pie chart
# when rounding bldec is 0, I delete it to draw
Mur_per <- Mur_per[-3,]
Pie_Mur <- ggplot(Mur_per, aes(x = "", y = value, fill = TIPO)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = value), color = "black")+
  scale_fill_manual(values = c("yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank())

# Donut chart
Don_Mur <- ggplot(Mur_per, aes(x = 2, y = value, fill = TIPO)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = value), color = "black", size=18)+
  scale_fill_manual(values = c("yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) +
  xlim(0.5, 2.5)

# Pais Vasco + Rioja
# Add label position
names(PVRI_per)
PVRI_per <- PVRI_per %>%
  arrange(desc(TIPO)) %>%
  mutate(lab.ypos = cumsum(value) - 0.5*value)
PVRI_per

# Pie chart
Pie_PVRI <- ggplot(PVRI_per, aes(x = "", y = value, fill = TIPO)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = value), color = "black")+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank())

# Donut chart
Don_PVRI <- ggplot(PVRI_per, aes(x = 2, y = value, fill = TIPO)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = value), color = "black", size=18)+
  scale_fill_manual(values = c("goldenrod2","yellowgreen","forestgreen")) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) +
  xlim(0.5, 2.5)

# I save them
ggsave(filename = "./Plots/Don_Cat.png", plot = Don_Cat,
       width = 200, height = 140, units = "mm",
       dpi = 300, bg = "transparent")
ggsave(filename = "./Plots/Don_Ext.png", plot = Don_Ext,
       width = 200, height = 140, units = "mm",
       dpi = 300, bg = "transparent")
ggsave(filename = "./Plots/Don_Gal.png", plot = Don_Gal,
       width = 200, height = 140, units = "mm",
       dpi = 300, bg = "transparent")
ggsave(filename = "./Plots/Don_Mad.png", plot = Don_Mad,
       width = 200, height = 140, units = "mm",
       dpi = 300, bg = "transparent")
ggsave(filename = "./Plots/Don_Mur.png", plot = Don_Mur,
       width = 200, height = 140, units = "mm",
       dpi = 300, bg = "transparent")
ggsave(filename = "./Plots/Don_PVRI.png", plot = Don_PVRI,
       width = 200, height = 140, units = "mm",
       dpi = 300, bg = "transparent")

# Composition of base map + donuts
plot <- image_read("./Plots/base_map.png") 
offset_x <- c(300,1415,1980,1190,350,1125)
offset_y <- c(490,50,525,550,1050,1190)
donut <- c("Don_Gal","Don_PVRI","Don_Cat","Don_Mad","Don_Ext","Don_Mur")


for (i in 1:6) { 
  nombre <- paste0("./Plots/", donut[i], ".png")
  don <- image_read(nombre)
  offset_v <- paste0("+",offset_x[i],"+",offset_y[i])
  plot <- image_composite(plot, image_scale(don, "x250"), offset = offset_v)
}


# I add the leaves of the legend
offset_x <- c(265,265,265)
offset_y <- c(1440,1590,1670)
hojas <- c("falling oak leaf.png","evergreen leaf.png","pine needles.png")
col <- c("goldenrod2","yellowgreen","forestgreen")


for (i in 1:3) { 
  nombre <- paste0("./Plots/", hojas[i])
  tipo <- image_read(nombre)
  tipo <- image_fill(tipo, col[i], point = "+250+250", fuzz = 100)
  offset_v <- paste0("+",offset_x[i],"+",offset_y[i])
  plot <- image_composite(plot, image_scale(tipo, "x110"), offset = offset_v)
}

# I create the location map
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
# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- europe %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))  

pi <- map_data("world", region = c("Spain","Portugal"))
pi <- pi[is.na(pi$subregion),] #Quito las islas (subregiones)
ggplot(data = pi) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

# PI in Europe
gg_europe <- ggplot(europe, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "gray", color = "white") +
  coord_fixed(1.3) + 
  theme(legend.position = "none") +
  geom_polygon(data = pi, aes(x = long, y = lat, group = group), color = "white", fill = "grey10") + # get the state border back on top
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
# panel.border = element_rect(size=1, color="black", fill=NA))

gg_spain <- gg_europe + 
  coord_fixed(xlim = c(-11, 30),  ylim = c(35, 60), ratio = 1.3) 

ggsave(filename = "./Plots/loc1.png", plot = gg_spain, 
       width = 130, height = 100, units = "mm", dpi = 300)


# I add the location map
loc <- image_read("./Plots/loc1.png")
loc <- image_border(loc, "black", "10x10")
plot <- image_composite(plot, image_scale(loc, "x400"), 
                        offset = "+1480+1400")

image_write(plot, "./Plots/figure2.png")
