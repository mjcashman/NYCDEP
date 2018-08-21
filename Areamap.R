library(tidyverse)
library(readxl)
library(viridis)
library(ggpubr)
library(FedData)
suppressPackageStartupMessages(library(ggmap)) # ggplot functionality for maps
library(ggsn) # for scale bars/north arrows in ggplots
library(maps)
library(mapdata)
library(rasterVis)
suppressPackageStartupMessages(library(cowplot)) ## for arranging ggplots

#rm(list=ls())

# Load raw target and source data ----
directory <- "Y:/Access Databases/Catskills/Fingerprinting/"
palette <- c("#00AFBB", "#E7B800", "#FC4E07")
title <- "Raw Values"

target_values<-read.csv(paste0(directory,"TargetSamples_NDisRL_useMe.csv")) %>%
  mutate(Type = "ISCO")
target_info<-read.csv(paste0(directory,"TargetSamples_Info.csv"))
target <- inner_join(target_info,target_values) %>% dplyr::rename(lon = Long,lat = Lat) %>%
  mutate(lon = lon*-1)

source_values<-read.csv(paste0(directory,"SourceSamples_noRoad.csv")) 
source_info<-read.csv(paste0(directory,"SourceSamples_Info.csv"))
source <- inner_join(source_info,source_values) %>% dplyr::rename(lon = Long,lat = Lat) 

source <- source %>%
  mutate(Type=recode(Type, "Upper_Lacustrine" = "Lacustrine",
                     "Lower_Lacustrine" = "Lacustrine",
                     "Glacial_Till" = "Glacial Till",
                     "Bank_Alluvium" = "Alluvium",
                     "Terrace_Alluvium" = "Alluvium"))

#Calculate Bounding Box and Download NED and NHD ----

sbbox <- make_bbox(lon = source$lon, lat = source$lat, f = .1)
vepPolygon <- polygon_from_extent(raster::extent(c(sbbox[1],sbbox[3],sbbox[2],sbbox[4])), 
                                  proj4string='+init=EPSG:4326')

# Get NHD and NED (USA ONLY)
#NHD <- get_nhd(template=vepPolygon, label='VEPIIN', force.redo = TRUE)
#NED <- get_ned(template = vepPolygon, label = "VEPIIN",force.redo = TRUE)

# Plot the NHD data
slope = terrain(NED, opt='slope')
aspect = terrain(NED, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Switzerland')
plot(NED, col=rainbow(25, alpha=0.35), add=TRUE)

#   Convert rasters TO dataframes for plotting with ggplot
hdf <- rasterToPoints(hill); hdf <- data.frame(hdf)
colnames(hdf) <- c("X","Y","Hill")
ddf <- rasterToPoints(NED); ddf <- data.frame(ddf)
colnames(ddf) <- c("X","Y","DEM")

#   Create vectors for colour breaks
b.hs <- seq(min(hdf$Hill),max(hdf$Hill),length.out=100)
b.dem <- seq(min(ddf$DEM),max(ddf$DEM),length.out=100)


#Plot both together with transparency with DEM layer----
library(scales)
cols<-c(#"#06407F", "#317A9D","#4ABEBB",
  #"#40AE89", "#467B5D","#3C6D4D", "#1A572E", "#034C00", "#045D03", "#6C975F", "#6B823A", "#88A237",
  "#C5D16B", "#DDE580", "#FFF6AE", "#FBCB81", "#F0B16A",
  "#F2B16D", "#D18338", "#B16F33", "#825337")#, "#66422A", "#4F2C0C")
custom_cols <- c("#33602a","#75bd65","#c5e3be","#fff8eb")
green_pal <- c("#33602a",  "#3d7231","#468439", "#4f9541",  "#59a748", "#65b554","#75bd65","#85c477","#95cc89","#a5d49a","#b5dbac","#c5e3be","#d5ebd0","#e5f2e2","#f5faf3")
col <- terrain.colors(5)
colfunc <- colorRampPalette(green_pal)



#   Try to plot both together with transparency on the DEM layer
#Both Basin Map
ggplot(hdf) +
  geom_raster(data=ddf,aes(X,Y,fill=DEM,alpha=0.1)) +
  scale_fill_gradientn(name="Altitude",colours = rev(colfunc(100))) +
  geom_raster(aes(X,Y,alpha=Hill), fill = "grey20") +
  scale_alpha(range = c(1, 0)) +
  coord_equal()+
  guides(alpha = FALSE) +
  geom_polygon(data=fortify(NHD$`Area`), aes(x=long, y=lat, group=group),color="steelblue1",fill="steelblue1") +
  geom_polygon(data=fortify(NHD$`Waterbody`), aes(x=long, y=lat, group=group),fill="lightblue",color="lightblue") +
  geom_point(data=gageGPS, aes(x=dec_long_va, y=dec_lat_va), fill= "black", pch=23, size=4) +
  geom_point(data=source,  aes(x=lon, y = lat, group=Type, color=Type), size=4)+
  #scale_color_brewer(palette="Dark2")+ 
  scalebar(dist = 2, dd2km = TRUE, model  = "WGS84", y.min=41.99, y.max=42.17, x.min=-74.4, x.max=-74.2, anchor=c(x=-74.39, y=42.16), location="topleft")+
  north(y.min=41.99, y.max=42.17,   x.min=-74.4, x.max=-74.2, scale=0.1, symbol=14,   location="bottomright")+
  theme_void()
ggsave("Output/WholeSourcemap.png")

#SCC Map
SCC_Source<-subset(source,Basin=="Stony Clove Creek")
sbbox <- make_bbox(lon = SCC_Source$lon, lat = SCC_Source$lat, f = .1)
ggplot(hdf) +
  geom_raster(data=ddf,aes(X,Y,fill=DEM,alpha=0.2),show.legend = FALSE) +
  scale_fill_gradientn(name="Altitude",colours = rev(colfunc(100))) +
  geom_raster(aes(X,Y,alpha=Hill), fill = "grey20") +
  scale_alpha(range = c(1, 0)) +
  coord_equal(xlim = c(sbbox[[1]],sbbox[[3]]),ylim = c(sbbox[[2]],sbbox[[4]]))+
  guides(alpha = FALSE) +
  geom_polygon(data=fortify(NHD$`Area`), aes(x=long, y=lat, group=group),color="steelblue1",fill="steelblue1") +
  geom_polygon(data=fortify(NHD$`Waterbody`), aes(x=long, y=lat, group=group),fill="lightblue",color="lightblue") +
  geom_point(data=gageGPS, aes(x=dec_long_va, y=dec_lat_va), fill= "black", pch=23, size=4) +
  geom_point(data=source,  aes(x=lon, y = lat, group=Type, color=Type), size=4)+
  scale_color_brewer(palette="Dark2")+ 
  scalebar(dist = 1, dd2km = TRUE, model  = "WGS84", y.min=sbbox[[2]], y.max=sbbox[[4]], x.min=sbbox[[1]], x.max=sbbox[[3]], 
           anchor=c(x=sbbox[[3]]-0.02, y=sbbox[[2]]), location="bottomright")+
  north(y.min=sbbox[[2]], y.max=sbbox[[4]], x.min=sbbox[[1]], x.max=sbbox[[3]], scale=0.1, symbol=14, 
        anchor=c(x=sbbox[[3]]-0.025, y=sbbox[[2]]+0.005), location="bottomright")+
  theme_void()+
#  ggtitle("Stony Clove Creek Source Collections")+
  theme(legend.position = c(.9, .15), legend.background = element_rect(fill=alpha("white",0.8),color=alpha("white",0.8)))
ggsave("Output/SCCSourceMap.png")

#Woodland Map
Wood_Source<-subset(source,Basin=="Woodland Creek")
sbbox <- make_bbox(lon = Wood_Source$lon, lat = Wood_Source$lat, f = .1)
sbbox[1]<- -74.4
sbbox[2]<- 41.99
#sbbox[3]<- -74.3
sbbox[4]<- 42.09

ggplot(hdf) +
  geom_raster(data=ddf,aes(X,Y,fill=DEM,alpha=0.2),show.legend = FALSE) +
  scale_fill_gradientn(name="Altitude",colours = rev(colfunc(100))) +
  geom_raster(aes(X,Y,alpha=Hill), fill = "grey20") +
  scale_alpha(range = c(1, 0)) +
  guides(alpha = FALSE) +
  geom_polygon(data=fortify(NHD$`Area`), aes(x=long, y=lat, group=group),color="steelblue1",fill="steelblue1") +
  geom_polygon(data=fortify(NHD$`Waterbody`), aes(x=long, y=lat, group=group),fill="lightblue",color="lightblue") +
  geom_point(data=gageGPS, aes(x=dec_long_va, y=dec_lat_va), fill= "black", pch=23, size=4) +
  geom_point(data=source,  aes(x=lon, y = lat, group=Type, color=Type), size=4)+
  scale_color_brewer(palette="Dark2")+ 
  coord_equal(xlim = c(sbbox[[1]],sbbox[[3]]),ylim = c(sbbox[[2]],sbbox[[4]]))+
  scalebar(dist = 1, dd2km = TRUE, model  = "WGS84", y.min=sbbox[[2]], y.max=sbbox[[4]], x.min=sbbox[[1]], x.max=sbbox[[3]], 
           anchor=c(x=sbbox[[1]]+0.005, y=sbbox[[2]]+0.004), location="bottomleft")+
  north(y.min=sbbox[[2]], y.max=sbbox[[4]], x.min=sbbox[[1]], x.max=sbbox[[3]], scale=0.1, symbol=14, 
        anchor=c(x=sbbox[[1]]+0.012, y=sbbox[[2]]+0.006), location="bottomleft")+
  theme_void()+
  #  ggtitle("Stony Clove Creek Source Collections")+
  theme(legend.position = c(.88, .15), legend.background = element_rect(fill=alpha("white",0.8),color=alpha("white",0.8)))
ggsave("Output/WoodlandSourceMap.png")


# Map datasets onto google map outputs ----
# 
# directory <- "Y:/Access Databases/Catskills/Fingerprinting/"
# 
# 
# source_values<-read.csv(paste0(directory,"SourceSamples_noRoad.csv")) 
# source_info<-read.csv(paste0(directory,"SourceSamples_Info.csv"))
# source <- inner_join(source_info,source_values) %>% dplyr::rename(lon = Long,lat = Lat) 
# 
# source <- source %>%
#   mutate(Type=recode(Type, "Upper_Lacustrine" = "Lacustrine",
#                      "Lower_Lacustrine" = "Lacustrine",
#                      "Bank_Alluvium" = "Alluvium",
#                      "Terrace_Alluvium" = "Alluvium"))
# 
# library(ggmap)
# sbbox <- make_bbox(lon = source$lon, lat = source$lat, f = .1)
# sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")
# 
# library(dataRetrieval)
# 
# SCC_Chichester <- '01362370'
# SCC_Janssen <- '01362336' 
# OxClove <- '01362368'
# Warner <- '01362357'
# Woodland <-  '0136230002' 
# 
# sitelist<- c(OxClove, SCC_Chichester, SCC_Janssen, Warner, Woodland)
# pCodes <- c("00060","63680") 
# start <- "2017-10-29"
# end <- "2017-11-4"
# 
# readNWISsite(sitelist)
# 
# gageGPS<-readNWISsite(sitelist)[,c(2,3,7,8)]
# 
# 
# ggmap(sq_map) +
#   #geom_scatterpie(data = target, aes(x = lon, y = lat, r = 0.005), cols=colnames(target[16:19]),color="NA")+
#   #geom_text(data = source, aes(label = paste("  ", as.character(SampleName), sep="")), position=position_jitter(), angle = 30, hjust = 0, size = 3, color = "black")
#   #scale_color_viridis(discrete=TRUE) +
  # geom_point(data = source,  mapping = aes(x = lon, y = lat, fill=Type), color="black", pch=21, size=4) +
  # geom_point(data=gageGPS, aes(x=dec_long_va, y=dec_lat_va), fill= "black", pch=23, size=4) #+
  # scalebar(dist = 2, dd2km = TRUE, model  = "WGS84", y.min=41.99, y.max=42.17, x.min=-74.4, x.max=-74.2, anchor=c(x=-74.39, y=42.16), location="topleft")+
  # north(y.min=41.99, y.max=42.17,   x.min=-74.4, x.max=-74.2, scale=0.1, symbol=14,   location="bottomright")+
  # scale_fill_brewer(palette = "Set1")+ coord_equal()

