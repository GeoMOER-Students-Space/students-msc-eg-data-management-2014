#Verzeichnisse öffnen, Packages laden und instllieren, Raster einlesen
setwd ("C:/Users/Nena/Desktop/Uni-Master/1-Semester/Data-Management/Daten/dm-ws-08-01/")

#field_survey <- read.table("fogo_field_survey_2014/plots_veg_anm_geo_2014.csv", header = TRUE,
 #                          sep = ",")

if (!require(sp)){install.packages('sp')}
if (!require(RColorBrewer)){install.packages('RColorBrewer')}
if (!require(raster)){install.packages('raster')}
if (!require(rgdal)){install.packages('rgdal')}
if (!require(latticeExtra)){install.packages('latticeExtra')}

library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)
library(latticeExtra)



#field_survey <- read.table("fogo_field_survey_2014/plots_veg_anm_geo_2014.csv", header = TRUE,
#                           sep = ",")

#landsat_thermo <- raster("fogo_landsat_2014-11-24/LC82100502014328LGN00_B10.tif")
landsat_Kanal3 <- raster("fogo_landsat_2014-11-24/LC82100502014328LGN00_B3.tif")
plot(landsat_Kanal3)


#1

Beob <- readOGR("C:/Users/Nena/Desktop/Uni-Master/1-Semester/Data-Management/Daten/de-ws-10-01/Anhänge_2015128/data_2014_subset1.shp", "data_2014_subset1")
Beob <- spTransform(Beob, CRS(projection(landsat_Kanal3)))
Beob.Punkte <- Beob
plot(Beob.Punkte)
plot(Beob)

a <- function (raster, vector, palname="Reds"){
  vector_classes <- cut(vector@data$NAT, c(0, 2, 4, 6, 8, 10 ))
  vector_colors <- colorRampPalette(brewer.pal(4,palname))(5)
  min <- max(mean(getValues(raster)) - sd(getValues(raster)), 0)
  max <- mean(getValues(raster)) + sd(getValues(raster))
  
  breaks <- seq(min, max, length.out = 256)
  yat = seq(extent(raster)@ymin, 
            extent(raster)@ymax, length.out = 5)
  xat = seq(extent(raster)@xmin, 
            extent(raster)@xmax, length.out = 5)
  
  
  
  abb <- spplot(raster, col.regions = gray.colors(256), at = breaks,
                key = list(space = 'left', text = list(levels(vector_classes)), 
                           points = list(pch = 21, cex = 2, fill = vector_colors)),
                colorkey=list(space="right"),
                panel = function(...){
                  panel.levelplot(...)
                  panel.abline(h = yat, v = xat, col = "grey0", lwd = 0.8, lty = 3)
  
                }
  )
  
  Farben <- spplot(vector, zcol = "NAT", col.regions = vector_colors, 
                cuts = c(0, 2, 4, 6, 8, 10 ))
  
  abb + as.layer(Farben)
}


a(landsat_Kanal3, Beob.Punkte, "Greens")

