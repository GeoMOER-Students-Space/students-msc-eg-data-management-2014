visualizer <- function (raster, vector, palname="Reds",lines=5,obs){
  NAT <- vector@data$NAT
  ID <- vector@data$ID
  ELEV <- na.omit(vector@data$ELEV)
  COVRG <-vector@data$COVRG
  AGR <- vector@data$AGR
  ANIMALS <- na.omit(vector@data$ANIMALS)
  
  vector_classes <- cut(vector@data$NAT, seq(min(obs),max(obs),length.out = 5))
  vector_colors <- colorRampPalette(brewer.pal(5,palname))(5)
  min <- max(mean(getValues(raster)) - sd(getValues(raster)), 0)
  max <- mean(getValues(raster)) + sd(getValues(raster))
  
  breaks <- seq(min, max, length.out = 256)
  yat = seq(extent(raster)@ymin, 
            extent(raster)@ymax, length.out = lines)
  xat = seq(extent(raster)@xmin, 
            extent(raster)@xmax, length.out = lines)
  scales = list(x = list(at = xat),
                y = list(at = yat))
  
  
  abb <- spplot(raster, col.regions = gray.colors(256), at = breaks,
                key = list(space = 'left', text = list(levels(vector_classes)), 
                           points = list(pch = 21, cex = 2, fill = vector_colors)),
                colorkey=list(space="right"),
                panel = function(...){
                  panel.levelplot(...)
                  panel.abline(h = yat, v = xat, col = "grey0", lwd = 0.8, lty = 3)
                }
  )
  Farben <- spplot(vector, zcol = "NAT", col.regions = vector_colors, cuts = seq(min(obs),max(obs),length.out = 5))
  
  abb + as.layer(Farben)
}

visualizer(landsat_Kanal3, Beob.Punkte, "Reds",obs=AGR)
