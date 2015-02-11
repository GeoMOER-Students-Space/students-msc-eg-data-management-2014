
```r
library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)
library(latticeExtra)
library(shiny)

setwd ("C:/Users/Karsten/Dropbox/msc_environmental_geography/semester_1/datamanagement/data")
fogo.rast <- raster("LC82100502014328LGN00_B10.tif")
survey2014 <- readOGR("data_2014_subset1.shp", "data_2014_subset1")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "data_2014_subset1.shp", layer: "data_2014_subset1"
## with 161 features and 6 fields
## Feature type: wkbPoint with 2 dimensions
```

```r
survey2014 <- spTransform(survey2014, CRS(projection(fogo.rast)))

create.map <- function (tif, vector, grid.nmbr=5, color="Reds", classes=6){
  vector_classes <- cut(vector@data$COVRG, classes)
  vector_colors <- colorRampPalette(brewer.pal(classes,color))(classes)
  min <- max(mean(getValues(tif)) - sd(getValues(tif)), 0)
  max <- mean(getValues(tif)) + sd(getValues(tif))
  
  breaks <- seq(min, max, length.out = 256)
  
  yat = seq(extent(tif)@ymin, 
            extent(tif)@ymax, length.out = grid.nmbr)
  xat = seq(extent(tif)@xmin, 
            extent(tif)@xmax, length.out = grid.nmbr)
  
  
  plt <- spplot(tif, col.regions = gray.colors(256), at = breaks,
              key = list(space = 'left', text = list(levels(vector_classes)), 
              points = list(pch = 21, cex = 2, fill = vector_colors)),
                colorkey=list(space="right"),
                panel = function(...){
                  panel.levelplot(...)
                  panel.abline(h = yat, v = xat, col = "grey0", lwd = 0.8, lty = 5) 
                },
              scales = list(x = list(at = xat,rot=90),
                            y = list(at = yat)))
  
  orl <- spplot(vector, zcol = "COVRG", col.regions = vector_colors, 
                cuts = c(0, 20, 40, 60, 80, 100, 120))
  
  plt + as.layer(orl)
}
```


```r
inputPanel(

  selectInput(inputId = "color", label = "Coloration of plotted vector:",
              choices = c("Greens", "Blues", "Reds"), selected = "Greens"),

  selectInput(inputId = "grid.nmbr", label = "Grid lines:",
              choices = c(4, 6, 8), selected = 4),

  selectInput(inputId = "classes", label = "Classes for plotted vector:",
             choices = c(4, 6, 8), selected = 4),
  )
```

```
## Error in flowLayout(...): Argument fehlt ohne Standard
```

```r
renderPlot({
  map(tif, vector, classes = input$classes, grid.nmbr = input$grid.nmbr, vector_colors = input$color)  
})
```

<!--html_preserve--><div id="out31804afc2ca2b1de" class="shiny-plot-output" style="width: 100% ; height: 400px"></div><!--/html_preserve-->
