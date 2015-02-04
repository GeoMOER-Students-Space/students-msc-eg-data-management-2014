## Mapping a vector into a landsat image
========================================================

*things we need for this worksheet*

* Landsat raster image B10 
* vector data_2014_subset 

*Before we start writing our code, we need to do some preprocessing*

load your requiered workspace and libraries

```r
library(sp)
library(raster)
library(rgdal)
library(RColorBrewer)
library(latticeExtra)
library (grid)
```



write a function for natural species, including the underlaying Landsat image, legend, colorramp for this vector natural species

```r
NAT_MAP <- function (raster, vector, grnb=7, attr=3){

# Koordinatensystem des Rasters definieren mit einem siebener Grid
yat = seq(extent(raster)@ymin, 
                extent(raster)@ymax, length.out = 7)
xat = seq(extent(raster)@xmin, 
                extent(raster)@xmax, length.out = 7)

# Vekoren in Klassen einteilen und Farbschema auswählen (wenn vector_colors="default gewünsct")
# if(vector_colors == "default"){
 #   vector_colors <- colorRampPalette(brewer.pal(9,"YlGn"))(9)  
  #}

  breaks <- quantile(raster, seq(0.0, 1.0, length.out = 256))
  colorkey_scale <- 1/breaks

#Erstellen einer (kontinuierlichen) Legende für das Raster und den Vektor
vector_colors <- colorRampPalette(brewer.pal(9,"YlGn"))(256)

vector_classes <- cut(vector@data$NAT, seq(0, 9, length.out = 256))

#plot for raster
plt <- spplot(raster, col.regions = gray.colors(256), at = breaks,
       colorkey=list(space="right"),
       panel = function(...){
         panel.levelplot(...)
         panel.abline(h = yat, v = xat, col = "grey0", lwd = 0.8, lty = 3) 
         },
       scales = list(x = list(at = xat),
                     y = list(at = yat)))

# continous legend for vector
orl <- spplot(vector, zcol = "NAT", col.regions = vector_colors, cuts = 256,
              colorkey = TRUE)

comb <- plt + as.layer(orl)
print(comb)

downViewport(trellis.vpname(name = "figure")) 
vp1 <- viewport(x = -0.25, y = 1,
                height = 1, width = 1,
                just = c("centre", "top"),
                name = "key.vp") 
pushViewport(vp1) 
draw.colorkey(key = list(col = vector_colors, width = 2,
                          at = seq(0, 9, length.out = 256),
                          space = "left"), draw = TRUE)
# print out
plt + as.layer(orl)
}
```

Run the main function with vector_colors="default" (Therefore you need to copy "vector_colors="default" in the function head above)

```r
#NAT_MAP (raster, vector,vector_colors="default", grnb=7, attr =3)
```
Run the main function with continous scale

```r
NAT_MAP (raster, vector, grnb=7, attr =3)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) ![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png) 
