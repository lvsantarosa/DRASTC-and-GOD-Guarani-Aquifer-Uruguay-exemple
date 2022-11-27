################################################################################
################################### GOD method #################################
################################################################################

library(terra)
library(dplyr)

dir.create(path = 'temp', showWarnings = FALSE)

terraOptions(tempdir = 'temp')

################################################################################
################################### Geology ####################################
################################################################################

GEO = terra::vect("Data/Merge_Geol_Final.shp") 
terra::crs(GEO) = "epsg: 32721"

GEO_rst = terra::rasterize(GEO, MDE, "Id") 

#1  = Fm. Arapey =              DRASTIC = 0,5
#2  = Fm. Arapey Intertraps =   DRASTIC = 0,5
#3  = Fm. Buena Vista =         DRASTIC = 1
#4  = Fm. Las Arenas =          DRASTIC = 0,4
#5  = Fm. Rivera =              DRASTIC = 1
#6  = Fm. Tacuarembó =          DRASTIC = 0,9
#7  = Fm. Yaguari =             DRASTIC = 0,2
#8  = Fm. Planícies Aluviales = DRASTIC = 1
#9  = Fm. Diques =              DRASTIC = 0,5
#10 = Basamento Indiferenciado  DRASTIC = 0


GEO_G = rbind(c(1, 0.5), c(2, 0.5), c(3, 1), c(4, 0.4), c(5, 1), 
                  c(6, 0.9),c(7, 0.2), c(8, 1), c(9, 0.5), c(10, 0))

G = terra::classify(GEO_rst, GEO_G)
G = terra::resample(G, MDE)

terra::writeRaster(G, "Results/GOD_G.tif", overwrite=TRUE)

################################################################################
################################## Occurrence  #################################
################################################################################

#1  = Fm. Arapey =              DRASTIC = 0,80
#2  = Fm. Arapey Intertraps =   DRASTIC = 0,80
#3  = Fm. Buena Vista =         DRASTIC = 0,75
#4  = Fm. Las Arenas =          DRASTIC = 0,75
#5  = Fm. Rivera =              DRASTIC = 0,75
#6  = Fm. Tacuarembó =          DRASTIC = 0,70
#7  = Fm. Yaguari =             DRASTIC = 0,55
#8  = Fm. Planícies Aluviales = DRASTIC = 0,75
#9  = Fm. Diques =              DRASTIC = 0,80
#10 = Basamento Indiferenciado  DRASTIC = 0,60


GEO_O = rbind(c(1, 0.8), c(2, 0.8), c(3, 0.75), c(4, 0.75), c(5, 0.75), 
                  c(6, 0.7), c(7, 0.55), c(8, 0.75), c(9, 0.8), c(10, 0.6))


O = terra::classify(GEO_rst, GEO_O)
O = terra::resample(O, MDE)

terra::writeRaster(O, "Results/GOD_O.tif", overwrite=TRUE)


################################################################################
##################################### Deepth  ##################################
################################################################################


PZM = terra::rast("Data/Piezometry.tif.tif")#Gerado por cokriging en ArcGIS, utilizando os poços disponíveis na área
terra::crs(PZM) = "epsg: 32721"

MDE = terra::rast("Data/SRTM.tif") #Modelo Digital de elevação = SRTM *Fazer a modificação para o MDE dos dados IDE
terra::crs(MDE) = "epsg: 32721"

PZM_res = terra::resample(PZM, MDE)

DEEPTH = MDE - PZM_res

DEEPTH = terra::app(DEEPTH, fun=function(x){ x[x < 0] <- 2; return(x)})

min = c(0, 5, 20, 50)
max = c(5, 20, 50, 999)
Deepth = c(0.9, 0.8, 0.7, 0.6)

Df = data.frame(min, max, Deepth)

D = terra::classify(DEEPTH, Df)

terra::writeRaster(D, "Results/GOD_D.tif", overwrite=TRUE)

################################################################################
################################# GOD Final ####################################
################################################################################

GOD = G*O*D

min = c(-1, 0.1, 0.3, 0.5, 0.7) 
max = c(0.1, 0.3, 0.5, 0.7, 1)
Reclass_GOD = c(1, 2, 3, 4, 5)

Df_GOD = data.frame(min, max, Reclass_GOD)

GOD_fnl = terra::classify(GOD, Df_GOD)

terra::plot(GOD_fnl, col=c("green", "blue", "yellow","orange", "red"))
terra::writeRaster(GOD_fnl, "Results/GOD.tif", overwrite=TRUE)


################################################################################
############################### Interactive Maps  #############################
################################################################################

library(tmap)    
library(leaflet) 

col=c("green", "blue", "yellow","orange", "red")

tmap_leaflet(                                                    
  tm_shape(as(GOD_fnl, "Raster")) +  
    tm_raster(palette= col, breaks = seq(1, 5, by=1)) 
) 


unlink(x = list.files('temp', full.names = TRUE)) #apaga os temporararios