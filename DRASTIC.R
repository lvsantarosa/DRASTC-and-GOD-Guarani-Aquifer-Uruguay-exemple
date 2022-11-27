################################################################################
######## DRASTIC for the outcrop area of the Guarani aquifer in Uruguay  #######
######## Based on article - https://www.mdpi.com/2073-4441/12/5/1356/pdf #######
################################################################################

library(terra)
library(dplyr)

dir.create(path = 'temp', showWarnings = FALSE)

terraOptions(tempdir = 'temp') 

################################################################################
######################### Topography and Piezometry ############################
################################################################################

#Piezometric surface published in ISARM 2021 
#(https://ibn.idsi.md/sites/default/files/imag_file/Book%20of%20Abstracts%20%E2%80%93%20ISARM2021.pdf#page=66)
PZM = terra::rast("Data/Piezometry.tif")
terra::crs(PZM) = "epsg: 32721"

#From SRTM data
MDE = terra::rast("Data/SRTM.tif") 

PZM_res = terra::resample(PZM, MDE)

DEEPTH = MDE - PZM_res

DEEPTH = terra::app(DEEPTH, fun=function(x){ x[x < 0] <- 5; return(x)})

####Matrix to reclassify depth based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf)

min = c(0, 1.5, 4.6, 9.1, 15.2, 22.8, 30.4)
max = c(1.5, 4.6, 9.1, 15.2, 22.8, 30.4, 999)
Deepth = c(10, 9, 7, 5, 3, 2, 1)

Df = data.frame(min, max, Deepth)

D = terra::classify(DEEPTH, Df)

terra::writeRaster(D, "Results/D.tif", overwrite=TRUE)

################################################################################
############################# Aquifer Recharge #################################
################################################################################


##Aquifer Recharge calculated by water balance based on article 
#https://link.springer.com/article/10.1007/s12665-021-09382-3

RECH = terra::rast("Data/Recharge.tif") 
RECH = terra::project(RECH, "epsg: 32721")

terra::plot(RECH)

####Matrix to reclassify the aquifer recharge based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf)
min = c(0, 50.8, 101.6, 177.8, 254)
max = c(50.8, 101.6, 177.8, 254, 999)
Recharge = c(1, 3, 7, 8, 9)

Df_rec = data.frame(min, max, Recharge)

R = terra::classify(RECH, Df_rec)
R = terra::resample(R, MDE)

terra::writeRaster(R, "Results/R.tif", overwrite=TRUE)


################################################################################
##################################### Soil #####################################
################################################################################


#Reclassify names by factors

#Horizon A
Soil_A = terra::vect("Data/Dis_Solos_Hor_A_Project.shp") 
terra::crs(Soil_A) = "epsg: 32721" 

#Transform character data into factors and numbers and organize
Soil_A$factors = as.numeric(factor(Soil_A$NomeClass))
Soil_A = Soil_A[with(Soil_A, order(Soil_A$factors)),]  

# No Class         = 1
# Clay             = 2
# Silty Clay       = 3
# Sandy            = 4
# loamy Sandy      = 5
# Loam             = 6
# Clay Loam        = 7
# Sandy Loam       = 8
# Silty Clay Loam  = 9

Soil_A_rst = terra::rasterize(Soil_A, MDE, "factors")

####Matrix to reclassify the soil based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf

# No Class         = 1 = DRASTIC = 0
# Clay             = 2 = DRASTIC = 1 
# Silty Clay       = 3 = DRASTIC = 1 
# Sandy            = 4 = DRASTIC = 9
# Loamy Sandy      = 5 = DRASTIC = 6
# Loam             = 6 = DRASTIC = 5
# Clay Loam        = 7 = DRASTIC = 3
# Sandy Loam       = 8 = DRASTIC = 6
# Silty Clay Loam  = 9 = DRASTIC = 4


Soil_A_class = rbind(c(1, 0), c(2, 1), c(3, 1), c(4, 9), c(5, 6), c(6, 5),
                      c(7, 3), c(8, 6), c(9, 4)) 


SA = terra::classify(Soil_A_rst, Soil_A_class)
SA = terra::resample(SA, MDE)

#Horizon B
Soil_B = terra::vect("Data/Dis_Solos_Hor_B_Project.shp")
terra::crs(Soil_B) = "epsg: 32721" 

#Transform character data into factors and numbers and organize
Soil_B$factors = as.numeric(factor(Soil_B$NomeClass))
Soil_B = Soil_B[with(Soil_B, order(Soil_B$factors)),] 

# No Class         = 1
# Clay             = 2
# Silty Clay       = 3
# Sand Clay Loam   = 4 
# Sandy Clay       = 5 
# Loam             = 6
# Clay Loam        = 7
# Sandy Loam       = 8
# Silty Clay Loam  = 9

Soil_B_rst = terra::rasterize(Soil_B, MDE, "factors")

####Matrix to reclassify the soil based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf

# No Class         = 1 = DRASTIC = 0
# Clay             = 2 = DRASTIC = 1
# Silty Clay       = 3 = DRASTIC = 1
# Sand Clay Loam   = 4 = DRASTIC = 5 
# Sandy Clay       = 5 = DRASTIC = 5 
# Loam             = 6 = DRASTIC = 5
# Clay Loam        = 7 = DRASTIC = 3
# Sandy Loam       = 8 = DRASTIC = 6
# Silty Clay Loam  = 9 = DRASTIC = 4


Soil_B_class = rbind(c(1, 0), c(2, 1), c(3, 1), c(4, 5), c(5, 5), c(6, 5),
                     c(7, 3), c(8, 6), c(9, 4)) 


SB = terra::classify(Soil_B_rst, Soil_B_class)
SB = terra::resample(SB, MDE)

#Soil Final
S = (SA + SB) / 2 
terra::writeRaster(S, "Results/S.tif", overwrite=TRUE)


################################################################################
############################ Topography (slope %) ##############################
################################################################################

Slope = terra::rast("Data/slope.tif") 
terra::crs(Slope) = "epsg: 32721"


####Matrix to reclassify the slope based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf
min = c(0, 2, 6, 12, 18)
max = c(2, 6, 12, 18, 999)
Slope_Df = c(10, 9, 5, 3, 1)

Df_slope = data.frame(min, max, Slope_Df)

TP = terra::classify(Slope, Df_slope)
TP = terra::resample(TP, MDE)

terra::writeRaster(TP, "Results/T.tif", overwrite=TRUE)


################################################################################
#################################### Geology ###################################
################################################################################

GEO = terra::vect("Data/Merge_Geol_Final.shp") 
terra::crs(GEO) = "epsg: 32721"

################################################################################
################################ Aquifer Media  ################################
################################################################################


GEO_rst = terra::rasterize(GEO, MDE, "Id") 

#1  = Fm. Arapey =              DRASTIC = 4
#2  = Fm. Arapey Intertraps =   DRASTIC = 4
#3  = Fm. Buena Vista =         DRASTIC = 7
#4  = Fm. Las Arenas =          DRASTIC = 6
#5  = Fm. Rivera =              DRASTIC = 7
#6  = Fm. Tacuarembó =          DRASTIC = 6
#7  = Fm. Yaguari =             DRASTIC = 4
#8  = Fm. Planícies Aluviales = DRASTIC = 7
#9  = Fm. Diques =              DRASTIC = 4
#10 = Basamento Indiferenciado  DRASTIC = 3

####Matrix to reclassify the Aquifer Media based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf
GEO_class = rbind(c(1, 4), c(2, 4), c(3, 7), c(4, 6), c(5, 7), c(6, 6),
                      c(7, 4), c(8, 7), c(9, 4), c(10, 3))


A = terra::classify(GEO_rst, GEO_class)
A = terra::resample(A, MDE)

terra::writeRaster(A, "Results/A.tif", overwrite=TRUE)

################################################################################
############################ Vadose Zone Material  #############################
################################################################################

#1  = Fm. Arapey =              DRASTIC = 4
#2  = Fm. Arapey Intertraps =   DRASTIC = 4
#3  = Fm. Buena Vista =         DRASTIC = 6
#4  = Fm. Las Arenas =          DRASTIC = 7
#5  = Fm. Rivera =              DRASTIC = 6
#6  = Fm. Tacuarembó =          DRASTIC = 6
#7  = Fm. Yaguari =             DRASTIC = 4
#8  = Fm. Planícies Aluviales = DRASTIC = 7
#9  = Fm. Diques =              DRASTIC = 4
#10 = Basamento Indiferenciado  DRASTIC = 4


####Matrix to reclassify the Vadose Zone Material based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf
GEO_class_I = rbind(c(1, 4), c(2, 4), c(3, 6), c(4, 7), c(5, 6), c(6, 6),
                      c(7, 4), c(8, 7), c(9, 4), c(10, 4))


I = terra::classify(GEO_rst, GEO_class_I)
I = terra::resample(I, MDE)


terra::writeRaster(I, "Results/I.tif", overwrite=TRUE)


################################################################################
########################## Hydraulic Conductivity  #############################
################################################################################

#In this case, was based on the Hydraulic Conductivity of the soil 
#because of the importance of this layer in the infiltration process and
#absent of the Hydraulic Conductivity of geology.
#The soil texture was converted by FAO table
#https://www.fao.org/3/a0975e/a0975e01.pdf 

#####Horizon A
K_soil_A = terra::vect("Data/Dis_Solos_Hor_A_Project.shp") 
terra::crs(K_soil_A) = "epsg: 32721" 

K_A_rst = terra::rasterize(K_soil_A, MDE, "K")

####Matrix to reclassify the hydraulic conductivity based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf
min = c(0.001, 0.05, 0.1, 0.4, 1, 3, 6) 
max = c(0.05, 0.1, 0.4, 1, 3, 6, 999)
KA_class = c(1, 2, 2, 3, 5, 6, 8)

Df_KA = data.frame(min, max, KA_class)

CA = terra::classify(K_A_rst, Df_KA)
CA = terra::resample(CA, MDE)


#####Horizon B
K_soil_B = terra::vect("Data/Dis_Solos_Hor_B_Project.shp") #Carregar os dados
terra::crs(K_soil_B) = "epsg: 32721" #Mudar o CRS

K_B_rst = terra::rasterize(K_soil_B, MDE, "K")

####Matrix to reclassify the hydraulic conductivity based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf
min = c(0.001, 0.05, 0.1, 0.4, 1, 3, 6)
max = c(0.05, 0.1, 0.4, 1, 3, 6, 999)
KB_class = c(1, 2, 2, 3, 5, 6, 8)

Df_KB = data.frame(min, max, KB_class)

CB = terra::classify(K_B_rst, Df_KB)

CB = terra::resample(CB, MDE)


C = (CA+CB) / 2 

terra::writeRaster(C, "Results/C.tif", overwrite=TRUE)

################################################################################
################################### DRASTIC  ###################################
################################################################################


Dw  = 5
Rw  = 4
Aw  = 3
Sw  = 2
TPw = 1
Iw  = 5
Cw  = 3


DRASTIC = D*Dw + R*Rw + A*Aw + SB*Sw + TP*TPw + I*Iw + CB*Cw

####Matrix to reclassify the DRASTIC values based on article 
#https://www.mdpi.com/2073-4441/12/5/1356/pdf
min = c(0, 64, 105, 146, 187) 
max = c(64, 105, 146, 187, 230)
Reclass_DRASTIC = c(1, 2, 3, 4, 5)

Df_DRASTIC = data.frame(min, max, Reclass_DRASTIC)
DRASTIC_fnl = terra::classify(DRASTIC, Df_DRASTIC)

terra::plot(DRASTIC_fnl, col=c("green", "blue", "yellow","orange", "red"))
terra::writeRaster(DRASTIC_fnl, "Results/DRASTIC.tif", overwrite=TRUE)


################################################################################
############################### Interactive Maps  #############################
################################################################################

library(tmap)    
library(leaflet) 

col=c("green", "blue", "yellow","orange", "red")

tmap_leaflet(                                                    
  tm_shape(as(DRASTIC_fnl, "Raster")) +  
    tm_raster(palette= col, breaks = seq(1, 5, by=1)) 
) 



unlink(x = list.files('temp', full.names = TRUE)) 