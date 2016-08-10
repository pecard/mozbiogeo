#' Envelope Ambiental Bioclim
<<<<<<< HEAD
#' Modelo Entropia Maxent
=======
>>>>>>> origin/master
#' Distribuicao com base em dados de presenca
#' referencias:
#' https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis
#' http://vertnet.org/about/BITW-2012/DAY2/sdm_9_niche_demo.R
#' http://www.molecularecologist.com/2013/04/species-distribution-models-in-r/
#' http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf

save.image('..\\Distribution Models.RData')
load('..\\Distribution Models.RData')

#' Packages necessarios
kpacks <- c('ggplot2', 'scales', "ggmap",
            "gridExtra", "gtable")
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)

<<<<<<< HEAD

#' Dados de distribuicao ------------------------------------------------------
buf <- read.table(file.path(wd_dados, 'buffalo_moz.txt'),
                  header = T, sep = '\t',
                  stringsAsFactors = F, dec = '.')
=======
>>>>>>> origin/master
#' Administrative Data GADM
mz_adm <-getData('GADM', country='MOZ', level= 1)
mz_adm <- fortify(mz_adm)

#' Protected Area Network
'D:/Sig/MozBiogeo/shp/MOZ_areas_protegidas_utm36s.shp'
panet <- rgdal::readOGR('D:/Sig/MozBiogeo/shp', layer='MOZ_areas_protegidas_utm36s')
panet <- spTransform(panet, CRS('+init=epsg:4326'))
p_wgs84 <- CRS('+init=epsg:4326') # wgs84
panetdf <- fortify(panet) #! spdf to dataframe


#' Get Altitude data ----------------------------------------------------------
alt <- getData('alt', country = 'MOZ', mask = F)
plot(alt)

<<<<<<< HEAD
#' Climate data ---------------------------------------------------------------
=======
#' Climate data ----------------------------------------------------------------
>>>>>>> origin/master
#BIO1 = Annual Mean Temperature
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#BIO3 = Isothermality (BIO2/BIO7) (* 100)
#BIO4 = Temperature Seasonality (standard deviation *100)
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO7 = Temperature Annual Range (BIO5-BIO6)
#BIO8 = Mean Temperature of Wettest Quarter
#BIO9 = Mean Temperature of Driest Quarter
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter
#BIO12 = Annual Precipitation
#BIO13 = Precipitation of Wettest Month
#BIO14 = Precipitation of Driest Month
#BIO15 = Precipitation Seasonality (Coefficient of Variation)
#BIO16 = Precipitation of Wettest Quarter
#BIO17 = Precipitation of Driest Quarter
#BIO18 = Precipitation of Warmest Quarter
#BIO19 = Precipitation of Coldest Quarter

wcl <- getData('worldclim', var = 'bio', res = 2.5) # WorldClim Vars

#' Crop Global to MOZ limits using Alt raster
wcl_mz <- raster::crop(wcl, alt)

#' Resample Altimetry to worldClim resolutioin (coarser scale)
altr <- resample(alt, wcl_mz, method = "ngb") # set uniform resolution

wcl_mz <- addLayer(wcl_mz, altr) # add alt to rasterbrick
wcl_mzsubset <- subset(wcl_mz, c(1, 5, 6, 7, 12, 15, 20))
plot(wcl_mzsubset)

#' Species data: coordinates in decimal degrees!
pt <- buf[ ,3:4]
names(pt) <- c('lon', 'lat')

<<<<<<< HEAD
#' ggmap base layer
ctry.map <- get_map('Mozambique', zoom = 6, source = 'google', maptype = "roadmap") 

#' fit a BIOCLIM model --------------------------------------------------------
=======
#' fit a Bioclim model ---------------------------------------------------------
>>>>>>> origin/master
bclim <- bioclim(wcl_mzsubset, pt[,3:4])

#' predict Bioclim model to raster extent
pred <- predict(bclim, wcl_mzsubset, ext = wcl_mzsubset)

#' Convert to ggplot dataframe
t.pred <- rasterToPoints(pred) # Raster to dataframe
t.pred <- data.frame(t.pred)
colnames(t.pred) <- c("x",  "y", "Prob") # Coords: lat long
head(t.pred)

<<<<<<< HEAD
#'Plot
ggplot() +
  #ggmap(ao.map, extent = 'panel', darken = c(.8, "white")) +
  geom_raster(aes(x = x, y = y, fill = Prob),
              t.pred[t.pred$Prob != 0, ], alpha = .9) + 
  geom_polygon(aes(long, lat, group = group),
               data = mz_adm, colour = 'grey', fill = 'NA') +
  coord_equal() +
  theme_bw() +
  scale_fill_gradientn('Prob\nBioclim model',
                       colours = rev(c(terrain.colors(10)))) +
=======
#' ggmap base layer
ctry.map <- get_map('Mozambique', zoom = 6, source = 'google', maptype = "roadmap") 

ggplot() +
  #ggmap(ao.map, extent = 'panel', darken = c(.8, "white")) +
  geom_raster(aes(x = x, y = y, fill = Prob), t.pred[t.pred$Prob != 0, ], alpha = .9) + 
  geom_polygon(aes(long, lat, group = group), data = mz_adm, colour = 'grey', fill = 'NA') +
  coord_equal() +
  theme_bw() +
  scale_fill_gradientn('Prob\nBioclim model', colours = rev(c(terrain.colors(10)))) +
>>>>>>> origin/master
  #geom_polygon(inherit.aes = F, aes(x = long, y = lat, group = id),
  #             colour = 'NA', fill = 'darkblue', alpha = 0.2, size = 0.2,
  #             data = pandf)+
  geom_point(inherit.aes = F, aes(x = long, y = lat), size = 2,
             alpha = 0.9, data = ptdec)
  
<<<<<<< HEAD
#' Fit MAXENT model -----------------------------------------------------------
#' Split samples for train and predict
#' witholding a 20% sample for testing
=======
#' MAXENT
# witholding a 20% sample for testing 
occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]
>>>>>>> origin/master

fold <- kfold(pt, k=5)
occtest <- pt[fold == 1, ]
occtrain <- pt[fold != 1, ]

me <- maxent(wcl_mzsubset, occtrain)
rmax <- predict(me, wcl_mzsubset)
plot(rmax)

#' Convert to ggplot dataframe
t.predmax <- rasterToPoints(rmax) # Raster to dataframe
t.predmax <- data.frame(t.predmax)
colnames(t.predmax) <- c("x",  "y", "Prob") # Coords: lat long
<<<<<<< HEAD

#' Plot
ggplot() +
  #ggmap(ao.map, extent = 'panel', darken = c(.8, "white")) +
  geom_raster(aes(x = x, y = y, fill = Prob),
              t.predmax[t.predmax$Prob != 0, ], alpha = .9) + 
  geom_polygon(aes(long, lat, group = group),
               data = mz_adm, colour = 'grey', fill = 'NA') +
  coord_equal() +
  theme_bw() +
  scale_fill_gradientn('Prob\nMaxent model',
                       colours = rev(c(terrain.colors(10))))
=======
ggplot() +
  #ggmap(ao.map, extent = 'panel', darken = c(.8, "white")) +
  geom_raster(aes(x = x, y = y, fill = Prob), t.predmax[t.predmax$Prob != 0, ], alpha = .9) + 
  geom_polygon(aes(long, lat, group = group), data = mz_adm, colour = 'grey', fill = 'NA') +
  coord_equal() +
  theme_bw() +
  scale_fill_gradientn('Prob\nMaxent model', colours = rev(c(terrain.colors(10)))) 
>>>>>>> origin/master
