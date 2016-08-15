#' 2016 Aug 11
#' Envelope Ambiental Bioclim
#' Modelo Entropia Maxent
#' Distribuicao com base em dados de presenca
#' References -----------------------------------------------------------------
#'# SDM
#' https://sites.google.com/site/rodriguezsanchezf/news/usingrasagis
#' http://vertnet.org/about/BITW-2012/DAY2/sdm_9_niche_demo.R
#' http://www.molecularecologist.com/2013/04/species-distribution-models-in-r/
#' http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf
#'# Maxent
#' http://web2.uconn.edu/cyberinfra/module3/Downloads/Day%204%20-%20Maxent.pdf
#' file:///D:/Downloads/philips_tutorial.pdf
#' https://cran.r-project.org/web/packages/MaxentVariableSelection/vignettes/MaxentVariableSelection.pdf

save.image("D:/Dropbox/programacao/mozbiogeo_data/maxent-bioclim.RData")
load('D:/Dropbox/programacao/mozbiogeo_data/maxent-bioclim.RData')

#' Packages -------------------------------------------------------------------
kpacks <- c('ggplot2', 'scales', 'ggmap',
            'gridExtra', "gtable", 'dismo',
            'raster' ,'virtualspecies', 'rasterVis')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Set directories for conde and data
wd_dados <- 'D:/Dropbox/programacao/mozbiogeo_data'
wd_geo <- 'D:/SIG/MozBiogeo/shp'

#' Dados de distribuicao ------------------------------------------------------
buf <- read.table(file.path(wd_dados, 'buffalo_moz.txt'),
                  header = T, sep = '\t',
                  stringsAsFactors = F, dec = '.')

#' Administrative Data GADM
mz_adm <-getData('GADM', country='MOZ', level= 1)
mz_admdf <- fortify(mz_adm)

#' Protected Area Network -----------------------------------------------------
panet <- rgdal::readOGR('D:/Sig/MozBiogeo/shp'
                        , layer='MOZ_areas_protegidas_utm36s')
panet <- spTransform(panet, CRS('+init=epsg:4326'))
p_wgs84 <- CRS('+init=epsg:4326') # wgs84
panetdf <- fortify(panet) #! spdf to dataframe

#' Get Altitude data ----------------------------------------------------------
alt <- getData('alt', country = 'MOZ', mask = F)
plot(alt)

#' Climate data ----------------------------------------------------------------
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

#' Search and eliminate variable multicolinearity
par(mar = rep(2, 4), cex = 0.6)
set.seed(222)
rasters.crop.reduced <- removeCollinearity(wcl_mz
                                           , multicollinearity.cutoff = 0.65
                                           ,select.variables = TRUE
                                           , sample.points = FALSE
                                           , plot = T)
rasters.crop.reduced

wcl_mzsubset <- subset(wcl_mz, rasters.crop.reduced)

plot(wcl_mzsubset, legend=FALSE, axes=FALSE)

#' Species data: coordinates in decimal degrees!
#' Remove Nas and cell duplicated entries
pt <- buf[ ,3:4]
names(pt) <- c('lon', 'lat')
pt$cell <- extract(wcl_mzsubset, pt, cellnumbers=T)[ ,1]
pt <- pt[!is.na(pt$cell), ] # remove NA
pt[is.na(pt), ] # check NA presence
pt <- pt[!duplicated(pt$cell), ]
pt <- pt[ ,-3] # Remover colulas cell

fvirpt <- function(x){
  vsp <- x
  coordinates(vsp) <- ~lon+lat
  return(vsp)
}

levelplot(wcl_mzsubset[[1]], margin=FALSE)+
  layer(sp.points(fvirpt(pt)))
#' ggmap base layer
ctry.map <- get_map('Mozambique', zoom = 6, source = 'google', maptype = "roadmap") 

#' fit a BIOCLIM model --------------------------------------------------------
bclim <- bioclim(wcl_mzsubset, pt[ ,-3])

#' predict Bioclim model to raster extent
pred <- predict(bclim, wcl_mzsubset, ext = wcl_mzsubset)

#' Convert to ggplot dataframe
t.pred <- rasterToPoints(pred) # Raster to dataframe
t.pred <- data.frame(t.pred)
colnames(t.pred) <- c("x",  "y", "Prob") # Coords: lat long
head(t.pred)

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
                       colours = rev(c(terrain.colors(10))))
#' ggmap base layer
#ctry.map <- get_map('Mozambique', zoom = 6, source = 'google'
#                    , maptype = "roadmap") 

#' Fit MAXENT model -----------------------------------------------------------
#' Split samples for train and predict
#' witholding a 20% sample for testing
fold <- kfold(pt, k=5)
occtest <- pt[fold == 1, ]
occtrain <- pt[fold != 1, ]

#' Run Maxent on envvars, dist data and bckgd data
me <- dismo::maxent(x = wcl_mzsubset, p = occtrain[ ,-3]
                    , removeDuplicates = T
                    , args = c('-J', '-P'))
me # open html model output
plot(me)
response(me)

#' Testing Maxent model with background data ----------------------------------
bg <- randomPoints(wcl_mzsubset, 1000)

#' 1# Simple test with 'evaluate'
e1 <- evaluate(me, p=occtest[ ,-3], a=bg, x=wcl_mzsubset)
e1

# 2# alternative 1
# extract values
pvtest <- data.frame(extract(wcl_mzsubset, occtest[ ,-3]))
avtest <- data.frame(extract(wcl_mzsubset, bg))

e2 <- evaluate(me, p=pvtest, a=avtest)
e2

#' 3# alternative 2 
# predict to testing points 
testp <- predict(me, pvtest)
head(testp)
testa <- predict(me, avtest) 

e3 <- evaluate(p=testp, a=testa)
e3
threshold(e3)
plot(e3, 'ROC')

#' Predic to entire Area ------------------------------------------------------
rmax <- dismo::predict(me, wcl_mzsubset
                       , progress='text' 
                       , filename='D:/Dropbox/programacao/mozbiogeo/png/maxent_prediction.tif'
                       , overwrite = T)
plot(rmax)

#' Convert to ggplot dataframe
t.predmax <- rasterToPoints(rmax) # Raster to dataframe
t.predmax <- data.frame(t.predmax)
colnames(t.predmax) <- c("x",  "y", "Prob") # Coords: lat long

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
