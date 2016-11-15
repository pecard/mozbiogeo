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
            'raster' ,'virtualspecies', 'rasterVis', 'FFTrees')
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

# Political boundaries from GADM Global Adminsitrative Data -------------------
mz_adm <-getData('GADM', country='MOZ', level= 1)
mz_admdf <- fortify(mz_adm)

#' Protected Area Network -----------------------------------------------------
panet <- rgdal::readOGR('D:/Sig/MozBiogeo/shp'
                        , layer='MOZ_areas_protegidas_utm36s')
panet <- spTransform(panet, CRS('+init=epsg:4326'))
p_wgs84 <- CRS('+init=epsg:4326') # wgs84
panetdf <- fortify(panet) #! spdf to dataframe

#' Get Altitude data ----------------------------------------------------------
alt <- raster::getData('alt', country = 'MOZ', mask = F)
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

#' Search and eliminate variable multicolinearity in Bioclim Vars -------------
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

#' Fires ----------------------------------------------------------------------
fire <- rgdal::readOGR(dsn=file.path('D:/Sig/MozBiogeo/shp/firms')
                       ,layer = 'firms218371430920801_MCD14ML'
                       ,stringsAsFactors=F)
fire@data$ACQ_DATE <- ymd(fire@data$ACQ_DATE)
fire@data$YEAR <- year(fire@data$ACQ_DATE)
fire@data$MONTH <- lubridate::month(fire@data$ACQ_DATE)
rfire <- rasterize(fire, wcl_mzsubset, field = 'N', 'count')
rfire[is.na(rfire)] <- 0
rfire@data@names <- "nfires"
writeRaster(rfire, file.path(wd_dados, 'fires_2000-11-01_2015-02-27.tif'),
            overwrite=TRUE)
plot(rfire)

#' AFRIPOP UN Adjusted --------------------------------------------------------
#' UNITS: Estimated persons per grid square
pop <- raster('F:/Sig/Mozbiogeo/raster/MOZ-POP/MOZ15adjv4.tif')
pop <- aggregate(pop, fun = 'sum', fact=5)
pop <- resample(pop, wcl_mzsubset, method = 'ngb'
                , filename = file.path(wd_dados, 'popmoz2015.tif'),
                overwrite=TRUE)
pop@data@names <- "pop2015"
compareRaster(pop, wcl_mzsubset)

#' Human Footprint
hfp <- raster('D:/Sig/Raster/HumanFootprint/hfp2009_moz.tif')
hfp <- crop(hfp, wcl_mzsubset)
hfp <- aggregate(hfp, fun = 'mean', fact=5)
hfp <- resample(hfp, wcl_mzsubset, method = 'ngb'
                , filename = file.path(wd_dados, 'hfp2009_moz.tif'),
                overwrite=TRUE)
compareRaster(pop, wcl_mzsubset)

#' Collection of environmental Vars
wcl_mzsubst_mod <- mask(wcl_mzsubset, mz_adm)
#' With 'Human' variables
wcl_mzsubst_mod <- addLayer(wcl_mzsubset, pop, rfire, hfp)
wcl_mzsubst_mod <- mask(wcl_mzsubst_mod, mz_adm)

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
set.seed(123)
fold <- kfold(pt, k=5)
occtest <- pt[fold == 1, ]
occtrain <- pt[fold != 1, ]

#' Run Maxent on train set and envvars
me <- dismo::maxent(x = wcl_mzsubst_mod, p = occtrain[ ,-3]
                    , removeDuplicates = T
                    , args = c('-J', '-P'))
#me <- dismo::maxent(x = wcl_mzsubset, p = pt[ ,-3]
#                    , removeDuplicates = T
#                    , args = c('-J', '-P'))

me # open html model output
plot(me)
dismo::response(me)

dismo::threshold

#' Testing Maxent model with background data ----------------------------------
bg <- randomPoints(wcl_mzsubst_mod, 5000)

#' 1# Simple test with 'evaluate'
e1 <- evaluate(me, p=occtest[ ,-3], a = bg, x = wcl_mzsubst_mod)
threshold(e1)

#' 2# alternative 1
# extract values
pvtest <- data.frame(extract(wcl_mzsubst_mod, occtest[ ,-3]))
avtest <- data.frame(extract(wcl_mzsubst_mod, bg))

e2 <- evaluate(me, p = pvtest, a=avtest)
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
rmax <- dismo::predict(me, wcl_mzsubst_mod
                       , progress='text')

#' Calculate areas for specific Suitability intervals
sintervals <- c(0, .2, .4, .6, .8, 1) 
rmaxcut <- cut(rmax, sintervals, include.lowest = TRUE)
akm2 <- mean(getValues(area(rmaxcut)), na.rm=T)

tfreq <- data.frame(freq(rmaxcut, merge=TRUE))
tfreq$areakm2 <- tfreq$count * akm2
tfreq <- tfreq[complete.cases(tfreq), ]
tfreq
sum(tfreq$areakm2)
#' test cut
cut(seq(0,1,by=0.05), c(0,0.2,0.8,1), include.lowest=T)

writeRaster(rmax
            ,filename='D:/Dropbox/programacao/mozbiogeo/png/maxent_pred_bio_hum.tif'
            ,overwrite = T)

#' Plot with ggplot - to ggplot dataframe
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

#' FFT Fast Frugal Trees ------------------------------------------------------
#rmax <- raster('C:/Users/PCardoso/Google Drive/Programacao/r/mozbiogeo/maxent_prediction.tif')
#' Presences
pt_p <- pt
pt_p <- as.data.frame(extract(wcl_mzsubset, pt))
pt_p$pres <- TRUE
#rmax <- raster('C:/Users/PCardoso/Google Drive/Programacao/r/mozbiogeo/maxent_prediction.tif')
pt_p$prob <- extract(rmax, pt)
#' Pseudo Absences
pt_a <- bg
pt_a <- as.data.frame(extract(wcl_mzsubset, bg))
pt_a$pres <- FALSE
pt_a$prob <- extract(rmax, bg)
#pt_a <- pt_a[order(pt_a$prob, decreasing = F), ]
pt_a <- pt_a[pt_a$prob < 0.1, ]
#' Overall matrix with Presence and pseudo absences
ptpa <- rbind(pt_a, pt_p)
ptpa <- ptpa[ ,-9]

ptfft <- FFTrees::fft(formula = pres ~.
                      ,data = ptpa
                      ,train.p = .25)
ptfft
ptfft$auc
plot(ptfft, 
     main = "Probability of Occurrence", 
     decision.names = c("Present", "Absent"))

