#' http://gsif.r-forge.r-project.org/
#' 
kpacks <- c('dplyr', 'raster', 'rgdal', 'dismo')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#'# Grelha 0.5 graus WGS84 -> UTM36S -------------------------------------------
gr <- rgdal::readOGR(dsn= file.path(wd_geo), layer='MOZ_grid050g')
proj4string(gr) <- CRS('+init=epsg:4326')
gr050utm <- sp::spTransform(gr, CRS('+init=EPSG:32736'))
plot(gr050utm, axes=T)

#'# Altimetria de Mocambique ---------------------------------------------------
alt <- raster::getData('alt', country='MOZ', mask = F)
alt[is.na(alt)] <- 0
alt[alt < 0] <- 0

#'# Carta de Vegetacao de Mocambique em WGS84 - raster:: QGIS ------------------
veg <- raster('D:/Sig/MozBiogeo/raster/vegetacao_moz_wgs84.tif')
veg1 <- raster::resample(veg, alt,  method='ngb')
#alt <- raster::crop(alt, veg)
raster::compareRaster(alt, veg1)
gr_veg <- raster::extract(veg1, gr[1:2,], method='simple')

#'# Solos ----------------------------------------------------------------------
solo <- readOGR(dsn='D:/Sig/MozBiogeo/shp', layer = 'SolosMoz'
                , stringsAsFactors=F)
proj4string(solo) <- CRS('+init=epsg:4326')
#' ler a tabela do shapefile
soil_tipo <- solo@data %>% 
  dplyr::filter(!duplicated(.)) %>%
  dplyr::filter(nchar(AGRUP_SOLO) > 1) %>% # para retirar coluna vazia
  dplyr::arrange((AGRUP_SOLO)) %>%
  dplyr::mutate(idsolo=1:nrow(.))
soil_tipo$solo1[soil_tipo$idsolo %in% c(30:46)] <- 1
soil_tipo$solo1[!soil_tipo$idsolo %in% c(30:46)] <- 0
joinsolo <- solo@data %>% left_join(soil_tipo, by = 'AGRUP_SOLO')
soloagr <- sp::merge(solo, soil_tipo, by = 'AGRUP_SOLO')
#' rasterize
soil <- raster::rasterize(soloagr, veg1, 'solo1')
compareRaster(soil, alt)
#raster::resample(soil, alt,  method = 'ngb')

#'# Populacao por quadricula
pop <- readOGR(dsn='D:/Sig/MozBiogeo/shp', layer = 'MOZ_pop_total_polig'
               , stringsAsFactors=F)
gr_pop <- dplyr::select(pop@data, -2)
gr_pop$PopTotal_s[is.na(gr_pop$PopTotal_s)] <- 0
names(gr_pop)[2] <- 'poptot'

#' TESTE perc classes vegetacao ##
veg_tablist <- lapply(seq(gr_veg), FUN=f_tabulate
                      , extracted=gr_veg
                      , region=gr, regname="gr")
veg_tab <- do.call(rbind.data.frame, veg_tablist)
#' index: http://stackoverflow.com/a/30151018
veg_tab[["id"]] <- rep(seq_along(veg_tablist), sapply(veg_tablist, nrow))
#' spread: http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/

#' Uso agricola combinado -----------------------------------------------------
uso_agro <- readOGR(dsn='D:/Sig/MozBiogeo/shp'
                    , layer = 'MOZ_areas_agricolas_utm36s'
                    , stringsAsFactors=F)
proj4string(uso_agro) <- CRS("+init=epsg:32736")
uso_agro@data$ID <- 1
uso_agro <- spTransform(uso_agro, CRS("+init=epsg:4269"))
ruso_agro <- raster::rasterize(uso_agro, veg1, 1)
plot(ruso_agro)

#' Conflito armado ------------------------------------------------------------
renamo <- readOGR(dsn='D:/Sig/MozBiogeo/shp'
                  , layer = 'renamo__after_utm36s'
                  , stringsAsFactors=F)
proj4string(renamo) <- CRS("+init=epsg:32736")
renamo <- spTransform(renamo, CRS("+init=epsg:4269"))
rrenamo <- raster::rasterize(renamo, veg1, 1)
plot(rrenamo)

#' Fogos ----------------------------------------------------------------------
# usar fire_tab dos dados obtidos em NASA_FIRMS.R
fire_tab <- fire_tab %>%
  dplyr::left_join(gr@data['idu'], by = 'idu')

#' Dados Bioclimaticos --------------------------------------------------------
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

lon1 <- mean(coordinates(gr)[ ,1])
lat1 <- mean(coordinates(gr)[ ,2])
wcl <- raster::getData('worldclim', var='bio', res = 0.5
                       ,lon=lon1, lat=lat1) # WorldClim Vars
srtm <- raster::getData('SRTM' ,lon=lon1, lat=lat1)

#' Crop Bioclim para o limite da Grelha ###
wcl_moz <- raster::crop(wcl, veg1)
plot(wcl_moz[[1]])
plot(gr, add = T)

wcl_all <- raster::addLayer(wcl_moz, veg1, alt) # add alt to rasterbrick
names(wcl_all)
wcl_subset <- raster::subset(wcl_all, c(1,5,6,7,12,15, 21))
plot(wcl_subset)

#' FUNCOES ====================================================================

#' Funcao Tabulate ------------------------------------------------------------
f_tabulate <- function(indx, extracted, region, regname) {
  dat <- as.data.frame(table(extracted[[indx]]))
  dat$name <- region[[regname]][[indx]]
  return(dat)
}

#' Funcao Extrair Moda das variaveis para a grelha 0.5 graus ------------------
f_mod <- function(x, ...) {
  uniqx <- unique(x)
  uniqx <- uniqx[!is.na(uniqx)]
  m <- uniqx[which.max(tabulate(match(x, uniqx)))]
  return(m)
}

f_mod0 <- function(x, ...) { # excluir zeros - vegetacao, solos e populacao
  uniqx <- unique(x)
  uniqx <- uniqx[!is.na(uniqx)]
  uniqx <- uniqx[uniqx > 0]
  if(length(uniqx) == 0) uniqx <- 0
  m <- uniqx[which.max(tabulate(match(x, uniqx)))]
  return(m)
}

#' Funcao MODA: obter moda por quadricula -------------------------------------
#' http://gis.stackexchange.com/questions/130522/r-increase-speed-of-crop-mask-extract-raster-by-many-polygons
f_getgridmod <- function(x=x, y=y){
  lgr <- list()
  for(i in 1:nrow(y@data)){
    gri <- y[i, ]
    #gr_i <- raster::extract(veg1, gri, df = F, na.rm=TRUE)
    gr_i <- raster::extract(x, gri, df = F, na.rm=TRUE)
    uniqx <- unlist(unique(gr_i))
    uniqx <- uniqx[!is.na(uniqx)]
    uniqx <- uniqx[uniqx > 0]
    if(length(uniqx) == 0) uniqx <- 0
    m <- uniqx[which.max(tabulate(match(gr_i, uniqx)))]
    lgr[[i]] <- m
  }
  vegmod <- do.call(rbind.data.frame, lgr)
  #names(vegmod) <- c('veg')
}

#' Funcao PERC: obter Desv padrao da variavel por quadricula ------------------
f_getgriperc <- function(x=x, y=y){
  lgr <- list()
  for(i in y@data$idu){
    gri <- y[i, ]
    #proj4string(gri) <- CRS(proj4string(gr))
    #gr_i <- raster::extract(veg1, gri, df = F, na.rm=TRUE)
    gr_i <- raster::extract(x, gri, df = F, na.rm=TRUE)[[1]]
    gr_i[is.na(gr_i)] <- 0
    p <- length(gr_i[gr_i == 1])/length(gr_i)*100
    #uniqx <- unlist(unique(gr_i))
    #uniqx <- uniqx[!is.na(uniqx)]
    #uniqx <- uniqx[uniqx > 0]
    #if(length(uniqx) == 0) uniqx <- 0
    #m <- uniqx[which.max(tabulate(match(gr_i, uniqx)))]
    #lgr[[i]] <- m
    lgr[[i]] <- p
  }
  #vegmod <- do.call(rbind.data.frame, lgr)
  #names(vegmod) <- c('veg')
  return(lgr)
}

#' Funcao extrair  n classes (variedade) na grelha ----------------------------
f_getgridvar<- function(x=x, y=y){
  lgr <- list()
  for(i in 1:nrow(y@data)){
    gri <- y[i, ]
    #gr_i <- raster::extract(veg1, gri, df = F, na.rm=TRUE)
    gr_i <- raster::extract(x, gri, fun=function(x,...)length(unique(x))
                            , df = F, na.rm=TRUE)
    uniqx <- unlist(unique(gr_i))
    uniqx <- uniqx[!is.na(uniqx)]
    uniqx <- uniqx[uniqx > 0]
    if(length(uniqx) == 0) uniqx <- 0
    m <- uniqx[which.max(tabulate(match(gr_i, uniqx)))]
    lgr[[i]] <- m
  }
  vegvar <- do.call(rbind.data.frame, lgr)
  #names(vegmod) <- c('veg')
}

#'# SUMARIOS NA GRELHA ========================================================
#' Classe de vegetacao dominante na quadricula --------------------------------
vegmod <- f_getgridmod(x=veg1, y=gr)
names(vegmod) <- c('veg_mod')
#vegmod <- do.call(rbind.data.frame, lgr)
#names(vegmod) <- c('veg')
vegmod$ID <- as.numeric(row.names(vegmod))

#' Variedade de classes de vegetacao na quadricula ----------------------------
vegvar <- raster::extract(veg1, gr
                          ,fun=function(x,...)length(unique(x))
                          ,df = T, na.rm=TRUE)
names(vegvar)[2] <- 'vegvar'
vegvar

#' Solos sem ou fraca aptidao agricola ----------------------------------------
tsolo <- f_getgriperc(x=soil, y=gr)
head(tsolo[[1]])
tb_psolo <- do.call(rbind.data.frame, tsolo)
names(tb_psolo) <- c('perc')
tb_psolo$idu <- rownames(tb_psolo)

#' Uso do solo agricola combinado com veg de tipo agriflorestal ---------------
tusoagro <- f_getgriperc(x=ruso_agro, y=gr)
head(tusoagro[[1]])
tb_usoagro <- do.call(rbind.data.frame, tusoagro)
names(tb_usoagro) <- c('p_usoagro')
tb_usoagro$idu <- rownames(tb_usoagro)
names(tb_usoagro)

#' Area de conflito por quadricula -------------------------------------------- 
t_renamo <- f_getgriperc(x=rrenamo, y=gr)
head(t_renamo[[1]])
tb_renamo <- do.call(rbind.data.frame, t_renamo)
names(tb_renamo) <- c('p_conflict')
tb_renamo$idu <- rownames(tb_renamo)

#' Extract altitude -----------------------------------------------------------
gr_mod_alt <- raster::extract(alt ,gr, fun=f_mod0, df = T, na.rm=TRUE)
names(gr_mod_alt)[2] <- 'mod_alt'
gr_sd_alt <- raster::extract(alt ,gr, fun=sd, df = T, na.rm=TRUE)
names(gr_sd_alt)[2] <- 'sd_alt'

#' Extract biogeo vars --------------------------------------------------------
gr_mod_bio <- raster::extract(wcl ,gr, fun=f_mod0, df = T, na.rm=TRUE)
names(gr_mod_bio) <- sapply(strsplit(names(gr_mod_bio) , "_"), "[", 1)
#' COMBINAR VARIAVEIS NUM DATAFRAME ===========================================
#' Combinar variaveis ---------------------------------------------------------
vars <- cbind(
  # Biogeog
  gr_mod_bio
  ,gr_mod_alt['mod_alt']
  ,gr_sd_alt['sd_alt']
  # Fisionomia veg
  ,vegmod['veg_mod']
  ,vegvar['vegvar']
  # Humano
  ,gr_pop['poptot']
  ,tb_usoagro['p_usoagro']
  ,fire_tab['medfire']
  ,tb_renamo['p_conflict']
)
names(vars)
#' Quadriculas removidas das analises ----------------------------------------
zquad <- c(4,8,15,39,62,63,64,116,117,140,141,142,191,192,228
           ,243,297,335,336,344,347)
vars <- vars[!vars$ID %in% zquad, ]
#' Tabela e grupos  para analises ---------------------------------------------
vars$veg_mod <- factor(vars$veg_mod)
v_biogeo <- c("bio1","bio2","bio3","bio4","bio5"
              ,"bio6","bio7","bio8","bio9","bio10"
              ,"bio11","bio12","bio13","bio14","bio15"
              ,"bio16","bio17","bio18","bio19","mod_alt","sd_alt"
)
v_fision <- c("veg_mod","vegvar")
v_human <- c("poptot","p_usoagro","medfire","p_conflict")

#' Exportar tabela com todas as variaveis -------------------------------------
write.csv(vars, file=file.path('D:/Programacao/biogeo/data'
                               ,'variaveis_bioclimaticas.csv'))
gr_vars <- sp::merge(gr, vars, by.x='idu', by.y='ID')

pdf(paste('D:/Programacao/biogeo/output/Variaveis_Biogeograficas.pdf')
    ,paper='a4'
    ,onefile = T)
for(i in names(vars[,-1])){
  iplot <- spplot(
    gr_vars
    ,zcol=i
    ,main=paste0('variável: ',i)
  )

  print(iplot)
}
dev.off()
graphics.off()

writeOGR(gr_vars, dsn='D:/Programacao/biogeo/geodata'
         , driver='ESRI Shapefile', layer = 'gr_varsbiogeoclim'
         ,overwrite_layer = T)

vars_sp <- vars#[vars$ID %in% esp2[['idu']], ]
vars_sp$veg_mod <- factor(vars_sp$veg_mod)

writeOGR(grveg, dsn = 'D:/Sig/MozBiogeo/shp'
         , layer = 'grelha025veg_utm36s',
         driver = 'ESRI Shapefile', overwrite_layer=T)
