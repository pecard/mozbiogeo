#' Codigo para tratamento dos dados PTT-GPS
#' 06-04-2016
#' Paulo E. Cardoso

#' A Explorar
#' http://lists.faunalia.it/pipermail/animov/2004-November/000013.html
#' https://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf
#' https://cran.r-project.org/web/packages/adehabitatLT/vignettes/adehabitatLT.pdf
#' ver readGPS

#' Packages -------------------------------------------------------------------
kpacks <- c("ggmap", 'ggplot2', 'RColorBrewer', 'lubridate', 'sp',
            'adehabitatHR','adehabitatLT', 'rgdal', 'dplyr', 'data.table',
            'animation', 'scales', 'gridExtra', 'raster', 'tidyr', 'rasterVis')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

# save.image("D:/Programacao/biogeo/homerange_marr.RData")
save.image("D:/Google Drive/sync/data/homerange_marr.RData") # paulo personal
save.image("C:/Users/PCardoso/Google Drive/sync/data/homerange_marr.RData") # paulo office
# load("D:/Programacao/biogeo/homerange.RData")
# load("D:/Programacao/biogeo/homerange_marr.RData")
#' Load Paulo Data
load("D:/Google Drive/sync/data/homerange_marr.RData") # paulo personal
load('C:/Users/PCardoso/Google Drive/sync/data/homerange_marr.RData') # paulo office

dsnwork <- 'C:/Users/PCardoso/Google Drive/sync/data/Dados Telemetria'

#'-----------------------------------------------------------------------------

#'# NIASSA 
xgpsn <- readOGR(dsn=dsnwork,
                 layer='gpsacq_buffalo_niassa_utm37s',
                 stringsAsFactors = F)
xgpsn[['data_hora']] <- ymd_hms(xgpsn[['DHACQ']])

#'# MARROMEU
xgps <- readOGR(dsn='D:/Sig/MozBiogeo/shp',
                layer='gpsacq_buffalo_marromeu_utm36s',
                stringsAsFactors = F)
xgps[['data_hora']] <-  mdy_hms(paste(xgps[['DACQ']]
                                      ,substr(xgps[['DHACQ']], 1,8)))

head(xgps@data)
summary(xgps[['data_hora']])

#'# Niassa
dftrajn <- xgpsn@data # reter o data.frame do shapefile
dftrajn <- dftrajn[order(dftrajn$HERD, dftrajn$data_hora), ] # ordenar
dftrajn$idlinha <- seq(1:nrow(dftrajn))
dftrajn$uhour <- hour(dftrajn$data_hora) # variavel hora

#'# Marromeu: dataframe inicial
dftraj <- xgps@data # reter o data.frame do shapefile
dftraj <- dftraj[order(dftraj$HERD, dftraj$data_hora), ] # ordenar
dftraj$idlinha <- seq(1:nrow(dftraj))
dftraj$uhour <- hour(dftraj$data_hora) # variavel hora

#' Eliminar posicoes obtidas com o mesmo time_stamp ---------------------------
#'# Niassa
dftraj.1n <- dftrajn %>% dplyr::distinct(HERD, DACQ, uhour)
dftraj.1n[duplicated(dftraj.1n[, c('HERD', 'data_hora')]), ] # check dupl.

#'# Marromeu
dftraj.1 <- dftraj %>% dplyr::distinct(HERD, DACQ, uhour)
dftraj.1[duplicated(dftraj.1[, c('HERD', 'data_hora')]), ] # check dupl.

#' Construcao das Trajectorias ------------------------------------------------
#'# Niassa: objeto ltraj
xtrajn <- adehabitatLT::as.ltraj(xy = dftraj.1n[ ,c("LON","LAT")]
                                 ,date = dftraj.1n$data_hora, id = dftraj.1n$HERD)
xtrajn_all <- adehabitatLT::ld(xtrajn) # Converte para dataframe
xtrajn_all[is.na(xtrajn_all$R2n), ] # check NA
xtrajn_all <- subset(xtrajn_all, dist <= 5000) # REMOVE DIST > 5000m

#'# Marromeu
xtrajm <- adehabitatLT::as.ltraj(xy = dftraj.1[ ,c("y","x")]
                                 ,date = dftraj.1$data_hora, id = dftraj.1$HERD)
xtrajm_all <- adehabitatLT::ld(xtrajm) # Converte para dataframe
xtrajm_all[is.na(xtrajm_all$R2n), ] # check NA

#' Aqui ha problemas com grandes distancias. E preciso perceber ---------------
#' a natureza do problema
summary(xtrajm_all[xtrajm_all$dist > 5000, ]$dist)
summary(xtrajn_all[xtrajn_all$dist > 5000, ]$dist)

#'# Eliminar distancias duperiores a 5000m ------------------------------------
#'# Niassa
xmovn <- xtrajn_all
xmovn <- xmovn[order(xmovn$date, xmovn$id), ]
xmovn <- xmovn[complete.cases(xmovn), ]

#'# Marromeu
xmov <- xtrajm_all[xtrajm_all$dist < 5000, c('x', 'y', 'date', 'id', 'dist')]
xmov <- xmov[order(xmov$date, xmov$id), ]
xmov <- xmov[complete.cases(xmov), ]

#' Link ACTINACT --------------------------------------------------------------
#'# Niassa
xmovn %>%
  dplyr::left_join(dplyr::select(dftraj.1n, c(data_hora, ACTINACT))
                   , by = c("date" = "data_hora")) %>%
  dplyr::filter(ACTINACT == "INACT") %>%
  head()

#' Distancia minima a linha de agua -------------------------------------------
#' m1 obtido a partir das funcoes distance_point_line.R
xmovn$rdist <- m1

#'# Funcao para distinguir dia e noite ----------------------------------------
#'# Por testar!
calcDayNight <- function(x) {
  # if time is greater than the sunrise time or less than the sunset time,
  # apply DAY
  suntime <- suncalc(as.numeric(as.Date(x["date"], format="%y-%m-%d") - 
                                  as.Date("2013-01-01")), Lat=41.6, Long=-81.4)
  coytime <- as.numeric(strptime(x["LMT_TIME"], "%T") - strptime("00:00:00", "%T"))
  
  if(coytime > suntime$sunrise & coytime < suntime$sunset){
    x["dayornight"] <- "DAY"
  } else x["dayornight"] <- "NIGHT"
}
#'# Criar intervalos de tempo: Mes e Estacao -----------------------------------
#'# Niassa
xmovn$monthyy <- cut(xmovn$date, 'month') # mes do ano
xmovn$season <- cut(month(xmovn$date), breaks = c(1,3,6,9, +Inf)
                    , labels = c('1st trimester', '2nd trimester'
                                 , '3th trimester', '4th trimester')
                    , include.lowest = T)

#'# Marromeu
xmov$monthyy <- cut(xmov$date, 'month') # mes do ano
xmov$season <- cut(month(xmov$date), breaks = c(1,3,6,9, +Inf)
                   , labels = c('1st trimester', '2nd trimester'
                                , '3th trimester', '4th trimester')
                   , include.lowest = T)


#' Exportar tabela
#write.csv(xtraj_all, file='D:/Sig/MozBiogeo/data/xtraj_GPSACQ_BUFFALO.csv')

#' Codigo dos Individuos ------------------------------------------------------
#' Marromeu: Buf580084 Buf580086 Buf580088 Buf580089 Buf580090
#' Niassa: H01 H02 H03 H04 H05 H06 H09 H10 H11
unique(xmov$id)
unique(xmovn$id)

summary(xmov[xmov$id == 'Buf580089', ])

#'# ================================ OUTPUTS ==================================
#'
#' Daqui para baixo a analise faz-se para o objeto DTtraj e altera-se a origem
#' 
#'#============================================================================

#'# facet labels
flabels <- function(variable, value){
  value <- droplevels(value)
  names_li <- list("(0,250]"="(0-0.25]","(250,500]"="(0.25-0.5]"
                   ,"(500-2.5e+03]"="(0.5-2.5]", "(2.5e+03,5e+03]"="(2.5-5]")
  return(names_li[value])
}

breaks <- c(0,250,500,2500,5000, +Inf) # Classes de distancia

#'# converte dataframe para Data Table ----------------------------------------
#'# NIASSA converte para objeto data.table ===========
DTtraj <- data.table(xmovn)
DTtraj$distInt <- cut(DTtraj$dist, breaks=breaks)

#'# MARROMEU converte para objeto data.table =========
DTtraj <- data.table(xmov) 
DTtraj$distInt <- cut(DTtraj$dist, breaks=breaks)

#'# Delta X
head(DTtraj)
ggplot(aes(x=as.Date(date), y=abs((x[1]-x))/1000)
       , data=subset(DTtraj, dist<=5000)) +
  #geom_smooth(linetype=1, span = 0.8) +
  geom_line(aes(group=id, colour=id)) +
  facet_grid(id~., scales = 'free_y') +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("2 month")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position='none') +
  labs(list(x='Date', y='Delta X/1k'))

DTtraj[dist<=5000, mean(rdist, na.rm=T),
       by=list(id, monthyy)] %>%
  ggplot(aes(x=as.Date(monthyy), y=V1/1000)) +
  geom_smooth(linetype=1, span = 0.2) +
  #geom_path() +
  #stat_summary(aes(colour=monthyy, group=id),
  #             geom="line", fun.y="mean", size=1.2, alpha= .8) +
  #facet_grid(distInt~., scales = 'free') +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("2 month")) +
  labs(list(x='month', y='distancia media (km)')) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)
        ,legend.position = 'none') 

#'# Delta Y
head(DTtraj)
ggplot(aes(x=as.Date(date), y=abs((y[1]-y))/1000), data=DTtraj) +
  geom_line(aes(group=id, colour=id)) +
  facet_grid(id~., scales = 'free_y') +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("2 month")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        legend.position='none') +
  labs(list(x='Date', y='Delta Y/1k'))

#' Analise por Hora e movimentos sucessivos <= 5000m --------------------------
#'# Por individuo
DTtraj[dist<=5000, list(dist=sum(dist, na.rm=T)),
       by=list(id, hour(date), season, distInt)] %>%
  ggplot(aes(x=factor(hour), y=dist/1000),aes(colour=distInt, group=distInt)) +
  geom_smooth(linetype=1, span = 0.2) +
  stat_summary(geom="line", fun.y="mean", size=1.2, alpha=.8) +
  facet_grid(distInt~., scales = 'free', labeller = flabels) +
  labs(list(x='hora', y='distancia media (km)')) +
  theme(legend.position = 'bottom') +
  guides(colour=guide_legend(nrow=2))
#'# Agrupados
DTtraj[dist<=5000, list(dist=mean(dist, na.rm=T)),
       by=list(id, hour(date), season, distInt)] %>%
  ggplot(aes(x=factor(hour), y=dist)) +
  aes(group=distInt, colour = distInt) +
  stat_summary(geom="line", fun.y="mean", size=1.2, alpha=.8) +
  facet_grid(distInt~., scales = 'free', labeller = flabels)+
  labs(list(x='hora', y='distancia media (km/h)')) +
  theme(legend.position = 'none') +
  guides(colour=guide_legend(nrow=1, title="classe"))

#' Analise horaria por trimestre e classe de distancia em movimentos sucessivos
DTtraj[dist>2500 & dist <= 5000,
       list(distm=mean(dist, na.rm=T)),
       by=list(id, hour(date), season, distInt)] %>%
  ggplot(aes(x=factor(hour), y=distm)) +
  aes(group=distInt, colour = distInt) +
  stat_summary(geom="line", fun.y="mean", size=1.2, alpha=.8) +
  facet_grid(season~distInt, scales = 'free')+
  labs(list(x='hora', y='distancia media(km/h)')) +
  theme(legend.position = 'none') +
  guides(colour=guide_legend(nrow=1))

#' Analise por Estacao  e movimentos sucessivos < 5000m -----------------------
DTtraj[dist <= 5000, sum(dist, na.rm=T),
       by=list(id, distInt, season)] %>%
  ggplot(aes(x=factor(season), y=V1/1000)) +
  stat_summary(aes(colour=distInt, group=distInt),
               geom="line", fun.y="mean", size=1.2, alpha= .8) +
  facet_grid(distInt~., scales = 'free')+
  labs(list(x='season', y='distancia media (km)')) +
  theme(legend.position = 'none')

#' Analise por Mes  e movimentos sucessivos < 5000m ---------------------------
DTtraj[dist <= 5000, sum(dist, na.rm=T),
       by=list(id, distInt, monthyy)]%>%
  ggplot(aes(x=factor(monthyy), y=V1/1000)) +
  stat_summary(aes(colour=distInt, group=distInt),
               geom="line", fun.y="mean", size=1.2, alpha= .8) +
  facet_grid(distInt~., scales = 'free') +
  labs(list(x='month', y='distancia media (km)')) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)
        ,legend.position = 'none') 

#' Distancia ao rio ----------------------------------------------------------
#' Analise por Mes e movimentos sucessivos < 5000m ---------------------------
DTtraj[dist<5000, mean(rdist, na.rm=T),
       by=list(id, monthyy)] %>%
  ggplot(aes(x=as.Date(monthyy), y=V1/1000)) +
  geom_smooth(linetype=1, span = 0.2) +
  #geom_path() +
  #stat_summary(aes(colour=monthyy, group=id),
  #             geom="line", fun.y="mean", size=1.2, alpha= .8) +
  #facet_grid(distInt~., scales = 'free') +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("2 month")) +
  labs(list(x='month', y='distancia media (km)')) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)
        ,legend.position = 'none') 

##
traj_dist <- xtrajm_all %>%
  mutate(dia=day(date), hora=hour(date))%>%
  group_by(ind=id, dia, hora)%>%
  summarise(tdist=sum(dist, na.rm=T))

ggplot(aes(V1)) +
  stat_summary(aes(colour=ind), geom="line", fun.y="mean", size=2)

#'# Analise de base: Kernel Areas ------------------------------------------
#'# Marromeu
locs <- xmov[, c('id', 'monthyy', 'x', 'y')]
locs$idm <- paste(locs$id, locs$monthyy)
locs <- locs[complete.cases(locs), ]
coordinates(locs) <- c('x', 'y')
proj4string(locs) <- CRS('+init=epsg:32736') # utm 36s
hr.k <- kernelUD(locs[ ,3])
#proj4string(hr.ker) = CRS("+init=epsg:3763") #! projeçao PTTM-06

#'# Niassa
locsn <- xmovn[, c('id', 'monthyy', 'x', 'y')]
locsn$idm <- paste(locsn$id, locsn$monthyy)
locsn$herd <- as.character(locsn$id)
locsn <- locsn[complete.cases(locsn), ]
locsn <- locsn %>% filter(herd != 'H10')
coordinates(locsn) <- c('x', 'y')
proj4string(locsn) <- CRS('+init=epsg:32737') # utm 37s
hr.k <- kernelUD(locsn[ ,3])

#'#### Kernel .95 ----------------------------------------------------------
nk <- 95
hr.ker95 <- getverticeshr(hr.k, nk)
#' individualizar os fatores id e data
ids <- strsplit(as.character(hr.ker95@data$id), "\\s+")
hr.ker95@data$idu <- do.call(rbind, ids)[ ,1]
hr.ker95@data$data <- do.call(rbind, ids)[ ,2]
#' associar projecao ao spdf
proj4string(hr.ker95) <- CRS("+init=epsg:32736") #! EPSG do UTM 37S 32737
#' transformar projecao para geograficas
hr.ker95wgs84 <- spTransform(hr.ker95, CRS('+init=epsg:4326'))

hr.ker95@data %>%
  ggplot(aes(x=factor(data), y=area)) +
  geom_path(aes(colour=idu, group=idu)) +
  labs(list(x='month', y='home range (m2)')) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)
        ,legend.position = 'bottom') 


#'#### Exportar shapefiles para cada mes - alterar area!
writeOGR(hr.ker95, dsn = 'D:/Sig/MozBiogeo/shp'
         , layer = paste0('kernel_', nk,'_niassa')
         ,driver = 'ESRI Shapefile', overwrite_layer=T)
#'#### Exportar Tabela com o home-range (km2)
write.csv(as.data.frame(hr.ker95),
          file = file.path('D:/Sig/MozBiogeo', 'areas_vitaisk95_marromeu.csv'))



#'### Figura 22 Mapa ================================================================
hr.kerm95df <- fortify(hr.kerm95wgs84)
hr.kerm95df$ano <- as.numeric(substr(hr.kerm95df$id, 1, 4))
hr.kerm95df$mes <- as.numeric(substr(hr.kerm95df$id, 6, 7))
str(hr.kerm95df)
hr.kerm95df <- hr.kerm95df[hr.kerm95df$ano %in% c(2009:2014), ]

bbox <- ggmap::make_bbox(long, lat, hr.kerm95df, f = 0.1)
map_loc <- get_map(location = bbox, source = 'google')
map <- ggmap(map_loc, extent = 'device', maprange=FALSE, darken = c(0.5, "white"))
fig22 <- map +
  geom_path(aes(x = long, y = lat, group = group), colour = "#1f78b4",
            data = hr.kerm95df) +
  geom_point(aes(x = Longitude, y = Latitude), size = 1.2,
             data = ag.bsj) +
  facet_wrap(~ mes, ncol = 3)
ggsave(filename = file.path(outdir1, paste0("fig22_area_vital_mes2014_Bonelli.png")),
       plot = fig22, dpi = 96, height = 18, width = 14, unit = 'cm')

#'### Mapa ================================================================
##### Kernel Areas: Periodo dia
locs <- subset(gps, ano == 2014, select = c('per_dia', 'Longitude.E.', 'Latitude.N.'))
coordinates(locs) <- c('Longitude.E.', 'Latitude.N.')
proj4string(locs) <- CRS('+init=epsg:4326')
locs <- spTransform(locs, CRS('+init=epsg:3763'))
hr.k <- kernelUD(locs[ ,1])
#proj4string(hr.k) = CRS("+init=epsg:3763") #! EPSG do UTM 33S 32733 (32633 UTM 33N)
nk <- 95
hr.kerm95 <- getverticeshr(hr.k, nk)
proj4string(hr.kerm95) <- CRS("+init=epsg:3763") #! EPSG do UTM 33S 32733 (32633 UTM 33N)
hr.kerm95wgs84 <- spTransform(hr.kerm95, CRS('+init=epsg:4326'))
##### Exportar shapefiles para cada ano
writeOGR(hr.kerm95, dsn = outdir1, layer = paste0(nk,'kernel_perdia2014_pttm06'),
         driver = 'ESRI Shapefile', overwrite_layer=T)
##### Exportar Tabela com o home-range (km2)
write.table(as.data.frame(hr.kerm95), row.names = F, sep = '\t',
            file = file.path(outdir1, paste0(nk,'kernel95_perdia2014_pttm06.txt')))
##### Mapa
hr.kerm95df <- fortify(hr.kerm95wgs84)
str(hr.kerm95df)
hr.kerm95df$idf <- factor(hr.kerm95df$id)
hr.kerm95df$idf <- factor(hr.kerm95df$idf, levels=c('(5,11]', '(11,15]','(15,21]'),
                          labels=c("06-12h","12-15h", "16-21h"))
bbox <- ggmap::make_bbox(long, lat, hr.kerm95df, f = 0.5)
map_loc <- get_map(location = bbox, source = 'google', maptype = "roadmap")
map <- ggmap(map_loc, extent = 'device', maprange=FALSE, darken = c(0.5, "white"))
fig28 <- map +
  geom_path(aes(x = long, y = lat, group = group, colour = idf),
            data = hr.kerm95df) +
  geom_point(aes(x = Longitude, y = Latitude), size = 1.2,
             data = ag.bsj) +
  labs(colour = "Per?odo\ndo dia") 

ggsave(filename = file.path(outdir1, paste0("fig28_area_vital_per_dia_2014_Bonelli.png")),
       plot = fig28, dpi = 96, height = 12, width = 14, unit = 'cm')
