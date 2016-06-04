#' NASA FIRMS
kpacks <- c('dplyr', 'lubridate', 'rgdal', 'tidyr', 'magrittr'
            , 'ggplot2', 'scales')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#save.image("D:/Programacao/biogeo/NASAFIRMS_2005-2015.RData")
load("D:/Programacao/biogeo/NASAFIRMS_2005-2015.RData")
wd_geo <- 'D:/SIG/MozBiogeo/shp'

gr <- rgdal::readOGR(dsn= file.path(wd_geo), layer='MOZ_grid050g')
proj4string(gr) <- CRS('+init=epsg:4326')
summary(gr)

ap <- rgdal::readOGR(dsn= file.path(wd_geo), layer='ap_buffer_inout_10km_wgs84')
proj4string(ap) <- CRS('+init=epsg:4326')
summary(ap)

fire <- rgdal::readOGR(dsn=file.path('D:/Sig/MozBiogeo/shp/firms')
                       ,layer = 'firms218371430920801_MCD14ML'
                       ,stringsAsFactors=F)
proj4string(fire) <- CRS('+init=epsg:4326')
summary(fire)
fire <- fire[""]
fire@data$idu <- sp::over(fire, gr[,'idu'])$idu
fire@data$bufap <- sp::over(fire, ap[,'nbuf'])$nbuf

firetbl <- as.tbl(fire@data)
firetbl$ACQ_DATE <- ymd(firetbl$ACQ_DATE)
firetbl$ano <- year(firetbl$ACQ_DATE)
firetbl$mes <- lubridate::month(firetbl$ACQ_DATE)
firetbl$bufap <- fire@data$bufap
length(
  sort(unique(firetbl$mes[firetbl$ano == 2001]))
  )

firetbl %>%
  filter(ano>2000, ano<2015, CONFIDENCE > 75) %>%
  group_by(ano) %>%
  summarise(n=length(N))

fire_tab <- firetbl %>%
  filter(ano>2000, ano<2015, CONFIDENCE > 75) %>%
  dplyr::group_by(idu) %>%
  dplyr::summarise(nfire=length(N)
                   ,medfire = round(nfire/14, 1)
                   )%>%
  dplyr::filter(complete.cases(.)) %>%
  merge(gr@data, by='idu', all=T) %>%
  dplyr::mutate(nfire=ifelse(is.na(nfire),0,nfire)
                ,medfire=ifelse(is.na(medfire),0,medfire))
fire_tab 
write.csv(fire_tab, file='D:/Programacao/biogeo/data/fire2001-2014_cfdce75.csv')

#' Fogos dentro e fora das areas protegidas -----------------------------------
fire_aptab <- firetbl %>%
  filter(ano>2010
         , CONFIDENCE > 80
         #, bufap %in% c('Niassa_out'
         #               ,'Niassa_in'
  ) %>%
  dplyr::group_by('Ano'=ano
                  ,'Mes' = mes
                  ,'Area'=bufap) %>%
  dplyr::summarise('NPixel' = length(N)) %>%
  dplyr::mutate(AP=gsub("\\_.*","", Area)
                ,Ano_Mes = ymd(paste(Ano, Mes,01))
  ) %>%
  dplyr::filter(!is.na(Area))

ggplot(aes(x=as.Date(Ano_Mes), y=NPixel), data=fire_aptab) +
  geom_line(aes(group=Area, colour=Area)) +
  facet_grid(AP~.) +
  #scale_x_continuous(breaks=unique(fire_aptab$ano)) +
  scale_x_date(labels = date_format("%m-%Y")
               ,breaks = date_breaks("month")) +
  theme(axis.text.x = element_text(angle = 90
                                   ,hjust = 0.5
                                   ,vjust = 0.5))

write.table(fire_aptab, file = 'clipboard', row.names = F)

spplot(
  sp::merge(gr, fire_tab, by = 'idu')
  ,zcol='n'
  ,main='número médio de fogos/ano\n2005-2014'
)

