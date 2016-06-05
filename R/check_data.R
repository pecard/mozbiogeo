#' Install and load R Packages ------------------------------------------------
kpacks <- c('dplyr', 'tidyr', 'rgdal', 'vegan', 'cluster'
            , 'reshape2', 'betapart', 'labdsv', 'magrittr'
            , 'ggplot2', 'dendextend', 'fuzzySim', 'ggdendro', 'colorspace'
            , 'FactoMineR'
            , 'RColorBrewer'
            , 'gridExtra'
            , 'lubridate')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Session Data
#' /mozbiogeo_data/biogeo.RData
#' Local folder: Alterar para a pasta local do Drive -------------------
wd_dados <- 'D:/Dropbox/programacao/mozbiogeo_data' # Estou aqui!!
wd_geo <- 'D:/Sig/MozBiogeo/shp'

#' Ler a base de dados de fauna em .csv [comma delimited] ---------------------
data0 <- read.table(file.path(wd_dados, 'fauna_12_7_2015_rev2_R.txt')
                  ,header = T, dec = '.', sep = '\t'
                  ,quote = "", stringsAsFactors = F)
head(data0)
names(data0) <- tolower(names(data0))

data0 <- as.tbl(data0) %>%
  dplyr::select(-c(x,x.1))

#' Remover espacos dos nomes das variaveis ------------------------------------
names(data0) <- gsub("[[:punct:]]","",names(data0))
head(data0)

#' Remover espacos no inicio ou fim do nome
data0$esp <- sub("[[:space:]]","",data0$species)

#' Visualizar eventuais espacos indesejados
print(as.data.frame(sort(unique(data0$esp))), quote=T)

#' Verificar Coordenadas = Zero ou positivas
data0[data0$latitude == 0, ]
data0$latitude[data0$latitude > 0]

#' Corrigir dados errados
data0$number[data0$number == ''] <- "2"
data0$number[data0$number == 'tracks'] <- "1"
data0 <- data0%>%
  mutate(number =as.numeric(number))


#' Summary: Total observations by species
data0 %>%
  dplyr::group_by(esp) %>%
  dplyr::summarise(obs=n()) %>%
  dplyr::arrange(desc(obs))

#' Deal with dates ------------------------------------------------------------
#' data0$date0 <- dmy(data0$date)
#' data0$year <- year(data0$date[1])

#' Export Dataset
write.table(data0, file = file.path(wd_data, 'fauna_12_7_2015_rev1_R.csv')
            ,sep = '\t', row.names = F)

#' Visualizar os dados espaciais ----------------------------------------------
#' Limites administrativos - provincias
#' Obtem a partir da net!
moz <- raster::getData(name='GADM', country='MOZ', level=1, download = T)
proj4string(moz) <- CRS('+init=epsg:4326')
moz@data$NAME_1[moz@data$NAME_1 == 'Nassa'] <- 'Niassa'
moz@data$NAME_1[moz@data$NAME_1 == 'Maputo City'] <- 'Maputo'

#' Exportar shapefile GADM: !sao os mesmos dos nossos mapas GADM
#' Grava o shapefile pasta D:/ no disco local. Alterar!
rgdal::writeOGR(moz, dsn='D:', driver='ESRI Shapefile', layer = 'moz_R'
                ,overwrite_layer = T)

#' Rede de Areas Protegidas de MOZ --------------------------------------------
#' Le a partir da pasta wd_geo no disco local
pa <- rgdal::readOGR(dsn=file.path(wd_geo)
                     ,layer = 'general_MOZ-shapefile-polygons'
                     ,stringsAsFactors=F)
proj4string(pa) <- CRS('+init=epsg:4326')

#' Plot dos dados sobre o mapa ------------------------------------------------
mozdf <- fortify(moz) # converter em data.frame
ggplot(data = mozdf, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour="black", fill='grey') +
  geom_point(data = data0, aes(x=longitude, y = latitude)
             ,size=.5, inherit.aes=FALSE, col='red') +
  coord_map() +
  theme_bw()

#' Taxonomy: Basic info for taxonomic manipulation ----------------------------
tax0 <- read.csv(file.path(wd_dados, 'taxonomia54.csv'), header = T, sep = ','
                   ,stringsAsFactors = F)
#' Sumarios graficos ----------------------------------------------------------
write.table(
  data0 %>%
    dplyr::left_join(tax0, by=c('species' = 'especie')) %>%
    dplyr::group_by(paste(nomecomum, sciname)) %>%
    dplyr::summarise(n=sum(number, na.rm = F)) %>%
    dplyr::arrange(desc(n)) %>%
    head(10)
  , file = file.path(wd_dados, 'top10.txt'), sep = '\t', row.names = F
  )

data0 %>%
  #select_('id', 'species') %>%
  #filter(!duplicated(species)) %>%
  dplyr::left_join(tax0, by=c('species' = 'especie')) %>%
  #select_('id', 'species', 'grupo')
  #dplyr::summarise(n=sum(number, na.rm = F))
  #dplyr::filter(grupo == 'Carnívoro') %>%   #     <----------- Editar
  #dplyr::group_by(grupo, nomecomum) %>%     #     <----------- Editar
  dplyr::group_by(nomecomum) %>%             #     <----------- Editar
  dplyr::summarise(n=sum(number, na.rm = F)) %>%
  #dplyr::ungroup()%>%
  #dplyr::select_('grupo', 'nomecomum', 'sciname', 'n')%>%
  dplyr::arrange(desc(n)) %>%
  #), quote=T ) %>%
  head(10) %>%
  ggplot(aes(x=reorder(nomecomum, -n), y = n)) +
  geom_bar(stat="identity") +
  #facet_grid(.~grupo, scales = 'free') +
  theme_classic() +
  theme(axis.text.x=element_text(hjust = 0.3, vjust = 0.3
                                 , angle=90 + 1e-01))+
  labs(y='N. indivíduos', x='Espécie'
       #, title = 'Carnívoros'
       )
#' Exporta o plot como imagem PNG para a pasta indicada no filename
ggsave(last_plot()
       , filename = 'D:/Dropbox/programacao/mozbiogeo/png/hist_carnivoros.png'
       , width = 7
       , height = 6
       , units = 'cm',
       dpi=300)

#' --------------------------- SPATIAL DATA -----------------------------------

#' Create SPDF object for Observations ----------------------------------------
dadosg <- as.data.frame(data0)
coordinates(dadosg) <- ~longitude+latitude
proj4string(dadosg) <- CRS('+init=epsg:4326')
writeOGR(dadosg, dsn='S:/Mozambique/MozBiogeo/data', layer = 'dadosg'
         , driver="ESRI Shapefile", overwrite_layer=T)
length(
  unique(dadosg@data$idu)
)
