#' Install and load R Packages ------------------------------------------------
kpacks <- c('dplyr', 'raster', 'rgdal', 'ggplot2', 'mapproj', 'MODISTools')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Local folder: Alterar para a pasta local do Drive --------------------------
wd_dados <- 'C:/Users/Carlos Bento/Google Drive/biogeo/data' # Estou aqui!!
wd_geo <- 'D:/Sig/MozBiogeo/shp'

#' Ler a base de dados de fauna em .csv [comma delimited] ---------------------
data0 <- read.csv(file.path(wd_dados, 'fauna_12_7_2015_rev1.csv')
                  ,header = T, dec = '.', stringsAsFactors = F)
head(data0)

#wd_data <- 'C:/Users/Carlos Bento/Google Drive/biogeo/data'
#names(data0) <- tolower(names(data0))
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

data0 %>%
  dplyr::group_by(esp) %>%
  summarise(obs=n())
#' Exportar dados
write.table(data0, file = file.path(wd_data, 'fauna_12_7_2015_rev1_R.txt')
            ,sep = '\t', row.names = F)

#' Visualizar os dados espaciais ----------------------------------------------
#' Limites administrativos - provincias
#' Obtem a partir da net!
moz <- raster::getData(name='GADM', country='MOZ', level=1, download = T)
proj4string(moz) <- CRS('+init=epsg:4326')
moz@data$NAME_1[moz@data$NAME_1 == 'Nassa'] <- 'Niassa'

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

#' CRia objeto espacial a partir dos dados - classe spdf ----------------------
dadosg <- as.data.frame(data0)
coordinates(dadosg) <- ~longitude+latitude
proj4string(dadosg) <- CRS('+init=epsg:4326')
writeOGR(dadosg, dsn='S:/Mozambique/MozBiogeo/data', layer = 'dadosg'
         , driver="ESRI Shapefile", overwrite_layer=T)
length(
  unique(dadosg@data$idu)
)

#' Overlay dos dados aos temas vetoriais --------------------------------------
p1 <- as.data.frame(sp::over(dadosg, moz[,'NAME_1']))
data0$provincia <- p1[ ,1]
p2 <- as.data.frame(sp::over(dadosg, pa[,'name']))
data0$aprotegida <- p2[ ,1]

#' Sumarios graficos ----------------------------------------------------------
write.table(
  data0 %>%
    dplyr::left_join(tax0, by=c('species' = 'especie')) %>%
    dplyr::group_by(paste(nomecomum, sciname)) %>%
    dplyr::summarise(n=sum(number, na.rm = F)) %>%
    dplyr::arrange(desc(n)) %>%
    head(10)
  , file = 'clipboard', sep = '\t', row.names = F)

data0 %>%
  #select_('id', 'species') %>%
  #filter(!duplicated(species)) %>%
  dplyr::left_join(tax0, by=c('species' = 'especie')) %>%
  #select_('id', 'species', 'grupo')
  #dplyr::summarise(n=sum(number, na.rm = F))
  dplyr::filter(grupo == 'Carnívoro') %>%   #     <----------- Editar
  dplyr::group_by(grupo, nomecomum) %>%     #     <----------- Editar
  dplyr::summarise(n=sum(number, na.rm = F)) %>%
  #dplyr::ungroup()%>%
  #dplyr::select_('grupo', 'nomecomum', 'sciname', 'n')%>%
  dplyr::arrange(desc(n)) %>%
  #), quote=T ) %>%
  head(10) %>%
  ggplot(aes(x=reorder(nomecomum, -n), y=n)) +
  geom_bar(stat="identity") +
  #facet_grid(.~grupo, scales = 'free') +
  theme(axis.text.x=element_text(vjust = .2, angle=90))+
  labs(y='N Indivíduos', x='Espécie', title = 'Carnívoros')

#' Exporta o plot como imagem PNG para a pasta indicada no filename
ggsave(last_plot()
       , filename = 'D:/Programacao/biogeo/apresentacao/hist_carnivoros.png'
       , width = 7.5
       , height = 8
       , units = 'cm',
       dpi=300)

#' Sumario dos Dados por Provincia --------------------------------------------
sp_prov <- as.data.frame(data0 %>%
                           dplyr::group_by(provincia) %>%
                           dplyr::summarise(nind = sum(number, na.rm = F),
                                            nesp = length(unique(esp))) %>%
                           dplyr::arrange(desc(nind)) %>%
                           filter(complete.cases(.))%>%
                           #filter(nind>1000) %>%
                           tidyr::gather(provincia))
names(sp_prov)[2] <- 'var'
ggplot(aes(x=reorder(provincia, -value), y=value), data=sp_prov) +
  geom_bar(stat="identity") +
  facet_grid(var ~., scales = 'free') +
  theme(axis.text.x=element_text(vjust = .2, angle=90))+
  labs(x='Províncias', y='N')


#' Exporta o plot como imagem PNG para a pasta indicada no filename
ggsave(last_plot()
       , filename = 'D:/Programacao/biogeo/apresentacao/hist_nind_prov.png'
       , width = 7.13
       , height = 8
       , units = 'cm',
       dpi=300)

#' Dados por AP ---------------------------------------------------------------
sp_aprot <- as.data.frame(data0 %>%
                            dplyr::group_by(aprotegida) %>%
                            dplyr::summarise(nind = sum(number, na.rm = F),
                                             nesp = length(unique(esp))) %>%
                            dplyr::arrange(desc(nind)) %>%
                            filter(complete.cases(.))%>%
                            filter(nesp>23) %>%
                            tidyr::gather(aprotegida))
names(sp_aprot)[2] <- 'var'
ggplot(aes(x=reorder(aprotegida, -value), y=value), data=sp_aprot) +
  geom_bar(stat="identity") +
  facet_grid(var ~., scales = 'free') +
  theme(axis.text.x=element_text(vjust = .2, angle=90)) +
  labs(x='Área Protegida', y='N') + 
  scale_x_discrete(breaks = sp_aprot$aprotegida[1:(nrow(sp_aprot)/2)]
                   , labels=c("Niassa","Marromeu","C Of11","C Of10","C Of12"))

#' Exporta o plot como imagem PNG para a pasta indicada no filename
ggsave(last_plot()
       , filename = 'D:/Programacao/biogeo/apresentacao/hist_nind_ap.png'
       , width = 7.13
       , height = 8
       , units = 'cm',
       dpi=300)

#' Dados por Quad -------------------------------------------------------------
sp_grid <- as.data.frame(dadosg@data %>%
                           dplyr::group_by(idu) %>%
                           dplyr::summarise(nind = sum(number, na.rm = F),
                                            nesp = length(unique(esp))) %>%
                           dplyr::arrange(desc(nind)) %>%
                           #filter(complete.cases(.))%>%
                           #filter(nesp>2) %>%
                           tidyr::gather(idu)
)
names(sp_grid)[2] <- 'var'

spnind_quad <- idu %>% 
  left_join(as.data.frame(dadosg@data %>%
                            dplyr::group_by(idu) %>%
                            dplyr::summarise(nind = sum(number, na.rm = F),
                                             nesp = length(unique(esp)))),
            by=c('idu' = 'idu')) %>%
  mutate(nesp=ifelse(is.na(nesp),0, nesp),
         nind=ifelse(is.na(nind),0, nind))
write.csv(spnind_quad, file = 'D:/Programacao/biogeo/data/nspgrid05.csv')

nrow(spnind_quad[spnind_quad$nesp == 0,])/nrow(spnind_quad)


