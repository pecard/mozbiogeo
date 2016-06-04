#' Biogeo Analysis - Cluster hierarquico
#' ---A Explorar!---
#' http://www.r-bloggers.com/r-user-group-recap-heatmaps-and-using-the-caret-package/
#' http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R
#' http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' http://www.r-statistics.com/2015/06/dendextend-version-1-0-1-user2015-presentation/
#' http://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
#' http://koaning.io/posts/r-got-good-at-scraping.html?utm_content=buffer031de&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
#' ---Imagens apresentacoes---
#' http://www.girafamania.com.br/introducao/aprendendo_animais_antilopes.html
#' http://www.oocities.org/vila_luisa/Fauna_Especies.htm
#' 
#' Packages ------------------------------------------------------------------- 
kpacks <- c('dplyr', 'tidyr', 'rgdal', 'vegan', 'cluster'
            , 'reshape2', 'betapart', 'labdsv', 'magrittr'
            , 'ggplot2', 'dendextend', 'fuzzySim', 'ggdendro', 'colorspace'
            , 'FactoMineR'
            , 'RColorBrewer'
            ,'gridExtra')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Local Folders Paulo: alterar para dados locais
wd_dados <- 'D:/Programacao/biogeo/data'
wd_geo <- 'D:/SIG/MozBiogeo/shp'
wd_geo <- 'G:/SIG/MozBiogeo/shp' # drive ext

#' Local Folders CARLOS: alterar para dados locais
wd_dados <- 'C:/Users/Carlos Bento/Google Drive/biogeo/data' # Estou aqui!!
wd_geo <- 'C:/Users/Carlos Bento/Google Drive/biogeo/geodata/vetor'

#' dados de distribuicao <- check data.R:data0
data0
#' Ler a grelha 0.5 graus -----------------------------------------------------
gr <- rgdal::readOGR(dsn= file.path(wd_geo), layer='MOZ_grid050g')
proj4string(gr) <- CRS('+init=epsg:4326')
grdf <- fortify(gr)
idu <- gr@data

#' Centroides da grelha -------------------------------------------------------
ctd <- as.data.frame(coordinates(gr))
names(ctd) <- c('long', 'lat')
ctd <- cbind(idu, ctd)

ggplot(aes(x=long, y=lat, group=group), data=grdf) +
  geom_polygon(colour = 'white', fill = NA) +
  coord_equal() +
  geom_text(inherit.aes=F, aes(x=long, y=lat), label=idu, data = ctd, size=2)


#' sobrepor dados sp a grelha -------------------------------------------------
dadosg@data$idu <- sp::over(dadosg, gr)[ ,1]
dadosg@data %>%filter(idu==243) # quadricula costa sem dados

#' Sumario para analise de cluster --------------------------------------------
#' Subset dos dados de distribuicao ---
#' Aves ---
aves <- c('B.leadbeateri',  'B.regulorum', 'E.senegalensis'
          ,'G.carunculatus', 'L.crumeniferus','P.onocrotalus'
          ,'P.ruber')
#' Primata ---
primata <- c('C.albogularis', 'C.pygerythrus', 'P.cynocephalus'
             ,'P.ursinus')
#' small carnivore ---
scarnivore <- c('A.paludinosus', 'H.ichneumon', 'G.angolensis')
#' Rodent ---
rodent <- c('P.capensis')
#' Reptiles ---
reptile <- c('C.niloticus')

esp1 <- as.tbl(dadosg@data) %>%
  dplyr::filter(!esp %in% c(aves,primata,scarnivore,rodent,reptile))
esp1 <- esp1 %>%
  dplyr::select(idu, esp) %>% # Seleciona apenas as 2 variaveis
  dplyr::group_by(idu) %>% # Grupo para sumario
  dplyr::mutate(nsp = length(unique(esp))) %>% # nova var com n de esp/idu
  dplyr::filter(nsp > 2) %>% # filtra idu com mais de 2 esp
  dplyr::group_by(idu, esp) %>% # Grupos para sumario
  dplyr::summarize(n = 1) %>% # pivot esp por quad com valor = 1
  tidyr::spread(esp, n, fill = 0) %>%
  dplyr::filter(complete.cases(.))

esp2 <- dplyr::left_join(esp1, ctd, by='idu')

colnames(esp2) <- substr(colnames(esp2), 1,6)
names(esp2)
print(as.data.frame(names(esp2)), quote=T)

#' Fuzzy - Matriz de dissimiladidades -----------------------------------------
#' FuzzySim distPres
fuzsim <- distPres(esp2, sp.cols = 2:(ncol(esp2)-2)
                   , coord.cols = c("long", "lat")
                   , id.col = 1, p = 1, inv = TRUE)

head(fuzsim)
write.csv(fuzsim, file = 'D:/Programacao/biogeo/data/fuzzysim_38sp.csv')

#' Transpose da matriz para calculo relativo as quadriculas -------------------
fuzsimt <- transpose(fuzsim, sp.cols = 2:ncol(fuzsim), reg.names = 1)
head(fuzsimt)

#' For species
simmatb_esp <- simMat(fuzsim[ ,-1], method = "Baroni")
dsimmat_esp <- as.dist(1 - simmatb_esp)


# mar()
png(filename = 'D:/Programacao/biogeo/apresentacao/matrizsemelhanças_esp.png'
    ,width = 400
    ,height = 400
    ,units = 'px'
    ,bg=NA
    #,res=100
)
op <- par(cex=0.8, mar=c(11,11,.2,.2))
image(x=1:ncol(simmatb_esp), y=1:nrow(simmatb_esp), z=simmatb_esp, xlab=''
      , ylab='', axes = F)
axis(side = 1, at = 1:ncol(simmatb_esp), tick = F, labels = commonnames, las = 2
     , mgp=c(2 , 0.1, 0.1), col.axis='white')
axis(side = 2, at = 1:nrow(simmatb_esp), tick = F, labels = commonnames, las = 2
     ,mgp=c(2 , 0.1, 0.1), col.axis='white')
dev.off()
par(op)
dev.off()

#' For sites ------------------------------------------------------------------
simmat <- simMat(fuzsimt, method = "Jaccard")
simmatb <- simMat(fuzsimt, method = "Baroni")
head(simmatb)

dsimmat <- as.dist(1 - simmat)
dsimmatb <- as.dist(1 - simmatb) # Matriz de Distancias

#' Simpson Disimilarities -----------------------------------------------------

# dis <- vegdist(esp1[,-1], method="cao") # Distancia de CAO
#simdis <- beta.pair(esp1[ ,-c(1)], index.family="sor")[[1]] # Simpson

#' Cluster hierarquico --------------------------------------------------------
cluc <- hclust(dsimmatb, method = "ave")
plot(cluc)

#' Corte da arvore de classificacao para grupos -------------------------------
kgrupos <- 4
cut1 <- cutree(cluc, k=kgrupos)
as.data.frame(cut1)
kclu <- data.frame(idu=esp2$idu, clu=cut1)

#' Imagem ordenada da matriz de semelhança das quadriculas
png(filename = 'D:/Programacao/biogeo/apresentacao/matrizsemelhanças_grid.png'
    ,width = 400
    ,height = 400
    ,units = 'px'
    ,bg=NA
    #,res=100
)
op <- par(cex=0.8, mar=c(11,11,.2,.2))
image(as.matrix(simmatb), xlab='', ylab=''
      ,axes = F)
axis(side = 1, at = seq(0,1,length.out=length(cut1)), tick = F
     , labels = row.names(simmatb)[order(cut1)], las = 0
     , mgp=c(2 , 0.1, 0.1), col.axis='white')
axis(side = 2, at = seq(0,1,length.out=length(cut1)), tick = F
     , labels = row.names(simmatb)[order(cut1)], las = 0
     ,mgp=c(2 , 0.1, 0.1), col.axis='white')
dev.off()
par(op)
dev.off()


#' Cophenetics ----------------------------------------------------------------
cphcor <- cor(cophenetic(cluc), as.dist(dsimmatb)) # sites
mantel(cophenetic(cluc), as.dist(dsimmatb))
cphcor
ggclu <- ggdendro::dendro_data(cluc)
ggclu$labels$labeln <- as.numeric(as.character(ggclu$labels$label))
ggclu$labels <- left_join(ggclu$labels, kclu, by = c('labeln' = 'idu'))

gg1 <- ggplot() + 
  geom_segment(data=segment(ggclu), aes(x=x, y=y, xend=xend, yend=yend),
               colour = 'green') +
  geom_text(data=label(ggclu)
            ,aes(x=x, y=y, label=label
                 , colour = factor(clu)
                 , hjust=.5), size=2) +
  coord_flip() +
  theme_bw() +
  guides(colour=FALSE) +
  #labs(title = paste0("correlação cofenética: ", round(cphcor, 2)))
  png(filename = 'D:/Programacao/biogeo/apresentacao/dendro_grid.png'
      ,width = 6
      ,height = 8
      ,units = 'cm'
      ,bg=NA
      ,res=150
  )
gg1
dev.off()

#' Visualizacao da segregacao dos dados com PCO -------------------------------
cmds <- as.data.frame(cmdscale(dsimmatb, k=2))
ggplot(cmds, aes(x=V1, y=V2)) +
  geom_point(aes(colour=factor(cut1)), size=3) +
  theme_bw() +
  scale_colour_discrete('grupos')

#' Exportar dados para ligar ao shapefile no sig ------------------------------
#mclu <- data.frame(idu=esp1$idu, clu=cut1)
# idu <- data.frame(idu= 1:1234)
mclu_gr <- dplyr::left_join(gr@data, kclu, by='idu')
mclu_gr$clu[is.na(mclu_gr$clu)] <- 0
write.csv(kclu, file = file.path(wd_dados,'clu_fuzzy3_esp.csv'))

#' Merge cluster DataFrame a grid 0.5 -----------------------------------------
grclu <- sp::merge(gr, mclu_gr, by="idu")
grcludf <- grclu %>% fortify(.) %>%
  #dplyr::mutate(order1 = 1:nrow(.)) %>%
  dplyr::mutate(idn = as.numeric(id)+1) %>%
  sp::merge(mclu_gr, by.x ='idn', by.y='idu')

head(grcludf); tail(grcludf)
gplot <- ggplot(grcludf, aes(x=long, y=lat)) +
  geom_polygon(aes(fill=as.factor(clu), group = id), col = 'white') +
  geom_polygon(aes(x = long, y = lat, group = group)
               ,data=mozdf, aes.inherit=F, colour="black", fill = NA)+
  coord_equal()
gplot

#' Indval: Indicator Species --------------------------------------------------
ind1 <- labdsv::indval(esp2[ ,2:(ncol(esp2)-2)], cut1)

maxcls1 <- as.data.frame(cbind(as.matrix(ind1$maxcls)
                               ,as.matrix(ind1$indcls)
                               ,as.matrix(ind1$pval)))
names(maxcls1) <- c('cluster', 'Indval', 'pvalue')
maxcls1$sp <- row.names(maxcls1)
maxclsord <- maxcls1[order(maxcls1$cluster, maxcls1$Indval), ]

#' Selecao das especies significativas/representativas
write.table(
  maxclsord %>%
    filter(pvalue < 0.05, Indval > .3) %>%
    mutate(Indval=round(Indval,2))
  ,file = 'clipboard', sep = '\t', row.names = F)

#' Plot a UPGMA dendrogram from each similarity matrix ------------------------
#' Sites ------
clu <- hclust(dsimmat, method = "ward.D")
club <- hclust(dsimmatb, method = "ward.D")
plot(clu,  main = "Fuzzy cluster dendrogram", hang=-1, cex = .5)
plot(club,  main = "Fuzzy cluster dendrogram", hang=-1, cex = .5)

#' Especies ---
clu_esp <- hclust(dsimmat_esp, method = "ave")
#plot(clu_esp,  main = "Fuzzy cluster dendrogram", hang=-1, cex = .9)
dclu_esp <- as.dendrogram(clu_esp)
dclu_esp <- dendextend::hang.dendrogram(dclu_esp, hang_height=0.001)
#' Colorir o dendrograma com o IndVal
indval_dend <- unname(ind1$maxcls)
cols <- scales::hue_pal()(kgrupos+1)
#' [1] "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3" "#F564E3"
cols[2:(kgrupos+1)][indval_dend][order.dendrogram(dclu_esp)]
labels_colors(dclu_esp) <- scales::hue_pal()(kgrupos+1)[2:(kgrupos+1)][indval_dend][order.dendrogram(dclu_esp)]
#labels(dclu_esp) <- gsub('_D', '',labels(dclu_esp))
labels(dclu_esp) <- paste(strtrim(labels(dclu_esp), 6),
                          round(maxcls1$Indval[order.dendrogram(dclu_esp)],2))

dclu_esp <- dendextend::set(dclu_esp, "labels_cex", 0.7)
par(mar = c(3,1,1,3))
plot(dclu_esp, 
     #main = 'Fuzzy cluster dendrogram - Especies', 
     horiz =  T,  nodePar = list(cex = .007))  

#' PCA: Importancia das variaveis ---------------------------------------------
#'http://www.sthda.com/english/wiki/principal-component-analysis-how-to-reveal-the-most-important-variables-in-your-data-r-software-and-data-mining
#' library("FactoMineR")
#' PCA Biogeoclimatica
pca_biogeo <- FactoMineR::PCA(subset(vars, select = v_biogeo)
                              , graph = F)
print(pca_biogeo)
eigenv <- pca_biogeo$eig

#' PCA fisionomia da vegetacao
pca_human <- FactoMineR::PCA(subset(vars, select = v_human)
                              , graph = T)
print(pca_human)
eigenv <- pca_human$eig

head(eigenv[, 1:3])

#' Contribuicao das variaveis para os eixos da PCA ----------------------------
contrib <- as.data.frame(pca_biogeo$var$contrib)
str(contrib)
contrib <- as.data.frame(pca_human$var$contrib)
str(contrib)
#' Selecao de 6 variaveis mais importantes
dims <- c(row.names(contrib[order(contrib$Dim.1, decreasing=T), ])[1:2]
          ,row.names(contrib[order(contrib$Dim.2, decreasing=T),])[1:2])
#' Regex: ajustar nomes sem _37
dims #<- sapply(strsplit(dims , "_"), "[", 1)

#' Simpson Disimilarities -----------------------------------------------------
# dis <- vegdist(vars[,-1], method="cao") # Distancia de CAO
#simdis <- beta.pair(subset(vars, select = dims), index.family="sor")[[1]] # Simpson

#' Cluster das quad com as variaveis ambientais -------------------------------
kgr <- 2:6
for(i in kgr){
kcluster <- 
  kmeans(
  scale(
    subset(vars, select = dims)
    ), i
)

kclu_Var <- data.frame('idu'=as.numeric(names(kcluster$cluster))
                          , 'clu'=unname(kcluster$cluster))
mclu_gr_var <- dplyr::inner_join(gr@data, kclu_Var, by='idu')
write.csv(mclu_gr_var, file = file.path('D:/Programacao/biogeo/output',
                                         paste0('clugrid_human_k', i,'.csv')))

#' Merge cluster DataFrame a grid 0.5 -----------------------------------------
grclu_var <- sp::merge(gr, mclu_gr_var, by="idu", all.x=F)
grcludf_var <- grclu_var %>% fortify(.) %>%
  #dplyr::mutate(order1 = 1:nrow(.)) %>%
  dplyr::mutate(idn = as.numeric(id)+1) %>%
  dplyr::inner_join(mclu_gr_var, by=c('idn'='idu'))

#head(grcludf_var); tail(grcludf_var)
ggplot(grcludf_var, aes(x=long, y=lat)) +
  geom_polygon(aes(fill=as.factor(clu), group = group), col = 'white',
               size=.1) +
  geom_polygon(aes(x = long, y = lat, group = group)
               ,data=mozdf, aes.inherit=F, colour="black", fill = NA
               ,size=.1)+
  coord_equal() +
  theme_bw(base_size = 6, base_family = "")+ 
  scale_fill_discrete(name = "Human") +
  labs(list(x='', y=''))+
  guides(colour = guide_legend(override.aes = list(size=.2))
         ,fill = guide_legend(override.aes = list(size=.2)))

ggsave(filename=file.path(paste0('D:/Programacao/biogeo/output/clu_human_'
                          ,i,'cl.png'))
       ,plot=last_plot()
       ,height = 80, width = 80, units='mm', dpi=150)
}

dev.off()
#' Plots da PCA package factoextra --------------------------------------------
#install.packages("devtools")
library(devtools)
#install_github("kassambara/factoextra")
library('factoextra')
fviz_screeplot(pca_vars, ncp=6) +
  labs(title = "Variances - PCA",
       x = "Componentes Principais", y = "% Variância\nexplicada")
ggsave(last_plot()
       , filename = 'D:/Programacao/biogeo/apresentacao/pca_contrib.png'
       , width = 8
       , height = 7.8
       , units = 'cm',
       dpi=300)

head(pca_vars$var$coord)
fviz_contrib(pca_vars, choice = "var", axes = 2, top = 7)
fviz_pca_var(pca_vars, col.var="cos2", labelsize = 3) +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) +
  theme_minimal()

ggsave(last_plot()
       , filename = 'D:/Programacao/biogeo/apresentacao/pca_varsbiogeo.png'
       , width = 10
       , height = 7.8
       , units = 'cm',
       dpi=300)

#' dbRDA - capscale -----------------------------------------------------------
#' Gavin Simpson: http://r.789695.n4.nabble.com/R-question-about-capscale-vegan-td812694.html
#dist.mat <- vegdist(varespec, method = "bray")
#vare.cap2 <- capscale(dist.mat ~ N + P + K, 
#                      data = varechem, 
#                      comm = varespec) 
#vare.cap2

#' For species ---
# simmatb_esp <- simMat(fuzsim[ ,-1], method = "Baroni")
# dsimmat_esp <- as.dist(1 - simmatb_esp)
varsdist <- as.dist(dsimmatb, diag = FALSE, upper = FALSE)
simdis #simpson
head(varsdist)
names(vars_sp)[2:22] <- sapply(strsplit(names(vars_sp)[2:22] , "_"), "[", 1)


dbRDA <- capscale(varsdist ~ bio6+bio4+bio7+
                    alt+bio8+bio19
                  ,data=vars_sp,
                  comm=esp2[ ,-c(1,40,41)],
                  sqrt.dist= TRUE)
summary(dbRDA)
dbRDA0 <- capscale(varsdist ~ 1
                   ,data=vars_sp,
                   comm=esp2[ ,-c(1,40,41)],
                   sqrt.dist= TRUE)
anova(dbRDA, perm.max=99)
anova(dbRDA, by="axis", perm.max=99) # signif eixos
anova(dbRDA, by="terms", permu=99) # signif variaveis biogeo
model <- step(dbRDA0, scope = formula(dbRDA), test = "perm")
summary(model)
model$anova
modelback <- step(dbRDA, test = "perm")
summary(modelback)
modelback$anova

plot(model)
plot(dbRDA)

png(filename = 'D:/Programacao/biogeo/apresentacao/dbRDA.png'
    ,width = 700
    ,height = 700
    ,units = 'px'
    #,bg=NA
    ,res=120
)
par(cex=.8)
plot(dbRDA, dis = c("cn", "sp"))
dev.off()
#text(cex=0.5)

plot(dbRDA, type = "n", display = "sites")
points(dbRDA, display = "species")
points(dbRDA, display = "bp")
text(dbRDA, dis="cn", cex=.7, col = 'blue')

plot(dbRDA, type="n")
text(dbRDA, dis="cn", cex=.8)
points(dbRDA, pch=21, col="red", bg="yellow", cex=1.2)
text(dbRDA, "species", col="blue", cex=0.8)

#' Variance Partitioning RDA --------------------------------------------------

mod <- varpart(mite, ~ SubsDens + WatrCont, ~ Substrate + Shrub + Topo,
               mite.pcnm, data=mite.env, transfo="hel")
mod
showvarparts(3)
plot(mod)
# An alternative formulation of the previous model using
# matrices mm1 amd mm2 and Hellinger transformed species data
mm1 <- model.matrix(~ SubsDens + WatrCont, mite.env)[,-1]
mm2 <- model.matrix(~ Substrate + Shrub + Topo, mite.env)[, -1]
mite.hel <- decostand(mite, "hel")
mod <- varpart(mite.hel, mm1, mm2, mite.pcnm)
# Use RDA to test fraction [a]
# Matrix can be an argument in formula
rda.result <- rda(mite.hel ~ mm1 + Condition(mm2) +
                    Condition(as.matrix(mite.pcnm)))
anova(rda.result, step=200, perm.max=200)

#' GLM fuzzysim::multGLM ------------------------------------------------------
tab_glm <- cbind(esp2[ ,1]
                 , esp2[, which(names(esp2) == 'H.niger')]
                 #, vars_sp[, names(vars_sp) %in% dims]
                 , vars_sp[, -c(1)]
                 )
names(tab_glm)
glm_mod <- fuzzySim::multGLM(tab_glm, sp.cols = 2
                            , var.cols = c(3:21)
                            , id.col = 1
                            , step = T, FDR = F
                            , trim = TRUE,
                            family = 'binomial')
print(glm_mod)
glm_mod$predictions

gr_glm <- merge(gr, glm_mod$predictions, by = 'idu')

#' Glm stats:glm --------------------------------------------------------------
glm_fit <- glm(H.niger~.
               , family=binomial, data = tab_glm[,-c(1,22,23)])
glm_rfit <- step(glm_fit)
summary(glm_rfit)
#' Model fit based on R^2
(1-exp((glm_rfit$deviance - glm_rfit$null.deviance)/115))/
  (1-exp(-glm_rfit$null.deviance/115))
glm_rfit$fitted.values
#' Plot -----------------------------------------------------------------------
gr_glm <- merge(gr
                ,data.frame('fit'= glm_fit$fitted.values
                            , 'idu'= as.numeric(names(glm_fit$fitted.values)))
                , by = 'idu')
spplot(gr_glm['fit'], cuts = 4
       , col.regions = brewer.pal(5, "Greens"))

