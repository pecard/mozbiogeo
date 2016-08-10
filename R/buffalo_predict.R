kpacks <- c('dplyr', 'tidyr', 'rgdal'
            , 'ggplot2', 'dismo', 'raster', 'rJava')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Inicial configuration
#' Opcao para utilizar um 4GB de RAM
#' Useful when receiving message: java.lang.OutOfMemoryError
options(java.parameters="-Xmx1g")

#' Set directories for conde and data
wd_dados <- 'D:/Dropbox/programacao/mozbiogeo_data'
wd_geo <- 'D:/SIG/MozBiogeo/shp'

#Load data on Afican Buffalo Prensence only
buf <- read.table(file.path(wd_dados, 'buffalo_moz.txt'), header = T, sep = '\t',
                  stringsAsFactors = F, dec = '.')
View (buf)

#' Carregar os layers de trabalho (tem que ter o mesmo tamanho e mesma resolucao)
layers <- stack(list.files("./layers","asc",full.names=TRUE))
#names(layers)<-sub("*.asc$","",list.files("./layers","asc",full.names=FALSE))
#layers<-readAll(layers)
plot(layers)
plot(layers[[1]])
layers #Ver as caracteristicas dos layers
layers[[1]] #Ver as caracteristicas de um layer particular
points(buffalopresence[,2:3],pch=18,cex=0.6)
#Run model (correr o modelo sobre um layer de ecoregiao)
me <- maxent(layers, buffalopresence[,2:3], factors='ecoreg', removeDuplicates=TRUE, 
             path="./outputR")

#Formato SWD (samples with data)
pres.covs<-extract(layers, buffalopresence[,2:3])
pres.covs<-na.omit(pres.covs)
pres.covs<-unique(pres.covs)
View(pres.covs)

#Uma maneira mais "correta" para eliminar as duplicatas de uma mesma c?lula
pres.covs<-extract(layers, buffalopresence[,2:3],cellnumbers=T)
View(pres.covs)
pres.covs<-na.omit(pres.covs)
pres.covs<-unique(pres.covs)
pres.covs<-pres.covs[,-1] #Remover o n?mero de c?lulas da coluna

#Formato SWD: Dados com o background
bkg.covs<-sampleRandom(layers,10000,cells=T)#Cria 10000 pontos aleatorios
bkg.covs<-unique(bkg.covs)
bkg.covs<-bkg.covs[,-1] #Remover o n?mero de c?lulas da coluna

# Condensar tudo em uma ?nica tabela
env.values<-data.frame(rbind(pres.covs,bkg.covs)) #rbind une tabelas verticalmente
env.values$ecoreg<-as.factor(env.values$ecoreg) #Importante quando vari?veis categ?ricas s?o usados

#Etiquetar presencas (1) e background (0)
y <- c(rep(1,nrow(pres.covs)), rep(0,nrow(bkg.covs)))
me <- maxent(env.values, y, args=c("addsamplestobackground=true"), 
             path="./outputR")

#Visualizar os resultados
map <- predict(me, layers, progress="text")
plot(map)

#Guardar os resultados
writeRaster(map,"./outputR/map.tif",overwrite=T)
save(me,file="./outputR/mx_obj.RData")


#avalia??o do Modelo usando "kfold partitioning"
#Exemplo para 1 "fold"
fold <- kfold(pres.covs, k=5) #Gera um ?ndice aleat?rio de "folds"
buffalopresencetest <- pres.covs[fold == 1, ]
buffalopresencetrain <- pres.covs[fold != 1, ]
y<-c(rep(1,nrow(buffalopresencetrain)), rep(0,nrow(bkg.covs)))

env.values<-data.frame(rbind(buffalopresencetrain, bkg.covs))
env.values$ecoreg<-as.factor(env.values$ecoreg) #Importante quando vari?veis categ?ricas s?o usados

me <- maxent(env.values, y, args=c("addsamplestobackground=true"), path="./outputR")
e <- evaluate(me, p=data.frame(buffalopresencetest), a=data.frame(bkg.covs))
str(e)
??ModelEvaluation

#Threshold value that maximizes Kappa
plot(e@t,e@kappa,type="l")
e@t[which.max(e@kappa)]

#Computing True Skill Statistic = TPR(Sensitivity)+TNR(Specificity)-1
tss <- e@TPR+e@TNR-1
plot(e@t,tss,type="l")
e@t[which.max(tss)]

#AUC Plot: X=1-Specificity, Y=Sensitivity
plot((1-e@TNR),e@TPR,type="l",xlab="Fractional Predicted Area (1 - Specificity",
     ylab="Sensitiviy")
e@auc

#Algumas leituras sobre o desempenho estat?stica e valida??o de modelos
#Fielding, A.H. & Bell, J.F. (1997) A review of methods for the assessment of
#prediction errors in conservation presence/absence models. 
#Environmental Conservation, 24, 38-49.

#ALLOUCHE, O., TSOAR, A. and KADMON, R. (2006), Assessing the accuracy of
#species distribution models: prevalence, kappa and the true skill statistic
#(TSS). Journal of Applied Ecology, 43: 1223-1232. 

#Now, for all folds
auc<-rep(NA,5)
max.tss<-rep(NA,5)
for (i in 1:5){
  buffalopresencetest <- pres.covs[fold == i, ]
  buffalopresencetrain <- pres.covs[fold != i, ]
  env.values<-data.frame(rbind(buffalopresencetrain, bkg.covs))
  env.values$ecoreg<-as.factor(env.values$ecoreg) #Importante quando vari?veis categ?ricas s?o usados
  y<-c(rep(1,nrow(buffalopresencetrain)), rep(0,nrow(bkg.covs)))
  me <- maxent(env.values, y, args=c("addsamplestobackground=true"), path="./outputR")
  e<-evaluate(me, p=data.frame(buffalopresencetest), a=data.frame(bkg.covs))
  auc[i]<-e@auc
  lines((1-e@TNR),e@TPR)
  tss<-e@TPR+e@TNR-1
  max.tss[i]<-e@t[which.max(tss)]
}
mean(auc)
sd(auc)
mean(max.tss)
#Curvas Gr?ficos-resposta. Lembro-me que era o nome do objeto
#MaxEnt com o modelo com todas as presen?as, que foi substitu?do no
#O ciclo anterior. No entanto, podemos restaurar e mant?-lo.

load("./outputR/mx_obj.RData")
response(me)
response(me,var=1) #Usando indicador de columna
response(me, var="ecoreg") #Usando nome da variavel

#modelos de proje??o para outros epocas
hotlayers <- stack(list.files("./hotlayers", "*.asc$", full.names=T))
map.future <- predict(me, hotlayers, progress="text")

par(mfrow=c(1,2)) #Gr?fico janela dividida comparando modelos
plot(map)
plot(map.future)

par(mfrow=c(1,2)) #Dividir ventana de grafico para comparar modelos
plot(map)
plot(map.future)

#Visualmente compare modelos que aplicam um limiar
umbral.tss <- mean(max.tss)
plot(map >= umbral.tss)
plot(map.future >= umbral.tss)

map.tss <- (map>=umbral.tss) #Criar limite objeto raster
map.future.tss <- (map.future>=umbral.tss) #Criar limite objeto raster

#Calcular ?reas
area.layers <- area(layers[[1]]) # Os Layers usadas s?o 0,05 graus correspondentes no Equador para 6 km aprox
plot(area.layers)
plot(map.tss*area.layers)
cellStats(map.tss, sum)*(6*6) # ?rea em km2 presente projec??o n?o corrigida
cellStats(map.tss * area.layers, sum) # ?rea em km2 presente corrigido
cellStats(map.future.tss * area.layers, sum) # ?rea futura em km2

#"Custom" Maxent
response(me)
mxnt.args=c("autofeature=FALSE",
            "linear=TRUE",
            "quadratic=TRUE",
            "product=FALSE",
            "hinge=FALSE",
            "threshold=FALSE") # Selecionar recursos manualmente
me.mfeatures <- maxent(env.values, y, args=mxnt.args, path="./outputR")
response(me.mfeatures)
par(mfrow=c(1,2))
response(me,var="h_dem") # Altitude
response(me.mfeatures,var="h_dem")

#Elith, J.et al. (2011), A statistical explanation of MaxEnt for ecologists. 
#Diversity and Distributions, 17: 43-57. 

#Argumentos para proje??o
map2<-predict(me.mfeatures, layers, progress="text")
map.future2<-predict(me.mfeatures, hotlayers, 
                     args=c("extrapolate=FALSE", "doclamp=TRUE"), 
                     progress="text")
plot(map.future)
plot(map.future2)

#Elith, J., Kearney, M. and Phillips, S. (2010), The art of modelling 
#range-shifting species. Methods in Ecology and Evolution, 1: 330-342. 

#Avaliar novo modelo
#ev.stats <- evModel(fold, pres.covs, bkg.covs, factor.ind=3, mxnt.args,
#                    path="./outputR")
#mean(auc) # Estat?sticas modelo autofeature
#mean(ev.stats$auc) #Estat?sticas modelo autofeature

#compara??o presente
plot(map.tss)
plot(map2>mean(ev.stats$max.tss))

# compara??o futuras
plot(map.future.tss)
plot(map.future2>mean(ev.stats$max.tss))

# Dois casos especiais

###Selecci?n de variables (Warren & Seifert 2011)
#Warren, D. L. and Seifert, S. N. (2011), Ecological niche modeling in 
#Maxent: the importance of model complexity and the performance of model
#selection criteria. Ecological Applications, 21: 335-342.

#An Introduction to Statistical Learning: with Applications in R (Capitulo 6)

getLambdaTable(me@lambdas)

betas=c(0.02, 0.1, 0.46, 1, 2.2, 4.6)
opt.lambda <- data.frame(beta=betas, nparams=NA, mean.auc=NA, mean.mtss=NA)

for(i in 1:length(betas)){
  mxnt.args=c("autofeature=FALSE",
              "linear=TRUE",
              "quadratic=TRUE",
              "product=FALSE",
              "hinge=FALSE",
              "threshold=FALSE",
              paste0("betamultiplier=",betas[i])) # Selecionar recursos manualmente
  ev.stats <- evModel2(fold, pres.covs, bkg.covs, factor.ind=3,mxnt.args,
                       path="./outputR")
  opt.lambda[i,2:4]<-sapply(ev.stats,mean)
}

par(mfrow=c(1,1))
plot(opt.lambda$beta,opt.lambda$nparams,type="l",
     xlab="Beta multiplier",ylab="N?mero m?dio de par?metros")

plot(opt.lambda$beta,opt.lambda$mean.auc,type="l",
     xlab="Beta multiplier",ylab="AUC",col="blue")

opt.lambda

# Consulte a ajuda para mais bandeiras Maxent

##Tomando fundo de M
#Anderson, R. P. and Raza, A. (2010), The effect of the extent of the study
#region on GIS models of species geographic distributions and estimates of
#niche evolution: preliminary tests with montane rodents (genus Nephelomys)
#in Venezuela. Journal of Biogeography, 37: 1378-1393. 

plot(layers[["ecoreg"]],col=rainbow(15))
points(buffalopresence[,2:3], pch=18,cex=0.6)
eco.mask<-layers[["ecoreg"]] %in% unique(pres.covs[,"ecoreg"])
eco.mask[eco.mask==0] <- NA
plot(eco.mask)
writeRaster(eco.mask, "./layers/ecomask.asc", overwrite=T)
layers.eco <- stack(list.files("./layers","*.asc$",full.names=T))

me.eco <- maxent(layers.eco, occs[,2:3], factors='ecoreg', removeDuplicates=TRUE, path="./outputR")
map.eco <- predict(layers.eco, me.eco, progress="text")
plot(map.eco)

eco.mask[!is.na(layers[[1]])] <- 1
plot(eco.mask)
writeRaster(!is.na(layers[["ecoreg"]]), "./layers/ecomask.asc", overwrite=T)
layers.eco <- stack(list.files("./layers","*.asc$",full.names=T))

map.proj <- predict(layers.eco, me.eco, progress="text")

par(mfrow=c(1,2))
plot(map.eco)
plot(map.proj)
