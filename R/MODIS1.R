#' Using the MODISTools Package
#' http://www.r-bloggers.com/modis-processing-with-r-gdal-and-the-nco-tools/
#' https://cran.r-project.org/web/packages/MODISTools/vignettes/UsingMODISTools.pdf
#' https://conservationecology.wordpress.com/2014/08/11/bulk-downloading-and-analysing-modis-data-in-r/
#' http://www.r-bloggers.com/modis-r-package-tutorial/
#' http://daacmodis.ornl.gov/cgi-bin/MODIS/GLBVIZ_1_Glb/modis_subset_order_global_col5.pl


library(MODISTools)
library(ggplot2)
library(dplyr)
library(scales)

GetProducts()
GetBands(Product = "MOD13Q1")
# MODISTools requires you to make a query data.frame
#coord <- c(-18.7,35.9) # Coordinates south of Mount Kilimanjaro
product <- "MOD13Q1"
bands <- c("250m_16_days_NDVI","250m_16_days_pixel_reliability") # What to query. You can get the names via GetBands
# modis_dates <- GetDates(Product = "MOD13Q1", Lat =coord[1], Long = coord[2])
savedir <- "D:\\Programacao\\biogeo\\data\\modis" # local folder
pixel <- c(90, 100) # Get the central pixel only (0,0) or a quadratic tile around it

period <- data.frame(lat=c(-18.7, -12.1),long=c(35.9, 37.9)
                     ,start.date=2009,end.date=2013
                     ,ID=c('Marromeu', 'Niassa'))

period2 <- data.frame(lat=c(-12.1),long=c(37.1)
                     ,start.date=2009,end.date=2013
                     ,ID=c('Niassa2'))

period1 <- data.frame(lat=c(-12.1),long=c(38.4)
                      ,start.date=2009,end.date=2013
                      ,ID=c('Niassa1'))

#' To download the pixels
MODISSubsets(LoadDat = period1, Products = product
             , Bands = bands, Size = pixel
             , SaveDir = savedir, StartDate = T)

MODISGrid(Dir = "D:/Programacao/biogeo/data/modis"
          , DirName = "MODIS_GRID", SubDir = TRUE,
          NoDataValues = list("MOD13Q1" = c("250m_16_days_NDVI" = -9999
                                            ,"250m_16_days_pixel_reliability" = -1)))


modlist <- list()
for(i in 1:length(list.files(savedir, pattern = ".asc$"))){
  name <- gsub("__.*$", '', list.files(savedir, pattern = ".asc"))[i]
  mod_asc <- read.csv(file.path(savedir,list.files(savedir, pattern = ".asc"))[i]
                      , header = F, as.is = TRUE)
  linendvi <- grep("_NDVI", mod_asc$V6)
  ndvi <- mod_asc[linendvi, ]
  ndvi <- ndvi[ ,c(8, 11:ncol(ndvi))]
  ndvil <- ndvi %>%
    tidyr::gather('var', 'ndvi', -1) %>%
    dplyr::mutate(data = gsub(pattern = 'A', '', V8),
                  data = as.POSIXct(strptime(data, "%Y%j", tz = "GMT")),
                  year = round(as.Date(data, 'year')),
                  id = name)
  quality <- mod_asc[-linendvi, ]
  qualityv <- as.vector(as.matrix(quality[ ,11:ncol(quality)], by.row=T))
  ndvif <- ndvil[which(qualityv < 2), ]
  modlist[[i]] <- ndvif
}
ndvi_all <- rbind_all(modlist)

ggplot(ndvi_all, aes(x=as.Date(data), y=ndvi*0.0001, colour = id)) + 
  stat_summary(
    geom="line", fun.y="mean", size=1.2, alpha=.8) +
  labs(list(x='data', y='ndvi')) +
  #facet_grid(id~.) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  scale_x_date(date_breaks = "2 months", 
               date_labels="%b-%Y")

#' Complementos ---------------------------------------------------------------
geom_line(aes(group=1))
MODISSummaries(LoadDat = period, FileSep = ","
               , Product = "MOD13Q1"
               , Bands = "250m_16_days_NDVI"
               , ValidRange = c(-2000,10000)
               , NoDataFill = -3000
               , ScaleFactor = 0.0001
               , StartDate = TRUE
               , Yield = T
               , Interpolate = T
               , QualityScreen = TRUE
               , QualityThreshold = 0
               , QualityBand = "250m_16_days_pixel_reliability")


# https://conservationecology.wordpress.com/2014/08/11/bulk-downloading-and-analysing-modis-data-in-r/# MODISTools requires you to make a query data.frame
coord <- c(-3.223774, 37.424605) # Coordinates south of Mount Kilimanjaro
product <- "MOD13Q1"
bands <- c("250m_16_days_EVI","250m_16_days_pixel_reliability") # What to query. You can get the names via GetBands
savedir <- "tmp/" # You can save the downloaded File in a specific folder
pixel <- c(0,0) # Get the central pixel only (0,0) or a quadratic tile around it
period <- data.frame(lat=coord[1],long=coord[2],start.date=2013,end.date=2014,id=1)
# To download the pixels
MODISSubsets(LoadDat = period, Products = product,Bands = bands,Size = pixel,SaveDir = savedir, StartDate = T)
MODISSummaries(LoadDat = period, FileSep = ",", Product = "MOD13Q1", Bands = "250m_16_days_EVI",ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,StartDate = TRUE,Yield = T,Interpolate = T, QualityScreen = TRUE, QualityThreshold = 0,QualityBand = "250m_16_days_pixel_reliability")
# Finally read the output
read.table("MODIS_Summary_MOD13Q1_2014-08-10.csv",header = T,sep = ",")