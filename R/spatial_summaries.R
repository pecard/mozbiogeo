#' Spatial summaries

#' Overlay data0 to vector layers ---------------------------------------------
#' Province
p1 <- as.data.frame(sp::over(dadosg, moz[,'NAME_1']))
data0$provincia <- p1[ ,1]

#' Conservation Area
p2 <- as.data.frame(sp::over(dadosg, pa[,'name']))
data0$aprotegida <- p2[ ,1]

#' Sumario dos Dados por Provincia --------------------------------------------
sp_prov <- as.data.frame(data0 %>%
                           dplyr::group_by(provincia) %>%
                           dplyr::summarise(nind = sum(number, na.rm = F)
                                            , nesp = length(unique(esp))
                                            , record = n()
                                            ) %>%
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
                            dplyr::summarise(nind = sum(number, na.rm = F)
                                             ,nesp = length(unique(esp))
                                             ,record = n()) %>%
                            dplyr::arrange(desc(nind)) #%>%
                            #filter(complete.cases(.))%>%
                            #filter(nesp>23) %>%
                            #tidyr::gather(aprotegida)
                          )
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
