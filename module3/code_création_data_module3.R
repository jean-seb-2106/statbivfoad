#Code de la cr?ation des dif?rents graph du module 3


library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#Nombre d'individus par groupe
n <- 500

set.seed(1922)

creat_taille <- function(n,a,b,c,d,e,f){
  taille1 <- rnorm(n,a,b)
  taille2 <- rnorm(n,c,d)
  taille3 <- rnorm(n,e,f)
  return(c(taille1,taille2,taille3))
}

#Création variable taille dépendant
taille_dep <- creat_taille(n,175,0,185,0,195,0)
sd_taille_dep <- sd(taille_dep)
#Création variable taille indépendant (avec écart-type )
taille_indep <- creat_taille(n,185,sd_taille_dep,185,sd_taille_dep,185,sd_taille_dep-0.1)
taille_interm1 <- creat_taille(n,181,sd_taille_dep-1.2,185,sd_taille_dep,188,sd_taille_dep-1.2)
taille_interm2 <- creat_taille(n,179,sd_taille_dep-2.8,185,sd_taille_dep,191,sd_taille_dep-2.8)
taille_interm3 <- creat_taille(n,176,sd_taille_dep-4.5,185,sd_taille_dep-4.5,194,sd_taille_dep-4.5)
taille_extreme <- creat_taille(n,185,0,185,0,185,0)




creat_surface <- function(n,surface){
  surface1 <- rep(surface[1],n)
  surface2 <- rep(surface[2],n)
  surface3 <- rep(surface[3],n)
  c(surface1,surface2,surface3)
}

surface <- creat_surface(n,c("Terre battue","Surface dure","Gazon"))

#Base initiale
base <- data.frame(surface,taille_dep,taille_indep,taille_interm1,taille_interm2,taille_interm3,taille_extreme)


#Indicateurs


moy_taille_dep <- mean(base$taille_dep)
moy_taille_indep <- mean(base$taille_indep)
moy_taille_interm1 <- mean(base$taille_interm1)
moy_taille_interm2 <- mean(base$taille_interm2)
moy_taille_interm3 <- mean(base$taille_interm3)
moy_taille_extreme<- mean(base$taille_extreme)

var_taille_dep <- round(var(taille_dep),0)
var_taille_indep <- round(var(taille_indep),0)
var_taille_interm1 <- round(var(taille_interm1),0)
var_taille_interm2 <- round(var(taille_interm2),0)
var_taille_interm3 <- round(var(taille_interm3),0)
var_taille_extreme <- var(taille_extreme)

eta2_taille_dep <- round(BioStatR::eta2(taille_dep,surface),2)
eta2_taille_indep <- round(BioStatR::eta2(taille_indep,surface),2)
eta2_taille_interm1 <- round(BioStatR::eta2(taille_interm1,surface),2)
eta2_taille_interm2 <- round(BioStatR::eta2(taille_interm2,surface),2)
eta2_taille_interm3 <- round(BioStatR::eta2(taille_interm3,surface),2)
eta2_taille_extreme <- round(BioStatR::eta2(taille_extreme,surface),2)



#Cr?eation des fonctions pour calculer les variance inter et intra 
variance_intra<- function(df,taille,moy_taille){


base<-base %>% 
  group_by({{surface}}) %>% 
  summarise(n=n(),
            contrib_var_intra = n*var({{taille}}))
  
variance_intra<- sum(base$contrib_var_intra)/1500
return(round(variance_intra,0))
}


variance_inter<- function(df,taille,moy_taille){
  
  
  base<-base %>% 
    group_by({{surface}}) %>% 
    summarise(n=n(),
              contrib_var_inter = n*({{moy_taille}}-mean({{taille}}))^2)
  

  variance_inter<- sum(base$contrib_var_inter)/1500
  return(round(variance_inter,0))
}




# ###
# base1<-base %>% 
#   group_by(surface) %>% 
#   summarise(n=n(),
#             contrib_var_inter = n*(moy_taille_extreme-mean(taille_extreme))^2)
# 
# 
# variance_inter<- sum(base$contrib_var_inter)/1500
# 
# moy_taille_extreme
# ###

# Calcul des variances intra 
variance_intra_indep<-variance_intra(base,taille_indep,moy_taille_indep)
variance_intra_interm1<-variance_intra(base,taille_interm1,moy_taille_interm1)
variance_intra_interm2<-variance_intra(base,taille_interm2,moy_taille_interm2)
variance_intra_interm3<-variance_intra(base,taille_interm3,moy_taille_interm3)
variance_intra_dep<-variance_intra(base,taille_dep,moy_taille_dep)
variance_intra_extreme<-variance_intra(base,taille_extreme,moy_taille_extreme)

# Calcul des variances inter 
variance_inter_indep<-variance_inter(base,taille_indep,moy_taille_indep)
variance_inter_interm1<-variance_inter(base,taille_interm1,moy_taille_interm1)
variance_inter_interm2<-variance_inter(base,taille_interm2,moy_taille_interm2)
variance_inter_interm3<-variance_inter(base,taille_interm3,moy_taille_interm3)
variance_inter_dep<-variance_inter(base,taille_dep,moy_taille_dep)
variance_inter_extreme<-variance_inter(base,taille_extreme,moy_taille_extreme)



#fonction pour g?n?rer un nuage de points quali-quanti
creat_nuage_qualiquanti <- function(df,quali,quanti,moy,var,eta2,var_intra,var_inter,titre){
  
  
  grob1 <- grobTree(textGrob(paste("Variance = ",var), x=0.8,  y=0.95, hjust=0,
                             gp=gpar(col="red", fontsize=10, fontface="italic")))
  
  grob2 <- grobTree(textGrob(paste("eta2 = ",eta2), x=0.8,  y=0.85, hjust=0,
                             gp=gpar(col="darkgreen", fontsize=10, fontface="italic")))
  
  grob3 <- grobTree(textGrob(paste("Var_intra = ",var_intra), x=0.8,  y=0.75, hjust=0,
                             gp=gpar(col="darkblue", fontsize=10, fontface="italic")))
  
  grob4 <- grobTree(textGrob(paste("var_inter = ",var_inter), x=0.8,  y=0.65, hjust=0,
                             gp=gpar(col="darkblue", fontsize=10, fontface="italic")))
  

  
  
  ggplot(df,aes(x = {{quali}},y={{quanti}},color={{quali}})) +
    geom_point(show.legend = FALSE) +
    geom_hline(yintercept = moy,size=1)+
    stat_summary(fun=mean,geom = "crossbar",size=0.5,width=0.5,show.legend = FALSE)+
    theme_light()+
    annotation_custom(grob1)+
    annotation_custom(grob2)+
    annotation_custom(grob3)+
    annotation_custom(grob4)+
    xlab("")+
    ylab("taille (en cm)")+
    ggtitle(titre,subtitle = waiver())+
    scale_y_continuous(breaks = seq(160,210,by=10),limits = c(160,210))
}


# grob <- grobTree(textGrob(paste("Variance = ",var_prix_dep), x=0.85,  y=0.95, hjust=0,
#                           gp=gpar(col="red", fontsize=10, fontface="italic")))


graph_indep <- creat_nuage_qualiquanti(base,
                                       quali = surface,
                                       quanti=taille_indep,
                                       moy=moy_taille_indep,
                                       var=var_taille_indep,
                                       eta2=eta2_taille_indep,
                                       var_intra=variance_intra_indep,
                                       var_inter=variance_inter_indep,
                                       "Situation d'indépendance")

graph_dep <- creat_nuage_qualiquanti(base,
                                     quali = surface,
                                     quanti=taille_dep,
                                     moy=moy_taille_dep,
                                     var=var_taille_dep,
                                     eta2_taille_dep,
                                     var_intra=variance_intra_dep,
                                     var_inter=variance_inter_dep,
                                     "Situation de dépendance")

graph_interm1 <- creat_nuage_qualiquanti(base,
                                         quali = surface,
                                         quanti=taille_interm1,
                                         moy=moy_taille_interm1,
                                         var=var_taille_interm1,
                                         eta2_taille_interm1,
                                         var_intra=variance_intra_interm1,
                                         var_inter=variance_inter_interm1,
                                         "Situation intermédiaire1")

graph_interm2 <- creat_nuage_qualiquanti(base,
                                         quali = surface
                                         ,quanti=taille_interm2,
                                         moy=moy_taille_interm2,
                                         var=var_taille_interm2,
                                         eta2_taille_interm2,
                                         var_intra=variance_intra_interm2,
                                         var_inter=variance_inter_interm2,
                                         "Situation intermédiaire2")

graph_interm3 <- creat_nuage_qualiquanti(base,
                                         quali = surface,
                                         quanti=taille_interm3,
                                         moy=moy_taille_interm3,
                                         var=var_taille_interm3,
                                         eta2_taille_interm3,
                                         var_intra=variance_intra_interm3,
                                         var_inter=variance_inter_interm3,
                                         "Situation intermédiaire3")

graph_extreme <- creat_nuage_qualiquanti(base,
                                         quali = surface,
                                         quanti=taille_extreme,
                                         moy=moy_taille_extreme,
                                         var=var_taille_extreme,
                                         var_intra=variance_intra_extreme,
                                         var_inter=variance_inter_extreme,
                                         eta2 = "Non def",
                                         "Situation extrême")



#grid.arrange(graph_indep,graph_interm1,graph_interm2)
#grid.arrange(graph_interm2,graph_interm3,graph_dep)
#grid.arrange(graph_indep,graph_interm1,graph_interm2,graph_interm3,graph_dep)


setwd("C:/Users/T1YU0C/Desktop/Stage/Image")

graph_indep
ggsave("module3/graphes_svg/graph_indep.svg")
graph_interm1
ggsave("module3/graphes_svg/graph_interm1.svg")
graph_interm2
ggsave("module3/graphes_svg/graph_inter2.svg")
graph_interm3
ggsave("module3/graphes_svg/graph_interm3.svg")
graph_dep
ggsave("module3/graphes_svg/graph_dep.svg")
graph_extreme
ggsave("module3/graphes_svg/graph_extreme.svg")



ggplot(data=base,
       aes(x = surface, y=taille_interm3 ))+ geom_boxplot(outlier.colour = "black")+xlab("surface pr?f?r?e")+ylab("Taille (en cm)")

ggsave("boxplot_quanti_quali.svg")


ggplot(data=base,
       aes(, y=taille_interm3 ))+ geom_boxplot(outlier.colour = "black")+ylab("Taille (en cm)")



taille_test<-rep(185,1500)
BioStatR::eta2(taille_test,surface)




