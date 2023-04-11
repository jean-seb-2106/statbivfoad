library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#Nombre d'individus par groupe
# 2 marée par jour donc 730 en tout sur l'année
n1 <- 415 #nombre de marée dont le mascater sera mauvais
n2 <- 250 #nombre de marée dont le mascater sera moyen
n3 <- 65  #nombre de marée dont le mascater sera bon

set.seed(1940)

#j'ai repris le code du module trois en introduisant le 3 variable de taille car les trois groupes sont ici de tailles diférentes

creat_coef <- function(n1,n2,n3,a,b,c,d,e,f){
  coef1 <- rnorm(n1,a,b)
  coef2 <- rnorm(n2,c,d)
  coef3 <- rnorm(n3,e,f)
  return(c(coef1,coef2,coef3))
}


#CrÃ©ation variable coef
 coef_maree<- creat_coef(n1,n2,n3,60,5,70,5,94,2)

 
creat_mascaret <- function(mascaret){
  mascaret1 <- rep(mascaret[1],n1)
  mascaret2 <- rep(mascaret[2],n2)
  mascaret3 <- rep(mascaret[3],n3)
  c(mascaret1,mascaret2,mascaret3)
}

mascaret <- creat_mascaret(c("Mauvais mascaret","Mascaret moyen","Bon mascaret"))

#Base initiale
base <- data.frame(mascaret,coef_maree)


#Indicateurs


moy_coef <- mean(base$coef_maree)


var_coef <- round(var(coef_maree),0)


eta2 <- round(BioStatR::eta2(coef_maree,mascaret),2)




#Créeation des fonctions pour calculer les variance inter et intra 
variance_intra<- function(df,coef_maree,moy_coef){
  
  
  base<-base %>% 
    group_by({{mascaret}}) %>% 
    summarise(n=n(),
              contrib_var_intra = n*var({{coef_maree}}))
  
  variance_intra<- sum(base$contrib_var_intra)/730
  return(round(variance_intra,0))
}


variance_inter<- function(df,coef_maree,moy_coef){
  
  
  base<-base %>% 
    group_by({{mascaret}}) %>% 
    summarise(n=n(),
              contrib_var_inter = n*({{moy_coef}}-mean({{coef_maree}}))^2)
  
  
  variance_inter<- sum(base$contrib_var_inter)/730
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
variance_intra<-variance_intra(base,coef_maree,moy_coef)

# Calcul des variances inter 
variance_inter<-variance_inter(base,coef_maree,moy_coef)



#fonction pour générer un nuage de points quali-quanti
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
    ylab("coefficient de marée")+
    ggtitle(titre,subtitle = waiver())+
    scale_y_continuous(breaks = seq(50,110,by=5),limits = c(50,110))
}


# grob <- grobTree(textGrob(paste("Variance = ",var_prix_dep), x=0.85,  y=0.95, hjust=0,
#                           gp=gpar(col="red", fontsize=10, fontface="italic")))


graph <- creat_nuage_qualiquanti(base,
                                       quali = mascaret,
                                       quanti=coef_maree,
                                       moy=moy_coef,
                                       var=var_coef,
                                       eta2=eta2,
                                       var_intra=variance_intra,
                                       var_inter=variance_inter,
                                       "Qualité du mascaret en fonction du coefficient de marée")





#grid.arrange(graph_indep,graph_interm1,graph_interm2)
#grid.arrange(graph_interm2,graph_interm3,graph_dep)
#grid.arrange(graph_indep,graph_interm1,graph_interm2,graph_interm3,graph_dep)


setwd("C:/Users/T1YU0C/Desktop/Stage/Image")

graph
ggsave("graph_mascaret.svg")

#test pour faire un tableau de moyenne
tableau_de_moyennes<-base %>% 
  group_by({{mascaret}}) %>% 
  summarise(n=n(),
            contrib_var_inter = n*(moy_coef-mean(coef_maree))^2,
            contrib_var_intra = n*var(coef_maree),
            
            moyenne_coef = mean(coef_maree),
            coef_var_intra = sd(coef_maree)/mean(coef_maree)*100,
            coef_var_inter = ((moy_coef-mean(coef_maree))^2)/mean(coef_maree)*100
  )

## la j'ai eu des soucis avec les coeff de variation donc j'ai tout calculé à la main et j'ai mis les tableaux dans le impress tableau.odt




variance_inter<- sum(base$contrib_var_inter)/730


ggplot(data=base,
       aes(x= mascaret, y=coef_maree ))+ geom_boxplot(outlier.colour = "black")+ylab("Coefficient de marée")




