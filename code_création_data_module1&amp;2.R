library(dplyr)
library(ggplot2)
library(stringr)
library(report)
library(questionr)


######## Code utilisé par Paul Aventin pour générer la base de données sur le tennis ------

#Tennis<-read.csv("donneebis.csv") Ce fichier est introuvable...

# Création de ma table "Tennis" qui est le socle de ma base de donnée 
# Tennis<-Tennis %>%
#   mutate(atpnorm=(Point.atp-mean(Point.atp))/sd(Point.atp)) %>%
#   mutate(heure_entrainement=4*atpnorm +rnorm(100,mean=10,sd=2)) %>%
#   mutate(code_pays=str_sub(Nom,-5)) %>%
#   mutate(continent=case_when(code_pays == "(SRB)" ~ "Europe",
#                              code_pays == "(RUS)" ~ "Europe",
#                              code_pays == "(ESP)" ~ "Europe",
#                              code_pays == "(AUT)" ~ "Europe",
#                              code_pays == "(GRE)" ~ "Europe",
#                              code_pays == "(ALL)" ~ "Europe",
#                              code_pays == "(SUI)" ~ "Europe",
#                              code_pays == "(ITA)" ~ "Europe",
#                              code_pays == "(FRA)" ~ "Europe",
#                              code_pays == "(BEL)" ~ "Europe",
#                              code_pays == "(POL)" ~ "Europe",
#                              code_pays == "(BUL)" ~ "Europe",
#                              code_pays == "(NOR)" ~ "Europe",
#                              code_pays == "(CRO)" ~ "Europe",
#                              code_pays == "(G-B)" ~ "Europe",
#                              code_pays == "(GEO)" ~ "Europe",
#                              code_pays == "(HON)" ~ "Europe",
#                              code_pays == "(SLO)" ~ "Europe",
#                              code_pays == "(RTC)" ~ "Europe",
#                              code_pays == "(FIN)" ~ "Europe",
#                              code_pays == "(MDA)" ~ "Europe",
#                              code_pays == "(BLR)" ~ "Europe",
#                              code_pays == "(LTU)" ~ "Europe",
#                              code_pays == "(SUE)" ~ "Europe",
#                              code_pays == "(SVK)" ~ "Europe",
#                              code_pays == "(GEO)" ~ "Europe",
#                              code_pays == "(E-U)" ~ "AMERIQUE",
#                              code_pays == "(ARG)" ~ "AMERIQUE",
#                              code_pays == "(CAN)" ~ "AMERIQUE",
#                              code_pays == "(CHI)" ~ "AMERIQUE",
#                              code_pays == "(URU)" ~ "AMERIQUE",
#                              code_pays == "(BRE)" ~ "AMERIQUE",
#                              code_pays == "(JPN)" ~ "ASIE",
#                              code_pays == "(AUS)" ~ "ASIE",
#                              code_pays == "(KAZ)" ~ "ASIE",
#                              code_pays == "(COR)" ~ "ASIE",
#                              code_pays == "(AFS)" ~ "ASIE",
#   )) %>%
#   mutate(surface_preferee = case_when(continent == "Europe" ~ sample(c('Dure','Terre_Battue','Gazon'),100,replace=T, prob = c(0.2,0.35,0.45)),
#                                       continent == "ASIE" ~ sample(c('Dure','Terre_Battue','Gazon'),100,replace=T, prob = c(0.8,0.1,0.1)),
#                                       continent == "AMERIQUE" ~ sample(c('Dure','Terre_Battue','Gazon'),100,replace=T, prob = c(0.5,0.4,0.1) )))
  #select(!c(HeureGauss,HeureCroiss))

#exportation de la table Tennis en csv pour pouvoir la stocker 
# Bdd<-write.csv(Tennis,"C:/Users/T1YU0C/Desktop/Stage/Id?e/donnee_tennis.csv", row.names = FALSE)

# 1 - Etude des corrélations entre deux quantitatives

#Importation de la base donnees_tennis.csv sous le nom tennis
tennis <-read.csv("donnee_tennis.csv")


####Test des correlation quanti quanti#####
ggplot(data=tennis,
       aes(x = heure_entrainement, y= Point.atp  )) +
       geom_point() + 
       theme_classic()+geom_smooth(method=lm,se=FALSE,fullrange=TRUE)


ggsave("images/regression_lin.svg")

cor(tennis$heure_entrainement,tennis$Point.atp) 

# explication du nuage de points avec corrélations negatives

cor_neg<-tennis %>%
  select(!heure_entrainement)%>%
  mutate(heure_entrainement=10-4*atpnorm +rnorm(100,mean=10,sd=2)) 

ggplot(data=cor_neg,
       aes(x = heure_entrainement, y= Point.atp  ))+
  geom_point()+ theme_classic()+geom_smooth(method=lm,se=FALSE,fullrange=TRUE)
ggsave("images/regression_lin_negative.svg")





#Cr?ation de la table val_extreme qui est egale ? tennis 2 moins les 8 meilleurs joueurs, permettant de mettre en lumiere les effets de valeurs extr?mes sur le coef de corr 

val_extreme <-tennis %>% filter(rang>8) %>% 
  mutate(taille=case_when(
    Point.atp<2000 ~ rnorm(92,mean=1.85,sd=0.12),
    Point.atp>=2000 ~ rnorm(92,mean=1.95,sd=0.09)
    )) %>% 
  mutate(gr_taille=case_when(
    taille<1.85 ~ "petit", 
    taille>1.85 ~ "grand")) %>%
  mutate(heure_ent_bis=case_when(
    gr_taille=="petit"~4*atpnorm +rnorm(92,mean=5,sd=0.7),
    gr_taille=="grand"~rnorm(92,mean=4,sd=0.1)))




ggplot(data=val_extreme,
       aes(x = heure_entrainement, y= Point.atp ))+ geom_point()+
  theme_classic()+geom_smooth(method=lm,se=FALSE)

cor(val_extreme$heure_entrainement,val_extreme$Point.atp) 

ggsave("images/regression_lin_sans_Val_extreme.svg")



## Partie pour mettre en évidence deux sous-populations ------

#creation de mon autre table ""Tennis 4et5" pour avoir la liaison croissante puis decroissante du niveau en fct de la taille ( ce sont le table tennis 4 et 5 que l'on regroupe en "lien taille" a la fin)

croissante <-tennis %>%
  filter(rang<15) %>% 
  mutate(taille=rnorm(14,mean=1.82,sd=0.06)) %>%
mutate(Score_ATP=50**taille+rnorm(14,mean=200,sd=20))

  
  
  
decroissante <-filter(cor_neg,rang>15)
decroissante <-filter(decroissante,rang<75)
decroissante<-decroissante %>%
 # select(!c(Identifiant,numero,Nom))%>%
  mutate(taille=rnorm(59,mean=2.1,sd=0.03)) %>%
  mutate(Score_ATP=4300-45**taille+rnorm(59,mean=20,sd=20))


quiz<-filter(cor_neg, rang>75)
quiz<-quiz %>%
  # select(!c(Identifiant,numero,Nom))%>%
  mutate(taille=rnorm(25,mean=1.9,sd=0.03)) %>%
  mutate(Score_ATP=560*taille+rnorm(25,mean=20,sd=20))
  

Lien_taille<-rbind(croissante,decroissante,quiz)


ggplot(data=Lien_taille,
       aes(x = taille, y=Score_ATP ))+ geom_point()+
  theme_classic()


cor(Lien_taille$taille,Lien_taille$Score_ATP) 

ggsave("regression_lin_sspop.svg")


  


#tests pour avoir les differetes echelles de correlation 

Tennis7<-Tennis2
Tennis7<-Tennis7 %>%
  #select(!heure_entrainement)%>%
mutate(heure_entrainement=4*atpnorm +rnorm(100,mean=0,sd=200000.5))

cor(Tennis7$heure_entrainement,Tennis7$Point.atp) 

ggplot(data=Tennis7,
       aes(x = heure_entrainement, y=Point.atp ))+ geom_point()+
  theme_classic()+geom_smooth(method=lm, se=FALSE)



ggsave("regression_lin_exemple6.svg")

# 2 - Etude des corrélations entre deux variables quantitatives--------

#Test des correlations quali/quali
chi2<-chisq.test(Tennis2$continent, Tennis2$surface_preferee)
N<-nrow(Tennis2)
p<-nlevels(as.factor(Tennis2$continent))
q<-nlevels(as.factor(Tennis2$surface_preferee))
Vcramer<-sqrt(chi2$statistic/(N*min(p-1,q-1)))

table(Tennis2$continent,Tennis2$surface_preferee)

test<-write.csv(lprop(table(Tennis2$continent,Tennis2$surface_preferee)
),"C:/Users/T1YU0C/Desktop/Stage/Id?e/test.csv", row.names = FALSE)

lprop(table(Tennis2$continent,Tennis2$surface_preferee))
cprop(table(Tennis2$continent,Tennis2$surface_preferee))



# rajout de plus d'individus pour que ca marche bien avec le khi2. J'ai juste dedoubl? la base de donn?es Tennis2 pour avoir 200 joueur dans l'?tude.
Tennis_quali<-rbind(Tennis2,Tennis2)

chi2<-chisq.test(Tennis_quali$continent, Tennis_quali$surface_preferee)
N<-nrow(Tennis_quali)
p<-nlevels(as.factor(Tennis_quali$continent))
q<-nlevels(as.factor(Tennis_quali$surface_preferee))
Vcramer<-sqrt(chi2$statistic/(N*min(p-1,q-1)))

table(Tennis_quali$continent,Tennis_quali$surface_preferee)
lprop(table(Tennis_quali$continent,Tennis_quali$surface_preferee))
cprop(table(Tennis_quali$continent,Tennis_quali$surface_preferee))


install.packages("svglite")
library(ggplot2)
library(svglite)


# test pour enregistrer en svg

#test<-ggplot(data=Tennis2,
       aes(x = heure_entrainement, y= Point.atp  ))+
  geom_point()+ theme_classic()+geom_smooth(method=lm,se=FALSE,fullrange=TRUE)



#ggsave("teeeest.svg", plot=test)







#####La partie qui suit servait ? generer mon mod?le pour le module quanti/quali mais finalement on l'avait refait avec les fonctions

# # REST POUR LE MODULE 3  ANALYSE QUANTI/QUALI
# 
# #Cr?ation d'une table qui va bien pour avoir un bon lien entre taille et surfacce pref
# 
# Tennis_anova<-Tennis2
# Tennis_anova<-Tennis_anova %>%
# 
#   mutate(taille= case_when(surface_preferee =="Gazon" ~ rnorm(100,1.95,0.1),
#                            surface_preferee=="Dure"~ rnorm(100,1.85,0.1),
#                            surface_preferee=="Terre_Battue"~ rnorm(100,1.77,0.081)))
# 
# Tennis_anova_bis<-Tennis_anova %>% 
#   group_by(surface_preferee) %>% 
#   summarise(n=n(),
#             contrib_var_intra = n*var(taille),
#             contrib_var_inter = n*(taille_moyenne-mean(taille))^2)
# 
# variance_intra<- sum(Tennis_anova_bis$contrib_var_intra)/100
# variance_inter<- sum(Tennis_anova_bis$contrib_var_inter)/100
# 
# taille_moyennebis<-mean(Tennis_anova$taille)
# 
# var1<-variance_intra+variance_inter
# var2<-var(Tennis6$taille)
# rapport_cor<-variance_inter/var1
# sd<-sqrt(var2)
# 
# sum((Tennis6$taille-taille_moyenne)^2)/100
# 
# BioStatR::eta2(Tennis6$taille, Tennis6$surface_preferee)
# 
#  # rename("surface pr?f?r?e"=surface_preferee)
#   
# taille_moyenne<-mean(Tennis_anova$taille)
# 
# #test en faisant des boxplot 
# ggplot(data=Tennis_anova,
#        aes(x = surface_preferee, y=taille ))+ geom_boxplot(outlier.colour = "black")+xlab("surface pr?f?r?e")
# 
# ggplot(data=Tennis_anova,
#        aes(x = surface_preferee, y=taille, color=surface_preferee ))+geom_point(size=0.2)+ theme_classic() +stat_summary(fun=mean, geom="crossbar", size=0.5, width=0.5 ,color="black") + geom_hline(yintercept =taille_moyenne,color="red", size=1)+xlab("surface pr?f?r?e") + ggtitle("R?partition  par classe en fonctions de la taille",subtitle = waiver())
# 
# ggsave("nuage_r?el_anova.svg")
# #test en faisant un nuage de points  
# ggplot(data=Tennis_anova,
#        aes(x = surface_preferee, y=taille, color=surface_preferee ))+geom_point(size=2)+ theme_classic() +stat_summary(fun=mean, geom="crossbar", size=0.5, width=0.5 ,color="black") + geom_hline(yintercept =taille_moyenne,color="red", size=1) +xlab("surface pr?f?r?e")  
# ggsave("box_plots.svg")
# 
# var(Tennis_anova$taille)
# 
# 
# BioStatR::eta2(Tennis6$taille,Tennis6$continent)
# 
# 
# 
# #res_aov<-aov(Tennis$Point.atp ~ Tennis$continent)
# #summary(res_aov)
# 
# #report(res_aov)
# BioStatR::eta2(Tennis2$Point.atp,Tennis2$continent)
# BioStatR::eta2(Tennis2$heure_entrainement,Tennis2$continent)
# install.packages(questionr)
# 
# 
# # cas de la variance intra classe nulle
# Tennis10<-Tennis2
# Tennis10<-Tennis10 %>%
#   mutate(taille= case_when(surface_preferee=="Gazon" ~ rnorm(100,1.95,0),
#                            surface_preferee=="Dure"~ rnorm(100,1.85,0.),
#                            surface_preferee=="Terre_Battue"~ rnorm(100,1.77,0.)))
# taille_moyenne10<-mean(Tennis10$taille)
# 
# ggplot(data=Tennis10,
#        aes(x = surface_preferee, y=taille, color=surface_preferee ))+geom_point(size=2)+ theme_classic() +stat_summary(fun=mean, geom="crossbar", size=0.5, width=0.5 ,color="black") + geom_hline(yintercept =taille_moyenne10,color="red", size=1)
# ggsave("var_intra_nulle.svg")
# 
# # cas de la variance inter classe nulle
# 
# Tennis11<-Tennis2
# Tennis11<-Tennis11 %>%
#   mutate(taille= case_when(surface_preferee=="Gazon" ~ runif(100,1.75,1.95),
#                            surface_preferee=="Dure"~runif(100,1.75,1.95),
#                            surface_preferee=="Terre_Battue"~ runif(100,1.75,1.95)))
# 
# 
# 
# 
#   
# taille_moyenne11<-mean(Tennis11$taille)
# 
# ggplot(data=Tennis11,
#        aes(x = surface_preferee, y=taille, color=surface_preferee ))+geom_point(size=0.2)+ theme_classic() +stat_summary(fun=mean, geom="crossbar", size=0.5, width=0.5 ,color="black") + geom_hline(yintercept =taille_moyenne11,color="red", size=1)
# 
# ggsave("var_inter_nulle3.svg")
# 
# count(Tennis11, where)
# 
# count(Tennis11)


