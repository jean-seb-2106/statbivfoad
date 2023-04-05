library(dplyr)
library(ggplot2)
library(stringr)
library(report)
library(questionr)
library(forcats)


#Création de la base de donnée avec le vin 
set.seed(05)
vin <-sample(c("Rouge","Rosé","Blanc"),500, replace = TRUE, prob = c(0.65,0.2,0.15))
table_vin<-data.frame(vin)

set.seed(06)
#Ajout de la variable avec les récompenses pour chaque type de vin
table_vin <- table_vin %>% 
  mutate(medaille= case_when(vin=="Rouge" ~ sample(c("Or","Argent","Bronze","Non médaillé"),500, replace = TRUE, prob = c(0.60,0.15,0.15,0.10)),
                             vin=="Rosé"~ sample(c("Or","Argent","Bronze","Non médaillé"),500, replace = TRUE, prob = c(0.1,0.35,0.350,0.2)),
                             vin=="Blanc"~ sample(c("Or","Argent","Bronze","Non médaillé"),500, replace = TRUE, prob = c(0.10,0.20,0.25,0.45)))
         
         ) %>% 
  mutate(vin = factor(vin,levels = c("Rouge","Rosé","Blanc")),
         medaille = factor(medaille,c("Or","Argent","Bronze","Non médaillé"))) 



tab_conting <- table(table_vin$vin,table_vin$medaille)
table(table_vin$vin)
table(table_vin$medaille)
tab_conting
lprop(table(tab_conting))
cprop(table(tab_conting))


chi2<-chisq.test(table_vin$Vin,table_vin$medaille)
N<-nrow(table_vin)
p<-nlevels(as.factor(table_vin$Vin))
q<-nlevels(as.factor(table_vin$medaille))
Vcramer<-sqrt(chi2$statistic/(N*min(p-1,q-1)))

table(table_vin$Vin,table_vin$medaille)

