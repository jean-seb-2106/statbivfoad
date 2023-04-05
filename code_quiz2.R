library(dplyr)
library(ggplot2)
library(stringr)
library(report)
library(questionr)


#Création de la base de donnée avec le vin 
vin <-sample(c("Rouge","Rosé","Blanc"),500, replace = TRUE, prob = c(0.65,0.2,0.15))
table_vin<-data.frame(vin)
set.seed(05)

table_vin<-table_vin %>% 
  mutate(medaille= case_when(Vin=="Rouge" ~ sample(c("Or","Argent","Bronze","Non médaillé"),500, replace = TRUE, prob = c(0.60,0.15,0.15,0.10)),
                             Vin=="Rosé"~ sample(c("Or","Argent","Bronze","Non m?daillé"),500, replace = TRUE, prob = c(0.1,0.35,0.350,0.2)),
                             Vin=="Blanc"~ sample(c("Or","Argent","Bronze","Non médaillé"),500, replace = TRUE, prob = c(0.10,0.20,0.25,0.45))))

lprop(table(table_vin$Vin,table_vin$medaille))
cprop(table(table_vin$Vin,table_vin$medaille))


chi2<-chisq.test(table_vin$Vin,table_vin$medaille)
N<-nrow(table_vin)
p<-nlevels(as.factor(table_vin$Vin))
q<-nlevels(as.factor(table_vin$medaille))
Vcramer<-sqrt(chi2$statistic/(N*min(p-1,q-1)))

table(table_vin$Vin,table_vin$medaille)

