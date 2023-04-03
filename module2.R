library(questionr)
library(kableExtra)

#import de la base
tennis <-read.csv("donnee_tennis.csv")

# Etude des corrélations entre deux variables quantitatives--------


# rajout de plus d'individus pour que ca marche bien avec le khi2. 
#doublement de la base de données pour que ça marche mieux
tennis_quali<-rbind(tennis,tennis)

chi2<-chisq.test(tennis_quali$continent, tennis_quali$surface_preferee)$statistic
N<-nrow(tennis_quali)
p<-nlevels(as.factor(tennis_quali$continent))
q<-nlevels(as.factor(tennis_quali$surface_preferee))
vcramer<-sqrt(chi2/(N*min(p-1,q-1)))

table(tennis_quali$continent,tennis_quali$surface_preferee)
lprop(table(tennis_quali$continent,tennis_quali$surface_preferee))
cprop(table(tennis_quali$continent,tennis_quali$surface_preferee))

#Exercice pour le fichier détail : 

couleur_préférée <- c("bleu","vert","rouge","rouge","bleu","rouge","bleu","rouge","vert","rouge",
             "bleu","vert","rouge","rouge","bleu","rouge","bleu","rouge","vert","rouge")
sexe <- c("homme","femme","femme","femme","homme","homme","femme","femme","femme","homme",
          "homme","femme","femme","femme","homme","homme","femme","femme","femme","homme")
data_exo1 <- data.frame(couleur_préférée,sexe)
table(data_exo1$couleur_préférée,data_exo1$sexe)

data_exo1

