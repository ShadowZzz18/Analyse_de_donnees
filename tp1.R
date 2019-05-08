#BRUNA Arnaud
#PUAUX Tristan

Database <- read.delim("/amuhome/p17013552/analyse_de_donnees/data1TP1.txt", header = TRUE)
Database2 <- read.delim("/amuhome/p17013552/analyse_de_donnees/data2TP1.txt", header = TRUE)
tBase <- read.delim("/amuhome/p17013552/analyse_de_donnees/t_table.pdf", header = TRUE)


#Question_1
attach(Database)
par(mfrow=c(2,3))
plot(Database$A, Database$Y)
plot(Database$B, Database$Y)
plot(Database$C, Database$Y)
plot(Database$D, Database$Y)
plot(Database$E, Database$Y)
#On observe que : 
# - Database$A : Fonction decroissante monotone, relation non-lineaire
# - Database$B : Fontion croissante monotone, relation non-lineaire
# - Database$C : Nuage de points aleatoires, pas de relation
# - Database$D : Fontion exponentielle, relation non-lineaire
# - Database$E : Fontion parabolique, relation non-lineaire

#Question_2
coeffpearson <- function(X,Y) {
  r <- cov(X,Y)/(sd(X) * sd(Y))
  return(r)
}

coeffpearson(Database$A, Database$Y)
correlation <- c(1,5)
correlation[1] = coeffpearson(Database$A, Database$Y)
correlation[2] = coeffpearson(Database$B, Database$Y)
correlation[3] = coeffpearson(Database$C, Database$Y)
correlation[4] = coeffpearson(Database$D, Database$Y)
correlation[5] = coeffpearson(Database$E, Database$Y)
which.min(abs(correlation))
# c'est E qui a la plus petite correlation (plus proche de 0), car c'est une parabole
#cor(Database$A, Database$Y)

#Question_3
coeffspearman <- function(X,Y) {
  sum = 0
  rX = rank(X)
  rY = rank(Y)

  sum <- 6 * sum((rX - rY)^2)
  sous <- length(X)^3 - length(X)
  sum <- sum / sous
  sum <- 1 - sum
  return(sum)
}

coeffspearman(Database$A, Database$Y)
score <- c(1,5)
score[1] = coeffspearman(Database$A, Database$Y)
score[2] = coeffspearman(Database$B, Database$Y)
score[3] = coeffspearman(Database$C, Database$Y)
score[4] = coeffspearman(Database$D, Database$Y)
score[5] = coeffspearman(Database$E, Database$Y)
which.min(abs(score))
#Le resultat minimum est egalement a l'indice 5 et est plus grand que celui du tableau correlation
#cor(Database$A,Databse$Y,method="spearman")

#Question_4
# Il existe diverses facons de calculer la relation non-lineaire et non-monotone entre
#variables E et Y tel que : faire une transformation, utiliser une fonction non-lineaire...

#Question_5
independance<-function(X){
  moy<-mean(X)
  ecart<-sd(X)
  res<-abs(moy-19)/(ecart/sqrt(length(X)))
  return(res)
}

independance(Database2$Marseille)
#On regarde sur le fichier pdf t table avec les parametres suivants : 2 tailed, level of signifiance : 0.05,
#degre de liberte : n - 1 (15 - 1 = 14). On remarque que dans le fichier pdf la valeur est de 2.145 
# et ici nous obtenons 2.177 ce qui montre que avec alpha = 5% l'inflation n'affecte pas tellement le cout
# de la vie a marseille.

#Question_6
independance2<-function(X,Y){
  moyX<-mean(X)
  moyY<-mean(Y)
  ecartX<-sd(X)
  ecartY<-sd(Y)
  res<-abs(moyX-moyY)/sqrt(((ecartX^2)/length(X))+((ecartY^2)/length(Y)))
  return(res)
}

independance2(Database2$Marseille,Database2$Aix)
#On regarde sur le fichier pdf t table avec les parametres suivants : 2 tailed, level of signifiance : 0.05/0.02,
#degré de liberte : n + n - 2 (15 + 15 - 2 = 28)
# avec alpha = 5% : etant donne que le resultat obtenu est de 2.32 au lieu de 2.048 
# il y a une dependance significative entre Marseille et Aix. 
# avec alpha = 2% : on a 2.468 ce qui est nettement plus proche et superieur a notre valeur, 
#il y a donc moins de dependance.

#Quesion_7_a
senteur<-matrix(c(9,3,3,1,1528,106,117,381) ,ncol = 4 ,byrow = TRUE)
colnames(senteur) <- c("violet,long", "violet,rond", "rouge,long", "rouge,rond")
rownames(senteur) <- c("ratio genetique", "observe")
senteur <- as.data.frame(senteur)

valTheorique <- function(ratio) {
  sum_ratio = 16
  sum_observe = 2132
  res = (ratio/sum_ratio)*sum_observe
  return(res)
}

E <- c(1:4)
E[1] <- valTheorique(senteur$`violet,long`[1])
E[2] <- valTheorique(senteur$`violet,rond`[1])
E[3] <- valTheorique(senteur$`rouge,long`[1])
E[4] <- valTheorique(senteur$`rouge,rond`[1])

#Question_7_b
khideux <- function () {
  res = 0
  o <- c(1:4)
  for (i in c(1:4)) {
    o[i] = senteur[2,i]
  }
  for (i in 1:4) {
    res = res + (o[i] - E[i])^2/E[i]
  }
  return (res)
}

khideux()

#Question_7_c
# En regardant le fichier pdf table_khi_2 on remarque que nous devrions avoir une valeur inferieure ou egale a
#7.81 donc nous sommes largement au dessus l'hypothese est fausse.

#Question_8

form = rbind(c(29,5,46), c(40,32,8), c(18,22,0))
color = rbind(c(20,60), c(29,51), c(12,28))
tot = sum(form)

khideux_indep <- function(mat) {
  t <- matrix(nrow=nrow(mat), ncol=ncol(mat))
  khi_deux = 0
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      t[i,j] = (sum(mat[i,])*sum(mat[,j]))/tot
      khi_deux =  khi_deux + ((mat[i,j]-t[i,j])^2/t[i,j])
    }
  }
  return (khi_deux)
}

#On remarque que le resultat de khideux_indep(form) est 75.1564 > la valeur obtenue avec 
#alpha = 5% donc la variable est dependante
khideux_indep(form)
#On remarque que le resultat de khideux_indep(color) est 2.39415 < la valeur obtenue avec 
#alpha = 5% donc la variable est independante
khideux_indep(color)

#La premiere variable etant dependante elle est importante pour detecter un melanome

#Question_9
#D'apres les questions precedentes, le test Student/t est parametrique car il se base sur une distribution 
#statistique sur les donnees
#A contrario, le test du Khi Deux est non parametrique car il ne se base pas sur une distribution statistique.
#Ce test utilise les donnees de maniere independante.

#Question_10

#Nous ne pouvons pas appliquer les coefficents de Pearson et Spearman car ils permettent respectivement d'analyser
#des relations lineaires et non lineaires en utilisant des methodes de correlation.

detach(Database)

