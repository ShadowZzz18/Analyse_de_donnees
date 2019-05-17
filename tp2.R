#BRUNA Arnaud
#PUAUX Tristan

library("plot3D");
nuage <- read.delim("/amuhome/p17013552/analyse_de_donnees/data1TP2.txt", header = TRUE);

#Question_1
#Stockage des colonnes
a <- nuage$Stature;
b <- nuage$Poids;
c <- nuage$Taille;

A <- cbind(a,b,c)
#Affichage du nuage des 10 points en 3D
scatter3D(a,b,c,add = FALSE, pch = 19);

#Question_2
#Calcul des moyennes de chaque colonnes
MoyA = sum(a)/10;
MoyB = sum(b)/10;
MoyC = sum(c)/10;

#Calcul des ecarts par rapport a la moyenne pour chaque colonne
Ac = a -MoyA;
Bc = b -MoyB;
Cc = c -MoyC;

#Creation du tableau centre B
TabB <- c(Ac,Bc,Cc);

#Convertion du tableau contenant les ecarts a la moyenne en matrice
B = cbind(Ac,Bc,Cc);
#Creation de la matrice de covariance
V = cov(B);

#Question_3
#Creation des valeurs propres et vecteurs propres
ev <- eigen(V);
#Valeurs propres dans values
values <- ev$values;
#Vecteurs propres dans vectors
vectors <- ev$vectors;

#Question_4
#Les axes principaux sont les vecteurs propres ainsi dans l'ordre ils dependent des valeurs propres.

#Question_5
#Multiplication de B par les vecteurs propres de V
TabC <- B %*% vectors;
test <- princomp(A)$scores;

#Question_6
d <- TabC[,1];
e <- TabC[,2];
f <- TabC[,3];
scatter3D(d,e,f,add = FALSE, pch = 19)
scatter3D(x= c(0,-300*vectors[1,1]), y = c(0,-300*vectors[2,1]), z = c(0,-300*vectors[3,1]), add = TRUE, type="l")

#Question_7
# Projection 2D des points sur les 2 premiers axes principaux
plot(TabC[,1], TabC[,2], xlab="Stature", ylab="Poids", col = 1:10, pch = 19)

#Question_8
# De manière générale, les résultats sont cohérents
# La stature a une majorité d'éléments supérieurs à 0, elle est donc un critère plus homogène. Elle est donc plus importante que le poids.
