# 1
log(640320^3 + 744)/(sqrt(163))
pi

# 2
notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10)
notes <- c(notes, 4)
notes

# 3
v <- c(1, 2, 3, 4, 5)
u <- c(5, 4, 3, 2, 1)
v + 1
2*v
v/3
u * v
u == 2
v > pi

# 4
notes10 <- notes/2
notes10
notes10 > 6
# On compte 5 élèves ayant eu plus de 6

# 5
moyenne3p <- (notes10[1] + notes10[3] + notes10[length(notes)])/3
moyenne3p

# 6
l <- length(notes[notes>10])
l
# La fonction length donne 5 notes strictement supérieures à 10

#7
notesfloored <- floor(notes)
entiers <- notesfloored == notes
minnotesentieres <- min(notes[entiers])
minnotesentieres

# 8
notes2 <- notes - 2
notes2

# 9
# Il y a 2 notes négatives
negatives = notes2 < 0
notes2[negatives] <- 0
notes2

# 10
sequenceadn <- strsplit("ACAAGATGCCATTGTC", split = "")[[1]]
facteuradn <- factor(sequenceadn)
levels(facteuradn)
nlevels(facteuradn)

# 11
table(facteuradn)

# 12
X <- read.csv("sy02.data")
length(X) # Nombre de colonnes
ncol(X) # Nombre de colonnes
nrow(X) # Nombre de lignes
names(X) # Nom des colonnes

# 13
head(X) # Premiers éléments
summary(X) # Donne des statistiques sur chaque colonne
# Pour les variables quantitatives, on peut relever:
# median, final, moyenne
# Pour les variables qualitatives, on peut relever:
# correcteur.median, correcteur.final, resultat

# 14
X[,2]
X[,ncol(X)]

# 15
mean(X$median[X$correcteur.median == "EG"])

# 16
mean(X$final > X$median)

# 17
mean(X$final)
sd(X$final) # Ecart-type
var(X$final)
median(X$final)
max(X$final) 
min(X$final)
max(X$final) - min(X$final)

# 18
summary(X$final)

# 19
# Calcul des quantiles (Q1 et Q3)
quantile(X$median, probs = c(0.25, 0.75))

# Étendue inter-quartiles manuelle
quantile(X$median, 0.75) - quantile(X$median, 0.25)
IQR(X$median)

# 20
notes_triees <- sort(X$median)
n <- length(notes_triees)
notes_tronquees <- notes_triees[11:(n - 10)] # Exclusion des 10 premières et 10 dernières valeurs
mean(notes_tronquees)

# 21
tab_med <- table(X$correcteur.median)
tab_med
barplot(tab_med, col = "blue", xlab = "Correcteur", ylab = "Effectif",main = "Nombre de copies corrigées par correcteur (médian)")
names(which.max(tab_med))

# 22
bp <- boxplot(X$final, main = "Boîte à moustaches des notes du final", ylab = "Notes")
summary(X$final)
IQR(X$final)

