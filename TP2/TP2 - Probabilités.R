library(MASS)

# 1. 
par(mfrow = c(2, 2)) # configure la fenêtre graphique pour afficher 4 histogrammes
criteres <- c("Composition", "Drawing", "Colour", "Expression") 
for (crit in criteres) {
  hist(painters[[crit]], 
       main = paste(crit), # titre
       xlab = "Note",
       breaks = seq(0, 20, by = 2))
}

# 2.
# notes_moyennes <- (painters$Composition + painters$Drawing + painters$Colour + painters$Expression)/4
notes_moyennes <- rowMeans(painters[, 1:4])
notes_moyennes

# 3.
n <= length(notes_moyennes)
moyenne_empirique <- sum(notes_moyennes) / n
variance <- sum((notes_moyennes - moyenne_empirique)^2) / (n-1)
variance_nc <- sum((notes_moyennes - moyenne_empirique)^2) / n
ecart_type <- sqrt(variance)
ecart_type_nc <- sqrt(variance_nc)
moyenne_empirique
variance
variance_nc
ecart_type
ecart_type_nc

# 4.
moyenne_empirique = mean(notes_moyennes)
variance = var(notes_moyennes)
ecart_type = sd(notes_moyennes)
moyenne_empirique
variance
ecart_type

# 5.
hist(notes_moyennes)
# Distribution de note classique autour de la moyenne, 10,5

# 6.
1 - pnorm(3)
pnorm(42, mean = 35, sd = 6)
pnorm(50, mean = 35, sd = 6) - pnorm(40, mean = 35, sd = 6)
liste <- c(5, 10, 30)
dbinom(liste-1, liste, 1/2)
1 - pbinom(14, 20, 1/2)
pbinom(15, 20, 1/2) - pbinom(10, 20, 1/2)

# 7.
alphas <- c(0.05, 0.1, 0.9)
qnorm(alphas)
qchisq(alphas, df = 10)
qt(alphas, df = 5)
qf(alphas, df1 = 2, df2 = 5)

# 8.
dloi <- function(x, b) {
  res <- (2 * x) / (b^2)
  res[x < 0 | x > b] <- 0
  return(res)
}
dloi(c(-1, 0, 1.5, 3, 4), b = 3)

# 9.
valeurs_x <- c(-1, 0, 1, 2, 3, 4, 5)
dloi(valeurs_x, b = 3)
curve(dloi(x, 3), from = -5, to = 5, main = "Densité de L(3)", ylab = "f(x)")

# 10.
# F(x) = intégrale de 0 à x de (2t/b^2) dt = x^2 / b^2
ploi <- function(x, b) {
  res <- (x^2) / (b^2)
  res[x < 0] <- 0
  res[x > b] <- 1
  return(res)
}

# 11.
curve(ploi(x, 3), from = -5, to = 5, main = "Fonction de répartition de L(3)", ylab = "F(x)")

# 12.
# F(x) = alpha <=> x^2 / b^2 = alpha <=> x = b * sqrt(alpha)
qloi <- function(alpha, b) {
  return(b * sqrt(alpha))
}
qloi(c(0, 0.25, 1), b = 3)

# 13.
rloi <- function(n, b) {
  u <- runif(n, min = 0, max = 1)
  return(qloi(u, b))
}

# 14.
par(mfrow = c(1, 2))
ech_petit <- rloi(100, b = 3)
hist(ech_petit, freq = FALSE, breaks = 15, main = "n = 100", xlab = "x")
curve(dloi(x, 3), from = 0, to = 3, col = "red", lwd = 2, add = TRUE)
ech_grand <- rloi(10000, b = 3)
hist(ech_grand, freq = FALSE, breaks = 30, main = "n = 10 000", xlab = "x")
curve(dloi(x, 3), from = 0, to = 3, col = "red", lwd = 2, add = TRUE)
par(mfrow = c(1, 1))
# quand n augmente, l'histogramme empirique colle parfaitement à la densité théorique rouge.
