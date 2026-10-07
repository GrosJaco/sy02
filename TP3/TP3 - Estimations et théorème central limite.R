runifa <- function(n) {
  if(!exists("param")) param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}
param

# 1. Estimateur par la méthode des moments d'ordre 1
# E(X) = X_bar  =>  a / 2 = X_bar  =>  a = 2 * X_bar
estim <- function(echantillon) {
  return(2 * mean(echantillon))
}

# 2. 
n <- 100
estim(runifa(n))
estim(runifa(n))
estim(runifa(n))
estim(runifa(n))

# 3. 
a_sims <- replicate(1000, estim(runifa(n)))

boxplot(a_sims, main = "1000 estimations successives de a", ylab = expression(hat(a)))
abline(h = param, col = "red")

cat("Médiane observée :", median(a_sims), "\n")
cat("Moyenne observée :", mean(a_sims), "\n")
cat("Vraie valeur de param :", param, "\n")

# 4. Moments d'ordre k
estim_k <- function(echantillon, k) {
  moment_empirique <- mean(echantillon^k)
  return(((k + 1) * moment_empirique)^(1 / k))
}

n_sim <- 1000
k_vals <- c(1, 2, 5, 10, 20)

estimations <- sapply(k_vals, function(k) {
  replicate(n_sim, estim_k(runifa(n), k))
})

colnames(estimations) <- paste0("k = ", k_vals)
boxplot(estimations, 
        main = "Dispersion des estimateurs selon k",
        ylab = expression(hat(a)[k]),
        col = "lightblue")
abline(h = param, col = "red", lwd = 2, lty = 2)

# Comparaison des variances pour juger de la précision
variances <- apply(estimations, 2, var)
print(variances)

# 5

runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}

a <- runknown()
x_grand <- runknown(1000000)
mean_exp <- mean(x_grand)
sd_exp <- sd(x_grand)
cat("Moyenne empirique :", mean_exp, "(théorique : 7.2)\n")
cat("Écart-type empirique :", sd_exp, "(théorique :", sqrt(32.36), ")\n")

# 6

n <- 1000
x_1000 <- runknown(n)
hist(x_1000, main = paste("Histogramme pour un échantillon de", n, "valeurs"), xlab = "Valeurs", breaks = 30)

# 7

plot(ecdf(x_1000), main = paste("Fonction de répartition de l'échantillon de", n, "valeurs"))

# 8

mu <- 7.2
sigma <- sqrt(32.36)
n <- 378 # nombre au hasard

x <- runknown(n)
T_val <- (mean(x) - mu) / (sigma / sqrt(n))
cat("Une seule réalisation de T :", T_val, "\n")

replicate(10, {
  ech <- runknown(n)
  (mean(ech) - mu) / (sigma / sqrt(n))
})
# On voit que le résultat obtenu semble correspondre à une loi normale centrée

# 9

random.T <- function(n) {
  x <- runknown(n)
  valeur <- (mean(x) - mu) / (sigma / sqrt(n))
  return(valeur)
}

# 10

n <- 100
t.1000 <- replicate(1000, random.T(n))

cat("Moyenne empirique de T :", mean(t.1000), "(attendue : 0)\n")
cat("Variance empirique de T :", var(t.1000), "(attendue : 1)\n")

# 11

plot(ecdf(t.1000), main = paste("Fonction de répartition empirique de T (n = ", n, ")"))

# 12

plot(ecdf(t.1000), main = paste("Comparaison Fn(t) avec la loi normale standard"))
curve(pnorm, col = "red", add = TRUE)

# 13

par(mfrow = c(2, 2))
tailles_n <- c(2, 5, 20, 100)

for (n_val in tailles_n) {
  t_sim <- replicate(1000, random.T(n_val))
  
  plot(ecdf(t_sim), 
       main = paste("n =", n_val), 
       xlab = "t", 
       ylab = "Fn(t)")
  curve(pnorm, col = "red", add = TRUE)
}

par(mfrow = c(1, 1))

# 14

f <- function(lambda, x) {
  dexp(x, rate = lambda)
}

# 15 

L <- function(lambda, x) {
  prod(f(lambda, x))
}

# 16

logL <- function(lambda, x) {
  sum(dexp(x, rate = lambda, log = TRUE))
}

# 17 

n <- 100
lambda_vrai <- 3
x <- rexp(n, rate = lambda_vrai)

logL_3_1 <- logL(3.1, x)
logL_2_8 <- logL(2.8, x)

cat("logL(lambda = 3.1) :", logL_3_1, "\n")
cat("logL(lambda = 2.8) :", logL_2_8, "\n")

# 18

lambdas <- seq(0, 6, 0.01) # génère une liste de valeurs allant de 0 à 6 avec un pas de 0.01
logL.lambdas <- sapply(lambdas, function(lambda) logL(lambda, x)) #Applique la fonction de logvraisemblance logL à chaque valeur de lambda
plot(lambdas, logL.lambdas, type = "l") # tracele graphe de log-vraisemblance

# 19

opt <- optimize(logL, lower = 0.01, upper = 10, x = x, maximum = TRUE)

lambda_hat_real <- opt$maximum
cat("Valeur la plus vraisemblable (lambda chapeau) :", lambda_hat_real, "\n")

# 20

sim.EMV <- function() {
  n <- 100
  lambda_vrai <- 3
  ech <- rexp(n, rate = lambda_vrai)
  
  opt <- optimize(logL, lower = 0.01, upper = 10, x = ech, maximum = TRUE)
  return(opt$maximum)
}

# 21

n_sim <- 10000
lambdas_estimes <- replicate(n_sim, sim.EMV())

# Estimation des moments
E_hat <- mean(lambdas_estimes)
Var_hat <- var(lambdas_estimes)

# Biais empirique
n <- 100
lambda_vrai <- 3
biais_empirique <- E_hat - lambda_vrai

# Biais théorique  (n / (n - 1)) * lambda - lambda
biais_theorique <- (n / (n - 1)) * lambda_vrai - lambda_vrai

cat("Espérance empirique de lambda_hat:", E_hat, "\n")
cat("Variance empirique de lambda_hat:", Var_hat, "\n")
cat("Biais empirique:", biais_empirique, "\n")
cat("Biais théorique:", biais_theorique, "\n")

