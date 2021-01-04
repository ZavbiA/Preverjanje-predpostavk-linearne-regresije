library("MASS")
library("sjstats")
library("doRNG")
library("parallel")
# library("doParallel")
library("reshape2")
library("ggplot2")
library("see")
library("effectsize")

# install.packages("lcmix", repos="http://R-Forge.R-project.org") # Dependancies: nnls,  matrixStats, R.methodsS3
# install.packages("nnls","matrixStats","R.methodsS3")
library("lcmix")
# # generiramo en set podatkov
# # pojasnjevalne spremenljivke
# # GENERIRANJE ENEGA PRIMERA SETA PODATKOV
# 
# # Ostanki bodo porazdeljeni Gamma(shape,rate) zato potrebujemo funkcije za standardizacijo
gammaMean <- function(shape, rate) return(shape/rate)
gammaVar <- function(shape, rate) return(shape/rate**2)
gammaScale <- function(x, shape, rate) return((x-gammaMean(shape, rate))/sqrt(gammaVar(shape, rate)))
gammaScale2 <- function(x, shape, rate) return(x/sqrt(gammaVar(shape, rate))) # Samo delimo z std odklonom
# 
# 
# # koralacija pojsnjevalnih spremenljivk
# r <- 0.3
# # velikost vzorca
# n <- 100
# b <- c(1, 1, 1, 1, 0)
# 
# 
# 
# 
# corr_mtx = matrix(0.3, nrow=5, ncol=5)
# diag(corr_mtx) = 1
# X <- rmvgamma(n = n, shape=0.2, rate = 5, corr = corr_mtx)
# # ostanki
# eps_noscaled <- rgamma(n = n, shape = 1, rate = 5)
# # odvisna spremenljivka
# Y <- (X %*% b) + gammaScale(eps_noscaled, shape = 1, rate = 5)
# 
# # izvedemo linearno regresijo - ali se vrednosti parametrov priblizno ujemajo?
# model <- lm(Y ~ X)
# summary(model)
# par(mfrow = c(2, 2)); plot(model)
# hist(model$residuals)
# 
# # izvedemo glm regresijo, brez da specificiramo družino
# gml1 <- glm(Y ~ X)
# summary(gml1) # display results
# # Tako dobimo identične rezultate
# gml2 <- glm(Y ~ X, family = Gamma()) # Problem pri standardiziranih podatkih ker se pojavijo negativne vrednosti!
# summary(gml2) # display results


# SIMULACIJE --------------------------------------------------------------

# stevilo vzorcev v simulacijah
m <- 10

# korelacije, ki jih bomo upostevali
korelacija <- seq(from = 0, to = 0.9, by = 0.3)
# parametri za asimetricnost napak, ki jih bomo upostevali
alpha <- seq(from = 1, to = 5, by = 0.5)
# velikosti vzorcev, ki jih bomo upostevali
n <- c(10, 15, 20, 30, 50, 100, 500, 1000)

# simulacije 
zasnova <- expand.grid(korelacija, alpha, n)
zasnova <- do.call(rbind, replicate(m, zasnova, simplify=FALSE))
colnames(zasnova) <- c("korelacija", "alpha", "n")
b <- c(1, 1, 1, 1, 0)

# poglej stevilo jedr in odstej enega (za nase delo)
no_cores <- detectCores() - 1

# doloci "skupine" za delo
cl <- makeCluster(no_cores)

# PARALELNA SIMULACIJA za OLS
registerDoParallel(cl)
simulacija <- foreach(i = 1:nrow(zasnova), .combine = "rbind", .packages = c("lcmix", "dplyr", "MASS")) %dorng% {
  # uvozimo parametre 
  n <- zasnova[i, "n"]
  r <- zasnova[i, "korelacija"]
  alpha <- zasnova[i, "alpha"]
  
  # generiramo podatke
  corr_mtx = matrix(r, nrow=5, ncol=5)
  diag(corr_mtx) = 1
  X <- rmvgamma(n = n, shape=0.2, rate = 5, corr = corr_mtx)
  eps_noscaled <- rgamma(n = n, shape = alpha, rate = 5)
  Y <- (X %*% b) + gammaScale2(eps_noscaled, shape = alpha, rate = 5)
  
  # izvedemo linearno regresijo z LM in izračunamo intervale zaupanja, enako z GLM 
  model_ols <- lm(Y ~ X)
  ci_ols <- confint(model_ols)
  model_glm <- glm(Y ~ X, family = Gamma(link=log))
  ci_glm <-  tryCatch(confint(model_glm), error = function(c) {confint.default(model_glm)}) 
  
  # izracunamo pokritost intervalov zaupanja in sirine intervalov zaupanja
  results_ols <- c("n" = n, 
                   "korelacija" = r, 
                   "alpha" = alpha ,
                   "pokritost_B0" = (0 > ci_ols[1,1]) & (0 < ci_ols[1,2]), 
                   "pokritost_B1" = (b[1]>ci_ols[2,1]) & (b[1] < ci_ols[2,2]), 
                   "pokritost_B2" = (b[2]>ci_ols[3,1]) & (b[2] < ci_ols[3,2]), 
                   "pokritost_B3" = (b[3]>ci_ols[4,1]) & (b[3] < ci_ols[4,2]),
                   "pokritost_B4" = (b[4]>ci_ols[5,1]) & (b[4] < ci_ols[5,2]), 
                   "pokritost_B5" = (b[5]>ci_ols[6,1]) & (b[5] < ci_ols[6,2]), 
                   "sirina_B0" =  ci_ols[1,2]-ci_ols[1,1], 
                   "sirina_B1" =  ci_ols[2,2]-ci_ols[2,1], 
                   "sirina_B2" =  ci_ols[3,2]-ci_ols[3,1],
                   "sirina_B3" =  ci_ols[4,2]-ci_ols[4,1], 
                   "sirina_B4" =  ci_ols[5,2]-ci_ols[5,1],
                   "sirina_B5" =  ci_ols[6,2]-ci_ols[6,1],
                   "B0" = as.numeric(model_ols[[1]][1] - 0),
                   "B1" = as.numeric(model_ols[[1]][2] - b[1]),
                   "B2" = as.numeric(model_ols[[1]][3] - b[2]),
                   "B3" = as.numeric(model_ols[[1]][4] - b[3]),
                   "B4" = as.numeric(model_ols[[1]][4] - b[4]),
                   "B5" = as.numeric(model_ols[[1]][6] - b[5]))
  results_glm <- c("n" = n, 
                   "korelacija" = r, 
                   "alpha" = alpha ,
                   "pokritost_B0" = (0 > ci_glm[1,1]) & (0 < ci_glm[1,2]), 
                   "pokritost_B1" = (b[1]>ci_glm[2,1]) & (b[1] < ci_glm[2,2]), 
                   "pokritost_B2" = (b[2]>ci_glm[3,1]) & (b[2] < ci_glm[3,2]), 
                   "pokritost_B3" = (b[3]>ci_glm[4,1]) & (b[3] < ci_glm[4,2]),
                   "pokritost_B4" = (b[4]>ci_glm[5,1]) & (b[4] < ci_glm[5,2]), 
                   "pokritost_B5" = (b[5]>ci_glm[6,1]) & (b[5] < ci_glm[6,2]), 
                   "sirina_B0" =  ci_glm[1,2]-ci_glm[1,1], 
                   "sirina_B1" =  ci_glm[2,2]-ci_glm[2,1], 
                   "sirina_B2" =  ci_glm[3,2]-ci_glm[3,1],
                   "sirina_B3" =  ci_glm[4,2]-ci_glm[4,1], 
                   "sirina_B4" =  ci_glm[5,2]-ci_glm[5,1],
                   "sirina_B5" =  ci_glm[6,2]-ci_glm[6,1],
                   "B0" = as.numeric(model_glm[[1]][1] - 0),
                   "B1" = as.numeric(model_glm[[1]][2] - b[1]),
                   "B2" = as.numeric(model_glm[[1]][3] - b[2]),
                   "B3" = as.numeric(model_glm[[1]][4] - b[3]),
                   "B4" = as.numeric(model_glm[[1]][4] - b[4]),
                   "B5" = as.numeric(model_glm[[1]][6] - b[5]))
  
  # združimo rezultate
  results_i <- rbind(results_ols, results_glm) %>% as.data.frame()
  results_i$model <- c("ols", 'gls')
  results_i
}
stopCluster(cl)
head(simulacija)

dump("simulacija", file="rezultati_simulacije.R")

# ANALIZA -----------------------------------------------------------------

# pokritost
pokritost <- c("pokritost_B0", "pokritost_B1", "pokritost_B2", "pokritost_B3", "pokritost_B4", "pokritost_B5")
MeltedData <- melt(as.data.frame(simulacija), id.vars = c("model","korelacija", "alpha", "n"), measure.vars = pokritost)

ggplot(MeltedData, aes(x = alpha, y = value, col = variable)) +
  facet_grid(n~korelacija+model) +
  stat_summary(fun = mean, geom="line") +
  stat_summary(fun = mean, geom="point") +
  labs(x = "stopnja asimetrije (shape)", y = "pokritost IZ")

# sirina intervalov zaupanja
sirina <- c("sirina_B0", "sirina_B1", "sirina_B2", "sirina_B3", "sirina_B4", "sirina_B5")
MeltedData <- melt(as.data.frame(simulacija), id.vars = c("model","korelacija", "alpha", "n"), measure.vars = sirina)
ggplot(MeltedData, aes(x = alpha, y = value, col = variable)) +
  facet_grid(n~korelacija+model) +
  stat_summary(fun = mean, geom="line") +
  stat_summary(fun = mean, geom="point") +
  labs(x = "stopnja asimetrije (alpha)", y = "sirina IZ")

# pristranskost
pristranskost <- c("B0", "B1", "B2", "B3", "B4", "B5")
MeltedData <- melt(as.data.frame(simulacija), id.vars = c("model", "korelacija", "alpha", "n"), measure.vars = pristranskost)
ggplot(MeltedData, aes(x = alpha, y = value, col = variable)) +
  facet_grid(n~korelacija+model) +
  stat_summary(fun = mean, geom="line") +
  stat_summary(fun = mean, geom="point") +
  labs(x = "stopnja asimetrije (alpha)", y = "pristranskost")

# statisticna analiza
model <- aov(pokritost_B1 ~ as.factor(korelacija) * as.factor(alpha) * as.factor(n) * as.factor(model), data = as.data.frame(simulacija))
summary(model)
plot(eta_squared(model))

model <- aov(sirina_B1 ~ as.factor(korelacija) * as.factor(alpha) * as.factor(n) * as.factor(model), data = as.data.frame(simulacija))
summary(model)
plot(eta_squared(model))

model <- aov(B1 ~ as.factor(korelacija) * as.factor(alpha) * as.factor(n) * as.factor(model), data = as.data.frame(simulacija))
summary(model)
plot(eta_squared(model))
