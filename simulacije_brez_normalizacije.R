library("sjstats")
library("doRNG")
library("parallel")
library("doParallel")
library("reshape2")
library("ggplot2")
library("see")
library("effectsize")
library("stats")
library("MASS")

# install.packages("lcmix", repos="http://R-Forge.R-project.org") # Dependancies: nnls,  matrixStats, R.methodsS3
# install.packages("nnls","matrixStats","R.methodsS3")
library("lcmix")

#library("lme4") # ali to rabimo?

# # GENERIRANJE ENEGA PRIMERA SETA PODATKOV
# # Ostanki bodo porazdeljeni Gamma(shape,rate) zato potrebujemo funkcije za standardizacijo
gammaMean <- function(shape, rate) return(shape/rate)
gammaVar <- function(shape, rate) return(shape/rate**2)
gammaScale <- function(x, shape, rate) return((x-gammaMean(shape, rate))/sqrt(gammaVar(shape, rate))) # standardizacija
gammaScale2 <- function(x, shape, rate) return(x/sqrt(gammaVar(shape, rate))) # Samo delimo s std odklonom
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
m <- 1000

# korelacije, ki jih bomo upostevali
korelacija <- seq(from = 0, to = 0.9, by = 0.3)
# parametri za asimetricnost napak, ki jih bomo upostevali
alpha <- seq(from = 1, to = 5, by = 2)
# velikosti vzorcev, ki jih bomo upostevali
n <- c(10, 50, 100, 500, 1000)
alpha_X = c(2,5)

# simulacije 
zasnova <- expand.grid(korelacija, alpha, n, alpha_X)
zasnova <- do.call(rbind, replicate(m, zasnova, simplify=FALSE))
colnames(zasnova) <- c("korelacija", "alpha", "n", "alpha_X")
b <- c(1, 1, 0)

# poglej stevilo jeder in odstej enega (za nase delo)
no_cores <- detectCores() - 1

# doloci "skupine" za delo
cl <- makeCluster(no_cores)

# PARALELNA SIMULACIJA za OLS
registerDoParallel(cl)
simulacija <- foreach(i = 1:nrow(zasnova), .combine = "rbind", .packages = c("lcmix", "dplyr", "MASS", "lme4")) %dorng% {
#for (i in 1:nrow(zasnova)){
  # uvozimo parametre 
  n <- zasnova[i, "n"]
  r <- zasnova[i, "korelacija"]
  alpha <- zasnova[i, "alpha"]
  alpha_X <- zasnova[i, "alpha_X"]
  
  # generiramo podatke
  corr_mtx = matrix(r, nrow=3, ncol=3)
  diag(corr_mtx) = 1
  X <- rmvgamma(n = n, shape=alpha_X, rate = 5, corr = corr_mtx)
  Y <- 1 + (X %*% b) + rgamma(n = n, shape = alpha, rate = 5)
  
  # izvedemo linearno regresijo z LM in izracunamo intervale zaupanja, enako z GLM 
  model_ols <- lm(Y ~ X)
  ci_ols <- confint(model_ols)
  model_glm <- glm(Y ~ X, family = Gamma(link="identity"))
  ci_glm <-  confint(model_glm)
  
  # ostranimo x2
  X2 = X[,c(1,3)]
  model_ols_2 <- lm(Y ~ X2)
  ci_ols_2 <- confint(model_ols_2)
  model_glm_2 <- glm(Y ~ X2, family = Gamma(link="identity"))
  ci_glm_2 <-  confint(model_glm_2)  
  
  # ostranimo x3
  X3 = X[,c(1,2)]
  model_ols_3 <- lm(Y ~ X3)
  ci_ols_3 <- confint(model_ols_3)
  model_glm_3 <- glm(Y ~ X3, family = Gamma(link="identity"))
  ci_glm_3 <-  confint(model_glm_3)  
  
  # izracunamo pokritost intervalov zaupanja in sirine intervalov zaupanja
  # res = data.frame()
  # models = list("ols" = model_ols, "glm" = model_glm)
  # for (j in 1:length(models)){
  #   model_j = models[j][[1]]
  #   model_j_name = names(models)[j]
  #   ci = confint(model_j)
  #   
  #   bOrg <- model_j$coefficients
  #   
  #   #naivni interval zaupanja
  #   bNaiv <- t(apply(res[,1:3], 2, FUN = quantile, probs=c(0.05/2,1-0.05/2)))
  #   # obrnjen interval zaupanja
  #   cbind(2*bOrg,2*bOrg) - bNaiv[, c(2,1)]
  #   
  #   
  #   res_i = data.frame("n"= n,
  #                      "model" = model_j_name,
  #                      "korelacija"=r,
  #                      "alpha" = alpha,
  #                      "parameter" = paste0("b",0:3),
  #                      "pokritost" = sapply(0:3, function(x){
  #                        if(x==0){
  #                          (0 > ci[1,1]) & (0 < ci[1,2])
  #                          }else{
  #                            (b[x]>ci[x+1,1]) & (b[x] < ci[x+1,2])
  #                            }
  #                        }
  #                        )
  #                      )
  #   res = rbind(res, res_i)
  # }
  
  results_ols <- c("n" = n, 
                   "korelacija" = r, 
                   "alpha" = alpha ,
                   "alpha_X" = alpha_X,
                   "pokritost_B0" = (1 > ci_ols[1,1]) & (1 < ci_ols[1,2]), 
                   "pokritost_B1" = (b[1]>ci_ols[2,1]) & (b[1] < ci_ols[2,2]), 
                   "pokritost_B2" = (b[2]>ci_ols[3,1]) & (b[2] < ci_ols[3,2]), 
                   "pokritost_B3" = (b[3]>ci_ols[4,1]) & (b[3] < ci_ols[4,2]), 
                   "sirina_B0" =  ci_ols[1,2]-ci_ols[1,1], 
                   "sirina_B1" =  ci_ols[2,2]-ci_ols[2,1], 
                   "sirina_B2" =  ci_ols[3,2]-ci_ols[3,1],
                   "sirina_B3" =  ci_ols[4,2]-ci_ols[4,1],
                   "B0" = as.numeric(model_ols[[1]][1]),
                   "B1" = as.numeric(model_ols[[1]][2]),
                   "B2" = as.numeric(model_ols[[1]][3]),
                   "B3" = as.numeric(model_ols[[1]][4]))
  
  results_glm <- c("n" = n, 
                   "korelacija" = r, 
                   "alpha" = alpha ,
                   "alpha_X" = alpha_X,
                   "pokritost_B0" = (1 > ci_glm[1,1]) & (1 < ci_glm[1,2]), 
                   "pokritost_B1" = (b[1]>ci_glm[2,1]) & (b[1] < ci_glm[2,2]), 
                   "pokritost_B2" = (b[2]>ci_glm[3,1]) & (b[2] < ci_glm[3,2]), 
                   "pokritost_B3" = (b[3]>ci_glm[4,1]) & (b[3] < ci_glm[4,2]), 
                   "sirina_B0" =  ci_glm[1,2]-ci_glm[1,1], 
                   "sirina_B1" =  ci_glm[2,2]-ci_glm[2,1], 
                   "sirina_B2" =  ci_glm[3,2]-ci_glm[3,1],
                   "sirina_B3" =  ci_glm[4,2]-ci_glm[4,1],
                   "B0" = as.numeric(model_glm[[1]][1]),
                   "B1" = as.numeric(model_glm[[1]][2]),
                   "B2" = as.numeric(model_glm[[1]][3]),
                   "B3" = as.numeric(model_glm[[1]][4]))
  
  results_ols_x2 <- c("n" = n, 
                      "korelacija" = r, 
                      "alpha" = alpha ,
                      "alpha_X" = alpha_X,
                      "pokritost_B0" = (1 > ci_ols_2[1,1]) & (1 < ci_ols_2[1,2]), 
                      "pokritost_B1" = (b[1]>ci_ols_2[2,1]) & (b[1] < ci_ols_2[2,2]), 
                      "pokritost_B2" = NA,
                      "pokritost_B3" = (b[3]>ci_ols_2[3,1]) & (b[3] < ci_ols_2[3,2]), 
                      "sirina_B0" =  ci_ols_2[1,2]-ci_ols_2[1,1], 
                      "sirina_B1" =  ci_ols_2[2,2]-ci_ols_2[2,1], 
                      "sirina_B2" = NA,
                      "sirina_B3" =  ci_ols_2[3,2]-ci_ols_2[3,1],
                      "B0" = as.numeric(model_ols_2[[1]][1]),
                      "B1" = as.numeric(model_ols_2[[1]][2]),
                      "B2" = NA,
                      "B3" = as.numeric(model_ols_2[[1]][3]))

  results_glm_x2 <- c("n" = n, 
                      "korelacija" = r, 
                      "alpha" = alpha ,
                      "alpha_X" = alpha_X,
                      "pokritost_B0" = (1 > ci_glm_2[1,1]) & (1 < ci_glm_2[1,2]), 
                      "pokritost_B1" = (b[1]>ci_glm_2[2,1]) & (b[1] < ci_glm_2[2,2]), 
                      "pokritost_B2" = NA,
                      "pokritost_B3" = (b[3]>ci_glm_2[3,1]) & (b[3] < ci_glm_2[3,2]), 
                      "sirina_B0" =  ci_glm_2[1,2]-ci_glm_2[1,1], 
                      "sirina_B1" =  ci_glm_2[2,2]-ci_glm_2[2,1],
                      "sirina_B2" = NA,
                      "sirina_B3" =  ci_glm_2[3,2]-ci_glm_2[3,1],
                      "B0" = as.numeric(model_glm_2[[1]][1]),
                      "B1" = as.numeric(model_glm_2[[1]][2]),
                      "B2" = NA,
                      "B3" = as.numeric(model_glm_2[[1]][3]))
  
  results_ols_x3 <- c("n" = n, 
                      "korelacija" = r, 
                      "alpha" = alpha ,
                      "alpha_X" = alpha_X,
                      "pokritost_B0" = (1 > ci_ols_3[1,1]) & (1 < ci_ols_3[1,2]), 
                      "pokritost_B1" = (b[1]>ci_ols_3[2,1]) & (b[1] < ci_ols_3[2,2]), 
                      "pokritost_B2" = (b[2]>ci_ols_3[3,1]) & (b[2] < ci_ols_3[3,2]), 
                      "pokritost_B3" = NA,
                      "sirina_B0" =  ci_ols_3[1,2]-ci_ols_3[1,1], 
                      "sirina_B1" =  ci_ols_3[2,2]-ci_ols_3[2,1], 
                      "sirina_B2" =  ci_ols_3[3,2]-ci_ols_3[3,1],
                      "sirina_B3" = NA,
                      "B0" = as.numeric(model_ols_3[[1]][1]),
                      "B1" = as.numeric(model_ols_3[[1]][2]),
                      "B2" = as.numeric(model_ols_3[[1]][3]),
                      "B3" = NA)
  
  results_glm_x3 <- c("n" = n, 
                      "korelacija" = r, 
                      "alpha" = alpha ,
                      "alpha_X" = alpha_X,
                      "pokritost_B0" = (1 > ci_glm_3[1,1]) & (1 < ci_glm_3[1,2]), 
                      "pokritost_B1" = (b[1]>ci_glm_3[2,1]) & (b[1] < ci_glm_3[2,2]), 
                      "pokritost_B2" = (b[2]>ci_glm_3[3,1]) & (b[2] < ci_glm_3[3,2]), 
                      "pokritost_B3" = NA,
                      "sirina_B0" =  ci_glm_3[1,2]-ci_glm_3[1,1], 
                      "sirina_B1" =  ci_glm_3[2,2]-ci_glm_3[2,1], 
                      "sirina_B2" =  ci_glm_3[3,2]-ci_glm_3[3,1],
                      "sirina_B3" = NA,
                      "B0" = as.numeric(model_glm_3[[1]][1]),
                      "B1" = as.numeric(model_glm_3[[1]][2]),
                      "B2" = as.numeric(model_glm_3[[1]][3]),
                      "B3" = NA) 
  
   
  # združimo rezultate
  results_i <- do.call("rbind", list(results_ols, results_glm, results_ols_x2, results_glm_x2, results_ols_x3, results_glm_x3)) %>% as.data.frame()
  results_i$model <-c("ols", 'glm',"ols_x2", 'glm_x2',"ols_x3", 'glm_x3')
  results_i
}
stopCluster(cl)
head(simulacija)

save("simulacija", file="rezultati_simulacije_brez_norm.R")

