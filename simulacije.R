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

# Ostanki bodo porazdeljeni Gamma(shape,rate) zato potrebujemo funkcije za standardizacijo
gammaMean <- function(shape, rate) return(shape/rate)
gammaVar <- function(shape, rate) return(shape/rate**2)
gammaScale <- function(x, shape, rate) return((x-gammaMean(shape, rate))/sqrt(gammaVar(shape, rate)))
gammaScale2 <- function(x, shape, rate) return(x/sqrt(gammaVar(shape, rate))) # Samo delimo z std odklonom

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
  eps_noscaled <- rgamma(n = n, shape = alpha, rate = 5)
  Y <- 1 + (X %*% b) + gammaScale2(eps_noscaled, shape = alpha, rate = 5) # dodamo konstanto, sicer so problemi
  
  # ostranimo x2
  X2 = X[,c(1,3)]

  # ostranimo x3
  X3 = X[,c(1,2)]

  
  results <- c()
  models <- list("ols" = lm(Y ~ X),
                 "glm" = glm(Y ~ X, family = Gamma(link="identity")),
                 "glm_inv" = glm(Y ~ X, family = Gamma(link="inverse")),
                 "ols_x2" = lm(Y ~ X2),
                 "glm_x2" = glm(Y ~ X2, family = Gamma(link="identity")),
                 "glm_inv_x2" = glm(Y ~ X2, family = Gamma(link="inverse")),
                 "ols_x3" = lm(Y ~ X3),
                 "glm_x3" = glm(Y ~ X3, family = Gamma(link="identity")),
                 "glm_inv_x3" = glm(Y ~ X3, family = Gamma(link="inverse")))
  
  for (j in length(models)){
    model_j <- models[[j]]
    model_j_name <- names(models)[j]
    
    # izračunamo IZ, probamo z navadnim confint, če ne gre vzamemo confint.default
    ci <-  tryCatch(
      error = function(cnd){
        confint_success <- 0
        confint.default(model_j)
      },
      {
        confint_success <- 1
        confint(model_j)
      }
    )
    
    # generiramo rezultate
    res_j <- c("model" = model_j_name,
               "n" = n, 
               "korelacija" = r, 
               "alpha" = alpha ,
               "alpha_X" = alpha_X,
               "pokritost_B1" = (b[1]>ci[2,1]) & (b[1] < ci[2,2]), 
               "pokritost_B2" = if (model_j_name %in% c("ols_x2", "glm_x2")) NA else (b[2]>ci[3,1]) & (b[2] < ci[3,2]), 
               "pokritost_B3" = if (model_j_name %in% c("ols_x3", "glm_x3")) NA else (b[3]>ci[4,1]) & (b[3] < ci[4,2]), 
               "sirina_B1" =  ci[2,2]-ci[2,1], 
               "sirina_B2" =  if (model_j_name %in% c("ols_x2", "glm_x2")) NA else ci[3,2]-ci[3,1],
               "sirina_B3" =  if (model_j_name %in% c("ols_x3", "glm_x3")) NA else ci[4,2]-ci[4,1],
               "B1" = as.numeric(model_j[[1]][2]),
               "B2" = as.numeric(model_j[[1]][3]),
               "B3" = as.numeric(model_j[[1]][4]),
               "confint_succes" = confint_success)
    
    # dodamo rezultate v results
    results_i <- rbind(results, res_j)
  }
  
  results_i
}

stopCluster(cl)
head(simulacija)

save("simulacija", file="rezultati_simulacije.R")

