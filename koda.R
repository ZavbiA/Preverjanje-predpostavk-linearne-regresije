# Potrebni paketi:
library(ggplot2) # za risanje grafov
library(knitr) # lepši prikaz tabel
library(gridExtra) # lepši prikaz tabel
library(grid)
library(foreign)
library(MASS)
library(lattice)
library(multiUS) # npr. za izpis p-vrednosti
library(rms)
library(tidyverse)
library(reshape2)
library(psych)
library(Hmisc)
library(rgl)
library(blockmodeling)
library(mclust)
library(sn)
library(corrplot)

# POGLEJMO SAMO NA ENEM PRIMERU, KAJ SE DOGAJA:
n <- 100 # velikost vzorca
m <- 100 # stevilo vzorcev v simulacijah
r <- seq(from = 0, to = 0.9, by = 0.3) # korelacije, ki jih bomo upostevali

# pripravimo prostor za shranjevanje rezultatov:
R <- matrix(NA, nrow = length(r), ncol = 12)
rownames(R) <- r
b <- c(1, 1, 1, 1, 0)

for (i in 1:length(r)){
  # pripravimo prostor za shranjevanje vmesnih rezultatov
  K <- matrix(NA, nrow = m, ncol = 12)
  for (j in 1:m){
    # generiramo podatke
    # Osnovne spremenljivke, ki jih bomo potem seštevali:
    x1 <- rgamma(n,0.2,5)
    x2 <- rgamma(n,0.2,5)
    x3 <- rgamma(n,0.2,5)
    x4 <- rgamma(n,0.2,5)
    x5 <- rgamma(n,0.2,5)
    x6 <- rgamma(n,0.2,5)
    x7 <- rgamma(n,0.2,5)
    x8 <- rgamma(n,0.2,5)
    x9 <- rgamma(n,0.2,5)
    x0 <- rgamma(n,0.2,5)
    x14 <- rgamma(n,0.2,5)
    x15 <- rgamma(n,0.2,5)
    x16 <- rgamma(n,0.2,5)
    x17 <- rgamma(n,0.2,5)
    x18 <- rgamma(n,0.2,5)
    x19 <- rgamma(n,0.2,5)
    x10 <- rgamma(n,0.2,5)
    x24 <- rgamma(n,0.2,5)
    x25 <- rgamma(n,0.2,5)
    x26 <- rgamma(n,0.2,5)
    x27 <- rgamma(n,0.2,5)
    x28 <- rgamma(n,0.2,5)
    x29 <- rgamma(n,0.2,5)
    x20 <- rgamma(n,0.2,5)
    x34 <- rgamma(n,0.2,5)
    x35 <- rgamma(n,0.2,5)
    x36 <- rgamma(n,0.2,5)
    x37 <- rgamma(n,0.2,5)
    x38 <- rgamma(n,0.2,5)
    x39 <- rgamma(n,0.2,5)
    x30 <- rgamma(n,0.2,5)
    x44 <- rgamma(n,0.2,5)
    x45 <- rgamma(n,0.2,5)
    x46 <- rgamma(n,0.2,5)
    x47 <- rgamma(n,0.2,5)
    x48 <- rgamma(n,0.2,5)
    x49 <- rgamma(n,0.2,5)
    x40 <- rgamma(n,0.2,5)
    x11 <- rgamma(n,0.2,5)
    x12 <- rgamma(n,0.2,5)
    x13 <- rgamma(n,0.2,5)
    x21 <- rgamma(n,0.2,5)
    x22 <- rgamma(n,0.2,5)
    x23 <- rgamma(n,0.2,5)
    x31 <- rgamma(n,0.2,5)
    x32 <- rgamma(n,0.2,5)
    x33 <- rgamma(n,0.2,5)
    x41 <- rgamma(n,0.2,5)
    x42 <- rgamma(n,0.2,5)
    x43 <- rgamma(n,0.2,5)
    
    # Zdaj pa generiramo spremenljivke, med katerimi je korelacija 0.3:
    if (r[i]==0){
      x101 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x0
      x102 <- x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x10
      x103 <- x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x20
      x104 <- x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x30
      x105 <- x41 + x42 + x43 + x44 + x45 + x46 + x47 + x48 + x49 + x40
    }
    if (r[i]==0.3){
      x101 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x0
      x102 <- x1 + x2 + x3 + x14 + x15 + x16 + x17 + x18 + x19 + x10
      x103 <- x1 + x2 + x3 + x24 + x25 + x26 + x27 + x28 + x29 + x20
      x104 <- x1 + x2 + x3 + x34 + x35 + x36 + x37 + x38 + x39 + x30
      x105 <- x1 + x2 + x3 + x44 + x45 + x46 + x47 + x48 + x49 + x40
    }
    if (r[i]==0.6){
      x101 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x0
      x102 <- x1 + x2 + x3 + x4 + x5 + x6 + x17 + x18 + x19 + x10
      x103 <- x1 + x2 + x3 + x4 + x5 + x6 + x27 + x28 + x29 + x20
      x104 <- x1 + x2 + x3 + x4 + x5 + x6 + x37 + x38 + x39 + x30
      x105 <- x1 + x2 + x3 + x4 + x5 + x6 + x47 + x48 + x49 + x40
    }
    if (r[i]==0.9){
      x101 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x0
      x102 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
      x103 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x20
      x104 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x30
      x105 <- x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x40
    }
    
    X <- cbind(x101,x102)
    X <- cbind(X,x103)
    X <- cbind(X,x104)
    X <- cbind(X,x105)
    
    Y <- (X %*% b) + rgamma(n,1,5) # napake so tudi iz gamma
    # izvedemo linearno regresijo 
    fit <- lm(Y ~ X)
    # izracunamo intervale zaupanja
    ci <- confint(fit)
    # izracunamo pokritost intervalov zaupanja in sirine intervalov zaupanja
    K[j, ] <- c((c(0,b)>ci[,1])&(c(0,b)<ci[,2]) , ci[,2]-ci[,1])
  }
  R[i, ] <- apply(K, 2, mean)
}

dump("R", file="rezultati1.R")

par(mfrow = c(1, 2))
matplot(R[,1:6], type = "l", xlab = "r", ylab = "pokritost IZ (delez vrednosti znotraj IZ)", xaxt="n")
axis(1, at = 1:4, labels = r)
legend("topleft", col = 1:6, legend = c("presecisce", "X1", "X2", "X3", "X4", "X5"), lty = 1:6)

matplot(R[,7:12], type = "l", xlab = "r", ylab = "sirina intervala zaupanja", xaxt="n")
axis(1, at = 1:4, labels = r)
legend("topleft", col = 1:6, legend = c("presecisce", "X1", "X2", "X3", "X4", "X5"), lty = 1:6)
