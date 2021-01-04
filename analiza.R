# ANALIZA -----------------------------------------------------------------
source("rezultati_simulacije.R")
# pokritost
simulacija = as.data.frame(simulacija)
pokritost <- c("pokritost_B0", "pokritost_B1", "pokritost_B2", "pokritost_B3")
MeltedData <- melt(simulacija[simulacija$model == 'glm'| simulacija$model == 'ols',], id.vars = c("model","korelacija", "alpha", "n"), measure.vars = pokritost)

ggplot(MeltedData, aes(x = alpha, y = value, col = variable)) +
  facet_grid(n~korelacija+model) +
  stat_summary(fun = mean, geom="line") +
  stat_summary(fun = mean, geom="point") +
  labs(x = "stopnja asimetrije (alpha)", y = "pokritost IZ")

# sirina intervalov zaupanja
sirina <- c("sirina_B0", "sirina_B1", "sirina_B2", "sirina_B3")
MeltedData <- melt(as.data.frame(simulacija), id.vars = c("model","korelacija", "alpha", "n"), measure.vars = sirina)
ggplot(MeltedData, aes(x = alpha, y = value, col = variable)) +
  facet_grid(n~korelacija+model) +
  stat_summary(fun = mean, geom="line") +
  stat_summary(fun = mean, geom="point") +
  labs(x = "stopnja asimetrije (alpha)", y = "sirina IZ")

# pristranskost
pristranskost <- c("B0", "B1", "B2", "B3")
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
