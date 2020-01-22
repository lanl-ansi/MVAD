## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MVAD)

## ----get_data-----------------------------------------------------------------
dt.data <- get_test_data()
head(dt.data)
summary(dt.data)
dim(dt.data)

## ----plot_data----------------------------------------------------------------
library(ggplot2)
library(Rmisc)
p1 <- ggplot(dt.data, aes(x=(1:nrow(dt.data))/1800, y=v1.Y)) +
  geom_point(shape = ".", colour = "black") +
  labs(title = "Variable 1") + xlab("time (mins)") + ylab("unit for var1")
p2 <- ggplot(dt.data, aes(x=(1:nrow(dt.data))/1800, y=v2.Y)) +
  geom_point(shape = ".", colour = "black") +
  labs(title = "Variable 2") + xlab("time (mins)") + ylab("unit for var2")
p3 <- ggplot(dt.data, aes(x=(1:nrow(dt.data))/1800, y=v3.Y)) +
  geom_point(shape = ".", colour = "black") +
  labs(title = "Variable 3") + xlab("time (mins)") + ylab("unit for var3")
p4 <- ggplot(dt.data, aes(x=(1:nrow(dt.data))/1800, y=v4.Y)) +
  geom_point(shape = ".", colour = "black") +
  labs(title = "Variable 4") + xlab("time (mins)") + ylab("unit for var4")
multiplot(p1, p2, p3, p4, cols=2)

## ----partition_data-----------------------------------------------------------
dt.training_data <- dt.data[1:(1/1.1 * nrow(dt.data)),]
dt.testing_data <- dt.data[(1/1.1 * nrow(dt.data) + 1): nrow(dt.data),]

## ----setup model--------------------------------------------------------------
dt.ad <- setup_model()
show(dt.ad)

## ----add_data-----------------------------------------------------------------
dt.ad <- add_data(dt.ad, training_data = dt.training_data, testing_data = dt.testing_data)

## ----preprocess_data----------------------------------------------------------
dt.ad <- preprocess_data(dt.ad)
#>
xs <- (1:nrow(dt.ad$normalized_data$standardized_data))/120
p1 <- ggplot(dt.ad$normalized_data$standardized_data, aes(x=xs, y=v1.Y)) +
  geom_point(shape = "x", colour = "black") +
  labs(title = "Variable 1") + xlab("time (mins)")
p2 <- ggplot(dt.ad$normalized_data$standardized_data, aes(x=xs, y=v2.Y)) +
  geom_point(shape = "x", colour = "black") +
  labs(title = "Variable 2") + xlab("time (mins)") 
p3 <- ggplot(dt.ad$normalized_data$standardized_data, aes(x=xs, y=v3.Y)) +
  geom_point(shape = "x", colour = "black") +
  labs(title = "Variable 3") + xlab("time (mins)") 
p4 <- ggplot(dt.ad$normalized_data$standardized_data, aes(x=xs, y=v4.Y)) +
  geom_point(shape = "x", colour = "black") +
  labs(title = "Variable 4") + xlab("time (mins)")
multiplot(p1, p2, p3, p4, cols=2)

## ----train_model--------------------------------------------------------------
dt.ad <- train_model(dt.ad)
show(dt.ad$model)

## ----residual_data------------------------------------------------------------
dt.ad <- get_residual(dt.ad)

## ----score_data---------------------------------------------------------------
dt.ad <- get_anomaly_score(dt.ad)
ggplot(dt.ad$scores, aes(x=1:nrow(dt.ad$scores)/30, y=Mahalanobis)) +
  geom_point(shape = "x", colour = "black") +
  labs(title = "Multivariate Anomaly Score") + xlab("time (sec)")

