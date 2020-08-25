# File:    Cross Validation for Dependent Variable Transformations
# Author:  David Harris 
# Date:    17-04-2020


# INSTALL AND LOAD PACKAGES ################################

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, dplyr, MASS, plyr, rio, tidyverse)  


# SET WORKING DIRECTORY ####################################
setwd("C:/Users/dtunks/OneDrive - KPMG/Desktop/PD & Personal/R data analysis/R/Saved code and commands/Predictive Regression")


# LOAD & ORGANISE DATA #####################################
original_data <- rio::import("mazda.csv")

# Rename Variables for Simple Coding 
data <- original_data
names(data) <- c("Year", "y", "x")

# Inclusion of Age^2 variable
x_sq <- data$x^2                      
data <- tibble::add_column(data, x_sq, .after = 100)


# MODEL DEVELOPMENT ########################################
reg1 <- glm(y ~ x + x_sq,
            data = data)

fitted.1 <- stats::fitted(reg1)                                # assign fitted values
resid.1 <- stats::resid(reg1)                                  # assign residuals
stu.resid.1 <- MASS::studres(reg1)                             # assign studentised residuals
leverage.1 <- sur::leverage(reg1)                              # assign leverage
norm_lev.1 <- (nrow(data)*leverage.1)/(length(coef(reg1)))     # assigns normalised leverage

# Diagnostic Plots
par(mfrow = c(2, 2))                    
plot(fitted.1, data$y, main = "Actual vs Fitted", ylab = "y", xlab = "Fitted")
plot(fitted.1, stu.resid.1, main = "Studentised Residuals vs Fitted", ylab = "Student Residuals", xlab = "Fitted")
plot(norm_lev.1, stu.resid.1, main = "Studentised Residuals vs Normalised Leverage", ylab = "Student Residuals", xlab = "Normalised Leverage")
abline(v=2, col="blue")
abline(h=3, col="red")
abline(h=-3, col="red")
qqnorm(stu.resid.1, , main = "Normal quantile plot of Studentised Residuals",
       xlab = "Inverse Normal", ylab = "Studentised Residuals",
       plot.it = TRUE, datax = FALSE,)
qqline(stu.resid.1 , col="red")
par(mfrow=c(1, 1))                      


# BOX-COX TRANSFORMATION ###################################
box_cox <- MASS::boxcox(reg1, optimise = T, lambda = seq(-1,1, by=.1), main = "Box-Cox Power Transformation")
lamda_bc <- box_cox$x[which.max(box_cox$y)]
roundlamda_bc <- plyr::round_any(lamda_bc, .5)  
## We see the Box-Cox transformation suggests a Log-transformation is most appropriate, 
## however lamda is close enough to 0.5 to also consider a square-root transformation.


# LEAVE ONE OUT CV #########################################

# Function creation
dep.cv.glm <- function(x, data, mean.pred = TRUE, trans = "no"){
  if(trans == "no"){
    devn.1 <- list()
    for(i in 1:nrow(data)) {
      reg1.cv <- glm(formula(x),
                     data = dplyr::slice(data,-i))
      fitted.cv1 <- predict(reg1.cv, newdata = data)
      devn.1[i] <- ((data$y[i])-(fitted.cv1[i]))
    }
    devn.1 <- as.numeric(devn.1)
    devn.1 <- abs(devn.1)
    cv.lin <- mean(devn.1)
    cv.lin
  }
  else if((mean.pred == TRUE) & (trans == "log")){
    devn.1 <- list()
    for(i in 1:nrow(data)) {
      reg1l.cv <- glm(formula(x),
                      data = dplyr::slice(data,-i))
      logfitted.cv1l <- predict(reg1l.cv, newdata = data)
      mse <- summary(reg1l.cv)$dispersion
      fitted.cv1l <- exp(logfitted.cv1l)*exp(0.5*mse)
      devn.1[i] <- ((data$y[i])-(fitted.cv1l[i]))
    }
    devn.1 <- as.numeric(devn.1)
    devn.1 <- abs(devn.1)
    cv.log.mean <- mean(devn.1)
    cv.log.mean
  }
  else if((mean.pred == FALSE) & (trans == "log")){
    devn.1 <- list()
    for(i in 1:nrow(data)) {
      reg1l.cv <- glm(formula(x),
                      data = dplyr::slice(data,-i))
      logfitted.cv1l <- predict(reg1l.cv, newdata = data)
      mse <- summary(reg1l.cv)$dispersion
      fitted.cv1l <- exp(logfitted.cv1l)
      devn.1[i] <- ((data$y[i])-(fitted.cv1l[i]))
    }
    devn.1 <- as.numeric(devn.1)
    devn.1 <- abs(devn.1)
    cv.log.mean <- mean(devn.1)
    cv.log.mean
  }
  else if((mean.pred == TRUE) & (trans == "sqrt")){
    devn.1 <- list()
    for(i in 1:nrow(data)) {
      reg1s.cv <- glm(sqrt(y) ~ x + x_sq,
                      data = dplyr::slice(data,-i))
      sqrtfitted.cv1s <- predict(reg1s.cv, newdata = data)
      mse <- summary(reg1s.cv)$dispersion
      fitted.cv1s <- ((sqrtfitted.cv1s)^2)+mse
      devn.1[i] <- ((data$y[i])-(fitted.cv1s[i]))
    }
    devn.1 <- as.numeric(devn.1)
    devn.1 <- abs(devn.1)
    cv.sqrt.mean <- mean(devn.1)
    cv.sqrt.mean
  }
  else if((mean.pred == FALSE) & (trans == "sqrt")){
    devn.1 <- list()
    for(i in 1:nrow(data)) {
      reg1s.cv <- glm(sqrt(y) ~ x + x_sq,
                      data = dplyr::slice(data,-i))
      sqrtfitted.cv1s <- predict(reg1s.cv, newdata = data)
      mse <- summary(reg1s.cv)$dispersion
      fitted.cv1s <- ((sqrtfitted.cv1s)^2)
      devn.1[i] <- ((data$y[i])-(fitted.cv1s[i]))
    }
    devn.1 <- as.numeric(devn.1)
    devn.1 <- abs(devn.1)
    cv.sqrt.median <- mean(devn.1)
    cv.sqrt.median
  }    
}


# RESULTS ##################################################
MSE.LOO.cv <- list()

MSE.LOO.cv[1] <- dep.cv.glm(y ~ x + x_sq, data, mean.pred = TRUE, trans = "no")
MSE.LOO.cv[2] <- dep.cv.glm(log(y) ~ x + x_sq, data, mean.pred = FALSE, trans = "log")
MSE.LOO.cv[3] <- dep.cv.glm(log(y) ~ x + x_sq, data, mean.pred = TRUE, trans = "log")
MSE.LOO.cv[4] <- dep.cv.glm(sqrt(y) ~ x + x_sq, data, mean.pred = FALSE, trans = "sqrt")
MSE.LOO.cv[5] <- dep.cv.glm(sqrt(y) ~ x + x_sq, data, mean.pred = TRUE, trans = "sqrt")

names(MSE.LOO.cv) <- c("Linear", "Log-Median", "Log-Mean", "Sqrt-Median", "Sqrt-Mean")

MSE.LOO.cv

# Preferred model
reg1 <- glm(sqrt(y) ~ x + x_sq,
            data = data)

fitted.1 <- stats::fitted(reg1)                                # assign fitted values
resid.1 <- stats::resid(reg1)                                  # assign residuals
stu.resid.1 <- MASS::studres(reg1)                             # assign studentised residuals
leverage.1 <- sur::leverage(reg1)                              # assign leverage
norm_lev.1 <- (nrow(data)*leverage.1)/(length(coef(reg1)))     # assigns normalised leverage

# Diagnostic Plots (Preferred Model)
par(mfrow = c(2, 2))                    
plot(fitted.1, sqrt(data$y), main = "Actual vs Fitted", ylab = "sqrt(y)", xlab = "Fitted")
plot(fitted.1, stu.resid.1, main = "Studentised Residuals vs Fitted", ylab = "Student Residuals", xlab = "Fitted")
plot(norm_lev.1, stu.resid.1, main = "Studentised Residuals vs Normalised Leverage", ylab = "Student Residuals", xlab = "Normalised Leverage")
abline(v=2, col="blue")
abline(h=3, col="red")
abline(h=-3, col="red")
qqnorm(stu.resid.1, , main = "Normal quantile plot of Studentised Residuals",
       xlab = "Inverse Normal", ylab = "Studentised Residuals",
       plot.it = TRUE, datax = FALSE,)
qqline(stu.resid.1 , col="red")
par(mfrow=c(1, 1)) 

# LIST FUNCTIONS IN FILE ###################################
list.functions.in.file("C:/Users/dtunks/OneDrive - KPMG/Desktop/PD & Personal/R data analysis/R/Saved code and commands/Predictive Regression/Cross Validation (Dependent Transformations)", alphabetic = T) 

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
