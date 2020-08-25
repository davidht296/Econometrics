# File:    Predictive Regression
# Author:  David Harris 
# Date:    03-04-2020

# INSTALL AND LOAD PACKAGES ################################

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, aplot, boot, car, caret, expss, GGally,
               ggthemes, ggvis, httr, huxtable, jtools, lars, 
               lmtest, lubridate, 
               MASS, olsrr, plotly, plyr, 
               psych, rio, rmarkdown, sandwich, shiny, sur, 
               tidyverse)  

# package descriptions and help
p_help("sur", web = F)

# SET WORKING DIRECTORY ####################################
getwd()
setwd("C:/Users/dtunks/OneDrive - KPMG/Desktop/PD & Personal/R data analysis/R/Saved code and commands/Predictive Regression")
### remember to change \ to / ###

# LOAD DATA ################################################
original_data <- rio::import("nyc.csv")
head(data)  # view top few lines of data

#or
# Import data manually
# "Import Dataset" drop-down in the Environment window.
# see video https://www.youtube.com/watch?v=cnD1op2Oo3M

# ORGANISE DATA ############################################

# Rename Variables for Simple Coding 
data <- original_data
names(data) <- c("Case", "Restaurant", "y", "x1", "x2", "x3", "x4")
AA_names_list <- list()
AA_names_list <- c("Case = Case", "Restaurant = Restaurant", "y = Price", "x1 = Food", "x2 = Decor", "x3 = Service", "x4 = East")
AA_names_list

# VISUALISE DATA ###########################################

# Histograms
par(mfrow=c(1, 2))  
graphics::hist(data$y,
     breaks = 20,          # "Suggests" 20 bins
     freq   = F,           # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Dependent Variable"),
     xlab   = "y")
# Add a normal distribution
graphics::curve(dnorm(x, mean = mean(data$y), sd = sd(data$y)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph
# Add a kernel density
graphics::lines(density(data$y, adjust = 2), col = "purple", lwd = 2)  #adjust command uses moving average to smooth kernel density plot

graphics::hist(data$x1,
     breaks = 20,          # "Suggests" 20 bins
     freq   = F,           # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Independent Variable - x1"),
     xlab   = "x1")
# Add a normal distribution
graphics::curve(dnorm(x, mean = mean(data$x1), sd = sd(data$x1)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph
# Add a kernel density
graphics::lines(density(data$x1, adjust = 2), col = "purple", lwd = 2)  #adjust command uses moving average to smooth kernel density plot
par(mfrow=c(1, 1))

# Scatter Plots
graphics::plot(data$x1, data$y, 
     main = paste("Scatter y on x1"),
     ylab = "y",
     xlab = "x1")

# Scatter Plot Matrix
graphics::pairs(data[3:7])
graphics::pairs(original_data[3:7])

# WORK WITH DATA ###########################################

# Regression
reg1 <- stats::glm(y ~ x1 + x2 + x3 + x4,
                   data = data)
summ(reg1)                                  # regression output
                                            # where "dispersion parameted is the estimate for sigma-sq
rmse <- sqrt(summary(reg1)$dispersion)      # assign RMSE 
stats::anova(reg1)                          # Analysis of Variance Table

fitted.1 <- stats::fitted(reg1)                                # assign fitted values
resid.1 <- stats::resid(reg1)                                  # assign residuals
stu.resid.1 <- MASS::studres(reg1)                             # assign studentised residuals
leverage.1 <- sur::leverage(reg1)                              # assign leverage
norm_lev.1 <- (nrow(data)*leverage.1)/(length(coef(reg1)))     # assigns normalised leverage

data <- tibble::add_column(data, resid.1, .after = 100)       # tibble package installed with tidyverse
data <- tibble::add_column(data, stu.resid.1, .after = 100)   # note .after = 100 to put column at end
data <- tibble::add_column(data, leverage.1, .after = 100)
data <- tibble::add_column(data, norm_lev.1, .after = 100)
data <- tibble::add_column(data, fitted.1, .after = 100)      

# Diagnostic Plots
par(mfrow = c(2, 2))                      # Put graphs in 2 rows and 2 column
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
par(mfrow=c(1, 1))                        # Restore graphic parameter

hist(stu.resid.1,
     breaks = 10,          # "Suggests" 14 bins
     freq   = F,           # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Studentised Residuals - R1"),
     xlab   = "Studentised Residuals")
# Add a normal distribution
curve(dnorm(x, mean = mean(stu.resid.1), sd = sd(stu.resid.1)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph
# Add a kernel density
lines(density(stu.resid.1, adjust = 2), col = "purple", lwd = 2)  #adjust command uses moving average to smooth kernel density plot

# ADDED VARIABLE PLOTS #####################################
car::avPlots(reg1)

# Prediction Intervals
mean_x1 <- rep(mean(data$x1), times = nrow(data))
dm_x1 <- data$x1-mean_x1
Sx1 <- sum((dm_x1)^2)

mean_x2 <- rep(mean(data$x2), times = nrow(data))
dm_x2 <- data$x2-mean_x2
Sx2 <- sum((dm_x2)^2)

mean_x3 <- rep(mean(data$x3), times = nrow(data))
dm_x3 <- data$x3-mean_x3
Sx3 <- sum((dm_x3)^2)

mean_x4 <- rep(mean(data$x4), times = nrow(data))
dm_x4 <- data$x4-mean_x4
Sx4 <- sum((dm_x4)^2)

pred_lower95 <- fitted.1 - 1.96*sqrt((rmse^2)*(1+1/nrow(data)+(((data$x1-mean(data$x1))^2)/Sx1))) 
    ### add (((data$x2-mean(data$x2))^2)/Sx2) etc
pred_upper95 <- fitted.1 + 1.96*sqrt((rmse^2)*(1+1/nrow(data)+(((data$x1-mean(data$x1))^2)/Sx1)))
    ### add (((data$x2-mean(data$x2))^2)/Sx2) etc
data <- tibble::add_column(data, pred_lower95, .after = 100) 
data <- tibble::add_column(data, pred_upper95, .after = 100) 

# OMITTING OUTLIERS ########################################
data_o <- data[!(abs(data$stu.resid.1)>=3 & data$norm_lev.1>=2), ]

# F-TEST ###################################################
reg1_f <- lm(data$y ~ data$x1 + data$x2 + data$x3 + data$x4)
test.var <-  c("data$x2", "data$x3")
car::linearHypothesis(reg1_f, test.var)

# CROSS VALIDATION #########################################

# Independent Variable CV
MSE.LOO1 <- boot::cv.glm(data, reg1)$delta[1] # Test that correct answer achieved for no trans
reg2 <- stats::glm(y ~ x1 + x2 + x4,
                   data = data)
MSE.LOO2 <- boot::cv.glm(data, reg2)$delta[1]

# Dependent Variable CV ###(See Crossvalidation .R file)

# BOX-COX Transformations ################################
box_cox <- boxcox(reg1, lambda = seq(-3,3))
lamda_bc <- box_cox$x[which.max(box_cox$y)]
roundlamda_bc <- round_any(lamda_bc, .5)



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
