# File:    [INSERT]
# Author:  David Harris 
# Date:    [INSERT]

# INSTALL AND LOAD PACKAGES ################################

library(datasets)  # Load base packages manually

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, boot, car, caret, dplyr, GGally, ggplot2, 
               ggthemes, ggvis, httr, huxtable, jtools, lars, 
               lmtest, lubridate, 
               MASS, olsrr, plotly, plyr, 
               psych, rio, rmarkdown, sandwich, shiny, stringr, sur, 
               tidyverse, tidyr) 

# package descriptions and help
p_help("MASS", web = F)

# LOAD & LABEL DATA ################################################
?mtcars
head(mtcars)
data <- mtcars               # name dataset for easy coding

# WORK WITH DATA ###########################################
reg1 <- lm(mpg ~ cyl + disp + hp,
           data = mtcars)

# Core Regression Outputs #######################################
summ(reg1)                                # regression output
anova(reg1)                               # Analysis of Variance Table
summ(reg1, confint = TRUE, ci.width = .5) # Regression output w/ confidence intervals
summ(reg1, robust = "HC1")                # Regression output w/ robust SEs

# Other Regression Outputs ########################################
coef(reg1)                                # display coefficients only
confint(reg1)                             # CI for coefficients

# Assign Regression Outputs #######################################
fitted.1 <- fitted(reg1)                    # assign fitted values
resid.1 <- resid(reg1)                      # assign residuals
std.resid.1 <- stdres(reg1)                 # assign standardised residuals
stu.resid.1 <- studres(reg1)                # assign studentised residuals
leverage.1 <- leverage(reg1)                # assign leverage
data2008_omNA <- na.omit(data)              # removes observations with NA data2008 values
dim <- dim(data2008_omNA)
norm_lev.1 <- (dim[1]*leverage.1/2)           # assigns normalised leverage

# Diagnostic plots ################################################
par(mfrow = c(2, 2))                      # Put graphs in 2 rows and 2 column
plot(fitted.1, data2008_omNA$data2008, main = "Actual vs Fitted")
plot(fitted.1, stu.resid.1, main = "Studentised Residuals vs Fitted")
plot(norm_lev.1, stu.resid.1, main = "Studentised Residuals vs Normalised Leverage")
    abline(v=2, col="blue")
    abline(h=3, col="red")
    abline(v=-3, col="red")
qqnorm(stu.resid.1, , main = "Normal quantile plot of Studentised Residuals",
       xlab = "Inverse Normal", ylab = "Studentised Residuals",
       plot.it = TRUE, datax = FALSE,)
    qqline(stu.resid , col="red")
par(mfrow=c(1, 1))                        # Restore graphic parameter

# Select Transformation of Dependent Variable ####################
box_cox <- boxcox(reg1, lambda = seq(-3,3))
lamda_bc <- box_cox$x[which(box_cox$y==max(box_cox$y))]
roundlamda_bc <- round_any(lamda_bc, .5)

# REPORT #########################################################
export_summs(reg1, reg2, scale = TRUE)    # Significance table for report





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
