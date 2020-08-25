# File:    [INSERT]
# Author:  David Harris 
# Date:    [INSERT]

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
p_help("olsrr", web = F)

# Set Working Directory ####################################
getwd()
setwd("[INSERT ADDRESS*]") #*remember to change \ to /

# LOAD DATA ################################################
data <- import("[INSERT]")
head(data)  # view top few lines of data

#or
# Import data manually
# "Import Dataset" drop-down in the Environment window.
# see video https://www.youtube.com/watch?v=cnD1op2Oo3M


# WORK WITH DATA ###########################################



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
