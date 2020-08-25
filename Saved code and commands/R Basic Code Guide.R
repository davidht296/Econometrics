# File:    R Command Guide - R Studio
# Author:  David Harris, davidht296@gmail.com
# Date:    2020/03/25
# Video:   https://www.youtube.com/watch?v=_V8eKsto3Ug&t=444s

# CONTENTS #################################################
  #  ? - Handy shortcuts
  #  9 - Install and Load Packages
  # ?? - Import data files
  # 24 - Summarize Data
  # ?? - Visualise data
        # ?? - Plots
        # ?? - Bar Charts
        # ?? - Histograms
        # ?? - Overlaying charts
  # ?? - Selecting cases & conditional plots

# Handy Shortcuts ##########################################
  # Run line -> "Ctrl" + "Enter"
  # Quick type "gets" -> "Alt" + "-"


# INSTALL & LOAD PACKAGES ##################################

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# load packages.
pacman::p_load(pacman, boot, car, caret, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, huxtable, jtools, lars, 
               lmtest, lubridate, 
               MASS, olsrr, plotly, 
               psych, rio, rmarkdown, sandwich, shiny, stringr, 
               tidyverse, tidyr) 

library(datasets)  # Load built-in datasets

# package descriptions and help
p_help("boot", web = F)

# Import data files ########################################

# ABOUT EXCEL FILES ########################################
# From the official R documentation
browseURL("http://j.mp/2aFZUrJ")

# IMPORTING WITH RIO #######################################

rio_csv <- import("C:/Users/dtunks/OneDrive - KPMG/Desktop/R/R01_Course_Files/R01_5_4_ImportingData_Datasets/mbb.csv")
head(rio_csv)
View(rio_csv)

# Import data manually
# "Import Dataset" drop-down in the Environment window.
# see video https://www.youtube.com/watch?v=cnD1op2Oo3M

# SUMMARIZE DATA ###########################################

head(iris)         # Show the first six lines of iris data
summary(iris)      # Summary statistics for iris data
summary(iris$Species)       # Categorical variable
summary(iris$Sepal.Length)  # Quantitative variable
describe(iris)              # statictical summary for full data frame
describe(iris$Sepal.Length) # Gives statictical summary (mean, variance, min, max etc)

# PLOT DATA WITH PLOT() ####################################

?plot  # Help for plot()

plot(iris$Species)  # Categorical variable
plot(iris$Petal.Length)  # Quantitative variable
plot(iris$Species, iris$Petal.Width)  # Cat x quant
plot(iris$Petal.Length, iris$Petal.Width)  # Quant pair
plot(iris)   # Scatterplot matrix for iris data

# Plot with options
plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",  # Hex code for datalab.cc red
     cex = 1.5,        # Make 150% size
     pch = 19,         # Use solid circles for points
     main = "Iris: Petal Length vs. Petal Width",
     xlab = "Petal Length",
     ylab = "Petal Width")

# Bar Chart Plotting barplot() #############################

head(mtcars)

# Need a table with frequencies for each category
cylinders <- table(mtcars$cyl)  # Create table
barplot(cylinders)              # Bar chart
plot(cylinders)                 # Default X-Y plot (lines)

# BASIC HISTOGRAMS #########################################

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

# HISTOGRAM BY GROUP #######################################

# Set graphic paramaters
par(mfrow = c(3, 1))  # Put graphs in 3 rows and 1 column

# Histograms for each species using options
hist(iris$Petal.Width [iris$Species == "setosa"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Setosa",
     xlab = "",
     col = "red")

hist(iris$Petal.Width [iris$Species == "versicolor"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Versicolor",
     xlab = "",
     col = "purple")

hist(iris$Petal.Width [iris$Species == "virginica"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Virginica",
     xlab = "",
     col = "blue")

# Restore graphic parameter
par(mfrow=c(1, 1))


# Overlaying charts ################################################

# Default
hist(lynx)

# Add some options
hist(lynx,
     breaks = 14,          # "Suggests" 14 bins
     freq   = FALSE,       # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Annual Canadian Lynx",
                    "Trappings, 1821-1934"),
     xlab   = "Number of Lynx Trapped")

# Add a normal distribution
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph

# Add two kernel density estimators (non-parametric distribution curves)
lines(density(lynx), col = "blue", lwd = 2)
lines(density(lynx, adjust = 3), col = "purple", lwd = 2)  #adjust command uses moving average to smooth kernel density plot

# Add a rug plot (adds individual observation tally to the x-axis)
rug(lynx, lwd = 2, col = "gray")


# HISTOGRAM BY GROUP #######################################

# Set graphic paramaters
par(mfrow = c(3, 1))  # Put graphs in 3 rows and 1 column

# Histograms for each species using options
hist(iris$Petal.Width [iris$Species == "setosa"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Setosa",
     xlab = "",
     col = "red")

hist(iris$Petal.Width [iris$Species == "versicolor"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Versicolor",
     xlab = "",
     col = "purple")

hist(iris$Petal.Width [iris$Species == "virginica"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Virginica",
     xlab = "",
     col = "blue")

# Restore graphic parameter
par(mfrow=c(1, 1))


# SELECTING CASES ##########################################

# Select by category #######################################

# Versicolor
hist(iris$Petal.Length[iris$Species == "versicolor"],
     main = "Petal Length: Versicolor")

# Virginica
hist(iris$Petal.Length[iris$Species == "virginica"],
     main = "Petal Length: Virginica")

# Setosa
hist(iris$Petal.Length[iris$Species == "setosa"],
     main = "Petal Length: Setosa")

# SELECT BY VALUE ##########################################

# Short petals only (all Setosa)
hist(iris$Petal.Length[iris$Petal.Length < 2],
     main = "Petal Length < 2")

# MULTIPLE SELECTORS #######################################

# Short Virginica petals only
hist(iris$Petal.Length[iris$Species == "virginica" & 
                               iris$Petal.Length < 5.5],
     main = "Petal Length: Short Virginica")

# CREATE SUBSAMPLE #########################################

# Format: data[rows, columns]
# Leave rows or columns blank to select all
i.setosa <- iris[iris$Species == "setosa", ]

# EXPLORE SUBSAMPLE ########################################

head(i.setosa)
summary(i.setosa$Petal.Length)
hist(i.setosa$Petal.Length)


# Data Types & Structures ##################################

# DATA TYPES ###############################################

# Numeric

n1 <- 15  # Double precision by default
n1
typeof(n1)

n2 <- 1.5
n2
typeof(n2)

# Character - text and strings of text

c1 <- "c" # use "" for characters or strings of text
c1
typeof(c1)

c2 <- "a string of text"
c2
typeof(c2)

# Logical

l1 <- TRUE # TRUE or FALSE typed all caps or just T / F
l1
typeof(l1) # note no quotation to identify as a logical

l2 <- F
l2
typeof(l2) # note no quotation to identify as a logical

# DATA STRUCTURES ##########################################

# Vector # 1+ numbers in a 1 dimensional array
         # All same data type
         # R's basic data object

v1 <- c(1, 2, 3, 4, 5)
v1
is.vector(v1)

v2 <- c("a", "b", "c")
v2
is.vector(v2)

v3 <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
v3
is.vector(v3)

# Matrix # 2 dimensional data
         # Columns are the same length
         # Same data class
         # Columns not named, referred to by index numbers

m1 <- matrix(c(T, T, F, F, T, F), nrow = 2)
m1

m2 <- matrix(c("a", "b", 
               "c", "d"), nrow = 2, byrow = T) # use to enter data in the matrix form visually
m2

## Array ###################################################

# Give data, then dimemensions (rows, columns, tables)
a1 <- array(c( 1:24), c(4, 3, 2))
a1

# Frames # Can have vectors of multiple types
         # Vectors have same length
         # R's "Spreadsheet"
         # Special functionality for data frames

# Combine vectors of the same length, but different data types

vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)

df1 <- cbind(vNumeric, vCharacter, vLogical)
df1 # cbind() combines/coerces data to make a Matrix of least restrictive data type (character in this case)

# need to specifically combine as data frame using as.data.frame()
df2 <- as.data.frame(cbind(vNumeric, vCharacter, vLogical)) 
df2 # Makes a data frame with three different data types

#  Lists # Flexible, ordered collection of elements
         # Elements of any class, length, or structure
         # Can include other lists

o1 <- c(1, 2, 3)
o2 <- c("a", "b", "c", "d")
o3 <- c(T, F, T, T, F)

list1 <- list(o1, o2, o3)
list1

list2 <- list(o1, o2, o3, list1)  # Lists within lists!
list2

# "Coercion" is changing a data object from one type to another

# COERCING TYPES ###########################################

## Automatic coercion ######################################

# Goes to "least restrictive" data type

(coerce1 <- c(1, "b", TRUE))
# coerce1  # Parenthese around command above make this save and display
typeof(coerce1)

## Coerce numeric to integer ###############################

(coerce2 <- 5)
typeof(coerce2)

(coerce3 <- as.integer(5))
typeof(coerce3)

## Coerce character to numeric #############################

(coerce4 <- c("1", "2", "3"))
typeof(coerce4)

(coerce5 <- as.numeric(c("1", "2", "3")))
typeof(coerce5)

## Coerce matrix to data frame #############################

(coerce6 <- matrix(1:9, nrow= 3))
is.matrix(coerce6)

(coerce7 <- as.data.frame(matrix(1:9, nrow= 3)))
is.data.frame(coerce7)


# Factors ##################################################

# CREATE DATA

(x1 <- 1:3)
(y  <- 1:9)

# Combine variables
(df1 <- cbind.data.frame(x1, y)) # automatically repeats x1 to equate column lengths
typeof(df1$x1)
str(df1)

# AS.FACTOR ################################################

(x2  <- as.factor(c(1:3)))
(df2 <- cbind.data.frame(x2, y))
typeof(df2$x2)
str(df2)

# DEFINE EXISTING VARIABLE AS FACTOR #######################

x3  <- c(1:3)
df3 <- cbind.data.frame(x3, y)
(df3$x3 <- factor(df3$x3, levels = c(1, 2, 3))) #reclassify x3 as a factor
typeof(df3$x3)
str(df3)

# LABELS FOR FACTOR ########################################

x4  <- c(1:3)
df4 <- cbind.data.frame(x4, y)
df4$x4 <- factor(df4$x4,
                 levels = c(1, 2, 3),
                 labels = c("macOS", "Windows", "Linux"))
df4
typeof(df4$x4) # maintains factor as integer
str(df4) # shows factor names then their integer value

# ORDERED FACTORS AND LABELS ###############################

x5  <- c(1:3)
df5 <- cbind.data.frame(x5, y)
(df5$x5 <- ordered(df5$x5,
                   levels = c(3, 1, 2),
                   labels = c("No", "Maybe", "Yes")))
df5
typeof(df5$x5)
str(df5)


# Entering data ############################################

# COLON OPERATOR 
# Assigns number 0 through 10 to x1
x1 <- 0:10
x1
# Descending order
x2 <- 10:0
x2

# SEQ
?seq  # R help on seq
# Ascending values (duplicates 1:10)
(x3 <- seq(10))
# Specify change in values
(x4 <- seq(30, 0, by = -3)) # 30 to 0 by step-down 3s

# ENTER MULTIPLE VALUES WITH C
# c = concatenate (or combine or collect)
?c  # R help on c
x5 <- c(5, 4, 1, 6, 7, 2, 2, 3, 2, 8)
x5

# SCAN - easily enter data number by number
x6 <- scan()  # After running this command, go to console
# Hit return after each number
# Hit return twice to stop
x6

# REP
?rep  # R help on rep
x7 <- rep(TRUE, 5)
x7
# Repeats set
x8 <- rep(c(TRUE, FALSE), 5)
x8
# Repeats items in set
x9 <- rep(c(TRUE, FALSE), each = 5)
x9


# Regression Analysis ######################################

?USJudgeRatings
head(USJudgeRatings)
data <- USJudgeRatings # name dataset for easy coding

# Define variable groups for easy coding
x <- as.matrix(data[, -12]) # matrix of all rows and variables less the dependent variable
y <- data[, 12] # vector of dependent variable

# REGRESSION WITH SIMULTANEOUS ENTRY #######################

# Using variable groups
reg1 <- lm(y ~ x)

# Or specify variables individually
reg1 <- lm(RTEN ~ CONT + INTG + DMNR + DILG + CFMG +
             DECI + PREP + FAMI + ORAL + WRIT + PHYS,
           data = USJudgeRatings)

# Results
reg1           # Coefficients only
summary(reg1)  # Inferential tests

# MORE SUMMARIES & DIAGNOSTICS  ###########################################

anova(reg1)            # Coefficients w/inferential tests
coef(reg1)             # Coefficients (same as reg1)
confint(reg1)          # CI for coefficients

hist(residuals(reg1))  # Histogram of residuals
curve(dnorm(x, mean = mean(residuals(reg1)), sd = sd(residuals(reg1))),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph

qqnorm(residuals(reg1))        # QQ-Plot of residuals
ols_plot_resid_stud(reg1)      # studentized residual plot
ols_plot_resid_lev(reg1)       # studentized residual vs leverage plot
ols_plot_resid_stud_fit(reg1)  # studentized residual vs fitted plot

# ADDITIONAL REGRESSION MODELS ########################################

# Conventional stepwise regression
stepwise <- lars(x, y, type = "stepwise")

# Stagewise: Like stepwise but with better generalizability
forward <- lars(x, y, type = "forward.stagewise")

# Comparison of R^2 for new models
r2comp <- c(stepwise$R2[6], forward$R2[6]) %>% round(2)
names(r2comp) <- c("stepwise", "forward") 
r2comp  # Show values of R^2

# Hierarchical Clustering ##################################
head(mtcars)
cars <- mtcars[, c(1:4, 6:7, 9:11)]  # Select all rows and required column variables
head(cars)

# COMPUTE AND PLOT CLUSTERS
# Save hierarchical clustering to "hc." This codes uses pipes from dplyr.
hc <- cars   %>%  # Get cars data
  dist   %>%  # Compute distance/dissimilarity matrix
  hclust      # Computer hierarchical clusters

plot(hc)          # Plot dendrogram

# Principal Component Analysis #############################
  # See video (1:50:00)
head(mtcars)
cars <- mtcars[, c(1:4, 6:7, 9:11)]  # Select variables
head(cars)

# COMPUTE PCA ##############################################

# For entire data frame ####################################
pc <- prcomp(cars,
             center = TRUE,  # Centers means to 0 (optional)
             scale = TRUE)   # Sets unit variance (helpful)

# To specify variables #####################################

pc <- prcomp(~ mpg + cyl + disp + hp + wt + qsec + am +
               gear + carb, 
             data = mtcars, 
             center = TRUE,
             scale = TRUE)

# Get summary stats
summary(pc)

# Screeplot for number of components
plot(pc) # note that really only the first 2 components are significant

# Get standard deviations and rotation (correlation of PC components and the original variables)
pc

# See how cases load on PCs
predict(pc) %>% round(2)

# Biplot of first two components
biplot(pc)



# CLEAN UP #################################################

# Clear packages
p_unload(dplyr, tidyr, stringr) # Clear specific packages
p_unload(all)  # Easier: clears all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there is a plot

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")

# Clear mind :)
