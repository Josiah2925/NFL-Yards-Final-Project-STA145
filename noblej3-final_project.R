###############################################################################
#                Final Project Analysis Script – STA 145                     
#                Noblej3_final_project.R                                   
###############################################################################

##############################
### 1. Set Working Directory
##############################
setwd("/courses/STA145/noblej3")

##############################
### 2. Load Libraries
##############################
library(readr)
library(dplyr)        # needed for filter(), %>%, etc.
library(psych)        # optional, for describe()

##############################
### 3. Load Data
##############################
data <- read_csv("data.csv")
View(data)

##############################
### 4. Complete-Case Dataset
##############################
data_complete <- data %>%
  filter(complete.cases(select(., Yards_per_play, rushing_yards)))

##############################
### 5. Descriptive Statistics
##############################

# Yards per play
mean(yards_per_play)
sd(yards_per_play)
summary(yards_per_play)

# Rushing yards
mean(rushing_yards)
sd(rushing_yards)
summary(data_complete$rushing_yards)

##############################
### 6. Boxplot (Figure 1)
##############################
# Create data frame
data <- data.frame(
  Yards_per_play = c(
    5.48,5.88,4.85,6.14,6.55,5.17,6.08,4.61,6.25,6.38,5.75,5.74,5.35,5.17,3.54,
    3.98,5.9,4.1,3.4,6.3,6.6,5.6,4.3,6.4,5.5,5.3,3.9,4.3,4.8,6.7,5.2,6.7,5.52,
    4.12,6.98,6.07,4.98,4.43,4.1,5.99,5.64,4.22,5.03,4.49,5.04,4.05,5.2,4.61,
    3.63,5.49,4.92,5.68,6.62,5.57,5.26,6.37,6.79,4.08,5.68,6.06,5.33,6.25,7.11,
    5.07
  ),
  rushing_yards = c(
    58,107,193,214,122,101,127,112,197,215,176,98,139,108,78,
    70,63,7,123,87,186,158,177,172,83,136,65,128,116,74,45,120,
    133,100,125,116,94,101,97,96,208,113,81,53,95,169,118,114,
    113,91,152,117,81,108,58,133,71,58,127,130,34,111,57,129
  )
)

# boxplot of both variables side by side
boxplot(data,
        main = "Boxplot of Yards per Play and Rushing Yards",
        col = c("skyblue", "lightgreen"),
        ylab = "Value",
        names = c("Yards per Play", "Rushing Yards"))
##############################
### 7. Scatter Plot (Figure 2)
##############################
plot(
  data$rushing_yards,
  data$yards_per_play,
  main = "Scatterplot of Rushing Yards vs. Yards per Play",
  xlab = "Rushing Yards",
  ylab = "Yards_per_play",
  pch = 19,
  col = "darkblue"
)

# Mean lines
abline(v = mean(data_complete$rushing_yards), col = "orange", lwd = 2)
abline(h = mean(data_complete$Yards_per_play), col = "black", lwd = 2)

##############################
### 8. Linear Regression
##############################
model <- lm(yards_per_play ~ rushing_yards, data = data_complete)
summary(model)

# Add regression line
abline(col = "red", lwd = 2)

##############################
### 9. Residual Plot (Figure 3)
##############################
plot(
  data_complete$rushing_yards,
  residuals(rushing_yards),
  main = "Residual Plot",
  xlab = "Rushing Yards",
  ylab = "Residuals",
  pch = 19
)

abline(h = 0, col = "darkblue", lwd = 2)
