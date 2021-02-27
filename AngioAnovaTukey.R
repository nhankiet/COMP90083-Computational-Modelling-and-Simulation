# Authors: 
# Nhan Kiet To (1043668),
# Adam Thalhammer (972267),
# Will Walters (583639)

##
# This R script performs statistical ANOVA and Tukey's HSD tests on the angiogenesis 
# categorical variables and their impact on the total number of malignant cells. 
##

# Install packages for descriptive plots and testing the normality assumptions of the dataset.
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
library(tidyverse)
library(ggpubr)
library(rstatix)

# Load the data and slice the data frame
timeseries_results = read.csv("TumourAngioSim-timeseries-table-2.csv", header = TRUE)
df_angio = timeseries_results[, c("count.malignant.cells", "angiogenesis.signal.radius", "angiogenesis.min.demand")]

# Fit the parameters of interest to a linear model and run an inital ANOVA
anova.lm = anova(lm(df_angio$count.malignant.cells~df_angio$angiogenesis.min.demand*df_angio$angiogenesis.signal.radius))
anova.lm

# Run an Analysis of Variance test on the dataset, with the categorical variables of interest. 
anova.aov = aov(df_angio$count.malignant.cells~as.factor(df_angio$angiogenesis.min.demand)*as.factor(df_angio$angiogenesis.signal.radius))
summary(anova.aov)

# Perform a Tukey's Honestly Significant Difference test on the categorical variables of interest
# This is important to determine with statistical significance whether altering these parameters of interest make s 
TukeyHSD(anova.aov)
