
#   Ephyrae shrinkage

# Author: Daniel Ottmann
# Created: July 2020
# Last update: April 2021


###########################################
#       Readme

# This script evaluates the shrinkage of ephyrae after formaline 4% preservation
# Samples were colected in June/July 2019 in the TUNIBAL survey around the Balearic Islands
# Central Disc Diamter of ephryae were measured inmediatelly after sampling and 4-6?? months after preservation


#######################################################################################################################


##################################################################
# Clear environment:
rm(list = ls())

#############################
# Load packages:
library(tidyverse)
library(cowplot)


#############################################################
# Load the data
load("data/data_shrinkage.RData")

#########################
# Edit data frame:
df <- data %>%
  mutate(cdd_shrinkage = cdd_preserved - cdd_fresh,
         shrinkage_ratio = -100 * (cdd_shrinkage / cdd_fresh)) %>%
  filter(cdd_shrinkage < 0)


########################################################
# Get equation of shrinkage as a function of live size:
m0 <- lm(cdd_preserved ~ cdd_fresh, data = df)

summary(m0)  # CDD_preserved = 0.05 + 0.72 * CDD_live  ;  R^2 = 0.92


# Plot CDD live vs preserved:
p <- ggplot(data = df) +
  geom_abline(intercept = 0, slope = 1, color = "gray", alpha = .6) +
  geom_point(aes(x = cdd_fresh, y = cdd_preserved, color = stage), alpha = .5) +
  labs(color = "Stage") +
  geom_smooth(aes(x = cdd_fresh, y = cdd_preserved), method = "lm", color = "black", se = F, size = .5) +
  ylab("Preserved CDD (mm)") +
  xlab("Live CDD (mm)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(.17, .7))

# png(filename = "plots/shrinkage.png", width = 7, height = 7, units = "cm", res = 300)
p
dev.off()


######################################################
# Test differences of % shrinkage across sizes:
m0 <- lm(shrinkage_ratio ~ cdd_fresh, data = df)
m1 <- lm(shrinkage_ratio ~ 1, data = df)

anova(m0, m1) # Not signifficant

# What is the average schrincage?
mean(df$shrinkage_ratio)  # 24.6 %


######################
# Model diagnostics

# Residuals vs fitted values:
df$residuals <- resid(m0, type = "pearson")
df$fits <- fitted(m0)

# Plot it out:
p1 <- ggplot() +
  geom_point(data = df, aes(x = fits, y = residuals)) +
  ylab("Residuals") +
  xlab("Fitted values") +
  theme_bw() +
  theme(panel.grid = element_blank())


# Normal QQ-plot:
p2 <- ggplot(data = df, aes(sample = residuals)) +
  stat_qq() + stat_qq_line() +
  theme_bw() +
  theme(panel.grid = element_blank())


# png(filename = "plots/shrinkage_diagnostics.png", width = 14, height = 7, units = "cm", res = 300)
plot_grid(p1, p2,labels = "auto")
dev.off()


###########################################
# Plot % shrinkage across size and stages:
p <- ggplot(data = df) +
  geom_point(aes(x = cdd_fresh, y = shrinkage_ratio, color = stage), alpha = .5) +
  labs(color = "Stage") +
  geom_smooth(aes(x = cdd_fresh, y = shrinkage_ratio), method = "lm", color = "black", size = .5, alpha = .3) +
  ylab("% shrinkage") +
  xlab("Live CDD (mm)") +
  theme_bw() +
  theme(panel.grid = element_blank())

# png(filename = "plots/shrinkage_ratio.png", width = 9, height = 6, units = "cm", res = 300)
p
dev.off()


#                              END OF SCRIPT
##########################################################################