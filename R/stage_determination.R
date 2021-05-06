
#   Ephyrae shrincage

# Author: Daniel Ottmann
# Created: April 2021
# Last update: April 2021


###########################################
#       Readme

# This script evaluates size differences of different ephyrae stages 
# Samples were colected in June/July 2014-2016 in the Bluefint/TUNIBAL surveis around the Balearic Islands
# Central Disc Diamter of ephryae were measured after preservation in formalin

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
data <- read.delim('data/data_stage_determination.txt', sep = '\t', header = T, stringsAsFactors = F, dec = ".")
load("data/data_stage_determination.RData")


###############################################
# Edit data
df <- data  %>%
  mutate(stage = as.factor(stage),
         year = as.factor(year))
         

########################################
# Run a two-way ANOVA on original data:
m0 <- lm(size_mm ~ stage * year,
         data = df)

res_aov_m0 <- aov(m0)
summary(res_aov_m0)


########################
# Check model diagnostics:

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

# Plot residuals vs parameters:
# Year:
p2 <- ggplot() +
  geom_point(data = df, aes(x = year, y = residuals)) +
  ylab("Residuals") +
  xlab("Year") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Stage:
p3 <- ggplot() +
  geom_point(data = df, aes(x = stage, y = residuals)) +
  ylab("Stage") +
  xlab("Fitted values") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Normal QQ-plot:
p4 <- ggplot(data = df, aes(sample = residuals)) +
  stat_qq() + stat_qq_line() +
  theme_bw() +
  theme(panel.grid = element_blank())

# png(filename = "plots/stage_diagnostics.png", width = 14, height = 14, units = "cm", res = 300)
plot_grid(p1, p2, p3, p4, labels = "auto")
dev.off()


# There is heterogeneity at different stages: older stages have grater residuals


##########################################################################
# Lets see if log-transofrming the size gets rid of this problem:
df <- df %>%
  mutate(log_size = log10(size_mm))

# Run the model:
m0 <- lm(log_size ~ stage * year,
         data = df)

res_aov_m0 <- aov(m0)
summary(res_aov_m0) # Size-at-stage is equally signifficant as when the data was not log-transofrmed


##########################
# Check model diagnistics:

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

# Plot residuals vs parameters:
# Year:
p2 <- ggplot() +
  geom_point(data = df, aes(x = year, y = residuals)) +
  ylab("Residuals") +
  xlab("Year") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Stage:
p3 <- ggplot() +
  geom_point(data = df, aes(x = stage, y = residuals)) +
  ylab("Stage") +
  xlab("Fitted values") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Normal QQ-plot:
p4 <- ggplot(data = df, aes(sample = residuals)) +
  stat_qq() + stat_qq_line() +
  theme_bw() +
  theme(panel.grid = element_blank())

# png(filename = "plots/log_stage_diagnostics.png", width = 14, height = 14, units = "cm", res = 300)
plot_grid(p1, p2, p3, p4, labels = "auto")
dev.off()

# Now it looks good


###############################
# Plot original data as boxplot:
p <- ggplot() +
  geom_boxplot(data = df, aes(x = reorder(stage, -size_mm), y = size_mm, fill = year), size = .5, outlier.size = 1) +
  ylab("Central disc diameter (mm)") +
  xlab("Stage") +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .75),
        panel.grid = element_blank())

# png(filename = "plots/stage_boxplot.png", width = 7, height = 7, units = "cm", res = 300)
p
dev.off()

# As violin plots:
p <- ggplot() +
  geom_violin(data = df, aes(x = reorder(stage, -size_mm), y = size_mm, fill = year), size = .5) +
  ylab("Central disc diameter (mm)") +
  xlab("Stage") +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .75),
        panel.grid = element_blank())

# png(filename = "plots/violin.png", width = 7, height = 7, units = "cm", res = 300)
p
dev.off()


########################################
# Plot log-transformed data as boxplot:
p <- ggplot() +
  geom_boxplot(data = df, aes(x = reorder(stage, -log_size), y = log_size, fill = year), size = .5, outlier.size = 1) +
  ylab("Log (central disc diameter (mm))") +
  xlab("Stage") +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .75),
        panel.grid = element_blank())

# png(filename = "plots/log_boxplot.png", width = 7, height = 7, units = "cm", res = 300)
p
dev.off()


#                              END OF SCRIPT
##########################################################################