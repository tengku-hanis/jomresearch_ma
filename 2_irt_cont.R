#========================================================================#
# Title: Hands-on MA of mean difference
# Data: IRT data
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Nov23, 2023
#========================================================================#

# SECTION 1: Basic meta-analysis ------------------------------------------

# Packages
library(meta)
library(dmetar)
library(dplyr)

# Data
irt_data <- read.csv("data/irt.csv") 

# Fixed and random effect model ----
ma_irt <- metacont(n.e = n.e, 
                   mean.e = mean.e,
                   sd.e = sd.e,
                   n.c = n.c, 
                   mean.c = mean.c,
                   sd.c = sd.c,
                   studlab = studyID,
                   data = irt_data,
                   method.tau = "REML", #estimator
                   sm = "MD", 
                   fixed = T, 
                   random = T,
                   prediction = T, 
                   hakn = T, #reduce false positive
                   adhoc.hakn = "iqwig6") #adjust the possible narrow ci caused by hakn
ma_irt

## Update chosen model
ma_irt_RE <- update(ma_irt, fixed = F)

# Forest plot ----
forest(ma_irt_RE, sortvar = TE, label.left = "Favour IRT", label.right = "Favour control")

# Funnel plot ----
funnel(ma_irt_RE, studlab = T)

# Publication bias ----
metabias(ma_irt_RE, plotit = T, method.bias = "Egger") #generic
metabias(ma_irt_RE, plotit = T, method.bias = "Begg") #generic
metabias(ma_irt_RE, plotit = T, method.bias = "Pustejovsky") #specific for cont outcome
# 2 out of 3 test are significant, p < 0.05
# So we conclude that our model has a publication bias

# Assess outlier (I^2 > 50%) ----
find.outliers(ma_irt_RE) #cannot have NAs for this

# Influential diagnostics (I^2 > 50%) ----
baujat(ma_irt_RE)

ma_inf <- InfluenceAnalysis(ma_irt_RE, random = T) #better

plot(ma_inf, "baujat")
plot(ma_inf, "ES")
plot(ma_inf, "I2")
plot(ma_inf, "influence") #a bit advanced


# SECTION 2: Publication bias ----------------------------------------------
# For significant publication bias 
# There is significant publication bias in our model
# I^2 is high in our model, by right this method is not appropriate

# Trim and fill method (I^2 should be low) ----
tf <- trimfill(ma_irt_RE)
tf

funnel(tf, studlab = T)


# SECTION 3: Heterogeneity ------------------------------------------------
# To explain high heterogeneity 

# Subgroup analysis (k > 10) ----
ma_sub <- update(ma_irt_RE, subgroup = age_gp)
ma_sub

forest(ma_sub, sortvar = TE, subgroup.name = "Age group", label.left = "Favour IRT", label.right = "Favour control")

# Meta-regression (~ k > 10) ----
ma_irt_reg <- metareg(ma_irt_RE, ~ age_gp, 
                      hakn = T, 
                      intercept = T) 

ma_irt_reg #age_gp is not significantly influence our effect size
# effect estimate/SMD for age group <65 is expected to rise by 0.1 compared to the >65 group
# effect estimate/SMD for age group <65 = -7.6980 + 1.9576
# effect estimate/SMD for age group >65 = -7.6980

## Bubble plot of meta-regression (specific for mean difference)
bubble(ma_irt_reg, lwd = 2, lty = 2, col.line = "red", regline = TRUE, 
       main = "Bubble plot of age group")
mtext(line = 0.25, font = 3, 
      "(The treatment is effective as the mean difference moves towards negative value)")


