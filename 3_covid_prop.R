#========================================================================#
# Title: Hands-on MA of proportion 
# Data: COVID19 data
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Nov23, 2023
#========================================================================#

# SECTION 1: Basic meta-analysis ------------------------------------------

# Packages
library(meta)
library(dmetar)
library(dplyr)

# Data
covid_data <- read.csv("data/covid.csv") 

# Fixed and random effect model ----
ma_covid <- metaprop(event = cases, 
                     n = total,
                     studlab = authoryear,
                     data = covid_data,
                     method = "GLMM", #estimator
                     sm = "PLOGIT", #logit transformation
                     fixed = T, 
                     random = T,
                     prediction = T, 
                     hakn = T) #reduce false positive
ma_covid

## Update chosen model
ma_covid_RE <- update(ma_covid, fixed = F)

# Forest plot ----
forest(ma_covid_RE, sortvar = TE)

# Funnel plot ----
funnel(ma_covid_RE, studlab = T, xlim = c(-5.5, 2.5))

# Publication bias ----
metabias(ma_covid_RE, plotit = T, method.bias = "Egger") #generic
metabias(ma_covid_RE, plotit = T, method.bias = "Begg") #generic

# Assess outlier (I^2 > 50%) ----
find.outliers(ma_covid_RE) #cannot have NAs for this

# Influential diagnostics (I^2 > 50%) ----
baujat(ma_covid_RE)

ma_inf <- InfluenceAnalysis(ma_covid_RE, random = T) #better

plot(ma_inf, "baujat")
plot(ma_inf, "ES")
plot(ma_inf, "I2")
plot(ma_inf, "influence") #a bit advanced


# SECTION 2: Publication bias ----------------------------------------------
# For significant publication bias (our model not significant)
# I^2 high in our data

# Trim and fill method (I^2 should be low) ----
tf <- trimfill(ma_covid_RE)
tf

funnel(tf, studlab = T)


# SECTION 3: Heterogeneity ------------------------------------------------
# To explain high heterogeneity 

# Subgroup analysis (k > 10) ----
ma_sub <- update(ma_covid_RE, subgroup = group)
ma_sub

forest(ma_sub, sortvar = TE, bylab = "Group")

# Meta-regression (~ k > 10) ----
ma_covid_reg <- metareg(ma_covid_RE, ~ group, 
                        hakn = T, 
                        intercept = T) 

ma_covid_reg #effect estimate of age group >65 is expected to reduce by 0.1 compared to the <65 group




