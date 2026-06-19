################################################################################
# DIFFERENCE-IN-DIFFERENCES ANALYSIS (Callaway & Sant'Anna estimator)
#
# This script estimates staggered-adoption Difference-in-Differences (DiD)
# models using the `did` package (Callaway & Sant'Anna, 2021). It evaluates
# the effect of a treatment on a set of child-level outcomes and explores 
# heterogeneity by two channels: household wealth and returns to schooling.
#
# Structure:
#   0. Setup: packages and libraries
#   1. Main specification: ATT(g,t), simple ATT, and dynamic (event-study) ATT
#      for all outcome variables
#   2. Wealth channel: split sample by above/below median wealth
#   3. Returns-to-schooling channel: split sample by above/below median returns
#   4. Robustness: same main specification with an alternative set of controls
################################################################################


# ============================================================================
# 0. SETUP
# ============================================================================

# Install packages
install.packages("did")
install.packages("haven")
install.packages("tidyverse")

library(did)         # Callaway & Sant'Anna (2021) DiD estimator
library(tidyverse)   
library(haven)       
library(dplyr)     
library(ggplot2)


# ============================================================================
# 1. MAIN SPECIFICATION
#    Outcome variables: expenditure, school enrollment, and time-use measures
#    Controls: household wealth index (wi) and household size (hhsize)
# ============================================================================

# ---- 1.1 Load data ---------------------------------------------------------
dta <- read_dta("C:/Users/VICTOR/Desktop/IE/Bases_ie/muestra_FINAL.dta")

# Recode childid as a numeric factor so each child has a unique numeric ID
dta$childid <- as.numeric(as.factor(dta$childid))

# ---- 1.2 Estimate group-time ATTs for every outcome ------------------------
y_vars <- c("gasto_real", "estudia", "horas_school", "horas_study",
            "horas_chores", "horas_npaywork", "horas_paywork")

results <- list()  # will store one att_gt object per outcome

# Loop over outcomes and estimate att_gt() for each one.
# att_gt() computes group-time average treatment effects ATT(g,t):
#   yname        - outcome variable
#   tname        - time variable (survey round)
#   idname       - individual (child) identifier
#   gname        - variable indicating the period each unit was first treated
#   xformla      - covariates used for conditional parallel trends
#   clustervars  - variable to cluster standard errors on
#   allow_unbalanced_panel - allow individuals with missing periods
for (y in y_vars) {
  results[[y]] <- att_gt(
    yname = y,
    tname = "ronda",
    idname = "childid",
    gname = "G",
    data = dta,
    allow_unbalanced_panel = TRUE,
    clustervars = "clustid",
    xformla = ~ wi + hhsize
  )
}

## ---- 1.3 Group-time ATT results: summaries and plots ----------------------
# summary() prints the ATT(g,t) estimates; ggdid() plots them.
summary(results[["gasto_real"]])
ggdid(results[["gasto_real"]])

summary(results[["estudia"]])
ggdid(results[["estudia"]])

summary(results[["horas_school"]])
ggdid(results[["horas_school"]])

summary(results[["horas_study"]])
ggdid(results[["horas_study"]])

summary(results[["horas_chores"]])


## ---- 1.4 Overall ATT (simple aggregation) ---------------------------------
# type = "simple" collapses all ATT(g,t) into a single
# average treatment effect on the treated
att_generales <- list()

for (y in names(results)) {
  att_generales[[y]] <- aggte(results[[y]], type = "simple")
}

# Print the simple overall ATT for each outcome
att_generales[["gasto_real"]]
att_generales[["estudia"]]
att_generales[["horas_school"]]
att_generales[["horas_study"]]
att_generales[["horas_chores"]]
att_generales[["horas_npaywork"]]
att_generales[["horas_paywork"]]

# Full summary for every outcome at once
lapply(att_generales, summary)


## ---- 1.5 Dynamic ATT ----------------------------------------
# type = "dynamic" aggregates ATT(g,t) by event time
att_dinamicos <- list()

for (y in names(results)) {
  att_dinamicos[[y]] <- aggte(results[[y]], type = "dynamic")
}

# Print + plot the event-study ATT for each outcome
att_dinamicos[["gasto_real"]]
ggdid(att_dinamicos[["gasto_real"]])

att_dinamicos[["estudia"]]
ggdid(att_dinamicos[["estudia"]])

att_dinamicos[["horas_school"]]
ggdid(att_dinamicos[["horas_school"]])

att_dinamicos[["horas_study"]]
ggdid(att_dinamicos[["horas_study"]])

att_dinamicos[["horas_chores"]]
att_dinamicos[["horas_npaywork"]]
att_dinamicos[["horas_paywork"]]

# Full summary for all dynamic ATTs at once
lapply(att_dinamicos, summary)


# ============================================================================
# 2. WEALTH CHANNEL (HETEROGENEITY BY HOUSEHOLD WEALTH)
#    Question: does the effect on expenditure differ between richer and
#    poorer households (split at the median, "canal_riqueza_50")?
# ============================================================================

# ---- 2.1 Load data for this channel ----------------------------------------
dta <- read_dta("C:/Users/VICTOR/Desktop/IE/Bases_ie/muestra_FINAL2.dta")
dta$childid <- as.numeric(as.factor(dta$childid))

# ---- 2.2 Above-median wealth subsample (canal_riqueza_50 == 1) ------------
# Keep everyone except group 2
dta1 <- subset(dta, canal_riqueza_50 != 2)

y_vars <- c("gasto_real")
results1 <- list()

for (y in y_vars) {
  results1[[y]] <- att_gt(
    yname = y,
    tname = "ronda",
    idname = "childid",
    gname = "G",
    data = dta1,
    allow_unbalanced_panel = TRUE,
    clustervars = "clustid"
  )
}

att_generales1 <- list()
for (y in names(results1)) {
  att_generales1[[y]] <- aggte(results1[[y]], type = "simple")
}

# Overall ATT on expenditure for the above-median wealth group
att_generales1[["gasto_real"]]

# Pre-treatment mean of expenditure for this group, used as a baseline
# to express the ATT in percentage terms (e.g., ATT / pre_mean)
pre_mean <- dta %>%
  filter(G != 0) %>%                       # keep only eventually-treated units
  filter(canal_riqueza_50 == 1) %>%         # restrict to above-median wealth
  group_by(childid) %>%
  filter(ronda < G) %>%                     # keep only pre-treatment rounds
  summarise(mean_pre = mean(gasto_real, na.rm = TRUE)) %>%
  summarise(mean_pre_total = mean(mean_pre, na.rm = TRUE))

# ---- 2.3 Below-median wealth subsample (canal_riqueza_50 == 2) ------------
dta2 <- subset(dta, canal_riqueza_50 != 1)

y_vars <- c("gasto_real")
results2 <- list()

for (y in y_vars) {
  results2[[y]] <- att_gt(
    yname = y,
    tname = "ronda",
    idname = "childid",
    gname = "G",
    data = dta2,
    allow_unbalanced_panel = TRUE,
    clustervars = "clustid"
  )
}

att_generales2 <- list()
for (y in names(results2)) {
  att_generales2[[y]] <- aggte(results2[[y]], type = "simple")
}

# Overall ATT on expenditure for the below-median wealth group
att_generales2[["gasto_real"]]

# Pre-treatment mean of expenditure for the below-median wealth group
pre_mean2 <- dta %>%
  dplyr::filter(G != 0) %>%
  dplyr::filter(canal_riqueza_50 == 2) %>%
  dplyr::group_by(childid) %>%
  dplyr::filter(ronda < G) %>%
  dplyr::summarise(mean_pre = mean(gasto_real, na.rm = TRUE)) %>%
  dplyr::summarise(mean_pre_total = mean(mean_pre, na.rm = TRUE))


# ============================================================================
# 3. RETURNS-TO-SCHOOLING CHANNEL (HETEROGENEITY BY RETURNS)
#    Question: does the effect on schooling/time-use outcomes differ between
#    areas/households with higher vs. lower returns to schooling
#    (split at the median, "canal_retornos_75")?
# ============================================================================

# ---- 3.1 Load data for this channel ----------------------------------------
dta <- read_dta("C:/Users/VICTOR/Desktop/IE/Bases_ie/muestra_FINAL3.dta")
dta$childid <- as.numeric(as.factor(dta$childid))

# ---- 3.2 Above-median returns subsample (canal_retornos_50 == 1) ----------
dta1 <- subset(dta, canal_retornos_75 != 2)

y_vars <- c("estudia", "horas_chores", "horas_paywork", "horas_npaywork")
results1 <- list()

for (y in y_vars) {
  results1[[y]] <- att_gt(
    yname = y,
    tname = "ronda",
    idname = "childid",
    gname = "G",
    data = dta1,
    allow_unbalanced_panel = TRUE,
    clustervars = "clustid"
  )
}

att_generales1 <- list()
for (y in names(results1)) {
  att_generales1[[y]] <- aggte(results1[[y]], type = "simple")
}

# Overall ATTs for the above-median returns group
att_generales1[["estudia"]]
att_generales1[["horas_chores"]]
att_generales1[["horas_paywork"]]
att_generales1[["horas_npaywork"]]

# ---- 3.3 Below-median returns subsample (canal_retornos_75 == 2) ----------
dta2 <- subset(dta, canal_retornos_75 != 1)

y_vars <- c("estudia", "horas_chores", "horas_paywork", "horas_npaywork")
results2 <- list()

for (y in y_vars) {
  results2[[y]] <- att_gt(
    yname = y,
    tname = "ronda",
    idname = "childid",
    gname = "G",
    data = dta2,
    allow_unbalanced_panel = TRUE,
    clustervars = "clustid"
  )
}

att_generales2 <- list()
for (y in names(results2)) {
  att_generales2[[y]] <- aggte(results2[[y]], type = "simple")
}

# Overall ATTs for the below-median returns group
att_generales2[["estudia"]]
att_generales2[["horas_chores"]]
att_generales2[["horas_paywork"]]
att_generales2[["horas_npaywork"]]


# ============================================================================
# 4. ROBUSTNESS CHECK: ALTERNATIVE SET OF CONTROLS
#    Same main specification as Section 1, but replacing (wi, hhsize) with
#    (num_hermanos_men18, rural) as covariates for conditional parallel trends.
# ============================================================================

# ---- 4.1 Load data (same sample as the main specification) ----------------
dta <- read_dta("C:/Users/VICTOR/Desktop/IE/Bases_ie/muestra_FINAL.dta")
dta$childid <- as.numeric(as.factor(dta$childid))

# ---- 4.2 Estimate group-time ATTs with alternative controls ---------------
y_vars <- c("gasto_real", "estudia", "horas_school", "horas_study",
            "horas_chores", "horas_npaywork", "horas_paywork")
results <- list()

for (y in y_vars) {
  results[[y]] <- att_gt(
    yname = y,
    tname = "ronda",
    idname = "childid",
    gname = "G",
    data = dta,
    allow_unbalanced_panel = TRUE,
    clustervars = "clustid",
    xformla = ~ num_hermanos_men18 + rural
  )
}

## ---- 4.3 Overall ATT (simple aggregation) ---------------------------------
att_generales <- list()

for (y in names(results)) {
  att_generales[[y]] <- aggte(results[[y]], type = "simple")
}

# Overall ATT for each outcome under the alternative-controls specification
att_generales[["gasto_real"]]
att_generales[["estudia"]]
att_generales[["horas_school"]]
att_generales[["horas_study"]]
att_generales[["horas_chores"]]
att_generales[["horas_npaywork"]]
att_generales[["horas_paywork"]]

## ---- 4.4 Dynamic (event-study) ATT ----------------------------------------
att_dinamicos <- list()

for (y in names(results)) {
  att_dinamicos[[y]] <- aggte(results[[y]], type = "dynamic")
}

# Dynamic ATT for each outcome under the alternative-controls specification
att_dinamicos[["gasto_real"]]
att_dinamicos[["estudia"]]
att_dinamicos[["horas_school"]]
att_dinamicos[["horas_study"]]
att_dinamicos[["horas_chores"]]
att_dinamicos[["horas_npaywork"]]
att_dinamicos[["horas_paywork"]]