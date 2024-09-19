####################################
# Author: Isaac Shon
# Last Edited: 6/21/2024
# Reproduction of (2018)
####################################

# Install packages
#install.packages("rdd")
#install.packages("rdrobust")
#install.packages("dplyr")
#install.packages("stargazer")
#install.packages("vtable")

# Load packages
library(tidyverse)  # ggplot(), %>%, mutate(), etc.
library(dplyr)
library(rdrobust)  # For robust nonparametric regression discontinuity
library(rdd)
library(estimatr)  # Run 2SLS models in one step with iv_robust()
library(modelsummary)  # Create side-by-side regression tables
library(stargazer)
library(vtable)
library(haven) # Loading datasets in STATA and other formats

# Set Directory
setwd("C:/Users/isaac/OneDrive/Desktop/example_codes/RDD")

# Load Dataset
analysisdata <- read_dta("fuzzyrdd_data/analysis.dta")
View(analysisdata)

######################################################################################################################################################################

# Create summary table of variables:
summary_table <- st(analysisdata, vars = c("nb_registered_R1", "prop_registered_turnout_R1", "prop_registered_candvotes_R1",
                                           "prop_registered_blanknull_R1", "nb_candidates_R1", "prop_registered_turnout_R2",
                                           "prop_registered_candvotes_R2", "prop_registered_blanknull_R2", "nb_candidates_R2"), 
                    summ = c('mean(x)','median(x)','min(x)', 'max(x)', 'notNA(x)'), 
                    summ.names = c("Mean", "Median", "Min.", "Max.", "No. Obs."), 
                    labels = TRUE,
                    title = "Fuzzy RDD Summary Statistics", 
                    out = 'latex', file = 'writeup/fuzzysummary.tex')

# First-stage RD plot:
firststage <- rdplot(x = analysisdata$running, y = analysisdata$treatment, 
                c = 0, p = 1, nbins = c(20, 20),
                x.label = "Qualifying margin of the 3rd candidate in 1st round", y.label = "Treatment")
firststage <- firststage$rdplot
ggsave(firststage, filename = "writeup/fuzzyfirststage.pdf", device = "pdf")



# Compute IK optimal bandwidth from rdd package:
ikbw <- IKbandwidth(analysisdata$running, analysisdata$treatment, cutpoint = 0, kernel = "triangular")



# Reproduce First Stage Table
  # Column 1: Local Linear Regression w/ MSE-optimal bandwidth selector
  firststage_c1 <- rdrobust(x = analysisdata$running, y = analysisdata$treatment, c = 0, p = 1)
  
  # Column 2: Local Linear Regression w/ Imbens-Kalyanaraman Optimal Bandwidth
  firststage_c2 <- rdrobust(x = analysisdata$running, y = analysisdata$treatment, c = 0, p = 1, h = ikbw)
  
  # Column 3: Local Quadratic Regression w/ MSE-optimal bandwidth selector
  firststage_c3 <- rdrobust(x = analysisdata$running, y = analysisdata$treatment, c = 0, p = 2)
  
  # Column 4: Local Quadratic Regression w/ Imbens-Kalyanaraman Optimal Bandwidth
  firststage_c4 <- rdrobust(x = analysisdata$running, y = analysisdata$treatment, c = 0, p = 2, h = ikbw)
  
  # Create Table:
  tidy.rdrobust <- function(model, ...) {
    ret <- data.frame(
      term = row.names(model$coef),
      estimate = model$coef[, 1],
      std.error = model$se[, 1],
      p.value = model$pv[, 1]
    )
    row.names(ret) <- NULL
    ret
  }
  
  glance.rdrobust <- function(model, ...) {
    ret <- data.frame(
      Kernel = model$kernel,
      Bandwidth = model$bwselect,
      nobs.effective.left = model$N_h[1],
      nobs.effective.right = model$N_h[2]
    )
    ret
  }
  models <- list(firststage_c1, firststage_c2, firststage_c3, firststage_c4)
  modelsummary(models, statistic = "std.error", title = "First Stage Results", 
               stars = TRUE, output = "writeup/fuzzyfirststagetable.tex")
  
  
  
# Reproduce Table 3 from paper:
  # Impact on turnout, blank and null votes, and candidate votes
  
  c1 <- rdrobust(x = analysisdata$running, y = analysisdata$prop_registered_turnout_R2, c = 0, p = 1, fuzzy = analysisdata$treatment)
  c2 <- rdrobust(x = analysisdata$running, y = analysisdata$prop_registered_blanknull_R2, c = 0, p = 1, fuzzy = analysisdata$treatment)
  c3 <- rdrobust(x = analysisdata$running, y = analysisdata$prop_registered_candvotes_R2, c = 0, p = 1, fuzzy = analysisdata$treatment)
  fuzzy_table <- list("Turnout" = c1, "Blank and Null Votes" = c2, "Candidate Votes" = c3)
  modelsummary(fuzzy_table, statistic = "std.error", title = "Fuzzy RDD Results", 
               stars = TRUE,output = "writeup/fuzzytable.tex")

# Plot outcomes
fuzzyfigure1  <- rdplot(x = analysisdata$running, y = analysisdata$prop_registered_turnout_R2, 
            c = 0, p = 1, nbins = c(30, 30),
            x.label = "Running Variable", y.label = "Turnout in 2nd round")
fuzzyfigure1 <- fuzzyfigure1$rdplot
ggsave(fuzzyfigure1, filename = "writeup/fuzzyfigure1.pdf", device = "pdf")


fuzzyfigure2  <- rdplot(x = analysisdata$running, y = analysisdata$prop_registered_blanknull_R2, 
                        c = 0, p = 1, nbins = c(30, 30),
                        x.label = "Running Variable", y.label = "Blank and Null Votes")
fuzzyfigure2 <- fuzzyfigure2$rdplot
ggsave(fuzzyfigure2, filename = "writeup/fuzzyfigure2.pdf", device = "pdf")


fuzzyfigure3  <- rdplot(x = analysisdata$running, y = analysisdata$prop_registered_candvotes_R2, 
                        c = 0, p = 1, nbins = c(30, 30),
                        x.label = "Running Variable", y.label = "Candidate Votes")
fuzzyfigure3 <- fuzzyfigure3$rdplot
ggsave(fuzzyfigure3, filename = "writeup/fuzzyfigure3.pdf", device = "pdf")


# - END - #




