# Author: Isaac Shon
# Date: 9/12/2024
# Topic: Simulating Staggered DID Estimation
#-----------------------------------------------------------#

# First, install packages
# install.packages("tidyverse")
# devtools::install_github("bcallaway11/did")
# install.packages ("bacondecomp")
# install.packages("staggered")

# Set Directory
setwd("C:/Users/isaac/OneDrive/Desktop/example_codes/DID")

# Load in packages
library(fixest)
library(bacondecomp)
library(did)
library(staggered)
library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)
library(panelView)
library(vtable)
library(modelsummary)
library(tidyr)
#-----------------------------------------------------------#
# Let us load in our simulated data:
df <- read_dta("data/staggered_did_simulated.dta")
View(df)

# Table: Aggregate Summary Statistics by Treatment Status
summary_table <- st(df, vars = c("y_it", "alpha_i", "e_it"), group = "D_it", group.long = TRUE,
                    summ = c('mean(x)','median(x)','min(x)', 'max(x)', 'notNA(x)'), 
                    summ.names = c("Mean", "Median", "Min.", "Max.", "No. Obs."), 
                    labels = TRUE,
                    title = "Aggregate Summary Statistics by Treatment Status", 
                    out = 'latex', file = 'writeup/Table2.tex',
                    fit.page = NA)

# Plot treatment distribution over time:
treatment_dist <- panelView::panelview(
  y_it ~ D_it,
  data = df,
  index = c("i", "t"),
  xlab = "Year",
  ylab = "Unit",
  display.all = F,
  gridOff = T,
  by.timing = T
) + theme_bw() + theme(legend.position="bottom", aspect.ratio=.5/1)
ggsave(treatment_dist, filename = "writeup/Figure2a.pdf", device = "pdf")

# Plot time trend by cohort:
timetrends<-ggplot(aes(x = t, y = y_it, color = as.factor(g)), data = df) +
  stat_summary(fun.y = "mean", linewidth = 1, geom= "line") +
  xlim(2000,2009)+
  geom_vline(xintercept = 2003, linetype = "longdash", color = "gold") +
  geom_vline(xintercept = 2004, linetype = "longdash", color = "green") +
  geom_vline(xintercept = 2005, linetype = "longdash", color = "blue") +
  geom_vline(xintercept = 2006, linetype = "longdash", color = "magenta")+
  guides(color = guide_legend(title = "Treatment Cohort (g)")) + 
  theme_bw() +
  theme(legend.position="bottom", aspect.ratio=.5/1)
timetrends
ggsave(timetrends, filename = "writeup/Figure2b.pdf", device = "pdf")

#--------------------Estimation---------------------------------------#

# Traditional TWFE
feols(y_it ~ D_it | i + t, df)

# Plot TWFE weights
(bdecomp = bacon(y_it ~ D_it, df, id_var = "i", time_var = "t"))
(bdecomp_wm = weighted.mean(bdecomp$estimate, bdecomp$weight))

decomposition <- ggplot(bdecomp, aes(x = weight, y = estimate, shape = type, col = type)) +
  geom_hline(yintercept = bdecomp_wm, lty  = 2) +
  geom_point(size = 3) +
  labs(x = "Weight", y = "Estimate", shape = "Comparison Type", col = "Comparison Type") +
  theme(aspect.ratio=0.5/1) +
  theme_bw()
decomposition
ggsave(decomposition, filename = "writeup/Figure3.pdf", device = "pdf")

# Computing ATT(g,t)'s:
attgt <- att_gt(yname = "y_it",
                        tname = "t",
                        idname = "i",
                        gname = "g",
                        xformla = NULL,
                        data = df)
tidy(attgt)
glance(attgt)
# ATT(g,t) plots with CI bands: 
coefplot <- ggdid(attgt, ylim = c(-5, 35), ncol = 2, 
                xlab = "Time", ylab = "ATT(g,t) Estimate") 
  
coefplot
ggsave(coefplot, filename = "writeup/Figure4.pdf", device = "pdf")

# summarize the results into tables, different aggregation schemes:

# Simple Aggregation:
simple_agg <- aggte(attgt, type = "simple")
simple_agg

# Group-Based Aggregation:
group_agg <- aggte(attgt, type = "group")
tidy(group_agg)
glance(group_agg)

# Calendar-Based Aggregation:
cal_agg <- aggte(attgt, type = "calendar")
tidy(cal_agg)
glance(cal_agg)

options("modelsummary_format_numeric_latex" = "plain")
models <- list(group_agg, cal_agg)
modelsummary(models,
             statistic = "conf.int",
             shape = term + statistic ~ type,
             title = "Aggregated ATT(g,t) Estimates",
             output = "writeup/Table3.tex")

