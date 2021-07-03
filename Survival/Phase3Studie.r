#!/usr/bin/env Rscript

# Simulation of a phase 3 with one placebo group and n treatment groups


library(data.table)

library(survival)
library(survminer)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

#set.seed(42)

# Periods of treatment
# First period has no protection for all treatment groups 
# Second period the treatment groups are protected with 
# efficiency in vector protectionss

periods <- c(14,84)

# Names of the treatment groups

treatment_groups <- c(
  "None"
  , "Vaccine 1"
  , "Vaccine 2"
  , "Vaccine 3"
)

# Grade of protection in the treatment groups

protections <- c(0,0.85,0.9,0.95)

cohorts <- 1:length(treatment_groups)
cohort_size <- rep(1000,length(treatment_groups))
cohort_sum <- sum(cohort_size)

# Inzidenz per individual and day. 
# Propotion of people who get sick per 

Inzidenz <- 0.002

# Function to simulate one period

Simuliere <- function ( Individuals, r = 14, protected = FALSE) {
  
  for (i in 1:r) {

    if (protected) {
      
      survive <- (runif(cohort_sum, min = 0 , max = 1) > Individuals$p) | (runif(cohort_sum, min = 0 , max = 1) < Individuals$protection)
      
    }
    else
    {
      survive <- (runif(cohort_sum, min = 0 , max = 1) > Individuals$p)
      
    }

    Individuals$alive <- Individuals$alive & survive
    Individuals$n[Individuals$alive] <- Individuals$n[Individuals$alive] + 1
    
  }
  return (Individuals)
}

# Table of individuals and outcom of simulation

Individuals <- data.table(
  group = rep(cohorts,cohort_size)
  , protection = rep(protections,cohort_size)
  , status = rep(2,cohort_sum)
  , p = 1/rnorm(cohort_sum,1/Inzidenz,50)
  , n = 1 
  , alive = rep(TRUE,cohort_sum)
  )

Individuals <- Simuliere(Individuals,r=periods[1],protected=FALSE)

Individuals <- Simuliere(Individuals,r=periods[2],protected=TRUE)

Individuals$status[Individuals$n > sum(periods)] <- 1

fit <- survfit(Surv(n, status) ~ group, data = Individuals)

surfp <- ggsurvplot(
    fit
  , data = Individuals
  , main = "Simulation Phase 3 Studie"
  , submain = "Surviver based on Kaplan-Meier estimates"
  , caption = "created with survminer by Thomas Arend"
  , pval = TRUE
  , conf.int = TRUE
  , risk.table = TRUE
  , risk.table.col = "strata"
  , break.time.by = 14
  , ncensor.plot = FALSE
  , linetype = "strata"
  , ggtheme = theme_survminer(
    legend = "top"
  )
  , plot.margin = margin(4, 4, 4, 4, "cm")
  , palette = rainbow(length(treatment_groups))
  , legend.labs = treatment_groups
  #, ylim = c(0,0.025)
  , fun = "event"
)

ggsave( plot = print(surfp), file = paste('Phase3-',periods[2],'.png', sep="")
        , type = "cairo-png"
        , bg = "white"
        , width = 29.7, height = 21, units = "cm", dpi = 150)

Risk <- table(Individuals[,c(1,6)])
print (Risk[,1]- Risk[1,1])
print (round(100-Risk[,1]/Risk[1,1]*100,1))
