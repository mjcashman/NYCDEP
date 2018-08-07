library(tidyverse)
library(readxl)

rm(list=ls())

# Load raw target and source data ----
title <- "Raw Values"
mix.filename <- "Y:/Access Databases/Catskills/Fingerprinting/TargetSamples_NDisRL_useMe.csv"
target<-read.csv(mix.filename) %>%
  mutate(Type = "ISCO")
source.filename <- "Y:/Access Databases/Catskills/Fingerprinting/SourceSamples_noRoad.csv"
source<-read.csv(source.filename)
