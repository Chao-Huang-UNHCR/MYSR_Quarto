# pak::pkg_install("populationstatistics/popdata@fix-oauth")
library(tidyverse)
library(popdata)
library(readxl)
library(devtools)
library(usethis)

# No need to login again unless the PW changed
pd_login()

## import the reference data for funtions
reference <- read.csv("data-master/reference.csv")

source("functions/core.R")

asr2022<- ASR(2022)
mysr2023 <- MYSR(2023)

## column deduct is for the displaced stateless people, who have been double counted and should be deduplicated from the total PoC

### PoC and FDP for all regions

region_poc_mid_2023 <- POC(mysr2023,region_d,2023)

### PoC and FDP for all COA

COA <- POC(mysr2023, asylumCountry, 2023)

### PoC and FDP for all COO

COO <- POC(mysr2023, originCountry, 2023)

### refugee population ranking

ranking_COA <- COA %>% mutate(Refugee_Mandate = Refugees + OIP) %>%
  select(Year,asylumCountry, Refugee_Mandate, Refugees, OIP) %>% 
  arrange(desc(Refugee_Mandate))


ranking_COO <- COO %>% mutate(Refugee_Mandate = Refugees + OIP) %>%
  select(Year,originCountry, Refugee_Mandate, Refugees, OIP) %>% 
  arrange(desc(Refugee_Mandate))

### Get the clean data for your region

Region_prep("Asia",2023,"Mid")
Region_prep("Europe",2022,"End")
load("data-master/Asia_2023_clean_data.Rdata")
load("data-master/Asia_2022_clean_data.Rdata")

Global_prep(2023,"Mid")
load("data-master/global_2023_clean_data.Rdata")

agg_asylum <- POC(Asia_2023_clean_data, asylumCountry, 2023)
agg_origin <- POC(Asia_2023_clean_data, originCountry, 2023)

agg_asylum <- POC(Europe_2022_clean_data, asylumCountry, 2022)
sum(agg_asylum$POC_Total)
