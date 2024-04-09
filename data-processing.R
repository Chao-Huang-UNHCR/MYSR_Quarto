# pak::pkg_install("populationstatistics/popdata@fix-oauth")
library(tidyverse)
library(popdata)
library(readxl)
library(devtools)
library(usethis)

# No need to login again unless the PW changed
 pd_login()

MyRegion <- "Asia"
MyYear <- 2023

## import the reference data for funtions
reference <- read.csv("data-master/reference.csv")

source("functions/core.R")

asr2023<- ASR(MyYear)
mysr2023 <- MYSR(MyYear)

## for MYSR Data
Region_prep(MyRegion,MyYear,"End")

## for ASR Data 
Region_prep(MyRegion,MyYear-1,"End")


### PoC and FDP for all regions

region_poc_mid_2023 <- POC(ASR(MyYear),region_d,MyYear)

### PoC and FDP for all COA

load("data-raw/Asia_2023_clean_data.Rdata")
COA <- POC(Asia_2023_clean_data, asylumCountry, MyYear)
COO <- POC(Asia_2023_clean_data, originCountry, MyYear)

m2023ap <- POC(mysr2023 %>% filter(region_d == MyRegion) %>% 
                 arrange(asylum, origin),asylumCountry, MyYear)
missingCOA <- m2023ap %>%   filter(asylumCountry == "Australia")

COA_AP <- rbind(COA,missingCOA)
COA_AP$region = "Asia"
summary(COA_AP)

AP2023 <- POC(COA_AP,region, MyYear )

### PoC and FDP for all COO

COO <- POC(MYSR(MyYear), originCountry, MyYear)

### refugee population ranking

ranking_COA <- COA %>% mutate(Refugee_Mandate = Refugees + OIP) %>%
  select(Year,asylumCountry, Refugee_Mandate, Refugees, OIP) %>% 
  arrange(desc(Refugee_Mandate))


ranking_COO <- COO %>% mutate(Refugee_Mandate = Refugees + OIP) %>%
  select(Year,originCountry, Refugee_Mandate, Refugees, OIP) %>% 
  arrange(desc(Refugee_Mandate))

