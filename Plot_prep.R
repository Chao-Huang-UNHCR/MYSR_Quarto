#### the scripts here will generate all the datasets ready for plotting and save all the files into the folder data-master/plotdata. The datasets were named as Plot1 to Plot15 with short descriptions. While loading all the plotdata into the environment, you can use 'Plot1' ... 'Plot15' to refer to those datasets.

#### The key variables (MyRegion, MyYear, plot_title_region) can be changed for different purpose, and the datasets, plots and narratives will be refreshed automatically.

#### some specials for Asia-Pacific region when aggregating the data for COO or COA: HKG and MAC special districts for China. If you region have similar cases, please change the codes accordingly in Plot 3, 4, 5, 6, 7.

#### the plot for specific situations (Afghanistan, Myanmar) will not apply for other regions


library(refugees)
library(ggplot2)
library(unhcrthemes)
library(scales)
library(popdata)
library(tidyverse)
library(readxl)

###### Plot 1 --- 10 years trends for the POC ####

# (1) get data historical data from refugee package
library(tidyverse)
library(refugees)
library(readxl)
library(unhcrthemes)
library(scales)
source("functions/core.R")

### define the region and year you want to save the file
MyRegion <- "Asia"
MyYear <- 2023
plot_title_region <- "Asia Pacific"

regionDatYear <-  paste0(MyRegion,"_",MyYear ,"_clean_data")
regionDatYear_last <- paste0(MyRegion,"_",MyYear-1,"_clean_data")

dat <- refugees::population %>% filter(year > (MyYear - 10) & year < MyYear)
reference <- read.csv("data-master/reference.csv")

reference <- reference %>% 
  select(iso_3, UNHCRcode, ctryname, UNHCRBureau)

dat_iso <- merge(dat,reference, by.x = "coa", by.y = "UNHCRcode", all.x = T) %>%
  select(-iso_3, -ctryname)

dat_region <- dat_iso %>%
  filter(UNHCRBureau == MyRegion) %>% 
  group_by(year,UNHCRBureau)%>% 
  summarise(Refugees = sum(refugees), 
            asylum_seekers = sum(asylum_seekers), 
            IDPs = sum(idps), 
            "Returnees (Refugee)" = sum(returned_refugees), 
            "Returnees (IDP)" = sum(returned_idps), 
            "Others of Concern" = sum(ooc))

# (2) load the non displaced stateless data manually

ndp_sta_decade <- read_excel("data-master/ndp_sta_decade.xlsx")

dat_region_decade <- dat_region %>% 
  merge(ndp_sta_decade, by = "year", all.x=T) %>% 
  mutate("Refugees, Refugee-like and Asylum-Seekers" = Refugees + asylum_seekers) %>%
  select(year,"Refugees, Refugee-like and Asylum-Seekers", IDPs, "Non-Displaced Stateless", "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern")

# (3) merge the RDF data and popdata data

load(paste0("data-raw/",MyRegion,"_",MyYear,"_clean_data.Rdata"))

mysr2023_myregion <- POC(get(regionDatYear),region_d,MyYear) %>% 
  mutate("Refugees, Refugee-like and Asylum-Seekers"= Refugees + Asylum_Seekers,
         "Non-Displaced Stateless" = Stateless_Non_displaced,
         "Returnees (Refugee)" = Returnee_Refugee,
         "Returnees (IDP)" = Returnee_IDP,
         "Others of Concern" = OOC) %>%
  select("Refugees, Refugee-like and Asylum-Seekers", IDPs, "Non-Displaced Stateless", "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern") %>% 
  mutate(year = paste0("mid_",MyYear))

poc_2014_my2023_myregion <- rbind(dat_region_decade, mysr2023_myregion)

poc_2014_my2023_myregion_long <- poc_2014_my2023_myregion %>% 
  gather(key = "pop_type", value = "pop_figure", 2:7) %>% 
  mutate(pop_mil = pop_figure / 1000000,
         pop_type = factor(pop_type, levels = c("Others of Concern","Returnees (IDP)","Returnees (Refugee)","Non-Displaced Stateless","IDPs","Refugees, Refugee-like and Asylum-Seekers")))

sums <- poc_2014_my2023_myregion_long %>%
  group_by(year) %>%
  summarise(total_poc = sum(pop_mil))  

# (4) save the data into data-master/Plotdata for plotting

file_path <- file.path("data-master/Plotdata\\", paste0("Plot1_10year_trend.Rdata"))
assign("Plot1",poc_2014_my2023_myregion_long)
save(Plot1, file = file_path)

file_path <- file.path("data-master/Plotdata\\", paste0("Plot1_label_10year_trend.Rdata"))
assign("Plot1_label",sums)
save(Plot1_label, file = file_path)



###### Plot 2 --- regional population compared to last year, by populaiton type ####

# (1) get data for the current year and last year
Region_prep(MyRegion,MyYear-1,"End")
load(paste0("data-raw/",MyRegion,"_",MyYear-1,"_clean_data.Rdata"))

dat_lastyear <- POC(get(regionDatYear_last),region_d,MyYear-1) %>% 
  mutate("Refugees and Refugee-like" = Refugees,
        "Asylum-Seekers"= Asylum_Seekers,
         Stateless = Stateless_Total,
         "Returnees (Refugee)" = Returnee_Refugee,
         "Returnees (IDP)" = Returnee_IDP,
         "Others of Concern" = OOC) %>%
  select(Year, "Refugees and Refugee-like","Asylum-Seekers", IDPs, Stateless, "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern")

dat_year <- POC(get(regionDatYear),region_d,MyYear) %>% 
  mutate("Refugees and Refugee-like" = Refugees,
         "Asylum-Seekers"= Asylum_Seekers,
         Stateless = Stateless_Total,
         "Returnees (Refugee)" = Returnee_Refugee,
         "Returnees (IDP)" = Returnee_IDP,
         "Others of Concern" = OOC,
         Year = "mid_2023") %>%
  select(Year, "Refugees and Refugee-like","Asylum-Seekers", IDPs, Stateless, "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern")

dat_twoyears <- rbind(dat_lastyear, dat_year)
dat_twoyears_long <- dat_twoyears %>% 
  gather(key = "pop_type", value = "pop_figure", 2:8)

file_path <- file.path("data-master/Plotdata\\", paste0("Plot2_regional_pop_type.Rdata"))
assign("Plot2",dat_twoyears_long)
save(Plot2, file = file_path)

##### Plot 3 --- Refugees by Top 5 COO ####

ref_coo <- POC(MYSR(MyYear) %>% filter(region_o == MyRegion), originCountry, MyYear) 
  
ref_coo_asia <- ref_coo %>% 
  mutate(originCountry = replace(originCountry, originCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% 
  group_by(originCountry) %>% 
  summarise(Refugees = sum(Refugees),
            Refugee_reg = sum(Refugee_reg),
            Refugee_like = sum(Refugee_like))

Plot3 <- ref_coo_asia %>%   
  arrange(desc(Refugees)) %>%  
  slice_head(n = 5) %>% 
  select(originCountry, Refugee_reg, Refugee_like) %>% 
  rename("Registered Refugees" = Refugee_reg,
         "Refugee-like" = Refugee_like) %>% 
  gather(key = "pop_type", value = "ref_figure", 2:3) 



file_path <- file.path("data-master/Plotdata\\", paste0("Plot3_refugee_COO.Rdata"))
save(Plot3, file = file_path)


##### Plot 4 --- Refugees and SDG 10.7.4

url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"

destfile = "data-raw/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"
if (!file.exists(destfile)) {
  #setInternet2(TRUE)
  download.file(url ,destfile, method = "curl", quiet = FALSE) }

#Read data from WPP
pop_wpp = read_excel(destfile,sheet = 'Medium variant', skip = 16,col_names = TRUE, guess_max = 100000)

pop_year_wpp = filter(pop_wpp, Year == 2023)

ref_oip_coo <- POC(MYSR(MyYear) %>% filter(region_o == MyRegion), iso_o, MyYear) 

ref_coo_oip_asia <- ref_oip_coo %>% 
  mutate(iso_o = replace(iso_o, iso_o %in% c("HKG","MAC"),"CHN")) %>% 
  group_by(iso_o) %>% 
  summarise(Refugees = sum(Refugees),
            Refugee_reg = sum(Refugee_reg),
            Refugee_like = sum(Refugee_like),
            OIP = sum(OIP)) %>% 
  mutate(Refugee_Mandate = Refugees + OIP) %>%
  arrange(desc(Refugee_Mandate)) %>% 
  merge(x = ., y = pop_year_wpp, by.x = "iso_o", by.y = "ISO3 Alpha-code") %>% 
  mutate(population = as.numeric(as.character(`Total Population, as of 1 July (thousands)`))*1000) %>% 
  mutate(sdg = round(Refugee_Mandate * 1e5/ (Refugee_Mandate + population),-2)) %>% 
  filter (population > 1e5) %>% 
  arrange(desc(sdg)) %>% 
  slice_head(n = 5) 
  
Plot4 <- ref_coo_oip_asia %>% 
  select(iso_o, `Region, subregion, country or area *`, sdg) %>% 
  rename(country = `Region, subregion, country or area *`)

file_path <- file.path("data-master/Plotdata\\", paste0("Plot4_sdg.Rdata"))
save(Plot4, file = file_path)


##### Plot 5 --- Refugees by Top 5 COA

ref_coa <- POC(get(regionDatYear),asylumCountry,MyYear)

ref_coa_asia <- ref_coa %>% 
  mutate(asylumCountry = replace(asylumCountry, asylumCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% 
  group_by(asylumCountry) %>% 
  summarise(Refugees = sum(Refugees),
            Refugee_reg = sum(Refugee_reg),
            Refugee_like = sum(Refugee_like))

Plot5 <- ref_coa_asia %>%   
  arrange(desc(Refugees)) %>%  
  slice_head(n = 5) %>% 
  select(asylumCountry, Refugee_reg, Refugee_like) %>% 
  rename("Registered Refugees" = Refugee_reg,
         "Refugee-like" = Refugee_like) %>% 
  gather(key = "pop_type", value = "ref_figure", 2:3) 



file_path <- file.path("data-master/Plotdata\\", paste0("Plot5_refugee_coa.Rdata"))
save(Plot5, file = file_path)

##### Plot 6 --- Asylum seekers by top 5 COO

asy_coo_asia <- ref_coo %>% 
  mutate(originCountry = replace(originCountry, originCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% 
  group_by(originCountry) %>% 
  summarise(Asylum_Seekers = sum(Asylum_Seekers))

Plot6 <- asy_coo_asia %>%   
  arrange(desc(Asylum_Seekers)) %>%  
  slice_head(n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot6_asy_coo.Rdata"))
save(Plot6, file = file_path)


##### Plot 7 --- Asylum seekers by top 5 COA


asy_coa_asia <- ref_coa %>% 
  mutate(asylumCountry = replace(asylumCountry, asylumCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% 
  group_by(asylumCountry) %>% 
  summarise(Asylum_Seekers = sum(Asylum_Seekers))

Plot7 <- asy_coa_asia %>%   
  arrange(desc(Asylum_Seekers)) %>%  
  slice_head(n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot7_asy_coa.Rdata"))
save(Plot7, file = file_path)

##### Plot 8 --- IDP trends in last 5 years

Plot8 <- poc_2014_my2023_myregion %>% 
  select(year, IDPs) %>% 
  filter(year %in% c("2019","2020","2021","2022","mid_2023"))


file_path <- file.path("data-master/Plotdata\\", paste0("Plot8_idp_trends.Rdata"))
save(Plot8, file = file_path)



######Plot 9 IDPs by top 5 Countries and compared to last year ####

IDPYear <- POC(get(regionDatYear),asylumCountry,MyYear) %>% 
  arrange(desc(IDPs)) %>% 
  slice_head( n = 5) %>% 
  mutate(Year = paste0("mid_",MyYear))

IDPYear_last <- POC(get(regionDatYear_last),asylumCountry,MyYear) %>% 
  filter(asylumCountry %in% IDPYear$asylumCountry)

Plot9 <- rbind(IDPYear, IDPYear_last)

file_path <- file.path("data-master/Plotdata\\", paste0("Plot9_idp_coo.Rdata"))
save(Plot9, file = file_path)



###### Plot 10 Non-Displaced stateless ####

Plot10 <- POC(get(regionDatYear),asylumCountry,MyYear) %>% 
  arrange(desc(Stateless_Non_displaced)) %>% 
  slice_head( n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot10_sta_non_displaced.Rdata"))
save(Plot10, file = file_path)


###### Plot 11 Displaced stateless ####

Plot11 <- POC(get(regionDatYear),asylumCountry,MyYear) %>% 
  arrange(desc(Stateless_Displaced)) %>% 
  slice_head( n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot11_sta_displaced.Rdata"))
save(Plot11, file = file_path)


##### Plot 12 --- Refugee returnee trends in last 5 years

Plot12 <- poc_2014_my2023_myregion %>% 
  select(year, `Returnees (Refugee)`) %>% 
  filter(year %in% c("2019","2020","2021","2022","mid_2023"))


file_path <- file.path("data-master/Plotdata\\", paste0("Plot12_returnee_ref.Rdata"))
save(Plot12, file = file_path)





##### Plot 13 --- IDP returnee trends in last 5 years

Plot13 <- poc_2014_my2023_myregion %>% 
  select(year, `Returnees (IDP)`) %>% 
  filter(year %in% c("2019","2020","2021","2022","mid_2023"))


file_path <- file.path("data-master/Plotdata\\", paste0("Plot13_returnee_idp.Rdata"))
save(Plot13, file = file_path)



#### plot 14 Myanmar situation  ######

mya <- get(regionDatYear) %>% 
  group_by(origin) %>% 
  summarise("Refugees and refugee-like" = sum(Refugees, na.rm = TRUE), 
            "Asylum-Seekers" = sum(Asylum_Seekers, na.rm = TRUE), 
            IDPs = sum(IDPs, na.rm = TRUE), 
            "Returnees (Refugee)" = sum(Returnee_Refugee, na.rm = TRUE), 
            "Returnees (IDP)" = sum(Returnee_IDP, na.rm = TRUE), 
            "Others of Concern" = sum(OOC, na.rm = TRUE), 
            "Stateless" = sum(Stateless_Total, na.rm = TRUE), 
            'Displaced Stateless' = sum(Stateless_Displaced, na.rm = TRUE)) %>% 
  mutate("Non-Displaced Stateless"= Stateless - `Displaced Stateless`) %>%
  filter(origin == "MYA") %>% 
  select(IDPs, 'Non-Displaced Stateless','Displaced Stateless', 'Refugees and refugee-like','Returnees (IDP)', 'Asylum-Seekers') %>% 
  gather(key = "pop_type", value = "pop_figure")

mya$stack <- ifelse(mya$pop_type %in% c('Non-Displaced Stateless','Displaced Stateless'), "Stateless", mya$pop_type)

custom_order<- c("Asylum-Seekers", "Returnees (IDP)", "Refugees and refugee-like", 'Displaced Stateless', "Non-Displaced Stateless", "IDPs")
custom_order_stack <- c("Asylum-Seekers", "Returnees (IDP)", "Refugees and refugee-like", "Stateless", "IDPs")
mya$stack <- factor(mya$stack, levels = custom_order_stack)
mya$pop_type <- factor(mya$pop_type, levels = custom_order)

Plot14 <- mya

file_path <- file.path("data-master/Plotdata\\", paste0("Plot14_mya.Rdata"))
save(Plot14, file = file_path)


#### Plot 15 Rohingya ####

RoHYear <- get(regionDatYear) %>% 
  filter(origin == "MYA") %>% 
  select(Year, asylumCountry, Stateless_Total) %>% 
  arrange(desc(Stateless_Total)) %>% 
  slice_head(n=5) %>% 
  mutate(Year = paste0("mid_",MyYear))

RoHYear_last <- get(regionDatYear_last) %>% 
  filter(origin == "MYA") %>% 
  select(Year, asylumCountry, Stateless_Total) %>% 
  filter(asylumCountry %in% RoHYear$asylumCountry)

Plot15 <- rbind(RoHYear_last, RoHYear)

file_path <- file.path("data-master/Plotdata\\", paste0("Plot15_rohingya.Rdata"))
save(Plot15, file = file_path)



