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

file_path <- file.path("data-master/Plotdata\\", paste0("Plot1.Rdata"))
assign("Plot1",poc_2014_my2023_myregion_long)
save(Plot1, file = file_path)

file_path <- file.path("data-master/Plotdata\\", paste0("Plot1_label.Rdata"))
assign("Plot1_label",sums)
save(Plot1_label, file = file_path)

# Plot1

gg <- ggplot(Plot1) +
  geom_col(aes(x = year,
               y = pop_mil,
               fill = pop_type),
           width = 0.7)  +
  scale_fill_unhcr_d(palette = "pal_unhcr_poc",
                     nmax = 9,
                     order = c(8,6,9,5,4,1))+
  scale_y_continuous(breaks = pretty_breaks(n = 6), 
                     limits = c(0,14.5),
                     expand = expansion(c(0, 0.2)))+
  labs(title = paste0("People UNHCR protects and/or assists in the ",plot_title_region, " Region | ", MyYear - 9," - mid_", MyYear),
       y = "Number of people (million)")+
  theme_unhcr(grid = FALSE,
              axis_title = "y")

gg + geom_text(size = 9,
               data = Plot1_label, 
               aes(x = year, y = total_poc, label = round(total_poc, 1)), 
               vjust = -0.5) + 
  theme(text = element_text(size = 18))  

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

file_path <- file.path("data-master/Plotdata\\", paste0("Plot2.Rdata"))
assign("Plot2",dat_twoyears_long)
save(Plot2, file = file_path)

ggplot(Plot2) +
  geom_col(aes(x = pop_figure, 
               y = fct_rev(reorder(pop_type, -pop_figure)), 
               fill = as.character(Year)), 
           position = position_dodge(0.7), width = 0.6)+
  geom_text(aes(
    x = pop_figure,
    y = fct_rev(reorder(pop_type, -pop_figure)),
    group = as.character(Year),
    label = scales::comma(round(pop_figure,-2))),
    position = position_dodge(width = 0.7),
    hjust = -0.05, size = 6.5) +
  scale_fill_unhcr_d(palette = "pal_unhcr") +
  labs(title = paste0("Population Categories in ", plot_title_region, " | ", MyYear-1, " - mid_", MyYear),
       caption = "Note: Stateless persons have been included in both stateless and their respective displacement categories\nSource: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(labels = scales::comma, expand = expansion(c(0, 0.1))) +
  scale_y_discrete(labels = scales::label_wrap(17)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") +
  theme(text = element_text(size = 18)) 

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



file_path <- file.path("data-master/Plotdata\\", paste0("Plot3.Rdata"))
save(Plot3, file = file_path)

ggplot(Plot3) +
  geom_col(aes(x = ref_figure,
               y = reorder(originCountry, ref_figure),
               fill = pop_type), width = 0.7) +
  geom_text(data = filter(Plot3, ref_figure > 40000),
            aes(x = ref_figure,
                y = reorder(originCountry, ref_figure),
                group = pop_type,
                label = round(ref_figure / 1e6, 2)),
            position = position_stack(vjust = 0.75), size = 6) +
  labs(title = paste0("Refugees and people in refugee-like situations from ",  plot_title_region, " | mid_", MyYear),
       subtitle = "Number of people (millions)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_unhcr_d(palette = "pal_unhcr",
                     nmax = 10,
                     order = c(2, 1)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") + theme(text = element_text(size = 20)) + theme(plot.subtitle=element_text(size=18))

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

file_path <- file.path("data-master/Plotdata\\", paste0("Plot4.Rdata"))
save(Plot4, file = file_path)

ggplot(Plot4) + 
  geom_col(aes(x = reorder(country, sdg), y = sdg),
           fill = unhcr_pal(n=1, "pal_blue"))+
  coord_flip()+
  labs(title = paste0("Sustainable Development Goals indicator 10.7.4 | mid_", MyYear),
       x = "Countries" , y = "SDG Indicator 10.7.4") +
  geom_text(aes(x = country, 
                y = sdg, 
                label = scales::comma(sdg), 
                hjust = -0.1))+
  scale_y_continuous(expand = expansion(c(0,0.1)))+
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")

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



file_path <- file.path("data-master/Plotdata\\", paste0("Plot5.Rdata"))
save(Plot5, file = file_path)

ggplot(Plot5) +
  geom_col(aes(x = ref_figure,
               y = reorder(asylumCountry, ref_figure),
               fill = pop_type), width = 0.7) +
  geom_text(data = filter(Plot5, ref_figure > 40000),
            aes(x = ref_figure,
                y = reorder(asylumCountry, ref_figure),
                group = pop_type,
                label = round(ref_figure / 1e6, 2)),
            position = position_stack(vjust = 0.75), size = 6) +
  labs(title = paste0("Refugees and people in refugee-like situations hosted in ",  plot_title_region, " | mid_", MyYear),
       subtitle = "Number of people (millions)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_unhcr_d(palette = "pal_unhcr",
                     nmax = 10,
                     order = c(2, 1)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") + theme(text = element_text(size = 20)) + theme(plot.subtitle=element_text(size=18))


##### Plot 6 --- Asylum seekers by top 5 COO

asy_coo_asia <- ref_coo %>% 
  mutate(originCountry = replace(originCountry, originCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% 
  group_by(originCountry) %>% 
  summarise(Asylum_Seekers = sum(Asylum_Seekers))

Plot6 <- asy_coo_asia %>%   
  arrange(desc(Asylum_Seekers)) %>%  
  slice_head(n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot6.Rdata"))
save(Plot6, file = file_path)

ggplot(Plot6) +
  geom_col(aes(
    x = Asylum_Seekers,
    y = reorder(originCountry, Asylum_Seekers)),
    fill = unhcr_pal(n = 1, "pal_blue"),
    width = 0.8) +
  geom_text(aes(
    x = Asylum_Seekers,
    y = reorder(originCountry, Asylum_Seekers),
    label = scales::comma(round(Asylum_Seekers, -2)),
    hjust = -0.1), size = 7) +
  labs(title = paste0("Refugees and people in refugee-like situations from ",  plot_title_region, " | mid_", MyYear),
       caption = "Note: China includes Hong Kong, Macau and Tibet\nSource: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")+
  guides(fill="none") + theme(text = element_text(size = 20))


##### Plot 7 --- Asylum seekers by top 5 COA


asy_coa_asia <- ref_coa %>% 
  mutate(asylumCountry = replace(asylumCountry, asylumCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% 
  group_by(asylumCountry) %>% 
  summarise(Asylum_Seekers = sum(Asylum_Seekers))

Plot7 <- asy_coa_asia %>%   
  arrange(desc(Asylum_Seekers)) %>%  
  slice_head(n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot7.Rdata"))
save(Plot7, file = file_path)

ggplot(Plot7) +
  geom_col(aes(
    x = Asylum_Seekers,
    y = reorder(asylumCountry, Asylum_Seekers)),
    fill = unhcr_pal(n = 1, "pal_blue"),
    width = 0.8) +
  geom_text(aes(
    x = Asylum_Seekers,
    y = reorder(asylumCountry, Asylum_Seekers),
    label = scales::comma(round(Asylum_Seekers, -2)),
    hjust = -0.1), size = 7) +
  labs(title = paste0("Refugees and people in refugee-like situations in ",  plot_title_region, " | mid_", MyYear),
       caption = "Note: China includes Hong Kong, Macau and Tibet\nSource: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")+
  guides(fill="none") + theme(text = element_text(size = 20))



##### Plot 8 --- IDP trends in last 5 years

Plot8 <- poc_2014_my2023_myregion %>% 
  select(year, IDPs) %>% 
  filter(year %in% c("2019","2020","2021","2022","mid_2023"))


file_path <- file.path("data-master/Plotdata\\", paste0("Plot8.Rdata"))
save(Plot8, file = file_path)


ggplot(Plot8) +
  geom_line(aes(
    x = year,
    y = IDPs,
    group=1),
    linewidth = 1,
    color = unhcr_pal(n = 1, "pal_blue")) +
  geom_text(aes(
    x = year,
    y = IDPs,
    label = round(IDPs / 1e6, digits = 1),
    vjust = -1), size=9)+
  labs(title = paste0("IDP trends in  ",  plot_title_region, " | ", MyYear - 4, " - mid_", MyYear),
       y = "Number of people (millions)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_y_continuous(limits= c(0, 5.5 * 1e6))+
  theme_unhcr(grid = FALSE,
              axis = "x",
              axis_title = "y",
              axis_text = "x") + theme(text = element_text(size = 25)) 

######Plot 9 IDPs by top 5 Countries and compared to last year ####

IDPYear <- POC(get(regionDatYear),asylumCountry,MyYear) %>% 
  arrange(desc(IDPs)) %>% 
  slice_head( n = 5) %>% 
  mutate(Year = paste0("mid_",MyYear))

IDPYear_last <- POC(get(regionDatYear_last),asylumCountry,MyYear) %>% 
  filter(asylumCountry %in% IDPYear$asylumCountry)

Plot9 <- rbind(IDPYear, IDPYear_last)

file_path <- file.path("data-master/Plotdata\\", paste0("Plot9.Rdata"))
save(Plot9, file = file_path)

ggplot(Plot9) +
  geom_col(aes(x = IDPs, 
               y = fct_rev(reorder(asylumCountry, -IDPs)), 
               fill = as.character(Year)), 
           position = position_dodge(0.7), width = 0.6)+
  geom_text(aes(
    x = IDPs,
    y = fct_rev(reorder(asylumCountry, -IDPs)),
    group = as.character(Year),
    label = scales::comma(round(IDPs,-2))),
    position = position_dodge(width = 0.7),
    hjust = -0.03, size = 6.5) +
  scale_fill_unhcr_d(palette = "pal_unhcr") +
  labs(title = paste0("Countries with the most conflict-induced IDPs in\n", plot_title_region, " | ", MyYear-1," - mid_", MyYear),
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(labels = scales::comma, expand = expansion(c(0, 0.1))) +
  scale_y_discrete(labels = scales::label_wrap(17)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") + theme(text = element_text(size = 25))


###### Plot 10 Non-Displaced stateless ####

Plot10 <- POC(get(regionDatYear),asylumCountry,MyYear) %>% 
  arrange(desc(Stateless_Non_displaced)) %>% 
  slice_head( n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot10.Rdata"))
save(Plot10, file = file_path)

ggplot(Plot10) +
  geom_col(aes(
    x = Stateless_Non_displaced,
    y = reorder(asylumCountry, Stateless_Non_displaced)),
    fill = unhcr_pal(n = 1, "pal_blue"),
    width = 0.8) +
  geom_text(aes(
    x = Stateless_Non_displaced,
    y = reorder(asylumCountry, Stateless_Non_displaced),
    label = scales::comma(round(Stateless_Non_displaced, -2)),
    hjust = -0.05), size=8.5) +
  labs(title = paste0("Top Countries with Non-Displaced Stateless Persons in\n", plot_title_region, " | mid_", MyYear),
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")+
  guides(fill="none") + theme(text = element_text(size = 25)) + theme(plot.subtitle=element_text(size=18))



###### Plot 11 Displaced stateless ####

Plot11 <- POC(get(regionDatYear),asylumCountry,MyYear) %>% 
  arrange(desc(Stateless_Displaced)) %>% 
  slice_head( n = 5) 

file_path <- file.path("data-master/Plotdata\\", paste0("Plot11.Rdata"))
save(Plot11, file = file_path)

ggplot(Plot11) +
  geom_col(aes(
    x = Stateless_Displaced,
    y = reorder(asylumCountry, Stateless_Displaced)),
    fill = unhcr_pal(n = 1, "pal_blue"),
    width = 0.8) +
  geom_text(aes(
    x = Stateless_Displaced,
    y = reorder(asylumCountry, Stateless_Displaced),
    label = scales::comma(round(Stateless_Displaced, -2)),
    hjust = -0.05), size=8.5) +
  labs(title = paste0("Top Countries with Displaced Stateless Persons in\n", plot_title_region, " | mid_", MyYear),
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")+
  guides(fill="none") + theme(text = element_text(size = 25)) + theme(plot.subtitle=element_text(size=18))


##### Plot 12 --- Refugee returnee trends in last 5 years

Plot12 <- poc_2014_my2023_myregion %>% 
  select(year, `Returnees (Refugee)`) %>% 
  filter(year %in% c("2019","2020","2021","2022","mid_2023"))


file_path <- file.path("data-master/Plotdata\\", paste0("Plot12.Rdata"))
save(Plot12, file = file_path)


ggplot(Plot12) +
  geom_line(aes(
    x = year,
    y = `Returnees (Refugee)`,
    group=1),
    size = 1,
    color = unhcr_pal(n = 1, "pal_blue")) +
  geom_text(aes(
    x = year,
    y = `Returnees (Refugee)`,
    label = scales::comma(round(`Returnees (Refugee)`, -2)),
    vjust = -1), size = 9)+
  labs(title = paste0("Refugee Returnees Trends in  ",  plot_title_region, " | ",MyYear - 4, " - mid_", MyYear),
       y = "Number of people",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  theme_unhcr(grid = FALSE,
              axis = "x",
              axis_title = "y",
              axis_text = "x") + theme(text = element_text(size = 20))


##### Plot 13 --- IDP returnee trends in last 5 years

Plot13 <- poc_2014_my2023_myregion %>% 
  select(year, `Returnees (IDP)`) %>% 
  filter(year %in% c("2019","2020","2021","2022","mid_2023"))


file_path <- file.path("data-master/Plotdata\\", paste0("Plot13.Rdata"))
save(Plot13, file = file_path)

ggplot(Plot13) +
  geom_line(aes(
    x = year,
    y = `Returnees (IDP)`,
    group=1),
    size = 1,
    color = unhcr_pal(n = 1, "pal_blue")) +
  geom_text(aes(
    x = year,
    y = `Returnees (IDP)`,
    label = scales::comma(round(`Returnees (IDP)`, -2)),
    vjust = -1), size = 9)+
  labs(title = paste0("IDP Returnees Trends in  ",   plot_title_region, " | ",MyYear - 4, " - mid_", MyYear),
       y = "Number of people",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  theme_unhcr(grid = FALSE,
              axis = "x",
              axis_title = "y",
              axis_text = "x") + theme(text = element_text(size = 20))

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

file_path <- file.path("data-master/Plotdata\\", paste0("Plot14.Rdata"))
save(Plot14, file = file_path)

ggplot(Plot14) +
  geom_col(aes(x = pop_figure,
               y = stack,
               fill = pop_type), width = 0.7, position = "stack") +
  geom_text(aes(x = pop_figure,
                y =stack,
                label = round(pop_figure / 1e6, 2)),
            position = position_stack(vjust = 0.8), size=7) +
  labs(title = paste0("Myanmar Situation | mid_", MyYear),
       subtitle = "Number of people (millions)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_manual(values = c('Refugees and refugee-like' = "#0072BC", 'Asylum-Seekers' = "#0072BC", IDPs = "#0072BC", 'Non-Displaced Stateless' = "#0072BC", 'Returnees (IDP)' = "#0072BC", 'Displaced Stateless' = "#CCCCCC"), breaks = 'Displaced Stateless') +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") + theme(text = element_text(size = 20)) + theme(plot.subtitle=element_text(size=18))

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

file_path <- file.path("data-master/Plotdata\\", paste0("Plot15.Rdata"))
save(Plot15, file = file_path)

ggplot(Plot15) +
  geom_col(aes(x = Stateless_Total, 
               y = fct_rev(reorder(asylumCountry, -Stateless_Total)), 
               fill = as.character(Year)), 
           position = position_dodge(0.7), width = 0.6)+
  geom_text(aes(
    x = Stateless_Total,
    y = fct_rev(reorder(asylumCountry, -Stateless_Total)),
    group = as.character(Year),
    label = scales::comma(round(Stateless_Total,-2))),
    position = position_dodge(width = 0.7),
    hjust = -0.05, size = 6.5) +
  scale_fill_unhcr_d(palette = "pal_unhcr") +
  labs(title = paste0("Countries with Rohingya in ", plot_title_region," | ", MyYear-1," - mid_", MyYear),
       caption = "Note: Both displaced and non-displaced Rohingya included for Myanmar\nSource: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(labels = scales::comma, expand = expansion(c(0, 0.1))) +
  scale_y_discrete(labels = scales::label_wrap(17)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") + theme(text = element_text(size=18))


