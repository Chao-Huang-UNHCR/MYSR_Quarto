library(refugees)
library(ggplot2)
library(unhcrthemes)
library(scales)
library(popdata)
library(tidyverse)
library(readxl)


##########2014-2022 Data from refugee package###############
pop <- refugees::population %>% filter(year > 2013)

reference |> select(iso_3, UNHCRcode, ctryname, UNHCRBureau)

pop_reg <- merge(pop,reference, by.x = "coa", by.y = "UNHCRcode", all.x=T) %>% select(-iso_3, -ctryname)

pop_asia <- pop_reg %>% filter(UNHCRBureau == "Asia") %>% group_by(year,UNHCRBureau)%>% summarise(Refugees = sum(refugees), asylum_seekers = sum(asylum_seekers), IDPs = sum(idps), "Returnees (Refugee)" = sum(returned_refugees), "Returnees (IDP)" = sum(returned_idps), "Others of Concern" = sum(ooc))

ndp_sta_decade <- read_excel("data-master/ndp_sta_decade.xlsx")

pop_asia_decade <- pop_asia %>% merge(ndp_sta_decade, by = "year", all.x=T) %>% mutate("Refugees, Refugee-like and Asylum-Seekers" = Refugees + asylum_seekers) %>%  select(year,"Refugees, Refugee-like and Asylum-Seekers", IDPs, "Non-Displaced Stateless", "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern")

#############MYSR Data from function data-master folder################
load("data-master/Asia_2023_clean_data.Rdata")

mysr2023_asia <- Asia_2023_clean_data %>% mutate(Year = ifelse(Year == 2023, "mid-2023", Year)) %>% merge(reference, by.x = "asylum", by.y = "UNHCRcode", all.x=T) %>% select(Year, Refugees, Asylum_Seekers, IDPs, Returnee_Refugee, Returnee_IDP, Stateless_Total, Stateless_Displaced, OOC) %>% group_by(Year) %>% summarise("Refugees and refugee-like" = sum(Refugees, na.rm = TRUE), "Asylum-Seekers" = sum(Asylum_Seekers, na.rm = TRUE), IDPs = sum(IDPs, na.rm = TRUE), "Returnees (Refugee)" = sum(Returnee_Refugee, na.rm = TRUE), "Returnees (IDP)" = sum(Returnee_IDP, na.rm = TRUE), "Others of Concern" = sum(OOC, na.rm = TRUE), "Stateless" = sum(Stateless_Total, na.rm = TRUE), "Stateless_Displaced" = sum(Stateless_Displaced, na.rm = TRUE)) %>% mutate("Non-Displaced Stateless"= Stateless - Stateless_Displaced)
  
agg_mysr2023_asia <- mysr2023_asia %>% mutate("Refugees, Refugee-like and Asylum-Seekers"= mysr2023_asia[[2]]  + mysr2023_asia[[3]]) %>%  select(Year,"Refugees, Refugee-like and Asylum-Seekers", IDPs, "Non-Displaced Stateless", "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern")

colnames(agg_mysr2023_asia)[colnames(agg_mysr2023_asia) == "Year"] <- "year"

poc_2014_my2023_asia <- rbind(pop_asia_decade, agg_mysr2023_asia)

#########Graph POC Decade#######################
poc_2014_my2023_asia_long <- poc_2014_my2023_asia %>%  gather(key = "pop_type", value = "pop_figure", 2:7)
poc_2014_my2023_asia_long <- poc_2014_my2023_asia_long %>% mutate(pop_mil = pop_figure / 1000000)

poc_2014_my2023_asia_long$pop_type <- factor(poc_2014_my2023_asia_long$pop_type, levels = c("Others of Concern","Returnees (IDP)","Returnees (Refugee)","Non-Displaced Stateless","IDPs","Refugees, Refugee-like and Asylum-Seekers"))

sums <- poc_2014_my2023_asia_long %>%
  group_by(year) %>%
  summarise(total_poc = sum(pop_mil))

gg <- ggplot(poc_2014_my2023_asia_long) +
  geom_col(aes(x = year,
               y = pop_mil,
               fill = pop_type),
           width = 0.7)  +
  scale_fill_unhcr_d(palette = "pal_unhcr_poc",
                     nmax = 9,
                     order = c(8,6,9,5,4,1))+
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  scale_y_continuous(expand = expansion(c(0, 0.2))) +
  labs(title = "People UNHCR protects and/or assists in the Asia Pacific Region | 2014 -mid-2023",
       y = "Number of people (million)")+
  theme_unhcr(grid = FALSE,
              axis_title = "y")

gg + geom_text(data = sums, aes(x = year, y = total_poc, label = round(total_poc, 1)), vjust = -0.5)

##########Volrep and IDP returnees chart############
solution_2019_my2023 <- poc_2014_my2023_asia %>% filter(year > 2018) %>% select(year,`Returnees (IDP)`, `Returnees (Refugee)`)

ggplot(solution_2019_my2023) +
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
    vjust = -1))+
  labs(title = "IDP Returnees Trends | 2019 - mid-2023",
       y = "Number of people (millions)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  theme_unhcr(grid = FALSE,
              axis = "x",
              axis_title = "y",
              axis_text = "x")

ggplot(solution_2019_my2023) +
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
    vjust = -1))+
  labs(title = "Voluntary Repatriation Trends | 2019 - mid-2023",
       y = "Number of people (millions)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  theme_unhcr(grid = FALSE,
              axis = "x",
              axis_title = "y",
              axis_text = "x")

########IDP Charts##########

####IDP 2019-mid2023
idp_asia_2019_mysr2023 <- poc_2014_my2023_asia %>% select(year, IDPs) %>% filter(year %in% c("2019","2020","2021","2022","mid-2023"))

ggplot(idp_asia_2019_mysr2023) +
  geom_line(aes(
    x = year,
    y = IDPs,
    group=1),
  size = 1,
  color = unhcr_pal(n = 1, "pal_blue")) +
  geom_text(aes(
    x = year,
    y = IDPs,
    label = round(IDPs / 1e6, digits = 1),
    vjust = -1))+
  labs(title = "IDP trends in Asia-Pacific | 2019 - mid-2023",
    y = "Number of people (millions)",
    caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_y_continuous(limits= c(0, 5.5 * 1e6))+
  theme_unhcr(grid = FALSE,
    axis = "x",
    axis_title = "y",
    axis_text = "x")

####IDPs 2022 vs mid2023###########
idp_asr2022_asia <- Asia_2022_clean_data %>% select(Year,asylumCountry, IDPs) %>% group_by(Year, asylumCountry) %>% summarise(IDPs = sum(IDPs, na.rm = TRUE)) %>% arrange(desc(IDPs)) %>% slice_head(n = 5)
idp_asr2022_asia$Year <- as.character(idp_asr2022_asia$Year)

idp_mysr2023_asia <- Asia_2023_clean_data %>% select(Year, asylumCountry,IDPs) %>% group_by(Year, asylumCountry) %>% summarise(IDPs = sum(IDPs, na.rm = TRUE)) %>% arrange(desc(IDPs)) %>% slice_head(n = 5) %>% mutate(Year = ifelse(Year == 2023, "mid-2023", Year))

idp_asr2022_mysr2023 <- rbind(idp_asr2022_asia, idp_mysr2023_asia)

ggplot(idp_asr2022_mysr2023) +
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
    hjust = -0.25, size = 8 / .pt) +
  scale_fill_unhcr_d(palette = "pal_unhcr") +
  labs(title = "Countries with the most conflict-induced IDPs in Asia-Pacific | 2022 - mid-2023",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(labels = scales::comma, expand = expansion(c(0, 0.1))) +
  scale_y_discrete(labels = scales::label_wrap(17)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y")

##############Graph POC AP 2022 vs mid-2023#################
load("data-master/Asia_2022_clean_data.Rdata")

asr2022_asia <- Asia_2022_clean_data %>% select(Year, Refugees, Asylum_Seekers, IDPs, Returnee_Refugee, Returnee_IDP, Stateless_Total, Stateless_Displaced, OOC) %>% group_by(Year) %>% summarise("Refugees and refugee-like" = sum(Refugees, na.rm = TRUE), "Asylum-Seekers" = sum(Asylum_Seekers, na.rm = TRUE), IDPs = sum(IDPs, na.rm = TRUE), "Returnees (Refugee)" = sum(Returnee_Refugee, na.rm = TRUE), "Returnees (IDP)" = sum(Returnee_IDP, na.rm = TRUE), "Others of Concern" = sum(OOC, na.rm = TRUE), "Stateless" = sum(Stateless_Total, na.rm = TRUE), "Stateless_Displaced" = sum(Stateless_Displaced, na.rm = TRUE)) %>% mutate("Non-Displaced Stateless"= Stateless - Stateless_Displaced)

asr2022_asia_poc <- asr2022_asia %>% select(Year,"Refugees and refugee-like", "Asylum-Seekers",IDPs, Stateless, "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern")

mysr2023_asia_poc <- mysr2023_asia %>% select(Year,"Refugees and refugee-like", "Asylum-Seekers",IDPs, Stateless, "Returnees (Refugee)", "Returnees (IDP)", "Others of Concern")

mysr2023_asr2022_asia <- rbind(asr2022_asia_poc, mysr2023_asia_poc)

mysr2023_asr2022_asia_long <- mysr2023_asr2022_asia %>% gather(key = "pop_type", value = "pop_figure", 2:8)


ggplot(mysr2023_asr2022_asia_long) +
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
      hjust = -0.25, size = 8 / .pt) +
    scale_fill_unhcr_d(palette = "pal_unhcr") +
    labs(title = "Population Categories in Asia-Pacific | 2022 - mid-2023",
    caption = "Note: Stateless persons have been included in both stateless and their respective displacement categories\nSource: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
    scale_x_continuous(labels = scales::comma, expand = expansion(c(0, 0.1))) +
    scale_y_discrete(labels = scales::label_wrap(17)) +
    theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y")
  
###################Refugee charts#############
reference <- read.csv("data-master/reference.csv")
code <- reference %>% select(UNHCRcode, UNHCRBureau, ctryname)
code_d <- code %>%  dplyr::rename(asylum = UNHCRcode, asylum_region = UNHCRBureau, asy_ctry = ctryname)
code_o <- code %>%  dplyr::rename(origin = UNHCRcode, origin_region = UNHCRBureau, orig_ctry = ctryname)

pd_login()
ref_mysr2023 <- pd_mysr(table = "refugees", year = 2023, quiet = TRUE)
ref_mysr2023_fil <- ref_mysr2023[, c("asylum", "origin","midYearTotal")]
ref_mysr2023_fil <- ref_mysr2023_fil  %>% group_by(asylum, origin) %>% summarise(midYearTotal = sum(midYearTotal))
colnames(ref_mysr2023_fil)[colnames(ref_mysr2023_fil) == "midYearTotal"] <- "Refugees"


ref_like_mysr2023 = pd_mysr(table = "refugeeLike", year = 2023, quiet = TRUE)
ref_like_mysr2023_fil <- ref_like_mysr2023[, c("asylum", "origin","midYearTotal")]
ref_like_mysr2023_fil <- ref_like_mysr2023_fil  %>% group_by(asylum, origin) %>% summarise(midYearTotal = sum(midYearTotal))
colnames(ref_like_mysr2023_fil)[colnames(ref_like_mysr2023_fil) == "midYearTotal"] <- "Refugee_like"

ref_mysr2023_disag <- merge(ref_mysr2023_fil,ref_like_mysr2023_fil,by=c('asylum','origin'),all=TRUE)
ref_mysr2023_disag[is.na(ref_mysr2023_disag)] <- 0
ref_mysr2023_disag <- ref_mysr2023_disag %>% mutate("sum_ref" = ref_mysr2023_disag[[3]]+ref_mysr2023_disag[[4]])

ref_mysr2023_disag <- ref_mysr2023_disag %>% mutate(origin = replace(origin, origin %in% c("HKG","MAC","TIB"),"CHI")) %>% mutate(asylum = replace(asylum, asylum %in% c("HKG","MAC","TIB"),"CHI"))

ref_mysr2023_disag <- merge(ref_mysr2023_disag, code_d,by = "asylum", all.x = T)
ref_mysr2023_disag <- merge(ref_mysr2023_disag, code_o,by = "origin", all.x = T)

asy_asia_mysr_disag <- ref_mysr2023_disag %>% filter(asylum_region == "Asia") %>% group_by(asy_ctry) %>% summarise(Refugees = sum(Refugees, na.rm = TRUE), "Refugee-like" = sum(Refugee_like, na.rm = TRUE), sum_ref = sum(sum_ref, na.rm = TRUE))

top5_asy_asia_ref <- asy_asia_mysr_disag %>%
  arrange(desc(sum_ref)) %>%  
  slice_head(n = 5)    

filt_top5_asy_asia_ref_long <- top5_asy_asia_ref %>% select(asy_ctry, Refugees, `Refugee-like`) %>% gather(key = "pop_type", value = "ref_figure", 2:3)

ggplot(filt_top5_asy_asia_ref_long) +
  geom_col(aes(x = ref_figure,
               y = reorder(asy_ctry, ref_figure),
               fill = pop_type),
           width = 0.7) +
  geom_text(data = filter(filt_top5_asy_asia_ref_long, ref_figure > 40000),
    aes(x = ref_figure,
                y = reorder(asy_ctry, ref_figure),
                group = pop_type,
                label = round(ref_figure / 1e6, 2)),
            position = position_stack(vjust = 0.5),
            size = 8/.pt) +
  labs(title = "Refugees and people in refugee-like situations hosted in Asia-Pacific | mid-2023",
       subtitle = "Number of people (million)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_unhcr_d(palette = "pal_unhcr",
                     nmax = 10,
                     order = c(2, 1)) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")


orig_asia_mysr_disag <- ref_mysr2023_disag %>% filter(origin_region == "Asia") %>% group_by(orig_ctry) %>% summarise(Refugees = sum(Refugees, na.rm = TRUE), "Refugee-like" = sum(Refugee_like, na.rm = TRUE), sum_ref = sum(sum_ref, na.rm = TRUE))

top5_orig_asia_ref <- orig_asia_mysr_disag %>%
  arrange(desc(sum_ref)) %>%  
  slice_head(n = 5) 

top5_orig_asia_ref_long <- top5_orig_asia_ref %>% select(orig_ctry, Refugees, `Refugee-like`) %>% gather(key = "pop_type", value = "ref_figure", 2:3)

ggplot(top5_orig_asia_ref_long) +
  geom_col(aes(x = ref_figure,
               y = reorder(orig_ctry, ref_figure),
               fill = pop_type), width = 0.7) +
  geom_text(data = filter(top5_orig_asia_ref_long, ref_figure > 40000),
            aes(x = ref_figure,
                y = reorder(orig_ctry, ref_figure),
                group = pop_type,
                label = round(ref_figure / 1e6, 2)),
            position = position_stack(vjust = 0.6), size = 8/.pt) +
  labs(title = "Refugees and people in refugee-like situations hosted in Asia-Pacific | mid-2023",
       subtitle = "Number of people (million)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_unhcr_d(palette = "pal_unhcr",
                     nmax = 10,
                     order = c(2, 1)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y")

##########Asylum seekers chart###################

#Top 5 COA countries asylum-seekers
coa_as_asia_mysr2023 <- Asia_2023_clean_data %>% mutate(originCountry = replace(originCountry, originCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% mutate(asylumCountry = replace(asylumCountry, asylumCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% group_by(asylumCountry) %>% summarise(Asylum_Seekers = sum(Asylum_Seekers, na.rm = TRUE))

top5_as_coo_asia_mysr2023 <- coa_as_asia_mysr2023 %>%
  arrange(desc(Asylum_Seekers)) %>%  
  slice_head(n = 5)

ggplot(top5_as_coo_asia_mysr2023) +
  geom_col(aes(
    x = Asylum_Seekers,
    y = reorder(asylumCountry, Asylum_Seekers)),
  fill = unhcr_pal(n = 1, "pal_blue"),
  width = 0.8) +
  geom_text(aes(
    x = Asylum_Seekers,
    y = reorder(asylumCountry, Asylum_Seekers),
    label = scales::comma(round(Asylum_Seekers, -2)),
  hjust = -0.5)) +
  labs(title = "Asylum-seekers by host country in Asia-Pacific | mid-2023",
    caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
    axis = "y",
    axis_title = FALSE,
    axis_text = "y")+
  guides(fill="none")


#Top 5 COO countries asylum-seekers
load("data-master/Global_2023_clean_data.Rdata")

coo_as_asia_mysr2023 <- Global_2023_clean_data %>% mutate(originCountry = replace(originCountry, originCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% mutate(asylumCountry = replace(asylumCountry, asylumCountry %in% c("China, Hong Kong SAR","China, Macao SAR"),"China")) %>% filter(region_o == "Asia") %>% group_by(originCountry) %>% summarise(Asylum_Seekers = sum(Asylum_Seekers, na.rm = TRUE)) 

top5_as_coo_asia_mysr2023 <- coo_as_asia_mysr2023 %>%
  arrange(desc(Asylum_Seekers)) %>%  
  slice_head(n = 5)

ggplot(top5_as_coo_asia_mysr2023) +
  geom_col(aes(
    x = Asylum_Seekers,
    y = reorder(originCountry, Asylum_Seekers)),
    fill = unhcr_pal(n = 1, "pal_blue"),
    width = 0.8) +
  geom_text(aes(
    x = Asylum_Seekers,
    y = reorder(originCountry, Asylum_Seekers),
    label = scales::comma(round(Asylum_Seekers, -2)),
    hjust = -0.5)) +
  labs(title = "Asylum-seekers globally by country of origin from Asia-Pacific | mid-2023",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")+
  guides(fill="none")

display_unhcr_all()


#######Stateless charts###########
stateless_mysr2023_asia <- Asia_2023_clean_data %>% mutate(Year = ifelse(Year == 2023, "mid-2023", Year)) %>% group_by(Year,asylumCountry) %>% summarise(Stateless_Total = sum(Stateless_Total, na.rm = TRUE), Stateless_Displaced = sum(Stateless_Displaced, na.rm = TRUE)) %>% mutate(Stateless_NDP = Stateless_Total - Stateless_Displaced) %>% select(Year, asylumCountry, Stateless_Total, Stateless_NDP, Stateless_Displaced) 

top5_ndp_stateless_asia_mysr2023 <- stateless_mysr2023_asia %>% select(Year, asylumCountry, Stateless_NDP) %>% arrange(desc(Stateless_NDP)) %>% slice_head(n = 5)

top5_dis_stateless_asia_mysr2023 <- stateless_mysr2023_asia %>% select(Year, asylumCountry, Stateless_Displaced) %>% arrange(desc(Stateless_Displaced)) %>% slice_head(n = 5)

####Chart of non-displaced stateless########
ggplot(top5_ndp_stateless_asia_mysr2023) +
  geom_col(aes(
    x = Stateless_NDP,
    y = reorder(asylumCountry, Stateless_NDP)),
    fill = unhcr_pal(n = 1, "pal_blue"),
    width = 0.8) +
  geom_text(aes(
    x = Stateless_NDP,
    y = reorder(asylumCountry, Stateless_NDP),
    label = scales::comma(round(Stateless_NDP, -2)),
    hjust = -0.5)) +
  labs(title = "Top Countries with Non-Displaced Stateless Persons in Asia-Pacific | mid-2023",
       subtitle = "Number of stateless persons",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")+
  guides(fill="none")

####Chart of displaced stateless########
ggplot(top5_dis_stateless_asia_mysr2023) +
  geom_col(aes(
    x = Stateless_Displaced,
    y = reorder(asylumCountry, Stateless_Displaced)),
    fill = unhcr_pal(n = 1, "pal_blue"),
    width = 0.8) +
  geom_text(aes(
    x = Stateless_Displaced,
    y = reorder(asylumCountry, Stateless_Displaced),
    label = scales::comma(round(Stateless_Displaced, -2)),
    hjust = -0.5)) +
  labs(title = "Top Countries with Displaced Stateless Persons in Asia-Pacific | mid-2023",
       subtitle = "Number of stateless persons",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y")+
  guides(fill="none")






load("data-master/Refugee_2023_clean_data.Rdata")
ref_mysr2023_disag <- Refugee_2023_clean_data

#Refugees by COA
asy_asia_mysr_disag <- ref_mysr2023_disag %>% filter(region_d == "Asia") %>% group_by(asylumCountry) %>% summarise(Refugees = sum(Refugees, na.rm = TRUE), "Refugee-like" = sum(Refugee_like, na.rm = TRUE), sum_ref = sum(sum_ref, na.rm = TRUE))

top5_asy_asia_ref <- asy_asia_mysr_disag %>%
  arrange(desc(sum_ref)) %>%  
  slice_head(n = 5)    

filt_top5_asy_asia_ref_long <- top5_asy_asia_ref %>% select(asylumCountry, Refugees, `Refugee-like`) %>% gather(key = "pop_type", value = "ref_figure", 2:3)

ggplot(filt_top5_asy_asia_ref_long) +
  geom_col(aes(x = ref_figure,
               y = reorder(asylumCountry, ref_figure),
               fill = pop_type),
           width = 0.7) +
  geom_text(data = filter(filt_top5_asy_asia_ref_long, ref_figure > 40000),
            aes(x = ref_figure,
                y = reorder(asylumCountry, ref_figure),
                group = pop_type,
                label = round(ref_figure / 1e6, 2)),
            position = position_stack(vjust = 0.5),
            size = 10) +
  labs(title = "Refugees and people in refugee-like situations hosted in Asia-Pacific | mid-2023",
       subtitle = "Number of people (million)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_unhcr_d(palette = "pal_unhcr",
                     nmax = 10,
                     order = c(2, 1)) +
  theme_unhcr(grid = FALSE,
              axis = "y",
              axis_title = FALSE,
              axis_text = "y") + theme(text = element_text(size = 20)) + theme(plot.subtitle=element_text(size=18))

#Refugees by COO
orig_asia_mysr_disag <- ref_mysr2023_disag %>% filter(region_o == "Asia") %>% group_by(originCountry) %>% summarise(Refugees = sum(Refugees, na.rm = TRUE), "Refugee-like" = sum(Refugee_like, na.rm = TRUE), sum_ref = sum(sum_ref, na.rm = TRUE))

top5_orig_asia_ref <- orig_asia_mysr_disag %>%
  arrange(desc(sum_ref)) %>%  
  slice_head(n = 5) 

top5_orig_asia_ref_long <- top5_orig_asia_ref %>% select(originCountry, Refugees, `Refugee-like`) %>% gather(key = "pop_type", value = "ref_figure", 2:3)

ggplot(top5_orig_asia_ref_long) +
  geom_col(aes(x = ref_figure,
               y = reorder(originCountry, ref_figure),
               fill = pop_type), width = 0.7) +
  geom_text(data = filter(top5_orig_asia_ref_long, ref_figure > 40000),
            aes(x = ref_figure,
                y = reorder(originCountry, ref_figure),
                group = pop_type,
                label = round(ref_figure / 1e6, 2)),
            position = position_stack(vjust = 0.6), size = 10) +
  labs(title = "Refugees and people in refugee-like situations from Asia-Pacific | mid-2023",
       subtitle = "Number of people (million)",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_unhcr_d(palette = "pal_unhcr",
                     nmax = 10,
                     order = c(2, 1)) +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") + theme(text = element_text(size = 20)) + theme(plot.subtitle=element_text(size=18))


######Myanmar Chart#########
mya_mysr2023 <- Asia_2023_clean_data %>% group_by(origin) %>% summarise("Refugees and refugee-like" = sum(Refugees, na.rm = TRUE), "Asylum-Seekers" = sum(Asylum_Seekers, na.rm = TRUE), IDPs = sum(IDPs, na.rm = TRUE), "Returnees (Refugee)" = sum(Returnee_Refugee, na.rm = TRUE), "Returnees (IDP)" = sum(Returnee_IDP, na.rm = TRUE), "Others of Concern" = sum(OOC, na.rm = TRUE), "Stateless" = sum(Stateless_Total, na.rm = TRUE), "Stateless_Displaced" = sum(Stateless_Displaced, na.rm = TRUE)) %>% mutate("Non-Displaced Stateless"= Stateless - Stateless_Displaced) %>% filter(origin == "MYA")

mya_mysr2023 <- mya_mysr2023 %>% select(IDPs, 'Non-Displaced Stateless','Stateless_Displaced', 'Refugees and refugee-like','Returnees (IDP)', 'Asylum-Seekers')

mya_mysr2023_long <- mya_mysr2023 %>%
  gather(key = "pop_type", value = "pop_figure")

mya_mysr2023_long$stack <- ifelse(mya_mysr2023_long$pop_type %in% c('Non-Displaced Stateless','Stateless_Displaced'), "Stateless", mya_mysr2023_long$pop_type)

custom_order<- c("Asylum-Seekers", "Returnees (IDP)", "Refugees and refugee-like", "Stateless_Displaced", "Non-Displaced Stateless", "IDPs")
custom_order_stack <- c("Asylum-Seekers", "Returnees (IDP)", "Refugees and refugee-like", "Stateless", "IDPs")
mya_mysr2023_long$stack <- factor(mya_mysr2023_long$stack, levels = custom_order_stack)
mya_mysr2023_long$pop_type <- factor(mya_mysr2023_long$pop_type, levels = custom_order)

ggplot(mya_mysr2023_long) +
  geom_col(aes(x = pop_figure,
               y = stack,
               fill = pop_type), width = 0.7, position = "stack") +
  geom_text(aes(x = pop_figure,
                y =stack,
                label = round(pop_figure / 1e6, 2)),
            position = position_stack(vjust = 0.7), size=6) +
  labs(title = "Myanmar Situation | mid-2023",
       caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency") +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_manual(values = c('Refugees and refugee-like' = "#0072BC", 'Asylum-Seekers' = "#0072BC", IDPs = "#0072BC", 'Non-Displaced Stateless' = "#0072BC", 'Returnees (IDP)' = "#0072BC", 'Stateless_Displaced' = "#CCCCCC"), breaks = 'Stateless_Displaced') +
  theme_unhcr(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y") + theme(text = element_text(size = 20)) + theme(plot.subtitle=element_text(size=18))

display_unhcr_all()
