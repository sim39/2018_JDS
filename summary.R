#filename: summary.R
#
#load packages
library(dplyr)
library(tidyr)

#load data
load("summary_data_msc.rdata")
load("summary_data_tsc.rdata")
load("summary_data_combined.rdata")

##Farm characteristics summary
#How many farms were sampled twice?
farm_samplings <- data_combined %>%
  group_by(FarmID) %>%
  summarise(n_farmsamplings = n())
farm_samplingTimes <- farm_samplings %>%
  summarise(n_farms = n_distinct(FarmID),
            n_farmsamplings_1 = sum(n_farmsamplings=="1"),
            n_farmsamplings_2 = sum(n_farmsamplings=="2"))

#For table 2, summarize farm characteristics by region
table2_region <- data_combined %>%
  group_by(Region) %>%
  summarize(n_farms = n_distinct(FarmID),
            n_farmsamplings = n(),
            herdsize_mean = mean(herd.size),
            kg_milkprod_mean = mean(milk.cow.year)*0.453592, #convert milk.cow.year from lbs to kg
            kg_milkprod_sd = sd(milk.cow.year)*0.453592, 
            SCC_gm_mean = exp(mean(log(aveSCC), na.rm = TRUE)), #geometric mean
            n_housing_FS = sum(lacthous=="FS"), #number farm samplings per housing type
            per_housing_FS = n_housing_FS/n_farmsamplings,
            n_housing_TS = sum(lacthous=="TS"),
            per_housing_TS = n_housing_TS/n_farmsamplings,
            n_housing_BP = sum(lacthous=="B"),
            per_housing_BP = n_housing_BP/n_farmsamplings,
            n_housing_DL = sum(lacthous=="DL"),
            per_housing_DL = n_housing_DL/n_farmsamplings,
            n_bedmat_NS = sum(bedmat=="NS"), #number farm samplings per bedding material
            per_bedmat_NS = n_bedmat_NS/n_farmsamplings,
            n_bedmat_RS = sum(bedmat=="RS"),
            per_bedmat_RS = n_bedmat_RS/n_farmsamplings,
            n_bedmat_Ls = sum(bedmat=="Limestone"),
            perbedmat_RS = n_bedmat_Ls/n_farmsamplings,
            n_bedmat_Sh = sum(bedmat=="Sh"),
            perbedmat_Sh = n_bedmat_Sh/n_farmsamplings,
            n_bedmat_MS = sum(bedmat=="MS"),
            perbedmat_MS = n_bedmat_MS/n_farmsamplings,
            n_bedmat_St = sum(bedmat=="St"),
            perbedmat_St = n_bedmat_St/n_farmsamplings,
            n_bedmat_OO = sum(bedmat=="Other"),
            perbedmat_OO = n_bedmat_OO/n_farmsamplings)

table2_all <- data_combined %>%
  summarize(n_farms = n_distinct(FarmID),
            n_farmsamplings = n(),
            herdsize_mean = mean(herd.size),
            kg_milkprod_mean = mean(milk.cow.year)*0.453592, #convert milk.cow.year from lbs to kg
            kg_milkprod_sd = sd(milk.cow.year)*0.453592, 
            SCC_gm_mean = exp(mean(log(aveSCC), na.rm = TRUE)), #geometric mean
            n_housing_FS = sum(lacthous=="FS"), #number farm samplings per housing type
            per_housing_FS = n_housing_FS/n_farmsamplings,
            n_housing_TS = sum(lacthous=="TS"),
            per_housing_TS = n_housing_TS/n_farmsamplings,
            n_housing_BP = sum(lacthous=="B"),
            per_housing_BP = n_housing_BP/n_farmsamplings,
            n_housing_DL = sum(lacthous=="DL"),
            per_housing_DL = n_housing_DL/n_farmsamplings,
            n_bedmat_NS = sum(bedmat=="NS"), #number farm samplings per bedding material
            per_bedmat_NS = n_bedmat_NS/n_farmsamplings,
            n_bedmat_RS = sum(bedmat=="RS"),
            per_bedmat_RS = n_bedmat_RS/n_farmsamplings,
            n_bedmat_Ls = sum(bedmat=="Limestone"),
            perbedmat_RS = n_bedmat_Ls/n_farmsamplings,
            n_bedmat_Sh = sum(bedmat=="Sh"),
            perbedmat_Sh = n_bedmat_Sh/n_farmsamplings,
            n_bedmat_MS = sum(bedmat=="MS"),
            perbedmat_MS = n_bedmat_MS/n_farmsamplings,
            n_bedmat_St = sum(bedmat=="St"),
            perbedmat_St = n_bedmat_St/n_farmsamplings,
            n_bedmat_OO = sum(bedmat=="Other"),
            perbedmat_OO = n_bedmat_OO/n_farmsamplings)

##Additional summary
#all
summarize_all <- data_combined %>%
  summarize(n_farmsamplings = n(),
            herdsize_mean = mean(herd.size),
            herdsize_min = min(herd.size),
            herdsize_max = max(herd.size),
            herdsize_median = median(herd.size),
            perbreed_H = sum(Breed=="H")/n_farmsamplings,
            perbreed_JorBS = sum(Breed=="J"|Breed=="BS")/n_farmsamplings,
            perbreed_NOT_HorJorBS = sum(Breed!="H"&Breed!="J"&Breed!="BS")/n_farmsamplings,
            nolactcows_mean = mean(nolactcows),
            nolactcows_min = min(nolactcows),
            nolactcows_max = max(nolactcows),
            nolactcows_median = median(nolactcows),
            kg_milkprod_mean = mean(milk.cow.year)*0.453592, #convert milk.cow.year from lbs to kg
            kg_milkprod_sd = sd(milk.cow.year)*0.453592, 
            kg_milkprod_min = min(milk.cow.year)*0.453592,
            kg_milkprod_max = max(milk.cow.year)*0.453592,
            permilksys_Parlor = sum(milksys=="Parlor")/n_farmsamplings,
            permilksys_TS = sum(milksys=="Tie stall")/n_farmsamplings,
            permilksched_2 = sum(milksched=="2")/n_farmsamplings,
            permilksched_3 = sum(milksched=="3")/n_farmsamplings,
            permilksched_4 = sum(milksched=="4")/n_farmsamplings,
            per_clnmas_mean = mean(per_clnmas),
            per_clnmas_sd = sd(per_clnmas),
            per_clnmas_min = min(per_clnmas),
            per_clnmas_max = max(per_clnmas),
            SCC_gm_mean = exp(mean(log(aveSCC), na.rm = TRUE))*1000, #geometric mean, x1000 to get cells/mL
            SCC_min = min(aveSCC, na.rm = TRUE)*1000,
            SCC_max = max(aveSCC, na.rm = TRUE)*1000)

#bedding material by housing
summarize_byHousing <- data_combined %>%
  group_by(lacthous) %>%
  summarize(n_farmsamplings = n(),
            n_typeOR = sum(type=="OR"),
            per_typeOR = n_typeOR/n_farmsamplings,
            n_typeIOR = sum(type=="IOR"),
            per_typeIOR = n_typeIOR/n_farmsamplings,
            n_bedmat_NS = sum(bedmat=="NS"), #number farm samplings per bedding material
            per_bedmat_NS = n_bedmat_NS/n_farmsamplings,
            n_bedmat_RS = sum(bedmat=="RS"),
            per_bedmat_RS = n_bedmat_RS/n_farmsamplings,
            n_bedmat_Ls = sum(bedmat=="Limestone"),
            perbedmat_RS = n_bedmat_Ls/n_farmsamplings,
            n_bedmat_Sh = sum(bedmat=="Sh"),
            perbedmat_Sh = n_bedmat_Sh/n_farmsamplings,
            n_bedmat_MS = sum(bedmat=="MS"),
            perbedmat_MS = n_bedmat_MS/n_farmsamplings,
            n_bedmat_St = sum(bedmat=="St"),
            perbedmat_St = n_bedmat_St/n_farmsamplings,
            n_bedmat_OO = sum(bedmat=="Other"),
            perbedmat_OO = n_bedmat_OO/n_farmsamplings)
            
##MSC and TSC summary
#For the summary calculations, we will set zeros to half the limit of detection
data_msc <- data_msc %>%
  mutate(NBED_log_density = log10(NBED_both_plates/NBED_both_volumes)) %>%
  mutate(UBED_log_density = log10(UBED_both_plates/UBED_both_volumes)) %>%
  mutate(BTM_log_density = log10(BTM_both_plates/BTM_both_volumes))

data_tsc <- data_tsc %>%
  mutate(NBED_log_density = log10(NBED_both_plates/NBED_both_volumes)) %>%
  mutate(UBED_log_density = log10(UBED_both_plates/UBED_both_volumes)) %>%
  mutate(BTM_log_density = log10(BTM_both_plates/BTM_both_volumes))

data_msc$NBED_log_density <- 
  ifelse(data_msc$NBED_log_density == "-Inf", 1.698970, data_msc$NBED_log_density)
data_msc$BTM_log_density <- 
  ifelse(data_msc$BTM_log_density == "-Inf", -0.602060, data_msc$BTM_log_density)

data_tsc$NBED_log_density <- 
  ifelse(data_tsc$NBED_log_density == "-Inf", 1.698970, data_tsc$NBED_log_density)
data_tsc$BTM_log_density <- 
  ifelse(data_tsc$BTM_log_density == "-Inf", -0.602060, data_tsc$BTM_log_density)

#How many spore counts less than detection limit?
data_msc_ND <- data_msc %>%
  summarise(n = n(),
            ND_NBED_log_density = sum(NBED_log_density==1.698970),
            perND_NBED_log_density = ND_NBED_log_density/n,
            ND_UBED_log_density = sum(UBED_log_density==1.698970),
            perND_UBED_log_density = ND_UBED_log_density/n,
            ND_BTM_log_density = sum(BTM_log_density==-0.602060),
            perND_BTM_log_density = ND_BTM_log_density/n)

data_tsc_ND <- data_tsc %>%
  summarise(n = n(),
            ND_NBED_log_density = sum(NBED_log_density==1.698970),
            perND_NBED_log_density = ND_NBED_log_density/n,
            ND_UBED_log_density = sum(UBED_log_density==1.698970),
            perND_UBED_log_density = ND_UBED_log_density/n,
            ND_BTM_log_density = sum(BTM_log_density==-0.602060),
            perND_BTM_log_density = ND_BTM_log_density/n)

##Calculations for MSC and TSC summary section
#What are the mean, sd, min, and max log10/mL or g for nbed, ubed, and btm?
summary_msc <- data_msc %>%
  summarise(mean_NBED_log_density = mean(NBED_log_density),
            sd_NBED_log_density = sd(NBED_log_density),
            min_NBED_log_density = min(NBED_log_density),
            max_NBED_log_density = max(NBED_log_density),
            mean_UBED_log_density = mean(UBED_log_density),
            sd_UBED_log_density = sd(UBED_log_density),
            min_UBED_log_density = min(UBED_log_density),
            max_UBED_log_density = max(UBED_log_density),
            mean_BTM_log_density = mean(BTM_log_density),
            sd_BTM_log_density = sd(BTM_log_density),
            min_BTM_log_density = min(BTM_log_density),
            max_BTM_log_density = max(BTM_log_density))

summary_tsc <- data_tsc %>%
  summarise(mean_NBED_log_density = mean(NBED_log_density),
            sd_NBED_log_density = sd(NBED_log_density),
            min_NBED_log_density = min(NBED_log_density),
            max_NBED_log_density = max(NBED_log_density),
            mean_UBED_log_density = mean(UBED_log_density),
            sd_UBED_log_density = sd(UBED_log_density),
            min_UBED_log_density = min(UBED_log_density),
            max_UBED_log_density = max(UBED_log_density),
            mean_BTM_log_density = mean(BTM_log_density),
            sd_BTM_log_density = sd(BTM_log_density),
            min_BTM_log_density = min(BTM_log_density),
            max_BTM_log_density = max(BTM_log_density))

#What are the mean, sd, min, and max log10/mL or g for nbed, ubed, and btm by type?
summary_msc_type <- data_msc %>%
  group_by(type) %>%
  summarise(mean_NBED_log_density = mean(NBED_log_density),
            sd_NBED_log_density = sd(NBED_log_density),
            min_NBED_log_density = min(NBED_log_density),
            max_NBED_log_density = max(NBED_log_density),
            mean_UBED_log_density = mean(UBED_log_density),
            sd_UBED_log_density = sd(UBED_log_density),
            min_UBED_log_density = min(UBED_log_density),
            max_UBED_log_density = max(UBED_log_density),
            mean_BTM_log_density = mean(BTM_log_density),
            sd_BTM_log_density = sd(BTM_log_density),
            min_BTM_log_density = min(BTM_log_density),
            max_BTM_log_density = max(BTM_log_density))

summary_tsc_type <- data_tsc %>%
  group_by(type) %>%
  summarise(mean_NBED_log_density = mean(NBED_log_density),
            sd_NBED_log_density = sd(NBED_log_density),
            min_NBED_log_density = min(NBED_log_density),
            max_NBED_log_density = max(NBED_log_density),
            mean_UBED_log_density = mean(UBED_log_density),
            sd_UBED_log_density = sd(UBED_log_density),
            min_UBED_log_density = min(UBED_log_density),
            max_UBED_log_density = max(UBED_log_density),
            mean_BTM_log_density = mean(BTM_log_density),
            sd_BTM_log_density = sd(BTM_log_density),
            min_BTM_log_density = min(BTM_log_density),
            max_BTM_log_density = max(BTM_log_density))

#What are the mean, sd, min, and max log10/mL or g for nbed, ubed, and btm by bedmat?
summary_msc_bedmat <- data_msc %>%
  group_by(type,bedmat) %>%
  summarise(mean_NBED_log_density = mean(NBED_log_density),
            sd_NBED_log_density = sd(NBED_log_density),
            min_NBED_log_density = min(NBED_log_density),
            max_NBED_log_density = max(NBED_log_density),
            mean_UBED_log_density = mean(UBED_log_density),
            sd_UBED_log_density = sd(UBED_log_density),
            min_UBED_log_density = min(UBED_log_density),
            max_UBED_log_density = max(UBED_log_density),
            mean_BTM_log_density = mean(BTM_log_density),
            sd_BTM_log_density = sd(BTM_log_density),
            min_BTM_log_density = min(BTM_log_density),
            max_BTM_log_density = max(BTM_log_density))

summary_tsc_bedmat <- data_tsc %>%
  group_by(type,bedmat) %>%
  summarise(mean_NBED_log_density = mean(NBED_log_density),
            sd_NBED_log_density = sd(NBED_log_density),
            min_NBED_log_density = min(NBED_log_density),
            max_NBED_log_density = max(NBED_log_density),
            mean_UBED_log_density = mean(UBED_log_density),
            sd_UBED_log_density = sd(UBED_log_density),
            min_UBED_log_density = min(UBED_log_density),
            max_UBED_log_density = max(UBED_log_density),
            mean_BTM_log_density = mean(BTM_log_density),
            sd_BTM_log_density = sd(BTM_log_density),
            min_BTM_log_density = min(BTM_log_density),
            max_BTM_log_density = max(BTM_log_density))

#For table 3, summarize number of farms and samplings, spore counts for nbed, ubed, and btm
table3_msc <- data_msc %>%
  group_by(type,bedmat) %>%
  summarize(n = length(NBED_log_density),
            no.farms.represented = n_distinct(FarmID),
            NBED_mean = mean(NBED_log_density),
            NBED_sd = sd(NBED_log_density),
            UBED_mean = mean(UBED_log_density),
            UBED_sd = sd(UBED_log_density)) 

table3_tsc <- data_tsc %>%
  group_by(type,bedmat) %>%
  summarize(n = length(NBED_log_density),
            no.farms.represented = n_distinct(FarmID),
            NBED_mean = mean(NBED_log_density),
            NBED_sd = sd(NBED_log_density),
            UBED_mean = mean(UBED_log_density),
            UBED_sd = sd(UBED_log_density)) 

#summarize spore counts by bedding type for nbed, ubed, and btm
table3_mscT <- data_msc %>%
  group_by(type) %>%
  summarize(n = length(NBED_log_density),
            no.farms.represented = n_distinct(FarmID),
            NBED_mean = mean(NBED_log_density),
            NBED_sd = sd(NBED_log_density),
            UBED_mean = mean(UBED_log_density),
            UBED_sd = sd(UBED_log_density)) 

table3_tscT <- data_tsc %>%
  group_by(type) %>%
  summarize(n = length(NBED_log_density),
            no.farms.represented = n_distinct(FarmID),
            NBED_mean = mean(NBED_log_density),
            NBED_sd = sd(NBED_log_density),
            UBED_mean = mean(UBED_log_density),
            UBED_sd = sd(UBED_log_density)) 

##For supplemental figures 1 and 2, count distributions
library(ggplot2)
# 
p_msc <- ggplot(data_msc) +
  geom_histogram(aes(x = BTM_log_density),
                 binwidth = .075, fill = "grey", color = "black") +
  labs(x = "Bulk tank raw milk MSC (log10 CFU/mL)", y = "Count") +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12)) +
  scale_x_continuous(breaks = pretty(data_msc$BTM_log_density, n = 5)) 

p_tsc <- ggplot(data_tsc) +
  geom_histogram(aes(x = BTM_log_density),
                 binwidth = .075, fill = "grey", color = "black") +
  labs(x = "Bulk tank raw milk TSC (log10 CFU/mL)", y = "Count") +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=12)) +
  scale_x_continuous(breaks = pretty(data_msc$BTM_log_density, n = 5)) 
