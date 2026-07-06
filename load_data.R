
rm(list = ls())
library(openxlsx)
library(tidyverse)
library(countrycode)

#source("https://raw.githubusercontent.com/lambwf/Codebase/main/load_edgar_v10.R")
source("../Codebase/load_gcb_countries_v2025.R")
source("../Codebase/load_edgar_v2026.R")

## load EDGAR data (https://edgar.jrc.ec.europa.eu/dataset_ghg80)

edgar_co2 <- read.xlsx("sources/not public/IEA_EDGAR_CO2_1970_2025.xlsx",sheet="IPCC 2006",startRow=10)
edgar_ch4 <- read.xlsx("sources/not public/EDGAR_CH4_1970_2025.xlsx",sheet="IPCC 2006",startRow = 10)
edgar_n2o <- read.xlsx("sources/not public/EDGAR_N2O_1970_2025.xlsx",sheet="IPCC 2006",startRow = 10)
edgar_fgas <- read.xlsx("sources/not public/EDGAR_F-Gases_1970_2025.xlsx",sheet="IPCC 2006",startRow = 10)
data_ghg <- load_edgar(edgar_co2,edgar_ch4,edgar_n2o,edgar_fgas,2025)


## join GWPs and calculate CO2e

gwps <- read.csv("https://raw.githubusercontent.com/openclimatedata/globalwarmingpotentials/main/globalwarmingpotentials.csv",skip = 10)

data_ghg$gas <- gsub("-","",data_ghg$gas)
data_ghg <- left_join(data_ghg,gwps %>% select(gas=Species,AR6GWP100)) %>%
  mutate(AR6GWP100=ifelse(gas=="CO2",1,AR6GWP100))

## remove biogenic CO2

#data_ghg <- data_ghg %>%
#  filter(gas!="CO2bio")


## check all gwps joined

check <- data_ghg %>% filter(is.na(AR6GWP100))
check <- data_ghg %>% select(gas,AR6GWP100) %>% distinct()

data_ghg <- data_ghg %>% mutate(value_gwp=value*AR6GWP100)


list_fgases <- data_ghg %>%
  select(gas) %>%
  distinct() %>%
  mutate(group=ifelse(gas=="CO2","CO2 Fossil",NA)) %>%
  mutate(group=ifelse(gas=="CH4","CH4",group)) %>%
  mutate(group=ifelse(gas=="N2O","N2O",group)) %>%
  mutate(group=ifelse(is.na(group),"F-gases",group))

data_ghg <- left_join(data_ghg,list_fgases,by="gas")
data_ghg <- data_ghg %>%
  mutate(gas=group) %>%
  group_by(iso,country,code,code_description,gas,year) %>%
  summarise(value=sum(value_gwp,na.rm=TRUE))

## Gg to tons

data_ghg$value = data_ghg$value*1000



########## Inventory Land use change data

# JRC-NGHGI data (https://forest-observatory.ec.europa.eu/carbon)

data_inv_luc <- read.csv("sources/timeseries_NGHGI_v3.1.csv") %>% 
  filter(Category=="LULUCF") %>% 
  group_by(ISO3,Year) %>% 
  summarise(value=sum(CFluxes_yr,na.rm=T)) %>% 
  mutate(sector_lv1="Land use change (LULUCF)") %>% 
  mutate(sector_lv2="Land use change (LULUCF)") %>% 
  mutate(sector_lv2_order=8) %>% 
  select(iso=ISO3,sector_lv1,sector_lv2,sector_lv2_order,year=Year,value)


# get pre-2000 data where available

# cc_EU <- read.csv("https://raw.githubusercontent.com/openclimatedata/countrygroups/main/data/european-union.csv") %>% 
#   select(iso=Code) %>% 
#   mutate(EU="1")

load("../Tidy-GHG-Inventories/data/data_crts_v2.1.RData")

data_crts <- data_crts %>% 
  filter(sector_lv1=="Land use, land-use change and forestry") %>% 
  filter(gas=="co2") %>% 
  group_by(iso,year) %>% 
  summarise(value=sum(value,na.rm=T)/1e6) %>% 
  mutate(sector_lv1="Land use change (LULUCF)") %>% 
  mutate(sector_lv2="Land use change (LULUCF)") %>% 
  mutate(sector_lv2_order=8) %>% 
  filter(year<2000)

data_inv_luc <- data_inv_luc %>% 
  rbind(.,data_crts)

data_inv_luc <- data_inv_luc %>% 
  arrange(iso,year)
  
save(data_inv_luc,file="data/data_inv_luc.RData")




### consistent names

newnames <- data_ghg %>% 
  ungroup() %>% 
  select(iso) %>% 
  distinct() %>% 
  mutate(newname=countrycode(iso,"iso3c","country.name")) %>% 
  mutate(newname=ifelse(iso=="AIR","Intl. Aviation",newname)) %>% 
  mutate(newname=ifelse(iso=="SEA","Intl. Shipping",newname)) %>% 
  mutate(newname=ifelse(iso=="SCG","Serbia and Montenegro",newname)) %>% 
  mutate(newname=ifelse(iso=="ANT","Netherlands Antilles",newname)) %>% 
  mutate(newname=ifelse(iso=="WLD","World",newname))

data_ghg <- left_join(data_ghg,newnames,by="iso") %>% 
  mutate(country=newname) %>% 
  select(-newname) %>% 
  filter(!is.na(iso))

## aggregate sectors

data_ghg <- left_join(data_ghg,read.xlsx("sources/cc_sectors.xlsx",sheet = "sectors"),by=join_by(code, code_description))
check <- data_ghg %>% 
  filter(is.na(sector_lv3))

data_ghg$gas <- as.factor(data_ghg$gas)
data_ghg$gas <- fct_relevel(data_ghg$gas,"CO2 Fossil","CH4","N2O","F-gases")

data_ghg$sector_lv1 <- as.factor(data_ghg$sector_lv1)
data_ghg$sector_lv1 <- fct_reorder(data_ghg$sector_lv1,data_ghg$sector_lv1_order)

data_ghg$sector_lv2 <- as.factor(data_ghg$sector_lv2)
data_ghg$sector_lv2 <- fct_reorder(data_ghg$sector_lv2,data_ghg$sector_lv2_order)

data_ghg$sector_lv2 <- as.factor(data_ghg$sector_lv2)
data_ghg$sector_lv2 <- fct_reorder(data_ghg$sector_lv2,data_ghg$sector_lv2_order)

data_ghg$sector_lv3 <- as.factor(data_ghg$sector_lv3)
data_ghg$sector_lv3 <- fct_reorder(data_ghg$sector_lv3,data_ghg$sector_lv3_order)

blarg <- data_ghg %>% filter(year==2023) %>% group_by(code,code_description,sector_lv1,sector_lv2,sector_lv3) %>% summarise(value=sum(value,na.rm=T))

data_edgar <- data_ghg %>% 
  group_by(across(c(-code,-code_description,-sector_lv2_codes,-sector_lv3_codes))) %>% 
  summarise(value=sum(value,na.rm=TRUE))

data_edgar$country <- gsub("Turkey","Türkiye",data_edgar$country)

save(data_edgar,file="data/data_edgar.RData")



##########



# Load Global Carbon Budget (GCB) CO2 land use change (LUC) emissions (https://globalcarbonbudget.org/carbonbudget2023/)

data_gcb_luc <- load_gcb_countries_luc(
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2025v0.2.xlsx',range="A8:GT183",sheet=2),
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2025v0.2.xlsx',range="A8:GT183",sheet=3),
  readxl::read_xlsx('sources/National_LandUseChange_Carbon_Emissions_2025v0.2.xlsx',range="A8:GT183",sheet=4))


data_gcb_luc <- data_gcb_luc %>% 
  #filter(year>=1970) %>% 
  #filter(country=="Global") %>% 
  select(iso,country,year,value=mean) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(value=value*1e9)


save(data_gcb_luc,file="data/data_gcb_luc.RData")


##########

# Global Carbon Project CO2 FFI (https://globalcarbonbudget.org/carbonbudget2023/)

data_gcb_co2_ffi <- load_gcb_countries_ffi(read.xlsx("sources/National_Fossil_Carbon_Emissions_2025_v0.3.xlsx",sheet=2,startRow = 12))

### consistent names

newnames <- data_gcb_co2_ffi %>% 
  ungroup() %>% 
  select(iso) %>% 
  distinct() %>% 
  mutate(newname=countrycode(iso,"iso3c","country.name"))

data_gcb_co2_ffi <- data_gcb_co2_ffi %>% 
  left_join(.,newnames,by="iso") %>% 
  mutate(country=newname) %>% 
  select(-newname)

save(data_gcb_co2_ffi,file="data/data_gcb_co2_ffi.RData")


########

# population data from WDI


## Get population data from the World Bank

download.file("https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv",
              destfile = paste0("sources/WDI_indicators_population.zip"), mode="wb")

files <- unzip("sources/WDI_indicators_population.zip", exdir="sources")
file.rename(files[!grepl("Metadata", files)], paste0("sources/WDI_indicators_population_",format(Sys.Date(), "%Y-%m-%d"),".csv"))
file.remove("sources/WDI_indicators_population.zip")
file.remove(setdiff(files, files[!grepl("Metadata", files)]))


data_wdi_pop <- read.csv(paste0("sources/WDI_indicators_population_",format(Sys.Date(), "%Y-%m-%d"),".csv"),skip=3)
data_wdi_pop <- data_wdi_pop %>% 
  select(-X) %>% 
  gather(.,year,value,X1960:X2025)
data_wdi_pop$year <- gsub("X","",data_wdi_pop$year)
data_wdi_pop <- data_wdi_pop %>% 
  select(iso=Country.Code,year,population=value) %>% 
  mutate(year=as.numeric(year))

save(data_wdi_pop,file="data/data_wdi_pop.RData")


######## CO2 emissions from IEA, rescaled to EDGAR

## Get data, clean

data_iea <- read.xlsx("../Databases/IEA/IEA_world_ghg_emissions_v2020.xlsx")
data_iea <- data_iea %>% gather(.,year,value,`1960`:`2021`)
names(data_iea) <- c("flow","fuel","gas","year","value")
data_iea <- data_iea %>% 
  mutate(value=as.numeric(value),year=as.numeric(year)) %>% 
  mutate(gas = str_squish(gas)) %>% 
  mutate(flow = str_squish(flow)) %>% 
  mutate(gas = ifelse(gas=="Carbon Dioxide","CO2 Fossil",gas)) %>% 
  mutate(gas = ifelse(gas=="Methane","CH4",gas)) %>% 
  mutate(gas = ifelse(gas=="Nitrous Oxide","N2O",gas)) %>% 
  filter(!is.na(value),year>=1990,gas %in% c("CO2 Fossil","CH4","N2O"))


## convert GWP100 AR5 (IEA) to GWP100 AR6

data_iea <- data_iea %>% 
  mutate(value=ifelse(gas=="CH4",(value/28)*27.90,value)) %>% 
  mutate(value=ifelse(gas=="N2O",(value/265)*273.00,value))


## Map IEA sectors to tidy sectors

data_iea <- data_iea %>% 
  mutate(
    sector_lv2 = case_when(
      
      flow %in% c("Electricity and heat production","Other energy industry own use") ~ "Energy | Power",
      flow %in% c("Manufacturing industries and construction") ~ "Energy | Industry",
      flow %in% c("Transport","Memo: International marine bunkers","Memo: International aviation bunkers") ~ "Energy | Transport",
      flow %in% c("Residential","Commercial and public services","Agriculture/forestry","Fishing","Final consumption not elsewhere specified") ~ "Energy | Buildings & other",

      TRUE ~ NA_character_
    )
  )



## check totals

data_iea_totals <- data_iea %>% 
  filter(!is.na(sector_lv2),fuel=="Coal, peat and oil shale") %>% 
  group_by(year,sector_lv2,flow,fuel) %>% 
  summarise(value=sum(value)) %>% 
  left_join(.,data_iea %>% filter(flow=="GHG from fuel combustion",fuel=="Coal, peat and oil shale") %>% group_by(year,fuel) %>% 
              summarise(value_total=sum(value)))%>% 
  group_by(year) %>% 
  mutate(test_total = sum(value[!grepl("Memo", flow)], na.rm = TRUE))


## Filter IEA to tidy sectors, remove totals

data_iea <- data_iea %>% 
  filter(!is.na(sector_lv2),fuel!="Total") %>% 
  filter(fuel!="Memo: biofuels and renewable wastes") %>% 
  select(-flow,-gas) %>% 
  group_by(year,sector_lv2,fuel) %>% 
  summarise(value=sum(value))


## Join to EDGAR

data_fuels <- data_iea %>% 
  group_by(year,sector_lv2) %>% 
  mutate(totals_iea=sum(value)*1000) %>% 
  left_join(.,data_edgar %>%
              filter(sector_lv1=="Energy",sector_lv2!="Energy | Fuel production") %>% 
              group_by(year,sector_lv2,sector_lv2_order) %>% 
              summarise(totals_edgar=sum(value,na.rm=T)))


## rescale emissions by fuel to match the EDGAR totals

data_fuels <- data_fuels %>% 
  mutate(diff_rel=totals_edgar/totals_iea) %>% 
  mutate(value_trans=value*diff_rel)


## Create single aggregate dataset with fuels in energy sector

data_fuels <- data_fuels %>% mutate(value=value_trans) %>% 
  select(year,sector_lv2,sector_lv2_order,fuel,value) %>% 
  mutate(value=value*1000)

data_fuels <- data_fuels %>% 
  rbind(.,data_edgar %>% 
          filter(!sector_lv2 %in% c("Energy | Power","Energy | Industry","Energy | Transport","Energy | Buildings & other")) %>% 
          filter(year>=1990) %>% 
          group_by(year,sector_lv2,sector_lv2_order,sector_lv3,gas) %>% 
          mutate(gas=as.character(gas)) %>% 
          summarise(value=sum(value)))


## Classify all fossil / non-fossil sources

data_fuels <- data_fuels %>% 
  ungroup() %>% 
  mutate(gas=ifelse(is.na(gas),"GHG",gas)) %>% 
  mutate(fuel=ifelse(gas=="F-gases","Non-fossil",fuel)) %>% 
  mutate(fuel=ifelse(sector_lv3=="Chemicals" & is.na(fuel),"Other fossil",fuel)) %>% 
  mutate(fuel=ifelse(sector_lv3=="Metals" & is.na(fuel),"Other fossil",fuel)) %>% 
  mutate(fuel=ifelse(sector_lv3=="Cement (excl. carbonation)" & is.na(fuel),"Non-fossil",fuel)) %>% 
  mutate(fuel=ifelse(sector_lv3=="Other (Industrial processes)" & is.na(fuel),"Other fossil",fuel)) %>%
  mutate(fuel=ifelse(sector_lv3=="Other (Indirect N2O and fossil fuel fires)" & is.na(fuel),"Other fossil",fuel)) %>%
  mutate(fuel=ifelse(fuel=="Coal, peat and oil shale","Coal",fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Natural gas","Gas",fuel)) %>% 
  mutate(fuel=ifelse(fuel=="Other","Other fossil",fuel)) %>% 
  mutate(fuel=ifelse(sector_lv3=="Solid fuels" & is.na(fuel),"Coal",fuel)) %>% 
  mutate(fuel=ifelse(sector_lv3=="Oil and gas" & is.na(fuel),"Other fossil",fuel)) %>% 
  mutate(fuel=ifelse(sector_lv3=="Other (Fuel production)" & is.na(fuel),"Other fossil",fuel)) %>% 
  mutate(fuel=ifelse(is.na(fuel),"Non-fossil",fuel))


data_fuels <- data_fuels %>% 
  group_by(year,sector_lv2_order,sector_lv2,fuel) %>% 
  summarise(value=sum(value))


save(data_fuels,file="data/data_fuels.RData")
