
## Aggregations

cc_EU <- read.csv("https://raw.githubusercontent.com/openclimatedata/countrygroups/main/data/european-union.csv") %>% 
  select(iso=Code) %>% 
  mutate(EU="1")

cc_LDC <- read.csv("https://raw.githubusercontent.com/openclimatedata/countrygroups/refs/heads/main/data/ldc.csv") %>% 
  select(iso=Code) %>% 
  mutate(LDC=1)

cc_G20<- read.csv("https://raw.githubusercontent.com/openclimatedata/countrygroups/refs/heads/main/data/g20.csv") %>% 
  select(iso=Code) %>% 
  add_row(iso="EU27") %>% 
  mutate(G20=1)


## References

label_unep="UNEP Emissions Gap Report 2026"
label_edgar <- "EDGARv2026"
label_gcb <- "GCB 2026"
label_grassi <-  "JRC-NGHGI v3.1.1"
label_wb <- "World Bank 2026"
label_tidyinv <- "Tidy GHG Inventories"
label_iea <- "IEA 2026"

ref_edgar <- "EDGARv2026 (https://edgar.jrc.ec.europa.eu/report_2026)"
ref_gcb <- "GCB 2025 (https://globalcarbonbudget.org/gcb-2025/)"
ref_lamb <- "https://lambwf.github.io/UNEP-Gap-Report-2026-Chapter-2/"
ref_grassi <-  "JRC-NGHGI v3.1 (https://zenodo.org/records/18352395)"
ref_wb <- "World Bank 2026 (https://databank.worldbank.org/source/population-estimates-and-projections)"
ref_tidyinv <- "Tidy GHG Inventories (https://lambwf.github.io/Tidy-GHG-Inventories/)"
ref_iea <- "IEA 2026 (https://www.iea.org/data-and-statistics/data-product/greenhouse-gas-emissions-from-energy)"


## GCB projection (Friedlingstein et al. 2025 table 7)

#gcb_luc_projection <- 1.1*3.664
gcb_luc_projection <- 4.1

## max year

max_year=2025


## colours

colours_top6 <- c("Russian Federation" = "#ff9055ff", 
                  "United States of America" = "#659cccff",
                  "China" = "#e5b82cff",
                  "European Union" = "#17becfff",
                  "Brazil" = "#7dd396ff",
                  "Indonesia" = "#7dd396ff",
                  "India" = "#d45087ff",
                  "Intl. Transport" = "#818181ff")


region_colours <- c("Africa"="#d45087ff",
                    "Developed Countries"="#659cccff",
                    "Asia and developing Pacific"="#e5b82cff",
                    "E. Europe & W. Asia"="#ff9055ff",
                    "Latin America and Caribbean"="#7dd396ff",
                    "Middle East"="#17becfff")


## High emitters

load("data/data_edgar.RData")
high_emitters <- data_edgar %>% 
  group_by(iso,country,year) %>% 
  summarise(value=sum(value)) %>% 
  filter(year==max_year) %>% 
  arrange(desc(value)) %>% 
  head(5)

high_emitters <- c(high_emitters$country, "European Union","Intl. Transport")

