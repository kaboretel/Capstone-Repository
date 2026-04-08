# **Title: Capstone Project: From Service Coverage to Mortality Reduction: Predictive & spatial analysis of maternal health in Malawi**

## **installing packages**

install.packages(c("tidyverse","haven","sf","WDI"))


## **Load libraries**

library(tidyverse)
library(haven)
library(sf)
library(WDI)

## **I. Data Ingestion, Cleaning and Wrangling.**

## **Define your RAW data folder path (relative to project root**

raw_dir <- "1_Data/1_Raw"

## **check you are in the right place (project root)**

getwd()
list.files(raw_dir)

## **Load DHS data sets (Stata .dta / .DTA)**

dhs15_ir <- read_dta(file.path(raw_dir, "Dhs_15_ir.DTA"))
dhs15_br <- read_dta(file.path(raw_dir, "Dhs_15_br.DTA"))

dhs24_ir <- read_dta(file.path(raw_dir, "Dhs_24_ir.dta"))
dhs24_br <- read_dta(file.path(raw_dir, "Dhs_24_br.dta"))

## **Load GPS / spatial files (shape files)**

gps15 <- st_read(file.path(raw_dir, "Dhs_15_gps.shp"))
gps24 <- st_read(file.path(raw_dir, "Dhs_24_gps.shp"))

## **Quick verification**
glimpse(dhs15_ir)
glimpse(dhs24_ir)

nrow(dhs15_ir); nrow(dhs15_br)
nrow(dhs24_ir); nrow(dhs24_br)

st_geometry_type(gps15)
st_geometry_type(gps24)

st_crs(gps15)
st_crs(gps24)

## **Load WDI (CSV file)**

wdi_meta <- read_csv(file.path(raw_dir, "wdi_meta.csv"))
glimpse(wdi_meta)

## **Creating output folders**

out_dir <- "3_Output"
fig_dir <- file.path(out_dir, "figures")
ppt_dir <- file.path(out_dir, "ppt")

dir.create(out_dir, showWarnings = FALSE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(ppt_dir, showWarnings = FALSE, recursive = TRUE)

## **Creating a district “key” cleaner** 

clean_district_key <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    # Fix known DHS label inconsistencies
    stringr::str_replace_all("nkhatabay", "nkhata bay") %>%
    stringr::str_replace_all("nkhota kota", "nkhotakota") %>%
    stringr::str_replace_all("mulange", "mulanje") %>%
    stringr::str_replace_all("chradzulu", "chiradzulu") %>%
    stringr::str_replace_all("ndanje", "nsanje") %>%
    # Handle special 2024 category
    stringr::str_replace_all("dowa \\(camps\\)", "dowa") %>%
    # Optional: if your boundary file uses no "rural" suffix
    stringr::str_replace_all("\\s+rural$", "")
}

## **Building cleaned DHS district fields + weights (2015 & 2024 IR)**
## **2015 (district variable = sdist)**

dhs15_ir_clean <- dhs15_ir %>%
  mutate(
    district_name_raw = haven::as_factor(sdist),
    district_key = clean_district_key(district_name_raw),
    wt = v005 / 1000000
  )

## **2024 (district variable = sdistrict)**

dhs24_ir_clean <- dhs24_ir %>%
  mutate(
    district_name_raw = haven::as_factor(sdistrict),
    district_key = clean_district_key(district_name_raw),
    wt = v005 / 1000000
  )

## **Quick check: do we have 28 districts?**

dhs15_ir_clean %>% distinct(district_key) %>% arrange(district_key)
dhs24_ir_clean %>% distinct(district_key) %>% arrange(district_key)

dhs15_ir_clean %>% count(district_key, sort = TRUE) %>% print(n = 50)
dhs24_ir_clean %>% count(district_key, sort = TRUE) %>% print(n = 50)


## **II. Exploratory Data Analysis and Visualization.**

## **II.1. Goal**

## The goal of this exploratory data analysis is to examine patterns, 
## trends, and inequities in maternal health service utilization 
## in Malawi using the cleaned DHS 2015–16 and 2024 datasets. 
## Specifically, this stage focuses on descriptive and geographic 
## comparisons of skilled birth attendance, antenatal care, and 
## postnatal care to identify who is being left behind and where 
## gaps persist.


## **II.2. Data description**

## This analysis uses the Malawi Demographic and Health Surveys (DHS) 2015–16 
## and 2024, which are nationally representative household surveys providing 
## data on maternal health service utilization, demographics, and socioeconomic 
## characteristics. The datasets have been cleaned, harmonized, and weighted to 
## ensure comparability across survey years and to produce representative 
## estimates.

## **II.3. Descriptive Statistics**

## This section summarizes key maternal health indicators and examines their 
## distribution across the population. All estimates are weighted using DHS 
## sampling weights to ensure national representativeness.

### **Create Weight Variable**

library(tidyverse)

# Create shorter working names
dhs_2015 <- dhs15_ir_clean
dhs_2024 <- dhs24_ir_clean

# Add DHS sampling weights
dhs_2015 <- dhs_2015 %>%
  mutate(weight = v005 / 1000000)

dhs_2024 <- dhs_2024 %>%
  mutate(weight = v005 / 1000000)

glimpse(dhs_2015)
glimpse(dhs_2024)


## The Malawi DHS 2015 and 2024 individual recode datasets were successfully 
## loaded and prepared for analaysis. Both datasets contain large nationally 
## representative (approximately 24,500 women in 2015 and 21,500 in 2024), 
## ensuring strong statistical power for maternal health analysis. A normalized 
## sampling weight variable (weight) was created from the DHS weight variable 
## (v005) to allow for representative, population-level estimates. Key variables 
## of interest, including antenatal care visits (m14) and education (v133), were 
## confirmed to be present in both datasets, enabling consistent comparisons 
## across survey years.


## ** Create ANC4+ Variable** 

dhs_2015 <- dhs_2015 %>%
  mutate(
    anc4plus = case_when(
      is.na(m14_1) ~ NA_real_,
      m14_1 >= 4 ~ 1,
      TRUE ~ 0
    )
  )

dhs_2024 <- dhs_2024 %>%
  mutate(
    anc4plus = case_when(
      is.na(m14_1) ~ NA_real_,
      m14_1 >= 4 ~ 1,
      TRUE ~ 0
    )
  )



## ** Create SBA variable**

dhs_2015 <- dhs_2015 %>%
  mutate(
    sba = case_when(
      m3a_1 == 1 |  # doctor
        m3b_1 == 1 |  # nurse/midwife
        m3c_1 == 1 ~ 1,
      TRUE ~ 0
    )
  )

dhs_2024 <- dhs_2024 %>%
  mutate(
    sba = case_when(
      m3a_1 == 1 |
        m3b_1 == 1 |
        m3c_1 == 1 ~ 1,
      TRUE ~ 0
    )
  )

## **Create PNC Variable**

dhs_2015 <- dhs_2015 %>%
  mutate(
    pnc = case_when(
      m70_1 <= 2 ~ 1,   # within 2 days
      m70_1 > 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

dhs_2024 <- dhs_2024 %>%
  mutate(
    pnc = case_when(
      m70_1 <= 2 ~ 1,
      m70_1 > 2 ~ 0,
      TRUE ~ NA_real_
    )
  )



## **Descriptive statistics for all indicators**

# 2015
dhs_2015 %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  )

# 2024
dhs_2024 %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  )

## Between 2015 and 2024, Malawi shows notable improvement in antenatal care 
## utilization, with the proportion of women receiving at least four ANC visits 
## increasing from 51.0% to 63.1%. This suggests progress in early and continued 
## engagement with maternal health services. However, skilled birth attendance 
## (SBA) declined significantly, dropping from 49.7% in 2015 to 33.0% in 2024, 
## indicating a concerning gap between antenatal care uptake and access to 
## skilled delivery services. Postnatal care (PNC) coverage remains consistently 
## high in both years (~99%), suggesting strong performance in post-delivery 
## follow-up, though this may also reflect measurement or reporting patterns 
## that warrant further examination. Overall, the findings highlight a 
## disconnect along the maternal care continuum, where improvements in antenatal 
## care do not translate into increased use of skilled delivery services.


## **II.4. Visualization**

# Loading packages
library(tidyverse)
library(scales)


## **II.4.1. Overall maternal health coverage: 2015 vs 2024**

overall_2015 <- dhs_2015 %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  ) %>%
  mutate(year = "2015")

overall_2024 <- dhs_2024 %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  ) %>%
  mutate(year = "2024")

overall_long <- bind_rows(overall_2015, overall_2024) %>%
  pivot_longer(cols = c(ANC4, SBA, PNC),
               names_to = "indicator",
               values_to = "coverage")

ggplot(overall_long, aes(x = indicator, y = coverage, fill = year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Maternal Health Service Coverage in Malawi, 2015 vs 2024",
    x = "Indicator",
    y = "Weighted Coverage"
  ) +
  theme_minimal()


## The figure shows that antenatal care (ANC4+) coverage increased between 2015 
##and 2024, while skilled birth attendance (SBA) declined substantially. 
## Postnatal care (PNC) remained consistently high in both years. These results 
## suggest improvements in antenatal care utilization but a concerning drop in 
## access to skilled delivery services.


## **II.4.2. ANC4+ by wealth quintile

anc_wealth_2015 <- dhs_2015 %>%
  group_by(v190) %>%
  summarise(coverage = weighted.mean(anc4plus, weight, na.rm = TRUE)) %>%
  mutate(year = "2015")

anc_wealth_2024 <- dhs_2024 %>%
  group_by(v190) %>%
  summarise(coverage = weighted.mean(anc4plus, weight, na.rm = TRUE)) %>%
  mutate(year = "2024")

anc_wealth <- bind_rows(anc_wealth_2015, anc_wealth_2024) %>%
  mutate(
    wealth = factor(v190,
                    levels = c(1,2,3,4,5),
                    labels = c("Poorest","Poorer","Middle","Richer","Richest"))
  )

ggplot(anc_wealth, aes(x = wealth, y = coverage, fill = year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "ANC 4+ Coverage by Wealth Quintile",
    x = "Wealth Quintile",
    y = "Weighted Coverage"
  ) +
  theme_minimal()

## ANC4+ coverage improved across all wealth groups, but clear inequalities 
## persist, with higher coverage among the richest compared to the poorest.

## **II.4.3. SBA by wealth quintile

sba_wealth_2015 <- dhs_2015 %>%
  group_by(v190) %>%
  summarise(coverage = weighted.mean(sba, weight, na.rm = TRUE)) %>%
  mutate(year = "2015")

sba_wealth_2024 <- dhs_2024 %>%
  group_by(v190) %>%
  summarise(coverage = weighted.mean(sba, weight, na.rm = TRUE)) %>%
  mutate(year = "2024")

sba_wealth <- bind_rows(sba_wealth_2015, sba_wealth_2024) %>%
  mutate(
    wealth = factor(v190,
                    levels = c(1,2,3,4,5),
                    labels = c("Poorest","Poorer","Middle","Richer","Richest"))
  )

ggplot(sba_wealth, aes(x = wealth, y = coverage, fill = year)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Skilled Birth Attendance by Wealth Quintile",
    x = "Wealth Quintile",
    y = "Weighted Coverage"
  ) +
  theme_minimal()

## The chart shows that skilled birth attendance declines in 2024 across all 
## wealth groups compared to 2015. Coverage remains higher among wealthier 
## women, highlighting persistent inequalities, but the overall decline suggests 
## worsening access to skilled delivery services across the population.


## **II.4.4.Maternal health indicators by education level**

edu_2015 <- dhs_2015 %>%
  group_by(v106) %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  ) %>%
  mutate(year = "2015")

edu_2024 <- dhs_2024 %>%
  group_by(v106) %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  ) %>%
  mutate(year = "2024")

edu_long <- bind_rows(edu_2015, edu_2024) %>%
  mutate(
    education = factor(v106,
                       levels = c(0,1,2,3),
                       labels = c("No education","Primary","Secondary","Higher"))
  ) %>%
  pivot_longer(cols = c(ANC4, SBA, PNC),
               names_to = "indicator",
               values_to = "coverage")

ggplot(edu_long, aes(x = education, y = coverage, fill = year)) +
  geom_col(position = "dodge") +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Maternal Health Coverage by Education Level",
    x = "Education Level",
    y = "Weighted Coverage"
  ) +
  theme_minimal()

## Higher education is associated with better maternal health service 
## utilization, with persistent disparities across all indicators.



## **II.4.5.Maternal health indicators by by urban/rural residence**

res_2015 <- dhs_2015 %>%
  group_by(v025) %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  ) %>%
  mutate(year = "2015")

res_2024 <- dhs_2024 %>%
  group_by(v025) %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  ) %>%
  mutate(year = "2024")

res_long <- bind_rows(res_2015, res_2024) %>%
  mutate(
    residence = factor(v025,
                       levels = c(1,2),
                       labels = c("Urban","Rural"))
  ) %>%
  pivot_longer(cols = c(ANC4, SBA, PNC),
               names_to = "indicator",
               values_to = "coverage")

ggplot(res_long, aes(x = residence, y = coverage, fill = year)) +
  geom_col(position = "dodge") +
  facet_wrap(~indicator) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Maternal Health Coverage by Residence",
    x = "Residence",
    y = "Weighted Coverage"
  ) +
  theme_minimal()

## Urban areas have higher coverage than rural areas, with persistent 
## disparities and a decline in skilled birth attendance in both settings.


## **II.4.6.Distribution of ANC visits**

ggplot(dhs_2024, aes(x = m14_1)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Distribution of Antenatal Care Visits, Malawi 2024",
    x = "Number of ANC Visits",
    y = "Count"
  ) +
  theme_minimal()


ggplot(dhs_2015, aes(x = m14_1)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Distribution of Antenatal Care Visits, Malawi 2015",
    x = "Number of ANC Visits",
    y = "Count"
  ) +
  theme_minimal()

## Most women have 4 or more ANC visits, but some still fall below recommended 
## levels.

## **II.4.7.Missing values for key indicators**

missing_df <- tibble(
  variable = c("ANC4+", "SBA", "PNC"),
  missing_2015 = c(
    sum(is.na(dhs_2015$anc4plus)),
    sum(is.na(dhs_2015$sba)),
    sum(is.na(dhs_2015$pnc))
  ),
  missing_2024 = c(
    sum(is.na(dhs_2024$anc4plus)),
    sum(is.na(dhs_2024$sba)),
    sum(is.na(dhs_2024$pnc))
  )
) %>%
  pivot_longer(cols = c(missing_2015, missing_2024),
               names_to = "year",
               values_to = "missing_count")

ggplot(missing_df, aes(x = variable, y = missing_count, fill = year)) +
  geom_col(position = "dodge") +
  labs(
    title = "Missing Values for Key Maternal Health Indicators",
    x = "Indicator",
    y = "Number of Missing Values"
  ) +
  theme_minimal()

## Missing data is higher in 2024 for ANC and PNC, while SBA remains largely 
## complete, indicating potential data quality concerns for some indicators.

## **II.4.8.District-level descriptive visualization**


district_sba_2024 <- dhs_2024 %>%
  group_by(district_key) %>%
  summarise(sba_rate = weighted.mean(sba, weight, na.rm = TRUE)) %>%
  arrange(sba_rate)

ggplot(district_sba_2024, aes(x = reorder(district_key, sba_rate), y = sba_rate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Skilled Birth Attendance by District, Malawi 2024",
    x = "District",
    y = "Weighted SBA Coverage"
  ) +
  theme_minimal()

## The chart shows substantial variation in skilled birth attendance across 
## districts in Malawi in 2024. Some districts have relatively higher coverage, 
## while others lag significantly behind, indicating geographic disparities in 
## access to skilled delivery services.


## **Conclusion**

## The exploratory data analysis reveals mixed progress in maternal health 
## service utilization in Malawi between 2015 and 2024. Antenatal care (ANC4+) 
## coverage improved over time, and postnatal care (PNC) remained consistently 
## high. However, skilled birth attendance (SBA) declined, indicating a critical 
## gap in the maternal care continuum.






























































# 2015
dhs_2015 %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  )

# 2024
dhs_2024 %>%
  summarise(
    ANC4 = weighted.mean(anc4plus, weight, na.rm = TRUE),
    SBA  = weighted.mean(sba, weight, na.rm = TRUE),
    PNC  = weighted.mean(pnc, weight, na.rm = TRUE)
  )






















# Weighted ANC 4+ summary
dhs_2015 %>%
  summarise(anc4plus_rate = weighted.mean(anc4plus, weight, na.rm = TRUE))

dhs_2024 %>%
  summarise(anc4plus_rate = weighted.mean(anc4plus, weight, na.rm = TRUE))













names(dhs_2015)[grepl("sba|skilled|pnc|post|anc", names(dhs_2015), ignore.case = TRUE)]



















## **4) Choosing ANC 4+ (m14) and Education (v133) years indicator for mapping (simple and usually present)

c("m14", "v133") %in% names(dhs15_ir_clean)
c("m14", "v133") %in% names(dhs24_ir_clean)

## ** Creating the variables**

dhs15_ir_clean <- dhs15_ir_clean %>%
  mutate(
    anc4plus = case_when(
      is.na(m14) ~ NA_real_,
      m14 >= 4 ~ 1,
      TRUE ~ 0
    ),
    eduyears = v133
  )

dhs24_ir_clean <- dhs24_ir_clean %>%
  mutate(
    anc4plus = case_when(
      is.na(m14) ~ NA_real_,
      m14 >= 4 ~ 1,
      TRUE ~ 0
    ),
    eduyears = v133
  )

## **Aggregating DHS to district level (weighted)**

# **2015**

dhs15_ir_clean <- dhs15_ir_clean %>%
  mutate(
    anc_visits = m14_1,
    anc4plus = if_else(anc_visits >= 4, 1,0,
                       missing = NA_real_),
    eduyears = v133
  )

agg15 <- dhs15_ir_clean %>%
  group_by(district_key) %>%
  summarise(
    n = n(),
    anc4plus_pct_2015 = 100 * weighted.mean(anc4plus, wt, na.rm = TRUE),
    edu_mean_2015 = weighted.mean(eduyears, wt, na.rm = TRUE),
    .groups = "drop"
  )

# **2024**

names(dhs24_ir_clean)[grepl("^m14",
names(dhs24_ir_clean))]

dhs24_ir_clean <- dhs24_ir_clean %>%
  mutate(
    anc_visits = m14_1,
    anc4plus = if_else(anc_visits >= 4, 1,0,
                       missing = NA_real_),
    eduyears = v133
  )

agg24 <- dhs24_ir_clean %>%
  group_by(district_key) %>%
  summarise(
    n = n(),
    anc4plus_pct_2024 = 100 * weighted.mean(anc4plus, wt, na.rm = TRUE),
    edu_mean_2024 = weighted.mean(eduyears, wt, na.rm = TRUE),
    .groups = "drop"
  )


nrow(agg15); nrow(agg24)

## **Get the correct district boundary file (ADM1) inside R**

install.packages("geodata")
library(geodata)

mwi_adm1 <- geodata::gadm(country = "MWI", level = 1, path = file.path(raw_dir, "GADM"))
mwi_adm1 <- sf::st_as_sf(mwi_adm1)

nrow(mwi_adm1)  # should be 28

mwi_adm1 <- mwi_adm1 %>%
  mutate(
    district_name = NAME_1,
    district_key = clean_district_key(district_name)
  )

## **Validating merge keys before joining (prevents silent mapping errors)**

shp_keys <- mwi_adm1 %>% st_drop_geometry() %>% distinct(district_key)

anti_join(agg24 %>% distinct(district_key), shp_keys, by = "district_key")  # DHS not in shapefile
anti_join(shp_keys, agg24 %>% distinct(district_key), by = "district_key")  # shapefile not in DHS


## **Merging district data to the shape file (left join)** 

map24 <- mwi_adm1 %>% left_join(agg24, by = "district_key")
map15 <- mwi_adm1 %>% left_join(agg15, by = "district_key")

nrow(map24); nrow(map15)  # should remain 28


library(ggplot2)
library(sf)  # Ensure spatial data is supported

# Assuming map24 is your 2024 data with a cleaned ANC percentage variable
map_2024 <- ggplot(map24) +
  geom_sf(aes(fill = anc4plus_pct_2024), color = "white", linewidth = 0.2) +
  labs(
    title = "Malawi DHS 2024: % of Women with 4+ ANC Visits",
    fill = "% ANC 4+",
    caption = "Weighted using v005/1,000,000. Boundaries: GADM ADM1."
  ) +
  theme_minimal()

# Display the map
map_2024


## **9) WDI data set cleaning**

library(tidyverse)
library(dplyr)


raw_dir <- "1_Data/1_Raw"
wdi_meta <- readr::read_csv(file.path(raw_dir, "wdi_meta.csv"))
nrow(wdi_meta)

names(wdi_meta)

wdi_mwi <- wdi_meta %>%
  dplyr::filter(!is.na(`Country Code`)) %>%
  dplyr::filter(`Country Code` == "MWI")

nrow(wdi_mwi)

wdi_mwi <- wdi_meta %>%
  dplyr::filter(`Country Code` == "MWI")

nrow(wdi_mwi)   # should be 20

## **Filtering the MMR indicator and pivot years into long format**

wdi_mmr <- wdi_mwi %>%
  dplyr::filter(`Series Code` == "SH.STA.MMRT") %>%
  tidyr::pivot_longer(
    cols = dplyr::matches("^\\d{4}"),
    names_to = "year",
    values_to = "mmr"
  ) %>%
  dplyr::mutate(
    year = as.integer(stringr::str_extract(year, "^\\d{4}")),
    mmr  = as.numeric(mmr)
  ) %>%
  dplyr::filter(!is.na(mmr))

nrow(wdi_mmr)
head(wdi_mmr)

install.packages("janitor")
library(janitor)

p2 <- ggplot(wdi_mmr, aes(x = year, y = mmr)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Malawi Maternal Mortality Trend (2000–2024)",
    x = "Year",
    y = "Deaths per 100,000 live births",
    caption = "Source: World Bank World Development Indicators"
  ) +
  theme_minimal()

p2



ggsave(
  file.path(fig_dir, "malawi_mmr_trend.png"),
  p2,
  width = 11,
  height = 6,
  dpi = 300
)

p1

ggsave(
  file.path(fig_dir, "malawi_district_map.png"),
  p1,
  width = 11,
  height = 6,
  dpi = 300
)


## **District improvement map showing change in maternal health service coverage between 2015 and 2024**


install.packages("patchwork")
library(patchwork)

## Creating the 2015 and 2024 Maps**


map_2015 <- ggplot(map15) +
  geom_sf(aes(fill = anc4plus_pct_2015), color = "white", linewidth = 0.2) +
  labs(
    title = "DHS 2015–16",
    fill = "% ANC 4+"
  ) +
  theme_minimal()

map_2024 <- ggplot(map24) +
  geom_sf(aes(fill = anc4plus_pct_2024), color = "white", linewidth = 0.2) +
  labs(
    title = "DHS 2024",
    fill = "% ANC 4+"
  ) +
  theme_minimal()

## ** Combining Side by Side 2015 and 2024 Maps


library(patchwork)

p1 <- map_2015 + map_2024 +
  plot_annotation(
    title = "Malawi: District-Level Coverage of 4+ Antenatal Care Visits",
    subtitle = "Comparison of DHS 2015–16 and DHS 2024",
    caption = "Source: Malawi Demographic and Health Surveys"
  )

p1


ggsave(
  filename = file.path(fig_dir, "malawi_anc4_comparison_2015_2024.png"),
  plot = p1,
  width = 14,
  height = 7,
  dpi = 300
)









