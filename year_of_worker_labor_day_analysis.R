library(magrittr)
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(purrr)
library(tidyr)
library(lubridate)
library(zoo)
library(httr)

### Functions Section ###
# This function is now needed to access BLS data programmatically as per 
# their policy outlined here: https://www.bls.gov/bls/pss.htm
get_bls_data <- function(url, email) {
  bls_res <- GET(url = url, user_agent(email))
  stop_for_status(bls_res)
  
  bls_content <- content(bls_res, 
                         as = "parsed",
                         type = "text/tab-separated-values",
                         encoding = "UTF-8",
                         col_names = T,
                         col_types = cols(.default = col_character()),
                         trim_ws = T
  )
  return(bls_content)
  
}

# This function takes a row number fed to it from the for loop in the 
# Visualization 1 section (approx rows 98-108) and calculates the row number
# of the pre-recessionary peak payroll employment (i.e. where the `peak` column
# in the unemp_rec_joined is TRUE) for the most recent recession. For example
# for row 6 in the unemp_rec_joined dataframe the row of the most recent
# peak pre-recession employment would be row 1 (most recent row in the past)
# where the `peak` value is TRUE

calc_back_rows <- function(row_num) {
  back_row <- min(keep(map_int(peak_rows, function(x) row_num - x), ~.x > 0L))
  
  return(back_row)
}

# This function is a simple calculator of percent change of a vector for each
# value subsequent to the first one. Used to calculate the percent change
# of a column of CPI data. Each subsequent row is a different month (or quarter)
# in time from the first.
rolling_chg <- function(x) {
  old <- x[1]
  ((x - old) / old) * 100 -> y
  return(y)
}

### Data Reading Section ###
# Reading in entire TSV of data from the BLS Current Employment Statistics
# which includes payroll employment, earnings, and wage data.
# From text files here: https://www.bls.gov/ces/data/

all_ces <- get_bls_data(
  "https://download.bls.gov/pub/time.series/ce/ce.data.0.AllCESSeries",
                        "anesta@dotdashmdp.com"
  )

# Getting exact NBER recession dates from FRED (St. Louis Federal Reserve Bank): 
# https://fred.stlouisfed.org/series/USREC

recession_dates <- read_csv("./reference_data/USREC.csv", col_names = T,
                            col_types = "Di")

### Visualization section ###

## Visualization 1: Line graph of months taken for job market to recovery to peak pre-recession 
## payroll employment levels.

# Filtering recession dates data to 1969 and later. Editorial decision.
# Also adding column to identify "peak" pre-recession employment month (i.e.
# month directly prior to recession)

recession_dates %>% 
  rename_with(make_clean_names) %>% 
  filter(date >= base::as.Date("1969-12-01")) %>% 
  mutate(peak = replace_na(if_else(usrec == 0L & lead(usrec) == 1L, T, F), F)) -> rec_w_peak

# Filtering for CES data that includes seasonally-adjusted payroll employment 
# levels (in thousands) for all non-farm employees. (BLS series id: CES0000000001)
# Reference: https://beta.bls.gov/dataViewer/view/timeseries/CES0000000001

all_ces %>% 
  filter(series_id == "CES0000000001") %>% 
  mutate(date = base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01")),
         value = as.numeric(value)) %>% 
  filter(between(date, base::as.Date("1969-12-01"), max(rec_w_peak$date))) %>% 
  select(date, value) -> unemployment_post_1969

# Joining payroll employment data to recession dates data.
unemployment_post_1969 %>% 
  left_join(rec_w_peak, by = "date") -> unemp_rec_joined

# Getting row numbers of months that were pre-recession payroll employment peaks
peak_rows <- which(unemp_rec_joined$peak)

# Creating empty vectors for new columns. They will contain both the months since
# the pre-recession peak for every recessionary period and the percent change 
# from peak payroll employment levels for every recessionary period.

vector(mode = "double", length = nrow(unemp_rec_joined)) -> pct_of_peak_vtc
vector(mode = "double", length = nrow(unemp_rec_joined)) -> months_since_peak_vtc

# This for loop loops over every row of the unemp_rec_joined dataframe in order
# to create two new columns for it:
# 1. The first is the percent change in payroll employment levels from the most
# recent pre-recession peak to the date in the iterated row
# 2. The second is the number of months passed between the most recent pre-recession 
# peak date to the date in the iterated row

# This is accomplished with the `calc_back_rows()` function

for (i in 1L:nrow(unemp_rec_joined)) {
  
  if (unemp_rec_joined$peak[i]) {
    pct_of_peak_vtc[i] <- 0
    months_since_peak_vtc[i] <- 0
  } else {
    j <- i - calc_back_rows(i)
    pct_of_peak_vtc[i] <- (((unemp_rec_joined$value[i] - unemp_rec_joined$value[j]) / unemp_rec_joined$value[j]) * 100)
    months_since_peak_vtc[i] <- calc_back_rows(i)
  }
}

# Adding the additional columns
unemp_rec_joined %>% 
  mutate(months_since_peak = months_since_peak_vtc,
         pct_of_peak = pct_of_peak_vtc
         ) -> emp_rec_joined_chg_months

# This map function loops through the `peak_rows` vector to identify
# the rows in the main `emp_rec_joined_chg_months` dataframe that include the entire
# "recessionary period" (i.e. row that `peak` is TRUE to the row before the next
# row where peak is `TRUE`). It then renames the `pct_of_peak` column with the
# month before that recessionary period in the %b. %Y format.

map(1:length(peak_rows), function(x) {
  
  if (x == length(peak_rows)) {
    df <- slice(emp_rec_joined_chg_months, peak_rows[x]:nrow(emp_rec_joined_chg_months)) %>% 
      select(date, pct_of_peak, months_since_peak) 
    
    format(min(df$date), "%b. %Y") -> new_name
    
    df %>% 
      rename(!!new_name := pct_of_peak) %>% 
      select(-date) -> paired_df
    
    return(paired_df)


  } else {
    df <- slice(emp_rec_joined_chg_months, peak_rows[x]:(peak_rows[x + 1] - 1)) %>% 
      select(date, pct_of_peak, months_since_peak) 
    
    format(min(df$date), "%b. %Y") -> new_name
    
    df %>% 
      rename(!!new_name := pct_of_peak) %>% 
      select(-date) -> paired_df
    
    return(paired_df)
  }
  
}) %>% 
  set_names(., nm = map(.x = ., ~nrow(.))) -> emp_rec_list

# Joining the separate dataframes by the `months_since_peak` column and 
# ordering the columns based on how many months of data they have.
emp_rec_list[order(as.numeric(names(emp_rec_list)), decreasing = T)] %>%   
  reduce(left_join, by = "months_since_peak") %>% 
  relocate(months_since_peak, .before = `Dec. 2007`) -> emp_rec_joined_combo

# Identifying the row in each column with percent change in payroll employment
# level data (i.e. 2:9) where the percent change of jobs was equal to or greater
# than the pre-recession peak (i.e. >= 0) and then replacing all data in each
# column _after_ that row in NAs. Then filtering out any rows with NAs for
# _all_ recessions (i.e. there was no recession (column) where `months_since_peak` was
# still negative)

emp_rec_joined_combo %>% 
  mutate(across(2:ncol(.), function(x) {
    first_row <- max(which(x < 0)) + 2
    return(replace(x, first_row:146, NA_real_))
  })) %>% 
  filter(if_any(2:ncol(.), ~!is.na(.x))) -> emp_rec_final

months_col <- emp_rec_final %>% select(months_since_peak)
values_cols <- emp_rec_final %>% select(-months_since_peak)

# Re-ordering the columns sequentially based on recession start year
emp_rec_final_sorted <- bind_cols(months_col, 
         values_cols[order(as.integer(str_extract(names(values_cols), "\\d{4}")), decreasing = T)])
  
write_csv(emp_rec_final_sorted, "./visualizations/emp_rec_final.csv")  

## Visualization 2: Bar graph of industries that have gained & lost the most
## jobs since the same month in 2019 (i.e. pre-pandemic).

# Filtering to data post-2019 and excluding supersectors "total nonfarm", 
# "total private", "goods-producing", "service-producing", private service-producing")
# Editorial decision. Including only months of July '22 and February of '19 to compare.

all_ces %>% 
  filter(as.integer(year) %in% c(2019L, 2022L)) %>% 
  mutate(seas_adj = str_sub(series_id, 3, 3),
         supersector = str_sub(series_id, 4, 5),
         industry = str_sub(series_id, 6, 11),
         data_type = str_sub(series_id, 12, 13),
         date = base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01"))
         ) %>% 
  filter(seas_adj == "S",
         !(supersector %in% c("00", "05", "06", "07", "08")),
         industry == "000000",
         date %in% c(base::as.Date("2022-07-01"), base::as.Date("2019-02-01")),
         data_type == "01") -> supersector_latest_pre_covid

# Reading in industry code reference file
industry_codes <- get_bls_data(
  "https://download.bls.gov/pub/time.series/ce/ce.industry",
  "anesta@dotdashmdp.com"
  )

# Calculating the Feb. '19 to Jul. '22 payroll employment level percent change
# and joining the `industry_codes` reference file for industry name columns
supersector_latest_pre_covid %>% 
  arrange(series_id, desc(date)) %>% 
  mutate(value = as.numeric(value),
         job_diff = round((value - lead(value)) * 1000),
         industry_code = str_c(supersector, industry)) %>%
  left_join(industry_codes, by = "industry_code") %>%
  filter(date == max(date)) %>% 
  select(industry_name, date, job_diff) %>%
  arrange(desc(job_diff)) %>% 
  mutate(change_direction = if_else(job_diff > 0, "gain", "loss")) %>%
  rename(!!as.character(format(unique(.$date), "%b. '%y")) := job_diff) %>% 
  mutate(`Feb. '19` = 0) %>% 
  select(-date) %>% 
  relocate(4, .before = 2) -> supersector_job_diff

write_csv(supersector_job_diff, "./visualizations/supersector_job_diff.csv")

## Visualization 3: Bar graph of sub-industries that have gained & lost the most
## jobs since the same month in 2019 (i.e. pre-pandemic).

# Getting the more detailed subsector industry codes from the industry
# reference file
filter(industry_codes, !(display_level %in% c("0", "1", "2", "3"))) %>% 
  pull(industry_code) -> detailed_codes

# Filtering to data post-2019 and including only July of 2022 and Feburary of 2019
# to compare payroll employment levels
all_ces %>% 
  filter(as.integer(year) %in% c(2019L, 2022L)) %>% 
  mutate(seas_adj = str_sub(series_id, 3, 3),
         supersector = str_sub(series_id, 4, 5),
         industry = str_sub(series_id, 6, 11),
         industry_code = str_sub(series_id, 4, 11),
         data_type = str_sub(series_id, 12, 13),
         date = base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01"))
  ) %>% 
  filter(seas_adj == "S",
         industry_code %in% detailed_codes,
         date %in% c(base::as.Date("2022-07-01"), base::as.Date("2019-02-01")),
         data_type == "01") -> detailed_latest_pre_covid

# Calculating Feb. '19 to Jul. '22 percent change in employment levels by
# subindustry. Join the subindustry/supersector reference file.
detailed_latest_pre_covid %>% 
  arrange(series_id, desc(date)) %>% 
  mutate(value = as.numeric(value),
         `Total Change` = round((value - lead(value)) * 1000),
         `Percent Change` = ((value - lead(value)) / lead(value)) * 100) %>% 
  filter(date == max(date)) %>% 
  arrange(desc(`Percent Change`)) %>% 
  left_join(industry_codes, by = "industry_code") %>% 
  left_join(get_bls_data("https://download.bls.gov/pub/time.series/ce/ce.supersector",
                         "anesta@dotdashmdp.com"), by = c("supersector" = "supersector_code")) %>% 
  rename(Industry = industry_name, Supersector = supersector_name) %>% 
  select(Industry, Supersector, `Total Change`, `Percent Change`) -> detailed_industry_change

# Getting the Q1 (25th percentile) and Q3 (75th percentile) values for the total 
# percent change from Feb. '19 to Jul. '22. Needed for the IQR to identify outliers.
as.double(unname(summary(detailed_industry_change$`Percent Change`)[2])) -> Q1
as.double(unname(summary(detailed_industry_change$`Percent Change`)[5])) -> Q3

# Identifying the supersector/subindustries with the outlier changes (both positive
# and negative) from Feb. '19 to Jul. '22
detailed_industry_change %>% 
  mutate(is_outlier = case_when(
    `Percent Change` > Q3 + (3 * IQR(`Percent Change`)) ~ "Outlier",
    `Percent Change` < Q1 - (3 * IQR(`Percent Change`)) ~ "Outlier",
    T ~ "Not outlier"
  ),
  change_dir = if_else(`Percent Change` > 0, T, F)) %>% 
  filter(is_outlier == "Outlier") %>% 
  select(-c(is_outlier, `Total Change`)) -> detailed_industry_change_outliers

write_csv(detailed_industry_change_outliers, "./visualizations/detailed_industry_change_outliers.csv")

## Visualization 4: Change in prime age (25 - 54 yrs. old) employment levels from 
## pre-pandemic (Feb. '20) to July '22 by race and gender.

# Reading in CPS labor force data from the BLS
# Text files from here: https://www.bls.gov/cps/data.htm
all_cps_ln <- get_bls_data(
  "https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData",
  "anesta@dotdashmdp.com"
  )

# Reading in CPS series reference file to get each series' full name
ln_series <- get_bls_data(
  "https://download.bls.gov/pub/time.series/ln/ln.series",
  "anesta@dotdashmdp.com"
)

# Prime age employment levels series for White, Black, Latino/a, and Asian men & women
prime_age_emp_series <- c("LNU02000064", "LNU02000065",
                          "LNU02000067", "LNU02000068",
                          "LNU02000070", "LNU02000071",
                          "LNU02032330", "LNU02032371")

# Filtering to the correct time frame and series, doing a little
# column title data cleaning, pivoting data and then calculating
# the percent change in prime age employment level for each month 
# from Mar. '20 to Jul. '22

all_cps_ln %>% 
  filter(series_id %in% prime_age_emp_series, as.integer(year) > 2019L) %>% 
  mutate(
    date = base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01"))
  ) %>% 
  left_join(ln_series, by = "series_id") %>% 
  filter(between(date, base::as.Date("2020-02-01"), base::as.Date("2022-07-01"))) %>% 
  select(date, series_title, value) %>% 
  mutate(series_title = str_extract(
    str_remove_all(
      str_replace_all(
        series_title, "African American", "Black"
        ), ","
      ), "\\w+\\s\\w+$"
    ),
         value = as.numeric(value)) %>% 
  pivot_wider(names_from = series_title, values_from = value) %>% 
  mutate(across(!date, function(x) {
    old <- x[1]
    ((x - old) / old) * 100 -> y
    return(y)
    
  })) -> epr_race_gender

write_csv(epr_race_gender, "./visualizations/epr_race_gender.csv")

## Visualization 5: Change in the prime age (25 - 54 yrs. old) 
## employment to population ratio for Asian, Black, Latino/a, and White men and women
## from Jul. '19 (pre-pandemic) to Jul. '22.

# CPS series needed 
prime_age_epr_series <- c("LNU02300064", "LNU02300065",
                          "LNU02300067", "LNU02300068",
                          "LNU02300070", "LNU02300071",
                          "LNU02332330", "LNU02332371")

# Filtering to the correct time frame and series, doing a little
# column title data cleaning, pivoting data and then calculating
# the overall change in the prime age employment population ratio 
# for each racial & gender group from Jul. '19 to Jul. '22 (Yo3Y pre-pandemic to current)
all_cps_ln %>% 
  filter(series_id %in% prime_age_epr_series, as.integer(year) >= 2019L) %>% 
  mutate(
    date = base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01"))
  ) %>% 
  left_join(ln_series, by = "series_id") %>% 
  filter(date %in% c(base::as.Date("2019-07-01"), base::as.Date("2022-07-01"))) %>% 
  select(date, series_title, value) %>% 
  mutate(series_title = str_extract(
    str_remove_all(
      str_replace_all(
        series_title, "African American", "Black"
      ), ","
    ), "\\w+\\s\\w+$"
  ),
  value = as.double(value),
  date = format(date, "%b. '%y")) %>% 
  pivot_wider(names_from = date, values_from = value) %>% 
  separate(series_title, into = c("race_ethnicity", "gender"), sep = "\\s") %>% 
  mutate(change_direction = if_else(.[[4]] > .[[3]], "increase", "decrease"),
         change = .[[4]] - .[[3]],
         gender = str_to_title(gender)) %>% 
  arrange(desc(.[[4]])) -> epr_yo3y_chg_race_gender

write_csv(epr_yo3y_chg_race_gender, "./visualizations/epr_yo3y_chg_race_gender.csv")

# Getting only the Employment-to-population ratio series from the CPS series
# refernce file data
# ln_series %>% 
#   filter(str_detect(series_title, "nadj\\)\\s+Employed\\s|nadj\\)\\s+Employment\\s"),
#          periodicity_code == "M",
#          str_detect(series_title, "Population\\sRatio", negate = T)) %>% 
#   pull(series_id) -> emp_series
#   
# 
# all_cps_ln %>% 
#   filter(series_id %in% emp_series, as.integer(year) >= 2019L) %>% 
#   mutate(
#     date = base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01"))
#   ) %>% 
#   left_join(ln_series, by = "series_id") %>% 
#   filter(date >= base::as.Date("2019-07-01")) %>% 
#   select(date, series_id, series_title, value) %>% 
#   mutate(value = as.numeric(value)) -> cps_emp_levels
# 
# cps_emp_levels %>% 
#   filter(date %in% c(max(date), max(date) %m-% months(36))) %>% 
#   arrange(series_id, desc(date)) %>% 
#   mutate(pct_chg = (value - lead(value)) / lead(value)) %>% 
#   filter(date == max(date), value >= 100) %>% 
#   arrange(pct_chg) -> biggest_yo3y_changes


## Visualization 6: Wage & Inflation gains since COVID-19
## Looking at the percent chang in nominal compensation, inflation, 
## and inflation-adjusted "real" compensation for all civilian sector workers
## from Q1 2020 to Q2 2022 from the BLS Employment Cost Index

# From text files here: https://www.bls.gov/ncs/ect/data.htm
all_eci <- get_bls_data(
  "https://download.bls.gov/pub/time.series/ci/ci.data.1.AllData",
  "anesta@dotdashmdp.com"
)

# Converting dates to yearquarter data type and filtering to only include 
# data after Q1 2020 and for total compensation for all civilian workers
# https://beta.bls.gov/dataViewer/view/timeseries/CIS1010000000000I

all_eci %>% 
  mutate(
  date = as.yearqtr(paste(year, period)),
  value = as.double(value)) %>% 
  filter(between(date, as.yearqtr("2020 Q1"), as.yearqtr("2022 Q2")),
         series_id == "CIS1010000000000I") %>% 
  rename(eci = value) %>%
  select(date, eci) -> total_comp_eci

# Getting the CPI (inflation) change from the BLS CPI https://www.bls.gov/cpi/data.htm
# To apply to ECI compensation data. Then converting date column from months to 
# quarters (CPI data is monthly) and getting a quarterly average CPI value for 
# all items. All items seasonally-adjusted from here: https://beta.bls.gov/dataViewer/view/timeseries/CUSR0000SA0

general_cpi <- get_bls_data("https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems",
                            "anesta@dotdashmdp.com") %>% 
  mutate(date = as.yearqtr(base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01")), "%Y-%m-%d"),
         value = as.double(value)) %>% 
  filter(series_id == "CUSR0000SA0", date >= as.yearqtr("2020 Q1")) %>% 
  rename(cpi = value) %>% 
  group_by(date) %>% 
  summarize(cpi = mean(cpi)) %>% 
  select(date, cpi) %>% 
  mutate(pre_pan_chg = rolling_chg(cpi))

# Joining the ECI and CPI data by quarters and calculating both the general inflation
# rate change from Q1 2020 to Q2 2022 and the nominal compensation change from 
# Q1 2020 to Q2 2022. Real compensation is just the different between
# nominal compensation and inflation.
inner_join(total_comp_eci, general_cpi, by = "date") %>% 
  mutate(across(!date, function(x) {
    old <- x[1]
    ((x - old) / old) * 100 -> y
    return(y)
    
  }),
  `Real Compensation` = eci - cpi) %>% 
  select(-pre_pan_chg) %>% 
  rename(`Nominal Compensation` = eci,
         Inflation = cpi) -> wage_changes

write_csv(wage_changes, "./visualizations/wage_changes_general.csv")

## Visualization 7: Change in real and nominal compensation by major
## work sectors from Q1 2020 (pre-pandemic) to Q2 2022

# Reading in the ECI series reference file for correct sector name column
eci_series <- get_bls_data(
  "https://download.bls.gov/pub/time.series/ci/ci.series",
  "anesta@dotdashmdp.com"
)

# Joining the ECI series and industry reference file with the full ECI data.
all_eci_joined <- all_eci %>% 
  left_join(eci_series, by = "series_id") %>% 
  left_join(get_bls_data("https://download.bls.gov/pub/time.series/ci/ci.industry",
                         "anesta@dotdashmdp.com"), by = "industry_code")

# Filter ECI data to seasonally-adjusted nominal and real compensation in industries
# that don't include goods-producing, service-producing, or "all industries" 
# (Editorial decision). Calculating the percent change in compensation 
# from Q1 2020 to Q2 2022 and doing some data cleaning for the correct
# industry column title. "Real" compensation calculated via CPI data
# read in lines approx. ~430-440
all_eci_joined %>% 
  filter(
    seasonal == "S",
    owner_code == "2",
    estimate_code == "01",
    !(industry_code %in% c("G00000", "S00000", "000000")),
    occupation_code == "000000",
    subcell_code == "00",
    periodicity_code == "I"
  ) %>% 
  mutate(date = as.yearqtr(paste(year, period))) %>% 
  filter(date %in% c(as.yearqtr("2022 Q2"), as.yearqtr("2020 Q1"))) %>% 
  select(series_id, date, series_title, value, industry_code, display_level) %>% 
  arrange(series_id, desc(date)) %>% 
  mutate(
    value = as.double(value),
    pct_chg = ((value - lead(value)) / lead(value)) * 100
    ) %>% 
  filter(date == max(date)) %>% 
  arrange(desc(pct_chg)) %>% 
  left_join(general_cpi, by = "date") %>% 
  mutate(real_wage = pct_chg - pre_pan_chg,
         series_title = str_extract(series_title, "(?<=in\\s).*(?=,\\s+\\w+)")) %>% 
  filter(display_level == "2") %>%
  rename(Industry = series_title, `Nominal Compensation` = pct_chg, `Real Compensation` = real_wage) %>% 
  select(Industry, `Nominal Compensation`, `Real Compensation`) -> industry_wage_change

write_csv(industry_wage_change, "./visualizations/industry_wage_change.csv")

# Filter ECI data to seasonally-adjusted nominal and real compensation in occupations.
# Joining the occupations reference file to get correct column names.
# Calculating the percent change in compensation 
# from Q1 2020 to Q2 2022 and doing some data cleaning for the correct
# industry column title. "Real" compensation calculated via CPI data
# read in lines approx. ~430-440. 
all_eci_joined %>% 
  filter(
    seasonal == "S",
    owner_code == "2",
    estimate_code == "02",
    industry_code == "000000",
    subcell_code == "00",
    periodicity_code == "I"
  ) %>% 
  mutate(date = as.yearqtr(paste(year, period))) %>% 
  filter(date %in% c(as.yearqtr("2022 Q2"), as.yearqtr("2020 Q1"))) %>% 
  select(series_id, date, series_title, value, industry_code, occupation_code) %>% 
  arrange(series_id, desc(date)) %>% 
  mutate(
    value = as.double(value),
    pct_chg = ((value - lead(value)) / lead(value)) * 100
  ) %>% 
  filter(date == max(date)) %>% 
  left_join(get_bls_data("https://download.bls.gov/pub/time.series/ci/ci.occupation",
                         "anesta@dotdashmdp.com"), by = "occupation_code") %>% 
  
  arrange(desc(pct_chg)) %>% 
  left_join(general_cpi, by = "date") %>% 
  mutate(real_wage = pct_chg - pre_pan_chg,
         series_title = str_extract(series_title, "(?<=in\\s).*(?=,\\s+\\w+)")) %>% 
  filter(display_level %in% c("1")) %>%
  rename(Occupation = series_title, `Nominal Compensation` = pct_chg, `Real Compensation` = real_wage) %>% 
  select(Occupation, `Nominal Compensation`, `Real Compensation`) -> occupation_wage_change

write_csv(occupation_wage_change, "./visualizations/occupation_wage_change.csv")

## Visualization 8: Bar graph of change in usual weekly earnings from CPS
## data for Asian, Black, Latino/a, and White men and women from 
## Q2 '19 to Q2 '22 (Yo3Y, through the pandemic)

# Reading in the reference file for the CPS earnings data
get_bls_data("https://download.bls.gov/pub/time.series/le/le.series",
             "anesta@dotdashmdp.com") -> le_series

# Reading in the earnings data from the CPS 
get_bls_data("https://download.bls.gov/pub/time.series/le/le.data.1.AllData",
             "anesta@dotdashmdp.com") -> all_le_cps

# Series codes needed for Asian, Black, Latino/a, and White men and women
# usual weekly earnings
cps_earnings_series <- c("LEU0252884000",
                         "LEU0252884300",
                         "LEU0252884900",
                         "LEU0252885200",
                         "LEU0252885800",
                         "LEU0252886100",
                         "LEU0254871200",
                         "LEU0254871300"
)

# Filtering earnings CPS data to only the series above and only Q2 2019 and Q2 2022.
# Joining on the reference file to get the full column name for each series
# and then calculating the percent change in earnings Yo3Y to see pandemic changes. 
# Doing slight data cleaning to shore up the group titles and adding
# a directional change column for the different bar graph colors in Datawrapper.
all_le_cps %>% 
  filter(series_id %in% cps_earnings_series, as.integer(year) >= 2019L) %>% 
  mutate(date = as.yearqtr(paste(year, period))) %>% 
  filter(date %in% c(as.yearqtr("2022 Q2"), as.yearqtr("2019 Q2"))) %>% 
  left_join(le_series, by = "series_id") %>% 
  select(date, series_id, series_title, value) %>% 
  arrange(series_id, desc(date)) %>% 
  mutate(value = as.numeric(value),
         pct_chg = ((value - lead(value)) / lead(value)) * 100,
         series_title = str_remove_all(str_extract(series_title, "(?<=workers,\\s).*$"), "\\sor\\s|African\\sAmerican|Hispanic")) %>% 
  separate(series_title, into = c("Race", "Gender"), sep = ",\\s") %>% 
  filter(date == max(date)) %>% 
  arrange(desc(pct_chg)) %>% 
  mutate(change_direction = if_else(pct_chg > 0, "gain", "loss")) %>%
  select(Race, Gender, pct_chg, change_direction) -> biggest_yo3y_changes_cps_earnings

write_csv(biggest_yo3y_changes_cps_earnings, "./visualizations/biggest_yo3y_changes_cps_race_gender_earnings.csv")






