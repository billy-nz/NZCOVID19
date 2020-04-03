library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
library(lubridate)

library(rvest)
library(stringdist)



# ---- i. Prepare Data ----

confirmed_raw <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_raw    <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_raw <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

clean_jhd_to_long <- function(df) {
 df_str <- deparse(substitute(df))
 var_str <- substr(df_str, 1, str_length(df_str) - 4)
 
 df %>% group_by(`Country/Region`) %>%
  filter(`Country/Region` != "Cruise Ship" & `Country/Region` != "MS Zaandam" & `Country/Region` != "Diamond Princess") %>%
  select(-`Province/State`, -Lat, -Long) %>%
  mutate_at(vars(-group_cols()), sum) %>% 
  distinct() %>%
  ungroup() %>%
  rename(country = `Country/Region`) %>%
  pivot_longer(
   -country, 
   names_to = "date_str", 
   values_to = var_str
  ) %>%
  mutate(date = mdy(date_str)) %>%
  select(country, date, !! sym(var_str)) 
}


data <- clean_jhd_to_long(confirmed_raw) %>%
 full_join(clean_jhd_to_long(deaths_raw)) %>%
 full_join(clean_jhd_to_long(recovered_raw)) %>% 
 as.data.table()

# Save
saveRDS(jh_covid19_data, "data/jh_covid19_data.rds")


# ---- ii. Capture Country ID ----

# un_m49 <- read_html("https://unstats.un.org/unsd/methodology/m49/") %>%
#  html_table()
#
# un_m49 <- un_m49[[1]]
# colnames(un_m49) <- c("country_name1", "un_m49", "iso3c")
#
# # Update the lookup table to accomodate for name variations and unrecongised states
# extra.states <- data.frame(country_name1 = c("Kosovo", "Taiwan"),
#                            un_m49 = as.integer(c(383, 158)),
#                            iso3c = c("XKX", "TWN"))
#
# un_m49 <- setDT(rbind(un_m49, extra.states))
#
# un_m49$country_name2 <- copy(un_m49$country_name1)
#
# un_m49[country_name1 == "Bolivia (Plurinational State of)", country_name2 := "Bolivia"]
# un_m49[country_name1 == "Brunei Darussalam", country_name2 := "Brunei"]
# un_m49[country_name1 == "Congo", country_name2 := "Congo (Brazzaville)"]
# un_m49[country_name1 == "Democratic Republic of the Congo", country_name2 := "Congo (Kinshasa)"]
# un_m49[country_name1 == "Iran (Islamic Republic of)", country_name2 := "Iran"]
# un_m49[country_name1 == "Republic of Korea", country_name2 := "Korea, South"]
# un_m49[country_name1 == "Democratic People's Republic of Korea", country_name2 := "Korea, North"]
# un_m49[country_name1 == "Republic of Kosovo", country_name2 := "Kosovo"]
# un_m49[country_name1 == "Republic of Moldova", country_name2 := "Moldova"]
# un_m49[country_name1 == "Russian Federation", country_name2 := "Russia"]
# un_m49[country_name1 == "Taiwan", country_name2 := "Taiwan*"]
# un_m49[country_name1 == "Timor-Leste", country_name2 := "East Timor"]
# un_m49[country_name1 == "United Kingdom of Great Britain and Northern Ireland", country_name2 := "United Kingdom"]
# un_m49[country_name1 == "United Republic of Tanzania", country_name2 := "Tanzania"]
# un_m49[country_name1 == "United States of America", country_name2 := "US"]
# un_m49[country_name1 == "Venezuela (Bolivarian Republic of)", country_name2 := "Venezuela"]
# un_m49[country_name1 == "China, Hong Kong Special Administrative Region", country_name2 := "Hong Kong"]
# un_m49[country_name1 == "China, Macao Special Administrative Region", country_name2 := "Macau"]
# un_m49[country_name1 == "China", country_name2 := "People's Republic of China"]
# un_m49[country_name1 == "Falkland Islands (Malvinas)", country_name2 := "Falkland Islands"]
# un_m49[country_name1 == "Lao People's Democratic Republic", country_name2 := "Laos"]
# un_m49[country_name1 == "Micronesia (Federated States of)", country_name2 := "Micronesia"]
# un_m49[country_name1 == "Saint Martin (French Part)", country_name2 := "Saint Martin"]
# un_m49[country_name1 == "Sint Maarten (Dutch part)", country_name2 := "Sint Maarten"]
# un_m49[country_name1 == "United States Virgin Islands", country_name2 := "Virgin Islands"]
# un_m49[country_name1 == "Syrian Arab Republic", country_name2 := "Syria"]
# un_m49[country_name1 == "Myanmar", country_name2 := "Burma"]
# un_m49[country_name1 == "Côte d’Ivoire", country_name2 := "Cote d'Ivoire"]
# un_m49[country_name1 == "Viet Nam", country_name2 := "Vietnam"]
# un_m49[country_name1 == "State of Palestine", country_name2 := "West Bank and Gaza"]

# - For testing -
# jhd_countries <- setDT(tibble(country = unique(jh_covid19_data$country)) %>% arrange(country))
# jhd_countries[, iso3c := countryIDs$iso3c[match(country, countryIDs$country_name1)]]
# jhd_countries[is.na(iso3c), iso3c := countryIDs$iso3c[match(country, countryIDs$country_name2)]]
# - - - - - - - -


# ---- iii. Merge World Bank / Country Data ----
library(wbstats)

pull_worldbank_data <- function(vars) {
 new_cache <- wbcache()
 all_vars <- as.character(unique(new_cache$indicators$indicatorID))
 data_wide <- wb(indicator = vars, mrv = 10, return_wide = TRUE)
 new_cache$indicators[new_cache$indicators[,"indicatorID"] %in% vars, ] %>%
  rename(var_name = indicatorID) %>%
  mutate(var_def = paste(indicator, "\nNote:",
                         indicatorDesc, "\nSource:", sourceOrg)) %>%
  select(var_name, var_def) -> wb_data_def
 new_cache$countries %>%
  select(iso3c, iso2c, country, region, income) -> ctries
 left_join(data_wide, ctries, by = "iso3c") %>%
  rename(year = date,
         iso2c = iso2c.y,
         country = country.y) %>%
  select(iso3c, iso2c, country, region, income, everything()) %>%
  select(-iso2c.x, -country.x) %>%
  filter(!is.na(NY.GDP.PCAP.KD),
         region != "Aggregates") -> wb_data
 wb_data$year <- as.numeric(wb_data$year)
 wb_data_def<- left_join(data.frame(var_name = names(wb_data),
                                    stringsAsFactors = FALSE),
                         wb_data_def, by = "var_name")
 wb_data_def$var_def[1:6] <- c(
  "Three letter ISO country code as used by World Bank",
  "Two letter ISO country code as used by World Bank",
  "Country name as used by World Bank",
  "World Bank regional country classification",
  "World Bank income group classification",
  "Calendar year of observation"
 )
 wb_data_def$type = c("cs_id", rep("factor",  4), "ts_id",
                      rep("numeric", ncol(wb_data) - 6))
 return(list(wb_data, wb_data_def))
}

vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")
wb_list <- pull_worldbank_data(vars)
wb_data <- setDT(wb_list[[1]])
wb_data_def <- setDT(wb_list[[2]])

# Use this opportunity to update the country ID lookup table using World Bank labels
wb_labels <- unique(wb_data[, .(iso3c, country)])

un_m49[, country_name3 := wb_labels$country[match(iso3c, wb_labels$iso3c)]]
un_m49[is.na(country_name3), country_name3 := country_name1]

saveRDS(un_m49, "data/countryIDs.rds")

# Implement fussy match if still missing. Remove missing if un-matchable.

vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")
wb_list <- pull_worldbank_data(vars)
wb_data <- wb_list[[1]]
wb_data_def <- wb_list[[2]]

varnames <- c("population", "land_area_skm", "pop_density", "pop_largest_city", "life_expectancy", "gdp_capita")

wb_cs <- setDT(wb_data)[, (varnames) := lapply(.SD, function(x) last(na.omit(x))),
                       .SDcols = vars,
                       by = iso3c][, unique(.SD),
                                   .SDcols = c("iso3c", "country", "region", "income", varnames)]

setnames(wb_cs, "country", "country_name3")

# Manual Entry (for Taiwan)
extra.states <- data.frame(iso3c = "TWN",
                           country_name3 = "Taiwan",
                           region = "East Asia & Pacific",
                           income = "High income",
                           population = as.numeric(23806051),
                           land_area_skm = as.numeric(35410),
                           pop_density = as.numeric(367),
                           pop_largest_city = as.numeric(7871900),
                           life_expectancy = as.numeric(81.04),
                           gdp_capita = as.numeric(24827.898))

wb_cs <- rbind(wb_cs, extra.states)

to.char <- c("iso3c", "country_name3", "region", "income")

wb_cs[, (to.char) := lapply(wb_cs[,to.char,with=F], as.character)]

saveRDS(wb_cs, "data/countryDat.rds")






