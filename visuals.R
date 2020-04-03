library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gghighlight)

# ---- i. Import latest data & lookups ----

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
    rename(Country = `Country/Region`) %>%
    pivot_longer(
      -Country, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    mutate(date = mdy(date_str)) %>%
    select(Country, date, !! sym(var_str)) 
}

data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw)) %>%
  full_join(clean_jhd_to_long(recovered_raw)) %>% 
  as.data.table()

cid  <- readRDS("data/countryIDs.rds")
cdat <- readRDS("data/countryDat.rds") 

data[, iso3c := cid$iso3c[match(Country, cid$country_name1)]]
data[is.na(iso3c), iso3c := cid$iso3c[match(Country, cid$country_name2)]]
data[is.na(iso3c), iso3c := cid$iso3c[match(Country, cid$country_name3)]]

data[Country == "Taiwan*", Country := "Taiwan"]


# ---- i.  Plot ----

# Settings
selected.iso3c <- c("NZL", "TWN")


# Plot Third degree polynomia interpolation
data %>%
  filter(iso3c %in% selected.iso3c) %>% 
  group_by(Country) %>%
  # filter(date >= time0 & date - time0 <30) %>% 
  mutate(time = 1:n()) %>%
  lapply(., function(x) browser()) %>% 
  
  ggplot(., aes(x = time, y = confirmed, color = Country)) +
  xlab("Days since 100 cases") + ylab("Cumulative cases") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  # geom_smooth(method = "loess", se =F) +
  # geom_line() +
  theme_minimal() + 
  labs(title = "Focus on the first month: Confirmed Cases in New Zealand\n")




# Data Preparation
DF <- data %>% 
  left_join(CHN_time0, by = "iso3c") %>%
  mutate(
    edate_confirmed = as.numeric(date - time0)
  ) %>%
  filter(time0 >= 0) %>%
  group_by(country) %>%
  filter (n() >= 30) %>%
  ungroup() %>%
  left_join(cdat, by = "iso3c") %>% 
  mutate(
    confirmed_1e5pop = 1e5*confirmed/population)

# Plot Third degree polynomia interpolation
DF %>%
  filter(iso3c %in% selected.iso3c) %>% 
  group_by(country) %>%
  filter(date >= time0 & date - time0 <30) %>% 
  mutate(time = 1:n()) %>%
  ggplot(., aes(x = time, y = confirmed, color = country)) +
  xlab("Days since 100 cases") + ylab("Cumulative cases") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  theme_minimal() + 
  labs(title = "Focus on the first month: Confirmed Cases in New Zealand\n")



data %>%
  filter(iso3c %in% c("AUS", "NZL", "TWN", "FRA", "ESP")) %>%
  filter(confirmed >= 100) %>% 
  group_by(iso3c) %>%
  mutate(time = 1:n()) %>%
  ggplot(., aes(x = time, y = confirmed, color = iso3c)) +
  xlab("Days since 100 cases") + 
  ylab("Cumulative cases") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) + 
  theme_minimal()


# Time-zero for each country Where confirmed cases matches or exceeds the first avaiable Chinese case number (548 on 2020-01-22)
# Also a require each country to have at least 30 days post time zero
CHN_time0 <- data %>% 
  group_by(iso3c) %>% 
  filter(confirmed >= 100) %>% 
  summarise(time0 = min(date))

DF <- data %>% 
  left_join(CHN_time0, by = "iso3c") %>%
  mutate(
    edate_confirmed = as.numeric(date - time0)
  ) %>%
  filter(time0 >= 0) %>%
  group_by(country) %>%
  filter (n() >= 30) %>%
  ungroup() %>%
  left_join(cdat, by = "iso3c") %>% 
  mutate(
    confirmed_1e5pop = 1e5*confirmed/population
  )

DF %>%
  filter(region == "East Asia & Pacific" & iso3c != "CHN" & iso3c != "KOR") %>% 
  group_by(country) %>%
  filter(date >= time0 & date - time0 <30) %>% 
  mutate(time = 1:n()) %>%
  ggplot(., aes(x = time, y = confirmed, color = country)) +
  xlab("Days since 100 cases") + ylab("Cumulative cases") +
  geom_line() + theme_minimal() + labs(
    title = "Focus on the first month: Confirmed Cases\n"
  )

# Third degree polynomia interpolation
DF %>%
  filter(iso3c %in% c("NZL", "TWN")) %>% 
  group_by(country) %>%
  filter(date >= time0 & date - time0 <30) %>% 
  mutate(time = 1:n()) %>%
  ggplot(., aes(x = time, y = confirmed, color = country)) +
  xlab("Days since 100 cases") + ylab("Cumulative cases") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  theme_minimal() + 
  labs(title = "Focus on the first month: Confirmed Cases in New Zealand\n")


# I define event time zero where, for a given country, the confirmed
# cases match or exceed the Chinese case number at the beginning of the
# data so that all countries can be compared across event time.
# Also a require each country to have at least 7 days post event day 0

edates_confirmed <- data %>% 
 group_by(iso3c) %>%
 filter(confirmed >= min(data$confirmed[data$iso3c == "CHN"])) %>%
 summarise(edate_confirmed = min(date))

df <- data %>% 
  left_join(edates_confirmed, by = "iso3c") %>%
  mutate(
    edate_confirmed = as.numeric(date - edate_confirmed)
  ) %>%
  filter(edate_confirmed >= 0) %>%
  group_by(country) %>%
  # filter (n() >= 7) %>% 
  ungroup() %>%
  left_join(cdat, by = "iso3c") %>% 
  mutate(
    confirmed_1e5pop = 1e5*confirmed/population
  )


df <- data %>% 
 left_join(edates_confirmed, by = "iso3c") %>%
 mutate(
  edate_confirmed = as.numeric(date - edate_confirmed)
 ) %>%
 filter(edate_confirmed >= 0) %>%
 group_by(country) %>%
 # filter (n() >= 7) %>% 
 ungroup() %>%
 left_join(cdat, by = "iso3c") %>% 
 mutate(
  confirmed_1e5pop = 1e5*confirmed/population
 )

lab_notes <- paste0(
 "Data as provided by Johns Hopkins University Center for Systems Science ", 
 "and Engineering (JHU CSSE)\nand obtained on March 23, 2020. ",
 "The sample is limited to countries with at least seven days of positive\n", 
 "event days data. Code and walk-through: https://joachim-gassen.github.io."
)

lab_x_axis_confirmed <- sprintf(paste(
 "Days since confirmed cases matched or exceeded\n", 
 "initial value reported for China (%d cases)\n"
), min(data$confirmed[data$iso3c == "CHN"]))

gg_my_blob <- list(
 scale_y_continuous(trans='log10', labels = scales::comma),  
 theme_minimal(), 
 theme(
  plot.title.position = "plot", 
  plot.caption.position =  "plot",
  plot.caption = element_text(hjust = 0),
  axis.title.x = element_text(hjust = 1),
  axis.title.y = element_text(hjust = 1),
 ),
 labs(caption = lab_notes,
      x = lab_x_axis_confirmed,
      y = "Confirmed cases (logarithmic scale)"),
 gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
             label_params = list(segment.color = NA, nudge_x = 1))
)

ggplot(df %>% filter (edate_confirmed <= 30), 
       aes(x = edate_confirmed, color = country, y = confirmed)) +
 geom_line() +
 labs(
  title = "Focus on the first month: Confirmed Cases\n"
 ) + gg_my_blob

# Confirmed cases per 100,000 inhibitants
ggplot(df %>% filter (edate_confirmed <= 30 & region == "East Asia & Pacific"), 
       aes(x = edate_confirmed, color = country, y = confirmed_1e5pop)) +
 geom_line() +
 gg_my_blob +
 labs(
  y = "Confirmed cases per 100,000 inhabitants (in East Asian & Pacific Region)",
  title = "Cases relative to population\n"
 ) 

ggplot(df %>% filter (edate_confirmed <= 30 & region == "East Asia & Pacific"), 
       aes(x = edate_confirmed, color = country, y = confirmed)) +
 geom_line() +
 gg_my_blob +
 labs(
  y = "Confirmed cases per 100,000 inhabitants (in East Asian & Pacific Region)",
  title = "Cases relative to population\n"
 ) 



edates_confirmed <- data %>% 
 group_by(iso3c) %>%
 filter(confirmed >= min(data$confirmed[data$iso3c == "CHN"])) %>%
 summarise(edate_confirmed = min(date))

df <- data %>% 
 left_join(edates_confirmed, by = "iso3c") %>%
 mutate(
  edate_confirmed = as.numeric(date - edate_confirmed)
 ) %>%
 filter(edate_confirmed >= 0) %>%
 group_by(country) %>%
 # filter (n() >= 7) %>% 
 ungroup() %>%
 left_join(cdat, by = "iso3c") %>% 
 mutate(
  confirmed_1e5pop = 1e5*confirmed/population
 )

