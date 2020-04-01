
library(ggplot2)
library(gghighlight)
library(dplyr)

# ---- i. Import data & lookups ----

cid  <- readRDS("data/countryIDs.rds")
cdat <- readRDS("data/countryDat.rds") 
data <- readRDS("data/jh_covid19_data.rds")

data[, iso3c := cid$iso3c[match(country, cid$country_name1)]]
data[is.na(iso3c), iso3c := cid$iso3c[match(country, cid$country_name2)]]
data[is.na(iso3c), iso3c := cid$iso3c[match(country, cid$country_name3)]]

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

