# *************************************************************************************************************************
# Multifamily Construction Forecast
# *************************************************************************************************************************

# This script downloads multiple data series from the FRED website and other data sources to forecast MF construction units

# *************************************************************************************************************************
# CONFIGURATION
# *************************************************************************************************************************
rm(list = ls())
options(scipen = 999)
setwd("~/RStudio/Projects/MF_Construction")

# load libraries
library(tidyverse)
library(readxl)
library(slider)
library(lubridate)
library(glue)
library(GGally)
library(httr)
library(jsonlite)
library(janitor)
library(fpp3)
library(tseries)
library(knitr)
library(urca)
library(stringr)

# configure the forecast vintage, one MUST remain COMMENTED OUT
vintage <- c(
             "retro"
             #"latest"
             )

# the vintage date is the "production" month the forecast would have been run
vintage_date <- ymd("2025-09-01")

# *************************************************************************************************************************
# DATA SOURCING
# *************************************************************************************************************************
# create a list of urls to download...for the FRED urls we only need certain components
mf_urls <- list(
                  mf_underway = list(series_id = "UNDCON5MUSA", freq = "Monthly", cosd = "1970-01-01", coed = "2026-01-01", nd = "1970-01-01"),
                  total_starts = list(series_id = "HOUST", freq = "Monthly", cosd = "1959-01-01", coed = "2026-01-01", nd = "1959-01-01"),
                  total_comps = list(series_id = "COMPUTSA", freq = "Monthly", cosd = "1968-01-01", coed = "2026-01-01", nd = "1968-01-01"),
                  vacancy_rt = list(series_id = "RRVRUSQ156N", freq = "Quarterly", cosd = "1956-01-01", coed = "2026-01-01", nd = "1956-01-01"),
                  trs10yr_rt = list(series_id = "DGS10", freq = "Daily", cosd = "1970-01-01", coed = "2026-03-31", nd = "1962-01-02"), 
                  frm30yr_rt = list(series_id = "MORTGAGE30US", freq = "Weekly%2C%20Ending%20Thursday", cosd = "1972-01-01", coed = "2026-03-31", nd = "1971-04-02"),
                  ppi = list(series_id = "WPUSI012011", freq = "Monthly", cosd = "1970-01-01", coed = "2026-02-01", nd = "1947-01-01"),
                  hmi = "https://www.nahb.org/-/media/NAHB/news-and-economics/docs/housing-economics/hmi/2026-03/t2-national-hmi-history-202603.xls?rev=2b552d4318914cdc9a4ff257c2fd5959&hash=29606A9050FFC48A7AC571B288F01918"
                  )

# create a blank list to pass the downloaded files into
mf_vars <- list()

# loop through the names in the url list, download the appropriate files, and add the df's into the blank list
for (i in names(mf_urls))
  {var <- mf_urls[[i]]

   if (i == "hmi") 
     {download.file(var, glue("~/RStudio/NAHB/NAHB_{i}.xls"))
  
      # previewed excel file online, 2nd row is column names, the first 2 rows need to be dropped, and df needs to be pivoted
      mf_vars[[i]] <- read_excel(glue("~/RStudio/NAHB/NAHB_{i}.xls")) %>%
                        setNames(as.character(.[2, ])) %>%                                           # use 2nd row as headers
                        rename(year = 1) %>%                                              # add a header for the first column
                        slice(-(1:2)) %>%                                                                 # drop first 2 rows
                        pivot_longer(-year, names_to = "month", values_to = "var") %>%         
                        unite("date", month, year, sep = " ") %>%           # unite combines and drops month and year columns
                        mutate(date = yearmonth(date),
                        var = as.numeric(var),
                        var_desc = !!i) 
      } else 
          # glue the ltsm_url list elements into the link
          # link is too long to fit in one line, so it was separated...some of these link elements need to be dropped
          {fred_url_template <- glue("https://fred.stlouisfed.org/graph/fredgraph.csv?",
                                     "bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&",
                                     "height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&",
                                     "width=1140&nt=0&thu=0&trc=0&",
                                     "show_legend=yes&show_axis_titles=yes&show_tooltip=yes&",
                                     "id={var$series_id}&scale=left&cosd={var$cosd}&coed={var$coed}&",
                                     "line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&",
                                     "mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&",
                                     "fq={var$freq}&fam=avg&fgst=lin&fgsnd=2020-02-01&",
                                     "line_index=1&transformation=lin&",
                                     "vintage_date=2026-01-01&revision_date=2026-01-01&nd={var$nd}")

download.file(fred_url_template, glue("~/RStudio/FRED/Housing/FRED_{i}.csv"))

# df's with different periods need to be synced - EOM values for weekly and daily data, and same value for all months for Q data
mf_vars[[i]] <- read_csv(glue("~/RStudio/FRED/Housing/FRED_{i}.csv")) %>%
                  rename(date = observation_date, var = 2) %>%        # !!i := assigns alias i instead of literal
                  arrange(date) %>%
                  fill(var, .direction = "down") %>%
                  group_by(year(date), month(date)) %>%                                           
                  slice_max(date) %>%                                  # keep EOM dates for weekly and daily data
                  ungroup() %>%
                  select(date, var) %>%
                  mutate(date = floor_date(date, "month"),
                  var_desc = !!i) %>%
                  complete(date = seq(min(date), max(date), by = "month")) %>%   # fill missing months for Q data
                  fill(c(var,var_desc), .direction = "down")                     # fill missing values for Q data
           }
}

# *************************************************************************************************************************
# DATA PREPARATION
# *************************************************************************************************************************

# combine the lists together into a single df and pivot it back into a time-series
hist.mf_fcast_vars <- pmap_df(list(mf_vars), bind_rows) %>%
                      pivot_wider(names_from = var_desc, values_from = var) %>%
                      filter(!is.na(mf_underway)) %>%
                      mutate(cast_desc = "actual") 

# pull the mf factor names into a list from the headers so we can reference them later
mf_fcast_vars <- hist.mf_fcast_vars %>% select(-c(date, cast_desc)) %>% names()

# create the start date to forecast from
end_factor <- if (vintage == "retro") {vintage_date %m-% months(1)} else {max(hist.mf_fcast_vars$date)}
cast_vintage <- if (vintage == "retro") {vintage_date} else {end_factor %m+% months(1)}
# end_cast <- ceiling_date(max(hist.mf_fcast_vars$date), "year")
# cast_hz <- lubridate::interval(start_cast, end_cast) / months(1)

# add future months...
hist.mf_fcast_dummy_vars <- hist.mf_fcast_vars %>%
                            filter(date <= end_factor) %>%
                            bind_rows(data.frame(cast_desc = "forecast",
                                                 date = seq.Date(from = cast_vintage, by = "month", length.out = 3)))
# ...and dummy month variables
for (i in 1:11)
{hist.mf_fcast_dummy_vars <- hist.mf_fcast_dummy_vars %>%
                             # adds 1 if the month index = month(date) is TRUE, otherwise 0
                             mutate("{glue('month_{i}')}" := as.integer(month(date) == i))} 

# pull the dummy months into a list from the headers so we can reference them later
month_vars <- hist.mf_fcast_dummy_vars %>% select(matches("^month_[0-9]+$")) %>% names() 

# create a list for the lag of each mf factor
lag_list <- list(hmi = 24, total_starts = 12, total_comps = 3, vacancy_rt = 12, trs10yr_rt = 24, frm30yr_rt = 24, ppi = 3)

# pull the end of lag-year target
target_eoly <- hist.mf_fcast_vars %>% 
               filter(date == floor_date(max(date), "year") %m-% months(1)) %>% 
               select(mf_underway) %>% 
               pull()

hist.mf_flagcast_dummy_vars <- hist.mf_fcast_dummy_vars

for (i in names(lag_list))
  {hist.mf_flagcast_dummy_vars <- hist.mf_flagcast_dummy_vars %>% 
                                  arrange(date) %>%
                                  mutate(across(all_of(i), ~ lag(.x, lag_list[[i]]), .names = "{.col}_lag{lag_list[[i]]}"),
                                         target_yoy = lag(mf_underway, 12),
                                         date = yearmonth(date)) %>%
                                  tsibble(index = date)}

cast_split <- split(hist.mf_flagcast_dummy_vars, hist.mf_flagcast_dummy_vars$cast_desc)
 
fitcast.mf_flagcast_dummy_vars <- cast_split$actual
cast.mf_flagcast_dummy_vars <- cast_split$forecast

# *************************************************************************************************************************
# MODEL SPECIFICATION
# *************************************************************************************************************************

# match factors from mf_fcast_vars to factors in the lag_list - this will drop mf_underway
mf_fcast_lags <- mf_fcast_vars[mf_fcast_vars %in% names(lag_list)]
# create a new list combining mf_cast_vars with lag_list (i.e. hmi_lag1)
mf_flagcast_vars <- glue("{mf_fcast_lags}_lag{lag_list[mf_fcast_lags]}")

# create a vector with of the significant mf factors...this requires performing an attribute analysis
sig_list <- c("hmi", "total_comps", "vacancy_rt", "ppi")
# match factors from mf_flagcast_vars to factors in the sig_list - this will keep only the lagged sig vars
sig_flagcast_vars <- mf_flagcast_vars[str_detect(mf_flagcast_vars, glue_collapse(sig_list, '|'))]

# create the forecast functions
# multivariate regression with all lagged variables
ltsm_flagcast <- glue("mf_underway ~ {glue_collapse(mf_flagcast_vars, '+')}")
# pure ar1 forecast
pure_ar1_fcast <- glue("mf_underway ~ pdq(1,1,0) + PDQ(0,0,0) + {glue_collapse(month_vars, '+')}")
# ar1 with all lagged variables
ar1_all_vars_flagcast <- glue("mf_underway ~ pdq(1,1,0) + PDQ(0,0,0) + {glue_collapse(c(mf_flagcast_vars, month_vars), '+')}")
# pure ar2 forecast
pure_ar2_fcast <- glue("mf_underway ~ pdq(2,1,0) + PDQ(0,0,0) + {glue_collapse(month_vars, '+')}")
# ar2 with all lagged variables
ar2_all_vars_flagcast <- glue("mf_underway ~ pdq(2,1,0) + PDQ(0,0,0) + {glue_collapse(c(mf_flagcast_vars, month_vars), '+')}")
# ar2 with significant lagged variables...significance was determined through manual checks
ar2_sig_vars_flagcast <- glue("mf_underway ~ pdq(2,1,0) + PDQ(0,0,0) + {glue_collapse(c(sig_flagcast_vars, month_vars), '+')}")

# *************************************************************************************************************************
# MODEL FITS & FORECASTS
# *************************************************************************************************************************

fit.models_mf_units <- fitcast.mf_flagcast_dummy_vars %>% 
                       model(`Regression` = TSLM(as.formula(ltsm_flagcast)),
                             `Pure ar(1)` = ARIMA(as.formula(pure_ar1_fcast)),
                             `ar(1) + All Factors` = ARIMA(as.formula(ar1_all_vars_flagcast)),
                             `Pure ar(2)` = ARIMA(as.formula(pure_ar2_fcast)),
                             `ar(2) + All Factors` = ARIMA(as.formula(ar2_all_vars_flagcast)),
                             `ar(2) + Select Factors` = ARIMA(as.formula(ar2_sig_vars_flagcast)))

fcast.models_mf_units <- fit.models_mf_units %>% forecast(cast.mf_flagcast_dummy_vars)

mf_underway_fcast <- list(histcast.mf_underway =  data.frame(fitcast.mf_flagcast_dummy_vars) %>%
                                                  bind_rows(data.frame(fcast.models_mf_units) %>% 
                                                            select(-mf_underway) %>% 
                                                            rename(mf_underway = .mean)) %>%
                                                  select(-matches("^month_[0-9]+$")),
                          stats.mf_underway = data.frame(glance(fit.models_mf_units)) %>% 
                                              left_join(data.frame(accuracy(fit.models_mf_units)), by = ".model") %>%
                                              select(-c(ar_roots, ma_roots, .type)) %>%
                                              pivot_longer(-.model, names_to = "Statistic", values_to = "stat_value") %>%
                                              pivot_wider(names_from = .model, values_from = stat_value),
                          coefs.mf_underway = data.frame(coef(fit.models_mf_units)),
                          fcast.mf_underway = fcast.models_mf_units %>%
                                              hilo(95) %>%
                                              as.data.frame() %>% 
                                              unpack_hilo(`95%`) %>% 
                                              rename(lower = `95%_lower`, upper = `95%_upper`) %>%
                                              mutate(vintage = cast_vintage,
                                                     std_dev = (upper - lower) / (2 * 1.96),
                                                     steps_ahead = lubridate::interval(vintage, ym(date)) / months(1),
                                                     target_eoly = target_eoly,
                                                     target_eoly_prob = 1 - pnorm(target_eoly, .mean, std_dev),
                                                     target_yoy85 = target_yoy * .85,
                                                     target_yoy85_prob = 1 - pnorm(target_yoy85, .mean, std_dev)) %>%
                                              select(vintage, date, steps_ahead, .model, mf_underway = .mean, lower, upper, target_eoly, target_eoly_prob, target_yoy85, target_yoy85_prob),
                          bound.mf_underway = fcast.models_mf_units %>%
                                              hilo(95) %>%
                                              as.data.frame() %>% 
                                              unpack_hilo(`95%`) %>% 
                                              select(date, .model, lower = `95%_lower`, upper = `95%_upper`) %>%
                                              pivot_longer(c(lower, upper), names_to = "bound", values_to = "mf_underway"))
                          
for (i in names(mf_underway_fcast)) {write_csv(mf_underway_fcast[[i]], glue("./Forecast/{i}_",format.Date(cast_vintage, "%b%Y"),".csv"))}

# create a list of just the forecast files 
fcast_files <- list.files("./Forecast", pattern = c("fcast", ".csv"))

# load the forecast files into a list
fcast_list <- list()
for (i in fcast_files) {fcast_list[[i]] <- read_csv(glue("./Forecast/{i}"))}

hist.castmf_underway <- pmap_df(list(fcast_list), bind_rows) %>%
                        group_by(date, .model) %>%
                        arrange(desc(steps_ahead), .by_group = TRUE) %>%
                        mutate(fof_delta = mf_underway / lag(mf_underway) - 1)

write_csv(hist.castmf_underway, "./Report/hist.castmf_underway.csv")



