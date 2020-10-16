#> LOAD LIBRARIES ====

library(haven)
library(tidyverse)


#> LOAD AND WRANGLE DATA ==== 

# Load NIDS wave 1 household data, change variable types and
# create log per household income
df_NIDS <- read_dta("NIDS_data_w1.dta") %>% 
  as_tibble %>% 
  select(c("h_dwlrms", "head_age", "hhsizer"), everything()) %>% 
  mutate_at(vars(1:3), as.integer) %>% 
  mutate_at(vars(4:ncol(.)), as_factor) %>% 
  mutate(
    hhincome = as.double(hhincome),
    head_age = as.integer(head_age),
    hhsizer = as.integer(hhsizer),
    h_dwlrms = as.integer(h_dwlrms),
    log_income_pc = log(as.double(hhincome) / as.double(hhsizer))
  )

# Rename some variables, drop others
df_NIDS <- df_NIDS %>% 
  rename(
    members = hhsizer,
    rooms = h_dwlrms,
    dwelling = h_dwltyp,
    prov = hhprov, 
    roof = h_dwlmatroof, 
    wall = h_dwlmatrwll, 
    dwelling_own = h_ownd, 
    water = h_watsrc, 
    toilet = h_toi, 
    toilet_share = h_toishr, 
    electricity = h_enrgelec, 
    landline = h_tellnd, 
    cellphone_use = h_telcel, 
    refuse_removal = h_refrem, 
    street_light = h_strlght, 
    radio = h_ownrad, 
    hifi = h_ownhif, 
    tv = h_owntel, 
    satelite = h_ownsat, 
    video = h_ownvid, 
    computer = h_owncom, 
    camera = h_owncam, 
    cellphone_own = h_owncel, 
    tv_les = h_ownelestv, 
    gas_stove = h_owngasstv, 
    microwave = h_ownmic, 
    fridge = h_ownfrg, 
    washer = h_ownwsh,
  ) %>% 
  select(
    log_income_pc,
    members,
    rooms,
    head_age,
    head_educ,
    everything(),
    -c(
      hhincome, 
      expenditure, 
      hhid, 
      hhgeo, 
      h_ownsew:h_ownwhl, 
      hhcluster:dtwgt, 
      h_transtrain:h_transmini
    ) 
  ) 

# Recode education variable
df_NIDS <- df_NIDS %>% 
  mutate(
    head_educ = as.integer(head_educ),
    head_educ = case_when(
      head_educ <= 4 ~ NA_integer_,
      head_educ >= 5 & head_educ <= 17 ~ head_educ - 5L,
      head_educ >= 18 & head_educ <= 22 ~ 11L,
      head_educ >= 23 & head_educ <= 24 ~ 13L,
      head_educ == 25 ~ 15L,
      head_educ == 26 | head_educ == 27 ~ 16L,
      head_educ == 28 ~ 18L,
      head_educ == 29 ~ NA_integer_,
      head_educ == 30 ~ 0L,
      TRUE ~ head_educ
    )
  ) 

# Define functions and then recode uninformative factor values to NA
set_na_integer <- function(x) {
  if_else(x < 1, NA_integer_, x)
}
set_na_factor <- function(x) {
  na_index = if_else(
    x == "Don't Know" | 
      x == "Refused"  |
      x == "Not Applicable"  |
      x == "Missing" |
      x == "Unit in retirement village",
    TRUE, FALSE);
  is.na(x) <- na_index;
  x
}
reset_factor_levels <- function(x) {
  factor(x, levels = unique(x))
}

df_NIDS <- df_NIDS %>% 
  mutate_at(vars(2:4), set_na_integer) %>% 
  mutate_if(is.factor, set_na_factor) %>% 
  mutate_if(is.factor, reset_factor_levels)


# Drop rows with NA values
df_NIDS <- df_NIDS %>% 
  filter(complete.cases(.))


#> SAVE DATA ==== 
saveRDS(df_NIDS, "df_NIDS.rds")