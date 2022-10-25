

# notes -------------------------------------------------------------------

# https://github.com/hrecht/censusapi
# https://www.hrecht.com/censusapi/


# documentation -----------------------------------------------------------
# variable info here:
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-total-cities-and-towns.html


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions_utility.r"))

# devtools::install_github("hrecht/censusapi")
library(censusapi)


# urls and file names -----------------------------------------------------
# uny_20102016 for 2010-2016 -- appears to be last vintage that includes village of Salem - in case we want Salem
uny_20102016 <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/cities/totals/sub-est2016_36.csv"

# most recent vintage for 2010-2020
uny_20102020 <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/cities/SUB-EST2020_36.csv"
path_file(uny_20102020)

# 2020+ data
uny_20202021 <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/cities/totals/sub-est2021_36.csv"
path_file(uny_20202021)


# download poipulation estimates ---------------------------------------------------------------------
download.file(uny_20102020, here::here("data", path_file(uny_20102020)), mode="wb")
download.file(uny_20202021, here::here("data", path_file(uny_20202021)), mode="wb")


# read the pop estimates  -------------------------------------------------
# get the two time periods
pop1 <- read_csv(here::here("data", path_file(uny_20102020)), col_types = cols(.default = col_character()))
pop2 <- read_csv(here::here("data", path_file(uny_20202021)), col_types = cols(.default = col_character()))

glimpse(pop1)
glimpse(pop2)
idvars <- intersect(names(pop1), names(pop2)) |> str_subset("POPESTIMATE2020", negate = TRUE)
(idvars <- str_to_lower(idvars))


## long files ----
pop1a <- pop1 |> lcnames() |> pivot_longer(cols = -all_of(idvars), names_to = "variable")
pop2a <- pop2 |> lcnames() |> pivot_longer(cols = -all_of(idvars), names_to = "variable")

## combine time periods ----
pop3 <- bind_rows(pop1a |> mutate(src="2010"),
                  pop2a |> mutate(src="2020")) |> 
  mutate(value=as.numeric(value))
glimpse(pop3)
summary(pop3)

# Investigate before cleaning file ----
## why NAs? what happened to Salem? ----
pop3 |> filter(is.na(value)) # Kiryas Joel and a few other places

# look at the Salem records in the two time periods
pop1 |> 
  filter(COUNTY=="115", str_detect(NAME, "Salem")) |> 
  pull(NAME) # we only have the town in this vintage

pop2 |> 
  filter(COUNTY=="115", str_detect(NAME, "Salem")) |> 
  pull(NAME) # we only have the town in this vintage

pop3 |> filter(county=="115", str_detect(name, "Salem"))

## play around to figure out geography ----
tmp <- pop3 |> filter(county=="115")
count(tmp, place, cousub, concit, primgeo_flag, funcstat, name)
count(tmp |> filter(place!="00000"), place, cousub, concit, primgeo_flag, funcstat, name)
count(tmp |> filter(place!="00000", primgeo_flag=="0"), place, cousub, concit, primgeo_flag, funcstat, name)

# how to identify just the munis we want?
# place 00000 gives Wash Co (cousub 00000), all towns
# place !00000 
#   and funcstat F gives balances
#   and primgeo_flag 0 gives villages (funcstat A) and balance county (funcstat F)

cntyrec <- expression(place=="00000" & cousub=="00000")
vlgrec <- expression(place!="00000" & cousub=="00000" & primgeo_flag=="0" & funcstat=="A")
townrec <- expression(place=="00000" & cousub!="00000")

count(tmp |> filter(eval(cntyrec)), place, cousub, concit, primgeo_flag, funcstat, name)
count(tmp |> filter(eval(vlgrec)), place, cousub, concit, primgeo_flag, funcstat, name)
count(tmp |> filter(eval(townrec)), place, cousub, concit, primgeo_flag, funcstat, name)

count(washco, place, cousub, concit, primgeo_flag, funcstat, name)

# note that we have two values for popestimate2020 -- one from the 2010 file and one from the 2020 file
tmp |> 
  filter(name=="Hudson Falls village", primgeo_flag=="0", str_detect(variable, "popestimate20"))

tmp |> 
  filter(eval(vlgrec), str_detect(name, "Cambridge"), str_detect(variable, "popestimate20"))

tmp |> 
  filter(eval(vlgrec), str_detect(name, "Cambridge"))

count(tmp, variable)

# create a Washington County file ----
cntyrec <- expression(place=="00000" & cousub=="00000")
vlgrec <- expression(place!="00000" & cousub=="00000" & primgeo_flag=="0" & funcstat=="A")
townrec <- expression(place=="00000" & cousub!="00000")

estpop <- expression(str_detect(variable, "popestimate20") &
                      !(variable=="popestimate2020" & src=="2010")) # use the 2020 source for 2020
cenpop <- expression(variable %in% c("census2010pop", "estimatesbase2020"))

washco_ests1 <- pop3 |> 
  mutate(munitype=case_when(eval(cntyrec) ~ "county",
                            eval(vlgrec) ~ "village",
                            eval(townrec) ~ "town",
                            TRUE ~ "other"),
         vtype=case_when(eval(estpop) ~ "estimate",
                         eval(cenpop) ~ "census",
                         TRUE ~ "other")) |> 
  filter(county=="115") |> 
  filter(eval(cntyrec) | eval(vlgrec) | eval(townrec)) |> 
  mutate(year=ifelse(variable=="census2010pop", "2010", str_sub(variable, -4, -1)),
         year=as.integer(year)) |> 
  arrange(place, cousub, name, year)
glimpse(washco_ests1)
count(washco_ests1, name)
count(washco_ests1, vtype)
count(washco_ests1, munitype)

saveRDS(washco_ests1, here::here("data", "washpop.rds"))


# explore pop changes -----------------------------------------------------
washpop <- readRDS(here::here("data", "washpop.rds"))
count(washpop, name)

yrs <- c(2010, 2021)
yrs <- c(2015, 2021)
washpop |> 
  filter(vtype=="estimate",
         munitype %in% c("county", "village"),
         year %in% yrs) |> 
  select(name, year, value) |> 
  pivot_wider(names_from = year, names_prefix="y") |> 
  mutate(change=unlist(cur_data()[3]) - unlist(cur_data()[2]),
         pch=change / unlist(cur_data()[2])) |> 
  arrange(desc(pch))

washpop |> 
  filter(vtype=="estimate",
         year %in% c(2010, 2021)) |> 
  select(name, year, value) |> 
  pivot_wider(names_from = year, names_prefix="y") |> 
  mutate(change=y2021 - y2010, pch=change / y2010) |> 
  arrange(desc(pch))


washco1 |> 
  filter(str_detect(name, "Washington"))

washco2 <- washco1 |> 
  group_by(name, year, value) |> 
  arrange(place, cousub, concit, primgeo_flag, funcstat) |> 
  filter(row_number()==1) |> 
  ungroup()

count(washco2, name)

washcopop <- washco2 |> 
  select(stname, county, place, cousub, concit, name, year, pop=value)
count(washcopop, name)
saveRDS(washcopop, here::here("data", "washcopop.rds"))

count(washcopop, place, cousub, name)

places <- count(washcopop, place, cousub, name)
pparts <- c("Washing", "Cambridge", "Greenwich", "Creek", "Easton", "Jackson")
keep <- places |> 
  filter(str_detect_any(name, pparts))
keep


washcopop |> 
  right_join(keep |> select(place, cousub), by = c("place", "cousub")) |> 
  mutate(type=ifelse(str_detect(name, "County"), "county", "subcounty")) |> 
  group_by(name) |> 
  mutate(ivalue=pop / pop[year==2010]) |> 
  ungroup() |> 
  ggplot(aes(year, ivalue)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  scale_x_continuous(breaks=2010:2021) +
  # scale_y_continuous(breaks=seq(100, 10000, 25)) +
  scale_y_continuous(breaks=seq(0, 2, .01), labels = scales::percent_format(accuracy=1)) +
  theme_bw() +
  facet_wrap(~type+name, ncol=3, scales = "fixed")


# packages ----------------------------------------------------------------




get_estimates(
  geography,
  product = NULL,
  variables = NULL,
  breakdown = NULL,
  breakdown_labels = FALSE,
  year = 2019,
  state = NULL,
  county = NULL,
  time_series = FALSE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE,
  ...
)


df2019cousub <- get_estimates(
  geography="county subdivision",
  product = "population",
  # variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  year = 2019,
  state = "NY",
  county = "Washington",
  time_series = TRUE,
  output = "tidy" #,
  # geometry = FALSE,
  # keep_geo_vars = FALSE,
  # shift_geo = FALSE,
  # key = NULL,
  # show_call = FALSE,
  # ...
)
count(df2019cousub, NAME)


dfunk <- get_estimates(
  geography="county subdivision",
  product = "population",
  # variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2021,
  state = "NY",
  county = "Washington",
  time_series = TRUE,
  output = "tidy" #,
  # geometry = FALSE,
  # keep_geo_vars = FALSE,
  # shift_geo = FALSE,
  # key = NULL,
  # show_call = FALSE,
  # ...
)
count(dfunk, DATE)

count(df, NAME)


df2 <- df |> 
  lcnames() |> 
  mutate(variable=str_to_lower(variable))

count(df2, geoid, name)
count(df2, variable)
count(df2, date)

# date codes https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html
# 1 = April 1, 2010 Census 
# 2 = April 1, 2010 population estimates base
# 3 = July 1, 2010 population estimate
# 4 = July 1, 2011 
# 12 = July 1, 2019 population estimate


# geoid=="3611511836",  Cambridge town
df2 |> 
  filter(variable=="pop", date %in% 3:12) |> 
  mutate(year=date + 2007) |> 
  group_by(geoid, name) |> 
  mutate(ivalue=value / value[year==2010]) |> 
  ungroup() |> 
  ggplot(aes(year, ivalue)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  scale_x_continuous(breaks=2010:2020) +
  # scale_y_continuous(breaks=seq(100, 10000, 25)) +
  scale_y_continuous(breaks=seq(0, 2, .01), labels = scales::percent_format(accuracy=1)) +
  theme_bw() +
  facet_wrap(~geoid+name, ncol=4, scales = "fixed")


acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region = "place:*",
  regionin = "state:01")


apis <- listCensusApis()

tmp <- count(apis, title, sort=TRUE)

tmp <- apis |> 
  filter(str_detect(title, coll("decennial", ignore_case = TRUE)))
tmp1 <- tmp |> filter(vintage==2010)

popapis <- apis  |> 
  filter(str_detect(contact, coll("population.estimate", ignore_case = TRUE)))
# : Population Estimates
popapis <- apis  |> 
  filter(str_detect(title, coll(": Population Estimates", ignore_case = TRUE)))
# name==pep/population and vintage 2021 or 2019

popvars <- listCensusMetadata(
  name = "pep/population",
  vintage=2019,
  type = "variables")

popgeo <- listCensusMetadata(
  name = "pep/population",
  vintage=2019,
  type = "geographies") # geographies


# POP_2020 and POP_2021 are pop names from the vintage 2021 data
# also POP_BASE2020 Estimates Base Population, April 1, 2020
# POP from 2019 vintage

# 2021 only goes down to the state level

popdf1 <- getCensus(
  name = "pep/population",
  vintage = 2019,
  vars = c("NAME", "POP", "COUNTY"),
  region = "place",
  regionin = "state:36+county:113")



acspop <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region = "place:*",
  regionin = "state:01")

tmp <- apis |> 
  filter(str_detect(name, "acs1"))

