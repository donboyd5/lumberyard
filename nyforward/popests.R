
source(here::here("r", "libraries.r"))
source(here::here("r", "functions_utility.r"))

# devtools::install_github("hrecht/censusapi")
library(censusapi)

# https://github.com/hrecht/censusapi
# https://www.hrecht.com/censusapi/


# djb ---------------------------------------------------------------------

# uny_20102016 appears to be the last vintage that includes village of Salem
uny_20102016 <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/cities/totals/sub-est2016_36.csv"

uny_20102020 <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/cities/SUB-EST2020_36.csv"
path_file(uny_20102020)

uny_20202021 <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/cities/totals/sub-est2021_36.csv"
path_file(uny_20202021)

download.file(uny_20102020, here::here("data", path_file(uny_20102020)), mode="wb")
download.file(uny_20202021, here::here("data", path_file(uny_20202021)), mode="wb")


pop1 <- read_csv(here::here("data", path_file(uny_20102020)), col_types = cols(.default = col_character()))
pop2 <- read_csv(here::here("data", path_file(uny_20202021)), col_types = cols(.default = col_character()))

glimpse(pop1)
glimpse(pop2)
idvars <- intersect(names(pop1), names(pop2)) |> str_subset("POPESTIMATE2020", negate = TRUE)
(idvars <- str_to_lower(idvars))

pop1a <- pop1 |> lcnames() |> pivot_longer(cols = -all_of(idvars), names_to = "variable")
pop2a <- pop2 |> lcnames() |> pivot_longer(cols = -all_of(idvars), names_to = "variable")

pop3 <- bind_rows(pop1a |> mutate(src="2010"),
                  pop2a |> mutate(src="2020")) |> 
  mutate(value=as.numeric(value))
glimpse(pop3)
summary(pop3)
tmp <- pop3 |> filter(is.na(value))
tmp <- pop3 |> filter(county=="115")
count(tmp, place, cousub, concit, primgeo_flag, funcstat, name)

# figure out geography
tmp <- pop3 |> 
  filter(county=="115", funcstat!="F")
pop3 |> filter(county=="115", str_detect(name, "Salem"))

count(washco, place, cousub, concit, primgeo_flag, funcstat, name)

tmp |> 
  filter(name=="Hudson Falls village", primgeo_flag=="0", str_detect(variable, "popestimate20"))

washco1 <- pop3 |> 
  # filter(county=="115", funcstat!="F", primgeo_flag=="0", str_detect(variable, "popestimate20")) |>
  filter(county=="115", funcstat!="F", str_detect(variable, "popestimate20"),
         !str_detect(name, "(pt.)")) |> 
  filter(!(variable=="popestimate2020" & src=="2010")) |> # use the 2020 src value
  mutate(year=str_sub(variable, -4, -1) |> as.integer()) |> 
  arrange(place, name, year)

count(washco1, name)

washco1 |> 
  filter(str_detect(name, "Argyle village"), year==2021)

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

