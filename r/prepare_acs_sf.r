
# libraries ----------------------------------------------------------------
library(tidyverse)
tprint <- 50  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

# tools
library(vroom)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(RcppRoll)
library(fredr)
library(btools)
library(tidycensus)
library(archive)

# graphics
library(scales)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)

# locations ----------------------------------------------------------------
dny2009 <- r"(E:\data\acs\sf\2009_5year\ny\)"
dny2014 <- r"(E:\data\acs\sf\2014_5year\ny\)"
dny2019 <- r"(E:\data\acs\sf\2019_5year\ny\)"

# constants ----------------------------------------------------------------

# notes ----------------------------------------------------------------
# vroom(con, col_names=FALSE, 
#     col_types=cols(X1="c", X2="c", X3="c", X4="c", X5="c", X6="c",
#                    .default=col_double()))


# functions ----------------------------------------------------------------
f1 <- function(year){
  zpath <- paste0(r"(E:\data\acs\sf\)", year, r"(_5year\ny\NewYork_All_Geographies_Not_Tracts_Block_Groups.zip)")
  print(zpath)
  files <- unzip(zpath, list=TRUE)
  read_files <- files %>%
    filter(str_sub(Name, 1, 2) %in% c("e2", "m2"))
  # print(read_files$Name)
  g1 <- function(fname){
    # print(fname)
    # on.exit(close(con))
    con <- archive_read(zpath, fname)
    vroom(con, col_names=FALSE, 
          col_types=cols(X1="c", X2="c", X3="c", X4="c", X5="c", X6="c",
                         .default=col_double()))
  }
  # g1(read_files$Name[1])
  df <- map_dfr(read_files$Name, g1)
  df
}

# prepare data for each year ----------------------------------------------

year <- 2009 # 2009 2014 2019
df <- f1(year)
glimpse(df)
ht(df)

d <- paste0(r"(E:\data\acs\sf\)", year, r"(_5year\ny\)")

idvars <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")
vnames <- c(idvars, paste0("x", 1:(ncol(df) - length(idvars))))
length(vnames)
ncol(df)
df <- df %>% setNames(vnames)
write_parquet(df, paste0(d, "em", year, "5ny.parquet"))

# test using dataset method
ds <- open_dataset(paste0(d,  "em", year, "5ny.parquet"), 
                   partitioning = c("sequence", "logrecno"))

test1 <- ds %>%
  filter(as.integer(sequence)==2) %>%
  select(c(idvars, "x100", "x101", "x102")) %>%
  as_tibble()

# test using traditional read method
test1 <- read_parquet(paste0(d,  "em", year, "5ny.parquet"),
                   col_select=c(all_of(idvars), x100:x102)) %>%
  filter(as.integer(sequence)==2)


# prepare geo definition files --------------------------------------------
sumlevels <- readRDS(paste0(d, "sumlevels.rds"))

#.. get 2009 first because it has a different file structure ----
# https://www2.census.gov/programs-surveys/acs/summary_file/2009/

zpath <- paste0(dny2009, "NewYork_All_Geographies_Not_Tracts_Block_Groups.zip")
fname <- "g20095ny.txt"

# "component", "logrecno", "geoid", "geoname"
#fwf_positions(start, end = NULL, col_names = NULL)
# define var start, end locations
fcols <- fwf_cols(stabbr = c(7, 8), 
                  sumlevel = c(9, 11),
                  component = c(12, 13),
                  logrecno = c(14, 20),
                  geoid = c(179, 200),
                  geoname=c(219, 1218))
con <- archive_read(zpath, fname)
df2009 <- read_fwf(con, fcols, col_types="cccicc") %>%
  mutate(year=2009) %>%
  select(year, stabbr, sumlevel, component, logrecno, geoid, geoname)

saveRDS(df2009, paste0(dny2009, "geo2009ny.rds"))

# df <- vroom(con, col_names=FALSE, 
#             col_types=cols(.default="c"))
#.. get 2014 and 2019 ---------------------
getgeo <- function(year){
  d <- paste0(r"(E:\data\acs\sf\)", year, r"(_5year\ny\)")
  zpath <- paste0(r"(E:\data\acs\sf\)", year, r"(_5year\ny\NewYork_All_Geographies_Not_Tracts_Block_Groups.zip)")
  print(zpath)
  files <- unzip(zpath, list=TRUE)
  # g20095ny.txt 
  fname <- paste0("g", year, "5ny.csv")
  con <- archive_read(zpath, fname)
  
  df <- vroom(con, col_names=FALSE, 
              col_types=cols(X5="i", .default="c")) # logrecno is integer
  
  # slim it down
  vkeep <- c(1:5, 49:50)
  vnames <- c("fileid", "stabbr", "sumlevel", "component", "logrecno", "geoid", "geoname")
  print(vnames)
  
  df2 <- df %>%
    select(vkeep) %>%
    setNames(vnames) %>%
    mutate(year=!!year) %>%
    select(year, stabbr, sumlevel, component, logrecno, geoid, geoname)
  df2
}

df2014 <- getgeo(2014)
saveRDS(df2014, paste0(dny2014, "geo2014ny.rds"))

df2019 <- getgeo(2019)
saveRDS(df2019, paste0(dny2019, "geo2019ny.rds"))


#.. create a combined geo file ----
geoall1 <- bind_rows(df2009, df2014, df2019)




# I use Census names other than: stusab-->stabbr, name-->geoname 
vkeep <- c(1:5, 49:51)
vnames <- c("fileid", "stabbr", "sumlevel", "component", "logrecno", "geoid", "geoname", "year")
geo20191 <- getgeo(2019)
geo20192 <- geo20191 %>%
  select(vkeep) %>%
  setNames(all_of(vnames)) %>%
  mutate(year=!!year)
  select(stabbr, geoid, geoname)


# here are items 1-5 and 49-50
# FILEID Always equal to ACS Summary File identification 6 1 Record
# STUSAB State Postal Abbreviation 2 7 Record
# SUMLEVEL Summary Level 3 9 Record
# COMPONENT Geographic Component 2 12 Record
# LOGRECNO Logical Record Number 7 14 Record
# ...
# GEOID Geographic Identifier 40 179 Geographic
# NAME Area Name 1000 219 Geographic


# prepare table and variable definition files  ----------------------------



# get data ----------------------------------------------------------------
# Tables are grouped numerically by the “root” of their Table ID, (for example,
# Table B00001 is in sequence file 0001). • Tables with race iterations are
# grouped in the same sequence.



zpath1 <- r"(E:\data\acs\sf\2019_5year\NewYork_All_Geographies_Not_Tracts_Block_Groups.zip)"
seq <- 1
(fn <- paste0("e2019ny", str_pad(seq, width=4, side="left", pad="0"), "000.txt"))
con1 <- archive_read(zpath1, fn)
df1 <- read_csv(con1, col_names=FALSE)
glimpse(df1)





fn <- "g20195ny.csv"
fne <- "e20195ny.txt"
fnm <- "e20195ny.txt"

geog <- read_csv(paste0(d, fn), col_names = FALSE)
ests <- vroom(paste0(d, fne), col_names = FALSE)
moe <- vroom(paste0(d, fne), col_names = FALSE)

idvars <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")
vnames <- c(idvars, paste0("x", 1:(ncol(ests) - length(idvars))))
length(vnames)
em20195ny <- bind_rows(ests, moe) %>%
  setNames(vnames)

saveRDS(em20195ny, paste0(d, "em20195ny.rds"))

write_parquet(em20195ny, paste0(d, "em20195ny.parquet"))


cols <- c(idvars, )
df <- read_parquet(paste0(d, "em20195ny.parquet"),
                   col_select=c(all_of(idvars), x100:x102)) %>%
  filter(as.integer(sequence)==2)

ds <- open_dataset(paste0(d, "em20195ny.parquet"), 
                   partitioning = c("sequence", "logrecno"))

djb <- ds %>%
  filter(as.integer(sequence)==2) %>%
  select(c(idvars, "x100", "x101", "x102")) %>%
  as_tibble()

#   SEQUENCE table  ncols firstcol lastcol
#      <int> <chr>  <int>    <int>   <int>
# 1        2 B01002     3       94      96

tmp <- seqtab %>%
  mutate(rownum=row_number()) %>%
  group_by(SEQUENCE) %>%
  mutate(seqcolnum=row_number()) %>%  # the column number within the sequence
  ungroup

# this is the mapping we need to pick a table
seqtabmap <- tmp %>%
  group_by(SEQUENCE, table) %>%
  summarise(ncols=n(), firstcol=min(seqcolnum), lastcol=max(seqcolnum),
            .groups="drop")

tab <- "B01002"
tabvals <- seqtabmap %>% filter(table==tab)
seq <- tabvals$SEQUENCE
fcol <- tabvals$firstcol + 6
lcol <- tabvals$lastcol + 6
(fn <- paste0("e2019ny", str_pad(seq, width=4, side="left", pad="0"), "000.txt"))


zpath1 <- r"(E:\data\acs\sf\2019_5year\ny\NewYork_All_Geographies_Not_Tracts_Block_Groups.zip)"
showConnections()
closeAllConnections()
on.exit(close(con1))
con1 <- archive_read(zpath1, "e20195ny0001000.txt")
df1 <- vroom(con1, col_names=FALSE)
close(con1)
glimpse(df1)

zpath2 <- r"(E:\data\acs\sf\2019_5year\2019_5yr_Summary_FileTemplates.zip)"
con2 <- archive_read(zpath2, "seq1.xlsx")
df2 <- read_excel(con2)

read_excel(path=unz(zpath2, filename= "seq1.xlsx"))

f <- function(num){
  zpath2 <- r"(E:\data\acs\sf\2019_5year\2019_5yr_Summary_FileTemplates.zip)"
  fn <- paste0("seq", num, ".xlsx")
  df <- read_excel(path=unzip(zpath2, files=fn))
  df %>%
    mutate(SEQUENCE=num) %>%
    select(-c(FILEID, FILETYPE, STUSAB, CHARITER, LOGRECNO)) %>%
    pivot_longer(-SEQUENCE) %>%
    separate(name, into=c("table", "vnum")) %>%
    mutate(vnum=as.integer(vnum))
}
df <- map_dfr(1:141, f)
ns(df)
saveRDS(df, here::here("data", "seqtables_20195.rds"))

seqtab <- readRDS(here::here("data", "seqtables_20195.rds"))
ns(seqtab)

tmp <- seqtab %>%
  mutate(rownum=row_number()) %>%
  group_by(SEQUENCE) %>%
  mutate(seqcolnum=row_number()) %>%  # the column number within the sequence
  ungroup

# this is the mapping we need to pick a table
seqtabmap <- tmp %>%
  group_by(SEQUENCE, table) %>%
  summarise(ncols=n(), firstcol=min(seqcolnum), lastcol=max(seqcolnum),
            .groups="drop")

tab <- "B01002"
tabvals <- seqtabmap %>% filter(table==tab)
seq <- tabvals$SEQUENCE
fcol <- tabvals$firstcol + 6
lcol <- tabvals$lastcol + 6
(fn <- paste0("e2019ny", str_pad(seq, width=4, side="left", pad="0"), "000.txt"))
con1 <- archive_read(zpath1, fn)
df1 <- read_csv(con1, col_names=FALSE) %>%
  select(1:6, fcol:lcol)
glimpse(df1)

closeAllConnections()

count(seqtab, SEQUENCE, table)
count(seqtab, SEQUENCE, table) %>%
  group_by(SEQUENCE) %>%
  mutate(nvars=sum(n))

df %>%
  filter(table=="B01001")



zpath1 <- r"(E:\data\acs\sf\2019_5year\NewYork_All_Geographies_Not_Tracts_Block_Groups.zip)"
seq <- 1
(fn <- paste0("e2019ny", str_pad(seq, width=4, side="left", pad="0"), "000.txt"))
con1 <- archive_read(zpath1, fn)
df1 <- read_csv(con1, col_names=FALSE)
glimpse(df1)
# FILEID	FILETYPE	STUSAB	CHARITER	SEQUENCE	LOGRECNO

idvars <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")
vnames <- c(idvars, paste0("x", 1:(ncol(df1) - length(idvars))))
length(vnames)
vnames

df2 <- df1 %>%
  setNames(vnames)
glimpse(df2)


# save as rds: sumlevel codes ----------------------------------------------------------
# https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file/carto-boundary-summary-level.html
sumlevels <- read_fwf(
"
020 Region
030 Division
040 State
050 State-County
060 State-County-County Subdivision
067 State-County-County Subdivision-Subminor Civil Division
140 State-County-Census Tract
150 State-County-Census Tract-Block Group
160 State-Place
170 State-Consolidated City
230 State-Alaska Native Regional Corporation
250 American Indian Area/Alaska Native Area/Hawaiian Home Land
251 American Indian Area-Tribal Subdivision/Remainder
252 American Indian Area/Alaska Native Area (Reservation or Statistical Entity Only)
254 American Indian Area (Off-Reservation Trust Land Only)/Hawaiian Home Land
256 American Indian Area-Tribal Census Tract
258 American Indian Area-Tribal Census Tract-Tribal Block Group
310 Metropolitan Statistical Area/Micropolitan Statistical Area
314 Metropolitan Statistical Area-Metropolitan Division
330 Combined Statistical Area
332 Combined Statistical Area-Metropolitan Statistical Area/Micropolitan Statistical Area
335 Combined New England City and Town Area
337 Combined New England City and Town Area-New England City and Town Area
350 New England City and Town Area
352 New England City and Town Area-State-Principal City
355 New England City and Town Area (NECTA)-NECTA Division
361 State-New England City and Town Area-Principal City
500 State-Congressional District (111th)
610 State-State Legislative District (Upper Chamber)
620 State-State Legislative District (Lower Chamber)
700 State-County-Voting District/Remainder
860 5-Digit ZIP code Tabulation Area
950 State-School District (Elementary)/Remainder
960 State-School District (Secondary)/Remainder
970 State-School District (Unified)/Remainder
", fwf_cols(sumlevel=c(1, 3), geotype=c(5, 1000))
)
d <- r"(E:\data\acs\sf\)"
saveRDS(sumlevels, paste0(d, "sumlevels.rds"))

# Summary Level Code	Summary Level Name
# 020 Region
# 030 Division
# 040 State
# 050 State-County
# 060 State-County-County Subdivision
# 067 State-County-County Subdivision-Subminor Civil Division
# 140 State-County-Census Tract
# 150 State-County-Census Tract-Block Group
# 160 State-Place
# 170 State-Consolidated City
# 230 State-Alaska Native Regional Corporation
# 250 American Indian Area/Alaska Native Area/Hawaiian Home Land
# 251 American Indian Area-Tribal Subdivision/Remainder
# 252 American Indian Area/Alaska Native Area (Reservation or Statistical Entity Only)
# 254 American Indian Area (Off-Reservation Trust Land Only)/Hawaiian Home Land
# 256 American Indian Area-Tribal Census Tract
# 258 American Indian Area-Tribal Census Tract-Tribal Block Group
# 310 Metropolitan Statistical Area/Micropolitan Statistical Area
# 314 Metropolitan Statistical Area-Metropolitan Division
# 330 Combined Statistical Area
# 332 Combined Statistical Area-Metropolitan Statistical Area/Micropolitan Statistical Area
# 335 Combined New England City and Town Area
# 337 Combined New England City and Town Area-New England City and Town Area
# 350 New England City and Town Area
# 352 New England City and Town Area-State-Principal City
# 355 New England City and Town Area (NECTA)-NECTA Division
# 361 State-New England City and Town Area-Principal City
# 500 State-Congressional District (111th)
# 610 State-State Legislative District (Upper Chamber)
# 620 State-State Legislative District (Lower Chamber)
# 700 State-County-Voting District/Remainder
# 860 5-Digit ZIP code Tabulation Area
# 950 State-School District (Elementary)/Remainder
# 960 State-School District (Secondary)/Remainder
# 970 State-School District (Unified)/Remainder

