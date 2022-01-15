
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
library(arrow)

# graphics
library(scales)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)

# locations ----------------------------------------------------------------
dacssf <- r"(E:\data\acs\sf\)"
dny2009 <- r"(E:\data\acs\sf\2009_5year\ny\)"
dny2014 <- r"(E:\data\acs\sf\2014_5year\ny\)"
dny2019 <- r"(E:\data\acs\sf\2019_5year\ny\)"

# constants ----------------------------------------------------------------


# source files  ----------------------------------------------------------------
source(here::here("r", "functions_utility.r"))

# notes ----------------------------------------------------------------

# SUMLEVEL CODES: save as rds ----------------------------------------------------------
# https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file/carto-boundary-summary-level.html
# https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
# https://mcdc.missouri.edu/geography/sumlevs/more-about-sumlevels.html
# https://www.census.gov/geographies/reference-files/2010/geo/state-local-geo-guides-2010/new-york.html

# "place" is an incorporated city or town, or a census designated place, which
# is a census-defined entity that has no legal definition but is used as a unit
# for data reporting


sumlevels1 <- read_fwf(
"
020 Region
030 Division
040 State
050 State-County
060 State-County-County Subdivision
067 State-County-County Subdivision-Subminor Civil Division
070 State-County-County Subdivision-Place Remainder (or part) DISCONTINUED
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

unknown_codes <- c(080, 155, 260, 269, 270, 280, 283, 311, 312, 313, 315, 316, 320, 321, 322, 323, 324, 331, 333, 340, 341, 410, 430, 510, 550, 612, 622, 795)
unknown <- tibble(sumlevel=unknown_codes, geotype="unknown") %>%
  mutate(sumlevel=str_pad(sumlevel, width=3, side="left", pad="0"))

sumlevels <- sumlevels1 %>%
  bind_rows(unknown) %>%
  mutate(temp=as.integer(sumlevel),
         sgeotype=case_when(temp %in% c(20, 30, 40) ~ str_to_lower(geotype),
                            temp == 50 ~ "county",
                            temp == 60 ~ "cosub",
                            temp == 70 ~ "coplace", # includes NY single-
                            temp == 140 ~ "tract",
                            temp == 150 ~ "blockgroup",
                            temp == 160 ~ "place",
                            temp == 170 ~ "concit",
                            temp == 310 ~ "msamicro",
                            temp == 314 ~ "msametro",
                            temp == 330 ~ "csa",
                            temp == 350 ~ "necta",
                            temp == 860 ~ "zcta",
                            temp == 950 ~ "sdelem",
                            temp == 960 ~ "sdsecond",
                            temp == 970 ~ "sdusd",
                            temp %in% unknown_codes ~ "unknown",
                            TRUE ~ "other")) %>%
  select(-temp)
count(sumlevels, sgeotype, geotype)
saveRDS(sumlevels, paste0(dacssf, "sumlevels.rds"))
count(sumlevels, sgeotype, geotype)

# DATA: save as parquet ----------------------------------------------
#.. function to get data for a year ----
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
    # column 5 is sequence, 6 is logrecno -- we want them to be integer
    vroom(con, col_names=FALSE, 
          col_types=cols(X1="c", X2="c", X3="c", X4="c", X5="i", X6="i",
                         .default=col_double()))
  }
  # g1(read_files$Name[1])
  df <- map_dfr(read_files$Name, g1)
  df
}


# repeat for each year
year <- 2019 # 2009 2014 2019
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


# GEO DEFINITIONS: save as rds  --------------------------------------------
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
                  geoid = c(179, 206),
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
df2009 <- readRDS(paste0(dny2009, "geo2009ny.rds"))
df2014 <- readRDS(paste0(dny2014, "geo2014ny.rds"))
df2019 <- readRDS(paste0(dny2019, "geo2019ny.rds"))
sumlevels <- readRDS(paste0(dacssf, "sumlevels.rds"))

geoall1 <- bind_rows(df2009, df2014, df2019)
glimpse(geoall1)
ht(geoall1)

geoall2 <- geoall1 %>%
  left_join(sumlevels, by="sumlevel") %>%
  # create short names - not ideal, ok for now
  mutate(sgeoname=str_extract_before_first(geoname, ","))

# create a unique name that does not vary across years
# previously I verified that geoids are unique within year
geoall3 <- geoall2 %>%
  arrange(geoid, year) %>%
  group_by(geoid) %>%
  mutate(usgeoname=last(sgeoname), # unique short geoname
         namechange=length(unique(sgeoname)) > 1) %>%
  ungroup
saveRDS(geoall3, paste0(dacssf, "geoall.rds"))

ht(geoall3)
count(geoall3, namechange)
tmp <- geoall3 %>%
  filter(namechange) %>%
  ht(21)

tmp <- geoall3 %>% filter(str_detect(geoname, "Albany city"))
tmp <- geoall3 %>% filter(str_detect(geoname, "Argyle village"))
tmp <- geoall3 %>% filter(str_detect(geoname, "Cambridge"),
                          !str_detect(geoname, "ract"))

tmp <- geoall3 %>% filter(str_detect(sumlevel, "155"),
                          str_detect(geoname, "Washington"))

tmp <- geoall3 %>% filter(str_detect(sumlevel, "070"),
                          str_detect(geoname, "Washington County"))

# NY info
# county code if exists is pos 10-12 of geoid
# cities: sumlevel 060 (if don't cross boundaries?), DO have county code
# villages: sumlevel 160, geotype State-Place, do NOT have a county code!
# towns: sumlevel 060, geotype State-County-County Subdivision, DO have county code


count(geoall3, sgeotype)
count(geoall3 %>% filter(is.na(sgeotype)), sumlevel, geotype)
geoall3 %>%
  filter(namechange, sgeotype=="county")

tmp <- geoall3 %>%
  filter(namechange, sgeotype=="cosub")

tmp <- geoall3 %>%
  filter(namechange, sgeotype=="place") # note that places do not have a county code


# TABLES, SEQUENCES, VARIABLES: save as rds ----

getseq <- function(fname, year, td, flist){
  num <- str_extract(fname, "[[:digit:]]+") %>% as.integer()
  fpath <- file.path(td, fname)
  df <- read_excel(fpath)
  
  df2 <- df %>%
    mutate(sequence=!!num) %>%
    select(-c(FILEID, FILETYPE, STUSAB, SEQUENCE, CHARITER, LOGRECNO)) %>%
    pivot_longer(-c(sequence), values_to = "vdescription") %>%
    separate(name, into=c("table", "vnum")) %>%
    mutate(xvarnum=row_number(), vnum=as.integer(vnum),  year=!!year) %>%
    select(year, table, sequence, xvarnum, vnum, vdescription)
  df2
}

wrapper <- function(year){
  dir <- paste0(r"(E:\data\acs\sf\)", year, r"(_5year\)")
  zfname <- paste0(year, r"(_5yr_Summary_FileTemplates.zip)")
  zpath <- paste0(dir, zfname)
  
  print(zpath)
  # the temporary directory persists within an R session so delete all before proceeding
  unlink(td, recursive=TRUE) 
  td <- tempdir()
  unzip(zpath, exdir = td, junkpaths = TRUE)
  flist <- str_subset(list.files(td), coll("seq", ignore_case = TRUE))
  
  # df <- map_dfr(1:length(flist), getseq, year, td, flist)
  df <- map_dfr(flist, getseq, year, td)
  closeAllConnections()
  df
}

vars2009 <- wrapper(2009)
vars2014 <- wrapper(2014)
vars2019 <- wrapper(2019)

varsall <- bind_rows(vars2009, vars2014, vars2019) %>%
  mutate(vname=paste0(table, "_", str_pad(vnum, width = 3, side="left", pad="0")))
comment(varsall) <- "Variable names and associated tables and sequences"
glimpse(varsall)
ht(varsall)
saveRDS(varsall, paste0(dacssf, "varsall.rds"))


tabseq <- varsall %>%
  select(year, table, sequence, xvarnum) %>%
  arrange(year, table, sequence, xvarnum) %>%
  group_by(year, table) %>%
  mutate(ntabvars=n(), 
         vfirst=first(xvarnum), 
         vlast=last(xvarnum),
         nseq=length(unique(sequence))) %>%
  group_by(year, table, sequence) %>%
  summarise(ntabvars=first(ntabvars),
            vfirst=first(vfirst),
            vlast=first(vlast),
            nseq=first(nseq),
            nseqvars=n(), 
            seqvfirst=first(xvarnum),
            seqvlast=last(xvarnum),
            .groups="drop")
comment(tabseq) <- "Tables, sequences in which they are located, and positions within sequences"
ht(tabseq)
saveRDS(tabseq, paste0(dacssf, "tabseq.rds"))

tabseq %>%
  filter(ntabvars != nseqvars) 
tabseq %>%
  filter(ntabvars != nseqvars, year==2019) 

tmp <- tabseq %>% filter(table=="B05002", year==2009)
tmp <- varsall %>% filter(table=="B05002", year==2009)

# DATA RETRIEVAL TOOLS ----
#.. get all lookup information ----
geoall <- readRDS(paste0(dacssf, "geoall.rds"))
sumlevels <- readRDS(paste0(dacssf, "sumlevels.rds"))
tabseq <- readRDS(paste0(dacssf, "tabseq.rds"))
varsall <- readRDS(paste0(dacssf, "varsall.rds"))

#.. decide upon tables ----
# "E:\\data\\acs\\sf\\"
# look especially at the table shells
# B01002 MEDIAN AGE BY SEX
# B01003 TOTAL POPULATION

#.. decide upon geographies ----
gpath <- r"(E:\data\acs\sf\2019_5year\boyd_acs_table_tools.xlsx)"
geodf <- read_excel(gpath, sheet="geos_keep", skip=1)
keepdf <- geodf %>% filter(keep==1)

geokeep <- geoall %>%
  filter(geoid %in% keepdf$geoid)
(geoids <- unique(geokeep$geoid))

# define tables, and table to get
tabs <- c("B01002", "B01003")
table <- "B01002"

get_tabyear <- function(table, year){
  # get a single table in a single year -----
  # year <- 2019
  # table <- "B01002"
  
  # find sequences and positions for the table in this year
  seqpos <- tabseq %>%
    filter(year==!!year, table == !!table) %>%
    arrange(sequence) %>%
    filter(row_number()==1) # for now don't worry about multiple-sequence tables
  if(seqpos$nseq > 1) print("CAUTION: This table is in more than one sequence!")
  
  # define sequences and logical record numbers to get
  seqs <- seqpos %>% .$sequence
  logrecnos <- geokeep %>% filter(year==!!year) %>% .$logrecno
  
  idvars <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")
  xvars <- paste0("x", seqpos$vfirst:seqpos$vlast)
  vars <- c(idvars, xvars)
  
  # choose and open dataset
  dirs <- c(dny2009, dny2014, dny2019)
  dir <- str_subset(dirs, as.character(year))
  fn <- paste0("em", year, "5ny.parquet")
  ds <- open_dataset(paste0(dir, fn), partitioning = c("sequence", "logrecno"))
  
  # get the data
  df1 <- ds %>%
    filter(sequence %in% seqs, logrecno %in% logrecnos) %>%
    select(any_of(vars)) %>%
    as_tibble()
  
  # append desired info
  df2 <- df1 %>%
    left_join(geokeep %>%
                filter(year==!!year) %>%
                select(logrecno, stabbr, sumlevel, geoid, geoname, sgeotype,
                       usgeoname, namechange),
              by="logrecno") %>%
    mutate(year=!!year,
           valtype=case_when(str_sub(filetype, 5, 5)=="e" ~ "est",
                             str_sub(filetype, 5, 5)=="m" ~ "moe",
                             TRUE ~ "ERROR")) %>%
    select(-c(fileid, filetype, stusab, chariter)) %>%
    select(year, valtype, stabbr, sumlevel, geoid, geoname, sgeotype, usgeoname, namechange,
           sequence, logrecno,
           starts_with("x"))
  df2
}

# define function with inputs in either order so that we can map over either
get_yeartab <- function(year, table){
  get_tabyear(table, year)
}

tab <- "B01002"

get_tabyear(tab, 2009)
get_tabyear(tab, 2014)
get_tabyear(tab, 2019)

get_yeartab(2009, tab)
get_yeartab(2014, tab)
get_yeartab(2019, tab)





fn <- paste0(dny2019, "em20195ny.parquet")
df <- read_parquet(fn) %>%  # ,  col_select=c(all_of(vars))
  filter(sequence==seqs, logrecno %in% logrecnos)
count(df, filetype)

#.. define retrieval function(s) ----
ds2009 <- open_dataset(paste0(dny2009, "em20095ny.parquet"), 
                   partitioning = c("sequence", "logrecno"))

ds2014 <- open_dataset(paste0(dny2014, "em20145ny.parquet"), 
                       partitioning = c("sequence", "logrecno"))

ds2019 <- open_dataset(paste0(dny2019, "em20195ny.parquet"), 
                       partitioning = c("sequence", "logrecno"))

#.. get data ----
seq2009 <- tab_locations %>% filter(year==2009) %>% .$sequence
lrn2009 <- geokeep %>%
  filter(year==2009) %>%
  .$logrecno

djb <- ds2009 %>%
  filter(sequence %in% seq2009,
         logrecno %in% lrn2009) %>%
  select(any_of(c(idvars, "x156", "x157", "x158"))) %>%
  as_tibble() %>%
  left_join(geokeep %>%
              filter(year==2009),
            by="logrecno")


djb <- ds %>%
  filter(as.integer(sequence)==2) %>%
  select(c(idvars, "x100", "x101", "x102")) %>%
  as_tibble()



#.. junk ----
tmp <- geoall3 %>% filter(str_detect(geoname, "Albany city"))
tmp <- geoall3 %>% filter(str_detect(geoname, "Argyle village"))
tmp <- geoall3 %>% filter(str_detect(geoname, "Cambridge"),
                          !str_detect(geoname, "ract"))

tmp <- geoall3 %>% filter(str_detect(sumlevel, "155"),
                          str_detect(geoname, "Washington"))

tmp <- geoall3 %>% filter(str_detect(sumlevel, "070"),
                          str_detect(geoname, "Washington County"))

# NY info
# county code if exists is pos 10-12 of geoid
# cities: sumlevel 060 (if don't cross boundaries?), DO have county code
# villages: sumlevel 160, geotype State-Place, do NOT have a county code!
# towns: sumlevel 060, geotype State-County-County Subdivision, DO have county code



# OLD ----
#.. failed attempt usinmg lookup tables ----
path2009 <- paste0(dacssf, "2009_5year/",  "Sequence_Number_and_Table_Number_Lookup.xls")
path2014 <- paste0(dacssf, "2014_5year/",  "ACS_5yr_Seq_Table_Number_Lookup.xls")
path2019 <- paste0(dacssf, "2019_5year/",  "ACS_5yr_Seq_Table_Number_Lookup.xlsx")
# read_lines(path, n_max=5)
vars20091 <- read_excel(path2009)
vars20141 <- read_excel(path2014)
vars20191 <- read_excel(path2019)

names(vars20091)
names(vars20141)
names(vars20191)
vnames <- c("fileid", "table", "sequence", "line", "start", "ncellstab", "ncellsseq", "name", "subject")

varsall1 <- bind_rows(vars20091 %>%
                        setNames(vnames) %>%
                        mutate(year=2009),
                      vars20141 %>%
                        setNames(vnames) %>%
                        mutate(year=2014),
                      vars20191 %>%
                        setNames(vnames) %>%
                        mutate(year=2019))

tmp <- varsall1 %>%
  arrange(year, table, sequence, line)
  # filter(table=="B26101", year==2019)

varsall2 <- varsall1 %>%
  select(-fileid) %>%
  mutate(line=as.integer(line),
         rectype=case_when(!is.na(ncellstab) ~ "tabname", 
                           str_detect(tabname, "Universe") ~ "universe",
                           TRUE ~ NA_character_))) %>%
  fill(subject, .direction="down") %>%
  group_by(sequence) %>%
  mutate(row=row_number(), nseq=n()) %>%
  group_by(table) %>%
  mutate(ntable=n()) %>%
  ungroup
  
varsall2 %>%
  filter(year==2019, sequence==1) %>%
  select(table, sequence, line, row, starts_with("n")) %>%
  ht


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
