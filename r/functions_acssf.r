
get_tabyear <- function(table, year, 
                        # if the following are in the parent environment
                        # they do not need to be passed
                        .tabseq=tabseq, 
                        .geokeep=geokeep,
                        .dspaths=dspaths){
  # get a single table in a single year -----
  # year <- 2019
  # table <- "B01002"
  # this ASSUMES the following are in the environment
  # dny2009, dny2014, dny2019
  
  # find sequences and positions for the table in this year
  seqpos <- .tabseq %>%
    filter(year==!!year, table == !!table) %>%
    arrange(sequence) %>%
    filter(row_number()==1) # for now don't worry about multiple-sequence tables
  
  if(nrow(seqpos)==0){
    msg <- paste0("No sequences listed for table ", table, " in ", year, ".")
    print(msg)
    return()
  } else {
    if(seqpos$nseq > 1) print("CAUTION: This table is in more than one sequence!")
  }
  
  # define sequences and logical record numbers to get
  seqs <- seqpos %>% .$sequence
  logrecnos <- .geokeep %>% filter(year==!!year) %>% .$logrecno
  
  idvars <- c("fileid", "filetype", "stusab", "chariter", "sequence", "logrecno")
  xvars <- paste0("x", seqpos$vfirst:seqpos$vlast)
  vars <- c(idvars, xvars)
  
  # choose and open dataset
  dspath <- str_subset(.dspaths, as.character(year))
  ds <- open_dataset(dspath, partitioning = c("sequence", "logrecno"))
  
  # get the data
  df1 <- ds %>%
    filter(sequence %in% seqs, logrecno %in% logrecnos) %>%
    select(any_of(vars)) %>%
    as_tibble()
  
  if(nrow(df1)==0){
    msg <- paste0("No data returned for table ", table, " in ", year, ". Exiting.")
    print(msg)
    return()
  }
  
  # append desired info
  df2 <- df1 %>%
    left_join(.geokeep %>%
                filter(year==!!year) %>%
                select(logrecno, stabbr, sumlevel, geoid, geoname, sgeotype,
                       usgeoname, namechange),
              by="logrecno") %>%
    mutate(year=!!year,
           table=!!table,
           valtype=case_when(str_sub(filetype, 5, 5)=="e" ~ "est",
                             str_sub(filetype, 5, 5)=="m" ~ "moe",
                             TRUE ~ "ERROR")) %>%
    select(-c(fileid, filetype, stusab, chariter)) %>%
    select(year, table, valtype, stabbr, sumlevel, geoid, geoname, sgeotype, usgeoname, namechange,
           sequence, logrecno,
           starts_with("x"))
  df2
}

# define function with inputs in either order so that we can map over either
get_yeartab <- function(year, table,
                        # if the following are in the parent environment
                        # they do not need to be passed
                        .tabseq=tabseq, 
                        .geokeep=geokeep,
                        .dspaths=dspaths){
  get_tabyear(table, year, .tabseq, .geokeep, .dspaths)
}