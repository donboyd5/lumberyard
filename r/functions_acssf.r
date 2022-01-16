
get_tabyear <- function(table, year, 
                        # if the following are in the parent environment
                        # they do not need to be passed
                        .tabseq=tabseq, 
                        .geokeep=geokeep,
                        .tabdescribe=tabdescribe,
                        .varsall=varsall,
                        .dspaths=dspaths){
  # get a single table in a single year -----
  # year <- 2019
  # table <- "B01002"
  
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
  
  # make a data frame that has each xvar name, its number, and the table variable name
  vardf <- tibble(xvar=paste0("x", seqpos$vfirst:seqpos$vlast),
                  vnum=1:length(xvar),
                  tabvarname=paste0(table, "_", str_pad(vnum, width=3, side="left", pad="0")))
  # xvars <- paste0("x", seqpos$vfirst:seqpos$vlast)

  vars <- c(idvars, vardf$xvar) # variables we will get from the data file
  
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
  
  # make the file long, add vnum and table varnames
  df2 <- df1 %>%
    mutate(year=!!year,
           table=!!table,
           stusab=str_to_upper(stusab),
           valtype=case_when(str_sub(filetype, 5, 5)=="e" ~ "est",
                             str_sub(filetype, 5, 5)=="m" ~ "moe",
                             TRUE ~ "ERROR")) %>%
    select(year, table, valtype, stabbr=stusab, logrecno, starts_with("x")) %>%
    pivot_longer(starts_with("x"), names_to = "xvar") %>%
    left_join(vardf, by = "xvar")
  
  # bring in table description and variable description
  df3 <- df2 %>%
    left_join(.tabdescribe, by = c("year", "table")) %>%
    left_join(.varsall %>% select(year, tabvarname=vname, vdescription),
              by=c("year", 'tabvarname'))
  
  # bring in geographic variables and get valtype (est or moe)
  df4 <- df3 %>%
    left_join(.geokeep %>%
                filter(year==!!year) %>%
                select(logrecno, stabbr, sumlevel, geoid, geoname, sgeotype,
                       usgeoname, namechange),
              by=c("logrecno", "stabbr")) %>%
    relocate(c(xvar, vnum, tabvarname, value, vdescription), .after = last_col())
  df4
}

# define function with inputs in either order so that we can map over either
get_yeartab <- function(year, table,
                        # if the following are in the parent environment
                        # they do not need to be passed
                        .tabseq=tabseq, 
                        .geokeep=geokeep,
                        .tabdescribe=tabdescribe,
                        .varsall=varsall,
                        .dspaths=dspaths){
  get_tabyear(table, year, .tabseq, .geokeep, .tabdescribe, .varsall, .dspaths)
}

