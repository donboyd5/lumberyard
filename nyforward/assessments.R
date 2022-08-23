
source(here::here("r", "libraries.r"))
source(here::here("r", "functions_utility.r"))


# get assessment roll -----------------------------------------------------
# https://data.ny.gov/Government-Finance/Property-Assessment-Data-from-Local-Assessment-Rol/7vem-aaz7

fn <- "Property_Assessment_Data_from_Local_Assessment_Rolls.csv"
df <- read_csv(here::here("data", fn), n_max=Inf)
glimpse(df)

df2 <- df |> 
  filter(str_detect(`School District Name`, "Cambridge"))
glimpse(df2)

df3 <- df2 |> 
  select(year=`Roll Year`,
         cntyname=`County Name`,
         municode=`Municipality Code`,
         muniname=`Municipality Name`,
         sdcode=`School District Code`,
         sdname=`School District Name`,
         swis=`SWIS Code`,
         taxclass=`Tax Class`,
         section=`Roll Section`,
         class=`Property Class`,
         classdesc=`Property Class Description`,
         pkey=`Print Key Code`,
         addrnum=`Parcel Address Number`,
         addrstreet=`Parcel Address Street`,
         addrsuff=`Parcel Address Suff`,
         front=Front,
         depth=Depth,
         bank=Bank,
         coordeast=`Grid Coordinates East`,
         coordnorth=`Grid Coordinates North`,
         deedbook=`Deed Book`,
         page=Page,
         ownerfname=`Primary Owner First Name`,
         ownermi=`Primary Owner MI`,
         ownerlname=`Primary Owner Last Name`,
         ownersuff=`Primary Owner Suffix`,
         fmv=`Full Market Value`,
         avland=`Assessment Land`,
         avtotal=`Assessment Total`,
         avtxblcnty=`County Taxable Value`,
         avtxlbtown=`Town Taxable Value`,
         avtxblschool=`School Taxable`)

glimpse(df3)
count(df3, sdname)
count(df3, muniname)
tmp <- df3 |> 
  select(year, municode, muniname, class, classdesc, ownerfname, ownermi, ownerlname, addrnum, addrstreet, fmv, avtotal, avtxblschool) |> 
  arrange(desc(fmv))
  
