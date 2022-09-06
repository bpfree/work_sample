###################################################
###################################################
###################################################

######### Part 1 #########
## Pulling from NEFSC
##########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
data_dir <- "data\\a_raw_data"
list.files(data_dir)

# User parameters
uid <- 'bfree'
pwd <- scan(file = "", # default to stdin() as a standard input connection for read-only
            # read character values
            what = character(),
            # have a single data value read
            n = 1)

# data parameters
begin_year <- 1963
end_year <- 2021

###################################################
###################################################
###################################################

### Connecting to the NEFSC Oracle database
## There are two different methods

###################################################
#
# Method 1 -- from Sean Lucey (sean.lucey@noaa.gov)
# https://github.com/slucey/RSurvey/blob/master/Survdat.r
# Will need to be connected to the VPN if not already
# on the server
###################################################

# Connect to Sole database from the NEFSC server
# Should see list of datasets in the connections table in the R Studio environment
# Can manually inspect the tables from that tab by clicking the table symbol
sole1 <- DBI::dbConnect(odbc::odbc(), dsn = 'sole', uid = uid, pwd = pwd)


###################################################
# Method 2 -- from Andy Beet
# This approach is best for querying the database
# and pulling data directly into R sessions
#
###################################################

# Access NEFSC database utilities repository from Andy Beet (andrew.beet@noaa.gov)
# remotes::install_github("andybeet/dbutils", force = TRUE)
# 1
# library(dbutils)


## Will need to be connected to the VPN for this next part to be successful
## When prompted enter username and Oracle Password
## Can reset Oracle password here: https://nova.nefsc.noaa.gov/orachangepw/
# sole1 <- dbutils::connect_to_database(server="SOLE", uid=rstudioapi::askForPassword("Database user"))


###################################################
###################################################
###################################################

### Pull data from the Oracle database into the R environment
## Query statements
# Select all fields from the master cruise table that have a purpose code 10 (Bottom Trawl Survey)
# and have been conducted since 1963
# master_cruise_query <- "select * 
#                         from MSTR_cruise
#                         where purpose_code = 10
#                         and YEAR >= 1963"

# pull data from svdbs using the predefined query
# master_cruise <- DBI::dbGetQuery(sole1, master_cruise_query) %>%
#   dplyr::select(CRUISE6, SVVESSEL, YEAR, SEASON,
#                 STATUS_CODE) %>%
#   # rename fields to have them easier to read
#   dplyr::rename(cruise = CRUISE6,
#                 vessel = SVVESSEL,
#                 status_code = STATUS_CODE,
#                 season = SEASON,
#                 year = YEAR) %>%
#   # make season be in title case (i.e., Fall, Spring, Summer, Winter)
#   dplyr::mutate(season = str_to_title(season)) %>%
#   # remove any tows that have missing data (full data = 335, non-na data = 264)
#   na.omit() %>%
#   # obtain audited only bottom trawl survey cruises (audited only data = 242)
#   dplyr::filter(status_code == 10) %>%
#   dplyr::select(-status_code) %>%
#   # filter data to only include unique cruise for vessel by year and season (n = 172)
#   # length(unique(bts_cruise$cruise)) = 147, this is because certain cruises have different vessels for the same year
#   dplyr::group_by(cruise,
#                   vessel,
#                   season,
#                   year) %>%
#   # calculate the frequencies of the unique combinations
#   dplyr::summarise(freq = n()) %>%
#   # drop the frequency field to have only table with cruise, vessel, season, and year
#   dplyr::select(-freq)
  
svdbs_cruise_query <- "select * 
                        from SVDBS_CRUISES
                        where purpose_code = 10
                        and YEAR >= 1963"

bts_cruise <- DBI::dbGetQuery(sole1, svdbs_cruise_query) %>%
  dplyr::select(CRUISE6, YEAR, SEASON,
                STATUS_CODE) %>%
  # rename fields to have them easier to read
  dplyr::rename(cruise = CRUISE6,
                status_code = STATUS_CODE,
                season = SEASON,
                year = YEAR) %>%
  # make season be in title case (i.e., Fall, Spring, Summer, Winter)
  dplyr::mutate(season = str_to_title(season))

###################################################

## get the station data
station_query <- "select *
                  from UNION_FSCS_SVSTA"

## Will take a few minutes (~5 - 8) if query is not filtered
station <- DBI::dbGetQuery(sole1, station_query) %>%
  # select fields of interest
  dplyr::select(CRUISE6,STRATUM,TOW,STATION,
                ID, STATUS_CODE,SHG,TOGA, SVVESSEL,
                EST_YEAR,EST_MONTH,
                DECDEG_BEGLAT,DECDEG_BEGLON,
                DECDEG_ENDLAT,DECDEG_ENDLON,
                AVGDEPTH, SURFTEMP, BOTTEMP, 
                SURFSALIN, BOTSALIN) %>%
  # rename fields to be easier to understand
  dplyr::rename(cruise = CRUISE6,
                stratum = STRATUM,
                tow = TOW,
                station = STATION,
                status = STATUS_CODE,
                id = ID,
                shg = SHG,
                toga = TOGA,
                year = EST_YEAR,
                month = EST_MONTH,
                vessel = SVVESSEL,
                dd_start_lat = DECDEG_BEGLAT,
                dd_start_long = DECDEG_BEGLON,
                dd_end_lat = DECDEG_ENDLAT,
                dd_end_long = DECDEG_ENDLON,
                depth = AVGDEPTH,
                surface_temp = SURFTEMP,
                bottom_temp = BOTTEMP,
                surface_saline = SURFSALIN,
                bottom_saline = BOTSALIN)

###################################################

# obtain the catch data
catch_query <- "select *
                from UNION_FSCS_SVCAT"

## Will take a few minutes (~8 - 10) if query is not filtered
catch <- DBI::dbGetQuery(sole1, catch_query) %>%
  # do not carry cruise or cruise comment fields
  dplyr::select(-CRUISE, -CATCH_COMMENT) %>%
  # rename fields to be easier to understand
  dplyr::rename(cruise = CRUISE6,
                stratum = STRATUM,
                station = STATION,
                tow = TOW,
                status = STATUS_CODE,
                id = ID,
                species = SVSPP,
                sex = CATCHSEX,
                count = EXPCATCHNUM,
                weight = EXPCATCHWT,
                species_name = LOGGED_SPECIES_NAME)

unique(catch$status) # has codes 10 (audited) and 15 (preliminary)

###################################################

length_query <- "select *
                 from UNION_FSCS_SVLEN"

length <- DBI::dbGetQuery(sole1, length_query) %>%
  dplyr::select(-CRUISE,-ID,-LENGTH_COMMENT) %>%
  dplyr::rename(cruise = CRUISE6,
                stratum = STRATUM,
                station = STATION,
                tow = TOW,
                status = STATUS_CODE,
                species = SVSPP,
                sex = CATCHSEX,
                length = LENGTH,
                count = EXPNUMLEN,
                species_name = LOGGED_SPECIES_NAME)

###################################################

## get species list
species_query <- "select *
                  from SVSPECIES_LIST"

species <- DBI::dbGetQuery(sole1, species_query) %>%
  dplyr::select(SVSPP, SCINAME, COMNAME) %>%
  dplyr::rename(svspp = SVSPP,
                scientific_name = SCINAME,
                common_name = COMNAME)

###################################################

## get methodology catch conversions
conversion_query <- "select *
                     from SURVAN_CONVERSION_FACTORS"

conversion_factors <- DBI::dbGetQuery(sole1, conversion_query) %>%
  dplyr::rename(species = SVSPP,
                dcf_wt = DCF_WT,
                dcf_num = DCF_NUM,
                gcf_wt = GCF_WT,
                gcf_num = GCF_NUM,
                vcf_wt = VCF_WT,
                vcf_num = VCF_NUM)

###################################################
###################################################
###################################################

########
## Export data
# Raw database data as RDS format for opening in R
saveRDS(bts_cruise, file = paste0(data_dir, "/", "bts_cruises.rds"))
saveRDS(master_cruise, file = paste0(data_dir, "/", "master_cruises.rds"))
saveRDS(catch, file = paste0(data_dir, "/", "catch.rds"))
saveRDS(length, file = paste0(data_dir, "/", "length.rds"))
saveRDS(station, file = paste0(data_dir, "/", "station.rds"))
saveRDS(species, file = paste0(data_dir, "/", "species.rds"))
saveRDS(conversion_factors, file = paste0(data_dir, "/", "conversion_factors.rds"))

# Raw database data as CSV format for opening in Excel or similar software
write.csv(bts_cruise, file = paste0(data_dir, "/", "bts_cruises.csv"))
write.csv(master_cruise, file = paste0(data_dir, "/", "master_cruises.csv"))
write.csv(catch, file = paste0(data_dir, "/", "catch.csv"))
write.csv(length, file = paste0(data_dir, "/", "length.csv"))
write.csv(station, file = paste0(data_dir, "/", "station.csv"))
write.csv(species, file = paste0(data_dir, "/", "species.csv"))
write.csv(conversion_factors, file = paste0(data_dir, "/", "conversion_factors.csv"))
