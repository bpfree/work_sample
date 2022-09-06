###################################################
###################################################
###################################################


######### Part 4 #########
## Fixing conversions
##########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
data_dir <- "data\\a_raw_data"
bts_dir <- "data\\b_bts_data"
list.files(data_dir)
list.files(bts_dir)

###################################################
###################################################
###################################################

#### Load conversion and catch (count) data
#### Read RDS files and convert to data.table (to use code from Sean Lucey)
bts_representative <- as.data.table(readRDS(paste(bts_dir, "bts_representative.rds", sep = "/")))
conversion_table <- as.data.table(readRDS(paste(data_dir, "conversion_factors.rds", sep = "/"))) %>%
  dplyr::mutate(species = as.numeric(species))

###################################################
###################################################
###################################################

## Biomass conversions
# Door conversion factor (adopted from Sean Lucey)
dcf_spp_weight <- conversion_table[dcf_wt > 0, species]
for(i in 1:length(dcf_spp_weight)){
  # for when year is after 1985 and the species needs to have its weight updated by the door conversion
  bts_representative[year < 1985 & species == dcf_spp_weight[i],
                     # the weight will be the current weight multiplied by the weight of the species of interest from
                     # the conversion table
                     weight := weight * conversion_table[species == dcf_spp_weight[i], dcf_wt]]
}

# Gear conversion factor (Spring 1973 - 1981 Net Conversion) (adopted from Sean Lucey)
gcf_spp_weight <- conversion_table[gcf_wt > 0, species]
for(i in 1:length(gcf_spp_weight)){
  # for when season is Spring and the year is between 1972 and 1982 and it is a species needing converting
  # then the species needs to have its weight updated by the net conversion
  bts_representative[season == 'Spring' & year > 1972 & year < 1982 & species == gcf_spp_weight[i],
                     # the weight will be the current weight divided by the conversion value for the species of
                     # interest as from the conversion table
                     weight := weight / conversion_table[species == gcf_spp_weight[i], gcf_wt]]
}

# Vessel conversion factor (for the Delaware -- DE) (adopted from Sean Lucey)
vcf_spp_weight <- conversion_table[vcf_wt > 0, species]
for(i in 1:length(vcf_spp_weight)){
  # for when the vessel is the Delaware and the species is of interest
  bts_representative[vessel == 'DE' & species == vcf_spp_weight[i],
                     # the weight will be converted from the present weight multiplied by the
                     # species of interest weight as detailed in the conversion table
                     weight := weight * conversion_table[species == vcf_spp_weight[i], vcf_wt]]
}

###################################################
###################################################
###################################################

## Count conversions
# Door conversion factor (adopted from Sean Lucey)
dcf_spp_count <- conversion_table[dcf_wt > 0, species]
for(i in 1:length(dcf_spp_count)){
  # for when year is after 1985 and the species needs to have its count updated by the door conversion
  bts_representative[year < 1985 & species == dcf_spp_count[i],
          # the count will be the rounded value of the current count multiplied by the species of interest# conversion factor as detailed in the conversion table
          count := round(count * conversion_table[species == dcf_spp_count[i], dcf_num])]
}

# Gear conversion factor (Spring 1973 - 1981 Net Conversion) (adopted from Sean Lucey)
gcf_spp_count <- conversion_table[gcf_wt > 0, species]
for(i in 1:length(gcf_spp_count)){
  # for when season is Spring and the year is between 1972 and 1982 and it is a species needing converting
  # then the species needs to have its count updated by the net conversion
  bts_representative[season == 'Spring' & year > 1972 & year < 1982 & species == gcf_spp_count[i],
          # the count will be the current count divided by the conversion value for the species of
          # interest as from the conversion table
          count := round(count / conversion_table[species == gcf_spp_count[i], gcf_wt])]
}

# Vessel conversion factor (for the Delaware -- DE) (adopted from Sean Lucey)
vcf_spp_count <- conversion_table[vcf_wt > 0, species]
for(i in 1:length(vcf_spp_count)){
  # for when the vessel is the Delaware and the species is of interest
  bts_representative[vessel == 'DE' & species == vcf_spp_count[i],
          # the count will be converted from the present count multiplied by the
          # species of interest count as detailed in the conversion table
          count := round(count * conversion_table[species == vcf_spp_count[i], vcf_wt])]
}

###################################################
###################################################
###################################################

# Bigelow >2008 Vessel Conversion 
# Use Bigelow conversions for Pisces as well (PC)
# Tables 53-55 from Miller et al. 2010 - number estimators
hb_count <- data.table(species = c(12, 22, 24, 27, 28, 31, 33, 34, 73, 76, 106, 107,
                                 109, 121, 135, 136, 141, 143, 145, 149, 155, 164,
                                 171, 181, 193, 197, 502, 512, 15, 23, 26, 32, 72,
                                 74, 77, 78, 102, 103, 104, 105, 108, 131, 163, 301,
                                 313, 401, 503, 15, 23, 26, 32, 72, 74, 77, 78, 102,
                                 103, 104, 105, 108, 131, 163, 301, 313, 401, 503),
                     season = c(rep('both', 28), rep('spring', 19), rep('fall', 19)),
                     rho = c(1.161, 4.44, 6.689, 4.384, 3.792, 1.227, 1.323, 1.29,
                             1.987, 2.235, 2.49, 3.257, 8.249, 1.188, 1.16, 1.134,
                             3.416, 1.705, 1.577, 1.541, 1.456, 1.802, 4.494, 0.57,
                             4.575, 7.129,	1.38, 2.309, 1.202, 3.822, 3.08, 2.287,
                             6.283, 0.972, 3.959, 3.839, 2.074, 3.226, 3.099, 2.347,
                             3.311, 1.487, 3.56, 1.571, 3.343, 3.965, 2.034, 1.204,
                             2.609, 9.846, 2, 4.354, 0.243, 2.662, 2.319, 2.16,
                             2.405, 2.356, 2.366, 0.2, 0.172, 3.114, 1.586, 2.511,
                             3.166, 0.84))

#Tables 56-58 from Miller et al. 2010 Biomass estimators
hb_biomass <- data.table(species = c(12, 22, 24, 27, 28, 31, 33, 34, 73, 76, 106, 107,
                                     109, 121, 135, 136, 141, 143, 145, 149, 155, 164,
                                     171, 181, 193, 197, 502, 512, 15, 23, 26, 32, 72,
                                     74, 77, 78, 102, 103, 104, 105, 108, 131, 163, 301,
                                     313, 401, 503, 15, 23, 26, 32, 72, 74, 77, 78, 102,
                                     103, 104, 105, 108, 131, 163, 301, 313, 401, 503),
                         season = c(rep('both', 28), rep('spring', 19), rep('fall', 19)),
                         rhoW = c(1.082, 3.661, 6.189, 4.45, 3.626, 1.403, 1.1, 2.12,
                                  1.58, 2.088, 2.086, 3.257, 12.199, 0.868, 0.665, 1.125,
                                  2.827, 1.347, 1.994, 1.535, 1.191, 1.354, 3.259, 0.22,
                                  3.912, 8.062, 1.409, 2.075, 1.166, 3.718, 2.786, 5.394,
                                  4.591, 0.878, 3.712, 3.483, 2.092, 3.066, 3.05, 2.244,
                                  3.069, 2.356, 2.986, 1.272, 3.864, 1.85, 2.861, 1.21,
                                  2.174, 8.814, 1.95, 4.349, 1.489, 3, 2.405, 1.692,
                                  2.141, 2.151, 2.402, 1.901, 1.808, 2.771, 1.375, 2.479,
                                  3.151, 1.186))

# ID species to convert
spp_both <- hb_count[season == 'both', species]
spp_seasonal <- hb_count[season == 'fall', species]

###################################################
###################################################
###################################################

# Both seasons  
# weight (biomass)
for(ispp in 1:length(spp_both)){
  bts_representative[vessel %in% c('HB', 'PC') & species == spp_both[ispp],
                     weight := weight / hb_biomass[species == spp_both[ispp], rhoW]]
}

# count (abundance)
for(ispp in 1:length(spp_both)){
  bts_representative[vessel %in% c('HB', 'PC') & species == spp_both[ispp],
          count := round(count / hb_count[species == spp_both[ispp], rho])]
}

# Separate calibration factors by season
# weight (biomass)
for(ispp in 1:length(spp_seasonal)){
  bts_representative[vessel %in% c('HB', 'PC') & season == 'Spring' & species == spp_seasonal[ispp],
                     weight := weight/ hb_biomass[species == spp_seasonal[ispp] & season == 'spring',
                                                         rhoW]]
  
  bts_representative[vessel %in% c('HB', 'PC') & season == 'Fall' & species == spp_seasonal[ispp],
                     weight := weight / hb_biomass[species == spp_seasonal[ispp] & season == 'fall',
                                                          rhoW]]
}

for(ispp in 1:length(spp_seasonal)){
  bts_representative[vessel %in% c('HB', 'PC') & season == 'Spring' & species == spp_seasonal[ispp],
          count := round(count / hb_count[species == spp_seasonal[ispp] & 
                                        season == 'spring', rho])]
  
  bts_representative[vessel %in% c('HB', 'PC') & season == 'Fall' & species == spp_seasonal[ispp],
          count := round(count / hb_count[species == spp_seasonal[ispp] &
                                        season == 'fall', rho])]
}

###################################################
###################################################
###################################################

# Convert back to just data.frame
class(bts_representative) <- class(as.data.frame(bts_representative))

###################################################
###################################################
###################################################
## Export data
# Combined data as RDS format for opening in R
saveRDS(bts_representative, file = paste0(bts_dir, "/", "bts_representative_converted.rds"))

saveRDS(hb_count, file = paste0(bts_dir, "/", "hb_count.rds"))
saveRDS(hb_biomass, file = paste0(bts_dir, "/", "hb_biomass.rds"))

