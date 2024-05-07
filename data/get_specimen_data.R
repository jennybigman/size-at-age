## data query for Jenny Bigman temperature size rule project
remotes::install_github("afsc-gap-products/gapindex")
library(gapindex)

sql_channel <- gapindex::get_connected()

species_code <- c(21720, 21740, 10210, 10110) # 10111 is arrowtooth genus if needed

gapindex_data <-get_data(
                  year_set = 1982:2023,
                  survey_set = c("EBS", "NBS"),
                  spp_codes = species_code,
                  haul_type = 0:23, # include all haul types
                  abundance_haul = c("Y", "N"),
                  na_rm_strata = FALSE,
                  sql_channel = sql_channel,
                  pull_lengths = TRUE
                )

## Pull data from miscellaneous stations in 1985, 1988, and 1991 that were
## sampled in the NBS but are not a part of the standard stations used in the
## design-based index production. These cruises have a different survey
## definition IDs from the EBS (98), NBS (143) and BSS (78), so they will not
## come up in gapindex::get_data(). Thus, the next set of code consists of
## hard-coded SQL pulls from RACEBASE.
other_haul <-
  RODBC::sqlQuery(channel = sql_channel,
                  query = "SELECT *
                             FROM RACEBASE.HAUL
                             WHERE CRUISE IN (198502, 198808, 199102)
                             AND PERFORMANCE >= 0")

other_specimen <-
  RODBC::sqlQuery(
    channel = sql_channel,
    query = paste("SELECT SPECIES_CODE, CRUISEJOIN, HAULJOIN, REGION, VESSEL, CRUISE, HAUL, SPECIMENID, LENGTH, SEX, WEIGHT, AGE
                     FROM RACEBASE.SPECIMEN
                     WHERE SPECIES_CODE IN",
                  gapindex::stitch_entries(species_code),
                  "AND HAULJOIN IN",
                  gapindex::stitch_entries(other_haul$HAULJOIN)))


specimen <- rbind(gapindex_data$specimen, other_specimen)
haul <- rbind(gapindex_data$haul, other_haul)
df <- dplyr::left_join(specimen, haul)
df_sp <- dplyr::left_join(df, dplyr::select(gapindex_data$species, SPECIES_CODE, COMMON_NAME))

saveRDS(df_sp, "data/ak_bts.RDS")