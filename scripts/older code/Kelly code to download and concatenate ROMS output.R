library(ncdf4)
library(lubridate)
library(abind)

#-------------------
# Setup
#-------------------

yrblock <- seq(1985, 2010, by=5) # Calibration block

# Variable to read 

vname1 <- "temp_bottom5m" # Variable name in filename
vname2 <- "temp"          # Variable name in file

# Simulation names

hcsim <- "B10K-K20_CORECFS"                  # hindcast
fcsim <- "B10K-K20P19_CMIP6_cesm_historical" # forecast, historical period
fcssp <- "B10K-K20P19_CMIP6_cesm_ssp585"     # forecast, SSP 5-8.5

# The naming conventions for files are slightly different on mox compared to on the 
# PMEL server (PMEL server adds an extra folder level, with 5-year blocks).  In this script,
# we're reading hindcast data from the PMEL server (because it's faster than over the mox
# ssh connection) and forecast data from mox b/c it hasn't been added to the server yet.

tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
# moxbase <- "/gscratch/bumblereem/mox_bumblereem/roms_for_public" # top-level folder, mox, local access
moxbase <- "~/Documents/Research/Working/mox_bumblereem/roms_for_public" # top-level folder, mox (KK's remote mount)

#--------------------
# Read hindcast data
#--------------------

for (yr in yrblock) {

  fname <- file.path(tdsbase, 
                     hcsim, 
                     "Level2",
                     paste0(yr, "-", yr+4), 
                     paste0(hcsim, "_", yr, "-", yr+4, "_average_", vname1, ".nc"))
  
  # Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == yrblock[1]){
    thc <- ttmp
    vhc <- vtmp
  } else {
    thc <- c(thc, ttmp)
    vhc <- abind(vhc, vtmp)
  }
}
 
#--------------------
# Read forecast data
# (historical period)
#--------------------

for (yr in yrblock) {
  
  fname <- file.path(moxbase, 
                     fcsim,
                     "Level2",
                     paste0(fcsim, "_", yr, "-", yr+4, "_average_", vname1, ".nc"))
  
  # Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == yrblock[1]){
    tfc <- ttmp
    vfc <- vtmp
  } else {
    tfc <- c(tfc, ttmp)
    vfc <- abind(vfc, vtmp)
  }
}

#--------------------
# Read forecast data
# (SSP)
#--------------------

for (yr in seq(2015, 2095, by=5)) { 
  
  fname <- file.path(moxbase, 
                     fcssp,
                     "Level2",
                     paste0(fcssp, "_", yr, "-", yr+4, "_average_", vname1, ".nc"))
  
  # Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == 2015){
    tssp <- ttmp
    vssp <- vtmp
  } else {
    tssp <- c(tssp, ttmp)
    vssp <- abind(vssp, vtmp)
  }
}

#-------------------
# Data adjustment 
# factors
#-------------------

avghc <- apply(vhc, c(1,2), mean)
sighc <- apply(vhc, c(1,2), sd)

avgfc <- apply(vfc, c(1,2), mean)
sigfc <- apply(vfc, c(1,2), sd)

#-------------------
# Adjust SSP
#-------------------

d <- dim(vssp)

vssp_adjusted = vssp * 0 # prob a better way to preallocate... but this works
for (ii in 1:d[3]) {
  vssp_adjusted[,,ii] <- avghc + ((sighc/sigfc) * (vssp[,,ii] - avgfc))
} 

