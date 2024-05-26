
# Making a file combining all the session data from individual files to one file

## Loading needed packages
library(tidyverse)
library(readxl)
library(lubridate)

# List all files in the data folder
files <- list.files("data/session-data/")

# Make a list to store imports
file.list <- list()

# Loop over the list of files and read individual files
for(i in 1:length(files)) {
  
 file.list[[i]] <- read_excel(paste0("data/session-data/", files[i]), 
                              na = "na")

}

# Bind all files together
dat <- bind_rows(file.list)
dat


## Save imported data 
saveRDS(dat, "data/derivedData/session-data.RDS")


# filter out the 2 first mins of every bout
# for some reason i had to write one hour higher than intended...
# maybe the time-zone for the date variable [time] is different from the time-zone on my computer?
dat_6min <- dat %>% 
  filter(time >= "1899-12-31 03:00:00", 
         time <= "1899-12-31 09:00:00")

## save filtered (6min) imported data
saveRDS(dat_6min, "data/derivedData/session-data-6min.RDS")


# filter out the 4 first mins of every bout
dat_4min <- dat %>% 
  filter(time >= "1899-12-31 05:00:00", 
         time <= "1899-12-31 09:00:00")

## save filtered (4min) imported data
saveRDS(dat_4min, "data/derivedData/session-data-4min.RDS")

