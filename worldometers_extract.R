#                               #
#   files from the world meter  #
#                               #
# @author Danielle Klinger
#setwd("/Users/dedeklinger/Desktop/COVID_BCG/date_files_world_meter")
library(reticulate)
library(dplyr)
#use_python("python3", required = T)


get_worlodmeters_raw_data <- function() {
  fname <- paste0('data/worldometer_',format(Sys.time(), "%Y_%m_%d"), '.rds')
  if (file.exists(fname)) {
    return(readRDS(fname))
  }
  
  virtualenv_create(envname = 'python3_env', 
                    python = 'python3')#/usr/bin/
  
  virtualenv_install('python3_env', 
                     packages = c('datetime', 'bs4', 'pandas', 'BeautifulSoup4'))
  
  py_run_file("./worldometers_extract.py")
  #source_python("./worldometers_extract.py")
  
  country_list <- c("Italy", "Belgium", "Netherlands", "Spain","France","Switzerland","Germany","Czechia",
                    "Austria","UK","Ireland","Portugal","Hungary","USA","Canada","Sweden",
                    "Iran","Denmark","Norway","Ecuador","Romania","DominicanRepublic","Turkey","Serbia","Greece",
                    "Israel","Finland","Moldova","BosniaandHerzegovina","Panama","Slovakia","S.Korea","Algeria",
                    "Croatia","Brazil","Poland","Peru","Bulgaria","Lebanon")
  
  # read into a df the files from world meter
  file_names <- dir("./data/worldodmeter", full.names=TRUE)
  all_dates_df <- do.call(rbind, lapply(file_names, read.csv, stringsAsFactors = FALSE)) 
  all_dates_df <- as.data.frame(apply(all_dates_df, 2, function(x)gsub('\\s+', '', x))) %>%
    select(-c(1)) %>%
    filter(Country.Other %in% country_list)
  
  all_dates_df <- with(all_dates_df,  all_dates_df[order(Country.Other) , ])
  
  # merge with the data from google docs
  #setwd("/Users/dedeklinger/Desktop/COVID_BCG")
  # this is the data from google docs (only the pop_size_M and start_date)
  info_df <- read.csv(file = './data/Corona_BCG_edited.csv') %>%
    select(c(1,10,11))
  info_df <- droplevels(info_df[1:(which(info_df$COUNTRY=='')[1]-1),])
  info_df$COUNTRY <- stringr::str_to_title(info_df$COUNTRY)
  
  all_dates_df$Country.Other <- as.character(all_dates_df$Country.Other)

  all_dates_df$Country.Other[all_dates_df$Country.Other == 'BosniaandHerzegovina'] <- "Bosnia And Herzegovina"
  all_dates_df$Country.Other[all_dates_df$Country.Other == 'DominicanRepublic'] <- "Dominican Republic"
  
  info_df$COUNTRY[info_df$COUNTRY == 'Czech'] <- "Czechia"
  info_df$COUNTRY[info_df$COUNTRY == 'Uk'] <- "UK"
  info_df$COUNTRY[info_df$COUNTRY == 'Usa'] <- "USA"
  info_df$COUNTRY[info_df$COUNTRY == 'South Korea'] <- "S.Korea"
  
  
  all_dates_merge <- merge(all_dates_df, info_df, by.x="Country.Other", by.y='COUNTRY', all=T)
  all_dates_merge <- with(all_dates_merge,  all_dates_merge[order(Country.Other) , ])
  
  all_dates_merge$TotalCases <- as.numeric(as.character(all_dates_merge$TotalCases))
  all_dates_merge$TotalDeaths <- as.numeric(as.character(all_dates_merge$TotalDeaths))
  all_dates_merge$Serious.Critical <- as.numeric(as.character(all_dates_merge$Serious.Critical))
  
  all_dates_merge <- all_dates_merge %>%
    mutate(cases_per_1M = TotalCases / population.size..M.) %>%
    mutate(deaths_per_1M = TotalDeaths / population.size..M.) %>%
    mutate(deaths_per_0.5M = TotalDeaths / population.size..M. /2) %>%
    mutate(critical_per_1M = Serious.Critical / population.size..M.) %>%
    select(-c(9,10))
  saveRDS(all_dates_merge, fname)
  all_dates_merge
}  
  