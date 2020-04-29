#                               #
#   files from the world meter  #
#                               #
# @author Danielle Klinger
# Modified by Nadav Rappoport

library(reticulate)
library(dplyr)

get_worlodmeters_raw_data <- function() {
  fname <- 'data/latest_worldometer_download.rds'
  cache_fname_date <- 'data/latest_worldometer_download_date.txt'
  if(file.exists(fname) && file.exists(cache_fname_date) &&
     readLines(cache_fname_date) == as.character(Sys.Date())){
    return(readRDS(fname))
  }
  
  virtualenv_create(envname = 'python3_env', 
                    python = 'python3')#/usr/bin/
  virtualenv_install('python3_env',
                     packages = c('datetime', 'bs4', 'pandas', 'BeautifulSoup4'))
  use_virtualenv("python3_env", required = TRUE)
  
  tryCatch({
    #source_python("./worldometers_extract.py")
    py_run_file("./worldometers_extract.py")
    }, error = function(e) {
      warning(paste('Could not run worldometes_extract.py,',
      'returning latest version from', readLines(cache_fname_date)))
      return(readRDS(fname))
    })

  # country_list <- c("Italy", "Belgium", "Netherlands", "Spain","France","Switzerland","Germany","Czechia",
  #                   "Austria","UK","Ireland","Portugal","Hungary","USA","Canada","Sweden",
  #                   "Iran","Denmark","Norway","Ecuador","Romania","DominicanRepublic","Turkey","Serbia","Greece",
  #                   "Israel","Finland","Moldova","BosniaandHerzegovina","Panama","Slovakia","S.Korea","Algeria",
  #                   "Croatia","Brazil","Poland","Peru","Bulgaria","Lebanon")
  
  # read into a df the files from world meter
  file_names <- dir("./data/worldodmeter", full.names=TRUE)
  all_dates_df <- do.call(rbind, lapply(file_names, read.csv, stringsAsFactors = FALSE)) 
  all_dates_df <- as.data.frame(apply(all_dates_df, 2, function(x)gsub('\\s+', '', x))) %>%
    dplyr::select(-c(1)) #%>%
    #filter(Country.Other %in% country_list)
  
  all_dates_df <- with(all_dates_df,  all_dates_df[order(Country.Other) , ])
  
  # merge with the data from google docs
  #setwd("/Users/dedeklinger/Desktop/COVID_BCG")
  # this is the data from google docs (only the pop_size_M and start_date)
  #info_df <- read.csv(file = './data/Corona_BCG_edited.csv') %>%
  #  dplyr::select(c(1,10,11))
  info_df <- readRDS('data/latest_download.rds')[,c("Country", "population size (M)", "Date_1st_sick_perM")]
  if ('' %in% info_df$Country) {
    info_df <- droplevels(info_df[1:(which(info_df$Country=='')[1]-1),])
  }
  info_df$Country <- stringr::str_to_title(info_df$Country)
  
  all_dates_df$Country.Other <- as.character(all_dates_df$Country.Other)

  all_dates_df$Country.Other[all_dates_df$Country.Other == 'BosniaandHerzegovina'] <- "Bosnia And Herzegovina"
  all_dates_df$Country.Other[all_dates_df$Country.Other == 'DominicanRepublic'] <- "Dominican Republic"
  all_dates_df$Country.Other[all_dates_df$Country.Other == "U.K."] <- "UK"
  all_dates_df$Country.Other[all_dates_df$Country.Other == 'DominicanRepublic'] <- "Dominican Republic"
  
  info_df$Country[info_df$Country == 'Czech'] <- "Czechia"
  info_df$Country[info_df$Country == 'Uk'] <- "UK"
  info_df$Country[info_df$Country %in% c('Usa', 'United States')] <- "USA"
  info_df$Country[info_df$Country == 'South Korea'] <- "S.Korea"
  
  
  all_dates_merge <- merge(all_dates_df, info_df, by.x="Country.Other", by.y='Country', all=T)
  all_dates_merge <- with(all_dates_merge,  all_dates_merge[order(Country.Other) , ])
  
  all_dates_merge$TotalCases <- as.numeric(as.character(all_dates_merge$TotalCases))
  all_dates_merge$TotalDeaths <- as.numeric(as.character(all_dates_merge$TotalDeaths))
  all_dates_merge$TotalRecovered <- as.numeric(as.character(all_dates_merge$TotalRecovered))
  all_dates_merge$Serious.Critical <- as.numeric(as.character(all_dates_merge$Serious.Critical))
  
  all_dates_merge <- all_dates_merge %>%
    mutate(total_cases_per_1M = TotalCases / `population size (M)`) %>%
    mutate(total_deaths_per_1M = TotalDeaths / `population size (M)`) %>%
    mutate(critical_per_1M = Serious.Critical / `population size (M)`) %>%
    mutate(total_recovered_per_1M = TotalRecovered / `population size (M)`) %>%
    dplyr::select(-c(9,10))
  saveRDS(all_dates_merge, fname)
  writeLines(as.character(Sys.Date()), cache_fname_date)
  all_dates_merge
}
