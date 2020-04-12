library(tidyr)
library(stringr)
library(polycor) # for hetcor function
library(ggpubr) # For ggarrange function

# options(rsconnect.check.certificate = FALSE);rsconnect::deployApp()

get_raw_data <- function() {
  # library(googlesheets)
  # gs_auth(new_user = TRUE)
  # 
  # for_gs <- gs_key("1V9zUidoZl9j-MywE9tW49hCfHfv38I_fcGm-7Tt3ryI")
  # for_gs_sheet <- gs_read(for_gs)
  
  #myCSV<-read.csv("http://docs.google.com/spreadsheets/d/1V9zUidoZl9j-MywE9tW49hCfHfv38I_fcGm-7Tt3ryI/pub?output=csv")
  d <- read.csv('./data/Corona BCG initial table.csv')
  names(d) <- gsub('\\.', ' ', gsub('\\.\\.', ' ', names(d)))

  fix_columns(d)
}

fix_columns <- function(d) {
  int_cols <- c("age if recieve ")
  
  for (colname in int_cols) {
    if (class(d[,colname]) == 'factor')
      d[,colname] <- as.integer(as.character(d[,colname]))
    else
      d[,colname] <- as.integer(d[,colname])
  }
  d
}

to_long <- function(d) {
  df <- d[,grep('0d', names(d))]
  df$Country <- d$COUNTRY
  df <- tidyr::gather(df, var, value, `infected_10d`:`per_death_of_infected_growth_30d`, factor_key=TRUE)
  df$value <- as.numeric(df$value)
  df$Days <- as.integer(str_extract(string = df$var, pattern = "[0-9]0"))
  df$Var <- gsub('_[1-9]0d','', df$var)
  df
}

add_variables <- function(df) {
  df <- merge(df, d[,c('COUNTRY', 'age if recieve ')], by.x='Country', by.y='COUNTRY')
  df$BCG <- !is.na(df$`age if recieve `)
}

get_time_series_data <- function(cases='deaths', region='global', as_long=TRUE) {
  if (! cases %in% c('deaths', 'confirmed', 'recovered')) {
    warning(paste('Can not find', cases, 'cases'))
    return(NA)
  }
  if (! region %in% c('US', 'global')) {
    warning(paste('Can not find', cases, 'region. Can only be one of US, global'))
    return(NA)
  }
  
  tmp <- read.csv(paste0('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_',
                         cases, '_', region, '.csv'),
                  check.names = FALSE)
  tmp$Lat <- tmp$Long <- tmp$`Province/State` <- NULL
  # Aggregate by Country
  tmp <- aggregate(tmp[,2:76], by=list(Country=tmp$`Country/Region`), FUN=sum)
  if(as_long) {
    tmp <- tidyr::gather(tmp, Date, value, 2:ncol(tmp), factor_key=TRUE)
    tmp$Date <- as.Date(tmp$Date, "%m/%d/%y")
  }
  tmp$Var <- cases
  tmp
}

merge_data <- function() {
  l1 <- lapply(c('deaths', 'confirmed', 'recovered'),
              get_time_series_data, region='global')
  df <- do.call(rbind, l1)
  df$Var <- as.factor(df$Var)
  df
}

set_key_event_as_min_death <- function(df, min_death=10) {
  tmp <- df[df$Var == 'deaths' & df$value >= 10, ]
  tmp2 <- tapply(tmp$Date, tmp$Country, min)
  key_dates <- as.Date(tmp2, origin='1970-01-01')
  key_dates <- key_dates[!is.na(key_dates)]
  
  df <- df[df$Country %in% names(key_dates),]
  for (country in names(key_dates)) {
    df[df$Country == country, 'Days'] <- 
      as.integer(df[df$Country == country, 'Date'] - key_dates[country],
                 units='days')
  }
  #df <- df[df$Days >= 0,]
  droplevels(df)
}

get_covid_data <- function(min_death=10, from_cache=TRUE) {
  fname <- paste0('./data/cache_covid_data_',
                  'min_death_', min_death, '_',
                  format(Sys.time(), "%Y_%m_%d"), '.rds')
  if(from_cache & file.exists(fname))
    return(readRDS(fname))
  d <- set_key_event_as_min_death(merge_data(), min_death)
  saveRDS(d, fname)
  d
}

get_Danielle_data <- function() {
  #d <- read.csv('./data/Corona_BCG_edited_table_2020_04_10.csv', check.names = FALSE)
  d <- read.csv('./data/Corona_BCG_edited_table_2020_04_11.csv', check.names = FALSE)
  # Ingore first emtpy row and on
  d <- d[1:(which(d$COUNTRY=='')[1]-1),]
  # Convert contries names
  d$Country <- stringr::str_to_title(d$COUNTRY)
  d$Country[d$Country == 'Czech'] <- 'Czechia'
  d$Country[d$Country == 'Uk'] <- 'UK'
  d$Country[d$Country == 'Usa'] <- 'USA'
  #d$Country[d$Country == 'Bosnia And Herzegovina'] <- 'Bosnia and Herzegovina'
  d$Country[d$Country == 'South Korea'] <- 'S.Korea'
  d$Country <- as.factor(d$Country)
  rownames(d) <- d$Country
  d$COUNTRY <- NULL
  names(d)[names(d) == "TB cases by brackets"] <- 'TBcases5Groups'
  d$TBcases5Groups <- factor(d$TBcases5Groups, 
                             labels = c('<10', '11-20', '21-50', '50-100', '100-200'),
                             ordered = TRUE)
  names(d)[names(d) == "Two BCG groups"] <- 'BCG2Groups'
  d$BCG2Groups <- factor(d$BCG2Groups, 
                             labels = c('reccomendation for specific groups',
                                        'Current/past BCG policy for all'),
                             ordered = TRUE)
  names(d)[names(d) == "3 BCG groups"] <- 'BCG3Groups'
  d$BCG3Groups <- factor(d$BCG3Groups, ordered = TRUE)
  
  names(d)[names(d) == "income group brackets"] <- 'IncomeGroup'
  d$IncomeGroup <- factor(d$IncomeGroup, 
                             labels = c('high income', 'Upper middle income',
                                        'lower middle income'),
                          ordered = TRUE)
  # date of 1 sick per 1M
  # "Feb 22"  -> date
  d$Date_1st_sick_perM <- as.Date(d$`date of 1 sick per 1M`, format='%b %d')
  d$`date of 1 sick per 1M` <- NULL
  d
}

data_per_milion <- function(covid, countries) {
  tmp <- apply(covid, 1, function(x)
    as.numeric(x[3])/countries[as.character(x[1]), 'population size (M)'])
  covid$value <- tmp
  levels(covid$Var) <- paste0(levels(covid$Var), 'PerM')
  covid
}

get_covid_normalized <- function() {
  covid <- get_covid_data()
  countries <- get_Danielle_data()
  covid <- droplevels(covid[covid$Country %in% levels(countries$Country),])
  countries <- droplevels(countries[countries$Country %in% levels(covid$Country),])
  covid <- rbind(covid, data_per_milion(covid, countries))
  covid
}

get_worldometers_data <- function() {
  covid <- read.csv('./data/world_metes_COVID_data_all_dates.csv', check.names = FALSE)
  names(covid)[names(covid)=='Country.Other'] <- 'Country'
  # There are country names with leading spaces
  covid$Country <- gsub('^ ', '', covid$Country)
  covid$Country <- gsub(' $', '', covid$Country)
  covid$Country[covid$Country == 'BosniaandHerzegovina'] <- "Bosnia And Herzegovina"
  covid$Country[covid$Country == 'DominicanRepublic'] <- "Dominican Republic"
  if (names(covid)[1]=='') {
    covid[,1] <- NULL
  }
  covid$X <- NULL
  # 223 -> February second
  # First, integer to character, and then add space on second character
  covid$Date <- gsub('^([0-9]{1})([0-9]{2})$', '\\1 \\2', as.character(covid$Date))
  covid$Date <- as.Date(covid$Date, format='%m %d')
  # Many -1s, repalce with NA
  covid[covid==-1] <- NA
  covid$start_date <- NULL
  covid
}


align_by_var_and_val <- function(covid, var, value) {
  tmp <- covid[!is.na(covid[,var]) & covid[,var] >= value, c('Country', var, 'Date')]
  tmp <- tmp[order(tmp$Date),]
  tmp <- tmp[!duplicated(tmp$Country),c('Country', 'Date')]
  #key_dates <- as.Date(tmp2, origin='1970-01-01')
  #key_dates <- key_dates[!is.na(key_dates)]
  key_dates <- setNames(tmp$Date, tmp$Country)
  
  covid <- covid[covid$Country %in% names(key_dates),]
  for (country in names(key_dates)) {
    covid[covid$Country == country, 'Days'] <- 
      as.integer(covid[covid$Country == country, 'Date'] - key_dates[country],
                 units='days')
  }
  #covid <- covid[covid$Days >= 0,]
  droplevels(covid)
}

aggregate_and_merge_countries <- function(worldometer, var, days) {
  y <- worldometer[worldometer$Days %in% c(0,days), c('Country', var, 'Days')]
  y <- y[!is.na(y[,var]),]
  # Remove those with no two datapoints
  y <- y[y$Country %in% y$Country[duplicated(y$Country)],]
  y <- aggregate(as.formula(paste(var, '~ Country')), y, function(x) max(x)-min(x))
  x <- get_Danielle_data()
  x <- droplevels(merge(x,y))
  x$Date_1st_sick_perM <- NULL
  #countries <- x$Country
  x$Country <- NULL
  # Remove columns handwash as many NAs
  x$`handwash%` <- NULL
  x
}

regress <- function(worldometer, var, days) {
  x <- aggregate_and_merge_countries(worldometer, var, days)
  #vars <- names(x)
  #names(x)[1:22] <- letters[1:22]
  #res <- lm(as.formula(paste(var, '~.')), x)
  suppressWarnings({
    tmp <- hetcor(x[,2:ncol(x)])
  })
  cors <- data.frame(Var=names(x)[2:ncol(x)],
                Cor=tmp$correlations[,ncol(x)-1], 
                Pval=tmp$tests[ncol(x)-1,])
  cors <- cors[complete.cases(cors),]
  droplevels(cors[1:(nrow(cors)-1),])
}

outcome_plot <- function(x, var) {
  #comp <- expand.grid(levels(x$TBcases5Groups), levels(x$TBcases5Groups))
  comp_list <- function(n) {
    l <- 1:n
    comp <- expand.grid(l,l)
    comp <- comp[comp[,1] < comp[,2],]
    comp <- apply(comp, 1, list)
    comp <- lapply(comp, function(i) i[[1]])
  }
  
  #comp <- lapply(1:2, function(i) lapply((i+1):3, function(j) c(i,j)))
  levels(x$BCG2Groups) <- gsub(" ", "\n", levels(x$BCG2Groups))
  g1 <- ggboxplot(x, x = "BCG2Groups", y = var, color = "BCG2Groups", add = c("jitter"), palette = "jco") + 
    stat_compare_means() +
    theme(legend.position = "none", plot.margin = unit(c(1,1,1,1), "lines"))
  g2 <- ggboxplot(x, x = "BCG3Groups", y = var, color = "BCG3Groups", add = c("jitter"), palette = "jco") + 
    stat_compare_means() + 
    stat_compare_means(comparisons = comp_list(nlevels(x$BCG3Groups))) +
    theme(legend.position = "none", plot.margin = unit(c(1,1,1,1), "lines"))
  g3 <- ggboxplot(x, x = "TBcases5Groups", y = var, color = "TBcases5Groups", add = c("jitter"), palette = "jco") +
    stat_compare_means() +
    stat_compare_means(comparisons = comp_list(nlevels(x$TBcases5Groups))) +
    theme(legend.position = "none", plot.margin = unit(c(1,1,1,1), "lines"))
  ggarrange(g1, g2, g3, 
            labels = c("A", "B", "C"),
            ncol = 2, nrow = 2,
            heights = c(8, 8), align = "v")
}
