library(tidyr)
library(stringr)
library(polycor) # for hetcor function
library(ggpubr) # For ggarrange function

# options(rsconnect.check.certificate = FALSE);rsconnect::deployApp()

DEFAULT_MIN_VAL <- 1


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
  #d <- read.csv('./data/Corona_BCG_edited_table_2020_04_11.csv', check.names = FALSE)
  #d <- read.csv('./data/Corona_BCG_edited_table_2020_04_12.csv', check.names = FALSE)
  d <- read.csv('./data/Corona_BCG_edited_table_2020_04_14.csv', check.names = FALSE)
  # Ingore first emtpy row and on
  d <- droplevels(d[1:(which(d$COUNTRY=='')[1]-1),])
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
  d$TB_high <- d$TBcases5Groups != '<10'
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

get_raw_worldometer_data <- function() {
  source('./worldometers_extract.R')
  d <- get_worlodmeters_raw_data()
}

get_worldometers_data <- function() {
  #covid <- read.csv('./data/world_metes_COVID_data_all_dates.csv', check.names = FALSE)
  covid <- get_raw_worldometer_data()
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
  # Add columns death+critical
  covid$death_or_critical_perM <- covid$critical_per_1M + covid$deaths_per_1M
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
  # Remove columns if single levels appears
  x <- x[,sapply(x, nlevels) != 1]
  #vars <- names(x)
  #names(x)[1:22] <- letters[1:22]
  #res <- lm(as.formula(paste(var, '~.')), x)
  cols_with_na <- sapply(x, anyNA)
  suppressWarnings({
    tmp <- tryCatch(
      {
        hetcor(x[,!cols_with_na], use = 'pairwise.complete.obs')
      },
      error=function(cond) {
        return(NULL)
      })
  })
  if (is.null(tmp)) {
    warning(paste('Could not compute correlations. Had only',
                  nrow(x), 'countries.'))
    return(NULL)
  }
  cors <- data.frame(Var=names(tmp$correlations[1,]),
                Cor=tmp$correlations[,ncol(tmp$correlations)], 
                Pval=tmp$tests[ncol(tmp$tests),])
  cors <- cors[complete.cases(cors),]
  if (sum(cols_with_na)) {
    bcgcor <- cor.test(x$`BCG administration years`, x[,var], use = 'pairwise.complete.obs', method = 'pearson')
    cors <- rbind(cors, 
                  data.frame(Var='BCG administration years', Cor=bcgcor$estimate, 
                             Pval=bcgcor$p.value))
    rownames(cors)[nrow(cors)] <- 'BCG administration years'
  }
  droplevels(cors[cors$Var != var,])
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
    theme(legend.position = "none")
  #, plot.margin = unit(c(1,1,1,1), "lines")
  g2 <- ggboxplot(x, x = "BCG3Groups", y = var, color = "BCG3Groups", add = c("jitter"), palette = "jco") + 
    stat_compare_means(label.y = min(x[,var])) + 
    stat_compare_means(comparisons = comp_list(nlevels(x$BCG3Groups)),
                       label.y = max(x[,var])*c(.9,1,.8,1.1,1)) +
    theme(legend.position = "none") +
    #, plot.margin = unit(c(2,0,0,0), "lines")
    expand_limits(y=max(x[,var])*1.2)
  g3 <- ggboxplot(x, x = "TBcases5Groups", y = var, color = "TBcases5Groups", add = c("jitter"), palette = "jco") +
    stat_compare_means(label.y = min(x[,var])) +
    stat_compare_means(comparisons = comp_list(nlevels(x$TBcases5Groups)),
                       label.y = max(x[,var])*c(.9,1,.8,1.1,.95)) +
    theme(legend.position = "none")+
    expand_limits(y=max(x[,var])*1.2)
  
  g5 <- ggboxplot(x, x = "TB_high", y = var, color = "TB_high", add = c("jitter"), palette = "jco") +
    stat_compare_means() +
    theme(legend.position = "none")+
    expand_limits(y=max(x[,var])*1.2)
  
  # Correlated BCG administration years with outcome
  names(x)[names(x) == "BCG administration years"] <- "BCG_administration_years"
  #suppressWarnings({
  gscatter <- ggscatter(data=x, x = "BCG_administration_years", y = var,
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE # Add confidence interval
    ) + stat_cor(method = "pearson")#, label.x = 3, label.y = 30
  
  names(x)[names(x) == "Active_TB_%"] <- "Percents_Active_TB"
  gscatterTB <- ggscatter(data=x, x = "Percents_Active_TB", y = var,
                        add = "reg.line",  # Add regressin line
                        add.params = list(color = "blue", fill = "lightgray"),
                        conf.int = TRUE # Add confidence interval
  ) + stat_cor(method = "pearson")#, label.x = .02, label.y = 30
  #})
  
  ggarrange(g1, g2, gscatter, g3, g5, gscatterTB,
            labels = LETTERS[1:4],
            ncol = 3, nrow = 2)
}

get_ecdc_data <- function() {
  data <- utils::read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                   na.strings="", fileEncoding="UTF-8-BOM")
}




main <- function() {
  rm(list=ls())
  #source('~/Dropbox (BGU)/COVID19/functions.R')
  covid <- get_worldometers_data()
  covid <- get_worldometers_data()
  var_align <- 'deaths_per_1M'
  var_outcome <- 'critical_per_1M'#'deaths_per_1M' #
  days_align <- DEFAULT_MIN_VAL
  days_outcome <- 20
  covid <- align_by_var_and_val(covid, var=var_align, days_align)
  covid <- covid[!is.na(covid[,var_outcome]), ]
  covid <- droplevels(covid)
  x <- aggregate_and_merge_countries(covid, var_outcome, days_outcome) 
  cors <- regress(covid, var_outcome, days_outcome)
  outcome_plot(x, var = var_outcome)
}