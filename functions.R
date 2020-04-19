library(tidyr)
library(stringr)
library(polycor) # for hetcor function
library(ggpubr) # For ggarrange function
library(forcats) # For fct_rev function
library(jtools) # for summ function
library(rpart) # for decision tree
library(rpart.plot)
library(rattle) # for fancyRpartPlot function

# options(rsconnect.check.certificate = FALSE);rsconnect::deployApp()

DEFAULT_MIN_VAL <- .5


get_raw_data <- function() {
  # library(googlesheets)
  # gs_auth(new_user = TRUE)
  # 
  # for_gs <- gs_key("1V9zUidoZl9j-MywE9tW49hCfHfv38I_fcGm-7Tt3ryI")
  # for_gs_sheet <- gs_read(for_gs)
  myCSV <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQLpKsvPNrQaJmCu4zdr1oLnZjcl9B3UiNv29BFQTnACrBQ5CAl19N5ZvSv3WfGclSiuL-t3rEWFSqa/pub?gid=1666407311&single=true&output=csv')
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

get_Danielle_data <- function(remove_old_cohort_flag=TRUE) {
  #d <- read.csv('./data/Corona_BCG_edited_table_2020_04_14.csv', check.names = FALSE)
  cache_fname <- 'data/latest_download.rds'
  cache_fname_date <- 'data/latest_download_date.txt'
  if(file.exists(cache_fname) && file.exists(cache_fname_date) &&
     readLines(cache_fname_date) == as.character(Sys.Date())){
    return(readRDS(cache_fname))
  }
  
  d <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQLpKsvPNrQaJmCu4zdr1oLnZjcl9B3UiNv29BFQTnACrBQ5CAl19N5ZvSv3WfGclSiuL-t3rEWFSqa/pub?gid=1666407311&single=true&output=csv',
                    check.names=FALSE)
  d$CountrySet <- factor(ifelse(d$`Old country cohort` == 1, 'A', 'B'))
  d$`Old country cohort` <- NULL
  
  if ('Active_TB_.' %in% names(d)) {
    names(d)[names(d) == 'Active_TB_.'] <- 'Percents_Active_TB'
  }
  # Remove % signs and convert to numeric
  d$`% living in urban areas` <- as.numeric(gsub('%', '', d$`% living in urban areas`))
  # Ingore first emtpy row and on
  if ('' %in% d$COUNTRY) {
    d <- droplevels(d[1:(which(d$COUNTRY=='')[1]-1),])
  }
  # Convert contries names
  d$Country <- stringr::str_to_title(d$COUNTRY)
  d$Country[d$Country == 'Czech'] <- 'Czechia'
  d$Country[d$Country %in% c('Uk', 'United Kingdom')] <- 'UK'
  d$Country[d$Country %in% c('Usa', 'United States')] <- 'USA'
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
  
  saveRDS(d, cache_fname)
  writeLines(as.character(Sys.Date()), cache_fname_date)
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
  covid$deaths_per_0.5M <- NULL
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
  #covid$death_or_critical_perM <- covid$critical_per_1M + covid$deaths_per_1M
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

regress <- function(contriesBCG, var) {
  #x <- aggregate_and_merge_countries(worldometer, var, days)
  # Remove columns if single levels appears
  x <- contriesBCG[,sapply(contriesBCG, nlevels) != 1]
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
  
  names(x)[names(x) == "HIV_prevalence_19-45_yo"] <- "HIV_prevalence_19_45_yo"
  gscatterHIV <- ggscatter(data=x, x = "HIV_prevalence_19_45_yo", y = var,
                          add = "reg.line",  # Add regressin line
                          add.params = list(color = "blue", fill = "lightgray"),
                          conf.int = TRUE # Add confidence interval
  ) + stat_cor(method = "pearson")#, label.x = .02, label.y = 30
  
  gscatterHIV2 <- ggscatter(data=x, x = "HIV_prevalence_19_45_yo", y = var,
                           add = "reg.line",  # Add regressin line
                           add.params = list(color = "blue", fill = "lightgray"),
                           conf.int = TRUE) + 
    xlim(0,min(max(x$HIV_prevalence_19_45_yo), 1.1)) +
    ggtitle('HIV, outlier removed') +
    stat_cor(method = "pearson")
  
  ggarrange(g1, g2, gscatter, g3, g5, gscatterTB,gscatterHIV,gscatterHIV2,
            labels = LETTERS[1:8],
            ncol = 3, nrow = 3)
}

get_ecdc_data <- function() {
  data <- utils::read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                   na.strings="", fileEncoding="UTF-8-BOM")
}


get_stats <- function(covid, outcome, days, country_set) {
  #outcome <- 'deaths_per_1M'
  #days <- 20
  
  d <- droplevels(covid[!is.na(covid[, outcome]), ])
  x <- aggregate_and_merge_countries(d, outcome, days)
  x <- x[x$CountrySet %in% country_set, ]
  x <- x[,c("BCG administration years", outcome)]
  x <- x[complete.cases(x),]
  if(nrow(x) < 2) {
    cor_res <- list(estimate=NA, p.value=NA)
  } else {
    cor_res <- cor.test(x[,1], x[,2], use = 'complete.obs')
  }
  return(data.frame(cor=cor_res$estimate, pval=cor_res$p.value,n=nrow(x),
                    Days=days))
}

get_stats_table_outcome <- function(covid, outcome, country_set) {
  d <- as.data.frame(do.call(rbind,
               lapply(seq(10, 30, 5),
                      function(days) get_stats(covid, outcome, days, country_set))))
  d$Outcome <- outcome
  d
}

get_stats_table <- function(var_align, val_align, country_set) {
  covid <- get_worldometers_data()
  covid <- align_by_var_and_val(covid, var=var_align, val_align)
  
  
  outcomes <- c('total_deaths_per_1M', 'critical_per_1M', 'total_recovered_per_1M',
                'total_cases_per_1M')
  d <- do.call(rbind,
               lapply(outcomes, get_stats_table_outcome, covid=covid, country_set=country_set))

  d$'-Log10Pval' <- round(-log10(d$pval))
  d$Pval <- factor(ifelse(d$pval<0.001, '<0.001', 
                          ifelse(d$pval<0.01, '<0.01', 
                                 ifelse(d$pval<0.05, '<0.05', '≥0.05'))),
                   levels = c('<0.001', '<0.01', '<0.05', '≥0.05'), ordered = TRUE)
  # Set colours
  col_map <- setNames(c('#2D6F4C', '#81BA98', '#A8E1BF', '#D3D3D3'),
                      c("<0.001", "<0.01", "<0.05", ">=0.05"))
  d$colour <- col_map[d$Pval]
  d$Correlation <- round(d$cor, 2)
  # Set factor level's order
  d$Outcome <- factor(d$Outcome, 
                      levels = c("total_cases_per_1M", "total_deaths_per_1M", 
                                 "critical_per_1M", "total_recovered_per_1M"))
  
  ggplot(d,aes(x=Days,y= fct_rev(Outcome), fill = Pval,
               label=Correlation))+
    geom_point(aes(size=n), shape = 21) +
    theme_bw() +
    geom_text() +
    #scale_fill_brewer(palette="Set1") + 
    scale_size(range = range(d$n), breaks = c(1,2,3,10,20,30)) +
    ylab('Outcome') +
    ggtitle('Outcome\'s correlation to BCG administration period') + 
    guides(size=guide_legend(title="Number of countries")) +
    scale_fill_manual(values = d$colour, labels = d$Pval)
}

multi_var <- function(x, outcome) {
  #x <- aggregate_and_merge_countries(covid, outcome, days)
  x$TBcases5Groups <- NULL
  x$BCG3Groups <- NULL
  x$`percentage of population above 65 (2018)` <- NULL
  # Handwash has many missings
  x$handwash. <- NULL
  x <- droplevels(x[complete.cases(x),])
  # Remove columns with single level
  x <- x[,sapply(x,nlevels) !=1]
  numeric_cols <- sapply(x, function(i) ! 'factor' %in% class(i))
  xs <- sapply(x[,numeric_cols], scale)
  x2 <- as.data.frame(cbind(xs, x[,!numeric_cols]))
  #res <- lm(as.formula(paste(outcome, '~ .')), x)
  res2 <- lm(as.formula(paste(outcome, '~ .')), x2)
  #res3 <- lmer(as.formula(paste(outcome, '~ .')), x)
  #summ(res2)
  coefplot::coefplot(res2, sort = "magnitude")
}


decisionTree <- function(d, outcome) {
  #warning(class(d))
  #d <- aggregate_and_merge_countries(covid, outcome, days)
  fit <- rpart(as.formula(paste(outcome, '~ .')), data = d, 
               control=rpart.control(maxdepth=5, # at most 1 split
                        cp=0, # any positive improvement will do
                        minsplit=1,
                        minbucket=2, # even leaves with 1 point are accepted
                        xval=0)) # I don't need crossvalidation
  
  #rpart.plot(fit, extra = 1, main=paste('Decistion tree for', outcome))
  #prp(fit, main=paste('Decistion tree for', outcome),varlen=3)
  fancyRpartPlot(fit, main=paste('Decistion tree for', outcome), sub="")
}

main <- function() {
  rm(list=ls())
  source('./functions.R')
  countries <- get_Danielle_data()
  covid <- get_worldometers_data()
  var_align <- 'total_deaths_per_1M'
  val_align <- DEFAULT_MIN_VAL
  covid <- align_by_var_and_val(covid, var=var_align, val_align)
  
  var_outcome <- 'total_deaths_per_1M' #'critical_per_1M'#
  covid <- covid[!is.na(covid[,var_outcome]), ]
  covid <- droplevels(covid)
  days_outcome <- 20
  
  x <- aggregate_and_merge_countries(covid, var_outcome, days_outcome) 
  cors <- regress(covid, var_outcome, days_outcome)
  outcome_plot(x, var = var_outcome)
}