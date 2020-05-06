library(tidyr)
library(stringr)
library(polycor) # for hetcor function
library(ggpubr) # For ggarrange function
library(forcats) # For fct_rev function
library(jtools) # for summ function
library(rpart) # for decision tree
library(rpart.plot)
library(rattle) # for fancyRpartPlot function
library(parallel)
library(jmuOutlier) # for permutation test perm.cor.test
source('worldometers_extract.R')

# options(rsconnect.check.certificate = FALSE);rsconnect::deployApp()

DEFAULT_MIN_VAL <- .5


get_raw_data <- function() {
  # library(googlesheets)
  # gs_auth(new_user = TRUE)
  # 
  # for_gs <- gs_key("1V9zUidoZl9j-MywE9tW49hCfHfv38I_fcGm-7Tt3ryI")
  # for_gs_sheet <- gs_read(for_gs)
  #myCSV <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQLpKsvPNrQaJmCu4zdr1oLnZjcl9B3UiNv29BFQTnACrBQ5CAl19N5ZvSv3WfGclSiuL-t3rEWFSqa/pub?gid=1666407311&single=true&output=csv')
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
  names(d)[names(d) == 'Including minimal assumed years'] <- 'BCG admin years (Imp)'
  d$`Old country cohort` <- NULL
  names(d)[names(d) == 'percentage of population above 65 (2018)'] <- 
    'percentage of population above 65'
  
  if ('Active_TB_.' %in% names(d)) {
    names(d)[names(d) == 'Active_TB_.'] <- 'Percents_Active_Tuberculosis'
  }
  d$vaccinated_15_y <- factor(d$vaccinated_15_y,
                             labels=c('No BCG in last 15 years', 
                                      'BCG for < 7.5 years', 'BCG for 15 years'))
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
  
  names(d)[names(d) == "Percents_Active_TB"] <- 'Percents_Active_Tuberculosis'
  names(d)[names(d) == "Diabetes_._adults"] <- 'Percent adults T2D'
  names(d)[names(d) == "TB cases by brackets"] <- 'TBcases5Groups'
  d$TBcases5Groups <- factor(d$TBcases5Groups, 
                             labels = c('<10', '11-20', '21-50', '50-100', '100-200'),
                             ordered = TRUE)
  d$TB_high <- d$TBcases5Groups != '<10'
  # names(d)[names(d) == "Two BCG groups"] <- 'BCG2Groups'
  # d$BCG2Groups <- factor(d$BCG2Groups, 
  #                            labels = c('reccomendation for specific groups',
  #                                       'Current/past BCG policy for all'),
  #                            ordered = TRUE)
  d$BCG_for_all <- d$`Two BCG groups` == 2
  d$`Two BCG groups` <- NULL
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


get_worldometers_data <- function(end_date) {
  #covid <- read.csv('./data/world_metes_COVID_data_all_dates.csv', check.names = FALSE)
  covid <- get_worlodmeters_raw_data()
  
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
  covid <- covid[!is.na(covid$Date) & covid$Date <= end_date, ]
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

outcome_plot <- function(x, var, bcg_years_plot_only=FALSE,
                         return_fig5=FALSE) {
  #comp <- expand.grid(levels(x$TBcases5Groups), levels(x$TBcases5Groups))
  comp_list <- function(n) {
    l <- 1:n
    comp <- expand.grid(l,l)
    comp <- comp[comp[,1] < comp[,2],]
    comp <- apply(comp, 1, list)
    comp <- lapply(comp, function(i) i[[1]])
  }
  
  #comp <- lapply(1:2, function(i) lapply((i+1):3, function(j) c(i,j)))
  #levels(x$BCG2Groups) <- gsub(" ", "\n", levels(x$BCG2Groups))
  #g1 <- ggboxplot(x, x = "BCG2Groups", y = var, color = "BCG2Groups", add = c("jitter"), palette = "jco") +
  # g1 <- ggboxplot(x, x = "BCG_for_all", y = var, color = "BCG_for_all", add = c("jitter"), palette = "jco") + 
  #   stat_compare_means() +
  #   theme(legend.position = "none")
  #, plot.margin = unit(c(1,1,1,1), "lines")
  # g2 <- ggboxplot(x, x = "BCG3Groups", y = var, color = "BCG3Groups", add = c("jitter"), palette = "jco") + 
  #   stat_compare_means(label.y = min(x[,var])) + 
  #   stat_compare_means(comparisons = comp_list(nlevels(x$BCG3Groups)),
  #                      label.y = max(x[,var])*c(.9,1,.8,1.1,1)) +
  #   theme(legend.position = "none") +
  #   #, plot.margin = unit(c(2,0,0,0), "lines")
  #   expand_limits(y=max(x[,var])*1.2)
  # g3 <- ggboxplot(x, x = "TBcases5Groups", y = var, color = "TBcases5Groups", add = c("jitter"), palette = "jco") +
  #   stat_compare_means(label.y = min(x[,var])) +
  #   stat_compare_means(comparisons = comp_list(nlevels(x$TBcases5Groups)),
  #                      label.y = max(x[,var])*c(.9,1,.8,1.1,.95)) +
  #   theme(legend.position = "none")+
  #   expand_limits(y=max(x[,var])*1.2)
  
  # g5 <- ggboxplot(x, x = "TB_high", y = var, color = "TB_high", add = c("jitter"), palette = "jco") +
  #   stat_compare_means() +
  #   theme(legend.position = "none")+
  #   expand_limits(y=max(x[,var])*1.2)
  
  # Correlated BCG administration years with outcome
  names(x)[names(x) == "BCG administration years"] <- "BCG_administration_years"
  ytitle <- ifelse(var == 'total_deaths_per_1M', 'DPM diff',
                   ifelse(var == 'total_cases_per_1M', 'CPM diff',var))
  #suppressWarnings({
  gscatter <- ggscatter(data=x, x = "BCG_administration_years", y = var,
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE # Add confidence interval
    ) + stat_cor(method = "pearson") +
    xlab('Years of BCG admisnistration') +
    ylab(ytitle)
  if (bcg_years_plot_only) {
    return(gscatter)
  }
  #names(x)[names(x) == "Active_TB_%"] <- "Percents_Active_Tuberculosis"
  gscatterTB <- ggscatter(data=x, x = "Percents_Active_Tuberculosis", y = var,
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
  
  names(x)[names(x) == "BCG admin years (Imp)"] <- "BCG_years_imputed"
  gscatterMinimalAssumed <- ggscatter(data=x, x = "BCG_years_imputed", y = var,
                        add = "reg.line",  # Add regressin line
                        add.params = list(color = "blue", fill = "lightgray"),
                        conf.int = TRUE # Add confidence interval
  ) + stat_cor(method = "pearson")#, label.x = 3, label.y = 30
  
  
  #######################################
  # Additions by Danielle inserted here #
  #######################################
  # Correlated BCG administration years with outcome
  # age group: 0-24
  
  gscatter_under_25 <- ggscatter(data=x, x = "ps_under_25", y = var,
                                 xlab = "Relative BCG coverage, below 24 years population share",
                                 ylab = "Deaths per 1M",
                                 add = "reg.line",  # Add regressin line
                                 add.params = list(color = "blue", fill = "lightgray"),
                                 conf.int = TRUE # Add confidence interval
  ) + stat_cor(method = "pearson", label.x.npc = "center")
  
  # age group: 25-64
  gscatter_25_to_64 <- ggscatter(data=x, x = "ps_25_to_64", y = var,
                                 xlab = "Relative BCG coverage, 25-64 years population share",
                                 ylab = "Deaths per 1M",
                                 add = "reg.line",  # Add regressin line
                                 add.params = list(color = "blue", fill = "lightgray"),
                                 conf.int = TRUE # Add confidence interval
  ) + stat_cor(method = "pearson", label.x.npc = "center")
  
  # age group: 65+
  gscatter_over_65 <- ggscatter(data=x, x = "ps_over_65", y = var,
                                xlab = "Relative BCG coverage, above 65 years population share",
                                ylab = "Deaths per 1M",
                                add = "reg.line",  # Add regressin line
                                add.params = list(color = "blue", fill = "lightgray"),
                                conf.int = TRUE # Add confidence interval
  ) + stat_cor(method = "pearson", label.x.npc = "center")
                   
  # female share
  gscatter_female_share <- ggscatter(data=x, x = "female_share", y = var,
                                xlab = "female_share",
                                ylab = "Deaths per 1M",
                                add = "reg.line",  
                                add.params = list(color = "blue", fill = "lightgray"),
                                conf.int = TRUE 
  ) + stat_cor(method = "pearson", label.x.npc = "center")                 
  
  gscatter_median_down <- ggscatter(data=x, x = "median_down", y = var,
                                    xlab = "Relative BCG coverage, population below the median",
                                    add = "reg.line",  
                                    add.params = list(color = "blue", fill = "lightgray"),
                                    conf.int = TRUE 
  ) + stat_cor(method = "pearson", label.x.npc = "center")
  
  # above median
  gscatter_median_up <- ggscatter(data=x, x = "median_up", y = var,
                                  xlab = "Relative BCG coverage, population above the median",
                                  add = "reg.line",  
                                  add.params = list(color = "blue", fill = "lightgray"),
                                  conf.int = TRUE 
  ) + stat_cor(method = "pearson", label.x.npc = "center") 
  
  # MCV vaccine
  MCV <- ggboxplot(x, x = "MCV_group", y = var, color = "MCV_group", add = c("jitter"), palette = "jco") + 
    stat_compare_means(label.y = min(x[,var])) + 
    stat_compare_means(comparisons = comp_list(nlevels(x$MCV_group)),
                       label.y = max(x[,var])*c(.9,1,.8,1.1,1)) +
    theme(legend.position = "none") +
    expand_limits(y=max(x[,var])*1.2) 
                   
    # RCV vaccine
  RCV <- ggboxplot(x, x = "RCV_group", y = var, color = "RCV_group", add = c("jitter"), palette = "jco") + 
    stat_compare_means(label.y = min(x[,var])) + 
    stat_compare_means(comparisons = comp_list(nlevels(x$RCV_group)),
                       label.y = max(x[,var])*c(.9,1,.8,1.1,1)) +
    theme(legend.position = "none") +
    expand_limits(y=max(x[,var])*1.2) 
                   
     # 15 y vaccine
  g15 <- ggboxplot(x, x = "vaccinated_15_y", y = var, color = "vaccinated_15_y", add = c("jitter"), palette = "jco") + 
    stat_compare_means(label.y = min(x[,var])) + 
    stat_compare_means(comparisons = comp_list(nlevels(x$vaccinated_15_y)),
                       label.y = max(x[,var])*c(.9,1,.8,1.1,1)) +
    theme(legend.position = "none") +
    expand_limits(y=max(x[,var])*1.2)                 
  
  # To add Danielle's figure separetly:
  figure <- ggarrange(gscatter_under_25, gscatter_25_to_64, gscatter_over_65,g15,
                      ncol = 2, nrow = 2, labels = letters[1:3])
  if (return_fig5) {
    Dani_figure <- annotate_figure(figure,
                                   top = text_grob("Relative BCG coverage by age groups, acording to population share", 
                                                   color = "black", face = "bold", size = 14))      
    
    return(Dani_figure)
  }
  
  ###### BACK TO NADAV'S CODE
  
  ggarrange(gscatter, gscatterTB,gscatter_under_25, 
            gscatter_25_to_64, gscatter_over_65,gscatterHIV,
            gscatterHIV2, gscatterMinimalAssumed,gscatter_female_share, 
            gscatter_median_down, gscatter_median_up, g15,
            MCV, RCV,
            labels = letters[1:14],
            ncol = 2, nrow = 7)
}

get_ecdc_data <- function() {
  utils::read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                  na.strings="", fileEncoding="UTF-8-BOM")
}


get_stats <- function(covid, outcome, days, 
                      depended_var="BCG administration years",
                      premute_test=FALSE) {
  d <- droplevels(covid[!is.na(covid[, outcome]), ])
  x <- aggregate_and_merge_countries(d, outcome, days)
  
  x <- x[,c(depended_var, outcome)]
  x <- x[complete.cases(x),]
  if(nrow(x) < 2) {
    cor_res <- list(estimate=NA, p.value=NA)
    permute_pvalP <- permute_pvalS <- NA
  } else {
    cor_res <- cor.test(x[,1], x[,2], use = 'complete.obs')
    if (premute_test) {
      #permute_pval <- mean(unlist(lapply(1:10000, function(i)
      #  cor.test(x[,1], sample(x[,2]))$p.value < cor_res$p.value)))
      permute_pvalP <- perm.cor.test(x[,1], x[,2], "two.sided", "pearson", num.sim = 2000)$p.value
      #permute_pvalS <- perm.cor.test(x[,1], x[,2], "two.sided", "spearman", num.sim = 2000)$p.value
    } else {
      permute_pvalP <- permute_pvalS <- NA
    }
  }
  return(data.frame(cor=cor_res$estimate, pval=cor_res$p.value,n=nrow(x),
                    Days=days, permute_pvalP=permute_pvalP))
}

get_stats_table_outcome <- function(covid, outcome, 
                                    depended_var="BCG administration years",
                                    premute_test=FALSE) {
  d <- as.data.frame(
    do.call(rbind, lapply(seq(10, 40, 10),
                                     function(days)
                                       get_stats(covid, outcome, days, 
                                                 depended_var,
                                                 premute_test=premute_test))))
  d$Outcome <- outcome
  d
}

get_stats_table <- function(var_align, val_align,
                            depended_var="BCG administration years",
                            end_date, selected_countries=NULL,
                            get_data_only=FALSE) {
  
  covid <- get_worldometers_data(end_date)
  if (!is.null(selected_countries)) {
    covid <- droplevels(covid[covid$Country %in% selected_countries,])
  }
  covid <- align_by_var_and_val(covid, var=var_align, val_align)
  
  
  outcomes <- c('total_deaths_per_1M', 'critical_per_1M', 'total_recovered_per_1M',
                'total_cases_per_1M')
  #parallel::mc
  # mc.cores = parallel::detectCores()
  d <- do.call(rbind,
               lapply(outcomes, get_stats_table_outcome, covid=covid, 
                      # For plotting, there is no need for permutation test
                      depended_var=depended_var, premute_test=get_data_only))
  #warning(d[[1]])
  if(get_data_only) {
    return(d)
  }
  
  d$'-Log10Pval' <- round(-log10(d$pval))
  d$Pval <- factor(ifelse(d$pval<0.001, '<0.001',
                          ifelse(d$pval<0.01, '<0.01',
                                 ifelse(d$pval<0.05, '<0.05', '≥0.05'))),
                   levels = c('<0.001', '<0.01', '<0.05', '≥0.05'), ordered = TRUE)
  
  # Set colours
  col_map <- setNames(c('#5b3871', '#8a6fa0', '#d4ccde', '#D3D3D3'),
                      c("<0.001", "<0.01", "<0.05", "≥0.05"))
  d$colour <- col_map[as.character(d$Pval)]
  d$Correlation <- round(d$cor, 2)
  # Set factor level's order
  d$Outcome <- factor(d$Outcome, 
                      levels = c("total_cases_per_1M", "critical_per_1M",
                                 "total_deaths_per_1M", "total_recovered_per_1M"))
  d <- droplevels(d)

  ggplot(d,aes(x=Days,y= fct_rev(Outcome), fill = Pval,
               label=Correlation))+
    geom_point(aes(size=n), shape = 21) +
    theme_bw() +
    geom_text() +
    #scale_fill_brewer(palette="Set1") + 
    scale_size(range = range(d$n), breaks = c(10, 20, 30, 40, 50)) +
    ylab('Outcome') +
    ggtitle('Outcome\'s correlation to BCG administration period') + 
    guides(size=guide_legend(title="Number of countries")) +
    guides(fill = guide_legend(override.aes = list(size=10))) +
    scale_fill_manual(values = col_map[col_map %in% d$colour]) +
    xlab('Days from alignment')
    #theme(legend.position = "bottom", legend.box = "vertical")
}

prety_names <- function(n) {
  # Replace '_' in names with ' '
  n <- gsub('_' ,' ', n)
  n <- gsub('`' ,'', n)
  n <- gsub('\\.' ,'', n)
  all_lower_names <- n == tolower(n)
  n[all_lower_names] <- 
    stringr::str_to_sentence(n[all_lower_names])
  n[n=="obese W"] <- "Obese W"
  n[n=="obese M"] <- "Obese M"
  n[n=="population size (M)" ] <- "Population size (M)" 
  n[n == "Ps 25 to 64"] <- "PS 25 to 64"
  n[n == "Ps over 65"] <- "PS over 65"
  n[n == "Ps under 25"] <- "PS under 25"
  n[n == "smoking M"] <- "Smoking M"
  n[n == "smoking W"] <- "Smoking W"
  n[n == "total deaths per 1M"] <- "Total deaths per 1M"
  n
}

multi_var <- function(x, outcome, depended_var="BCG administration years",
                      remove_BCG=FALSE, remove_ps=FALSE, get_data_only=FALSE,
                      ps25only=FALSE) {
  #x <- aggregate_and_merge_countries(covid, outcome, days)
  names(x)[names(x) == 'TB_high'] <- 'High Tuberculosis'
  #names(x)[names(x) == 'Percents_Active_Tuberculosis'] <- 'High Tuberculosis'
  x$`percentage of population above 65` <- NULL
  x$IncomeGroup <- factor(as.numeric(x$IncomeGroup))
  x$TBcases5Groups <- NULL
  x$BCG3Groups <- NULL
  x$`percentage of population above 65 (2018)` <- NULL
  # remove median up and median down
  x$median_down <- x$median_up <- NULL
  x$vaccinated_15_y <- NULL
  # and TB_high as it is redundant with Percents_Active_Tuberculosis
  x$TB_high <- NULL
  x$`High Tuberculosis` <- NULL
  x$MCV_group <- x$RCV_group <- NULL
  x$BCG_for_all <- NULL
  if (remove_BCG) {
    x$`BCG administration years` <- NULL
    x$`median age` <- NULL
  }
  if (remove_ps) {
    x$ps_25_to_64 <- x$ps_over_65 <- x$ps_under_25 <- NULL
  }
  if (ps25only) {
    x$ps_over_65 <- x$ps_25_to_64 <- NULL
  }
  if (depended_var == "BCG administration years") {
    x$`BCG admin years (Imp)` <- NULL
  } else {
    x$`BCG administration years` <- NULL
  }
  
  # Handwash has many missings
  x$handwash. <- NULL
  x <- droplevels(x[complete.cases(x),])
  # Remove columns with single level
  x <- x[,sapply(x,nlevels) !=1]
  numeric_cols <- sapply(x, function(i) ! 'factor' %in% class(i))
  xs <- sapply(x[,numeric_cols], scale)
  x2 <- as.data.frame(cbind(xs, x[,!numeric_cols, drop=FALSE]))
  #res <- lm(as.formula(paste(outcome, '~ .')), x)
  res2 <- lm(as.formula(paste(outcome, '~ .')), x2)
  names(res2$coefficients) <- prety_names(names(res2$coefficients))
  
  #res3 <- lmer(as.formula(paste(outcome, '~ .')), x)
  #summ(res2)
  if (get_data_only) {
    return(summary(res2)$coefficients)
  }
  s <- as.data.frame(summary(res2)$coefficients)
  s <- s[order(s$Estimate),]
  s$order <- 1:nrow(s)
  s$Pval <- formatC(s$`Pr(>|t|)`, format = "e", digits = 2)
  s[s$`Pr(>|t|)`>.1, 'Pval'] <- ''
  s$Colour <- ifelse(rownames(s) %in% c('BCG administration years','BCG admin years (Imp)'),
                     'red', 'black')
  g <- coefplot::coefplot(res2, sort = "magnitude") + 
    geom_text(data=s,aes(x=Estimate,y=order,label=Pval),vjust=-.4, hjust=.5) +
    xlab('Beta coefficient') +
    theme(axis.text.y = element_text(colour = s$Colour),
          axis.text=element_text(size=12,face="bold"))
  g
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

get_regression_plot_only <- function(val_align=.5,
                                     var_align='total_deaths_per_1M',
                                     var_outcome='total_deaths_per_1M', #'critical_per_1M'#
                                     days_outcome=15) {
  #countries <- get_Danielle_data()
  covid <- get_worldometers_data(as.Date('2020-05-05'))
  covid <- align_by_var_and_val(covid, var=var_align, val_align)
  covid <- covid[!is.na(covid[,var_outcome]), ]
  covid <- droplevels(covid)
  x <- aggregate_and_merge_countries(covid, var_outcome, days_outcome) 
  outcome_plot(x, var = var_outcome, bcg_years_plot_only = TRUE)
}

fig2 <- function() {
  days <- 20
  g1 <- get_regression_plot_only(val_align = .5, var_align='total_deaths_per_1M',
                                 var_outcome='total_deaths_per_1M',days_outcome=days) +
    ggtitle(paste0('DPM diff at day ', days, ', aligned by 0.5 DPM'))
  g2 <- get_regression_plot_only(val_align = 1.5, var_align='total_deaths_per_1M',
                                 var_outcome='total_deaths_per_1M',days_outcome=days) +
    ggtitle(paste0('DPM diff at day ', days, ', aligned by 1.5 DPM'))
  g3 <- get_regression_plot_only(val_align = .5, var_align='total_deaths_per_1M',
                                 var_outcome='total_cases_per_1M',days_outcome=days) +
    ggtitle(paste0('CPM diff at day ', days, ', aligned by 0.5 DPM'))
  g4 <- get_regression_plot_only(val_align = 1.5, var_align='total_deaths_per_1M',
                                 var_outcome='total_cases_per_1M',days_outcome=days) +
    ggtitle(paste0('CPM diff at day ', days, ', aligned by 1.5 DPM'))
  ggarrange(g1, g2, g3, g4,
            ncol = 2, nrow = 2, labels = letters[1:4])
  #ggsave('../Covid_19_Research/Fig2.eps', width = 9, height = 9)
  ggsave('../Covid_19_Research/Fig2.pdf', width = 9, height = 9)
}

fig4 <- function() {
  #countries <- get_Danielle_data()
  covid <- get_worldometers_data(as.Date('2020-05-05'))
  var_align <- 'total_deaths_per_1M'
  val_align <- DEFAULT_MIN_VAL
  covid <- align_by_var_and_val(covid, var=var_align, val_align)
  
  outcome <- 'total_deaths_per_1M' #'critical_per_1M'#
  covid <- covid[!is.na(covid[,outcome]), ]
  covid <- droplevels(covid)
  days_outcome <- 15
  
  x <- aggregate_and_merge_countries(covid, outcome, days_outcome) 
  multi_var(x, outcome=outcome,
            depended_var="BCG administration years",
            remove_BCG=FALSE, remove_ps=TRUE, get_data_only=FALSE,
            ps25only=FALSE)
}

fig5 <- function() {
  #countries <- get_Danielle_data()
  covid <- get_worldometers_data(as.Date('2020-05-05'))
  var_align <- 'total_deaths_per_1M'
  val_align <- DEFAULT_MIN_VAL
  covid <- align_by_var_and_val(covid, var=var_align, val_align)
  
  outcome <- 'total_deaths_per_1M' #'critical_per_1M'#
  covid <- covid[!is.na(covid[,outcome]), ]
  covid <- droplevels(covid)
  days_outcome <- 20
  
  x <- aggregate_and_merge_countries(covid, outcome, days_outcome) 
  outcome_plot(x, var=outcome, bcg_years_plot_only=FALSE,
               return_fig5=TRUE)
  ggsave('../Covid_19_Research/Fig5.eps', width=9, height = 9)
  ggsave(paste0('../Covid_19_Research/Fig5_', days_outcome, '.pdf'), width=9, height = 9)
}

permute_test <- function() {
  set.seed(12345)
  library(jmuOutlier)    # Used for the permutation test
  library(uniftest)      # For tests of uniformity of permutation test p-values
  
  n<- 15        # Sample size
  sig<- 0.05    # Choose a desired significance level
  nrep<- 2000   # No. of repetitions for the MC experiment
  pvalp<- vector()
  tstat<- vector()
  dof<- n-1        #If there is no intercept in the fitted regression, d.o.f. = (n-1)
  tcrit<- qt(1-0.5*sig, df=dof)   #  2-sided critical value for the t-test
  beta1<- 0.0      # Intercept value
  beta2<- 0.0      # The null hypothesis is that Beta2=0
  x<- rnorm(n)     # Artificial x series, created just once
  
  for(i in 1:nrep) {               # Start of the Monte Carlo loop
    
    y<- beta1 + beta2*x + rnorm(n)   # The DGP includes an intercept
    fit<- lm(y ~ x  -1)              # A mis-specified model is estimated (unless Beta1 = 0)
    t<- unname(coef(summary(fit))[, "t value"])
    tstat<- c(tstat,t[1])   # Use 1st element because the intercept is omitted from the regression!
    
    # The simple t-test is equivalent to testing for a zero correlation between x and y, so:
    # Compute the p-value of the permutation test based on the correlation coeff. between x and y
    # (So, this is the "permutation t-test")
    
    pvp<- perm.cor.test(x, y, "two.sided", "pearson", num.sim = 2000)$p.value
    pvalp<- c(pvalp,pvp)
    
  }                         # End of the Monte Carlo loop
  
  hist(pvalp, main="Distribution of Permutation Test p-Values", xlab="p-Value", freq=FALSE, border="black", col="darkmagenta")               # This distribution of p-values should be uniform on (0,1) if the null is true
  powerp<- length(pvalp[pvalp <= sig])/nrep          # Power of the Permutation test
  powerp
  powert<- length(tstat[abs(tstat) > tcrit])/nrep    # Power of the t-test
  powert
  summary(tstat)
  kolmogorov.unif.test(pvalp)
  kuiper.unif.test(pvalp)
}


main <- function() {
  rm(list=ls())
  source('./functions.R')
  countries <- get_Danielle_data()
  covid <- get_worldometers_data(as.Date('2020-05-05'))
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
