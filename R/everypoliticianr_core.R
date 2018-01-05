#Basic api access to the everypolitician json files.

library(jsonlite)

expand_period_start_dates <- function(pop){
  #replace na values for start and end dates of memberships with legislative
  #period start and end dates
  memberships = pop$memberships
  periods = subset(pop$events, classification == "legislative period",
                   select=c(id,start_date,end_date))
  periods <- setNames(periods, c("legislative_period_id",
                                 "period_start_date",
                                 "period_end_date"))
  m = merge(memberships,periods,"legislative_period_id")
  m$start_date[is.na(m$start_date)] = m$period_start_date[is.na(m$start_date)]
  m$end_date[is.na(m$end_date)] = m$period_end_date[is.na(m$end_date)]
  pop$memberships = m
  pop$periods = periods
  return(pop)
}

#' EveryPolitician
#'
#' Downloads a popolo file from EveryPolitician or loads a local file
#' @param country_name #Country name as listed on everypolitician.org
#' @param chamber_name Name of chamber if a country has multiple (not required if country only has one in EveryPolitician)
#' @param popolo_file Direct path to popolo file stored locally or online. Will take priority over other parameters. 
#' everypolitician()
everypolitician <- function(country_name="",chamber_name="",popolo_file="") {
  
  default_countries_json = 'https://raw.githubusercontent.com/everypolitician/everypolitician-data/master/countries.json'
  
  if (country_name == "" & popolo_file == "") {
    stop("Requires a country_slug or a popolo_file")
  }
  
  if (popolo_file == ""){
    countries <- fromJSON(txt=default_countries_json)
    country_row = which(countries["name"] == country_name)
    if (length(country_row) == 0) {
      stop("Invalid Country Name")
    }
    country = countries[country_row,]
    
    legislatures = country$legislatures[[1]]
    if (nrow(legislatures) == 1) {
      chamber = legislatures[1,]
    } else {
      row = which(legislatures["name"] == chamber_name)
      if (length(row) == 0) {
        stop("Multiple chambers for this country, but chamber name either invalid or unspecified.")
      }
      chamber = legislatures[row,]
    }
    popolo <- fromJSON(txt=chamber$popolo_url)
    
  } else {
    popolo <- fromJSON(txt=popolo_file)
  }
  
  popolo <- expand_period_start_dates(popolo)
  
  return(popolo)
}

#' EveryPolitician ID Lookup
#'
#' Creates a lookup data frame from the alternate identifiers stored in the popolo file. 
#' @param popolo #popolo object to get lookup from
#' @param scheme alternate ID scheme. e.g use 'parlparse' to create a two-column lookup between id and the parlparse ids. 
#' ep_id_lookup()
ep_id_lookup <- function(popolo,scheme) {
  persons <- popolo$persons
  extract_identifer <- function (row) {
    identifers = row$identifiers
    scheme_row = which(identifers["scheme"] == scheme)
    if (length(scheme_row) == 0){
      id = ""
    } else {
      id = identifers[scheme_row,"identifier"]
    }
    return(id)
  }
  
  id_lookup = persons[c("id")]
  id_lookup <- setNames(id_lookup, c("ep_id"))
  id_lookup[scheme] = apply(persons,1,extract_identifer)
  return(id_lookup)
}

#' Approximate Date fixer function
#'
#' Not all everypolitician dates are complete,
#' This coerces incomplete dates to the first day in month or year.
#' @param date_string #popolo object to get lookup from
#' approx_date()
approx_date <- function(date_string) {
  #
  #
  if (is.na(date_string)) {
    return(date_string)
  }
  if (nchar(date_string) == 4){ #expand year-only dates
    date_string = paste(date_string,"01-01",sep="-")
  } else if (nchar(date_string) == 7) { #expland year-and-month dates
    date_string = paste(date_string,"01",sep="-")
  }
  v = as.Date(date_string)
}


#' Alternative Names
#'
#' Expands all alternative names + normal names to create data.frame that links names to ep_ids
#' @param popolo #popolo object to get lookup from
ep_alt_name_list <- function (popolo) {
  normal_names = subset(popolo$persons,select=c("id","name"))
  persons = subset(popolo$persons,select=c("id","other_names"))
  
  unfold_names <- function (row){
    all_names = row["other_names"][[1]]
    
    if (is.null(all_names)) {
      return (c(""))
    } else {
      just_names = unique(apply(all_names,1,function (x) {return(x["name"])}))
      return(just_names)
    }
  }
  
  persons$other_names = apply(persons,1,unfold_names)
  persons <- setNames(persons, c("id",
                                 "name"))
  persons <- unnest(persons)
  persons <- persons[-which(persons$name == ""), ]
  
  combo <- rbind(normal_names,persons)
  combo <- setNames(combo, c("ep_id",
                                "name"))
}
