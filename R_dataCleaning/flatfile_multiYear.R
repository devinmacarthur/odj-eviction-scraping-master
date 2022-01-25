library(fastDummies)
library(DBI)
library(stringr)
library(dplyr)
library(ggplot2)
library(fastDummies)
library(tidyr)
library(rstudioapi)


#Set today
today <- Sys.Date()
today <- format(today, format = "%Y%m%d")
#today <- "20211206"

getDates <- c("2020", "2021", "2022")

getCaseOverviews <- function(i){
  db_name <- paste("ojdevictions", i, sep = "_")
  con <- dbConnect(RPostgres::Postgres(),dbname = db_name,
                   host = 'localhost',
                   port = '5432',
                   user = 'postgres',
                   password = 'admin')
  query <- dbSendQuery(con, 'SELECT * FROM "case-overviews"')
  case_overviews <- dbFetch(query)
  dbClearResult(query)
  return(case_overviews)
}

getCaseParties <- function(i){
  db_name <- paste("ojdevictions", i, sep = "_")
  con <- dbConnect(RPostgres::Postgres(),dbname = db_name,
                   host = 'localhost',
                   port = '5432',
                   user = 'postgres',
                   password = 'admin')
  query <- dbSendQuery(con, 'SELECT * FROM "case-parties"')
  case_parties <- dbFetch(query)
  dbClearResult(query)
  return(case_parties)
}

getEvents <- function(i){
  db_name <- paste("ojdevictions", i, sep = "_")
  con <- dbConnect(RPostgres::Postgres(),dbname = db_name,
                   host = 'localhost',
                   port = '5432',
                   user = 'postgres',
                   password = 'admin')
  query <- dbSendQuery(con, 'SELECT * FROM "events"')
  events <- dbFetch(query)
  dbClearResult(query)
  return(events)
}

getFiles <- function(i){
  db_name <- paste("ojdevictions", i, sep = "_")
  con <- dbConnect(RPostgres::Postgres(),dbname = db_name,
                   host = 'localhost',
                   port = '5432',
                   user = 'postgres',
                   password = 'admin')
  query <- dbSendQuery(con, 'SELECT * FROM "files"')
  files <- dbFetch(query)
  dbClearResult(query)
  return(files)
}

getJudgments <- function(i){
  db_name <- paste("ojdevictions", i, sep = "_")
  con <- dbConnect(RPostgres::Postgres(),dbname = db_name,
                   host = 'localhost',
                   port = '5432',
                   user = 'postgres',
                   password = 'admin')
  query <- dbSendQuery(con, 'SELECT * FROM "judgments"')
  judgments <- dbFetch(query)
  dbClearResult(query)
  return(judgments)
}

getLawyers <- function(i){
  db_name <- paste("ojdevictions", i, sep = "_")
  con <- dbConnect(RPostgres::Postgres(),dbname = db_name,
                   host = 'localhost',
                   port = '5432',
                   user = 'postgres',
                   password = 'admin')
  query <- dbSendQuery(con, 'SELECT * FROM "lawyers"')
  lawyers <- dbFetch(query)
  dbClearResult(query)
  return(lawyers)
}

case_overviews <- Reduce("rbind", lapply(getDates, getCaseOverviews))
case_parties <- Reduce("rbind", lapply(getDates, getCaseParties))
events <- Reduce("rbind", lapply(getDates, getEvents))
files <- Reduce("rbind", lapply(getDates, getFiles))
judgments <- Reduce("rbind", lapply(getDates, getJudgments))
lawyers <- Reduce("rbind", lapply(getDates, getLawyers))



getDefendantInfo <- function() {
  case_parties %>%
    filter(party_side == "Defendant") %>%
    group_by(case_code) %>%
    summarize(defendant_names = paste(name, collapse = "; "),
              defendant_addr = paste(unique(addr), collapse = "; ")) %>% 
    return()
}

getAgent <- function() {
  case_parties %>%
    filter(party_side == "Agent") %>%
    group_by(case_code) %>%
    summarize(Agent = paste(name, collapse = "; ")) %>% 
    return()
}

createJudgmentDummies <- function() {
  judgments %>% 
    fastDummies::dummy_cols(select_columns = "case_type") %>% 
    group_by(case_code) %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    summarise(case_code = case_code,
              Judgment_General = ifelse(`case_type_Judgment - General` > 0 | 
                                          `case_type_Amended Judgment - General` > 0 |
                                          `case_type_Amended Judgment - Corrected General` > 0 | 
                                          `case_type_Judgment - Corrected General` |
                                          `case_type_Judgment - General Creates Lien` > 0 |
                                          `case_type_Amended Judgment - General Creates Lien` > 0 |
                                          `case_type_Judgment - Corrected General Creates Lien` > 0, 1, 0),
              Judgment_Creates_Lien = ifelse(`case_type_Judgment - General Creates Lien` > 0 |
                                               `case_type_Judgment - Supplemental Creates Lien` > 0 |
                                               `case_type_Judgment - Limited Creates Lien` > 0 |
                                               `case_type_Amended Judgment - General Creates Lien` > 0 |
                                               `case_type_Judgment - General Dismissal Creates Lien` > 0 |
                                               `case_type_Amended Judgment - Corrected General Creates Lien` > 0 |
                                               `case_type_Judgment - Corrected General Creates Lien` > 0 |
                                               `case_type_Amended Judgment - Supplemental Creates Lien` > 0 |
                                               `case_type_Amended Judgment - Limited Creates Lien` > 0 |
                                               `case_type_Amended Judgment - Corrected Limited Creates Lien` > 0 |
                                               `case_type_Amended Judgment - Corrected Supplemental Creates Lien` > 0 |
                                               `case_type_Judgment - Corrected Supplemental Creates Lien` > 0 |
                                               `case_type_Judgment - Limited Dismissal Creates Lien` > 0, 1, 0),
              Judgment_Dismissal = ifelse(`case_type_Judgment - General Dismissal` > 0 |
                                            `case_type_Judgment - Limited Dismissal` > 0 |
                                            `case_type_Amended Judgment - General Dismissal` > 0, 1, 0)) %>% 
    #`case_type_Amended Judgment - Limited Dismissal` > 0, 1, 0)) %>% 
    return()
}

addMoratoriumVars <- function() {
 case_overviews %>% 
    mutate(date2 = as.Date(case_overviews$date, "%m/%d/%Y"),
           Oregon_Moratorium = if_else(date2 >= as.Date('2020-3-22'), 1, 0),
           Multnomah_Moratorium = if_else(date2 >= as.Date('2020-3-17') & location == "Multnomah", 1, 0)) %>% 
    return()
}

getPlaintifNames <- function() {
  case_parties %>% 
    filter(party_side == "Plaintiff") %>% 
    group_by(case_code) %>% 
    summarize(plaintiff_name = paste(name, collapse = "; ")) %>% 
    return()
}

getLawyersByParty <- function() {
  case_parties %>%
    rename(party_name = name) %>% 
    select(case_code, party_name, party_side) %>% 
    right_join(lawyers %>% 
                 rename(lawyerName = name) %>%
                 select(case_code, party_name, lawyerName, status), by = c('case_code', 'party_name')) %>% 
    return()
}


getDefendantLawyers <- function() {
  getLawyersByParty() %>% 
    filter(party_side == "Defendant") %>% 
    group_by(case_code) %>% 
    summarize(party = paste(unique(party_name), collapse = "; "), 
              tenant_lawyer = paste(unique(lawyerName), collapse = "; ")) %>%
    return()
}

getPlaintiffLawyer <- function() {
  getLawyersByParty() %>% 
    filter(party_side == "Plaintiff") %>% 
    group_by(case_code) %>% 
    summarize(party = paste(unique(party_name), collapse = "; "), 
              landlord_lawyer = paste(unique(lawyerName), collapse = "; ")) %>% 
    return()
}

makeFTAvars <- function() {
  #makes Failure to Appear variable
  events %>% 
    filter(result == "FTA - Default" | result == "Failure to Appear") %>% 
    distinct(case_code) %>% 
    mutate(FTA = 1) %>% 
    return()
}

makeFTADefault <- function() {
  #makes Failure to Appear variable
  events %>% 
    filter(result == "FTA - Default") %>% 
    distinct(case_code) %>% 
    mutate(FTADefault = 1) %>% 
    return()
}

#x <- makeFTADefault()

makeFTAFirstAppearance <- function() {
  events %>% 
    filter(grepl("hearing", title, ignore.case = TRUE)) %>%
    mutate(firstHearing = !duplicated(case_code)) %>% 
    select(firstHearing, case_code, title, result) %>% 
    filter(firstHearing == "TRUE") %>% 
    filter(grepl("FTA|Failure to Appear", result, ignore.case = TRUE)) %>% 
    mutate(FTAFirst = 1) %>% 
    return()
}

makeFlatFile <- function() {
  # makes final flat_file output
  addMoratoriumVars() %>% 
    select(case_code, style, date, Oregon_Moratorium, Multnomah_Moratorium, status, location) %>% 
    full_join(getPlaintifNames() %>% select(case_code, plaintiff_name), by = 'case_code') %>% 
    full_join(getDefendantInfo() %>% select(case_code, defendant_names, defendant_addr), by = 'case_code') %>% 
    full_join(getAgent() %>% select(case_code, Agent)) %>% 
    full_join(createJudgmentDummies() %>% select(case_code, Judgment_General, 
                                                 Judgment_Creates_Lien, 
                                                 Judgment_Dismissal), by = 'case_code') %>% 
    full_join(getDefendantLawyers() %>% select(case_code, tenant_lawyer), by = 'case_code') %>% 
    full_join(getPlaintiffLawyer() %>% select(case_code, landlord_lawyer), by = 'case_code') %>% 
    full_join(makeFTAvars(), by = 'case_code') %>%
    full_join(makeFTADefault(), by = 'case_code') %>%
    full_join(makeFTAFirstAppearance() %>% select(case_code, FTAFirst), by = 'case_code') %>% 
    mutate(landlord_has_lawyer = ifelse(is.na(landlord_lawyer), 0, 1),
           tenant_has_lawyer = ifelse(is.na(tenant_lawyer), 0, 1),
           FTA = ifelse(is.na(FTA), 0, 1),
           FTADefault = ifelse(is.na(FTADefault), 0, 1),
           FTAFirst = ifelse(is.na(FTAFirst), 0, 1),
           FTAFirstXJudgmentGeneral = ifelse(Judgment_General == 1 & FTAFirst == 1, 1, 0),
           date = as.Date(date, "%m/%d/%Y"),
           month = as.Date(cut(date, breaks = "month")),
           # no_judgment = ifelse(status == "Closed" & judgment == "NULL", 1, 0),
           zip = word(defendant_addr, -1)) %>% 
    rename(case_name = style) %>% 
    return()
}

saveTablesRDS <- function() {
  saveRDS(makeFlatFile(), "flat_file.rds")
  saveRDS(getCaseOverviews(),  "case_overviews.rds")
  saveRDS(getCaseParties(), "case_parties.rds")
  saveRDS(getEvents(), "events.rds")
  saveRDS(getJudgments(), "judgments.rds")
  saveRDS(getLawyers(), "lawyers.rds")
  saveRDS(getFiles(),  "files.rds")
}


saveTablesCSV <- function(){
  write.csv(makeFlatFile(), paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/flat_file.csv", sep = ""))
  write.csv(case_overviews, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/case_overviews.csv", sep = ""))
  write.csv(case_parties, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/case_parties.csv", sep = ""))
  write.csv(events, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/events.csv", sep = ""))
  write.csv(judgments, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/judgments.csv", sep = ""))
  write.csv(lawyers, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/lawyers.csv", sep = ""))
  write.csv(files, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/files.csv", sep = ""))
}

# set save directory
#today <- "20210921"

# dir.create(paste("output/fullScrape", today, sep = ""))
# dir.create(paste("output/csv/full_Scrape", today, sep = "_"))

# Execute
# saveTablesRDS()

dir.create(paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, sep =""))
saveTablesCSV()

# file.copy(paste("output/csv/full_scrape", today, sep = "_"), paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape", today, sep ="_"), recursive = TRUE)




