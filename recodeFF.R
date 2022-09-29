source("flatfile_multiYear.R")

# coding judgments better
codeJudgments <- function(i){
  fj <- judgments %>% 
    filter(!grepl("supplemental", case_type, ignore.case = T)) %>% 
    filter(!grepl("vacated", decision, ignore.case = T)) %>% 
    filter(case_code == i)
  j <- if(n_distinct(fj$case_type) == 1) {unique(fj$case_type)}
  else if(nrow(fj) == 0) {"No Judgment"} 
  else{NA}
  return(j)
}

# undebug(codeJudgments)

codeLien <- function(i){
  fl <- judgments %>% 
    filter(grepl("lien", case_type, ignore.case = T)) %>% 
    filter(!grepl("vacated", decision, ignore.case = T)) %>% 
    filter(case_code == i)
  L <- if(nrow(fl) > 0) {1} else{0}
  return(L)
}

codeSA <- function(i){
  e <- events %>% filter(case_code == i)
  s <- e %>% filter(grepl("stipulated|agreement", title, ignore.case = T) | grepl("stipulated", status, ignore.case = T))
  sa <- ifelse(nrow(s)>0, 1, 0)
  return(sa)
  }

codeSANC <- function(i){
  e <- events %>% filter(case_code == i)
  s <- e %>% filter(grepl("non-compliance|non compliance", title, ignore.case = T))
  sanc <- ifelse(nrow(s)>0, 1, 0)
  return(sanc)
}

codeFA <- function(i){
  e <- events %>% filter(case_code == i)
  h <- e %>% filter(grepl("hearing|appearance", title, ignore.case = T))
  FA <- ifelse(nrow(h)>0, 1, 0)
  return(FA)
}

codeWrit <- function(i){
  e <- events %>% filter(case_code == i)
  h <- e %>% filter(grepl("writ", title, ignore.case = T))
  writ <- ifelse(nrow(h)>0, 1, 0)
  return(writ)
}

Lien <- sapply(case_overviews$case_code, codeLien) %>% as.data.frame()
colnames(Lien)[1] <- "Judgment_Creates_Lien"
Lien$case_code <- row.names(Lien)
Lien <- Lien %>% select(-case_code)

# y <- judgments %>% group_by(case_code) %>% summarise(n=n()) %>% filter(n>1)
# j1 <- sapply(y$case_code, codeJudgments) %>% as.data.frame()

j <- sapply(case_overviews$case_code, codeJudgments) %>% as.data.frame()
colnames(j)[1] <- "j"
j$case_code <- row.names(j)

SA <- sapply(case_overviews$case_code, codeSA) %>% as.data.frame()
colnames(SA)[1] <- "SA"

SANC <- sapply(case_overviews$case_code, codeSANC)
FA <- sapply(case_overviews$case_code, codeFA)
Writ <- sapply(case_overviews$case_code, codeWrit)
judg <- cbind(j, Lien, SA, SANC, FA, Writ)

q <- judg %>% 
  mutate(Judgment_General = ifelse(!grepl("Dismissal", j), 1, 0),
         # Judgment_General = ifelse(is.na(j) | j == "No Judgment", 0, Judgment_General),
         j = ifelse(is.na(j), "MULTIPLE", j),
         Judgment_General = ifelse(j == "MULTIPLE", 0, Judgment_General),
         Judgment_General = ifelse(j == "No Judgment", 0, Judgment_General),
         Judgment_General = ifelse(SANC == 1, 1, Judgment_General),
         Judgment_Dismissal = ifelse(grepl("Dismissal", j), 1, 0)) %>% 
  rename(Judgment = j)

flat_fileV2 <- flat_file %>% left_join(q, by = "case_code")

flat_fileV2 <- flat_fileV2 %>% 
  # Make Status Variables
  mutate(OPEN = ifelse(status == "Open", 1, 0),
         PENDING = ifelse(grepl("Appeal|Arbitration|Reinstated|Stayed|Closed", status) & Judgment_General == 0 & Judgment_Dismissal == 0 & Judgment_Creates_Lien == 0, 1, 0),
         CLOSED = ifelse(status == "Closed" & PENDING == 0, 1, 0)) %>% 
  
  # binary agent variables
  mutate(AGENT = ifelse(is.na(Agent) == FALSE, 1, 0)) %>% 
  rename(Agent_Name = Agent) %>% 
  
  # separate defendant names
  cbind(stringr::str_split_fixed(flat_file$defendant_names, ", |; ", 3)) %>% 
  rename(head_last_name = "1",
         head_first_name = "2",
         other_party = "3"
  )

saveTablesCSV <- function(){
  write.csv(flat_fileV2, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/flat_file.csv", sep = ""))
  write.csv(case_overviews, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/case_overviews.csv", sep = ""))
  write.csv(case_parties, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/case_parties.csv", sep = ""))
  write.csv(events, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/events.csv", sep = ""))
  write.csv(judgments, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/judgments.csv", sep = ""))
  write.csv(lawyers, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/lawyers.csv", sep = ""))
  write.csv(files, paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, "/files.csv", sep = ""))
}

dir.create(paste("G:/Shared drives/ojdevictions/ScrapeData/full_scrape_", today, sep =""))
saveTablesCSV()


# update county summaries

county_summs2020 <- read.csv("county_summs2020.csv")
county_summs2020 <- county_summs2020 %>% select(-1)

flat_fileV2 %>%
  filter(date >= "2022-01-01") %>% 
  group_by(location) %>% 
  summarize(Filings = n(), 
            Dismissals = sum(na.omit(Judgment_Dismissal) == 1), 
            `Judgments to Evict` = sum(na.omit(Judgment_General) == 1),
            `Open Cases` = sum(OPEN == 1),
            `Landlord has Lawyer` = sum(na.omit(landlord_has_lawyer) == 1),
            `Tenant has Lawyer` = sum(na.omit(tenant_has_lawyer) == 1)) %>% 
  add_row(location = "Oregon",
          Filings = sum(.$Filings), 
          Dismissals = sum(.$Dismissals),
          `Judgments to Evict` = sum(.$`Judgments to Evict`),
          `Open Cases` = sum(.$`Open Cases`),
          `Landlord has Lawyer` = sum(.$`Landlord has Lawyer`),
          `Tenant has Lawyer` = sum(.$`Tenant has Lawyer`)) %>% 
  left_join(county_summs2020, by = c("location"="NAM")) %>% 
  select(-GEOID) %>% 
  rename(`Poverty Rate` = AllPov,
         `Percent POC` = POC,
         `Number of Renter Units` = RentrUnts,
         `Percent Renter Units` = RentrUntsP) %>% 
  mutate(`Filing Rate per 100 Rental Units` = Filings*100 / `Number of Renter Units`) %>% 
  select(-NHWhite) %>%  
  write.csv(paste("G:/Shared drives/ojdevictions/ScrapeData/CountySummaries/CountySummary", today, ".csv", sep = ""))


# Weekly Judgments Multnomah County

judgments %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  left_join(case_overviews %>% select(case_code, location)) %>% 
  data.frame() %>% 
  filter(date >= "2022-01-01") %>%
  filter(location == "Multnomah") %>% 
  group_by(location, case_type, week = lubridate::floor_date(date, "week")) %>% 
  summarise(n=n()) %>% 
  tidyr::spread(case_type, n) %>% 
  write.csv(paste("G:/Shared drives/ojdevictions/ScrapeData/Weekly_Judgments/WeeklyJudgmentsMultnomahCo_2022_", today, ".csv", sep = ""))
