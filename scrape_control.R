#Load packages and scripts
source("scrape_requirements.R")

# Authentication and host URL
#Specify url in environment
#Sys.setenv(MONGOURL = "mongodb://localhost:27017/scholarball")
hosturl = set_parameter("MONGOURL")
project_dir = set_parameter("PROJECT_DIR", getwd())
log_tmp_dir = file.path(project_dir,'log')
collection_suffix = set_parameter("COLLECTION_SUFFIX", NULL)
# collection_suffix <- "ERL" #Option to set manually if not on a farm machine.
farmer_id = set_parameter("FARMER_ID", Sys.info()["nodename"][[1]])

#Note - This could cause someone problems if they are not using multiple computers simultaneously, but mid-stream switch between computers... Not sure how it should default?
enableFarmerFilter = TRUE

#Define mongo connections

{
  scholarDB <- connect_mongo(collection="scholars", suffix=collection_suffix)
  publicationsDB <- connect_mongo(collection="googlePublications")
  google_profilesDB <- connect_mongo(collection="googleProfiles")
}

date <- as.character(format.Date(Sys.Date(), "%m-%d-%Y"))
dir.create(project_dir, showWarnings = FALSE)
dir.create(log_tmp_dir, showWarnings = FALSE)
logname <- paste(date,"scraper.log", sep="_")
log <- file.path(log_tmp_dir,logname)
log_open(log, logdir=F)

#Option to scrape from a csv file or work against a mongo collection
  #Get list of scholar ID from MongoDB to query
  #A query filter can be set here to exclude scholars from scraping.

#These are to track loops and trigger remedies if stuck.
loop_number <- 1
n_repeat_identical_query <- 0

  while(T==T){
    scholar_ids <- scholarDB$find(query = '{ "$nor" : [ { "completeness" : { "$gte" : 1.0 } } ], "farmer" : { "$exists" : true }, "gsid" : { "$exists" : true } }',
                                  fields = '{"_id" : true, "gsid" : true}',
                                  limit = 500)
    
    updateStatus(scholar_ids)
    
    if (enableFarmerFilter == TRUE) {
    query = sprintf('{ "$or" : [ { "farmer" : "%s" }, { "farmer" : null } ], "$nor" : [ { "completeness" : { "$gte" : 1.0 } }, { "dup_gsid_flag" : true }, { "404_response" : { "$exists" : true } } ], "gsid" : { "$exists" : true } }', farmer_id)
    } else {
      query = '{"$nor" : [ { "completeness" : { "$gte" : 1.0 } }, { "dup_gsid_flag" : true }, { "404_response" : { "$exists" : true } } ], "gsid" : { "$exists" : true } }'
    }
    scholar_ids <- scholarDB$find(query = query,
                                  fields = '{"_id" : true, "gsid" : true, "farmer" : true}',
                                  limit = 10)
    
    if(nrow(scholar_ids) == 0) break
    
    if (enableFarmerFilter == TRUE) {
      claim_ids <- scholar_ids %>% pull(`_id`)
      if (!identical(claim_ids, character(0))) {
        for (id in claim_ids) {
          updateDocValue(scholarDB, id, "farmer", farmer_id)
        }
      }
    }
  scholar_ids %<>% rename(id = `_id`) %>% distinct(gsid, .keep_all = T) %>% select(id, gsid)

# Initiate scrape ---------------------------------------------------------
#Can also pass along commands of whether to scrape profiles, publications, or both
#Defaults: fetch_publications=TRUE, fetch_profile=TRUE
  n_scholars <<- nrow(scholar_ids)
  for(scholar.n in 1:n_scholars){
    scholar_id <- scholar_ids$id[scholar.n]
    gsid <- scholar_ids$gsid[scholar.n]
    
    collect_scholar(scholar_id, gsid, log_tmp_dir=log_tmp_dir)
    
    log_print(paste('Completed scholar gsid:', gsid, '- Query', scholar.n, '/', n_scholars))
    Sys.sleep(sample(4:14, 1))
  }
  
  #Sometimes there would be an issue where it gets stuck on a set of scholars that it can't finish due to various errors.
  #So this tries to remedy with the recheck pub script, else break it off (user needs to investigate)
  if (exists("previous_round_ids")) {
    if (identical(scholar_ids, previous_round_ids)) {
        log_print("Detected identical query loop. Rechecking publication numbers.")
      if (n_repeat_identical_query == 3) {
        log_print("Scraper got stuck. Quitting due to repeated query of same scholars. Please check any issue manually and restart if needed.")
        break
      }
      n_repeat <- n_repeat_identical_query + 1
      source("recheck_publication_numbers.R")
    }
  }
  
  #This is to try and head off any issues. Google seems to be making updates to scholar profiles at the same time we are scraping them, so sometimes it throws the numbers off. Rechecking them updates profile with new total number of publications that we're targeting.
  loop_number <- loop_number + 1
  if (loop_number %% 6 == 0) {
    log_print('Rechecking profile publication numbers.')
    source("recheck_publication_numbers.R")
  }
  
  previous_round_ids <- scholar_ids
  log_print('Completed loop. Initiating next round.')
  
  }
log_print('Stopping scraper. There are no more scholars to scrape.')
log_close()
