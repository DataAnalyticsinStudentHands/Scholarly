#Load packages and scripts
source("scrape_requirements.R")

scrape_from_file = FALSE
enableFarmerFilter = TRUE
farmer_id = 'fallback'

# collection_suffix <- "ERL" #Option to set manually if not on a farm machine.
# Authentication and host URL
#Specify url in environment
#Sys.setenv(MONGOURL = "mongodb://localhost:27017/scholarball")
hosturl = Sys.getenv("MONGOURL")
project_dir = Sys.getenv("PROJECT_DIR")
log_tmp_dir = file.path(project_dir,'log')
collection_suffix = Sys.getenv("COLLECTION_SUFFIX")
farmer_id = Sys.getenv("FARMER_ID")

#Define mongo connections

{
  scholarDB <- connect_mongo(collection="scholars", suffix=collection_suffix)
  publicationsDB <- connect_mongo(collection="publications",suffix='consolidated')
  google_profilesDB <- connect_mongo(collection="googleProfiles",suffix='consolidated')
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
  while(T==T){
    scholar_ids <- scholarDB$find(query = '{ "$nor" : [ { "completeness" : { "$gte" : 1.0 } } ], "farmer" : { "$exists" : true } }',
                                  fields = '{"_id" : true, "gsid" : true}',
                                  limit = 500)
    
    updateStatus(scholar_ids)
    
    query = sprintf('{ "$or" : [ { "farmer" : "%s" }, { "farmer" : null } ], "$nor" : [ { "completeness" : { "$gte" : 1.0 } }, { "dup_gsid_flag" : true }, { "404_response" : { "$exists" : true } } ] }', farmer_id)
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
    
    collect_scholar(id, gsid, log_tmp_dir=log_tmp_dir)
    
    log_print(paste('Completed scholar gsid:', gsid, '- Query', scholar.n, '/', n_scholars))
    Sys.sleep(sample(4:14, 1))
  log_print('Completed loop. Initiating next round.')
}
}
log_print('Stopping scraper.')
log_close()
