#Scraper coordinates the page-level functions
collect_scholar <- function(scholar_id, gsid, log_tmp_dir, fetch_profile=TRUE, fetch_publications=TRUE) {
  if(!exists("google_profilesDB") | !exists("publicationsDB")) {
    stop("Connection to MongoDB not found. Please connect to collection.")
  }
  tryCatch({
    log_print(paste('Fetching scholar gsid:', gsid))
    
    scholar_data <- scholar_query(source_gsid=gsid,scholar_id=scholar_id)
    if(is.null(scholar_data)) return(NULL)
  
    if(fetch_profile){
      #Get google profile page
      
      scholar_profile <- scholar_data$scholar_profile[[1]]
      scholar_profile %<>% rename(`_id`=scholar_id)
      
      #Handling for updated gsid values - Citation URLs do not redirect
        if ("gsid_updated" %in% colnames(scholar_profile)) {
          #update document source_file_gsid to gsid
          updateDocValue(scholarDB, scholar_id, "source_gsid", gsid)
          #Update GSID to new GSID (so as to query the publications...)
          gsid <- scholar_profile$gsid
          log_print(paste("Scholar", scholar_id, "has an updated gsid:", gsid))
          #update gsid to new gsid - scholar_profile$gsid
          updateDocValue(scholarDB, scholar_id, "gsid", gsid)
        }
        #Insert profile to MongoDB
        query = paste0('{"_id" : "', scholar_id, '" }')
        
        exists <- if(nrow(
          google_profilesDB$find(
            query = query,
            fields = '{"_id" : true, "gsid" : true}'
          )) > 0) TRUE else FALSE
        
        if (exists == FALSE) {
          mongo_insert(scholar_profile, google_profilesDB)
          log_print(paste("Inserting google profile:", gsid))
        } else {
          log_print(paste("There's already a googleProfile document for:", gsid))
        }
      }
    if(fetch_publications){
      profile_pubs_list <- scholar_data$pubs[[1]]
      
        query_criteria <- paste0('{"gsid" : "', gsid, '"}')
          pubs_in_mongo <- publicationsDB$find(query = query_criteria,
                                               fields = '{"_id" : true, "gsid" : true, "pubid" : true}')
          if (nrow(pubs_in_mongo) > 0) {
            pubs_to_query <- profile_pubs_list %>% anti_join(pubs_in_mongo, by = "pubid")
            pubs_removed_from_profile <- pubs_in_mongo %>% anti_join(profile_pubs_list, by='pubid') %>% pull(`_id`)
            addRemovalTag(pubs_removed_from_profile)
          } else {
            pubs_to_query <- profile_pubs_list
        }
      # pubs_to_query %<>% sample_n(3) #For testing/debugging purposes only.
      
      pub.n <- 0
      num_pubs <- length(pubs_to_query$pubid)
      for (pubid in pubs_to_query$pubid) {
        pub.n = pub.n + 1
        log_print(
          paste(
            "Querying publication:",
            pubid,
            'for scholar',
            gsid,
            '- Query',
            pub.n,
            '/',
            num_pubs
          )
        )
        
        tryCatch(citations_and_details <- get_publication_details(gsid, pubid),
                 error = function(e) next)
        
        pub_doc <- pubs_to_query %>%
          inner_join(citations_and_details,by = "pubid") %>%
          mutate_if(is.numeric, as.integer) %>%
          mutate_if(is.character, iconv, "UTF-8", "UTF-8", sub = '') %>%
          relocate(contains("ID"), last_updated, .before = 1) %>% 
          relocate(contains('link'), .after = last_col())
        
          # publicationsDB$insert(pub_doc)
          mongo_insert(pub_doc, publicationsDB)
          publication_complete_msg <- "Collected publication: {pubid} ({pub.n} of {length(pubs_to_query$pubid)}). Working on scholar {scholar_id}, GSID: {gsid}."
          print(glue(publication_complete_msg))
        if(pub.n != num_pubs) {
          Sys.sleep(sample(4:18,1))
        }
      }
    } else{
      Sys.sleep(sample(4:18, 1))
    }
  },
  error = function(error) {
    error_log <- data.frame(id, gsid,
        error = paste("Error message:", paste(error, collapse = ';')),
        timestamp = Sys.time(),
        source = "scholar_scrape()"
      )
    error_log_filename <-
      file.path(log_tmp_dir, 'scholars_with_errors_log.csv')
    write.table(
      error_log,
      error_log_filename,
      sep = ",",
      row.names = F,
      append = T
    )
    log_print(paste('Encountered error with', gsid, '- saved error log'))
  })
}
