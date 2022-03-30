#For an unknown reason, sometimes there is different values between the google profile n_pubs and the actual number of publications we get. Usually re-checking the profile resolves this problem, followed by an update for the new correct n_pubs

scholarDB <- connect_mongo('scholars_ERL')
google_profilesDB <- connect_mongo('googleProfiles')
publicationsDB <- connect_mongo('googlePublications')
# source('scrape_requirements.R')



  # query <- '{ "$or" : [ { "$and" : [ { "completeness" : { "$lt" : 1.0 } }, { "completeness" : { "$gt" : 0.9 } }, { "manual_verification" : { "$exists" : false } } ] }, { "$and" : [ { "completeness" : { "$gt" : 1.0 } }, { "manual_verification" : { "$exists" : false } } ] } ] }'
  query <- '{ "$or" : [ { "$and" : [ { "completeness" : { "$lt" : 1.0 } }, { "completeness" : { "$gt" : 0.9 } }, { "manual_verification" : { "$exists" : false } } ] }, { "$and" : [ { "completeness" : { "$gt" : 1.0 } } ] } ] }'
  scholar_ids <- scholarDB$find(query = query,
                                fields = '{"_id" : true, "gsid" : true}') 
  n_docs <- nrow(scholar_ids)
  print(sprintf('Found %d documents with completeness variance matching criteria.', n_docs))
  
  for(n in 1:n_docs) {
    scholar_to_query <- scholar_ids[n, ]
    scholar_id <- as.character(scholar_to_query$`_id`)
    gsid <- scholar_to_query$gsid
    gsidquery <- sprintf('{"gsid" : "%s"}', gsid)
    pubQuery = paste0('{"gsid": "', gsid, '", "removed_from_profile" : { "$ne" : true } }')
    scholar_data <-
      scholar_query(source_gsid = gsid, scholar_id = scholar_id)
    
    new_npubs <- scholar_data$scholar_profile[[1]]$n_publications
    print(new_npubs)
    
    old_value <- google_profilesDB$find(query = gsidquery,
                                        fields = '{"_id" : false, "n_publications" : true}',
                                        limit = 1) %>% pull
    
    pubs_scraped <- publicationsDB$count(query = pubQuery)
    if(new_npubs!=old_value){
      updateDocValue(google_profilesDB, gsid, "n_publications", new_npubs)
    }
    if(new_npubs<pubs_scraped){
      profile_pubs_list <- scholar_data$pubs[[1]]
      pubs_in_mongo <- publicationsDB$find(query = gsidquery,
                                           fields = '{"_id" : true, "gsid" : true, "pubid" : true}')
      pubs_to_query <- profile_pubs_list %>% anti_join(pubs_in_mongo, by = "pubid")
      pubs_removed_from_profile <- pubs_in_mongo %>% anti_join(profile_pubs_list, by='pubid') %>% pull(`_id`)
      addRemovalTag(pubs_removed_from_profile)

    } 
      print(
        sprintf(
          'Query %d of %d : Updated scholar %s. Old value: %d. New value: %d. Number of pubs actually scraped: %d',
          n,
          n_docs,
          scholar_id,
          old_value,
          new_npubs,
          pubs_scraped
        )
      )
      
    Sys.sleep(sample(4:18,1))
  }
  updateStatus(scholar_ids)
  