set_parameter <- function(varname,default) {
  env.var = Sys.getenv(varname)
  if(env.var != '' & !missing(default)) {
    return(env.var)
  } else if(!missing(default)) {
    return(default)
  } else {
    return()
  }
}
connect_mongo <- function(collection, suffix, url=hosturl) {
  if(!exists("hosturl")){
    url="mongodb://localhost/scholarly"
  }
  host <- str_extract(url, '(?<=(mongodb://))\\w+')
  
  if(missing(suffix)){
    target_collection <- collection
  } else {
    target_collection <- paste(collection, suffix, sep = "_")
  }
  connection <-  mongo(collection=target_collection,url=url)
  print(glue("Connected: {host}://{target_collection}"))
  return(connection)
}


checkGSID <- function(gsid) {
  qgsidQuery <-  paste0('{"queried_gsid": "',gsid,'"}')
  newGSID <-  google_profilesDB$find(query=qgsidQuery, fields = '{"_id" : false, "gsid" : true}')
  if(nrow(newGSID)>0) {
    return(pull(newGSID))
  } else {
    return(gsid)
  }
}

isScrapeComplete <- function(gsid) {
  gsidQuery = paste0('{"gsid": "', gsid, '"}')
  pubQuery = paste0('{"gsid": "', gsid, '", "removed_from_profile" : { "$ne" : true } }')
  n_expected <-
    google_profilesDB$find(query = gsidQuery, fields = '{"_id" : false, "n_publications" : true}', limit=1)
  if (nrow(n_expected) == 0) {
    return(NA)
  } else if(n_expected==0){
    return(1)
  } else {
    n_scraped <- publicationsDB$count(query = pubQuery)
    completeness = round(n_scraped / n_expected %>% pull(),2)
    # status <- case_when(n_expected == n_scraped ~ 'complete',
    #                     n_expected > n_scraped ~ 'incomplete',
    #                     n_expected < n_scraped ~ 'overage')
    return(completeness)
  }
}
updateDocValue <-
  function(collection, id, field, value, idType = 'string') {
    if (idType == 'string') {
      mongoID <- paste0('{"_id": "', id, '" }')
    } else if (idType == 'oid') {
      mongoID <- paste0('{"_id": { "$oid" : "', id, '" } }')
    }
    core <- paste0(field, '":', toJSON(value, auto_unbox = T))
    if(is.na(value) || is.null(value) || value == 'unset'){
      payload <- paste0('{"$unset":{"', field, '":1}}')
    } else {
      payload <- paste0('{"$set":{"', core, '}}')
    }
    collection$update(mongoID, payload)
  }

updateStatus <- function(scholar_ids) {
  for (n in 1:nrow(scholar_ids)) {
    scholar = scholar_ids[n,]
    gsid = checkGSID(scholar$gsid)
    id = scholar$`_id`
    
    completeness <- as.numeric(isScrapeComplete(gsid))
    print(paste(gsid, completeness, sep=' : '))
    if(is.na(completeness)) next
    updateDocValue(scholarDB, id, 'completeness', completeness)
  }
}


mongo_insert <- function(data, db) {
  tryCatch({
    db$insert(data)
    # print('inserting data! :)')
  }, error = function(error) {
    if (grepl('shark', error)) {
      print("Connection to database lost. Retrying in 3 minutes.")
      Sys.sleep(180)
      n=1
      
      while(n <= 3){
        tryCatch({
          db$insert(data)
          print("Connection restored.")
          break
        }, error= function(error){
          n=n+1
          print(glue("Retry number {n} / 3 failed. Retrying in 3 minutes."))
          Sys.sleep(180)
        })
      }
      print("Please reconnect to database and restart the scrape.")
    } else if(grepl('duplicate', error)){
      print("Error: Duplicate entry found. Cannot insert.")
    } else {
      print("Unknown error when inserting document to database.")
    }
  })
}


update_n_publications_variance <- function(){
  #For an unknown reason, 
  query <- '{ "$or" : [ { "$and" : [ { "completeness" : { "$lt" : 1.0 } }, { "completeness" : { "$gt" : 0.9 } }, { "manual_verification" : { "$exists" : false } } ] }, { "$and" : [ { "completeness" : { "$gt" : 1.0 } }, { "manual_verification" : { "$exists" : false } } ] } ] }'
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
      pubs_in_mongo <- publicationsDB$find(query = gsidcriteria,
                                           fields = '{"_id" : true, "gsid" : true, "pubid" : true}')
      pubs_to_query <- profile_pubs_list %>% anti_join(pubs_in_mongo, by = "pubid")
      pubs_removed_from_profile <- pubs_in_mongo %>% anti_join(profile_pubs_list, by='pubid') %>% pull(`_id`)
      addRemovalTag(pubs_removed_from_profile)
      
      print(
        sprintf(
          'Query %d of %d : Updated scholar %s. Old value: %d. New value: %d. Number of pubs actually scraped: %d. Something is fishy here..Updating with tag to manually verify.',
          n,
          n_docs,
          scholar_id,
          old_value,
          new_npubs,
          pubs_scraped
        )
      )
      updateDocValue(scholarDB, scholar_id, "manual_verification", TRUE)
    } else {
      
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
    }
    Sys.sleep(sample(4:18,1))
  }
  updateStatus(scholar_ids)
}

addRemovalTag <- function(removal_list){
  if(length(removal_list) > 0){
    for(id in removal_list){
    updateDocValue(publicationsDB, id, "removed_from_profile", TRUE, idType='oid')
      log_print(
        glue("Publication has been removed from profile: {id}")
      )
    }
  }
  else{
    return()
  }
}
