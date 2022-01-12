#UNFINISHED: Exporting/return results
#Purpose: Given a scraped publication, this script will scrape through Google Scholar publications that cite the original article.
# Definitions -------------------------------------------------------------

query_citation_record <- function(start_num,cid) {
  #Why num=19? Maximum query size that returns & doesn't get rate-limited
  url_skeleton <- 'https://scholar.google.com/scholar?start={start_num}&num=19&cites={cid}'
  url <- glue(url_skeleton)
  print(paste('url is',url))
  
  try(response <<- httr::GET(url), silent=TRUE)

  while(!exists('response')){
    log_print("Lost network connection. Retrying in 60 seconds. Click STOP to pause and resume later.")
    Sys.sleep(60)
    try(response <<- httr::GET(url), silent=TRUE)
  }

  if(response$status_code==200) {
    page <- response %>% read_html()
  } else {
    status <- response$status_code
    msg <- "{status} Response for pub id: {pub_master_id} **** GSID: {gsid}"
    print(glue(msg))
    # log_print(glue(msg))
    # error_log <- data.frame(
    #   scholar_id,gsid,error=glue(msg),status,url,timestamp = Sys.time(),
    #   source="load_scholar_page()"
    # )
    # error_log_filename <- file.path(log_tmp_dir,'scholars_with_errors_log.csv')
    # write.table(error_log, error_log_filename,sep=",",row.names=F, append=T)
    return(NULL)
  }
}

charZeroToNA <- function(data) {
  if (identical(data, character(0))) {
    return(NA)
  } else
    return(data)
}

parse_record <- function(citation_record){
  record_title <- citation_record %>% html_node('.gs_rt') %>% html_text()
  record_type <- str_extract(record_title, '(?<=\\[)\\w+')
  record_title <- str_trim(str_remove(record_title, '\\[.+\\]'), side='both')
  record_byline <- citation_record %>% html_node('.gs_a')
  record_byline_plaintext <- record_byline %>% html_text()
  record_short_desc <- citation_record %>% html_node('.gs_rs') %>% html_text()
  record_cited_by <- (citation_record %>% html_node('.gs_fl') %>% html_nodes('a'))[grepl('cites=',(citation_record %>% html_node('.gs_fl') %>% html_nodes('a') %>% html_attr('href')))] %>% html_text() %>% str_extract('\\d+')
  record_cid <- (citation_record %>% html_node('.gs_fl') %>% html_nodes('a'))[grepl('cites=',(citation_record %>% html_node('.gs_fl') %>% html_nodes('a') %>% html_attr('href')))] %>% html_attr('href') %>% str_extract('\\d+')
  record_publication_year <- str_extract(record_byline_plaintext, '(19|20)\\d{2}')
  
  
  {
    record_hrefs <- record_byline %>%  html_nodes('a') %>% html_attr('href')
    gsid <- str_extract(record_hrefs, '(?<=user=).{12}')
    name <- record_byline %>%  html_nodes('a') %>% html_text()
    gsid.df <- data.frame(gsid,name)
    if(nrow(gsid.df > 0))  {gsid.tibble <- tibble(authors = list(gsid.df)) } else gsid.tibble <- tibble(authors = NA) 
    }

  record.data = data.frame(
    record_type = charZeroToNA(record_type),
    record_title = charZeroToNA(record_title),
    record_publication_year = charZeroToNA(record_publication_year),
    record_byline_plaintext = charZeroToNA(record_byline_plaintext),
    record_short_desc = charZeroToNA(record_short_desc),
    record_cited_by = charZeroToNA(record_cited_by),
    record_cid = charZeroToNA(record_cid),
    gsid.tibble = charZeroToNA(gsid.tibble)
  )
  return(record.data)
}

# Get from Mongo ----------------------------------------------------------
# 
# pubs <- publicationsDB$find(fields = '{"_id": true, "scholar_id" : true, "gsid" : true, "pubid" : true, "cid" : true}') %>% filter(!is.na(cid))
# 
# 
# # Loop through all records? -----------------------------------------------
# # This will take forEVER...
# 
# # Query one pub record in GS Search ----------------------------------------------------
# 
# pub_to_query <- pubs[1200,]
# {
# start_num = 0
# is_last_page = FALSE
# cid=pub_to_query$cid
# gsid=pub_to_query$gsid
# pub_master_id <- pub_to_query$`_id`
# query.data <- data.frame()
# }
# 
# # Extract records via paginated results -----------------------------------
# 
# while (is_last_page == FALSE) {
#   print(paste('Querying records:', start_num, 'to', start_num+19))
#   result_page <- query_citation_record(start_num, cid)
#   get_records <- result_page %>% html_nodes('.gs_ri')
#   
#   
#   for (record in get_records) {
#     record.data <- parse_record(record)
#     query.data <- rbind(query.data, record.data)
#   }
#   
#   is_last_page <-
#     is.na((result_page %>% html_node(".gs_ico_nav_next")))
#   start_num <<- start_num + 19
#   Sys.sleep(sample(3:9, 1))
# }


# Export data -------------------------------------------------------------

#Push citation record as..update to pub? (Array style?) or as new collection type, using "queried _id" to link back?










# Trash section -----------------------------------------------------------


# (First approach ... should be okay to just parse a whole page at a time into lists.. But I think it may be more reliable to parse one record at a time in order to avoid unexpected field mismatches or length errors)
#  record_titles <- get_records %>% html_node('.gs_rt') %>% html_text()
#  record_byline <- get_records %>% html_node('.gs_a')
#  record_byline_plaintext <- record_byline %>% html_text()
#  record_short_desc <- get_records %>% html_node('.gs_rs') %>% html_text()
#  record_cited_by <- (get_records %>% html_node('.gs_fl') %>% html_nodes('a'))[grepl('cites=',(get_records %>% html_node('.gs_fl') %>% html_nodes('a') %>% html_attr('href')))] %>% html_text() %>% str_extract('\\d+')
#  record_cid <- (get_records %>% html_node('.gs_fl') %>% html_nodes('a'))[grepl('cites=',(get_records %>% html_node('.gs_fl') %>% html_nodes('a') %>% html_attr('href')))] %>% html_attr('href') %>% str_extract('\\d+')
#  record_publication_year <- str_extract(record_byline_plaintext, '(19|20)\\d{2}')
# 
#  extracted_gsid <- tibble()
#  for(record in record_byline) {
#    record_hrefs <- record %>%  html_nodes('a') %>% html_attr('href')
#    gsid <- str_extract(record_hrefs, '(?<=user=).{12}')
#    name <- record %>%  html_nodes('a') %>% html_text()
#    gsid.df <- data.frame(gsid,name)
#    if(nrow(gsid.df > 0))  {gsid.tibble <- tibble(authors = list(gsid.df)) } else gsid.tibble <- NA
#    extracted_gsid <<- rbind(extracted_gsid,gsid.tibble)
#  }
# 
# # --- Create result dataframe ---
#  page.data = data.frame(
#    charZeroToNA(record_titles),
#    charZeroToNA(record_publication_year),
#    charZeroToNA(record_byline_plaintext),
#    charZeroToNA(record_short_desc),
#    charZeroToNA(record_cited_by),
#    charZeroToNA(record_cid),
#    charZeroToNA(extracted_gsid)
#  )
