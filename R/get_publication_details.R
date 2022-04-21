extract_publication_info <- function(page) {
  #Gets the publication available from main profile page.
  title <- page %>% html_nodes('.gsc_a_at') %>% html_text()
  pubid <- page %>% html_nodes('.gsc_a_at') %>% html_attr('href') %>% str_extract("(?<=:).+$")
  year <- page %>% html_nodes('.gsc_a_y') %>% html_nodes('.gs_ibl') %>% html_text()
  cites <- page %>% html_nodes('.gsc_a_c') %>% html_nodes('.gsc_a_ac') %>% html_text() %>% as.numeric() %>% replace(is.na(.),0)
  pub_page <- data.frame(title,pubid,year,cites)
  return(pub_page)
}


#Following two functions target the publication page directly.
load_publication_page <- function(gsid,pubid) {
  site <- "https://scholar.google.com"
  url_options <- paste0("/citations?view_op=view_citation&hl=en&citation_for_view=")
  url_tail <- paste(gsid, pubid, sep = ":")
  url <- paste0(site, url_options, url_tail)
  
  try(response <<- httr::GET(url, silent=TRUE))
  
  while(!exists('response')){
    log_print("Lost network connection. Retrying in 60 seconds. Click STOP to pause and resume later.")
    Sys.sleep(60)
    try(response <<- httr::GET(url, silent=TRUE))
  }
  
  if(response$status_code==200) {
    page <- response %>% read_html()
    return(page)
  } else if(response$status_code==500) {
      Sys.sleep(20)
      try({
        response <<- httr::GET(url, silent=TRUE)
        page <- response %>% read_html()
        return(page)
      })
  } else {
    status <- response$status_code
    msg <- "{status} Response for pub id {pubid} **** GSID: {gsid}"
    log_print(glue(msg))
    error_log <- data.frame(
      id,gsid,error=glue(msg),status,url,timestamp = Sys.time(),
      source="load_publication_page()"
    )
    error_log_filename <- file.path(log_tmp_dir,'scholars_with_errors_log.csv')
    write.table(error_log, error_log_filename,sep=",",row.names=F, append=T)
    return(NULL)
  }
}

get_publication_details <- function (gsid, pubid) 
{
  tryCatch({
    
    doc <- load_publication_page(gsid, pubid)
    
    #Publication details
    
    all_details <- doc %>% html_nodes(".gs_scl")
    
    if(identical(all_details %>% html_text(), character(0))){
      pub_blank <- data.frame(pubid=pubid,
                              blank = T,
                              last_updated = Sys.time())
      return(pub_blank)
    }
    
    description <- doc %>% html_nodes("#gsc_vcd_descr") %>% html_text() %>%
      gsub(".*Publisher Summary", "", .)
    
    fields <- all_details %>% html_nodes('.gsc_oci_field') %>% html_text()
    values <- all_details %>% html_nodes('.gsc_oci_value') %>% html_text()
    
    #Citation numbers
    #Google changed classifiers...
    # years <- doc %>% html_nodes(xpath = "//*/div[@id='gsc_oci_graph_bars']/a") %>% 
    #   html_attr("href") %>% str_replace(".*as_yhi=(.*)$", 
    #                                     "\\1") %>% as.numeric()
    # vals <- doc %>% html_nodes(xpath = "//*/span[@class='gsc_vcd_g_al']") %>% 
    #   html_text() %>% as.numeric()
    
    years <- doc %>% html_nodes('.gsc_oci_g_a') %>% html_attr('href') %>% str_extract('(?<=(as_yhi=))\\d{4}') %>%  as.integer()
    vals <- doc %>% html_nodes('a.gsc_oci_g_a') %>% html_nodes('.gsc_oci_g_al') %>% html_text() %>% as.integer()
    
    pub_cites <- data.frame(year = as.integer(years), cites = as.integer(vals))
    
    discard_fields <- c("Scholar articles", "Total citations")
    
    #Cluster ID
    clusterID <- doc %>% html_nodes('.gsc_oci_merged_snippet') %>% html_node('a') %>% html_attr('href') %>% str_extract('(?<=cluster=)\\d+')
    clusterID <- unique(clusterID[!is.na(clusterID)])
    clusterID <- paste(clusterID, collapse=',')
    
    tryCatch({title_link <- doc %>% html_node('.gsc_oci_title_link') %>% html_attr('href') %>% replace0}, 
             error=function(e) {title_link <<- NA_character_})
    tryCatch({sidebar_link <- doc %>% html_node('.gsc_oci_title_ggi') %>% html_node('a') %>% html_attr('href') %>% replace0}, 
             error=function(e) {sidebar_link <<- NA_character_})
    tryCatch({sidebar_link_type <- doc %>% html_node('.gsc_vcd_title_ggt')  %>% html_text() %>% gsub('\\W', '',.) %>% replace0}, 
             error=function(e) {sidebar_link_type <<- NA_character_})
    
    
    pub_detail <- data.frame(last_updated = Sys.time(),
                             fields,
                             values,
                             pubid = pubid,
                             cid=clusterID,
                             title_link,
                             sidebar_link,
                             sidebar_link_type
                             ) %>%
      filter(fields %!in% discard_fields) %>%
      pivot_wider(everything(),
                  names_from = fields,
                  values_from = values) %>%
      as_tibble(.)
    
    names(pub_detail) <- str_to_lower(names(pub_detail))
    
    if (nrow(pub_cites) > 0) {
      pub_cites[is.na(pub_cites)] <- 0
      pub_detail$pub_cites = list(pub_cites)
    }
    
    return(pub_detail)
  }, error =function(error) {
    error_log <- data.table(gsid,pubid,url,error_msg = paste(error$message, collapse = ','),error_call = paste(error$call, collapse = ','), source="get_publication_details()",timestamp = Sys.time())
    error_log_filename <- file.path(log_tmp_dir,'scholars_with_errors_log.csv')
    write.table(error_log, error_log_filename,sep=",",row.names=F, append=T)
    query <- paste(gsid,pubid,sep=":")
    log_print(paste('Encountered error with publication query',query, '- saved error log'))
  })
}



