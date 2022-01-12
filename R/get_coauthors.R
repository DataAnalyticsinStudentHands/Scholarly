get_coauthors <- function(gsid) {

  url <- paste0("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=", gsid)
  try(response <<- httr::GET(url), silent=TRUE)
  
  while(!exists('response')){
    log_print("Lost network connection. Retrying in 60 seconds. Click STOP to pause and resume later.")
    Sys.sleep(6)
    try(response <<- httr::GET(url), silent=TRUE)
  }
  
  if(response$status_code==200) {
    coauthor_page <- read_html(response, encoding = "ISO-8859-1")
  } else {
    status <- response$status_code
    msg <- "{status} Response for colleagues; scholar id: {scholar_id} **** GSID: {gsid}"
    log_print(glue(msg))
    error_log <- data.frame(
      id,gsid,error=glue(msg),status,url,timestamp = Sys.time(),
      source="get_coauthors()"
    )
    error_log_filename <- file.path(log_tmp_dir,'scholars_with_errors_log.csv')
    write.table(error_log, error_log_filename,sep=",",row.names=F, append=T)
    return(NULL)
  }
  
  
  coauth_gsid <- coauthor_page %>%
    html_nodes(xpath = '//*[@class="gs_ai_name"]') %>%
    html_nodes('a') %>%
    html_attr("href")
  
  if(!identical(coauth_gsid,character(0))) coauth_gsid <- str_split(coauth_gsid, '=', simplify = TRUE)[,3]

  coauth_name <- coauthor_page %>%
    html_nodes(xpath = '//*[@class="gs_ai_name"]') %>%
    html_nodes('a') %>%
    html_text()
  
  coauth_institution <- coauthor_page %>%
    html_nodes(xpath = '//*[@class="gs_ai_aff"]') %>%
    html_text()

  coauth_df = data.frame(coauth_gsid, coauth_name, coauth_institution)
  coauth_df[coauth_df==""] <- NA

  if (dim(coauth_df)[1] == 0) {
    return(NULL)
  } else {
    return(coauth_df)
  }
}
