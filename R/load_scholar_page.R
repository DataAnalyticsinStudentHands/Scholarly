load_scholar_page <- function(gsid, cstart, scholar_id) {
  url <- compose_scrape_url(gsid, cstart)
  # page <- get_scholar_resp(url) %>`% read_html()
  # page <- read_html(url)
  try(response <<- httr::GET(url), silent=TRUE)
  
  captcha <- grepl('sorry', response$url)
  if(captcha==TRUE){
    log_print('Blocked by CAPTCHA')
    log_print(glue('Will wait and try again in 4 minutes. Can verify manually at {response$url}'))
    Sys.sleep(360)
    try(response <<- httr::GET(url), silent=TRUE)
    if(grepl('sorry', response$url)){
      log_print('Still blocked by CAPTCHA.')
      log_print(glue('Will wait a while and then move on to the next scholar. Can verify manually at {response$url}'))
      Sys.sleep(400)
      return(NULL)
    }
  }
  
  #If the GSID changed, need to construct URL and request again. The redirect removes the request parameters (cstart and sorting) which causes duplications and incompletes in getting the list of publications.
  
  ##Paste here something to handle 'sorry' url -
  try(response_gsid <- str_extract(response$url, '(?<=(user=)).{12}'))
  if(gsid != response_gsid){
    Sys.sleep(1)
    url <- compose_scrape_url(response_gsid, cstart)
    try(response <<- httr::GET(url), silent=TRUE)
  }
  
  while(!exists('response')){
    log_print("Lost network connection. Retrying in 60 seconds. Click STOP to pause and resume later.")
    Sys.sleep(60)
    try(response <<- httr::GET(url), silent=TRUE)
  }
  
  if(response$status_code==200) {
    page <- response %>% read_html()
  } else {
    status <- response$status_code
    if(status==404) updateDocValue(scholarDB, scholar_id, '404_response', TRUE)
    msg <- "{status} Response for scholar id: {scholar_id} **** GSID: {gsid}"
    log_print(glue(msg))
    error_log <- data.frame(
      scholar_id,gsid,error=glue(msg),status,url,timestamp = Sys.time(),
      source="load_scholar_page()"
    )
    error_log_filename <- file.path(log_tmp_dir,'scholars_with_errors_log.csv')
    write.table(error_log, error_log_filename,sep=",",row.names=F, append=T)
    return(NULL)
  }
}