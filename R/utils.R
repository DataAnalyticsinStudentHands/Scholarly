compose_scrape_url <- function(gsid, cstart=0) {
  stem <- "https://scholar.google.com/citations?user="
  request_settings <- paste0("&cstart=",cstart,"&pagesize=20&sortby=pubdate")
  url <- paste0(stem, gsid,request_settings)
  return(url)
}

replace0 <- function(x) {
  if(identical(x,character(0))) NA_character_ else x
}
