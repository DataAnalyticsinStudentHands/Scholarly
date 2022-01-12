get_scholar_profile <-  function (page, source_gsid, scholar_id, fetch_coauthors = TRUE) 
{
  # url <- compose_scrape_url(source_gsid)
  # page <- get_scholar_resp(url) %>% read_html()
  # page <- httr::GET(url) %>% read_html()
  tables <- page %>% html_table()
  if(length(tables) == 0) {updateDocValue(scholarDB, scholar_id, '404_response', TRUE); return(NULL)}
  stats <- tables[[1]]
  rows <- nrow(stats)
  gsid <- page %>% 
    html_nodes(xpath = "//*[@id='gsc_md_cbym_l']") %>%
    html_attr("data-act") %>%
    gsub('.+user=|&.+','',.)
  name <- page %>% html_nodes(xpath = "//*/div[@id='gsc_prf_in']") %>% 
    html_text()
  bio_info <- page %>% html_nodes(xpath = "//*/div[@class='gsc_prf_il']") %>% 
    html_text()
  affiliation <- bio_info[1]
  specs <- iconv(bio_info[2], from = "UTF8", to = "ASCII")
  specs <- str_trim(tolower(str_split(specs, ",")[[1]]))
  
  raw_fields <- page %>% html_nodes(".gsc_prf_inta") %>% html_text() 
  fields <- if(identical(raw_fields,character(0))) NA_character_ else matrix(raw_fields, nrow=1)
  
  homepage <- page %>% html_nodes(xpath = "//*/div[@id='gsc_prf_ivh']//a/@href") %>% html_text()
  homepage <- if(identical(homepage,character(0))) NA_character_ else homepage
  
  if(fetch_coauthors==TRUE){
    Sys.sleep(1)
    coauthor_info <- get_coauthors(gsid)
  } else {
    coauthor_info <- NULL
  }
  
  google_scholar_profile <- tibble(
    gsid = gsid,
    scholar_id = scholar_id,
    last_updated = Sys.time(),
    name = name,
    affiliation = affiliation,
    total_cites = as.numeric(as.character(stats[rows - 2,
                                                2])),
    h_index = as.numeric(as.character(stats[rows - 1, 2])),
    i10_index = as.numeric(as.character(stats[rows, 2])),
    specs = specs,
    fields = I(fields),
    homepage = homepage) %>%
    mutate_if(is.numeric, as.integer) %>% 
    mutate_if(is.character, iconv, "UTF-8", "UTF-8", sub = '')
  
  if(!is.null(coauthor_info)) {
    google_scholar_profile$coauthors <- list(coauthor_info)
  }
  
  # if (is.na(google_scholar_profile$coauthors)) {
  #   google_scholar_profile$coauthors <- NULL
  # }
  
  if (!identical(gsid, source_gsid)) {
    google_scholar_profile %<>% mutate(source_gsid = source_gsid,
                                       gsid_updated = TRUE,
                                       .before = last_updated)
  }
  return(google_scholar_profile)
}()