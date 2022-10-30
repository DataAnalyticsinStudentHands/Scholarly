scholar_query <- function(source_gsid, scholar_id, fetch_coauthors=TRUE) {
  cstart = 0
  pubs <- data.frame()
  page <- load_scholar_page(gsid=source_gsid, scholar_id=scholar_id, cstart=cstart)
  
  if(is.null(page)) return(NULL)
  pagination_button <- page %>% html_nodes('#gsc_bpf_more') %>% html_attr('disabled') %>% replace(is.na(.), 'enabled')
  
  scholar_profile <- get_scholar_profile(page, source_gsid=source_gsid, scholar_id=scholar_id, fetch_coauthors=fetch_coauthors)
  if(is.null(scholar_profile)) return(NULL)
  
  gsid <- scholar_profile$gsid
  
  pub_page <- extract_publication_info(page)
  pubs <- rbind(pubs, pub_page)
  
  #Iterate over paginated publications
  if (pagination_button == 'enabled') {
    continue <- TRUE
    while (continue == TRUE) {
      Sys.sleep(sample.int(2, 1))
      # cstart = cstart + 100
      cstart = nrow(pubs)
      print(glue("Scanned {cstart+20} publications"))
      page <- load_scholar_page(gsid = gsid, cstart = cstart)
      pagination_button <- page %>% html_nodes('#gsc_bpf_more') %>% html_attr('disabled') %>% replace(is.na(.), 'enabled')
      pub_page <- extract_publication_info(page)
      pubs <- rbind(pubs, pub_page)
      if (pagination_button  == 'enabled'){
        continue = TRUE
        if(cstart%%1000==0|cstart>1500){
          #For scholars with a very high number of pubs - we start getting kicked out after cstart ~1500-2000. Slowing down helps prevent this.
          Sys.sleep(sample(4:18,1))
        }
      } else {
        continue = FALSE
      }
    }
  }
  
  pubs <- pubs %>% 
    # mutate(across(.cols=everything()), na_if(., "")) %>%
    mutate(
      gsid = gsid
    ) %>%
    mutate_if(is.numeric, as.integer) %>% 
    distinct
  
  scholar_profile$n_publications <- as.integer(nrow(pubs))
  
  scholar_query_output <- tibble(scholar_profile=list(scholar_profile), pubs=list(pubs))
  return(scholar_query_output)
}
