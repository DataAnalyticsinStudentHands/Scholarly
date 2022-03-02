library(tidyverse)
library(magrittr)
library(mongolite)
library(rvest)
library(logr) 
library(glue)
library(stringr)
library(jsonlite)
`%!in%` <- Negate(`%in%`)



#Source custom functions
scripts <- list.files('R', full.names = T)
for(script in scripts){
  source(script)
  print(glue('Loaded: {script}'))
}
# source("R/get_coauthors.R")
# source("R/get_publication_details.R")
# source("R/scholar_scrape.R")
# source("R/get_profile_and_pubs.R")
# source("R/mongodb_utilities.R")

