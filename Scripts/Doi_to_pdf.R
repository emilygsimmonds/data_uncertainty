# SCRIPT TO DOWNLOAD PAPERS FROM DOI #

#### Set up ####

# load packages

library(tidyverse)
library(httr)

# load DOI

DOI <- read.csv("DOIs.csv", header = TRUE)

# add http: at the beginning

DOI <- DOI %>% mutate(DOI = paste0("http://doi.org/", DOI_ISBN))

#### Make DOI a url ####

headers <- lapply(DOI[,4], HEAD)

# open the urls
map(.x = headers, ~{
  browseURL(.x$url, browser = getOption("browser"),
            encodeIfNeeded = FALSE)
})


