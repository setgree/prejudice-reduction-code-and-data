library(dplyr)
library(RefManageR)

# subset studies that have DOIs
dat_with_dois  <- readRDS('data/prejudice_meta_data.rds') %>% 
  filter(!is.na(doi)) %>%
  select(title, author, year, doi, url) %>%
  distinct(title, .keep_all = T)


# write bibs into bib file
for(entry in dat_with_dois$doi){
  WriteBib(bib = GetBibEntryWithDOI(doi = entry),
           file = './refs.bib',
           append = T)
}

# add DOIs one at a time
apbib <- function(new_doi){
  WriteBib(bib = GetBibEntryWithDOI(new_doi),
           file = './refs.bib',
           append = T) 
}

apbib('10.1002/ejsp.2130')
apbib('10.1002/ejsp.2515')
# etc. 