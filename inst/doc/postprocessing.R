## -----------------------------------------------------------------------------
#| warning: false
library(EDCimport)
db1 = edc_example()
load_database(db1)
enrol$subjid %>% class()
enrol$subjid %>% head()

db2 = edc_example() %>% 
  edc_unify_subjid(preprocess=~paste0("#", .x))
load_database(db2)
enrol$subjid %>% class()
enrol$subjid %>% head()
#missing patients in table `ae`
ae$subjid %>% 
  forcats::fct_count() %>% 
  dplyr::filter(n==0) 


## -----------------------------------------------------------------------------
#| warning: false
library(EDCimport)
db = edc_example() %>% 
  edc_clean_names(toupper)
load_database(db)
names(enrol)


## -----------------------------------------------------------------------------
head(long_mixed)


## -----------------------------------------------------------------------------
#| warning: false
db = edc_example() %>% 
  edc_split_mixed(long_mixed)
load_database(db)
head(long_mixed_short) #one row per subject
head(long_mixed_long)  #one row per observation

