## -----------------------------------------------------------------------------
#| warning: false 
library(EDCimport) 
library(dplyr) 
db = edc_example(N=200) %>%
  edc_unify_subjid() %>%
  edc_clean_names() 
db 
load_database(db)


## -----------------------------------------------------------------------------
data_xx = enrol %>% 
  edc_left_join(data2) %>% 
  edc_right_join(data1) %>% 
  edc_full_join(ae)
dim(data_xx)
names(data_xx) %>% stringr::str_subset("crfname") #suffixes are automated


## -----------------------------------------------------------------------------
set.seed(42)
x = tibble(a=c("Yes", "No"), b=c("Oui", "Non"), c=0:1, d=c(TRUE, FALSE))
x
x %>% dplyr::mutate_all(fct_yesno)

