## -----------------------------------------------------------------------------
#| warning: false
#| message: true
library(EDCimport)
library(dplyr)
db = edc_example(N=200) %>% 
  edc_unify_subjid() %>% 
  edc_clean_names()
db
load_database(db)


## -----------------------------------------------------------------------------
enrol %>% 
  filter(age<25) %>% 
  edc_data_warn("Patients should be >25yo", issue_n=1)

ae %>% 
  filter(aegr<1 | aegr>5) %>% 
  edc_data_warn("Incorrect adverse event grade", issue_n=2)

data1 %>% 
  edc_left_join(enrol) %>% 
  filter(arm=="Trt") %>% 
  filter(date1<"2010-04-10") %>% 
  edc_data_warn("Treated patients should have been seen later", issue_n=3)


## -----------------------------------------------------------------------------
edc_data_warnings()


## -----------------------------------------------------------------------------
#| error: true
try({
enrol %>% 
  assert_no_duplicate() %>% 
  count(arm)

enrol %>% 
  edc_left_join(data1) %>% #oopsie
  assert_no_duplicate() %>% 
  count(arm)
})


## -----------------------------------------------------------------------------
lastnews_table(prefer="date10", except="data1", show_delta=TRUE) %>% 
  mutate(delta=round(delta)) %>% 
  arrange(desc(delta))

