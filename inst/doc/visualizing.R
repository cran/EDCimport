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
edc_swimmerplot(origin="enrol$enrol_date")


## -----------------------------------------------------------------------------
edc_crf_plot()


## -----------------------------------------------------------------------------
edc_patient_gridplot()


## -----------------------------------------------------------------------------
# Total population: all screened patients
pop_total <- c(1:100) %>% setdiff(12) #Software error, SUBJID attributed twice

# ITT (Intent-to-Treat): All randomized patients (excluding screening failures only)
pop_itt <- pop_total %>% setdiff(55)

# mITT (Modified ITT): All treated patients
pop_m_itt <- pop_itt %>% setdiff(68) #Patient 68 randomized but never received treatment

# PP (Per-Protocol): Patients who completed treatment without major protocol deviations
pop_pp <- pop_m_itt %>% setdiff(c(33, 79)) #Major deviations

# Safety: All patients who received at least one dose of treatment
pop_safety <- pop_itt %>% setdiff(68)  #Same as mITT

# Evaluable: Patients who completed required assessments for primary endpoint
pop_evaluable <- pop_itt %>% setdiff(c(44, 91))  #No primary endpoint assessment

l = list(
  "Total population"=pop_total,
  "ITT population"=pop_itt,
  "mITT population"=pop_m_itt,
  "PP population"=pop_pp,
  "Safety population"=pop_safety,
  "Evaluable population"=pop_evaluable
)
edc_population_plot(l[-1], ref=pop_total)

