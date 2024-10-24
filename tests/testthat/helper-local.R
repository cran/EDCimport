

cli::cli_inform(c(v="Initializer {.file helper-local.R} loaded"))


file_medea = test_path("REAL_nogit/MEDEA_Export_sas_V2_stat_(no_data_included)_SAS_XPORT_2023_09_05_14_24.zip")
file_mucila = test_path("REAL_nogit/MUCILA_Sas_export_SAS_XPORT_2023_10_13_12_04.zip")
file_atezo = test_path("REAL_nogit/ATEZO_ExportTemplate_2020_12_31_SAS_XPORT_2024_02_26_10_38.zip")

use_cache=TRUE
# use_cache="write"

# DO = TRUE
# DO = FALSE

if(!is_testing() && !is_checking()){
  cli_inform("Loading ATEZOLACC")
  

  # file_hrnbl2 = test_path("REAL_nogit/HRNBL2_Export_Stat_avec_missing_SAS_XPORT_2023_11_02_13_54.zip")
  # tm = read_trialmaster(file_hrnbl2, pw="0", use_cache=use_cache, verbose=FALSE)
  
  # file_mucila = test_path("REAL_nogit/MUCILA_Sas_export_SAS_XPORT_2023_10_13_12_04.zip")
  # tm = read_trialmaster(file_mucila , pw="mucila", use_cache=use_cache, verbose=FALSE)
  
  file_atezo = test_path("REAL_nogit/ATEZO_ExportTemplate_2020_12_31_SAS_XPORT_2024_02_26_10_38.zip")
  tm = read_trialmaster(file_atezo, pw="SamplePassword", use_cache=use_cache, verbose=FALSE)
  
  # tm = read_trialmaster(file_medea, pw="medea", use_cache=use_cache, verbose=FALSE) %>% expect_classed_conditions(warning="edc_read_column_error_warning")#erreurs sur GPAQ
  
  load_list(tm, remove=FALSE, env=rlang::global_env())
  # load_list(tm, env=rlang::global_env())
  
  # options(edc_subjid_ref=enrolres$SUBJID)
  
  # rm(datetime_extraction)
  # aaa="15151"
  # ae %>% head(20) %>% 
  #   edc_data_warn("aaa={aaa}")
  # x = ae
  # x$SUBJID[1]=1337
  # x %>% edc_warn_patient_diffs() %>% nrow()
  # x %>% edc_warn_patient_diffs(issue_n=8) %>% nrow()
  # 
  # edc_data_warnings()
}