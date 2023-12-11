
skip_on_cran()
skip_on_ci()


test_that("MUCILA OK", {
  file = "F:/Nextcloud GR/04 - Comite Pediatrie/MUCILA/01- MUCILA interim analysis 1/data/MUCILA_Sas_export_SAS_XPORT_2023_10_13_12_04.zip"
  tm1 = read_trialmaster(file, pw="mucila", use_cache=FALSE, verbose=FALSE)
  
  expect_equal(dim(tm1$.lookup), c(21,8))
})


test_that("MEDEA OK", {
  file = "F:/Nextcloud GR/03 - Comite Sein/MEDEA/11 - Stats/MEDEA - Analyse finale/data/MEDEA_Export_sas_V2_stat_(no_data_included)_SAS_XPORT_2023_09_05_14_24.zip"
  tm2 = read_trialmaster(file, pw="medea", use_cache=FALSE, verbose=FALSE) %>% 
    expect_classed_conditions(warning="edc_read_column_error_warning")#erreurs sur GPAQ
  
  expect_equal(dim(tm2$.lookup), c(32,8))
})


test_that("HRNBL2 OK", {
  file = "F:/Nextcloud GR/04 - Comite Pediatrie/HR-NBL2/IDMC1 - Dan/data/HRNBL2_Export_Stat_avec_missing_SAS_XPORT_2023_11_02_13_54.zip"
  tm3 = read_trialmaster(file, pw="0", use_cache=FALSE, verbose=FALSE)
  
  expect_equal(dim(tm3$.lookup), c(58,8))
})


