
skip_on_cran()
skip_on_ci()
skip_if(is_checking())

edc_options(edc_lookup_overwrite_warn=FALSE)

test_that("All OK", {
  # file = test_path("REAL_nogit/MUCILA_Sas_export_SAS_XPORT_2023_10_13_12_04.zip")
  # tm1 = read_trialmaster(file, pw="mucila", use_cache=FALSE, verbose=FALSE)
  # expect_equal(dim(tm1$.lookup), c(21,8))
  # file = test_path("REAL_nogit/MEDEA_Export_sas_V2_stat_(no_data_included)_SAS_XPORT_2023_09_05_14_24.zip")
  # tm2 = read_trialmaster(file, pw="medea", use_cache=FALSE, verbose=FALSE) %>% 
  #   expect_classed_conditions(warning="edc_read_column_error_warning")
  # expect_equal(dim(tm2$.lookup), c(32,8))
  # file = test_path("REAL_nogit/HRNBL2_Export_Stat_avec_missing_SAS_XPORT_2023_11_02_13_54.zip")
  # tm3 = read_trialmaster(file, pw="0", use_cache=FALSE, verbose=FALSE)
  # expect_equal(dim(tm3$.lookup), c(58,8))
  # file = test_path("REAL_nogit/ATEZO_ExportTemplate_2020_12_31_SAS_XPORT_2024_02_26_10_38.zip")
  # tm4 = read_trialmaster(file, pw="SamplePassword", use_cache=FALSE, verbose=FALSE)
  # expect_equal(dim(tm4$.lookup), c(38,8))
  expect_true(TRUE)
})







test_that("MUCILA OK", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  file = test_path("REAL_nogit/MUCILA_Sas_export_SAS_XPORT_2023_10_13_12_04.zip")
  tm1 = read_trialmaster(file, pw="mucila", use_cache=FALSE, verbose=FALSE)
  expect_equal(dim(tm1$.lookup), c(21,8))
  load_list(tm1)
  expect_snapshot({
    get_meta_cols()
    lastnews_table()
    get_datasets() %>% map(table_format, warn=FALSE) %>% unlist()
  })
  # a = get_meta_cols()
  # lastnews_table()
  # a = get_datasets() %>% map(table_format, warn=FALSE) %>% unlist()
  # expect_equal(a, c(f18___re = "wide", inccorr = "wide", mucoseva = "wide", consump = "mixed", 
  #                   diary = "mixed", treatmen = "mixed", laser = "mixed", hemato = "mixed", 
  #                   pac = "mixed", des1 = "wide", eos = "wide", vs = "wide", visit = "mixed", 
  #                   ec = "wide", enrolres = "wide", ic = "wide", pat = "wide", rand = "wide"
  # ))
})


test_that("MEDEA OK", {
  clean_lookup()
  local_options(edc_lookup_overwrite_warn=FALSE)
  file = test_path("REAL_nogit/MEDEA_Export_sas_V2_stat_(no_data_included)_SAS_XPORT_2023_09_05_14_24.zip")
  tm2 = read_trialmaster(file, pw="medea", use_cache=FALSE, verbose=FALSE)
  expect_equal(dim(tm2$.lookup), c(32,8))
  
  expect_s3_class(tm2$gpaq$GPAQ16, c("hms", "difftime"))#pas d'erreur sur GPAQ
  
  load_list(tm2)
  expect_snapshot({
    get_meta_cols()
    lastnews_table()
    get_datasets() %>% map(table_format, warn=FALSE) %>% unlist()
  })
})


test_that("HRNBL2 OK", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  file = test_path("REAL_nogit/HRNBL2_Export_Stat_avec_missing_SAS_XPORT_2023_11_02_13_54.zip")
  tm3 = read_trialmaster(file, pw="0", use_cache=FALSE, verbose=FALSE)
  expect_equal(dim(tm3$.lookup), c(58,8))
  
  load_list(tm3)
  expect_snapshot({
    get_meta_cols()
    lastnews_table()
    get_datasets() %>% map(table_format, warn=FALSE) %>% unlist()
  })
})

test_that("ATEZOLACC OK", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  file = test_path("REAL_nogit/ATEZO_ExportTemplate_2020_12_31_SAS_XPORT_2024_02_26_10_38.zip")
  tm4 = read_trialmaster(file, pw="SamplePassword", use_cache=FALSE, verbose=FALSE)
  expect_equal(dim(tm4$.lookup), c(38,8))
  
  load_list(tm4)
  expect_snapshot({
    get_meta_cols()
    lastnews_table()
    get_datasets() %>% map(table_format, warn=FALSE) %>% unlist()
  })
})


test_that("NIVOGLIO OK", {
  local_options(edc_lookup_overwrite_warn=FALSE)
  path = test_path("REAL_nogit/NIVOGLIO")
  
  tm5 = read_all_sas(path, format_file="formats.sas7bdat", verbose=FALSE) %>%  harmonize_subjid()
  #TODO
  # tm5 = read_all_sas(path, format_file="formats.sas7bcat", verbose=FALSE)
  # tm5 = read_all_sas(path, verbose=FALSE)
  expect_equal(dim(tm5$.lookup), c(22,8))
  expect_equal(levels(tm5$dm$GENDER), c("Male", "Female"))
  
  load_list(tm5)
  expect_snapshot({
    get_meta_cols()
    lastnews_table()
    get_datasets() %>% map(table_format, warn=FALSE) %>% unlist()
  })
})



# edc_options()
# 
# 
# # load_list(tm4)
# followup_table()
# get_meta_cols()
# 
# a1 = tm4 %>% split_mixed_datasets()
# a1 = tm4 %>% map(table_format, ignore_cols=get_meta_cols(0.95))
# a2 = tm4 %>% map(table_format, ignore_cols=get_meta_cols(0.5))
# cbind(a1, a2)
