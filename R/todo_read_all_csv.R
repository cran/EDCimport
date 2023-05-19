
#TODO
read_tm_all_csv = function(directory){
  datasets = dir(directory, pattern = "\\.csv$", full.names=TRUE)
  datasets_names = basename(datasets) %>% str_remove("\\.csv")
  datetime_extraction = get_folder_datetime(directory)
  rtn = datasets %>% 
    set_names(tolower(datasets_names)) %>% 
    imap(~{
      tryCatch({read.csv(.x) %>% as_tibble() %>% na_if("")}, 
               error=function(e) e)
    })
  
  #TODO mettre format_file=NULL en argument et lancer le Warn depuis read_trialmaster()
  #chercher format_file and chemin absolu, sinon en chemin relatif depuis wd, sinon en chemin relatif depuis directory
  format_file = file.path2(directory, "procformat.sas")
  if(file.exists(format_file)){
    sas_formats = read_sas_format(format_file)
    rtn = rtn %>%
      imap(~{
        if(is_error(.x)) return(.x)
        .x %>%
          apply_sas_formats(sas_formats) %>%
          haven::as_factor()
      })
  } else {
    cli_warn("No file {.val procformat.sas} found in {.arg directory}.
             Data formats cannot be applied.",
             class="edc_tm_no_procformat_warning")
  }
  
  errs = keep(rtn, is_error)
  if(length(errs)>0){
    cli_warn(c("SAS dataset{?s} {.val {names(errs)}} could not be read from the archive using {.fun haven::read_xpt}.",i="You can print the object{?s} to see the error message (e.g. run {.run print({names(errs[1])})})."), 
             class="edc_tm_problem_warning")
  }
  
  rtn$date_extraction = format_ymd(datetime_extraction)
  rtn$datetime_extraction = datetime_extraction
  rtn$.lookup = get_lookup(rtn)
  if(is.null(getOption("edc_lookup", NULL))){
    options(edc_lookup=rtn$.lookup)
  }
  rtn
}
