


# Base MACRO --------------------------------------------------------------


if(FALSE){
  #Lecture de la base de données et écriture dans un fichier RData
  #La date d'extraction est la date de modification du fichier de formats
  
  library(haven)
  # data_folder = "../../../11-DataBase/marci2/OUT/"
  data_folder = "//nas-01/SBE_ETUDES/MARCI/11-DataBase/DB/"
  format_file = paste0(data_folder, "formats.sas7bcat")
  extraction_date = file.info(format_file)$mtime %>% format("%Y-%m-%d")
  files = c()
  dir(data_folder) %>% 
    walk(~{
      print(.x)
      if(!str_ends(.x, "sas7bdat")) return(NULL)
      df=read_sas(paste0(data_folder, .x), format_file) %>%
        mutate(
          across(where(is.character), ~na_if(.x, "")), 
          across(where(is.labelled), as_factor)
        ) %>% 
        rename_all(tolower)
      files <<- c(files, str_remove(.x, "\\..*"))
      assign(str_remove(.x, "\\..*"), df, envir=global_env())
    })
  save.image(glue("data/marci_base_{extraction_date}.RData"))
  beep()
  
  
  
  needle = "lymp"
  tablelist %>% 
    mutate(labels2 = imap(labels, ~{
      x1=map(.x, str_subset, pattern=needle) %>% discard(~length(.x)==0)
      # x2=map(names(.x), str_subset, pattern=needle) %>% discard(~length(.x)==0)
      x2=NULL
      # browser()
      c(x1,x2) %>% unlist
    })) %>% 
    filter(map_lgl(labels2, ~length(.x)>0)) %>% 
    unnest(labels2)
  
  
  # Checks ------------------------------------------------------------------
  
  # datasets %>% set_names() %>% map(~get(.x) %>% dim)
  tibble(dataset=datasets, 
         label=map_chr(datasets, ~get(.x)$CRFNAME[1]),
         nrow=map_dbl(datasets, ~get(.x) %>% nrow), 
         ncol=map_dbl(datasets, ~get(.x) %>% ncol)) %>% 
    # arrange(nrow) %>% 
    as.data.frame()
  
  
}
