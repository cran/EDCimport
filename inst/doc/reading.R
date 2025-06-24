## -----------------------------------------------------------------------------
#| include: false
library(EDCimport)


## -----------------------------------------------------------------------------
db = edc_example()
load_database(db)
edc_lookup()
edc_find_column("date")
edc_find_value("immune")


## ----echo=FALSE, results='asis'-----------------------------------------------
if (file.exists("edc_viewer.webp")) {
  cat('![](edc_viewer.webp)')
}

