rm(list=ls())
Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
system("export PATH=$PATH:/usr/local/bin")
#Sys.setenv(R_MAX_VSIZE = 16e10)
library(targets)
source("_packages.R")
source("R/functions.R")


tar_option_set(memory = "transient", garbage_collection = TRUE)


# Only render site if needing to do new pages

#rmarkdown::render_site()
tar_make(callr_function = NULL)
#tar_make()


system("cp *html docs")
system("cp CNAME docs/CNAME")