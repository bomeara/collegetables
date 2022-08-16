Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
system("export PATH=$PATH:/usr/local/bin")

library(targets)
source("_packages.R")
source("R/functions.R")

# Only render site if needing to do new pages

# rmarkdown::render_site()

tar_make()

system("cp *html docs")
system("cp CNAME docs/CNAME")