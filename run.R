Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
system("export PATH=$PATH:/usr/local/bin")

library(targets)
source("_packages.R")
source("R/functions.R")

tar_make()

rmarkdown::render_site()
Sys.sleep(10)
#system("cp /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/data/*csv /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/docs")
system("git add docs")
system("git commit -m'updated data' -a")
system('git push')
