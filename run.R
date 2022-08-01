Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
system("export PATH=$PATH:/usr/local/bin")


library(targets)
source("_packages.R")
source("R/functions.R")
#rmarkdown::render_site()

tar_make()

#system("cp /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/data/*csv /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/docs")
#system("git add docs")
#system("git commit -m'updated data' -a")
#system('git push')

system("cp *html docs")