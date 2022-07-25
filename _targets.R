library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

source("R/functions.R")
source("_packages.R")
options(timeout=24*60*60) # let things download for at least 24 hours (important while on slow internet connection)
options(download.file.method = "libcurl")

tar_invalidate(pages)

# End this file with a list of target objects.
list(
  tar_target(college_data, AppendAbortion(AppendMisconduct(AppendVaccination(AggregateIPEDS())))),
  tar_target(save_raw_data, write.csv(college_data, "docs/college_data.csv")),
  tar_target(degree_granting , FilterForDegreeGranting(college_data)),
  tar_target(overview, GetOverviewColumns(degree_granting)),
  tar_target(pages, RenderInstitutionPages(overview, degree_granting))

)
