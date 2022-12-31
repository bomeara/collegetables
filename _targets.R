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

#tar_invalidate(pages)
tar_invalidate(index_et_al)
#tar_invalidate(about)
#tar_invalidate(sparklines)
#tar_invalidate(state_pages)
#tar_invalidate(pages_new)

# End this file with a list of target objects.
list(
 tar_target(maxcount, Inf),
 tar_target(ipeds_directly, GetIPEDSDirectly()),
 tar_target(ipeds_direct_and_db, AggregateDirectIPEDSDirect(ipeds_directly)),
 tar_target(comparison_table_core, CreateComparisonTables(ipeds_direct_and_db)),
 tar_target(comparison_table, AppendBiome(AppendAAUPCensure(AppendAbortion(AppendVaccination(AppendCATravelBan(AppendGunLaws(AppendMarriageRespect(AppendContraceptiveSupport(comparison_table_core))))))))),
 tar_target(index_table, CreateIndexTable(comparison_table)),
 tar_target(weatherspark, GetWeathersparkLinks()),
 tar_target(fields_and_majors, GetFieldsAndMajors(comparison_table, ipeds_direct_and_db, CIPS_codes)),
 tar_target(pages_new, RenderInstitutionPagesNew(comparison_table, fields_and_majors, maxcount=maxcount, CIPS_codes, weatherspark)),
 tar_target(CIPS_codes, GetCIPCodesExplanations()),
 #tar_target(majors, RenderMajorsPages(fields_and_majors, CIPS_codes)),
 tar_render(fields, "_fields.Rmd", output_file = "fields.html"),
 tar_render(about, "_about.Rmd", output_file = "about.html"),
 tar_target(index_et_al, RenderIndexPageEtAl(pages_new, index_table))

)
