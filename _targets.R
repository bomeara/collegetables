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
#tar_invalidate(index)
#tar_invalidate(about)
#tar_invalidate(sparklines)
#tar_invalidate(state_pages)
tar_invalidate(pages_new)

# End this file with a list of target objects.
list(
 tar_target(maxcount, Inf),
 tar_target(spark_height, 80),
 tar_target(spark_width, 200),
 tar_target(ipeds_directly, GetIPEDSDirectly()),
 tar_target(ipeds_direct_and_db, AggregateDirectIPEDSDirect(ipeds_directly)),
 tar_target(comparison_table_core, CreateComparisonTables(ipeds_direct_and_db)),
 tar_target(comparison_table, AppendCATravelBan(AppendGunLaws(AppendMarriageRespect(AppendContraceptiveSupport(comparison_table_core))))),
 tar_target(fields_and_majors, GetFieldsAndMajors(comparison_table, ipeds_direct_and_db)),
 tar_target(pages_new, RenderInstitutionPagesNew(comparison_table, spark_width, spark_height, maxcount=maxcount)),
 tar_target(CIPS_codes, GetCIPCodesExplanations()),
 tar_target(majors, RenderMajorsPages(fields_and_majors, CIPS_codes)),
 tar_render(fields, "fields.Rmd"),
 tar_render(about, "about.Rmd")

)

# old stuff no longer used
#  tar_target(college_data_ipeds, AggregateIPEDS()),
#   tar_target(college_data_prebioclim, AppendContraceptiveSupport(AppendMarriageRespect(AppendGunLaws(AppendBiome(AppendAAUPCensure(AppendAbortion(AppendMisconduct(AppendVaccination(FilterForDegreeGranting(college_data_ipeds)))))))))),
#   tar_target(college_data, AppendBioclim(college_data_prebioclim)),
#   tar_target(sparklines, RenderSparklines(spark_height=spark_height, spark_width=spark_width)),
#   tar_target(state_pops, GetPopulationByStateAtAge18()),
#   tar_target(students_by_state_by_institution, GetStudentsByStateByInstitution(degree_granting, state_pops)),
#   tar_target(student_demographics, GetStudentsDemographicsByInstitution(degree_granting)),
#   tar_target(faculty_counts, GetFacultyCountsByInstitution(degree_granting)),
#   tar_target(degree_granting , FilterForDegreeGranting(college_data)),
#   tar_target(degree_granting_enhanced, RoughRanking(AppendCrime(EnhanceData(degree_granting, faculty_counts, student_demographics, students_by_state_by_institution )) )),
#   tar_target(overview, GetOverviewColumns(degree_granting_enhanced)),
#   tar_target(pages, RenderInstitutionPages(overview, degree_granting_enhanced, maxcount, students_by_state_by_institution, student_demographics, faculty_counts, spark_width, spark_height)),
#   tar_target(state_pages, RenderStatePages(degree_granting_enhanced, students_by_state_by_institution, spark_width, spark_height, state_pops)),
#   tar_target(top,FilterForTopAndSave(overview)),
#   tar_target(index, RenderIndexPageEtAl(pages)),