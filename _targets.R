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



# End this file with a list of target objects.
list(
  tar_target(hesitancy_by_county, GetHesitancyByCounty()),
  tar_target(cdc_weekly_metadata, GetCommunityTransmissionReportLastDate()),
  tar_target(cdc_weekly, GetCommunityTransmissionReport(report_url=cdc_weekly_metadata$report_url)),
  tar_target(cdc_all_metadata, GetAllCommunityTransmissionReportDates()),
  tar_target(cdc_all_reports_old, GetAllCommunityTransmissionReports(cdc_all_metadata$report_urls, cdc_all_metadata$report_dates)),
  tar_target(cdc_reports_new_2022, GetCommunityLevels2022Format()),
  tar_target(cdc_all_reports, CombineOldAndNewCDC(cdc_all_reports_old, cdc_reports_new_2022)),
  tar_target(aggregation_by_county, JoinHesitancyWithCDCWeekly(hesitancy_by_county, cdc_weekly)),
  tar_target(aggregation_table_csv, WriteCSVTable(aggregation_by_county), format="file"),
  tar_target(daily_focal, CreateDailyFocal()),
  tar_target(schoolkids_daily, CreateSchoolkidsDaily()),
  tar_target(hospital_knox, CreateHospitalKnox()),
  tar_target(schools_oakridge, CreateSchoolsOakRidge()),
  tar_target(schools_knox, CreateSchoolsKnox()),
  tar_target(zukowski_data, GetZukowskiData()),
  tar_target(hhs_capacity_tn, CreateHHSDataTN()),
  tar_target(hhs_capacity_tn_focal, CreateHHSDataFocalCities(hhs_capacity_tn)),
  tar_target(hhs_capacity_tn_focal_latest_pretty, CreateHHSDataFocalCitiesPretty(hhs_capacity_tn_focal)),
  tar_target(biobot_sewage, GetBiobotSewage()),
  tar_target(covid_by_demographic_in_tn, CreateDemographicDataInTN()),
  tar_target(sumtab, CreateSummaryTable(covid_by_demographic_in_tn)),
  tar_target(sumtabfraction, ComputeSummaryTableFraction(sumtab)),
  tar_target(saliva_data, CreateSalivaData()),
  tar_target(utk.cases, ComputeUTKCasesOld()),
  tar_target(utk_active_cases_reported, ComputeUTKActiveCasesReported()),
  tar_target(utk_isolations_reported, ComputeUTKIsolationsReported()),
  tar_target(knox17to25, Get17To25Knox()),
  tar_target(tn_death_predictions, GetTNDeathPredictions()),
  tar_target(tn_death_history, GetTNDeathRecord()),
  tar_target(individual_schools_knox, CreateIndividualSchoolsKnox()),
  tar_target(sewage, GetSewageData()),
  tar_target(kcs_dashboard, read.delim("https://kcsdashboard.org/reports/export?Search%5Bschool_id%5D=&Search%5Bgrade%5D=&Search%5Bstart_date%5D=&Search%5Bend_date%5D=")),
  tar_target(individual_schools_knox_csv_out, target_save_csv(individual_schools_knox, filename="data/individual_schools_knox.csv"), format="file"),
  tar_target(daily_focal_out, target_save_csv(daily_focal, filename="data/daily_focal.csv"), format="file"),
  tar_target(schoolkids_daily_out, target_save_csv(schoolkids_daily, filename="data/schoolkids_daily.csv"), format="file"),
  tar_target(hospital_knox_out, target_save_csv(hospital_knox, filename="data/hospital_knox.csv"), format="file"),
  tar_target(schools_oakridge_out, target_save_csv(schools_oakridge, filename="data/schools_oakridge.csv"), format="file"),
  tar_target(schools_knox_out, target_save_csv(schools_knox, filename="data/schools_knox.csv"), format="file"),
  tar_target(microcovid_data, GetMicrocovid()),
  tar_target(microcovid_summary, SummarizeMicrocovidData(microcovid_data)),
  tar_target(tsa_throughput, GetTSAThroughput())
  #tar_target(tysflights, GetTYSFlights())


  #tar_render(yearbyyear, "yearbyyear.Rmd", params = list(daily_focal=daily_focal, schoolkids_daily=schoolkids_daily)),
  #tar_render(indexhtml, "index.Rmd", params = list(sumtab=sumtabfraction)),
  #tar_render(oakridgeschools, "oakridgeschools.Rmd", params = list(schools_oakridge=schools_oakridge))


)
