as.numeric.na0 <- function(x) {
	 x <- suppressWarnings(as.numeric(x))
	#  if(any(is.na(x))) {
	# 	warning(paste0("In as.numeric(x): 0 introduced by coercion"))	
	#  }
	 x[is.na(x)] <- 0
	 return(x)	
}

GetIPEDSNames <- function() {
	IPEDS_names <- c(
		Institutional_directory = "HDzzzz",
		Institutional_offerings = "ICzzzz",
		Institutional_charges = "ICzzzz_AY",
		Institutional_charges_vocational = "ICzzzz_PY",
		Twelve_month_headcount = "EFFYzzzz", 
		Twelve_month_instructional_activity = "EFIAzzzz",
		Admissions = "ADMzzzz",
		Enrollment_race = "EFzzzzA",
		Enrollment_major = "EFzzzzCP",
		Enrollment_age = "EFzzzzB",
		Enrollment_residence = "EFzzzzC",
		Enrollment_total = "EFzzzzD",
		#Enrollment_distance = "EFzzzzA_Dist",
		Completions_program = "Czzzz_A",
		Completions_race = "Czzzz_B",
		Completions_age = "Czzzz_C",
		#Competions_distance = "CzzzzDEP",
		Instructional_gender = "SALzzzz_IS",
		Instructional_noninstructional = "SALzzzz_NIS",
		Staff_category = "Szzzz_OC",
		Staff_tenure_status = "Szzzz_SIS",
		Staff_gender = "Szzzz_IS",
		Staff_new = "Szzzz_NH",
		Employees_position = "EAPzzzz",
		Finance_GASB = "Fxxyy_F1A",
		Finance_FASB = "Fxxyy_F2",
		Finance_ForProfits = "Fxxyy_F3",
		Student_aid = "SFAxxyy",
		Student_military = "SFAVxxyy", 
		Graduation_FourYears = "GRzzzz",
		Graduation_TwoYears = "GRzzzz_L2",
		Graduation_Pell = "GRzzzz_PELL_SSL",
		Outcome_levels = "OMzzzz",
		Academic_library = "ALzzzz"
  )	
  return(IPEDS_names)
}

GetIPEDSDirectly <- function(years=as.numeric(format(Sys.Date(), "%Y")):2010, IPEDS_names = GetIPEDSNames()) {
	print("Starting GetIPEDSDirectly")
	finished_downloads <- c()
  # Get IPEDS data directly from the IPEDS website
  # Input: years to download
  # Output: array with IPEDS data
  # Replace 'zzzz' with the year
  # Replace 'xxyy' with the last two digits of this year and the previous year: 1920 for 2019-20, 1819 for 2018-19, etc.
  # Source: https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2018&surveyNumber=-1 
  # I am using the category and then the first relevant word or so of the description (but things like "gender" might actually be "gender, ethnicity, and age")
 
  for (year_index in seq_along(years)) {
	zzzz <- years[year_index]
	xxyy <- paste0(-1+as.numeric(substr(zzzz, 3, 4)), substr(zzzz, 3, 4))
	IPEDS_names_local <- IPEDS_names
	IPEDS_names_local <- gsub("zzzz", zzzz, IPEDS_names_local)
	IPEDS_names_local <- gsub("xxyy", xxyy, IPEDS_names_local)
	#focal_year_df <- data.frame()
	for (IPEDS_index in seq_along(IPEDS_names_local)) {
		gc()
		try({
			#print(object.size(x=lapply(ls(), get)), units="Gb")
			# Get the file with raw data
			file_to_get <- paste0('https://nces.ed.gov/ipeds/datacenter/data/', IPEDS_names_local[IPEDS_index],'.zip')
			temp <- tempfile()
			download.file(file_to_get,temp)
			print(file.path(tempdir(), year_index, IPEDS_index))
			unzip(temp, exdir = file.path(tempdir(), year_index, IPEDS_index))
			potential_files <- list.files(file.path(tempdir(), year_index, IPEDS_index), pattern=".*csv", full.names = TRUE)
			print(potential_files)
			file_to_read <- potential_files[1]
			if (length(potential_files) > 1) {
				for (potential_file_index in seq_along(potential_files)) {
					if (grepl("rv", potential_files[potential_file_index])) { # revised!
						file_to_read <- potential_files[potential_file_index]
					}
				}
			}
			local_data <- NULL
			try(local_data <- as.data.frame(readr::read_csv(file_to_read, col_types="c", col_names=TRUE)))
			# local_data <- NULL
			# try(local_data <- arrow::read_csv_arrow(file_to_read,  col_names=TRUE), silent=TRUE)
			 if(is.null(local_data)) {
			 	try(local_data <- arrow::read_csv_arrow(file_to_read,  col_names=TRUE, as_data_frame=TRUE))
			 }

			unlink(temp)
			print(paste0("Read ", IPEDS_names_local[IPEDS_index], " for ", zzzz, ". Its size is:"))
			print(dim(local_data))
			print(object.size(local_data), units="Mb")

			# Get the dictionary converting codes to meaning
			dictionary_to_get <- paste0('https://nces.ed.gov/ipeds/datacenter/data/', IPEDS_names_local[IPEDS_index],'_Dict.zip')
			temp2 <- tempfile()
			
			download.file(dictionary_to_get,temp2)
			print(file.path(tempdir(), year_index, IPEDS_index, "dict"))
			unzip(temp2, exdir = file.path(tempdir(), year_index, IPEDS_index, "dict"))
			potential_files <- list.files(file.path(tempdir(), year_index, IPEDS_index, "dict"), pattern=".*xls.*", full.names = TRUE)
			print(potential_files)
			dictionary_to_read <- potential_files[1]
			dictionary_frequencies <- NULL
			dictionary_variables <- NULL
			try({dictionary_frequencies <- readxl::read_excel(dictionary_to_read, sheet = "Frequencies", col_types="text")}, silent=TRUE)
			if(is.null(dictionary_frequencies)) {
				try({dictionary_frequencies <- readxl::read_excel(dictionary_to_read, sheet = "frequencies", col_types="text")}, silent=TRUE)
			}
			try({dictionary_variables <- readxl::read_excel(dictionary_to_read, sheet = "varlist", col_types="text")}, silent=TRUE)
			if(is.null(dictionary_variables)) {
				try({dictionary_variables <- readxl::read_excel(dictionary_to_read, sheet = "Varlist", col_types="text")}, silent=TRUE)
			}
			unlink(temp2)
			
			# Ok, now we have the raw data and the dictionary. Let's convert the raw data to be more sensible
			try({
				#print(object.size(x=lapply(ls(), get)), units="Gb")
				unique_varnames_to_encode <- unique(dictionary_frequencies$varname)
				for (varname_index in seq_along(unique_varnames_to_encode)) {
					focal_freq <- subset(dictionary_frequencies, varname == unique_varnames_to_encode[varname_index])
					potential_varnames <- unique(local_data[, unique_varnames_to_encode[varname_index]])
					potential_varnames_numeric <- suppressWarnings(na.omit(as.numeric(potential_varnames)))
					print(dim(local_data))
					local_data <- local_data %>% tibble::add_column(original_placeholder = local_data[, unique_varnames_to_encode[varname_index]])
					colnames(local_data)[length(colnames(local_data))] <- paste0(unique_varnames_to_encode[varname_index], " ", unique_varnames_to_encode[varname_index], " ORIGINAL CODES")
					print(dim(local_data))
					# this is to handle matching when the dictionary has a leading 0
					if(length(potential_varnames_numeric) == length(potential_varnames)) {
						# Ok, we can convert this to numeric
						local_data[, unique_varnames_to_encode[varname_index]] <- focal_freq$valuelabel[match(c(as.numeric(local_data[, unique_varnames_to_encode[varname_index]])),as.numeric(focal_freq$codevalue))]
					} else {
						local_data[, unique_varnames_to_encode[varname_index]] <- focal_freq$valuelabel[match(c(as.character(local_data[, unique_varnames_to_encode[varname_index]])),as.character(focal_freq$codevalue))]
					}
					print(paste("For column ", unique_varnames_to_encode[varname_index], " we have ", length(potential_varnames), " unique values."))
					print(unique(local_data[, unique_varnames_to_encode[varname_index]]))
				}
			})
			for (col_index in sequence(ncol(local_data))) {
				matching <- match(colnames(local_data)[col_index],dictionary_variables$varname)
				if(!is.na(matching)) {
					colnames(local_data)[col_index] <- paste0(colnames(local_data)[col_index], " ", dictionary_variables$varTitle[matching])
				}
			}
			local_data <- local_data[colSums(!is.na(local_data)) > 0]
			local_data <- local_data[,grepl(" ", colnames(local_data))]
			local_data$`IPEDS Year` <- zzzz
			dimension <- "normal"
			#print(object.size(x=lapply(ls(), get)), units="Gb")
			do_wider <- FALSE
			if(max(table(local_data[,"UNITID Unique identification number of the institution"]))>1) {
				dimension <- "wide"
			}
			if(do_wider) {
				if(max(table(local_data[,"UNITID Unique identification number of the institution"]))>1) {
					# IPEDS is weird. Sometimes they have multiple rows for the same institution. So we split them up based on the factors used (the second column)
					unique_second_elements <- unique(local_data[,2])
					unique_second_elements <- unique_second_elements[!is.na(unique_second_elements)]
					wider_local_data <- data.frame() 
					for (unique_second_element_index in seq_along(unique_second_elements)) {
						gc()
						#print(object.size(wider_local_data, units="Mb"))
						print(dim(wider_local_data))
						local_data_slice <- base::subset(local_data, local_data[,2] %in% unique_second_elements[unique_second_element_index])
						local_data_slice <- local_data_slice[,-2]
						colnames(local_data_slice)[2:ncol(local_data_slice)] <- paste0(colnames(local_data_slice)[2:ncol(local_data_slice)], " ", unique_second_elements[unique_second_element_index])
						local_data_slice <- local_data_slice[!duplicated(local_data_slice$`UNITID Unique identification number of the institution`),]
						if(unique_second_element_index==1) {
							wider_local_data <- local_data_slice
						} else {
							wider_local_data <- dplyr::full_join(wider_local_data, local_data_slice, by="UNITID Unique identification number of the institution")
						}
					}	
					local_data <- wider_local_data
					#print(object.size(x=lapply(ls(), get)), units="Gb")

					rm(wider_local_data)
				}
			} 
			#print(object.size(x=lapply(ls(), get)), units="Gb")
			colnames(local_data)[-1] <- paste0(IPEDS_names[IPEDS_index], " ", colnames(local_data)[-1])
			
			# colnames(local_data) <- gsub(zzzz, "CurrentYear", colnames(local_data))
			# colnames(local_data) <- gsub(as.numeric(zzzz)-1, "PreviousYear", colnames(local_data))
			# colnames(local_data) <- gsub(as.numeric(zzzz)-2, "TwoYearsAgo", colnames(local_data))
			
			# LastYear_ThisYear <- paste0(as.numeric(zzzz)-1, '-', stringr::str_pad(-1+as.numeric(substr(zzzz, 3, 4)), 2, pad="0"))
			
			# colnames(local_data) <- gsub(LastYear_ThisYear, "LastYear_ThisYear", colnames(local_data))

			# ThisYear_NextYear <- paste0(as.numeric(zzzz), '-', stringr::str_pad(1+as.numeric(substr(zzzz, 3, 4)), 2, pad="0"))


			# colnames(local_data) <- gsub(ThisYear_NextYear, "ThisYear_NextYear", colnames(local_data))
						
			colnames(local_data)[grepl('IPEDS Year', colnames(local_data))] <- 'IPEDS Year'
			
			local_data <- mutate_all(local_data, as.character)
			new_top_row <- local_data[1,]
			new_top_row[1,] <- rep("z", ncol(new_top_row)) # so when we load things with arrow later it won't fail if there's a shorter initial row
			print(object.size(local_data), units="Mb")
			write.csv(rbind(new_top_row, local_data), paste0("data/IPEDS_", IPEDS_names[IPEDS_index], "_", dimension, "_", zzzz, ".csv"), row.names=FALSE)
			finished_downloads <- append(finished_downloads, paste0("data/IPEDS_", IPEDS_names[IPEDS_index], "_", dimension, "_", zzzz, ".csv"))
			rm(local_data)
			
			#save(focal_year_df, file=paste0("~/Downloads/focal_year_df_", zzzz, ".RData"))

		})
  	}
  }
  return(finished_downloads)
} 

# Idea here is one table to rule them all with all the info we want to compare between schools. 
CreateComparisonTables <- function(ipeds_direct_and_db) {
	db <- dbConnect(RSQLite::SQLite(), "data/db_IPEDS.sqlite")
	comparison_table <- tbl(db, "Institutional_directory") %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"HDzzzz.INSTNM.Institution..entity..name",
		"HDzzzz.IALIAS.Institution.name.alias",
		"HDzzzz.ADDR.Street.address.or.post.office.box",
		"HDzzzz.CITY.City.location.of.institution",
		"HDzzzz.STABBR.State.abbreviation",
		"HDzzzz.ZIP.ZIP.code",
		"HDzzzz.FIPS.FIPS.state.code",
		"HDzzzz.OBEREG.Bureau.of.Economic.Analysis..BEA..regions",
		"HDzzzz.CHFNM.Name.of.chief.administrator",
		"HDzzzz.CHFTITLE.Title.of.chief.administrator",
		"HDzzzz.OPEID.Office.of.Postsecondary.Education..OPE..ID.Number",
		"HDzzzz.OPEFLAG.OPE.Title.IV.eligibility.indicator.code",
		"HDzzzz.WEBADDR.Institution.s.internet.website.address",
		"HDzzzz.ADMINURL.Admissions.office.web.address",
		"HDzzzz.FAIDURL.Financial.aid.office.web.address",
		"HDzzzz.APPLURL.Online.application.web.address",
		"HDzzzz.NPRICURL.Net.price.calculator.web.address",
		"HDzzzz.VETURL.Veterans.and.Military.Servicemembers.tuition.policies.web.address",
		"HDzzzz.ATHURL.Student.Right.to.Know.student.athlete.graduation.rate.web.address",
		"HDzzzz.DISAURL.Disability.Services.Web.Address",
		"HDzzzz.SECTOR.Sector.of.institution",
		"HDzzzz.ICLEVEL.Level.of.institution",
		"HDzzzz.CONTROL.Control.of.institution",
		"HDzzzz.HLOFFER.Highest.level.of.offering",
		"HDzzzz.UGOFFER.Undergraduate.offering",
		"HDzzzz.GROFFER.Graduate.offering",
		"HDzzzz.HDEGOFR1.Highest.degree.offered",
		"HDzzzz.DEGGRANT.Degree.granting.status",
		"HDzzzz.HBCU.Historically.Black.College.or.University",
		"HDzzzz.HOSPITAL.Institution.has.hospital",
		"HDzzzz.MEDICAL.Institution.grants.a.medical.degree",
		"HDzzzz.TRIBAL.Tribal.college",
		"HDzzzz.LOCALE.Degree.of.urbanization..Urban.centric.locale.",
		"HDzzzz.OPENPUBL.Institution.open.to.the.general.public",
		"HDzzzz.ACT.Status.of.institution",
		"HDzzzz.NEWID.UNITID.for.merged.schools",
		"HDzzzz.DEATHYR.Year.institution.was.deleted.from.IPEDS",
		"HDzzzz.CLOSEDAT.Date.institution.closed",
		"HDzzzz.CYACTIVE.Institution.is.active.in.current.year",
		"HDzzzz.INSTCAT.Institutional.category",
		"HDzzzz.C21BASIC.Carnegie.Classification.2021..Basic",
		"HDzzzz.C21IPUG.Carnegie.Classification.2021..Undergraduate.Instructional.Program",
		"HDzzzz.C21IPGRD.Carnegie.Classification.2021..Graduate.Instructional.Program",
		"HDzzzz.C21UGPRF.Carnegie.Classification.2021..Undergraduate.Profile",
		"HDzzzz.C21ENPRF.Carnegie.Classification.2021..Enrollment.Profile",
		"HDzzzz.C21SZSET.Carnegie.Classification.2021..Size.and.Setting",
		"HDzzzz.LANDGRNT.Land.Grant.Institution",
		"HDzzzz.INSTSIZE.Institution.size.category",
		"HDzzzz.F1SYSTYP.Multi.institution.or.multi.campus.organization",
		"HDzzzz.F1SYSNAM.Name.of.multi.institution.or.multi.campus.organization",
		"HDzzzz.F1SYSCOD.Identification.number.of.multi.institution.or.multi.campus.organization",
		"HDzzzz.CBSA.Core.Based.Statistical.Area..CBSA.",
		"HDzzzz.CBSATYPE.CBSA.Type.Metropolitan.or.Micropolitan",
		"HDzzzz.CSA.Combined.Statistical.Area..CSA.",
		"HDzzzz.NECTA.New.England.City.and.Town.Area..NECTA.",
		"HDzzzz.COUNTYCD.Fips.County.code",
		"HDzzzz.COUNTYNM.County.name",
		"HDzzzz.LONGITUD.Longitude.location.of.institution",
		"HDzzzz.LATITUDE.Latitude.location.of.institution",
		"HDzzzz.DFRCGID.Data.Feedback.Report.comparison.group.created.by.NCES",
		"HDzzzz.DFRCUSCG.Data.Feedback.Report...Institution.submitted.a.custom.comparison.group",
		"IPEDS.Year",
		"HDzzzz.DFRCGID.Data.Feedback.Report.comparison.group.category.created.by.NCES",
		"HDzzzz.OBEREG.Geographic.region"
	)) %>% as.data.frame() 
	
	enrollment_undergrad <- tbl(db, "Enrollment_age") %>% dplyr::filter(EFzzzzB.LSTUDY.Level.of.student=="Undergraduate" & EFzzzzB.LINE.Original.line.number.on.survey.form=="Undergraduate, total") %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"IPEDS.Year",
		"EFzzzzB.EFAGE05.Full.time.total",
		"EFzzzzB.EFAGE06.Part.time.total"
	)) %>% dplyr::rename(c(
		Undergrad.full.time = "EFzzzzB.EFAGE05.Full.time.total",
		Undergrad.part.time = "EFzzzzB.EFAGE06.Part.time.total"
	)) %>% as.data.frame()  #  EFzzzzB.EFAGE05.Full.time.total for full time undergraduates


	enrollment_grad <- tbl(db, "Enrollment_age") %>% dplyr::filter(EFzzzzB.LSTUDY.Level.of.student=="Graduate" & EFzzzzB.EFBAGE.Age.category=='All age categories total') %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"IPEDS.Year",
		"EFzzzzB.EFAGE05.Full.time.total",
		"EFzzzzB.EFAGE06.Part.time.total"
	)) %>% dplyr::rename(c(
		GradStudent.full.time = "EFzzzzB.EFAGE05.Full.time.total",
		GradStudent.part.time = "EFzzzzB.EFAGE06.Part.time.total"
	)) %>% as.data.frame() 
	
	comparison_table <- dplyr::left_join(comparison_table, enrollment_undergrad, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	comparison_table <- dplyr::left_join(comparison_table, enrollment_grad, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	
	admissions <- tbl(db, "Admissions") %>% as.data.frame()
	
	comparison_table <- dplyr::left_join(comparison_table, admissions, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	
	enrollment_residence <- tbl(db, "Enrollment_residence") %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"EFzzzzC.EFCSTATE.State.of.residence.when.student.was.first.admitted",
		"EFzzzzC.EFRES01.First.time.degree.certificate.seeking.undergraduate.students",
		"IPEDS.Year"
	)) %>% dplyr::filter(`EFzzzzC.EFCSTATE.State.of.residence.when.student.was.first.admitted` != 'All first-time degree/certificate seeking undergraduates, total') %>% dplyr::filter(`EFzzzzC.EFCSTATE.State.of.residence.when.student.was.first.admitted` != 'z') %>% as.data.frame()
	
	enrollment_residence_wider <- enrollment_residence %>% pivot_wider(id_cols=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"), names_from='EFzzzzC.EFCSTATE.State.of.residence.when.student.was.first.admitted', values_from='EFzzzzC.EFRES01.First.time.degree.certificate.seeking.undergraduate.students') %>% as.data.frame()
	enrollment_residence_wider[is.na(enrollment_residence_wider)] <- 0
	colnames(enrollment_residence_wider)[-c(1:2)] <- paste0("First year students from ", colnames(enrollment_residence_wider)[-c(1:2)])
	
	comparison_table <- dplyr::left_join(comparison_table, enrollment_residence_wider, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))

	# finance
	
	

	finance_fasb <- tbl(db, "Finance_FASB") %>% as.data.frame() 
	colnames(finance_fasb) <- gsub("^[A-z0-9_]+\\.[A-Z0-9]+\\.", "", colnames(finance_fasb))
	finance_fasb <- finance_fasb %>% FixDuplicateColnames() %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"IPEDS.Year",
		"Total.assets",
		"Long.term.investments",
		"Total.liabilities",
		"Total.revenues.and.investment.return",
		"Total.expenses",
		"Pell.grants",
		"Tuition.and.fees...Total",
		"Federal.appropriations...Total",
		"State.appropriations...Total",
		"Local.appropriations...Total",
		"Federal.grants.and.contracts...Total",
		"State.grants.and.contracts...Total",
		"Local.grants.and.contracts...Total",
		"Private.gifts...Total",
		"Private.grants.and.contracts...Total",
		"Instruction.Total.amount",
		"Instruction.Salaries.and.wages",
		"Research.Total.amount",
		"Research.Salaries.and.wages",
		"Public.service.Total.amount",
		"Public.service.Salaries.and.wages",
		"Academic.support.Total.amount",
		"Academic.support.Salaries.and.wages",
		"Student.service.Total.amount",
		"Student.service.Salaries.and.wages",
		"Institutional.support.Total.amount",
		"Institutional.support.Salaries.and.wages",
		"Auxiliary.enterprises.Total.amount",
		"Auxiliary.enterprises.Salaries.and.wages",
		"Net.grant.aid.to.students.Total.amount",
		"Total.expenses.Total.amount"
	)) %>% dplyr::rename(c(
		"Tuition.and.fees"="Tuition.and.fees...Total",
		"Federal.appropriations"="Federal.appropriations...Total",
		"State.appropriations"="State.appropriations...Total",
		"Local.government.appropriations"="Local.appropriations...Total",
		"Federal.grants.and.contracts"="Federal.grants.and.contracts...Total",
		"State.grants.and.contracts"="State.grants.and.contracts...Total",
		"Local.grants.and.contracts"="Local.grants.and.contracts...Total"
	))
	for(col_index in 3:ncol(finance_fasb)){
		finance_fasb[,col_index] <- as.numeric(finance_fasb[,col_index])
	}
	finance_fasb$`Private.gifts..grants..and.contracts` <- as.numeric(finance_fasb$`Private.gifts...Total`) + as.numeric(finance_fasb$`Private.grants.and.contracts...Total`)
	
	finance_gasb <- tbl(db, "Finance_GASB") %>% as.data.frame()
	colnames(finance_gasb) <- gsub("^[A-z0-9_]+\\.[A-Z0-9]+\\.", "", colnames(finance_gasb))
	finance_gasb <- finance_gasb %>% FixDuplicateColnames() %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"IPEDS.Year",
		"Total.assets",
		"Total.revenues.and.other.additions",
		"Total.expenses.and.other.deductions",
		"Pell.grants..federal.",
		"Tuition.and.fees..after.deducting.discounts.and.allowances",
		"Federal.appropriations",
		"State.appropriations",
		"Local.appropriations..education.district.taxes..and.similar.support",
		"Federal.operating.grants.and.contracts",
		"State.operating.grants.and.contracts",
		"Local.private.operating.grants.and.contracts",
		"Federal.nonoperating.grants",
		"State.nonoperating.grants",
		"Local.nonoperating.grants",
		"Gifts..including.contributions.from.affiliated.organizations",
		"Private.operating.grants.and.contracts",
		"Total.all.revenues.and.other.additions",
		"Instruction...Current.year.total",
		"Instruction...Salaries.and.wages",
		"Research...Current.year.total",
		"Research...Salaries.and.wages",
		"Public.service...Current.year.total",
		"Public.service...Salaries.and.wages",
		"Academic.support...Current.year.total",
		"Academic.support...Salaries.and.wages",
		"Student.services...Current.year.total",
		"Student.services...Salaries.and.wages",
		"Institutional.support...Current.year.total",
		"Institutional.support...Salaries.and.wages",
		"Auxiliary.enterprises....Current.year.total",
		"Auxiliary.enterprises....Salaries.and.wages",
		"Scholarships.and.fellowships.expenses....Current.year.total",
		"Total.expenses.and.deductions...Current.year.total"
	)) %>% dplyr::rename(c(
		"Total.expenses"="Total.expenses.and.other.deductions",
		"Pell.grants"="Pell.grants..federal.",
		"Tuition.and.fees"="Tuition.and.fees..after.deducting.discounts.and.allowances",
		"Local.government.appropriations"="Local.appropriations..education.district.taxes..and.similar.support",
		"Total.revenues.and.investment.return"="Total.all.revenues.and.other.additions",
		"Instruction.Total.amount"="Instruction...Current.year.total",
		"Instruction.Salaries.and.wages"="Instruction...Salaries.and.wages",
		"Research.Total.amount"="Research...Current.year.total",
		"Research.Salaries.and.wages"="Research...Salaries.and.wages",
		"Public.service.Total.amount"="Public.service...Current.year.total",
		"Public.service.Salaries.and.wages"="Public.service...Salaries.and.wages",
		"Academic.support.Total.amount"="Academic.support...Current.year.total",
		"Academic.support.Salaries.and.wages"="Academic.support...Salaries.and.wages",
		"Student.service.Total.amount"="Student.services...Current.year.total",
		"Student.service.Salaries.and.wages"="Student.services...Salaries.and.wages",
		"Institutional.support.Total.amount"="Institutional.support...Current.year.total",
		"Institutional.support.Salaries.and.wages"="Institutional.support...Salaries.and.wages",
		"Auxiliary.enterprises.Total.amount"="Auxiliary.enterprises....Current.year.total",
		"Auxiliary.enterprises.Salaries.and.wages"="Auxiliary.enterprises....Salaries.and.wages",
		"Net.grant.aid.to.students.Total.amount"="Scholarships.and.fellowships.expenses....Current.year.total",
		"Total.expenses.Total.amount"="Total.expenses.and.deductions...Current.year.total"
	))
	for(col_index in 3:ncol(finance_gasb)){
		finance_gasb[,col_index] <- as.numeric(finance_gasb[,col_index])
	}
	finance_gasb$"Federal.grants.and.contracts" <- finance_gasb$"Federal.operating.grants.and.contracts" + finance_gasb$"Federal.nonoperating.grants"
	finance_gasb$"State.grants.and.contracts" <- finance_gasb$"State.operating.grants.and.contracts" + finance_gasb$"State.nonoperating.grants"
	finance_gasb$"Local.government.and.contracts" <- finance_gasb$"Local.private.operating.grants.and.contracts" + finance_gasb$"Local.nonoperating.grants"
	finance_gasb$"Private.grants.and.contracts" <- finance_gasb$"Private.operating.grants.and.contracts" + finance_gasb$"Gifts..including.contributions.from.affiliated.organizations"
	
	finance_forprofit <- tbl(db, "Finance_ForProfits") %>% as.data.frame()
	colnames(finance_forprofit) <- gsub("^[A-z0-9_]+\\.[A-Z0-9]+\\.", "", colnames(finance_forprofit))
	finance_forprofit <- finance_forprofit %>% FixDuplicateColnames() %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"IPEDS.Year",
		"Total.assets",
		"Long.term.investments",
		"Total.liabilities",
		"Total.revenues.and.investment.return",
		"Total.expenses",
		"Pell.grants",
		"Tuition.and.fees",
		"Federal.appropriations",
		"State.appropriations",
		"Local.government.appropriations",
		"Federal.grants.and.contracts",
		"State.grants.and.contracts",
		"Local.government.and.contracts",
		"Private.gifts..grants..and.contracts",
		"Instruction.Total.amount",
		"Instruction.Salaries.and.wages",
		"Research.Total.amount",
		"Research.Salaries.and.wages",
		"Public.service.Total.amount",
		"Public.service.Salaries.and.wages",
		"Academic.support.Total.amount",
		"Academic.support.Salaries.and.wages",
		"Student.service.Total.amount",
		"Student.service.Salaries.and.wages",
		"Institutional.support.Total.amount",
		"Institutional.support.Salaries.and.wages",
		"Auxiliary.enterprises.Total.amount",
		"Auxiliary.enterprises.Salaries.and.wages",
		"Net.grant.aid.to.students.Total.amount",
		"Total.expenses.Total.amount"
	))
	for(col_index in 3:ncol(finance_forprofit)){
		finance_forprofit[,col_index] <- as.numeric(finance_forprofit[,col_index])
	}
	
	
	finance <- dplyr::bind_rows(FixDuplicateColnames(finance_fasb), FixDuplicateColnames(finance_gasb), FixDuplicateColnames(finance_forprofit))
	
	comparison_table <- dplyr::left_join(comparison_table, finance, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))

	student_aid_core <- tbl(db, "Student_aid") %>% dplyr::select(contains(c(
		"UNITID.Unique.identification.number.of.the.institution",
		"IPEDS.Year",
		"Percent.of.full.time.first.time.undergraduates.awarded.any.financial.aid",
		"Percent.of.full.time.first.time.undergraduates.awarded.any.loans.to.students.or.grant.aid..from.federal.state.local.government.or.the.institution",
		"Percent.of.full.time.first.time.undergraduates.awarded.federal..state..local.or.institutional.grant.aid",
		"Average.amount.of.federal..state..local.or.institutional.grant.aid.awarded",
		"Percent.of.full.time.first.time.undergraduates.awarded.federal.grant.aid",
		"Average.amount.of.federal.grant.aid.awarded.to.full.time.first.time.undergraduates",
		"Percent.of.full.time.first.time.undergraduates.awarded.Pell.grants",
		"Average.amount.of.Pell.grant.aid.awarded.to.full.time.first.time.undergraduates",
		"Percent.of.full.time.first.time.undergraduates.awarded.other.federal.grant.aid",
		"Average.amount.of.other.federal.grant.aid.awarded.to.full.time.first.time.undergraduates",
		"Percent.of.full.time.first.time.undergraduates.awarded.state.local.grant.aid",
		"Average.amount.of.state.local.grant.aid.awarded.to.full.time.first.time.undergraduates",
		"Percent.of.full.time.first.time.undergraduates.awarded.institutional.grant.aid",
		"Average.amount.of.institutional.grant.aid.awarded.to.full.time.first.time.undergraduates",
		"Percent.of.full.time.first.time.undergraduates.awarded.student.loans",
		"Average.amount.of.student.loans.awarded.to.full.time.first.time.undergraduates",
		"Percent.of.full.time.first.time.undergraduates.awarded.federal.student.loans",
		"Average.amount.of.federal.student.loans.awarded.to.full.time.first.time.undergraduates",
		"Percent.of.full.time.first.time.undergraduates.awarded.other.student.loans",
		"Total.amount.of.other.student.loans.awarded.to.full.time.first.time.undergraduates",
		"Average.amount.of.other.student.loans.awarded.to.full.time.first.time.undergraduates"
		))) %>% as.data.frame()

	for(col_index in 3:ncol(student_aid_core)){
		student_aid_core[,col_index] <- as.numeric(student_aid_core[,col_index])
	}
	
	comparison_table <- dplyr::left_join(comparison_table, student_aid_core, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	
	student_aid_messy_years <- tbl(db, "Student_aid") %>% dplyr::select(c(
		"UNITID.Unique.identification.number.of.the.institution",
		contains("Average.net.price.students.awarded.grant.or.scholarship.aid")
		)) %>% as.data.frame() 
	
	colnames(student_aid_messy_years) <-  gsub("\\.[0-9]+$", "", gsub("Average.net.price.students.awarded.grant.or.scholarship.aid..", "", gsub("^[A-z]+\\.[A-Z0-9_]+\\.", "", colnames(student_aid_messy_years))))
	
	#student_aid_messy_years <- student_aid_messy_years %>% FixDuplicateColnames()	
	
	student_aid_messy_pivoted <- student_aid_messy_years %>% tidyr::pivot_longer(cols = -UNITID.Unique.identification.number.of.the.institution, names_to = "IPEDS.Year", values_to = "Average.net.price.students.awarded.grant.or.scholarship.aid")
	student_aid_messy_pivoted <- student_aid_messy_pivoted %>% filter(!is.na(Average.net.price.students.awarded.grant.or.scholarship.aid)) %>% filter(UNITID.Unique.identification.number.of.the.institution!="z")
	student_aid_messy_pivoted$Average.net.price.students.awarded.grant.or.scholarship.aid <- as.numeric(student_aid_messy_pivoted$Average.net.price.students.awarded.grant.or.scholarship.aid)
	student_aid_messy_pivoted <- student_aid_messy_pivoted %>% group_by(UNITID.Unique.identification.number.of.the.institution, IPEDS.Year) %>% summarise(Average.net.price.students.awarded.grant.or.scholarship.aid = mean(Average.net.price.students.awarded.grant.or.scholarship.aid))
	student_aid_messy_pivoted <- student_aid_messy_pivoted %>% group_by(UNITID.Unique.identification.number.of.the.institution, IPEDS.Year) %>% summarise(Average.net.price.students.awarded.grant.or.scholarship.aid = mean(Average.net.price.students.awarded.grant.or.scholarship.aid))
	
	comparison_table <- dplyr::left_join(comparison_table, student_aid_messy_pivoted, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	
	academic_library <- tbl(db, "Academic_library") %>% as.data.frame()
	
	comparison_table <- dplyr::left_join(comparison_table, academic_library, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))

	# Graduation rate 4 year institutions
	
	graduation_rate_four <- tbl(db, "Graduation_FourYears") %>% as.data.frame()
	graduation_rate_four <- graduation_rate_four[grepl("Bachelor's or equiv subcohort \\(4-yr institution\\)", graduation_rate_four$GRzzzz.GRTYPE.Cohort.data),]
	
	graduation_rate_four <- graduation_rate_four %>% dplyr::select(-ends_with('..old')) %>% dplyr::select(-ends_with('..new')) %>% dplyr::select(-ends_with('..derived')) %>% dplyr::select(-ends_with('ORIGINAL.CODES'))
	
	graduation_rate_four_cleaned <- data.frame()
	
	for(UNITID in unique(graduation_rate_four$UNITID.Unique.identification.number.of.the.institution)){
		graduation_local <- graduation_rate_four %>% filter(UNITID.Unique.identification.number.of.the.institution==UNITID)
		for(year in unique(graduation_local$IPEDS.Year)) {
			
			graduation_local_year <- graduation_local %>% filter(IPEDS.Year==year)	
			
			first_col_to_include <- which(colnames(graduation_local_year)=="GRzzzz.GRTOTLT.Grand.total")
			
			last_col_to_include <- -1 + which(colnames(graduation_local_year)=="IPEDS.Year")
			
			enrollments <- as.numeric(graduation_local_year[graduation_local_year$GRzzzz.CHRTSTAT.Graduation.rate.status.in.cohort=='Adjusted cohort (revised cohort minus exclusions)',])
			
			completions150 <- 100*as.numeric(graduation_local_year[graduation_local_year$GRzzzz.CHRTSTAT.Graduation.rate.status.in.cohort=='Completers within 150% of normal time',])/enrollments
			names(completions150) <- paste0("Graduation Rate Bachelors in within 150 percent of normal time ", gsub("^[A-z]+\\.[A-Z0-9]+\\.", "", colnames(graduation_local_year)))
			
			completions4yr <- 100*as.numeric(graduation_local_year[graduation_local_year$GRzzzz.CHRTSTAT.Graduation.rate.status.in.cohort=="Completers of bachelor's or equivalent degrees in 4 years or less",])/enrollments
			names(completions4yr) <- paste0("Graduation Rate Bachelors in 4 years or less ", gsub("^[A-z]+\\.[A-Z0-9]+\\.", "", colnames(graduation_local_year)))
			
			
			transfers <- 100*as.numeric(graduation_local_year[graduation_local_year$GRzzzz.CHRTSTAT.Graduation.rate.status.in.cohort=='Transfer-out students',])/enrollments
			names(transfers) <- paste0("Graduation Rate Transfer out of bachelors ", gsub("^[A-z]+\\.[A-Z0-9]+\\.", "", colnames(graduation_local_year)))
			
			focal_row <- c(UNITID.Unique.identification.number.of.the.institution=UNITID, IPEDS.Year=year, completions150[first_col_to_include:last_col_to_include], completions4yr[first_col_to_include:last_col_to_include], transfers[first_col_to_include:last_col_to_include])
			
			graduation_rate_four_cleaned <- dplyr::bind_rows(graduation_rate_four_cleaned, as.data.frame(t(focal_row)))

			
		}
	}
		
	comparison_table <- dplyr::left_join(comparison_table, graduation_rate_four_cleaned, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))

	
	

	# Instructional staff

	staff_tenure_demographics <- tbl(db, "Staff_gender") %>% as.data.frame()
	colnames(staff_tenure_demographics) <- gsub("^[A-z]+\\.[A-Z0-9]+\\.", "", colnames(staff_tenure_demographics))
	


	tenure_stream_current <- staff_tenure_demographics %>% filter(Instructional.staff.category %in% c('Tenured total', 'On-Tenure track total')) %>% filter(Academic.rank=="All ranks") %>% group_by(IPEDS.Year, UNITID.Unique.identification.number.of.the.institution) %>% summarise(Grand.total = sum(as.numeric(Grand.total)), Grand.total.women = sum(as.numeric(Grand.total.women)), Grand.total.men=sum(as.numeric(Grand.total.men)), American.Indian.or.Alaska.Native=sum(as.numeric(American.Indian.or.Alaska.Native.total)), Asian=sum(as.numeric(Asian.total)), Black.or.African.American=sum(as.numeric(Black.or.African.American.total)), Hispanic.or.Latino=sum(as.numeric(Hispanic.or.Latino.total)), Native.Hawaiian.or.Other.Pacific.Islander=sum(as.numeric(Native.Hawaiian.or.Other.Pacific.Islander.total)), White=sum(as.numeric(White.total)), Two.or.more.races=sum(as.numeric(Two.or.more.races.total)), Race.ethnicity.unknown=sum(as.numeric(Race.ethnicity.unknown.total)), Nonresident.alien=sum(as.numeric(Nonresident.alien.total))) 
	
	colnames(tenure_stream_current)[-c(1,2)] <- paste0("Tenure-stream.", colnames(tenure_stream_current)[-c(1,2)])

	comparison_table <- dplyr::left_join(comparison_table, tenure_stream_current, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))

	ntt_stream_current <- staff_tenure_demographics %>% filter(Instructional.staff.category=='Not on tenure track/No tenure system system total') %>% filter(Academic.rank=="All ranks") %>% group_by(IPEDS.Year, UNITID.Unique.identification.number.of.the.institution) %>% summarise(Grand.total = sum(as.numeric(Grand.total)), Grand.total.women = sum(as.numeric(Grand.total.women)), Grand.total.men=sum(as.numeric(Grand.total.men)), American.Indian.or.Alaska.Native=sum(as.numeric(American.Indian.or.Alaska.Native.total)), Asian=sum(as.numeric(Asian.total)), Black.or.African.American=sum(as.numeric(Black.or.African.American.total)), Hispanic.or.Latino=sum(as.numeric(Hispanic.or.Latino.total)), Native.Hawaiian.or.Other.Pacific.Islander=sum(as.numeric(Native.Hawaiian.or.Other.Pacific.Islander.total)), White=sum(as.numeric(White.total)), Two.or.more.races=sum(as.numeric(Two.or.more.races.total)), Race.ethnicity.unknown=sum(as.numeric(Race.ethnicity.unknown.total)), Nonresident.alien=sum(as.numeric(Nonresident.alien.total))) 
	
	colnames(ntt_stream_current)[-c(1,2)] <- paste0("NTT-stream.", colnames(ntt_stream_current)[-c(1,2)])
	
	comparison_table <- dplyr::left_join(comparison_table, ntt_stream_current, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))


	# Institutional offerings
	
	institutional_offerings <- tbl(db, "Institutional_offerings") %>% dplyr::select(!contains("Enrolled")) %>% dplyr::select(!contains("Admissions")) %>% dplyr::select(!contains("Applicants")) %>% as.data.frame() %>% FixDuplicateColnames()
	
	comparison_table <- dplyr::left_join(comparison_table, institutional_offerings, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	
	
	# Student demographics freshmen
	
	enrollment_race_fulltime_freshmen <- tbl(db, "Enrollment_race") %>% dplyr::filter(EFzzzzA.EFALEVEL.Level.of.student=="Full-time students, Undergraduate, Degree/certificate-seeking, First-time") %>% dplyr::select(!contains("Level")) %>% dplyr::select(!contains("Attendance")) %>% as.data.frame()
	
	colnames(enrollment_race_fulltime_freshmen)[grepl("EFzzzz", colnames(enrollment_race_fulltime_freshmen))] <- paste0("FirstYearFullTimeDegreeSeekingUndergrads.", colnames(enrollment_race_fulltime_freshmen)[grepl("EFzzzz", colnames(enrollment_race_fulltime_freshmen))])
	
	colnames(enrollment_race_fulltime_freshmen) <- gsub("EF[A-z]+\\.[A-Z0-9_]+\\.", "", colnames(enrollment_race_fulltime_freshmen))

	comparison_table <- dplyr::left_join(comparison_table, enrollment_race_fulltime_freshmen, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	
	enrollment_race_fulltime_freshmen_percent <- enrollment_race_fulltime_freshmen
	focal_columns <- which(grepl('FirstYearFullTimeDegreeSeekingUndergrads', colnames(enrollment_race_fulltime_freshmen_percent)))
	for (focal_index in focal_columns) {
		enrollment_race_fulltime_freshmen_percent[,focal_index] <- 100*as.numeric(enrollment_race_fulltime_freshmen[,focal_index])/as.numeric(enrollment_race_fulltime_freshmen$'FirstYearFullTimeDegreeSeekingUndergrads.Grand.total')
		colnames(enrollment_race_fulltime_freshmen_percent)[focal_index] <- paste0(colnames(enrollment_race_fulltime_freshmen_percent)[focal_index], ".percent")
	}
	enrollment_race_fulltime_freshmen_percent <- enrollment_race_fulltime_freshmen_percent %>% dplyr::select(-ends_with("Grand.total"))
	
	comparison_table <- dplyr::left_join(comparison_table, enrollment_race_fulltime_freshmen_percent, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))

	
	# Student demographics all undergrads. Includes non degree seeking students, part time students, etc.

	enrollment_race_undergrads <- tbl(db, "Enrollment_race") %>% dplyr::filter(EFzzzzA.EFALEVEL.Level.of.student=="All students, Undergraduate total") %>% dplyr::select(!contains("Level")) %>% dplyr::select(!contains("Attendance")) %>% as.data.frame()
	
	colnames(enrollment_race_undergrads)[grepl("EFzzzz", colnames(enrollment_race_undergrads))] <- paste0("AllUndergrads.", colnames(enrollment_race_undergrads)[grepl("EFzzzz", colnames(enrollment_race_undergrads))])
	
	colnames(enrollment_race_undergrads) <- gsub("EF[A-z]+\\.[A-Z0-9_]+\\.", "", colnames(enrollment_race_undergrads))
	
	comparison_table <- dplyr::left_join(comparison_table, enrollment_race_undergrads, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))

	# Student demographics all grad students. Includes non degree seeking students, part time students, etc.

	enrollment_race_grad_students <- tbl(db, "Enrollment_race") %>% dplyr::filter(EFzzzzA.EFALEVEL.Level.of.student=="All students, Graduate") %>% dplyr::select(!contains("Level")) %>% dplyr::select(!contains("Attendance")) %>% as.data.frame()
	
	colnames(enrollment_race_grad_students)[grepl("EFzzzz", colnames(enrollment_race_grad_students))] <- paste0("AllGradStudents.", colnames(enrollment_race_grad_students)[grepl("EFzzzz", colnames(enrollment_race_grad_students))])
	
	colnames(enrollment_race_grad_students) <- gsub("EF[A-z]+\\.[A-Z0-9_]+\\.", "", colnames(enrollment_race_grad_students))
	
	comparison_table <- dplyr::left_join(comparison_table, enrollment_race_grad_students, by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))


	# Wrapup steps
	
	colnames(comparison_table) <- gsub("  ", " ", gsub('\\.', ' ', gsub("^[A-z]+\\.[A-Z0-9_]+\\.", "", colnames(comparison_table))))

	comparison_table <- subset(comparison_table, comparison_table$`UNITID Unique identification number of the institution` != "z") #kill the placeholder
	
	comparison_table <- comparison_table %>% rename(State = "State abbreviation")  #since it's no longer an abbreviation

	
	comparison_table$`Admission percentage total` <- 100*as.numeric(comparison_table$`Admissions total`)/as.numeric(comparison_table$`Applicants total`)
	comparison_table$`Admission percentage women` <- 100*as.numeric(comparison_table$`Admissions women`)/as.numeric(comparison_table$`Applicants women`)
	comparison_table$`Admission percentage men` <- 100*as.numeric(comparison_table$`Admissions men`)/as.numeric(comparison_table$`Applicants men`)

	comparison_table$`Yield percentage total` <- 100*as.numeric(comparison_table$`Enrolled total`)/as.numeric(comparison_table$`Admissions total`)
	comparison_table$`Yield percentage women` <- 100*as.numeric(comparison_table$`Enrolled women`)/as.numeric(comparison_table$`Admissions women`) 
	comparison_table$`Yield percentage men` <- 100*as.numeric(comparison_table$`Enrolled men`)/as.numeric(comparison_table$`Admissions men`)
	

	
	comparison_table$`Percent of revenue from tuition and fees` <- 100 * as.numeric(comparison_table$`Tuition and fees`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Percent of revenue from federal appropriations` <- 100 * as.numeric(comparison_table$`Federal appropriations`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Percent of revenue from state appropriations` <- 100 * as.numeric(comparison_table$`State appropriations`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Percent of revenue from local appropriations` <- 100 * as.numeric(comparison_table$`Local government appropriations`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Percent of revenue from government appropriations` <- comparison_table$`Percent of revenue from federal appropriations` + comparison_table$`Percent of revenue from state appropriations` + comparison_table$`Percent of revenue from local appropriations`
	
	comparison_table$`Percent of revenue from federal grants and contracts` <- 100 * as.numeric(comparison_table$`Federal grants and contracts`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Percent of revenue from state grants and contracts` <- 100 * as.numeric(comparison_table$`State grants and contracts`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Percent of revenue from local grants and contracts` <- 100 * as.numeric(comparison_table$`Local grants and contracts`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Percent of revenue from government grants and contracts` <- comparison_table$`Percent of revenue from federal grants and contracts` + comparison_table$`Percent of revenue from state grants and contracts` + comparison_table$`Percent of revenue from local grants and contracts`
	
	comparison_table$`Percent of revenue from private gifts grants and contracts` <- 100 * as.numeric(comparison_table$`Private gifts grants and contracts`)/as.numeric(comparison_table$`Total revenues and investment return`)
	
	comparison_table$`Revenue minus expenses` <- as.numeric(comparison_table$`Total revenues and investment return`)-as.numeric(comparison_table$`Total expenses`)
	
	comparison_table$`Full time undergrad enrollment divided by dorm capacity` <- as.numeric(comparison_table$`Undergrad full time`)/as.numeric(comparison_table$`Total dormitory capacity`)
	
	comparison_table$`Total instructors` <- ifelse(is.na(as.numeric(comparison_table$`NTT-stream Grand total`)),0, as.numeric(comparison_table$`NTT-stream Grand total`) ) + ifelse(is.na(as.numeric(comparison_table$`Tenure-stream Grand total`)),0, as.numeric(comparison_table$`Tenure-stream Grand total`) ) # So if it has no tenure stream faculty and 500 NTT, we don't add NA + 500 and get NA
	comparison_table$`Total instructors`[comparison_table$`Total instructors`==0] <- NA # If no data, make it NA
	
	comparison_table$`Full time undergrad enrollment divided by total instructors` <- as.numeric(comparison_table$`Undergrad full time`)/as.numeric(comparison_table$`Total instructors`)
	
	comparison_table$`Full time undergrad enrollment divided by tenure stream faculty` <- as.numeric(comparison_table$`Undergrad full time`) / as.numeric(comparison_table$`Tenure-stream Grand total`)
		
	comparison_table$`Physical library circulations per students and faculty` <- as.numeric(comparison_table$`Total physical library circulations books and media `)/(as.numeric(comparison_table$`Undergrad full time`)+as.numeric(comparison_table$`Undergrad part time`)+as.numeric(comparison_table$`GradStudent full time`)+as.numeric(comparison_table$`GradStudent part time`)+as.numeric(comparison_table$`Total instructors`))
	
	comparison_table$`Digital library circulations per students and faculty` <- as.numeric(comparison_table$`Total digital electronic circulations books and media `)/(as.numeric(comparison_table$`Undergrad full time`)+as.numeric(comparison_table$`Undergrad part time`)+as.numeric(comparison_table$`GradStudent full time`)+as.numeric(comparison_table$`GradStudent part time`)+as.numeric(comparison_table$`Total instructors`))
	
	comparison_table$`Disability percentage` <- NA
	comparison_table$`Disability percentage`[comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`=="3 percent or less" & !is.na(comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`)] <- "≤3"
	comparison_table$`Disability percentage`[comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`=="More than 3 percent" & !is.na(comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`)] <- comparison_table$`Percent of undergraduates who are formally registered as students with disabilities when percentage is more than 3 percent`[comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`=="More than 3 percent" & !is.na(comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`)]
	
	comparison_table$`Disability percentage numeric` <- NA
		
	comparison_table$`Disability percentage numeric`[comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`=="3 percent or less" & !is.na(comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`)] <- 3

	comparison_table$`Disability percentage numeric`[comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`=="More than 3 percent" & !is.na(comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`)] <- comparison_table$`Percent of undergraduates who are formally registered as students with disabilities when percentage is more than 3 percent`[comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`=="More than 3 percent" & !is.na(comparison_table$`Percent indicator of undergraduates formally registered as students with disabilities`)]

	
	
	dbWriteTable(db,  "comparison_table", comparison_table, overwrite=TRUE)

	
	dbDisconnect(db)
	return(comparison_table)
}



AggregateDirectIPEDSDirect <- function(ipeds_directly, IPEDS_names = GetIPEDSNames(), remove=TRUE) {
	if(remove) {
		try(file.remove("data/db_IPEDS.sqlite"))
	}
	db <- dbConnect(RSQLite::SQLite(), "data/db_IPEDS.sqlite")
	ipeds_directly_normal <- ipeds_directly[grepl("normal", ipeds_directly)]
	ipeds_directly_wide <- ipeds_directly[grepl("wide", ipeds_directly)]
	working_files <- c()
	for (ipeds_base_index in seq_along(IPEDS_names)) {
		ipeds_directly_normal_base <- ipeds_directly_normal[grepl(paste0(IPEDS_names[ipeds_base_index], "_normal"), ipeds_directly_normal)]
		ipeds_directly_wide_base <- ipeds_directly_wide[grepl(paste0(IPEDS_names[ipeds_base_index], "_wide"), ipeds_directly_wide)]
		ipeds_all_base <- c(ipeds_directly_wide_base, ipeds_directly_normal_base)
		local_df <- data.frame()
		print(paste0(IPEDS_names[ipeds_base_index]))
		
		for (ipeds_directly_all_index in seq_along(ipeds_all_base)) {
			try({
				print(ipeds_all_base[ipeds_directly_all_index])
				#local_data <- arrow::read_csv_arrow(ipeds_directly_normal[ipeds_directly_all_index],  col_names=TRUE)
				local_data <- read.csv(ipeds_all_base[ipeds_directly_all_index], stringsAsFactors=FALSE)


				if(ipeds_directly_all_index==1) {
					local_df <- local_data
				} else {

					#local_df <- full_join(local_df, local_data)
					local_df <- dplyr::bind_rows(local_df, local_data)
				}
				gc()
				print(dim(local_df))
				print(object.size(local_df), units="Mb")
				#print(tail(colnames(local_df)))
				#arrow::write_dataset(ipeds_merged, path="data", format="csv", existing_data_behavior="overwrite")
				file_output <- paste0("data/MERGED_", names(IPEDS_names)[ipeds_base_index], ".csv")
				write.csv(local_df, file=file_output, row.names=FALSE)
				working_files <- c(working_files, file_output)
			})
		}
		if(nrow(local_df)>0) {
			print(paste0("Now trying to load into database ", names(IPEDS_names)[ipeds_base_index]))
			 if(ncol(local_df)>1000) {
			 	print("Too many columns, trying to merge some")
				if(names(IPEDS_names)[ipeds_base_index]=="Student_aid") {
					local_df <- local_df %>% dplyr::select(-contains("income"))
				}
				if(ncol(local_df)>1000) {

					colnames(local_df) <- colnames(local_df) <- gsub("^[A-z0-9]+\\.[A-Z0-9_]+\\.", "", colnames(local_df))

					local_df <- local_df %>% FixDuplicateColnames()
					if(ncol(local_df)>1000) {
						print("Still many columns, taking the first 1000")

			 		#NA_counts <- sort(colSums(is.na(local_df)), decreasing=FALSE)
			 		#local_df <- local_df[,colSums(is.na(local_df))<=NA_counts[1000]]
					local_df <- local_df[,1:1000]
					}
				}
			 }
			try({
				dbWriteTable(db,  names(IPEDS_names)[ipeds_base_index], local_df, overwrite=TRUE)
			})
		} else {
			print(paste0("Failed to load into database ", names(IPEDS_names)[ipeds_base_index]))
		}
	}
	dbDisconnect(db)
	return(unique(working_files))
}

FixDuplicateColnames <- function(df) {
	colnames(df) <- gsub(colnames(df), pattern="\\.[0-9]+$", replacement="")
	df_duplicated <- colnames(df)[duplicated(colnames(df))]
	while(length(df_duplicated)>0) {
		for (bad_col in df_duplicated) {
			indices <- which(colnames(df)==bad_col)
			if(length(indices)>1) {
				for (i in sequence(nrow(df))) {
					if(is.na(df[i,indices[1]])) {
						df[i,indices[1]] <- df[i,indices[2]]
					}
				}
				df <- df[,-indices[2]]
			}
		}
		df_duplicated <- colnames(df)[duplicated(colnames(df))]	
	}
	return(FixTooManyColumns(df))
}

FixTooManyColumns <- function(local_df) {
	threshold <- 1000
	while(ncol(local_df)>1000) {
		NA_counts <- sort(colSums(is.na(local_df)), decreasing=FALSE)
		local_df <- local_df[,colSums(is.na(local_df))<=NA_counts[threshold]]
		threshold <- threshold - 1
	}
	return(local_df)	
}




AggregateFromDB <- function(ipeds_direct_and_db) {
	db <- dbConnect(RSQLite::SQLite(), "data/db_IPEDS.sqlite")
	#together <- inner_join(tbl(db, 'Institutional_directory'), tbl(db, 'Institutional_offerings'))
	institution_ids <- unique(pull(tbl(db, 'Institutional_directory'), 'UNITID.Unique.identification.number.of.the.institution'))
	for (institution_id in institution_ids) {
		individual_result <- AggregateForOneInstitution(institution_id, db)
	}
}

AggregateIPEDS <- function() {
	raw_data <- mutate_all(read_csv("data/ipeds_Data_7-15-2022---124.csv", col_types="c"), as.character)
	traits <- mutate_all(read_csv("data/ipeds_ValueLabels_7-15-2022---124.csv", col_types="c"), as.character)
	possible_colnames <- unique(traits$VariableName)
	for (col_index in seq_along(possible_colnames)) {
		if(!grepl("fips|abbreviation", possible_colnames[col_index], ignore.case=TRUE)) {
			matching_col <- which(colnames(raw_data) == possible_colnames[col_index])
			trait_subset <- traits[traits$VariableName == possible_colnames[col_index],]
			for (value_index in sequence(nrow(trait_subset))) {
				ones_to_change <- which(as.character(pull(raw_data, matching_col)) == as.character(trait_subset$Value[value_index]))
				raw_data[ones_to_change,matching_col] <- as.character(trait_subset$ValueLabel[value_index])
			}
		}
	}
	raw_data <- dplyr::select(raw_data, -'...253')

	raw_data2 <- mutate_all(read_csv("data/ipeds_Data_7-24-2022---404.csv", col_types="c"), as.character)
	traits <- mutate_all(read_csv("data/ipeds_ValueLabels_7-24-2022---404.csv", col_types="c"), as.character)
	possible_colnames <- unique(traits$VariableName)
	for (col_index in seq_along(possible_colnames)) {
		if(!grepl("fips|abbreviation", possible_colnames[col_index], ignore.case=TRUE)) {
			matching_col <- which(colnames(raw_data2) == possible_colnames[col_index])
			trait_subset <- traits[traits$VariableName == possible_colnames[col_index],]
			for (value_index in sequence(nrow(trait_subset))) {
				ones_to_change <- which(as.character(pull(raw_data2, matching_col)) == as.character(trait_subset$Value[value_index]))
				raw_data2[ones_to_change,matching_col] <- as.character(trait_subset$ValueLabel[value_index])
			}
		}
	}
	raw_data2 <- dplyr::select(raw_data2, -'...242')
	raw_data_joined <- full_join(raw_data, raw_data2, by="Institution Name") %>% dplyr::left_join(distinct(mutate_all(read_csv("data/Data_7-24-2022_NTT.csv", col_types="c"), as.character)), by="Institution Name") %>% dplyr::left_join(distinct(mutate_all(read_csv("data/Data_7-24-2022_tenured.csv", col_types="c"), as.character)), by="Institution Name") %>% dplyr::left_join(distinct(mutate_all(read_csv("data/Data_7-24-2022_TTnontenured.csv", col_types="c"), as.character)), by="Institution Name") %>%rename_at(
		vars(ends_with(".x")),
		~str_replace(., "\\..$","")
		) %>% 
		dplyr::select_at(
		vars(-ends_with(".y"))
		)
		
	raw_data3 <- mutate_all(read_csv("data/ipeds_Data_8-14-2022_price_equity.csv", col_types="c"), as.character)
	raw_data3 <- dplyr::select(raw_data3, -'Institution Name')
	raw_data_joined <- full_join(raw_data_joined, raw_data3, by="UnitID")
	
	# Getting data for transfer and student loans
	raw_data4 <- mutate_all(read_csv("data/ipeds_Data_8-23-2022.csv", col_types="c"), as.character)
	raw_data4 <- dplyr::select(raw_data4, -'Institution Name')
	raw_data_joined <- full_join(raw_data_joined, raw_data4, by="UnitID")
	
	
	raw_data <- raw_data_joined[!duplicated(raw_data_joined$`Institution Name`),]
	raw_data$`Institution Name` <- gsub('/', '_', raw_data$`Institution Name`)
	raw_data$"Undergraduate enrollment (DRVEF2019_RV)" <- as.numeric(raw_data$"Undergraduate enrollment (DRVEF2019_RV)")
	raw_data <- subset(raw_data, !is.na(raw_data$"Undergraduate enrollment (DRVEF2019_RV)"))
	raw_data <- subset(raw_data, raw_data$"Undergraduate enrollment (DRVEF2019_RV)">0)
	#raw_data <- raw_data_joined

	
	load("data/epa_walkability.rda")	
	walkability$D4A[walkability$D4A == -99999] <- NA
	walkability_aggregated <- walkability %>% group_by(CBSA_Name) %>% summarise(NatWalkInd=round(median(NatWalkInd),1), Population=sum(TotPop), HousingUnits = sum(CountHU, na.rm=TRUE), DistanceToTransit = round(min(D4A, na.rm=TRUE)))
	raw_data <- left_join(raw_data, walkability_aggregated, by=c("Core Based Statistical Area (CBSA) (HD2019)" = "CBSA_Name"))
	rm(walkability)
	banned_states <- c("AL", "AR", "FL", "ID", "IN", "IA", "KS", "KY", "MS", "MT", "NC", "ND", "OH", "OK", "SC", "SD", "TN", "TX", "UT", "WV")
	raw_data$BannedCATravel <- "No"
	raw_data$BannedCATravel[pull(raw_data, `State abbreviation (HD2019)`) %in% banned_states] <- "Yes"
	raw_data <- CleanNames(raw_data)
	raw_data$NumberOfPhysicalBooksPerUndergrad <- round(as.numeric(pull(raw_data, "Number of physical books"))/as.numeric(pull(raw_data, "Undergraduate enrollment")))
	raw_data$NumberOfDigitalBooksPerUndergrad <- round(as.numeric(pull( raw_data,  "Number of digital/electronic books" ))/as.numeric(pull(raw_data, "Undergraduate enrollment")))
	#raw_data$TenureTrackFacultyCount <- as.numeric(pull(raw_data, "All ranks (All full-time instructional staff)")) - ((as.numeric(pull(raw_data, "Professors (All full-time instructional staff)" ))) + (as.numeric(pull(raw_data, "Associate professors (All full-time instructional staff)" ))) + (as.numeric(pull(raw_data, "Assistant professors (All full-time instructional staff)" ))))
	raw_data$TenureTrackFacultyCount  <- NA # NEED to compute this properly; look at Iowa State U as an example
	raw_data$PercentageOfTenureTrackInstructors <- round(100*as.numeric(pull(raw_data, "TenureTrackFacultyCount")/as.numeric(pull(raw_data, "All ranks (All full-time instructional staff)"))))
	raw_data$StudentToTenureTrackRatio <- round(as.numeric(pull(raw_data, "Undergraduate enrollment"))/as.numeric(pull(raw_data, "TenureTrackFacultyCount")),1)
	raw_data$CollegeType <- paste0(pull(raw_data, "Carnegie Classification 2018: Basic"))
	# for (i in sequence(nrow(raw_data))) {
	# 	if(raw_data$"Historically Black College or University"[i] == "Yes") {
	# 		raw_data$CollegeType[i] <- paste0(raw_data$CollegeType[i], "; Historically Black College or University (HBCU)")
	# 	}	
	# 	if(raw_data$"Land Grant Institution"[i] == "Land Grant Institution") {
	# 		raw_data$CollegeType[i] <- paste0(raw_data$CollegeType[i], "; Land Grant Institution")
	# 	}	
	# }
	undergrad_cols <- which(grepl("First-time degree/certificate-seeking undergraduate students", colnames(raw_data), ignore.case=TRUE)) 
	colnames(raw_data)[undergrad_cols] <- paste0("Count of first-time undergrads from ", gsub("First-time degree/certificate-seeking undergraduate students \\(EF2019C_RV  ", "", gsub("\\)","",colnames(raw_data)[undergrad_cols])))
	pops_by_state <- GetPopulationByStateAtAge18()
	for (i in sequence(nrow(pops_by_state))) {
		raw_data[, paste0("Percentage of all 18 year olds in the focal state enrolling in this college from ", pops_by_state$State[i])] <- 
		100*as.numeric(pull(raw_data, paste0("Count of first-time undergrads from ", pops_by_state$State[i])))/as.numeric(pops_by_state$Population_age_18[i])
	}
	for(i in sequence(nrow(raw_data))) { ## For the colleges who don't understand URLs.
		if(!grepl("http", raw_data$`Mission statement URL (if mission statement not provided) (IC2019mission)`[i])) {
			raw_data$`Mission statement URL (if mission statement not provided) (IC2019mission)`[i] <- paste0("http://", raw_data$`Mission statement URL (if mission statement not provided) (IC2019mission)`[i])
		}
	}
	raw_data$`Institution Name` <- gsub("The ", "", raw_data$`Institution Name`) # Sorry, THE Ohio State, but this will make it easier for people to search
	raw_data <- rename(raw_data, latitude="Latitude location of institution", longitude="Longitude location of institution")
	raw_data$latitude <- as.numeric(raw_data$latitude)
	raw_data$longitude <- as.numeric(raw_data$longitude)
	return(raw_data)
}

# From https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html
GetPopulationByStateAtAge18 <- function() {
	pops <- read.csv("data/sc-est2019-agesex-civ.csv")
	pops <- subset(pops, SEX==0 & AGE==18) # all sexes. only age 18
	pops <- dplyr::select(pops, c("STATE", "NAME", "POPEST2019_CIV"))
	colnames(pops) <- c("State_FIPS", "State", "Population_age_18")
	pops <- subset(pops, State_FIPS>0)
	return(pops)
}

CleanNames <- function(college_data) {
	RemoveHD <- function(x) {
		return(gsub(" \\(DRVAL2019_RV\\)", "", gsub(" \\(DRVGR2019_RV\\)", "", gsub("S2019_SIS_RV  ", "", gsub(" \\(IC2019_RV\\)", "", gsub(" \\(DRVF2019_RV\\)", "", gsub("  \\(DRVIC2019\\)", "", gsub(" \\(AL2019_RV\\)", "", gsub(" \\(DRVEF2019_RV\\)", "", gsub(" \\(ADM2019_RV\\)", "", gsub(" \\(EF2019D_RV\\)" ,"", gsub(" \\(DRVADM2019_RV\\)", "", gsub("S2019_IS_RV  ", "", gsub(" \\(IC2019\\)", "", gsub(" \\(DRVAL2020\\)", "", gsub(" \\(FLAGS2019\\)", "", gsub(" \\(HD2020\\)", "", gsub(" \\(HD2019\\)", "", x))))))))))))))))))
	}
	colnames(college_data) <- RemoveHD(colnames(college_data))
	college_data$"Institution Name" <- gsub(" in the City of New York", "", college_data$"Institution Name")
	return(college_data)
}

AppendVaccinationOld <- function(college_data) {
	vaccination <- read.csv("data/data-7G1ie.csv", header=TRUE) # From the Chronicle of Higher Education
	vaccination$CollegeName <- gsub(".+\\@\\@", "", vaccination$College)
	vaccination$AllEmployees <- "No"
	vaccination$AllEmployees[vaccination$"All.employees....Vaccination.required.." == "✓"] <- "Yes"
	vaccination$AllStudents <- "No"
	vaccination$AllStudents[vaccination$"All.students....Vaccination.required.." == "✓"] <- "Yes"
	college_data$AllStudentsVaccinatedAgainstCovid19 <- "No"
	college_data$AllEmployeesVaccinatedAgainstCovid19 <- "No"

	for (i in seq_along(vaccination$CollegeName)) {
		distances <- adist(vaccination$CollegeName[i], college_data$"Institution Name")
		matches <- which(distances == min(distances))
		college_data$AllStudentsVaccinatedAgainstCovid19[matches] <- vaccination$AllStudents[i]
		college_data$AllEmployeesVaccinatedAgainstCovid19[matches] <- vaccination$AllEmployees[i]
	}
	return(college_data)
}

AppendVaccination <- function(comparison_table) {
	print("Appending vaccination data")
	vaccination <- read.csv("data/data-7G1ie.csv", header=TRUE) # From the Chronicle of Higher Education
	vaccination$CollegeName <- gsub(".+\\@\\@", "", vaccination$College)
	vaccination$AllEmployees <- "No"
	vaccination$AllEmployees[vaccination$"All.employees....Vaccination.required.." == "✓"] <- "Yes"
	vaccination$AllStudents <- "No"
	vaccination$AllStudents[vaccination$"All.students....Vaccination.required.." == "✓"] <- "Yes"
	comparison_table$AllStudentsVaccinatedAgainstCovid19 <- "No"
	comparison_table$AllEmployeesVaccinatedAgainstCovid19 <- "No"

	for (i in seq_along(vaccination$CollegeName)) {
		distances <- adist(vaccination$CollegeName[i], comparison_table$"Institution entity name")
		matches <- which(distances == min(distances,na.rm=TRUE))
		comparison_table$AllStudentsVaccinatedAgainstCovid19[matches] <- vaccination$AllStudents[i]
		comparison_table$AllEmployeesVaccinatedAgainstCovid19[matches] <- vaccination$AllEmployees[i]
	}
	return(comparison_table)
}

AppendMisconduct <- function(college_data) {
	print("Appending misconduct data")
	asmd <- misconduct::get_misconduct(agree=TRUE)
	asmd_colleges <- unique(asmd$Institution)
	college_data$InMisconductDatabase <- "No"
	for (i in seq_along(asmd_colleges)) {
		distances <- adist(asmd_colleges[i], college_data$"Institution Name")
		matches <- which(distances == min(distances,na.rm=TRUE))
		college_data$InMisconductDatabase[matches] <- "Yes"
	}	
	return(college_data)
}

AppendAbortionOld <- function(college_data) {
	#abortion <- read.csv("data/abortion.csv", header=TRUE)
	#colnames(abortion) <- gsub("\\.", " ", colnames(abortion))
	abortion <- read.csv("data/worldpopulationreview_abortion.csv")
	abortion$`State abbreviation` <- state.abb[match(abortion$State,state.name)]
	abortion_simple <- dplyr::select(abortion, c("State abbreviation", "Status"))
	colnames(abortion_simple)[2] <- "Abortion"
	return(left_join(college_data, abortion_simple, by="State abbreviation"))
}

AppendAbortion <- function(comparison_table) {
	print("Appending abortion data")
	#abortion <- read.csv("data/abortion.csv", header=TRUE)
	#colnames(abortion) <- gsub("\\.", " ", colnames(abortion))
	abortion <- read.csv("data/worldpopulationreview_abortion.csv")
	#abortion$`State abbreviation` <- state.abb[match(abortion$State,state.name)]
	abortion_simple <- dplyr::select(abortion, c("State", "Status"))
	colnames(abortion_simple)[2] <- "Abortion"
	return(left_join(comparison_table, abortion_simple, by="State"))
}

AppendCATravelBan <- function(comparison_table) {
	print("Appending California travel ban data")
	banned_states <- state.name[match(c("AL", "AR", "FL", "ID", "IN", "IA", "KS", "KY", "MS", "MT", "NC", "ND", "OH", "OK", "SC", "SD", "TN", "TX", "UT", "WV"),state.abb)]
	ban_table <- data.frame(State=state.name, California.Travel.Ban=FALSE)
	ban_table$California.Travel.Ban[match(banned_states, ban_table$State)] <- TRUE
	ban_table <- ban_table %>% dplyr::rename(`California Travel Ban` = 'California.Travel.Ban')
	return(left_join(comparison_table, ban_table, by="State"))
}

AppendGunLawsOld <- function(college_data) {
	guns <- read.csv("data/gunlaws_giffords_scorecard.csv")
	guns$`State abbreviation` <- state.abb[match(guns$State,state.name)]
	guns_simple <- dplyr::select(guns, c("State abbreviation", "grade2022"))
	colnames(guns_simple)[2] <- "Gun law stringency"
	return(left_join(college_data, guns_simple, by="State abbreviation"))
}

AppendGunLaws <- function(comparison_table) {
	print("Appending gun law data")
	guns <- read.csv("data/gunlaws_giffords_scorecard.csv")
	guns <- dplyr::select(guns, c("State", "grade2022")) %>% dplyr::rename("Gun Law Stringency"="grade2022")
	return(left_join(comparison_table, guns, by="State"))
}

AppendAAUPCensureOld <- function(college_data) {
	aaup <- read.csv("data/aaup_censure_July2022.csv", header=TRUE)
	college_data$AAUP_Censure <- "No"
	for (i in sequence(nrow(aaup))) {
		matches <- which(college_data$"Institution Name"==aaup[i,1])
		if(length(matches)>0) {
			college_data$AAUP_Censure[matches] <- "Yes"
		}
	}	
	return(college_data)
}

AppendAAUPCensure <- function(comparison_table) {
	print("Appending AAUP censure data")
	aaup <- read.csv("data/aaup_censure_July2022.csv", header=TRUE)
	comparison_table$AAUP_Censure <- "No"
	for (i in sequence(nrow(aaup))) {
		matches <- which(comparison_table$"Institution entity name"==aaup[i,1])
		if(length(matches)>0) {
			comparison_table$AAUP_Censure[matches] <- "Yes"
		}
	}	
	return(comparison_table)
}

AggregateVotesByState <- function(voting_data) {
	voting_data$VoteInFavor <- 0
	voting_data$VoteInFavor[voting_data$Vote == "Yea"] <- 1
	voting_data$VotesAgainst <- 0
	voting_data$VotesAgainst[voting_data$Vote == "Nay"] <- 1
	voting_aggregated <- voting_data %>% group_by(State) %>% summarise(
		VotesInFavor = sum(VoteInFavor),
		VotesAgainst = sum(VotesAgainst),
		VoteCount = n()
	)
	voting_aggregated$ProportionVotesInFavor <- voting_aggregated$VotesInFavor / voting_aggregated$VoteCount
	voting_aggregated$ProportionVotesInAgainst <- voting_aggregated$VotesAgainst / voting_aggregated$VoteCount
	return(voting_aggregated)
}

AppendMarriageRespectOld <- function(college_data) {
	marriage_defense <- read.csv("data/MarriageAct_HR8404_2022.csv", header=TRUE)
	marriage_aggregated <- AggregateVotesByState(marriage_defense)
	marriage_aggregated$`State abbreviation` <- state.abb[match(marriage_aggregated$State,state.name)]
	marriage_aggregated_simple <- dplyr::select(marriage_aggregated, c("State abbreviation", "ProportionVotesInFavor"))
	colnames(marriage_aggregated_simple)[2] <- "Proportion of reps voting in favor of respect for marriage act"
	return(left_join(college_data, marriage_aggregated_simple, by="State abbreviation"))
}

AppendMarriageRespect <- function(comparison_table) {
	print("Appending marriage respect data")
	marriage_defense <- read.csv("data/MarriageAct_HR8404_2022.csv", header=TRUE)
	marriage_aggregated <- AggregateVotesByState(marriage_defense) %>%
	dplyr::select(c("State", "ProportionVotesInFavor")) %>% dplyr::rename("Proportion of reps voting in favor of respect for marriage act" = "ProportionVotesInFavor" )
	return(left_join(comparison_table, marriage_aggregated, by="State"))
}

AppendContraceptiveSupport <- function(comparison_table) {
	print("Appending contraceptive support data")
	contraceptive_defense <- read.csv("data/ContraceptiveAct_HR8373_July2022.csv", header=TRUE)
	contraceptive_aggregated <- AggregateVotesByState(contraceptive_defense) %>% dplyr::select(c("State", "ProportionVotesInFavor")) %>% dplyr::rename("Proportion of reps voting in favor of respect for right to contraception act" = "ProportionVotesInFavor" )
	return(left_join(comparison_table, contraceptive_aggregated, by="State"))
}

AppendContraceptiveSupportOld <- function(college_data) {
	contraceptive_defense <- read.csv("data/ContraceptiveAct_HR8373_July2022.csv", header=TRUE)
	contraceptive_aggregated <- AggregateVotesByState(contraceptive_defense)
	contraceptive_aggregated$`State abbreviation` <- state.abb[match(contraceptive_aggregated$State,state.name)]
	contraceptive_aggregated_simple <- dplyr::select(contraceptive_aggregated, c("State abbreviation", "ProportionVotesInFavor"))
	colnames(contraceptive_aggregated_simple)[2] <- "Proportion of reps voting in favor of respect for right to contraception act"
	return(left_join(college_data, contraceptive_aggregated_simple, by="State abbreviation"))
}

FilterForDegreeGranting <- function(college_data) {
	return(subset(college_data, college_data$"Degree-granting status"=="Degree-granting"))	
}

RoughRanking <- function(college_data) {
	admission <- 100*as.numeric(college_data$"Admission")
	admission[which(is.na(admission)) ]<- 100
	graduation <- 100*as.numeric(college_data$"Graduation")
	graduation[which(is.na(graduation))] <- 0
	enrollment <- as.numeric(college_data$`Undergraduate enrollment`)
	enrollment[which(is.na(enrollment))] <- 0
	
	college_data$RankingProxy <- (100 - admission) + graduation + (enrollment^(1/4)) 
	college_data$RankingProxy <- as.numeric(college_data$RankingProxy) 
	college_data <- subset(college_data, !is.na(college_data$RankingProxy))
	college_data <- dplyr::distinct(college_data[order(college_data$RankingProxy, decreasing=TRUE),])
	return(college_data)
}

EnhanceData <- function(college_data, faculty_counts, student_demographics, students_by_state_by_institution ) {
	college_data_enhanced <- as.data.frame(college_data)

	college_data_enhanced$NatWalkInd <- round(as.numeric(college_data_enhanced$NatWalkInd)/20,2)
	#college_data_enhanced <- dplyr::rename(college_data_enhanced,  `State full`="State")
	college_data_enhanced <- dplyr::rename(college_data_enhanced, Name = `Institution Name`, Sector=`Sector of institution`, Type=CollegeType, Locale=`Degree of urbanization (Urban-centric locale)`, City="City location of institution", State="State abbreviation", Walkability=NatWalkInd, `Anti-LGBTQ+ state laws`=BannedCATravel, Yield="Admissions yield - total", `Distance (m) to transit`=DistanceToTransit, Admission="Percent admitted - total", `First year retention`="Full-time retention rate  2019", "Graduation"="Graduation rate  total cohort", "Covid vax (students)"="AllStudentsVaccinatedAgainstCovid19", "Covid vax (employees)"="AllEmployeesVaccinatedAgainstCovid19", "Misconduct reports"="InMisconductDatabase")
	
	college_data_enhanced$`Minimum distance to mass transit (minutes walking)` <- round((as.numeric(college_data_enhanced$`Distance (m) to transit`)/1.34)/60) #using an estimate of walk speed of 1.34 m/s from https://www.healthline.com/health/exercise-fitness/average-walking-speed#average-speed-by-age
	college_data_enhanced$`Minimum distance to mass transit (minutes walking)`[is.infinite(college_data_enhanced$`Minimum distance to mass transit (minutes walking)`)] <- NA
	
	
	college_data_enhanced$Yield <- as.numeric(college_data_enhanced$Yield)/100
	college_data_enhanced$Admission <- as.numeric(college_data_enhanced$Admission)/100
	college_data_enhanced$`First year retention` <- as.numeric(college_data_enhanced$`First year retention`)/100
	college_data_enhanced$Graduation <- as.numeric(college_data_enhanced$Graduation)/100
	college_data_enhanced$`Anti-LGBTQ+ state laws` <- as.factor(college_data_enhanced$`Anti-LGBTQ+ state laws`)
	college_data_enhanced$`Undergrads per tenure-track professor` <- 0
	college_data_enhanced$`Undergrads per instructor` <- 0
	college_data_enhanced$`Distance (m) to transit` <- as.numeric(college_data_enhanced$`Distance (m) to transit`)
	college_data_enhanced$`Covid vax (students)` <- as.factor(college_data_enhanced$`Covid vax (students)`)
	college_data_enhanced$`Covid vax (employees)` <- as.factor(college_data_enhanced$`Covid vax (employees)`)
	# college_data_enhanced$`Misconduct reports` <- as.factor(college_data_enhanced$`Misconduct reports`)
	# college_data_enhanced$`Abortion` <- as.factor(college_data_enhanced$`Abortion`)
	# college_data_enhanced$Sector <- as.factor(college_data_enhanced$Sector)
	# college_data_enhanced$State <- as.factor(college_data_enhanced$State)
    college_data_enhanced$LocaleChar <- as.character(college_data_enhanced$Locale)
	# college_data_enhanced$Locale <- as.factor(college_data_enhanced$Locale)
	college_data_enhanced$`Undergraduate enrollment` <- 0
	college_data_enhanced$`Grad enrollment` <- 0
	college_data_enhanced$`TTFaculty` <- 0
	college_data_enhanced$`NTTFaculty` <- 0
	college_data_enhanced$`Students plus Faculty` <- 0
	college_data_enhanced$ShortName <- college_data_enhanced$Name
	college_data_enhanced$Name <- paste0(college_data_enhanced$Name, " (", college_data_enhanced$City, ", ", college_data_enhanced$State, ")")
	# name_collisions <- college_data_enhanced$ShortName[duplicated(college_data_enhanced$ShortName)]
	# for(name in name_collisions) {
	# 	college_data_enhanced$ShortName[college_data_enhanced$ShortName==name] <- college_data_enhanced$Name[college_data_enhanced$ShortName==name]
	# }
	college_data_enhanced$URL <- paste0(  utils::URLencode(gsub(" ", "", college_data_enhanced$ShortName), reserved=TRUE), ".html")
	college_data_enhanced$`Tenure track fraction` <- 0
	college_data_enhanced$`Fraction of tenure track faculty who are women` <- 0
	college_data_enhanced$`Fraction of tenure track faculty who are men` <- 0
	college_data_enhanced$`Fraction of non-tenure track faculty who are women` <- 0
	college_data_enhanced$`Fraction of non-tenure track faculty who are men` <- 0
	college_data_enhanced$`Fraction of tenure track faculty who are BIPOC` <- 0
	college_data_enhanced$`Fraction of tenure track faculty who are white` <- 0
	college_data_enhanced$`Fraction of non-tenure track faculty who are BIPOC` <- 0
	college_data_enhanced$`Fraction of non-tenure track faculty who are white` <- 0

	college_data_enhanced$`Fraction of undergrads who are women` <- 0
	college_data_enhanced$`Fraction of undergrads who are men` <- 0
	college_data_enhanced$`Fraction of undergrads who are BIPOC` <- 0
	college_data_enhanced$`Fraction of undergrads who are white` <- 0
	rowMeansAsNumeric <- function(x, ...) {
		for (i in sequence(ncol(x))) {
			x[,i] <- as.numeric(x[,i])
		}
		return(rowMeans(x, ...))
	}
	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Tuition and fees as a percent of core revenues` = dplyr::select(., starts_with("Tuition and fees as a percent of core revenues")) %>% rowMeansAsNumeric( na.rm = TRUE))
	
	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`State appropriations as percent of core revenues` = dplyr::select(., starts_with("State appropriations as percent of core revenues")) %>% rowMeansAsNumeric( na.rm = TRUE))

	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Local appropriations as a percent of core revenues` = dplyr::select(., starts_with("Local appropriations as a percent of core revenues")) %>% rowMeansAsNumeric( na.rm = TRUE))
	
	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Government grants and contracts as a percent of core revenues` = dplyr::select(., starts_with("Government grants and contracts as a percent of core revenues")) %>% rowMeansAsNumeric( na.rm = TRUE))

	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Private gifts  grants  and contracts as a percent of core revenues` = dplyr::select(., starts_with("Private gifts  grants  and contracts as a percent of core revenues")) %>% rowMeansAsNumeric( na.rm = TRUE))

	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Investment return as a percent of core revenues` = dplyr::select(., starts_with("Investment return as a percent of core revenues")) %>% rowMeansAsNumeric( na.rm = TRUE))

					 
	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Endowment assets per FTE` = dplyr::select(., starts_with("Endowment assets")) %>% rowMeansAsNumeric( na.rm = TRUE))
	
	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Average net price-students awarded grant or scholarship aid` = dplyr::select(., starts_with("Average net price-students awarded grant or scholarship aid")) %>% rowMeansAsNumeric( na.rm = TRUE))
	
	college_data_enhanced <- college_data_enhanced %>% dplyr::mutate(`Equity ratio` = dplyr::select(., starts_with("Equity ratio")) %>% rowMeansAsNumeric( na.rm = TRUE))

		 
	#college_data_enhanced <- dplyr::select(college_data_enhanced, -RankingProxy)
	#college_data_enhanced <- select(college_data_enhanced, -City)
	
	for (i in sequence(nrow(college_data_enhanced))) {
		local_student_demographics <-  student_demographics[[college_data_enhanced$ShortName[i]]]
		local_faculty <- faculty_counts[[college_data_enhanced$ShortName[i]]]
		college_data_enhanced$`Undergraduate enrollment`[i] <- local_student_demographics[1,1] + local_student_demographics[2,1]
		college_data_enhanced$`Grad enrollment`[i] <- local_student_demographics[1,2] + local_student_demographics[2,2]
		college_data_enhanced$`TTFaculty`[i] <- local_faculty[1,1] + local_faculty[1,2] + local_faculty[2,1] + local_faculty[2,2]
		college_data_enhanced$`NTTFaculty`[i] <- local_faculty[1,3] + local_faculty[2,3] 
		college_data_enhanced$`Undergrads per tenure-track professor`[i] <- round(college_data_enhanced$`Undergraduate enrollment`[i] / college_data_enhanced$`TTFaculty`[i],1)
		college_data_enhanced$`Undergrads per instructor`[i] <- round(college_data_enhanced$`Undergraduate enrollment`[i] / (college_data_enhanced$`NTTFaculty`[i] + college_data_enhanced$`TTFaculty`[i]),1)
		college_data_enhanced$`Students plus Faculty`[i] <- college_data_enhanced$`TTFaculty`[i]  + college_data_enhanced$`NTTFaculty`[i] + college_data_enhanced$`Grad enrollment`[i]  + college_data_enhanced$`Undergraduate enrollment`[i] 
		college_data_enhanced$`Tenure track fraction`[i] <- round(college_data_enhanced$`TTFaculty`[i]/(college_data_enhanced$`TTFaculty`[i] + college_data_enhanced$`NTTFaculty`[i]),3)
		college_data_enhanced$`Fraction of tenure track faculty who are women`[i]<- round((local_faculty['Grand total women', 'Tenure-track (count)'] + local_faculty['Grand total women', 'Tenured (count)']) / college_data_enhanced$`TTFaculty`[i] ,3)
		college_data_enhanced$`Fraction of tenure track faculty who are men`[i] <- round((local_faculty['Grand total men', 'Tenure-track (count)'] + local_faculty['Grand total men', 'Tenured (count)']) / college_data_enhanced$`TTFaculty`[i],3)
		college_data_enhanced$`Fraction of non-tenure track faculty who are women`[i] <- round(local_faculty['Grand total women', 'Non-tenure-track (count)']  / college_data_enhanced$`NTTFaculty`[i], 3)
		college_data_enhanced$`Fraction of non-tenure track faculty who are men`[i] <- round(local_faculty['Grand total men', 'Non-tenure-track (count)']  / college_data_enhanced$`NTTFaculty`[i], 3)
		college_data_enhanced$`Fraction of tenure track faculty who are BIPOC`[i] <- round(sum(local_faculty[c('American Indian or Alaska Native men', 'American Indian or Alaska Native women',  'Asian men', 'Asian women', 'Black or African American men', 'Black or African American women', 'Hispanic or Latino men', 'Hispanic or Latino women', 'Native Hawaiian or Other Pacific Islander men', 'Native Hawaiian or Other Pacific Islander women'), c('Tenured (count)', 'Tenure-track (count)')])/college_data_enhanced$`TTFaculty`[i],3)
		college_data_enhanced$`Fraction of tenure track faculty who are white`[i] <- round(sum(local_faculty[c('White men', 'White women'), c('Tenured (count)', 'Tenure-track (count)')])/college_data_enhanced$`TTFaculty`[i],3)

		college_data_enhanced$`Fraction of non-tenure track faculty who are BIPOC`[i] <- round(sum(local_faculty[c('American Indian or Alaska Native men', 'American Indian or Alaska Native women',  'Asian men', 'Asian women', 'Black or African American men', 'Black or African American women', 'Hispanic or Latino men', 'Hispanic or Latino women', 'Native Hawaiian or Other Pacific Islander men', 'Native Hawaiian or Other Pacific Islander women'), 'Non-tenure-track (count)'])/college_data_enhanced$`NTTFaculty`[i] ,3)
		college_data_enhanced$`Fraction of non-tenure track faculty who are white`[i] <- round(sum(local_faculty[c('White men', 'White women'), 'Non-tenure-track (count)'])/college_data_enhanced$`NTTFaculty`[i],3) 
		
		college_data_enhanced$`Fraction of undergrads who are women`[i] <- round(sum(local_student_demographics['Grand total women','Undergrad (count)'])/college_data_enhanced$`Undergraduate enrollment`[i],3)
		college_data_enhanced$`Fraction of undergrads who are men`[i] <- round(sum(local_student_demographics['Grand total men','Undergrad (count)'])/college_data_enhanced$`Undergraduate enrollment`[i],3)

		college_data_enhanced$`Fraction of undergrads who are BIPOC`[i] <- round(sum(local_student_demographics[c('American Indian or Alaska Native men', 'American Indian or Alaska Native women',  'Asian men', 'Asian women', 'Black or African American men', 'Black or African American women', 'Hispanic men', 'Hispanic women', 'Native Hawaiian or Other Pacific Islander men', 'Native Hawaiian or Other Pacific Islander women'),'Undergrad (count)' ])/college_data_enhanced$`Undergraduate enrollment`[i],3)
		
		college_data_enhanced$`Fraction of undergrads who are white`[i] <- round(sum(local_student_demographics[c('White men', 'White women'),'Undergrad (count)' ])/college_data_enhanced$`Undergraduate enrollment`[i],3)
		

	}

	college_data_enhanced <- college_data_enhanced[!duplicated(college_data_enhanced$UnitID),]
	return(college_data_enhanced)
}

GetOverviewColumns <- function(college_data) {
	college_data$`Average library loans (physical) per student or faculty member` <- round(as.numeric(college_data$`Total physical library circulations (books and media)`) / as.numeric(college_data$`Students plus Faculty`),1)
	college_data$`Average library loans (physical + digital) per student or faculty member` <- round(as.numeric(college_data$`Total library circulations (physical and digital/electronic)`) / as.numeric(college_data$`Students plus Faculty`),1)


	overview <- as.data.frame(college_data %>% dplyr::select(
		"Name", 
		"City", 
		"State", 
		"Sector", 
		"Level of institution", 
		"Historically Black College or University", 
		"Tribal college", 
		"Undergraduate enrollment", 
		"Grad enrollment", 
		"Yield", 
		"Admission", 
		"Graduation", 
		"First year retention", 
		"Walkability", 
		"Undergrads per tenure-track professor", 
		"Undergrads per instructor", 
		"URL", 
		"Tuition and fees as a percent of core revenues", 
		"Endowment assets per FTE", 
		"Average net price-students awarded grant or scholarship aid", 
		"Liquor discipline per student (3 yr avg)",
		"Liquor arrest per student (3 yr avg)",
		"Drug discipline per student (3 yr avg)",
		"Drug arrest per student (3 yr avg)",
		"Weapon discipline per student (3 yr avg)",
		"Weapon arrest per student (3 yr avg)",
		"Reported rape per student (3 yr avg)",
		"Reported fondling per student (3 yr avg)",
		"Number of physical books", 
		"Number of physical media", 
		"Number of digital/electronic books",
		"Total library circulations (physical and digital/electronic)",
		"Average library loans (physical) per student or faculty member",
		"Average library loans (physical + digital) per student or faculty member",
		"Warmest month max temp (F)",
		"Coldest month min temp (F)",
		"Annual precipitation (inches)",
		"biome",
		"Covid vax (employees)",
		"Covid vax (students)",
		"Abortion",
		"Gun law stringency",
		"Proportion of reps voting in favor of respect for right to contraception act",
		"Proportion of reps voting in favor of respect for marriage act"
		
		
	))
	return(overview)
}

RenderSparklines <- function(spark_height=5, spark_width=40) {
	dir.create("docs/images")
	percentages <- seq(from=0, to=100, by=1)
	for (pct_index in seq_along(percentages)) {
		pct <- percentages[pct_index]
		p <- ggplot(data=data.frame(focal=c(TRUE, FALSE), percent=c(pct, 100-pct), x=c(1,1)), aes(fill=focal, y=percent, x=x)) + geom_bar(position="fill", stat="identity") +  theme_void() + theme(legend.position="none") + coord_flip() + scale_fill_manual(values=c("darkgray", GetColorFromPercentile(pct))) + theme(panel.background=element_rect(colour=GetColorFromPercentile(pct)))
		ggsave(
  			plot = p,
  			filename = paste0("docs/images/pct_bar_", pct, ".png"),
  			bg = "transparent",
			width = spark_width,
			height= spark_height,
			units = "px"
		)
	}	
	
	for (pct_index in seq_along(percentages)) {
		pct <- percentages[pct_index]
		p <- ggplot(data=data.frame(focal=c(TRUE, FALSE), percent=c(pct, 100-pct), x=c(1,1)), aes(fill=focal, y=percent, x=x)) + geom_bar(position="fill", stat="identity") +  theme_void() + theme(legend.position="none") + coord_flip() + scale_fill_manual(values=c("darkgray", GetColorFromPercentile(pct, direction=1)))  + theme(panel.background=element_rect(colour=GetColorFromPercentile(pct, direction=1)))
		ggsave(
  			plot = p,
  			filename = paste0("docs/images/pct_bar_rev_good_", pct, ".png"),
  			bg = "transparent",
			width = spark_width,
			height= spark_height,
			units = "px"
		)
	}	
	
	for (pct_index in seq_along(percentages)) {
		pct <- percentages[pct_index]
		p <- ggplot(data=data.frame(focal=c(TRUE, FALSE), percent=c(pct, 100-pct), x=c(1,1)), aes(fill=focal, y=percent, x=x)) + geom_bar(position="fill", stat="identity") +  theme_void() + theme(legend.position="none") + coord_flip() + scale_fill_manual(values=c("darkgray", "black")) 
		ggsave(
  			plot = p,
  			filename = paste0("docs/images/pct_bar_grayscale_", pct, ".png"),
  			bg = "transparent",
			width = spark_width,
			height= spark_height,
			units = "px"
		)
	}	
}

RenderInstitutionPagesNew <- function(comparison_table, fields_and_majors, maxcount=40, CIPS_codes, weatherspark, yml) {
	
	institution_ids <- unique(comparison_table$`UNITID Unique identification number of the institution`)
	dead_institutions <- subset(comparison_table, comparison_table$`Status of institution`%in% c('Closed in current year (active has data)', 'Combined with other institution ', 'Delete out of business'))
	institution_ids <- setdiff(institution_ids, dead_institutions$`UNITID Unique identification number of the institution`)
	comparison_table <- subset(comparison_table, comparison_table$`UNITID Unique identification number of the institution` %in% institution_ids) # remove dead institutions so we don't compare with them
	
	# prioritize the "selective" schools to render first
	comparison_table$pseudoranking <- (100-as.numeric(comparison_table$`Admission percentage total`))*sqrt(as.numeric(comparison_table$`Total instructors`))
	rejection_ranking <- comparison_table[order(comparison_table$pseudoranking, decreasing=TRUE),] 
	institution_ids <- unique(rejection_ranking$`UNITID Unique identification number of the institution`)
	
	

	
	failures <- c()
	#for (i in seq_along(institutions)) {
	for (i in sequence(min(maxcount, length(institution_ids)))) {
		failed <- TRUE
		try({
			institution_id <- institution_ids[i]
			institution_name <- rownames(t(t(sort(table(comparison_table$`Institution entity name`[comparison_table$`UNITID Unique identification number of the institution` == institution_id]), decreasing=TRUE))))[1] # sometimes the name changes a bit; take the most common one		
			print(paste0("Rendering ", institution_name, " (", i, " of ", length(institution_ids), ")"))
			
			rmarkdown::render(
				input="_institution.Rmd", 
				output_file="docs/institution.html", 
				params = list(
					institution_name = institution_name,
					institution_long_name = institution_name,
					institution_id =  institution_id,
					comparison_table = comparison_table,
					weatherspark=weatherspark
				),
				quiet=TRUE
			)
			#Sys.sleep(1)
			system("sed -i '' 's/&gt;/>/g' docs/institution.html") # because htmlTable doesn't escape well; the '' is a requirement of OS X's version of sed, apparently
			system("sed -i '' 's/&lt;/</g' docs/institution.html")
			
			#Sys.sleep(1)


			file.copy("docs/institution.html", paste0("docs/", utils::URLencode(gsub(" ", "", institution_id)), ".html"), overwrite=TRUE)
			print(paste0("docs/", utils::URLencode(gsub(" ", "", institution_id)), ".html"))
			#Sys.sleep(1)
			failed <- FALSE
		}, silent=TRUE)
		if(failed) {
			failures <- c(failures, comparison_table$`Institution entity name`[i])
		}
	}
	return(failures)
}


RenderMajorsPages <- function(fields_and_majors, CIPS_codes, yml, maxcount) {
	db <- dbConnect(RSQLite::SQLite(), "data/db_IPEDS.sqlite")
	field_data <- tbl(db, fields_and_majors[1]) %>% as.data.frame()
	completions_with_percentage <- tbl(db, fields_and_majors[2]) %>% as.data.frame()
	dbDisconnect(db)
	
	for (row_index in sequence(nrow(field_data))) {
		rmarkdown::render(
				input="_majors.Rmd", 
				output_file="docs/majors.html", 
				params = list(
					degree = field_data$Degree[row_index],
					field = field_data$Field[row_index],
					filtered_table = completions_with_percentage %>% dplyr::filter(Degree==field_data$Degree[row_index]) %>% dplyr::filter(Field==field_data$Field[row_index]) %>% dplyr::select(-Degree) %>% dplyr::select(-Field),
					definition = CIPS_codes$CIPDefinition[CIPS_codes$CIPTitle == field_data$Field[row_index]]
				),
				quiet=TRUE
			)
			#Sys.sleep(1)
			system("sed -i '' 's/&gt;/>/g' docs/majors.html") # because htmlTable doesn't escape well; the '' is a requirement of OS X's version of sed, apparently
			system("sed -i '' 's/&lt;/</g' docs/majors.html")
			
			#Sys.sleep(1)

			#encode_field <- utils::URLencode(gsub('-', '_', gsub(')', '_', gsub('(', '_', gsub("\\.", "_", gsub("'", "", gsub('/', "_",gsub(",", "_", gsub(" ", "", )))))))))
			
			file.copy("docs/majors.html", paste0("docs/majors_", utils::URLencode(gsub(" ", "", field_data$Degree[row_index])), "_", EncodeField(field_data$Field[row_index]) ,".html"), overwrite=TRUE)
			#Sys.sleep(1)
	}
}

EncodeField <- function(x) {
	return(utils::URLencode(gsub('-', '_', gsub("\\(", '_', gsub("\\)", '_', gsub("\\.", "_", gsub("'", "", gsub('/', "_",gsub(",", "_", gsub(" ", "", x))))))))))	
}

RenderStatePages <- function(degree_granting, students_by_state_by_institution, spark_width, spark_height, state_pops) {	
	states <- unique(state_pops$State)
	failures <- c()
	#for (i in seq_along(institutions)) {
	for (i in seq_along(states)) {
		failed <- TRUE
		try({
			print(states[i])
			rmarkdown::render(
				input="_state.Rmd", 
				output_file="docs/state.html", 
				params = list(
					raw_table = degree_granting,
					students_by_state_by_institution = students_by_state_by_institution,
					focal_state_long = states[i],
  					focal_state_abbr = state.abb[match(states[i],state.name)]
				),
				quiet=TRUE
			)
			Sys.sleep(1)
			system("sed -i '' 's/&gt;/>/g' docs/state.html") # because htmlTable doesn't escape well; the '' is a requirement of OS X's version of sed, apparently
			system("sed -i '' 's/&lt;/</g' docs/state.html")
			
			#system("sed -i '' 's/‘/\x27/g' docs/institution.html")
			#system("sed -i '' 's/’/\x27/g' docs/institution.html")
			Sys.sleep(1)


			file.copy("docs/state.html", paste0("docs/", utils::URLencode(gsub(" ", "", states[i])), ".html"), overwrite=TRUE)
			#print(paste0("docs/", utils::URLencode(gsub(" ", "", institutions[i])), ".html"))
			Sys.sleep(1)
			# quarto::quarto_render(
			# 	input="_institution.qmd", 
			# 	output_file=paste0(  "docs/", utils::URLencode(gsub(" ", "", institutions[i])), ".html"), 
			# 	execute_params = list(
			# 		institution_name = institutions[i],
			# 		institution_long_name = degree_granting$ShortName[i],
			# 		overview_table = subset(overview, overview$ShortName == institutions[i]),
			# 		raw_table = degree_granting,
			# 		students_by_state_by_institution = students_by_state_by_institution,
			# 		student_demographics = student_demographics,
			# 		faculty_counts = faculty_counts
			# 	),
			# 	quiet=FALSE
			# )
			failed <- FALSE
		}, silent=TRUE)
		if(failed) {
			failures <- c(failures, states[i])
		}
	}
	return(failures)
}

# pages is there just so it will run this after the pages are run
RenderIndexPageEtAl <- function(pages, index_table, yml) {
	system("rm docs/index.html")
	#system("rm docs/degrees_by*")


	rmarkdown::render(
				input="_index.Rmd", 
				output_file="docs/index.html", 
				params = list(
					index_table = index_table
				),
				quiet=TRUE
	)
	
	# rmarkdown::render(
	# 			input="degrees_by_conference.Rmd", 
	# 			output_file="docs/degrees_by_conference.html", 
	# 			quiet=TRUE
	# )
	
	# rmarkdown::render(
	# 			input="degrees_by_institution.Rmd", 
	# 			output_file="docs/degrees_by_institution.html", 
	# 			quiet=TRUE
	# )
	
	# rmarkdown::render(
	# 			input="degrees_by_type.Rmd", 
	# 			output_file="docs/degrees_by_type.html", 
	# 			quiet=TRUE
	# )
	rmarkdown::render(
		input="_about.Rmd", 
		output_file="docs/about.html", 
		quiet=FALSE
	)
	
	rmarkdown::render(
		input="_fields_overview.Rmd", 
		output_file="docs/fields_overview.html", 
		quiet=FALSE
	)	
}

FilterForTopAndSave <- function(overview) {
	overview_top <- as.data.frame(overview)[c(1:1000),]
	write.csv(overview_top, file="overview_top.csv")
}

GetStudentsByState <- function(state_pops, focal_raw) {
	Students_by_state <- data.frame(fips=rep(NA,  length(state.name)), abbr=state.abb, State=state.name, Undergrad_Count=rep(0.0, length(state.name)), Percent_of_18_yo_from_this_state_enrolled_here=rep(0.0, length(state.name)))


	for (i in sequence(nrow(Students_by_state))) {
		Students_by_state$Undergrad_Count[i] <- unname(unlist(focal_raw[paste0("Count of first-time undergrads from ", Students_by_state$State[i])]))
		Students_by_state$Percent_of_18_yo_from_this_state_enrolled_here[i] <- unname(unlist(focal_raw[paste0("Percentage of all 18 year olds in the focal state enrolling in this college from ", Students_by_state$State[i])]))
		if(is.na(Students_by_state$Undergrad_Count[i])) {
			Students_by_state$Undergrad_Count[i] <- 0
			Students_by_state$Percent_of_18_yo_from_this_state_enrolled_here[i] <- 0
		}
		Students_by_state$fips[i] <- state_pops$State_FIPS[which(state_pops$State==Students_by_state$State[i])]
	}
	count_data <- as.data.frame(dplyr::select(Students_by_state, c("fips", "Undergrad_Count")))
	colnames(count_data) <- c("fips", "values")
	count_data$fips <- as.character(count_data$fips)
	count_data$values[is.na(count_data$values)] <- 0
	count_data[,2] <- as.numeric(count_data[,2])

	pretty_table <- Students_by_state[, c('fips', 'State','Undergrad_Count')]
	colnames(pretty_table) <- c('fips', "State", "Number of students")
	pretty_table$'Fraction of student body from each state' <- paste0('1/',round(sum(as.numeric(pretty_table$'Number of students'))/as.numeric(pretty_table$'Number of students')))
	pretty_table$'Fraction of student body from each state' <- gsub('1/Inf', '0', pretty_table$'Fraction of student body from each state')
	pretty_table$'Fraction of 18 year olds from each state enrolled here' <- paste0('1/',round(100/Students_by_state$Percent_of_18_yo_from_this_state_enrolled_here))
	pretty_table$'Fraction of 18 year olds from each state enrolled here' <- gsub('1/Inf', '0', pretty_table$'Fraction of 18 year olds from each state enrolled here')	
	return(pretty_table)
}

GetStudentsByStateByInstitution <- function(college_data, state_pops) {
	results <- list()
	for (i in sequence(nrow(college_data))) {
		results[[i]] <- GetStudentsByState(state_pops, college_data[i,])
	}	
	names(results) <- unlist(college_data$`Institution Name`)
	return(results)
}

CharacterFractionToNumeric <- function(x) {
	return(unname(sapply(x, function(x) eval(parse(text = x)))))
}

GetStudentCounts <- function(focal_raw) {
	student_demographics <- gsub(" \\(EF2019A_RV  Full-time students  Undergraduate total\\)", "", colnames(focal_raw)[grepl("(EF2019A_RV  Full-time students  Undergraduate total)", colnames(focal_raw))])


	student_types <- c("(EF2019A_RV  Full-time students  Undergraduate total)", "(EF2019A_RV  Full-time students  Graduate)")

	combinations <- expand.grid(student_demographics = student_demographics, rank=student_types)


	student_counts <- as.data.frame(matrix(0, nrow=length(student_demographics), ncol=length(student_types)))
	rownames(student_counts) <- student_demographics
	colnames(student_counts) <-student_types
	for (row_index in sequence(nrow(student_counts))) {
		for(col_index in sequence(ncol(student_counts))) {
			try(student_counts[row_index, col_index] <- as.numeric(focal_raw[paste0(rownames(student_counts)[row_index], " ", colnames(student_counts)[col_index])]), silent=TRUE)
			if(is.na(student_counts[row_index, col_index])) {
				student_counts[row_index, col_index] <- 0	
			}
		}	
	}

	total_undergrads <- student_counts[1,1] + student_counts[2,1]
	total_grads <- student_counts[1,2] + student_counts[2,2]
	colnames(student_counts) <- c("Undergrad (count)", "Grad student (count)")
	student_counts$`Undergrads (percent)` <- "0%"
	student_counts$`Grad students (percent)` <- "0%"
	for (i in sequence(nrow(student_counts))) {
		student_counts[i,3] <- paste0(round(100*student_counts[i,1]/total_undergrads,1), '%')
		student_counts[i,4] <- paste0(round(100*student_counts[i,2]/total_grads,1), '%')

	}	
	return(student_counts)
}

GetStudentsDemographicsByInstitution <- function(college_data) {
	results <- list()
	for (i in sequence(nrow(college_data))) {
		results[[i]] <- GetStudentCounts(college_data[i,])
	}	
	names(results) <- unlist(college_data$`Institution Name`)
	return(results)
}

GetFacultyCounts <- function(focal_raw) {
	instructor_levels <- gsub("Black or African American men ", "", colnames(focal_raw)[grepl("Black or African American men \\(Full", colnames(focal_raw))])
	instructor_groups <- unique(gsub(" \\(.*\\)", "", colnames(focal_raw)[grepl("Full-time instructional", colnames(focal_raw))]))

	get_level <- function(x) {
		return(unname(gsub("\\)", "", strsplit(x, "  +")[[1]][2])))
	}
	professional_levels <- unique(sapply(instructor_levels, get_level))
	professional_levels <- professional_levels[!is.na(professional_levels)]
	combinations <- expand.grid(professional_levels = professional_levels, rank=c("(Full-time instructional not on tenure/no tenure system  ", "(Full-time instructional on-tenure track  ", "(Full-time instructional tenured  "))

	faculty_counts <- matrix(0, nrow=length(instructor_groups), ncol=nrow(combinations))
	rownames(faculty_counts) <- instructor_groups
	colnames(faculty_counts) <- paste0(combinations$rank, combinations$professional_levels, ")")
	for (row_index in sequence(nrow(faculty_counts))) {
		for(col_index in sequence(ncol(faculty_counts))) {
			try(faculty_counts[row_index, col_index] <- as.numeric(focal_raw[paste0(rownames(faculty_counts)[row_index], " ", colnames(faculty_counts)[col_index])]), silent=TRUE)
		}	
	}
	faculty_counts[which(is.na(faculty_counts))] <- 0
	#faculty_counts <- faculty_counts[,which(colSums(faculty_counts)>0)]
	colnames(faculty_counts) <- gsub('\\(Full-time instructional ', '', gsub('\\)', '', colnames(faculty_counts)))

	faculty_counts_summary <- as.data.frame(matrix(0, nrow=nrow(faculty_counts), ncol=6))
	rownames(faculty_counts_summary) <- rownames(faculty_counts)
	colnames(faculty_counts_summary) <- c("Tenured (count)", "Tenure-track (count)", "Non-tenure-track (count)", "Tenured (percent)", "Tenure-track (percent)", "Non-tenure-track (percent)")
	total_faculty <- sum(sum(faculty_counts[c("Grand total men", "Grand total women"),]))
	faculty_counts_summary[,4:6] <- as.character(faculty_counts_summary[,4:6])
	# tenured
	matching_cols <- which(grepl("tenured", colnames(faculty_counts)))
	for (row_index in sequence(nrow(faculty_counts_summary))) {
		if(length(matching_cols)>=1) {
			faculty_counts_summary[row_index,1] <- sum(faculty_counts[row_index,matching_cols])
			faculty_counts_summary[row_index,4] <- paste0(100*round(faculty_counts_summary[row_index,1]/total_faculty,3), "%")
		}
	}
	# tt
	matching_cols <- which(grepl("on-tenure", colnames(faculty_counts)))
	for (row_index in sequence(nrow(faculty_counts_summary))) {
		if(length(matching_cols)>=1) {
			faculty_counts_summary[row_index,2] <- sum(faculty_counts[row_index,matching_cols])
			faculty_counts_summary[row_index,5] <- paste0(100*round(faculty_counts_summary[row_index,2]/total_faculty,3), "%")
		}
	}
	# ntt
	matching_cols <- which(grepl("no tenure", colnames(faculty_counts)))
	for (row_index in sequence(nrow(faculty_counts_summary))) {
		if(length(matching_cols)>=1) {
			faculty_counts_summary[row_index,3] <- sum(faculty_counts[row_index,matching_cols])
			faculty_counts_summary[row_index,6] <- paste0(100*round(faculty_counts_summary[row_index,3]/total_faculty,3), "%")
		}
	}
	return(faculty_counts_summary)
}

GetFacultyCountsByInstitution <- function(college_data) {
	results <- list()
	for (i in sequence(nrow(college_data))) {
		results[[i]] <- GetFacultyCounts(college_data[i,])
		if(i%%2==0) {
			cat("\r", i, " of ", nrow(college_data))
		}
	}	
	names(results) <- unlist(college_data$`Institution Name`)
	return(results)
}

AppendBiome <- function(comparison_table) {
	print("Appending biome data")
	locations <- comparison_table[,c("Longitude location of institution", "Latitude location of institution")]
	locations[,1] <- as.numeric(locations[,1])
	locations[,2] <- as.numeric(locations[,2])
	colnames(locations) <- c("lon", "lat")
	bad_locations <- which(is.na(locations$lon) | is.na(locations$lat))
	locations[bad_locations,] <- c(36, -84.3)
  locations.spatial <- sp::SpatialPointsDataFrame(coords=locations, data=locations)
  wwf <- speciesgeocodeR::WWFload(tempdir())
  mappedregions <- sp::over(locations.spatial, wwf)
  realms <- data.frame(code=c("AA", "AN", "AT", "IM", "NA", "NT", "OC", "PA"), realm=c("Australasia", "Antarctic", "Afrotropics", "IndoMalay", "Nearctic", "Neotropics", "Oceania", "Palearctic"), stringsAsFactors=FALSE)
  biomes <- c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrubland", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves")
  comparison_table$eco_name <- mappedregions$ECO_NAME
  comparison_table$biome <- biomes[mappedregions$BIOME]
  comparison_table$realm <- NA
  for (i in sequence(nrow(comparison_table))) {
    comparison_table$realm[i] <- realms$realm[which(realms$code==mappedregions$REALM)]
  }
  comparison_table$eco_name[bad_locations] <- NA
  comparison_table$biome[bad_locations] <- NA
  comparison_table$realm[bad_locations] <- NA
  return(comparison_table)
}

AppendBioclim <- function(college_data) {
	college_data$`Annual precipitation (inches)` <- NA
	college_data$`Coldest month min temp (F)` <- NA
	college_data$`Warmest month max temp (F)` <- NA
	for (row_index in sequence(nrow(college_data))) {
		latitude <- as.numeric(college_data[row_index, 'latitude'])
		longitude <- as.numeric(college_data[row_index, 'longitude'])
		bioclim_slice <-terra::extract(raster::getData(name="worldclim", var="bio", res=0.5, download=TRUE, lat=latitude, lon=longitude, path=tempdir()), data.frame(lon=longitude, lat=latitude))
		college_data$`Annual precipitation (inches)`[row_index] <- round(as.numeric(unname(bioclim_slice[1,12]))*0.0393701,1)
		college_data$`Coldest month min temp (F)`[row_index] <- round(32+0.1*as.numeric(unname(bioclim_slice[1,6]))*(9/5),1)
		college_data$`Warmest month max temp (F)`[row_index] <- round(32+0.1*as.numeric(unname(bioclim_slice[1,5]))*(9/5),1)
		# if(row_index%%2==0) {
		# 	cat("\r", row_index, " of ", nrow(college_data))
		# }
		cat("\r", row_index, " of ", nrow(college_data), ": ", paste0(college_data$`Institution Name`[row_index], " ", college_data$`State abbreviation`[row_index], ": ", college_data$`Annual precipitation (inches)`[row_index], " ", college_data$`Coldest month min temp (F)`[row_index], " ", college_data$`Warmest month max temp (F)`[row_index], "           "))

	}
	return(college_data)
}

AppendCrime <- function(college_data) {
	crimelist <- list(
		crimes = read.csv("data/oncampuscrime181920.csv"),
		arrests = read.csv("data/oncampusarrest181920.csv"),
		discipline =read.csv("data/oncampusdiscipline181920.csv")
	)
	for (i in sequence(length(crimelist))) {
		crimelist[[i]]$UnitID <- substr(crimelist[[i]]$UNITID_P,1,6)	
	}
	crimelist[['crimes']] <- crimelist[['crimes']] %>% group_by(UnitID) %>% summarise(
		RAPE18 = sum(RAPE18, na.rm=TRUE),
		RAPE19 = sum(RAPE19, na.rm=TRUE),
		RAPE20 = sum(RAPE20, na.rm=TRUE),
		FONDL18 = sum(FONDL18, na.rm=TRUE),
		FONDL19 = sum(FONDL19, na.rm=TRUE),
		FONDL20 = sum(FONDL20, na.rm=TRUE)
	)
	crimelist[['crimes']] <- data.frame(UnitID=crimelist[['crimes']]$UnitID, OnCampusReportedRapes3yr = crimelist[['crimes']]$RAPE18 + crimelist[['crimes']]$RAPE19 + crimelist[['crimes']]$RAPE20, OnCampusReportedFondling3yr = crimelist[['crimes']]$FONDL18 + crimelist[['crimes']]$FONDL19 + crimelist[['crimes']]$FONDL20)
	
	crimelist[['arrests']] <- crimelist[['arrests']] %>% group_by(UnitID) %>% summarise(
		WEAPON18 = sum(WEAPON18, na.rm=TRUE),
		WEAPON19 = sum(WEAPON19, na.rm=TRUE),
		WEAPON20 = sum(WEAPON20, na.rm=TRUE),
		DRUG18 = sum(DRUG18, na.rm=TRUE),
		DRUG19 = sum(DRUG19, na.rm=TRUE),
		DRUG20 = sum(DRUG20, na.rm=TRUE),
		LIQUOR18 = sum(LIQUOR18, na.rm=TRUE),
		LIQUOR19 = sum(LIQUOR19, na.rm=TRUE),
		LIQUOR20 = sum(LIQUOR20, na.rm=TRUE)
	)
	crimelist[['arrests']] <- data.frame(UnitID=crimelist[['arrests']]$UnitID, OnCampusReportedWeaponsArrests3yr = crimelist[['arrests']]$WEAPON18 + crimelist[['arrests']]$WEAPON19 + crimelist[['arrests']]$WEAPON20, OnCampusReportedDrugArrests3yr = crimelist[['arrests']]$DRUG18 + crimelist[['arrests']]$DRUG19 + crimelist[['arrests']]$DRUG20, OnCampusReportedLiquorArrests3yr = crimelist[['arrests']]$LIQUOR18 + crimelist[['arrests']]$LIQUOR19 + crimelist[['arrests']]$LIQUOR20)


	crimelist[['discipline']] <- crimelist[['discipline']] %>% group_by(UnitID) %>% summarise(
		WEAPON18 = sum(WEAPON18, na.rm=TRUE),
		WEAPON19 = sum(WEAPON19, na.rm=TRUE),
		WEAPON20 = sum(WEAPON20, na.rm=TRUE),
		DRUG18 = sum(DRUG18, na.rm=TRUE),
		DRUG19 = sum(DRUG19, na.rm=TRUE),
		DRUG20 = sum(DRUG20, na.rm=TRUE),
		LIQUOR18 = sum(LIQUOR18, na.rm=TRUE),
		LIQUOR19 = sum(LIQUOR19, na.rm=TRUE),
		LIQUOR20 = sum(LIQUOR20, na.rm=TRUE)
	)
	crimelist[['discipline']] <- data.frame(UnitID=crimelist[['discipline']]$UnitID, OnCampusReportedWeaponsDiscipline3yr = crimelist[['discipline']]$WEAPON18 + crimelist[['discipline']]$WEAPON19 + crimelist[['discipline']]$WEAPON20, OnCampusReportedDrugDiscipline3yr = crimelist[['discipline']]$DRUG18 + crimelist[['discipline']]$DRUG19 + crimelist[['discipline']]$DRUG20, OnCampusReportedLiquorDiscipline3yr = crimelist[['discipline']]$LIQUOR18 + crimelist[['discipline']]$LIQUOR19 + crimelist[['discipline']]$LIQUOR20)

	merged_data1 <- base::merge(crimelist[['crimes']], crimelist[['arrests']], by="UnitID", all.x=TRUE)
	merged_data2 <- base::merge(merged_data1, crimelist[['discipline']], by="UnitID", all.x=TRUE)
	college_data <- base::merge(college_data, merged_data2, by="UnitID", all.x=TRUE)
	college_data$`Total undergrad plus grad students` <- as.numeric(college_data$`Undergraduate enrollment`) + as.numeric(college_data$`Grad enrollment`)
	college_data$`Liquor discipline per student (3 yr avg)` <- (college_data$OnCampusReportedLiquorDiscipline3yr/college_data$`Total undergrad plus grad students`)/3
	college_data$`Liquor arrest per student (3 yr avg)` <- (college_data$OnCampusReportedLiquorArrests3yr/college_data$`Total undergrad plus grad students`)/3
	college_data$`Weapon discipline per student (3 yr avg)` <- (college_data$OnCampusReportedWeaponsDiscipline3yr/college_data$`Total undergrad plus grad students`)/3
	college_data$`Weapon arrest per student (3 yr avg)` <- (college_data$OnCampusReportedWeaponsArrests3yr/college_data$`Total undergrad plus grad students`)/3
	college_data$`Drug discipline per student (3 yr avg)` <- (college_data$OnCampusReportedDrugDiscipline3yr/college_data$`Total undergrad plus grad students`)/3
	college_data$`Drug arrest per student (3 yr avg)` <- (college_data$OnCampusReportedDrugArrests3yr/college_data$`Total undergrad plus grad students`)/3
	college_data$`Reported rape per student (3 yr avg)` <- (college_data$OnCampusReportedRapes3yr/college_data$`Total undergrad plus grad students`)/3
	college_data$`Reported fondling per student (3 yr avg)` <- (college_data$OnCampusReportedFondling3yr/college_data$`Total undergrad plus grad students`)/3	
	return(college_data)
}

GetColorFromPercentile <- function(x, direction=-1) {
	x <- as.integer(round(x))
	begin <- "red"
	end <- "blue"
	if(direction==1) {
		begin <- "blue"
		end <- "red"
	}
	colorVector <- colorRampPalette(c(begin, end))(101)
	colorchoice <- colorVector[x+1]
	if(nchar(colorchoice)!=7) {
		colorchoice <- "black"
	}
	return(colorchoice)
}


GetColorFromPercentileYellowPurple <- function(x, direction=-1) {
	x <- as.integer(round(x))
	begin <- 0.1
	end <- 1
	if(direction==1) {
		begin <- 0
		end <- 0.9
	}
	colorchoice <- viridisLite::plasma(n=101, begin=begin, end=end, direction=direction)[x+1]
	if(nchar(colorchoice)==9) {
		colorchoice <- substr(colorchoice, 1, nchar(colorchoice)-2)
	} else {
		colorchoice <- "black"
	}
	
	return(colorchoice)
}

PrependHttpsIfNeeded <- function(url) {
	if(substr(url, 1, 4)!="http") {
		url <- paste0("https://", url)
	}
	return(url)	
}

FirstNaOmit <- function(x) {
	return(dplyr::first(na.omit(x)))	
}




formatMe <- function(x, digits=0, prefix="") {
	if(is.na(x)) {
		return("")
	}
	x_sign <- sign(x)
	x <- abs(x)
	suffix <- ""
	if(x>1e12) {
		x <- x/1e12
		suffix <- " T"
	} else if(x>1e9) {
		x <- x/1e9
		suffix <- " B"
	} else if(x>1e6) {
		x <- x/1e6
		suffix <- " M"
	} 
	if(x < 10 && digits==0) {
		digits <- digits + 1	
	}
	return(paste0(ifelse(x_sign<0, "-", ""), prefix, format(round(as.numeric(x), digits), nsmall=digits, big.mark=","), suffix))
}


GetFieldsAndMajors <- function(comparison_table, ipeds_direct_and_db, CIPS_codes) {
	db <- dbConnect(RSQLite::SQLite(), "data/db_IPEDS.sqlite")

	completions_program <- tbl(db, "Completions_program") %>% as.data.frame()
	colnames(completions_program) <- gsub("  ", " ", gsub('\\.', ' ', gsub("^[A-z]+\\.[A-Z0-9_]+\\.", "", colnames(completions_program))))
	completions_program <- subset(completions_program, completions_program$`UNITID Unique identification number of the institution` != "z") #kill the placeholder
	completions_program$`IPEDS Year` <- as.numeric(completions_program$`IPEDS Year`)
	max_year <- max(completions_program$`IPEDS Year`)
	completions_program <- completions_program %>% dplyr::filter(`IPEDS Year`==max(completions_program$`IPEDS Year`)) %>% dplyr::filter(`First or Second Major`=="First major")

	comparison_table$`IPEDS Year` <- as.numeric(comparison_table$`IPEDS Year`)
	
	comparison_table <- comparison_table %>% dplyr::filter(`IPEDS Year`==max(comparison_table$`IPEDS Year`))
	comparison_table <- comparison_table %>% dplyr::select(-c("IPEDS Year"))

	completions_first_major <- dplyr::left_join(completions_program, comparison_table)
	completions_simple <- completions_first_major %>% dplyr::select(c(
		"CIP Code  2020 Classification",
		"CIPCODE ORIGINAL CODES",
		"Grand total",
		"Award Level code",
		"Institution entity name",
		"State",
		"UNITID Unique identification number of the institution",
		"Admission percentage total"
		)) %>%
		dplyr::rename(c(
			Field="CIP Code  2020 Classification",
			Code="CIPCODE ORIGINAL CODES",
			Completions="Grand total",
			Degree="Award Level code",
			Institution="Institution entity name",
			UNITID="UNITID Unique identification number of the institution",
			AdmissionRate = "Admission percentage total"
		))
	
	completions_simple$CodeBroad <- substr(completions_simple$Code, 1, 2)
		
	completions_simple <- completions_simple %>% dplyr::filter(Field!="Grand total")

	completions_simple$RejectionRate <- 100-as.numeric(completions_simple$AdmissionRate) # Since people want to see "selective" institutions first

	completions_simple$Degree[grepl("Doctor", completions_simple$Degree)] <- "Doctorate"

	completions_simple$Degree[grepl("Certificate", completions_simple$Degree, ignore.case=TRUE)] <- "Certificate"
	
	completions_simple$Degree[grepl("Associate", completions_simple$Degree, ignore.case=TRUE)] <- "Associates"
	
	completions_simple$Degree[grepl("Bachelor", completions_simple$Degree, ignore.case=TRUE)] <- "Bachelors"
	
	completions_simple$Degree[grepl("Master", completions_simple$Degree, ignore.case=TRUE)] <- "Masters"
	

	completions_simple$Completions <- as.numeric(completions_simple$Completions)

	

	completions_with_percentage <- completions_simple %>% dplyr::filter(Completions > 0) %>% dplyr::group_by(UNITID,Degree) %>% mutate(AllDegrees = sum(Completions)) %>% mutate('Percent at Institution' = round(100*Completions/AllDegrees,3)) %>% dplyr::filter(Completions > 0) %>% ungroup() %>% dplyr::group_by(Field,Degree) %>% mutate(AllByField = sum(Completions)) %>% mutate('Percent in Field' = round(100*Completions/AllByField,3)) %>% ungroup()


	completions_with_percentage$Institution <-  paste0("<a href='https://collegetables.info/", utils::URLencode(gsub(" ", "", completions_with_percentage$UNITID)),  ".html'>", completions_with_percentage$Institution, "</a>")

	completions_with_percentage <- completions_with_percentage %>% dplyr::select(c(
		Field,
		Institution,
		Completions,
		`Percent at Institution`,
		`Percent in Field`,
		Degree,
		RejectionRate,
		UNITID
	)) 

	completions_with_percentage <- completions_with_percentage[order(completions_with_percentage$RejectionRate,completions_with_percentage$`Percent at Institution`, decreasing=TRUE),] %>% dplyr::select(-RejectionRate)
	
	field_data <- completions_simple %>% dplyr::group_by(Field,Degree) %>% dplyr::summarise('Schools offering' = n(), 'Degrees completed' = sum(Completions))
	
	colnames(completions_with_percentage) <- gsub("Completions", paste0("Degrees awarded ", max_year), colnames(completions_with_percentage))
	
	dbWriteTable(db,  "Field_data", field_data, overwrite=TRUE)
	dbWriteTable(db, "Completions_with_percentage", completions_with_percentage, overwrite=TRUE)
	dbDisconnect(db)
	return(c('Field_data', 'Completions_with_percentage')) # just which tables they are
}

# From https://nces.ed.gov/ipeds/cipcode/resources.aspx?y=56. These are the explanations for what a field has, i.e.: 
# 01.0301	
#Agricultural Production Operations, General.	
# A program that focuses on the general planning, economics, and use of facilities, natural resources, equipment, labor, and capital to produce plant and animal products, and that may prepare individuals for work in farming, ranching, and agribusiness.

# These are revised every 10 years; the 2020 version is the most recent. Since the only way to download it is javascript, I just have a saved csv
GetCIPCodesExplanations <- function() {
	cip <- read.csv("data/CIPCode2020.csv", stringsAsFactors=FALSE)
	cip <- cip %>% dplyr::select(c("CIPFamily", "CIPCode", "CIPTitle", "CIPDefinition", "CrossReferences", "Examples"))
	cip$CIPTitle <- gsub("\\.$", "", cip$CIPTitle) # remove trailing period
	cip$CIPFamilyName <- cip$CIPTitle[1]
	previous_family=1
	for (row_index in 2:nrow(cip)) {
		current_family <- cip$CIPFamily[row_index]
		if (current_family != previous_family) {
			cip$CIPFamilyName[row_index] <- cip$CIPTitle[row_index]
			previous_family <- current_family
		} else {
			cip$CIPFamilyName[row_index] <- cip$CIPFamilyName[row_index-1]
		}
	}
	cip$CIPFamilyName <- stringr::str_to_title(cip$CIPFamilyName)
	cip <- cip[!grepl('Instructional content ', cip$CIPDefinition),] # eliminates cruft
	#cip <- cip[!duplicated(cip$CIPTitle),] # remove duplicates
	cip <- cip[!grepl("Reserved", cip$CIPFamilyName),] # remove reserved codes
	return(cip)
}

SummarizeDemographicsForPlotting <- function(subsection) {
	genders <- c("women", "men") #this is too narrow, but it reflects the only available ones in federal data
	gender_df <- data.frame()
	for (i in seq_along(genders)) {
		cols_to_get <- c("Grand total", colnames(subsection)[grepl(paste0(" ", genders[i]), colnames(subsection))])[-2]
		focal <- apply(subsection[,cols_to_get], 2, as.numeric)
		focal_percent <- 100*focal/focal[,1]
		focal_percent <- as.data.frame(focal_percent[,-1])
		focal_percent$Gender <- genders[i]
		focal_percent$Year <- subsection$Year
		colnames(focal_percent) <- gsub(paste0(" ", genders[i]), "", colnames(focal_percent))
		if(i==1) {
			gender_df <- focal_percent
		} else {
			gender_df <- rbind(gender_df, focal_percent)
		}
	}


	gender_df_tall <- tidyr::pivot_longer(gender_df, cols=colnames(gender_df)[!(colnames(gender_df) %in% c("Year", "Gender"))], names_to="Group", values_to="Percent")

	gender_df_tall$Group <- gsub("\\.", " ", gender_df_tall$Group)
	#gender_df_tall <- subset(gender_df_tall, gender_df_tall$Group != "White")
	gender_df_tall$Group <- gsub("American Indian or Alaska Native", "Native American", gender_df_tall$Group)
	gender_df_tall$Group <- gsub("Black or African American", "Black", gender_df_tall$Group)
	gender_df_tall$Group <- gsub("Native Hawaiian or Other Pacific Islander", "Pacific Islander", gender_df_tall$Group)
	gender_df_tall$Group <- gsub("Two or more races", "2+ races", gender_df_tall$Group)
	gender_df_tall$Group <- gsub("Race ethnicity unknown", "Unknown", gender_df_tall$Group)
	gender_df_tall$Group <- gsub( "Nonresident alien", "International", gender_df_tall$Group)



	gender_df_tall$Year <- as.integer(gender_df_tall$Year)
	#ggplot(gender_df_tall, aes(x=Year, y=Percent, group=Group)) + facet_grid(Group ~ Gender, scales="free", labeller = labeller(groupwrap = label_wrap_gen(10))) + geom_line() + theme_linedraw() + ylab("Percent of undergrads") + ylim(c(0, NA))
	#ggsave("~/Downloads/subsection.jpg", width=6, height=10)
	return(gender_df_tall)
}

GetWikipediaSummaryFirstParagraph <- function(institution) {
  URL <- paste0('https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&titles=', utils::URLencode(institution))
  text <- jsonlite::fromJSON(URL)$query$pages[[1]]$extract
  #to handle "U.S.:
  text <- gsub("U\\.S\\.A\\.", "United States of America", text)
  text <- gsub("U\\.S\\.", "United States", text)
  text <- paste0(strsplit(text, "\\.[A-Z]")[[1]][1], ".")
  return(text)
}

GetWeathersparkLinks <- function() {
	state_results <- data.frame()
	for(state in state.abb) {
		cat(state, "\r")
		URL <- paste0("https://weatherspark.com/countries/US/", state)
		state_result <- rvest::read_html(URL) %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "ListPage-columns_6", " " ))]') %>% html_elements("a")  %>% as.character() 
		state_location_urls <- state_result %>% stringr::str_extract_all('href="(.*)"') %>% unlist() %>% stringr::str_replace_all('href="', '') %>% stringr::str_replace_all('"', '')  %>% stringr::str_replace_all( " class=ListPage-f.*", "")
		state_location_names <- state_result %>% stringr::str_extract_all('>(.*)</a>') %>% unlist() %>% stringr::str_replace_all('</a>', '') %>% stringr::str_replace_all('>', '') 
		state_results <- rbind(state_results, data.frame(state_code=state, state_name=state.name[match(state,state.abb)], location=state_location_names, url=state_location_urls))
		Sys.sleep(5)
	}
	state_results_new <- state_results
	for (target_index in seq_along(state_results$url)) {
		if(target_index %% 10 == 0) {
			cat("\r",target_index)
		}
		target_url <- state_results$url[target_index]
		try({
			if(grepl("countries", target_url)) { # It's a county page; dig in
				URL <- paste0("https://weatherspark.com", target_url)
				
				county_result <- rvest::read_html(URL) %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "ListPage-columns_6", " " ))]') %>% html_elements("a")  %>% as.character()
				
				county_location_urls <- county_result %>% stringr::str_extract_all('href="(.*)"') %>% unlist() %>% stringr::str_replace_all('href="', '') %>% stringr::str_replace_all('"', '')  %>% stringr::str_replace_all( " class=ListPage-f.*", "")
				
				county_location_names <- county_result %>% stringr::str_extract_all('>(.*)</a>') %>% unlist() %>% stringr::str_replace_all('</a>', '') %>% stringr::str_replace_all('>', '') 
				
				if(length(county_location_names)>0 & length(county_location_urls)>0) {
					state_results_new <- rbind(state_results_new, data.frame(state_code=state_results$state_code[target_index], state_name=state_results$state_name[target_index], location=county_location_names, url=county_location_urls))
				}
				Sys.sleep(5)
			}
		}, silent=TRUE)
	}
	state_results_new <- state_results_new[!duplicated(state_results_new),]
	return(state_results_new)
}

GetClosestWeatherspark <- function(most_current_info, weatherspark) {
	matching_row <- data.frame()
	try(matching_row <- weatherspark[weatherspark$state_name == most_current_info['State'] & weatherspark$location == most_current_info['City location of institution'],])
	if(nrow(matching_row)!=0) {
		return(gsub('\\/\\/', '/', paste0('https://weatherspark.com/', matching_row$url[1], '#Figures-Summary')))
	}
	return(paste0('https://weatherspark.com/countries/US/', state.abb[match(most_current_info['State'],state.name)], '#Figures-Temperature'))
}

CreateIndexTable <- function(comparison_table) {
	index_conversions <- read.csv("data/ConversionToIndexTable.csv")
	index_conversions <- index_conversions[index_conversions$ColumnName!="Full-time undergrad enrollment trend",] # since that doesn't exist yet
	index_table <- data.frame()
	for (focal_row in sequence(nrow(index_conversions))) {
		focal_column <- comparison_table[, index_conversions$ColumnName[focal_row]]
		if(focal_row==1) {
			index_table <- data.frame(focal_column)	
		} else {
			index_table <- cbind(index_table, focal_column)
		}
		colnames(index_table)[ncol(index_table)] <- index_conversions$Rename[focal_row]
	}
	index_table <- index_table[order(index_table$`IPEDS Year`, decreasing=TRUE),]
	index_table_summary <- data.frame(matrix(NA, nrow=length(unique(index_table$UNITID)), ncol=ncol(index_table)))
	colnames(index_table_summary) <- colnames(index_table)
	enrollment_slopes <- rep("", length(unique(index_table$UNITID)))
	unique_UNITIDs <- unique(index_table$UNITID)
	
	# for debugging: 
	#unique_UNITIDs <- unique_UNITIDs[1:50]
	
	for (UNITID_index in seq_along(unique_UNITIDs)) {
		UNITID_number <- unique_UNITIDs[UNITID_index]
		focal <- apply(subset(index_table, index_table$UNITID==UNITID_number), 2, FirstNaOmit)
		index_table_summary[UNITID_index,] <- focal
		enrollment_data <- subset(index_table, index_table$UNITID==UNITID_number) %>% dplyr::select('Undergrad full time', 'IPEDS Year') %>% dplyr::rename('Enrollment'='Undergrad full time', 'Year'='IPEDS Year') %>% dplyr::mutate(Enrollment=as.numeric(Enrollment)) %>% dplyr::mutate(Year=as.numeric(Year)) %>% dplyr::filter(!is.na(Enrollment)) %>% dplyr::filter(!is.na(Year))
		try({
			if(nrow(enrollment_data)>1) {
				fit <- stats::lm(Enrollment ~ Year, data=enrollment_data)
				if(summary(fit)$coefficients[2,4]<0.05) {
					slope <- round(summary(fit)$coefficients[2,1],0)
					change <- c('Increasing', 'gaining')
					if(sign(summary(fit)$coefficients[2,1])<0) {
						change <- c('Decreasing', "losing")
					}
					#CI <- round(unname(confint(fit)[2,]),0)
					#enrollment_slopes[UNITID_index] <- paste0(change[1], " (", change[2]," ", abs(slope), " full-time undergrads per year)")
					enrollment_slopes[UNITID_index] <- change[1]
				} else {
					enrollment_slopes[UNITID_index] <- 'Approximately stable'	
				}
			}
		})
		cat("  ", UNITID_index, "\r")
	}
	
	index_table_summary <- cbind(index_table_summary, enrollment_slopes)
	
	colnames(index_table_summary)[ncol(index_table_summary)] <- 'Full-time undergrad enrollment trend'
	index_table_summary$`State rep support for contraception` <- 100*as.numeric(index_table_summary$`State rep support for contraception`)
	index_table_summary$`State support for interracial and same-sex marriage` <- 100*as.numeric(index_table_summary$`State support for interracial and same-sex marriage`)
	index_table_summary$`Students can get federal financial aid`[index_table_summary$`Students can get federal financial aid` %in% c("Branch campus of a main campus that participates in Title IV", "New participants (became eligible during spring collection)", "New participants (became eligible during winter collection)", "Participates in Title IV federal financial aid programs")] <- "Yes"
	index_table_summary$`Students can get federal financial aid`[index_table_summary$`Students can get federal financial aid` %in% c("Deferment only - limited participation", "Not currently participating in Title IV, does not have OPE ID number", "Not currently participating in Title IV, has an OPE ID number", "Stopped participating during the survey year")] <- "No"
	index_table_summary$`California travel ban` <- ifelse(index_table_summary$`California travel ban`==TRUE, "Yes", "No")
	index_table_summary$`California travel ban`[nchar(index_table_summary$`California travel ban`)<1] <- "No"
	index_table_summary$`Category` <- gsub(' \n', "", index_table_summary$`Category`)
	index_table_summary$`Category`[nchar(index_table_summary$`Category`)<1] <- "Unknown"

	index_table_summary$`Overall type` <- "Not accredited or non-degree granting"
	index_table_summary$`Overall type`[grepl("Four", index_table_summary$`Size and setting`)] <- "Four-year"
	index_table_summary$`Overall type`[grepl("Two", index_table_summary$`Size and setting`)] <- "Two-year"
	index_table_summary$`Overall type`[grepl("graduate", index_table_summary$`Size and setting`)] <- "Graduate/Professional"

	index_table_summary$`Full-time undergrad enrollment trend`[nchar(index_table_summary$`Full-time undergrad enrollment trend`)<2] <- "Unknown or not applicable"

	
	# now format
	for (focal_row in sequence(nrow(index_conversions))) {
		if(index_conversions$Type[focal_row]=="Percent") {
			index_table_summary[, index_conversions$Rename[focal_row]] <- round(as.numeric(index_table_summary[, index_conversions$Rename[focal_row]]), 0)
		}
		if(index_conversions$Type[focal_row]=="Money") {
			money <- as.numeric(index_table_summary[, index_conversions$Rename[focal_row]])
			index_table_summary[, index_conversions$Rename[focal_row]] <- round(money, 0)
		}
		if(index_conversions$Type[focal_row]=="Number") {
			index_table_summary[, index_conversions$Rename[focal_row]] <- round(as.numeric(index_table_summary[, index_conversions$Rename[focal_row]]), 0)
		}
	}
	
	#index_table_summary[is.na(index_table_summary)] <- ""
	index_table_summary[index_table_summary=="Not applicable"] <- ""
	
	index_table_summary$pseudoranking <- (100-as.numeric.na0(index_table_summary$`Admission percentage total`)) + as.numeric.na0(index_table_summary$`Grad rate in six years`) + as.numeric.na0(index_table_summary$`Yield percentage total`) + log1p(as.numeric.na0(index_table_summary$`Undergrad full time`)) + log1p(as.numeric.na0(index_table_summary$`Tenure-stream faculty`))
	index_table_summary$pseudoranking[is.na(index_table_summary$pseudoranking)] <- 0
	index_table_summary <- index_table_summary[order(index_table_summary$pseudoranking, decreasing=TRUE),]
	
	return(index_table_summary)
}

URLEncodeTotal <- function(x) {
	return(utils::URLencode(gsub('-', '_', gsub(',', '_', gsub(' ', '_', gsub('&', '_', gsub('/', '_', x))))), reserved=TRUE))
}

RenderFieldPages <- function(CIPS_codes, fields_and_majors, yml) {
	CIP_headers <- CIPS_codes[!duplicated(CIPS_codes$CIPFamily),]
	CIP_content <- CIPS_codes[duplicated(CIPS_codes$CIPFamily),]
	for (header_index in sequence(nrow(CIP_headers))) {
		CIP_code <- CIP_headers$CIPFamily[header_index]
		rmarkdown::render(
			input="_fields.Rmd", 
			output_file="docs/field.html",
			params = list(
				field_name = CIP_headers$CIPFamilyName[header_index],
				cips_headers = subset(CIP_headers, CIP_headers$CIPFamily==CIP_code),
				cips_content = subset(CIP_content, CIP_content$CIPFamily==CIP_code)
			),
			quiet=TRUE
		)
		#Sys.sleep(1)
		system("sed -i '' 's/&gt;/>/g' docs/field.html") # because htmlTable doesn't escape well; the '' is a requirement of OS X's version of sed, apparently
		system("sed -i '' 's/&lt;/</g' docs/field.html")
			


		file.copy("docs/field.html", paste0("docs/", URLEncodeTotal(CIP_headers$CIPFamilyName[header_index]), ".html"), overwrite=TRUE)
	}
}

CreateYMLForSite <- function(CIPS_codes) {
	file.copy("CNAME", "docs/CNAME", overwrite=TRUE)
	file.copy("GA_Script.html", "docs/GA_Script.html", overwrite=TRUE)
	yml <- 'name: "College Tables"
navbar:
  title: "College Tables"
  left:
    - text: "Home"
      href: index.html
    - text: "About"
      href: about.html
    - text: "Fields"
      href: fields_overview.html'
	
	# yml <- paste0(yml, '    - text: "Fields"
    #   menu: '
	# CIP_headers <- CIPS_codes[!duplicated(CIPS_codes$CIPFamily),]

	# for (CIP_index in sequence(nrow(CIP_headers))) {
	# 	yml <- paste0(yml, '
    #     - text: "', CIP_headers$CIPFamilyName[CIP_index], '"
    #       href: "', URLEncodeTotal(CIP_headers$CIPFamilyName[CIP_index]), '.html"', collapse="")
	# }
	yml <- paste0(yml, '	
output_dir: "docs"
', collapse="")
	write(yml, "_site.yml")
	file.copy("_site.yml", "docs/_site.yml", overwrite=TRUE)	
	return(TRUE)
}

# From https://glin.github.io/reactable/articles/custom-filtering.html

dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
  function(values, name) {
    dataListId <- sprintf("%s-%s-list", tableId, name)
    tagList(
      tags$input(
        type = "text",
        list = dataListId,
        oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
        "aria-label" = sprintf("Filter %s", name),
        style = style
      ),
      tags$datalist(
        id = dataListId,
        lapply(unique(values), function(value) tags$option(value = value))
      )
    )
  }
}