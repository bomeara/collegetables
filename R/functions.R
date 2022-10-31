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

GetIPEDSDirectly <- function(years=c(2021, 2020, 2018, 2016, 2014, 2012), IPEDS_names = GetIPEDSNames()) {
  # Get IPEDS data directly from the IPEDS website
  # Input: years to download
  # Output: array with IPEDS data
  # Replace 'zzzz' with the year
  # Replace 'xxyy' with the last two digits of this year and the previous year: 1920 for 2019-20, 1819 for 2018-19, etc.
  # Source: https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=2018&surveyNumber=-1 
  # I am using the category and then the first relevant word or so of the description (but things like "gender" might actually be "gender, ethnicity, and age")
 
  all_results <- list()
  for (year_index in seq_along(years)) {
	zzzz <- years[year_index]
	xxyy <- paste0(-1+as.numeric(substr(zzzz, 3, 4)), substr(zzzz, 3, 4))
	IPEDS_names_local <- IPEDS_names
	IPEDS_names_local <- gsub("zzzz", zzzz, IPEDS_names_local)
	IPEDS_names_local <- gsub("xxyy", xxyy, IPEDS_names_local)
	focal_year_df <- data.frame()
	for (IPEDS_index in seq_along(IPEDS_names_local)) {
		try({
			# Get the file with raw data
			file_to_get <- paste0('https://nces.ed.gov/ipeds/datacenter/data/', IPEDS_names_local[IPEDS_index],'.zip')
			temp <- tempfile()
			download.file(file_to_get,temp)
			unzip(temp, exdir = file.path(tempdir(), IPEDS_names_local[IPEDS_index]))
			potential_files <- list.files(file.path(tempdir(), IPEDS_names_local[IPEDS_index]), pattern=".*csv", full.names = TRUE)
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
			#print(paste0("Read ", IPEDS_names_local[IPEDS_index], " for ", zzzz, ". Its size is:"))
			print(dim(local_data))
			
			# Get the dictionary converting codes to meaning
			dictionary_to_get <- paste0('https://nces.ed.gov/ipeds/datacenter/data/', IPEDS_names_local[IPEDS_index],'_Dict.zip')
			temp2 <- tempfile()
			
			download.file(dictionary_to_get,temp2)
			unzip(temp2, exdir = file.path(tempdir(), IPEDS_names_local[IPEDS_index]))
			potential_files <- list.files(file.path(tempdir(), IPEDS_names_local[IPEDS_index]), pattern=".*xlsx", full.names = TRUE)
			dictionary_to_read <- potential_files[1]
			dictionary_frequencies <- NULL
			dictionary_variables <- NULL
			try({dictionary_frequencies <- readxl::read_excel(dictionary_to_read, sheet = "Frequencies")})
			dictionary_variables <- readxl::read_excel(dictionary_to_read, sheet = "varlist")
			unlink(temp2)
			
			# Ok, now we have the raw data and the dictionary. Let's convert the raw data to be more sensible
			try({
				unique_varnames_to_encode <- unique(dictionary_frequencies$varname)
				for (varname_index in seq_along(unique_varnames_to_encode)) {
					focal_freq <- subset(dictionary_frequencies, varname == unique_varnames_to_encode[varname_index])
					local_data[, unique_varnames_to_encode[varname_index]] <- focal_freq$valuelabel[match(c(as.character(local_data[, unique_varnames_to_encode[varname_index]])),as.character(focal_freq$codevalue))]
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

			if(IPEDS_index==1) {
				focal_year_df <- local_data
			} else {
				if(max(table(local_data[,"UNITID Unique identification number of the institution"]))>1) {
					# IPEDS is weird. Sometimes they have multiple rows for the same institution. So we split them up based on the factors used (the second column)
					unique_second_elements <- unique(local_data[,2])
					unique_second_elements <- unique_second_elements[!is.na(unique_second_elements)]
					wider_local_data <- data.frame() 
					for (unique_second_element_index in seq_along(unique_second_elements)) {
						local_data_slice <- base::subset(local_data, local_data[,2] %in% unique_second_elements[unique_second_element_index])
						local_data_slice <- local_data_slice[,-2]
						colnames(local_data_slice)[2:ncol(local_data_slice)] <- paste0(colnames(local_data_slice)[2:ncol(local_data_slice)], " ", unique_second_elements[unique_second_element_index])
						if(unique_second_element_index==1) {
							wider_local_data <- local_data_slice
						} else {
							wider_local_data <- dplyr::full_join(wider_local_data, local_data_slice, by="UNITID Unique identification number of the institution")
						}
					}	
					colnames(wider_local_data)[-1] <- paste0(IPEDS_names[IPEDS_index], " ", colnames(wider_local_data)[-1])
					focal_year_df <- dplyr::full_join(focal_year_df, wider_local_data, by = "UNITID Unique identification number of the institution")
					print(paste("Doing wider join for", names(IPEDS_names_local)[IPEDS_index]))
					print(IPEDS_index)
					print(dim(focal_year_df))
				} else {
					colnames(local_data)[-1] <- paste0(IPEDS_names[IPEDS_index], " ", colnames(local_data)[-1])
					focal_year_df <- dplyr::full_join(focal_year_df, local_data, by = "UNITID Unique identification number of the institution")
					print(paste("Doing normal join for", names(IPEDS_names_local)[IPEDS_index]))
					print(IPEDS_index)
					print(dim(focal_year_df))
					
				}
			}
			#save(focal_year_df, file=paste0("~/Downloads/focal_year_df_", zzzz, ".RData"))

		})
  	}
	all_results[[year_index]] <- focal_year_df[,grepl(" ", colnames(focal_year_df))] #names that weren't in the dictionary are dropped
	all_results[[year_index]]$focal_year <- zzzz
	save(all_results, file = paste0("~/Downloads/IPEDS_all.RData"))
	names(all_results)[year_index] <- years[year_index]
  }
  return(all_results)
} 

GetIPEDSDirectlyTry2 <- function(years=2021:2010, IPEDS_names = GetIPEDSNames()) {
	print("Starting GetIPEDSDirectlyTry2")
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
			try({dictionary_frequencies <- readxl::read_excel(dictionary_to_read, sheet = "Frequencies")})
			dictionary_variables <- readxl::read_excel(dictionary_to_read, sheet = "varlist")
			unlink(temp2)
			
			# Ok, now we have the raw data and the dictionary. Let's convert the raw data to be more sensible
			try({
				#print(object.size(x=lapply(ls(), get)), units="Gb")
				unique_varnames_to_encode <- unique(dictionary_frequencies$varname)
				for (varname_index in seq_along(unique_varnames_to_encode)) {
					focal_freq <- subset(dictionary_frequencies, varname == unique_varnames_to_encode[varname_index])
					local_data[, unique_varnames_to_encode[varname_index]] <- focal_freq$valuelabel[match(c(as.character(local_data[, unique_varnames_to_encode[varname_index]])),as.character(focal_freq$codevalue))]
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
			try({
				dbWriteTable(db,  names(IPEDS_names)[ipeds_base_index], local_df, overwrite=TRUE)
			})
		}
	}
	dbDisconnect(db)
	return(unique(working_files))
}

FixDuplicateColnames <- function(df) {
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
	return(df)
}

AggregateForOneInstitution <- function(institution_id, db) {
	
	institution_id <- as.character(institution_id)
	local_tables <- list()
	
	institutional_directory <- tbl(db, "Institutional_directory") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(institutional_directory) <- gsub("HDzzzz\\.[A-Z0-9]+\\.", "", colnames(institutional_directory))

	
	institutional_directory$latest_year <- FALSE # flag for the latest year we have data for for this institution for this field
	institutional_directory$latest_year[which.max(institutional_directory$`IPEDS.Year`)] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- institutional_directory
	names(local_tables)[length(local_tables)] <- "Institutional_directory"
	
	
	# Enrollment
	
	twelve_month_headcount <- tbl(db, "Twelve_month_headcount") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	twelve_month_headcount_detail <- twelve_month_headcount %>% dplyr::filter(!is.na(`EFFYzzzz.EFFYALEV.Level.and.degree.certificate.seeking.status.of.student`)) %>% dplyr::filter(`EFFYzzzz.EFFYALEV.Level.and.degree.certificate.seeking.status.of.student` %in% c('All students total', 'All students, Undergraduate total', 'Full-time students, Undergraduate total' ))
	
	twelve_month_headcount_vague <- twelve_month_headcount %>% dplyr::filter(is.na(`EFFYzzzz.EFFYALEV.Level.and.degree.certificate.seeking.status.of.student`))
	
	twelve_month_headcount_merged <- dplyr::bind_rows(twelve_month_headcount_detail, twelve_month_headcount_vague)

	last_col_to_include <- which(grepl("IPEDS.Year", colnames(twelve_month_headcount_merged)))
	
	twelve_month_headcount_merged <- twelve_month_headcount_merged[,1:last_col_to_include]
	
	colnames(twelve_month_headcount_merged) <- gsub("EFFYzzzz\\.[A-Z0-9]+\\.", "", colnames(twelve_month_headcount_merged))
	
	twelve_month_headcount_merged$`Original.level.of.study.on.survey.form`[which(twelve_month_headcount_merged$`Level.and.degree.certificate.seeking.status.of.student` == "Full-time students, Undergraduate total")] <- "Undergraduate Full-time"
	 
	twelve_month_headcount_merged <- twelve_month_headcount_merged %>% dplyr::select(-c(`Level.and.degree.certificate.seeking.status.of.student`, `Undergraduate.or.graduate.level.of.student`)) %>% dplyr::rename("Student.level" = `Original.level.of.study.on.survey.form` ) %>% dplyr::filter(!is.na(`Grand.total`))
	
	twelve_month_headcount_merged$latest_year <- FALSE
	twelve_month_headcount_merged$latest_year[which(twelve_month_headcount_merged$`IPEDS.Year` == max(twelve_month_headcount_merged$`IPEDS.Year`))] <- TRUE
	
	
	
	local_tables[[length(local_tables)+1]] <- twelve_month_headcount_merged %>% pivot_longer(cols=c(ends_with("men"), ends_with("total")))
	names(local_tables)[length(local_tables)] <- "Twelve_month_headcount"
	
	# People completing the programs, looking at just first majors (which can include people getting doctorates, masters, bachelor's, associates, etc.)
	
	completions_program_first_major <- tbl(db, "Completions_program") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% dplyr::filter(`Czzzz_A.MAJORNUM.First.or.Second.Major`=='First major') %>% dplyr::select(c(`UNITID.Unique.identification.number.of.the.institution`,`Czzzz_A.CIPCODE.CIP.Code....2020.Classification`, `Czzzz_A.CTOTALT.Grand.total`, `IPEDS.Year`)) %>% dplyr::rename(Subject=`Czzzz_A.CIPCODE.CIP.Code....2020.Classification`, `Total.graduates`=`Czzzz_A.CTOTALT.Grand.total`) %>% dplyr::group_by(`UNITID.Unique.identification.number.of.the.institution`, Subject, IPEDS.Year) %>% dplyr::summarise(`Total.graduates`=sum(`Total.graduates`)) %>% dplyr::filter(!is.na(Subject)) %>% as.data.frame()
	
	completions_program_first_major$latest_year <- FALSE
	completions_program_first_major$latest_year[which(completions_program_first_major$IPEDS.Year==max(completions_program_first_major$IPEDS.Year))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- completions_program_first_major
	names(local_tables)[length(local_tables)] <- "Completions_program_first_major"
	
	# Admissions
	
	admissions <- tbl(db, "Admissions") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(admissions) <- gsub("ADMzzzz\\.[A-Z0-9]+\\.", "", colnames(admissions))
	admissions$Admission.percentage.total <- 100*as.numeric(admissions$`Admissions.total`)/as.numeric(admissions$`Applicants.total`)
	admissions$Admission.percentage.women <- 100*as.numeric(admissions$`Admissions.women`)/as.numeric(admissions$`Applicants.women`)
	admissions$Admission.percentage.men <- 100*as.numeric(admissions$`Admissions.men`)/as.numeric(admissions$`Applicants.men`)

	admissions$Yield.percentage.total <- 100*as.numeric(admissions$`Enrolled.total`)/as.numeric(admissions$`Admissions.total`)
	admissions$Yield.percentage.women <- 100*as.numeric(admissions$`Enrolled..women`)/as.numeric(admissions$`Admissions.women`) # yes, the .. in the column name is correct
	admissions$Yield.percentage.men <- 100*as.numeric(admissions$`Enrolled..men`)/as.numeric(admissions$`Admissions.men`)
	
	admissions$latest_year <- FALSE
	admissions$latest_year[which(admissions$`IPEDS.Year`==max(admissions$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- admissions
	names(local_tables)[length(local_tables)] <- "Admissions"

	# Geographic source of students
	
	enrollment_residence <- tbl(db, "Enrollment_residence") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(enrollment_residence) <- gsub("[A-z]+\\.[A-Z0-9]+\\.", "", colnames(enrollment_residence))
	
	enrollment_residence$latest_year <- FALSE
	enrollment_residence$latest_year[which(enrollment_residence$`IPEDS.Year`==max(enrollment_residence$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- enrollment_residence
	names(local_tables)[length(local_tables)] <- "Enrollment_residence"	
	
	# Ages of students
	
	# note this is a way of getting full time and part time undergrads and grad students 
	
	enrollment_age <- tbl(db, "Enrollment_age") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% dplyr::filter(`EFzzzzB.LINE.Original.line.number.on.survey.form`!="Generated record not on survey form") %>% as.data.frame()
	
	colnames(enrollment_age) <- gsub("[A-z]+\\.[A-Z0-9]+\\.", "", colnames(enrollment_age))

	enrollment_age$latest_year <- FALSE
	enrollment_age$latest_year[which(enrollment_age$`IPEDS.Year`==max(enrollment_age$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- enrollment_age
	names(local_tables)[length(local_tables)] <- "Enrollment_age"
	
	# Race/ethnicity
	
	enrollment_race <- tbl(db, "Enrollment_race") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(enrollment_race) <- gsub("[A-z]+\\.[A-Z0-9]+\\.", "", colnames(enrollment_race))

	enrollment_race$latest_year <- FALSE
	enrollment_race$latest_year[which(enrollment_race$`IPEDS.Year`==max(enrollment_race$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- enrollment_race
	names(local_tables)[length(local_tables)] <- "Enrollment_race"
	
	
		
	# Finances
	

	finance_fasb <- tbl(db, "Finance_FASB") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	colnames(finance_fasb) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(finance_fasb))

		
	finance_gasb <- tbl(db, "Finance_GASB") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	colnames(finance_gasb) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(finance_gasb))
	
	finance_forprofit <- tbl(db, "Finance_ForProfits") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	colnames(finance_forprofit) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(finance_forprofit))
		
	finance <- finance_fasb
	if(nrow(finance_gasb)>0) { 
		finance <- finance_gasb
	}
	if(nrow(finance_forprofit)>0) { 
		finance <- finance_forprofit
	}
	
	finance$latest_year <- FALSE
	finance$latest_year[which(finance$`IPEDS.Year`==max(finance$`IPEDS.Year`))] <- TRUE
	

	
	local_tables[[length(local_tables)+1]] <- finance
	names(local_tables)[length(local_tables)] <- "Finance"
	
	# Institutional offerings (info on room and board, freshmen required to live on campus, students with disabilities, etc.)
	
	institutional_offerings <- tbl(db, "Institutional_offerings") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	colnames(institutional_offerings) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(institutional_offerings))
	
	institutional_offerings$latest_year <- FALSE
	institutional_offerings$latest_year[which(institutional_offerings$`IPEDS.Year`==max(institutional_offerings$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- institutional_offerings
	names(local_tables)[length(local_tables)] <- "Institutional_offerings"

	# Student aid
	
	student_aid <- tbl(db, "Student_aid") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(student_aid) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(student_aid))
	
	student_aid$latest_year <- FALSE
	student_aid$latest_year[which(student_aid$`IPEDS.Year`==max(student_aid$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- student_aid
	names(local_tables)[length(local_tables)] <- "Student_aid"

	
	# Staff category: by occupation, gender, ethnicity, etc. Pruned to full time only
	
	staff_category <- tbl(db, "Staff_category") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% dplyr::filter(`Szzzz_OC.FTPT.Full.time.or.part.time.status`=="Full-time") %>% as.data.frame()
	
	colnames(staff_category) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(staff_category))
	
	staff_category$latest_year <- FALSE
	staff_category$latest_year[which(staff_category$`IPEDS.Year`==max(staff_category$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- staff_category
	names(local_tables)[length(local_tables)] <- "Staff_category"

	# Staff gender: instructional staff by demographics, tenure status, rank. # IMPORTANT TABLE, so RENAMING to 
	# staff_tenure_demographics
	
	staff_tenure_demographics <- tbl(db, "Staff_gender") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(staff_tenure_demographics) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(staff_tenure_demographics))
	
	staff_tenure_demographics$latest_year <- FALSE
	staff_tenure_demographics$latest_year[which(staff_tenure_demographics$`IPEDS.Year`==max(staff_tenure_demographics$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- staff_tenure_demographics
	names(local_tables)[length(local_tables)] <- "Staff_tenure_demographics"
	
	# Useful for plotting: 
	# staff_tenure_demographics[which(staff_tenure_demographics$`Faculty.and.tenure.status`=='With faculty status, tenured' & staff_tenure_demographics$`Academic.rank`=="All ranks"),]
	
	# Staff new: by occupation, gender, ethnicity, etc. Good to combine with staff_category to look at turnover year by year (number in year Y = number in year Y-1 + number hired in year Y - number left in year Y)
	
	staff_new <- tbl(db, "Staff_new") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(staff_new) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(staff_new))
	
	staff_new$latest_year <- FALSE
	staff_new$latest_year[which(staff_new$`IPEDS.Year`==max(staff_new$`IPEDS.Year`))] <- TRUE
	
	local_tables[[length(local_tables)+1]] <- staff_new
	names(local_tables)[length(local_tables)] <- "Staff_new"

	# Staff tenure status: redundant with other tables
	
	# Academic library
	
	academic_library <- tbl(db, "Academic_library") %>% dplyr::filter(`UNITID.Unique.identification.number.of.the.institution`==institution_id) %>% as.data.frame()
	
	colnames(academic_library) <- gsub("[A-z_0-9]+\\.[A-Z0-9]+\\.", "", colnames(academic_library))
	
	academic_library$latest_year <- FALSE
	academic_library$latest_year[which(academic_library$`IPEDS.Year`==max(academic_library$`IPEDS.Year`))] <- TRUE
	
	academic_library <- dplyr::select(academic_library, c(
		`UNITID.Unique.identification.number.of.the.institution`,
		`Number.of.physical.books`,
		`Number.of.digital.electronic.books`,
		`Number.of.physical.media`,
		`Number.of.physical.serials`,
		`Number.of.electronic.serials`,
		`Total.physical.library.circulations..books.and.media.`,
		`Total.digital.electronic.circulations..books.and.media.`,
		`Total.expenditures..salaries.wages..benefits..materials.services..and.operations.maintenance.`,
		`One.time.purchases.of.books..serial.backfiles..and.other.materials`,
		`Ongoing.commitments.to.subscriptions`,
		`IPEDS.Year`,
		`latest_year`
		)
	)
	
	local_tables[[length(local_tables)+1]] <- academic_library
	names(local_tables)[length(local_tables)] <- "Academic_library"
	
	single_row_per_year_tables <- which(sapply(local_tables, function(x) !any(duplicated(x$`IPEDS.Year`))))
	
	joint <- FixDuplicateColnames(local_tables[[single_row_per_year_tables[1]]] %>% dplyr::select(-`latest_year`))
	for (table_join_index in single_row_per_year_tables[-1]) {
		joint <- dplyr::full_join(joint, dplyr::select(FixDuplicateColnames(local_tables[[table_join_index]]),-`latest_year`), by=c("UNITID.Unique.identification.number.of.the.institution", "IPEDS.Year"))
	}
	
	# all_years <- unique(unlist(sapply(local_tables, function(x) unique(x$`IPEDS.Year`))))
	# wide_tables <- list()
	
	# for (table_index in sequence(length(local_tables))){
	# 	if(max(table(local_tables[[table_index]]$`IPEDS.Year`))>1){
	# 		local_data <- local_tables[[table_index]]
	# 	}
	# }
	
	# To make lookup easier, we'll add the various filters for institution binning to the tables
	for (table_index in sequence(length(local_tables))){
		if(names(local_tables)[table_index] != "Institutional_directory") {
			local_tables[[table_index]]$Sector.of.institution <- institutional_directory$Sector.of.institution[which(institutional_directory$latest_year)]
			local_tables[[table_index]]$Level.of.institution <- institutional_directory$Level.of.institution[which(institutional_directory$latest_year)]
			local_tables[[table_index]]$Historically.Black.College.or.University <- institutional_directory$Historically.Black.College.or.University[which(institutional_directory$latest_year)]
			local_tables[[table_index]]$Tribal.College <- institutional_directory$Tribal.College[which(institutional_directory$latest_year)]
			local_tables[[table_index]]$State.abbreviation <- institutional_directory$State.abbreviation[which(institutional_directory$latest_year)]
		}
		if(names(local_tables)[table_index] != "Institutional_offerings") {
			local_tables[[table_index]]$conference.number.cross.country.track <- institutional_offerings$conference.number.cross.country.track[which(institutional_offerings$latest_year)]
		}

		dbWriteTable(db,  paste0(names(local_tables)[table_index], "_cleaned"), local_tables[[table_index]], overwrite=FALSE)
	}
	
	return(TRUE)
}

AggregateForAllInstitutions <- function(ipeds_direct_and_db){
	db <- dbConnect(RSQLite::SQLite(), "data/db_IPEDS.sqlite")
	institutional_names <- tbl(db, "Institutional_directory")  %>% as.data.frame() 
	institutions <- unique(institutional_names$`UNITID.Unique.identification.number.of.the.institution`)
	rm(institutional_names)
	institutions <- institutions[which(institutions!="z")] # remove the dummy row
	
	
	for (institution_index in sequence(length(institutions))){
		AggregateForInstitution(db, institutions[institution_index])
	}
	
	dbDisconnect(db)
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

AppendVaccination <- function(college_data) {
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

AppendMisconduct <- function(college_data) {
	asmd <- misconduct::get_misconduct(agree=TRUE)
	asmd_colleges <- unique(asmd$Institution)
	college_data$InMisconductDatabase <- "No"
	for (i in seq_along(asmd_colleges)) {
		distances <- adist(asmd_colleges[i], college_data$"Institution Name")
		matches <- which(distances == min(distances))
		college_data$InMisconductDatabase[matches] <- "Yes"
	}	
	return(college_data)
}

AppendAbortion <- function(college_data) {
	#abortion <- read.csv("data/abortion.csv", header=TRUE)
	#colnames(abortion) <- gsub("\\.", " ", colnames(abortion))
	abortion <- read.csv("data/worldpopulationreview_abortion.csv")
	abortion$`State abbreviation` <- state.abb[match(abortion$State,state.name)]
	abortion_simple <- dplyr::select(abortion, c("State abbreviation", "Status"))
	colnames(abortion_simple)[2] <- "Abortion"
	return(left_join(college_data, abortion_simple, by="State abbreviation"))
}

AppendGunLaws <- function(college_data) {
	guns <- read.csv("data/gunlaws_giffords_scorecard.csv")
	guns$`State abbreviation` <- state.abb[match(guns$State,state.name)]
	guns_simple <- dplyr::select(guns, c("State abbreviation", "grade2022"))
	colnames(guns_simple)[2] <- "Gun law stringency"
	return(left_join(college_data, guns_simple, by="State abbreviation"))
}

AppendAAUPCensure <- function(college_data) {
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

AppendMarriageRespect <- function(college_data) {
	marriage_defense <- read.csv("data/MarriageAct_HR8404_2022.csv", header=TRUE)
	marriage_aggregated <- AggregateVotesByState(marriage_defense)
	marriage_aggregated$`State abbreviation` <- state.abb[match(marriage_aggregated$State,state.name)]
	marriage_aggregated_simple <- dplyr::select(marriage_aggregated, c("State abbreviation", "ProportionVotesInFavor"))
	colnames(marriage_aggregated_simple)[2] <- "Proportion of reps voting in favor of respect for marriage act"
	return(left_join(college_data, marriage_aggregated_simple, by="State abbreviation"))
}

AppendContraceptiveSupport <- function(college_data) {
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
	college_data_enhanced$URL <- paste0(  utils::URLencode(gsub(" ", "", college_data_enhanced$ShortName)), ".html")
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

RenderInstitutionPages <- function(overview, degree_granting, maxcount=30, students_by_state_by_institution, student_demographics, faculty_counts, spark_width, spark_height) {	
	institutions <- unique(degree_granting$ShortName)
	failures <- c()
	try(system("rm all_colleges.rda"), silent=TRUE)
	#for (i in seq_along(institutions)) {
	for (i in sequence(min(maxcount, length(institutions)))) {
		failed <- TRUE
		try({
			print(institutions[i])
			if(is.null(student_demographics[[institutions[i]]])) {
				print("No data for " + institutions[i])
				stop()
			}
			rmarkdown::render(
				input="_institution.Rmd", 
				output_file="docs/institution.html", 
				params = list(
					institution_name = institutions[i],
					institution_long_name = degree_granting$ShortName[i],
					overview_table = subset(overview, overview$ShortName == institutions[i]),
					raw_table = degree_granting,
					students_by_state_by_institution = students_by_state_by_institution,
					student_demographics = student_demographics,
					faculty_counts = faculty_counts,
					spark_width = spark_width,
					spark_height = spark_height
				),
				quiet=TRUE
			)
			#Sys.sleep(1)
			system("sed -i '' 's/&gt;/>/g' docs/institution.html") # because htmlTable doesn't escape well; the '' is a requirement of OS X's version of sed, apparently
			system("sed -i '' 's/&lt;/</g' docs/institution.html")
			
			#system("sed -i '' 's/‘/\x27/g' docs/institution.html")
			#system("sed -i '' 's/’/\x27/g' docs/institution.html")
			#Sys.sleep(1)


			file.copy("docs/institution.html", paste0("docs/", utils::URLencode(gsub(" ", "", institutions[i])), ".html"))
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
			failures <- c(failures, institutions[i])
		}
	}
	load("all_colleges.rda")
	return(all_colleges)
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


			file.copy("docs/state.html", paste0("docs/", utils::URLencode(gsub(" ", "", states[i])), ".html"))
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
RenderIndexPageEtAl <- function(pages) {
	system("rm docs/index.html")
	system("rm docs/degrees_by*")
	rmarkdown::render(
				input="index.Rmd", 
				output_file="docs/index.html", 
				quiet=TRUE
	)
	
	rmarkdown::render(
				input="degrees_by_conference.Rmd", 
				output_file="docs/degrees_by_conference.html", 
				quiet=TRUE
	)
	
	rmarkdown::render(
				input="degrees_by_institution.Rmd", 
				output_file="docs/degrees_by_institution.html", 
				quiet=TRUE
	)
	
	rmarkdown::render(
				input="degrees_by_type.Rmd", 
				output_file="docs/degrees_by_type.html", 
				quiet=TRUE
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


### From my chapter2 package


#' Use azizka/speciesgeocodeR/ and WWF data to encode locations for habitat and biome
#'
#' Uses info from http://omap.africanmarineatlas.org/BIOSPHERE/data/note_areas_sp/Ecoregions_Ecosystems/WWF_Ecoregions/WWFecoregions.htm to convert codes to more readable text
#'
#' @param locations Data.frame containing points (latitude and longitude, perhaps other data as columns)
#' @return data.frame with columns for habitat and biome.
#' @export
#' @examples
#' locations <- spocc_taxon_query("Myrmecocystus", limit=50)
#' locations <- locality_clean(locations)
#' locations <- locality_add_habitat_biome(locations)
#' print(head(locations))
AppendBiome <- function(locations) {
  locations.spatial <- sp::SpatialPointsDataFrame(coords=locations[,c("longitude", "latitude")], data=locations)
  wwf <- speciesgeocodeR::WWFload(tempdir())
  mappedregions <- sp::over(locations.spatial, wwf)
  realms <- data.frame(code=c("AA", "AN", "AT", "IM", "NA", "NT", "OC", "PA"), realm=c("Australasia", "Antarctic", "Afrotropics", "IndoMalay", "Nearctic", "Neotropics", "Oceania", "Palearctic"), stringsAsFactors=FALSE)
  biomes <- c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrubland", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves")
  locations$eco_name <- mappedregions$ECO_NAME
  locations$biome <- biomes[mappedregions$BIOME]
  locations$realm <- NA
  for (i in sequence(nrow(locations))) {
    locations$realm[i] <- realms$realm[which(realms$code==mappedregions$REALM)]
  }
  return(locations)
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