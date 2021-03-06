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
	raw_data <- select(raw_data, -'...253')

	raw_data2 <- mutate_all(read_csv("data/ipeds_Data_7-24-2022---404.csv", col_types="c"), as.character)
	traits <- mutate_all(read_csv("data/ipeds_Data_7-24-2022---404.csv", col_types="c"), as.character)
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
	raw_data2 <- select(raw_data2, -'...242')
	raw_data_joined <- full_join(raw_data, raw_data2, by="Institution Name") %>% dplyr::left_join(distinct(mutate_all(read_csv("data/Data_7-24-2022_NTT.csv", col_types="c"), as.character)), by="Institution Name") %>% dplyr::left_join(distinct(mutate_all(read_csv("data/Data_7-24-2022_tenured.csv", col_types="c"), as.character)), by="Institution Name") %>% dplyr::left_join(distinct(mutate_all(read_csv("data/Data_7-24-2022_TTnontenured.csv", col_types="c"), as.character)), by="Institution Name") %>%rename_at(
		vars(ends_with(".x")),
		~str_replace(., "\\..$","")
		) %>% 
		select_at(
		vars(-ends_with(".y"))
		)
	raw_data <- raw_data_joined[!duplicated(raw_data_joined$`Institution Name`),]
	raw_data$"Undergraduate enrollment (DRVEF2019_RV)" <- as.numeric(raw_data$"Undergraduate enrollment (DRVEF2019_RV)")
	raw_data <- subset(raw_data, !is.na(raw_data$"Undergraduate enrollment (DRVEF2019_RV)"))
	raw_data <- subset(raw_data, raw_data$"Undergraduate enrollment (DRVEF2019_RV)">0)
	#raw_data <- raw_data_joined

	
	load("data/epa_walkability.rda")	
	walkability$D4A[walkability$D4A == -99999] <- NA
	walkability_aggregated <- walkability %>% group_by(CBSA_Name) %>% summarise(NatWalkInd=round(median(NatWalkInd),1), Population=sum(TotPop), HousingUnits = sum(CountHU, na.rm=TRUE), DistanceToTransit = round(min(D4A, na.rm=TRUE)))
	raw_data <- left_join(raw_data, walkability_aggregated, by=c("Core Based Statistical Area (CBSA) (HD2019)" = "CBSA_Name"))
	rm(walkability)
	banned_states <- c("AL", "AR", "FL", "ID", "IN", "IA", "KS", "KY", "MI", "MO", "NC", "ND", "OH", "OK", "SC", "SD", "TN", "TX", "UT", "WV")
	raw_data$BannedCATravel <- "No"
	raw_data$BannedCATravel[pull(raw_data, `State abbreviation (HD2019)`) %in% banned_states] <- "Yes"
	raw_data <- CleanNames(raw_data)
	raw_data$NumberOfPhysicalBooksPerUndergrad <- round(as.numeric(pull(raw_data, "Number of physical books"))/as.numeric(pull(raw_data, "Undergraduate enrollment")))
	raw_data$NumberOfDigitalBooksPerUndergrad <- round(as.numeric(pull( raw_data,  "Number of digital/electronic books" ))/as.numeric(pull(raw_data, "Undergraduate enrollment")))
	raw_data$TenureTrackFacultyCount <- as.numeric(pull(raw_data, "All ranks (All full-time instructional staff)")) - ((as.numeric(pull(raw_data, "Professors (All full-time instructional staff)" ))) + (as.numeric(pull(raw_data, "Associate professors (All full-time instructional staff)" ))) + (as.numeric(pull(raw_data, "Assistant professors (All full-time instructional staff)" ))))
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
	
	
	return(raw_data)
}

# From https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html
GetPopulationByStateAtAge18 <- function() {
	pops <- read.csv("data/sc-est2019-agesex-civ.csv")
	pops <- subset(pops, SEX==0 & AGE==18) # all sexes. only age 18
	pops <- select(pops, c("STATE", "NAME", "POPEST2019_CIV"))
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
	abortion <- read.csv("data/abortion.csv", header=TRUE)
	colnames(abortion) <- gsub("\\.", " ", colnames(abortion))
	return(left_join(college_data, abortion, by="State abbreviation"))
	
}

FilterForDegreeGranting <- function(college_data) {
	return(subset(college_data, college_data$"Degree-granting status"=="Degree-granting"))	
}

GetOverviewColumns <- function(college_data) {
	overview <- as.data.frame(college_data %>% select("Institution Name", "Sector of institution", "CollegeType","Historically Black College or University", "Undergraduate enrollment","City location of institution","State abbreviation",  "Percent admitted - total", "Admissions yield - total", "Full-time retention rate  2019",  "Graduation rate  total cohort",   "StudentToTenureTrackRatio", "Degree of urbanization (Urban-centric locale)", "NatWalkInd", "DistanceToTransit", "BannedCATravel", "Abortion restrictions", "NumberOfPhysicalBooksPerUndergrad", "AllStudentsVaccinatedAgainstCovid19", "AllEmployeesVaccinatedAgainstCovid19", "InMisconductDatabase",  "Percent of undergraduate enrollment that are women", "Percent of undergraduate enrollment that are American Indian or Alaska Native", "Percent of undergraduate enrollment that are American Indian or Alaska Native", "Percent of undergraduate enrollment that are American Indian or Alaska Native", "Percent of undergraduate enrollment that are American Indian or Alaska Native", "Percent of undergraduate enrollment that are Black or African American" , "Percent of undergraduate enrollment that are Hispanic/Latino" ,  "Percent of undergraduate enrollment that are White" ,  "Percent of undergraduate enrollment that are two or more races"  , "Percent of undergraduate enrollment that are Race/ethnicity unknown"  , "Percent of undergraduate enrollment that are Nonresident Alien" , "Grand total men (Full-time instructional with faculty status)", "Grand total women (Full-time instructional with faculty status)" , "American Indian or Alaska Native men (Full-time instructional with faculty status)" ,    "American Indian or Alaska Native women (Full-time instructional with faculty status)" , "Asian men (Full-time instructional with faculty status)"   ,"Asian women (Full-time instructional with faculty status)" ,"Black or African American men (Full-time instructional with faculty status)"  , "Black or African American women (Full-time instructional with faculty status)", "Hispanic or Latino men (Full-time instructional with faculty status)" ,"Hispanic or Latino women (Full-time instructional with faculty status)"  ,"Native Hawaiian or Other Pacific Islander men (Full-time instructional with faculty status)" ,"Native Hawaiian or Other Pacific Islander women (Full-time instructional with faculty status)"  , "White men (Full-time instructional with faculty status)" ,"White women (Full-time instructional with faculty status)"    ,   "Two or more races men (Full-time instructional with faculty status)"  ,"Two or more races women (Full-time instructional with faculty status)" ,"Race/ethnicity unknown men (Full-time instructional with faculty status)"      ,"Race/ethnicity unknown women (Full-time instructional with faculty status)"   ,"Nonresident alien men (Full-time instructional with faculty status)", "Nonresident alien women (Full-time instructional with faculty status)"  ))
	overview$RankingProxy <- (100 - as.numeric(overview$"Percent admitted - total")) + as.numeric(overview$"Graduation rate  total cohort") + (as.numeric(overview$`Undergraduate enrollment`))^(1/4)
	overview$RankingProxy <- as.numeric(overview$RankingProxy) 
	overview <- subset(overview, !is.na(overview$RankingProxy))
	overview <- dplyr::distinct(overview[order(overview$RankingProxy, decreasing=TRUE),])
	overview$NatWalkInd <- round(as.numeric(overview$NatWalkInd)/20,2)
	
	overview <- dplyr::rename(overview, Name = `Institution Name`, Sector=`Sector of institution`, Type=CollegeType, Locale=`Degree of urbanization (Urban-centric locale)`, City="City location of institution", State="State abbreviation", Walkability=NatWalkInd, `Anti-LGBTQ+ state laws`=BannedCATravel, `Books/student`=NumberOfPhysicalBooksPerUndergrad, `Students per tenure-track professor`=StudentToTenureTrackRatio, Yield="Admissions yield - total", `Distance (m) to transit`=DistanceToTransit, Admission="Percent admitted - total", `First year retention`="Full-time retention rate  2019", "Graduation"="Graduation rate  total cohort", "Covid vax (students)"="AllStudentsVaccinatedAgainstCovid19", "Covid vax (employees)"="AllEmployeesVaccinatedAgainstCovid19", "Misconduct reports"="InMisconductDatabase")
	
	overview$`Minimum distance to mass transit (minutes walking)` <- round((as.numeric(overview$`Distance (m) to transit`)/1.34)/60) #using an estimate of walk speed of 1.34 m/s from https://www.healthline.com/health/exercise-fitness/average-walking-speed#average-speed-by-age
	Percent_of_undergrad_cols <- which(grepl("Percent of undergraduate enrollment that are", colnames(overview)))
	for (i in seq_along(Percent_of_undergrad_cols)) {
		colnames(overview)[Percent_of_undergrad_cols[i]] <- gsub("two or more", "Two or more", gsub("women", "Women", paste0(gsub("Percent of undergraduate enrollment that are ", "", colnames(overview)[Percent_of_undergrad_cols[i]])," (undergrads)")))
		overview[,Percent_of_undergrad_cols[i]] <- 0.01*(as.numeric(overview[,Percent_of_undergrad_cols[i]]))
	}
	
	Full_time_faculty_cols <- which(grepl("Full-time instructional with faculty status", colnames(overview)))
	AllFaculty <- as.numeric(overview[,"Grand total men (Full-time instructional with faculty status)"]) + as.numeric(overview[,"Grand total women (Full-time instructional with faculty status)"])
	for (i in seq_along(Full_time_faculty_cols)) {
		colnames(overview)[Full_time_faculty_cols[i]] <- gsub("Full-time instructional with faculty status", "Full-time faculty", colnames(overview)[Full_time_faculty_cols[i]])
		overview[,Full_time_faculty_cols[i]] <- paste0(overview[,Full_time_faculty_cols[i]], " (", 100*round(as.numeric(overview[,Full_time_faculty_cols[i]])/AllFaculty,3), "%)")
	}
	
	overview$Yield <- as.numeric(overview$Yield)/100
	overview$Admission <- as.numeric(overview$Admission)/100
	overview$`First year retention` <- as.numeric(overview$`First year retention`)/100
	overview$Graduation <- as.numeric(overview$Graduation)/100
	overview$`Anti-LGBTQ+ state laws` <- as.factor(overview$`Anti-LGBTQ+ state laws`)
	overview$`Books/student` <- as.numeric(overview$`Books/student`)
	overview$`Students per tenure-track professor` <- as.numeric(overview$`Students per tenure-track professor`)
	overview$`Distance (m) to transit` <- as.numeric(overview$`Distance (m) to transit`)
	overview$`Covid vax (students)` <- as.factor(overview$`Covid vax (students)`)
	overview$`Covid vax (employees)` <- as.factor(overview$`Covid vax (employees)`)
	overview$`Misconduct reports` <- as.factor(overview$`Misconduct reports`)
	overview$`Abortion restrictions` <- as.factor(overview$`Abortion restrictions`)
	overview$Sector <- as.factor(overview$Sector)
	overview$State <- as.factor(overview$State)
	overview$Locale <- as.factor(overview$Locale)
	overview$`Undergraduate enrollment` <- as.numeric(overview$`Undergraduate enrollment`)
	overview$ShortName <- overview$Name
	overview$Name <- paste0(overview$Name, " (", overview$City, ", ", overview$State, ")")
	overview$URL <- paste0(  utils::URLencode(gsub(" ", "", overview$ShortName)), ".html")

	overview <- select(overview, -RankingProxy)
	overview <- select(overview, -City)

	
	return(overview)
}


RenderInstitutionPages <- function(overview, degree_granting) {
	institutions <- unique(overview$ShortName)
	#for (i in seq_along(institutions)) {
	for (i in sequence(10)) {
	#	try({
		print(institutions[i])
		rmarkdown::render(input="institution.Rmd", output_file=paste0(  utils::URLencode(gsub(" ", "", institutions[i])), ".html"), 
        params = list(
			institution_name = institutions[i],
			overview_table = subset(overview, overview$ShortName == institutions[i]),
			raw_table = degree_granting
        ))
		#}, silent=TRUE)
	}
}

FilterForTopAndSave <- function(overview) {
	overview_top <- as.data.frame(overview)[c(1:1000),]
	write.csv(overview_top, file="overview_top.csv")
}