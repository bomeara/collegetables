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
	banned_states <- c("AL", "AR", "FL", "ID", "IN", "IA", "KS", "KY", "MI", "MO", "NC", "ND", "OH", "OK", "SC", "SD", "TN", "TX", "UT", "WV")
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
		distances <- adist(aaup[i,1], college_data$"Institution Name")
		matches <- which(distances == min(distances))
		college_data$AAUP_Censure[matches] <- "Yes"
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
	admission <- as.numeric(college_data$"Admission")
	admission[which(is.na(admission)) ]<- 100
	graduation <- as.numeric(college_data$"Graduation")
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

	college_data_enhanced <- college_data_enhanced[!duplicated(college_data_enhanced$ShortName),]
	return(college_data_enhanced)
}

GetOverviewColumns <- function(college_data) {
	#overview <- as.data.frame(college_data %>% select("Name", "Sector of institution", "CollegeType","Historically Black College or University", "Tribal college", "Undergraduate enrollment","City location of institution","State abbreviation",  "Percent admitted - total", "Admissions yield - total", "Full-time retention rate  2019",  "Graduation rate  total cohort", "Degree of urbanization (Urban-centric locale)", "NatWalkInd", "DistanceToTransit", "BannedCATravel", "Abortion restrictions", "AllStudentsVaccinatedAgainstCovid19", "AllEmployeesVaccinatedAgainstCovid19", "InMisconductDatabase",  "RankingProxy" ))
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
		"LocaleChar", 
		"TTFaculty",
		"NTTFaculty", 
		"Undergrads per tenure-track professor", 
		"Undergrads per instructor", 
		"AAUP_Censure",  
		"Tenure track fraction", 
		"Fraction of tenure track faculty who are women", 
		"Fraction of tenure track faculty who are men", 
		"Fraction of non-tenure track faculty who are women", 
		"Fraction of non-tenure track faculty who are men", 
		"Fraction of tenure track faculty who are BIPOC", 
		"Fraction of tenure track faculty who are white", 
		"Fraction of non-tenure track faculty who are BIPOC", 
		"Fraction of non-tenure track faculty who are white", 
		"Fraction of undergrads who are women", 
		"Fraction of undergrads who are men", 
		"Fraction of undergrads who are BIPOC", 
		"Fraction of undergrads who are white", 
		"URL", 
		"Tuition and fees as a percent of core revenues", 
		"State appropriations as percent of core revenues", 
		"Local appropriations as a percent of core revenues", 
		"Government grants and contracts as a percent of core revenues",
		"Private gifts  grants  and contracts as a percent of core revenues", 
		"Investment return as a percent of core revenues", 
		"Endowment assets per FTE", 
		"Equity ratio", 
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
		p <- ggplot(data=data.frame(focal=c(TRUE, FALSE), percent=c(pct, 100-pct), x=c(1,1)), aes(fill=focal, y=percent, x=x)) + geom_bar(position="fill", stat="identity") +  theme_void() + theme(legend.position="none") + coord_flip() + scale_fill_manual(values=c("darkgray", GetColorFromPercentile(pct))) 
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
		p <- ggplot(data=data.frame(focal=c(TRUE, FALSE), percent=c(pct, 100-pct), x=c(1,1)), aes(fill=focal, y=percent, x=x)) + geom_bar(position="fill", stat="identity") +  theme_void() + theme(legend.position="none") + coord_flip() + scale_fill_manual(values=c("darkgray", GetColorFromPercentile(pct, direction=1))) 
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

RenderInstitutionPages <- function(overview, degree_granting, maxcount=30, students_by_state_by_institution, student_demographics, faculty_counts, institituion_rmd_change_tracking, spark_width, spark_height) {	
	institutions <- unique(degree_granting$ShortName)
	failures <- c()
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
			Sys.sleep(1)
			system("sed -i '' 's/&gt;/>/g' docs/institution.html") # because htmlTable doesn't escape well; the '' is a requirement of OS X's version of sed, apparently
			system("sed -i '' 's/&lt;/</g' docs/institution.html")
			
			#system("sed -i '' 's/‘/\x27/g' docs/institution.html")
			#system("sed -i '' 's/’/\x27/g' docs/institution.html")
			Sys.sleep(1)


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
	return(failures)
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

	merged_data1 <- base::merge(crimelist[['crimes']], crimelist[['arrests']], by="UnitID")
	merged_data2 <- base::merge(merged_data1, crimelist[['discipline']], by="UnitID")
	college_data <- base::merge(college_data, merged_data2, by="UnitID")
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