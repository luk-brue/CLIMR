################################################################################

# CLIMR -- Deviation Handling

################################################################################

# The code below handles technical errors and cases where there were
# deviations from the procedures. This code is not optimized for efficiency.
# Rather, it is written to make it easy to add and remove components and to be
# transparent about each step in the process. As a consequence, it may run
# somewhat slowly, but none of the operations are very computationally
# demanding.

# AU_04, AU_01: Known duplicate submissions ------------------------------------

# There are notes about this case of known duplicate submissions included in
# the comments of the script that handles these cases.

source("R/import/CLIMR_duplicate-data-handling.R")

# MY_01: Test cases ------------------------------------------------------------

# Lab MY_01 included a test run case in the data, which should be removed
# prior to analysis. This case is marked with "PILOT" as it ID.

raw <- raw %>% 
  filter(id_subject != "PILOT" | is.na(id_subject))

# NL_02: Test cases ------------------------------------------------------------

# NL_02 reported testing the experiments a few times, flagging their test
# cases using the subject ID variable. These cases should be removed.

raw <- raw %>% 
  filter(!str_detect(id_subject, "test|Test") | is.na(id_subject))

# US_12: Incorrect surveys for some participants and test cases ----------------

# Notes about a technical error are included in the script that is run below

source("R/import/CLIMR_survey-error-handling.R")

# US_12 reported testing the experiments a few times, flagging their test
# cases using the subject ID variable. These cases should be removed.

raw <- raw %>% 
  filter(id_subject != "US_12_9998"  | is.na(id_subject)) %>% 
  filter(id_subject != "US_12_9999"  | is.na(id_subject)) %>% 
  filter(id_subject != "US_12_99999" | is.na(id_subject)) %>% 
  filter(id_subject != "us129999"    | is.na(id_subject))

# US_06: Test cases ------------------------------------------------------------

# US_06 reported testing the experiments a few times, and these test cases are
# evident from their lack of identifiers for the SONA system automation.

raw <- raw %>% 
  filter(id_sona != "%SURVEY_CODE%" | is.na(id_sona)) %>% 
  filter(id_internal != "97400") # This test case was identified manually

# US_19: Test cases ------------------------------------------------------------

# US_19 reported running the experiment to record their procedure video. This
# case is identified by using "CLIMR" in the ID field.

raw <- raw %>% 
  filter(id_subject != "CLIMR" | is.na(id_subject))

# US_08: Repeated participation ------------------------------------------------

# US_08 reported that two participants did the study twice, and they provided us
# with the ID numbers of the second instances of participation. These cases
# should be removed.

raw <- raw %>% 
  filter(!(id_subject == "274" & lab == "US_08") | is.na(id_subject)) %>%
  filter(!(id_subject == "339" & lab == "US_08") | is.na(id_subject))

# DE_01: Test cases ------------------------------------------------------------

# DE_01 tested the experiments a few times and identified these cases by
# entering "climr" or "climbr" in the ID field or the ethnicity field. These
# cases should be removed. There are also some other random character strings
# in the id field that were used to mark a test case. 

# First step, get all IDs that contain non-numeric characters. 

de01_non_num <- raw %>% 
  filter(lab == "DE_01") %>% 
  purrr::pluck("id_subject") %>% 
  stringi::stri_subset(regex = "[^0-9]")

# Second: Not all of these are test cases. IDs that end with "CC" or "cc"
# should be kept, they are valid. 
# (The CC suffix arose when experimenters started
# collecting data in a second location called CC, and did not yet have a
# standard for giving discernable names to IDs from this location. Later on, a 
# standard was established: every ID that is > 500 comes from the second
# location)

de01_testcases <- de01_non_num[
  !(endsWith(de01_non_num, "CC") | endsWith(de01_non_num, "cc"))] %>% 
  unique()

# Third: Remove the test cases, also remove a test case that was not covered
# by the non-numeric approach (ID 00000). 

raw <- raw %>%
  filter(!is.na(id_subject)) %>% 
  filter(!(lab == "DE_01" & id_subject %in% de01_testcases)) %>% 
  filter(!(lab == "DE_01" & id_subject == "00000")) %>% 
  filter(ethnicity  != "climr"  | is.na(ethnicity))

# DE_01: Invalid or incomplete cases -------------------------------------------

# DE_01 noted some subject IDs which are invalid and should be excluded.

# Person with ID 609 did not read an instruction and could not go back
# to re-read it. Experimenters started a fresh questionnaire with a new ID,
# 610, which was completed. Therefore 609 is an incomplete duplicate.

raw <- raw %>% 
  filter(!(lab == "DE_01" & id_subject == 609))

# On two occasions (IDs 71 and 754), the computer with the questionnaire
# crashed, and experimenters gave participants a fresh questionnaire but entered
# the same subject_id as in the crashed questionnaire. A quick way to decide 
# which of these duplicates is the incomplete case: Look at the the panas_1 
# column, which is near the end of the questionnaire, to discern incomplete from
# complete duplicate

raw <- raw %>% 
  filter(!(lab == "DE_01" & id_subject %in% c(71, 754) & is.na(panas_1) == TRUE))

# Person 713 did not find their national identity in the drop down list.
# They entered Germany because the field is required. But now, afterwards it 
# should be set to NA to reflect reality that national identity of that person 
# is unknown. 

raw$national_identity[raw$id_subject == 713 & raw$lab == "DE_01"] <- NA

# DE_01: Clean Up IDs from different locations----------------------------------

# DE_01 used two different locations for data collection.
# id_subject was used to discern those, but for the first few persons at the
# second location, there is no consistent scheme. Later, a consistent scheme was
# adopted - an ID > 500 means the data were collected at the second location.
# The IDs of the first few persons that do not adhere to this should be
# retrofitted with the > 500 scheme. This facilitates location-wise analysis.

# The IDs 1 to 5 which were recorded on 2023-12-11 should be renamed 501 to 505:
# Best way for this might be to use `sub` which uniquely identifies each ID.
# This assumes `sub` is unique even across all labs

first_five <- raw %>% 
  filter(lab == "DE_01") %>% 
  filter(as_date(recorded_date) == as_date("2023-12-11")) %>% 
  filter(id_subject %in% 1:5) %>% 
  select(recorded_date, id_subject, sub)

raw$id_subject[raw$sub %in% first_five$sub] <- 
  as.numeric(first_five$id_subject) + 500

# The IDs 7 to 15 have been suffixed with either "CC" or "cc".
# Get subs of those IDs
cc_suffix <- raw %>% 
  filter(lab == "DE_01") %>% 
  filter(id_subject %>%  endsWith("CC") |
           id_subject %>% endsWith("cc")) %>% 
  select(sub, id_subject)

# Remove the last two characters from those IDs and add 500

raw$id_subject[raw$lab == "DE_01" & raw$sub %in% cc_suffix$sub] <-
  substr(cc_suffix$id_subject, 1, nchar(cc_suffix$id_subject) - 2) %>% 
    as.numeric() %>% 
    `+`(500)

# Clean up...
rm(cc_suffix, de01_non_num, de01_testcases, first_five)
  
# Duplicated participant IDs ---------------------------------------------------

# Some labs reported accidentally reusing some participant ID values These
# values are not used in any analysis, so these non-unique cases are retained.
# They don't seem to pose any practical issue, since the Qualtrics response ID
# still identifies each unique response.

# Removal of known invalid or incomplete cases ---------------------------------

# There are some cases that should be removed because of their missing or
# invalid demographic information. In these rows of data, there are no
# responses to the experimental questions, since the session was terminated
# prior to responding to any items or because the would-be participant entered
# invalid information (e.g., being under 18) and was thus prevented from
# proceeding. These cases should be removed.

raw <- raw %>% 
  filter(age >= 18 & !is.na(age))
