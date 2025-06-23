library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

dir.create("3_data_analysis/1_data_preparation/3_validation_cohort_subject_info")

###validation cohort 1
subject1 <- readxl::read_xlsx("2_data/participant_info_validation_cohort1.xlsx", sheet = 2)
subject2 <- readxl::read_xlsx("2_data/participant_info_validation_cohort1.xlsx", sheet = 1)

head(subject1)

colnames(subject1) <- c(
  "subject_id",
  "participant_name",
  "number",
  "sex",
  "age",
  "bmi",
  "disease_year",
  "esr",
  "crp",
  "hgb",
  "tp",
  "alb",
  "fc",
  "mayo_score",
  "Montreal"
)

head(subject2)

colnames(subject2) <- c("subject_id", "sample_number", "participant_name", "sex", "age2", "bmi", "age")

subject1$subject_id <- paste("UC", subject1$subject_id, sep = "_")
subject2$subject_id <- paste("Ctr", subject2$subject_id, sep = "_")

subject1$subject_id
subject2$subject_id

dim(subject1)
dim(subject2)

subject_info <-
  subject1 %>%
  dplyr::full_join(subject2, by = intersect(colnames(subject2), colnames(subject1)))

subject_info <-
  subject_info %>%
  dplyr::mutate(sex = case_when(sex == "男" ~ "Male", sex == "女" ~ "Female")) %>%
  dplyr::mutate(sample_id = subject_id)

subject_info_validation1 <- subject_info

save(subject_info_validation1, file = "3_data_analysis/1_data_preparation/3_validation_cohort_subject_info/subject_info_validation1.RData")

###validation cohort 2
subject1 <- readxl::read_xlsx("2_data/participant_info_validation_cohort2.xlsx", sheet = 2)
subject2 <- readxl::read_xlsx("2_data/participant_info_validation_cohort2.xlsx", sheet = 1)

head(subject1)

colnames(subject1) <- c(
  "subject_id",
  "number",
  "sex",
  "age",
  "bmi",
  "disease_year",
  "esr",
  "crp",
  "hgb",
  "tp",
  "alb",
  "fc",
  "mayo_score",
  "no"
)

subject1 <-
  subject1 %>%
  dplyr::select(-no)

head(subject2)

colnames(subject2) <- c("subject_id", "participant_name", "sex", "age2", "bmi", "age")

subject1$subject_id <- paste("UC", subject1$subject_id, sep = "_")
subject2$subject_id <- paste("Ctr", subject2$subject_id, sep = "_")

dim(subject1)
dim(subject2)

subject_info <-
  subject1 %>%
  dplyr::full_join(subject2, by = intersect(colnames(subject2), colnames(subject1)))

subject_info <-
  subject_info %>%
  dplyr::mutate(sex = case_when(sex == "男" ~ "Male", sex == "女" ~ "Female")) %>%
  dplyr::mutate(sample_id = subject_id)

subject_info_validation2 <- subject_info

save(subject_info_validation2, file = "3_data_analysis/1_data_preparation/3_validation_cohort_subject_info/subject_info_validation2.RData")





###validation cohort 3
subject1 <- readxl::read_xlsx("2_data/participant_info_validation_cohort3.xlsx", sheet = 2)
subject2 <- readxl::read_xlsx("2_data/participant_info_validation_cohort3.xlsx", sheet = 1)

head(subject1)

colnames(subject1) <- c(
  "subject_id",
  "group_old",
  "number",
  "sex",
  "age",
  "bmi",
  "disease_year",
  "esr",
  "crp",
  "hgb",
  "tp",
  "alb",
  "fc",
  "mayo_score"
)

head(subject2)

colnames(subject2) <- c(
  "subject_id",
  "number",
  "group_old",
  "sample_number",
  "participant_name",
  "sex",
  "age",
  "id3",
  "blood_date",
  "bmi"
)

subject1$subject_id <- paste("UC", subject1$subject_id, sep = "_")
subject2$subject_id <- paste("Ctr", subject2$subject_id, sep = "_")

dim(subject1)
dim(subject2)

subject_info <-
  subject1 %>%
  dplyr::full_join(subject2, by = intersect(colnames(subject2), colnames(subject1)))

subject_info <-
  subject_info %>%
  dplyr::mutate(sex = case_when(sex == "男" ~ "Male", sex == "女" ~ "Female")) %>%
  dplyr::mutate(sample_id = subject_id)

subject_info_validation3 <- subject_info

save(subject_info_validation3, file = "3_data_analysis/1_data_preparation/3_validation_cohort_subject_info/subject_info_validation3.RData")
