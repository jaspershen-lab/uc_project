library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

####discovery cohort
subject1 <- readxl::read_xlsx("2_data/participant_info.xlsx", sheet = 1)
subject2 <- readxl::read_xlsx("2_data/participant_info.xlsx", sheet = 2)

colnames(subject1) <- c(
  "subject_id",
  "number",
  "participant_name",
  "case_id",
  "date",
  "mayo_score",
  "sex",
  "age",
  "GWDB",
  "ESR",
  "CPR",
  "HGB",
  "TP",
  "ALB",
  "height",
  "weight",
  "BMI",
  "diagnosis"
)

colnames(subject2) <- c("subject_id", "number", "participant_name", "sex", "age", "BMI")


subject1$subject_id <- paste("UC", subject1$subject_id, sep = "_")
subject2$subject_id <- paste("Ctr", subject2$subject_id, sep = "_")

dim(subject1)
dim(subject2)

subject_info <-
  subject1 %>%
  dplyr::full_join(subject2, by = colnames(subject2))

subject_info <-
  subject_info %>%
  dplyr::mutate(sex = case_when(sex == "男" ~ "Male", sex == "女" ~ "Female")) %>%
  dplyr::mutate(sample_id = subject_id)


head(subject_info)

subject_info <-
  subject_info %>%
  dplyr::select(subject_id, mayo_score, sex, age, BMI)

subject_info_discovery <-
  subject_info

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

colnames(subject2) <- c("subject_id",
                        "sample_number",
                        "participant_name",
                        "sex",
                        "age2",
                        "bmi",
                        "age")

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

subject_info_validation1 <-
  subject_info %>%
  dplyr::select(subject_id, mayo_score, sex, age, bmi)


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



subject_info_validation2 <-
  subject_info %>%
  dplyr::select(subject_id, mayo_score, sex, age, bmi)


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


subject_info_validation3 <-
  subject_info %>%
  dplyr::select(subject_id, mayo_score, sex, age, bmi)

dir.create("3_data_analysis/supplementary_data/data1", recursive = TRUE)
setwd("3_data_analysis/supplementary_data/data1")

head(subject_info_discovery)
head(subject_info_validation1)
head(subject_info_validation2)
head(subject_info_validation3)

subject_info_discovery <-
  subject_info_discovery %>%
  dplyr::rename("bmi" = "BMI")

subject_info <-
  rbind(
    data.frame(cohort = "discovery", subject_info_discovery),
    data.frame(cohort = "validation1", subject_info_validation1),
    data.frame(cohort = "validation2", subject_info_validation2),
    data.frame(cohort = "validation3", subject_info_validation3)
  )

subject_info <-
  subject_info %>%
  dplyr::mutate(group = case_when(
    grepl("Ctr", subject_id) ~ "Control",
    grepl("UC", subject_id) ~ "UC",
    TRUE ~ "Unknown"
  ))

library(openxlsx)

openxlsx::write.xlsx(subject_info, "supplementary_data1.xlsx")
