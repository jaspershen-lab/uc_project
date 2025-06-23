library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')

data_discovery <-
  readxl::read_xlsx("2_data/diet K方检验.xlsx", sheet = 2, col_names = FALSE)

data_discovery[c(2,4),2:4] %>%
  as.data.frame() %>%
  chisq.test()

data_discovery[c(2,4),6:8] %>%
  as.data.frame() %>%
  chisq.test()


data_validation1 <-
  readxl::read_xlsx("2_data/diet K方检验.xlsx", sheet = 3, col_names = FALSE)

data_validation1[c(2,4),2:4] %>%
  as.data.frame() %>%
  chisq.test()

data_validation1[c(2,4),6:8] %>%
  as.data.frame() %>%
  chisq.test()


data_validation2 <-
  readxl::read_xlsx("2_data/diet K方检验.xlsx", sheet = 4, col_names = FALSE)

data_validation2[c(2,4),2:4] %>%
  as.data.frame() %>%
  chisq.test()

data_validation2[c(2,4),6:8] %>%
  as.data.frame() %>%
  chisq.test()


data_validation3 <-
  readxl::read_xlsx("2_data/diet K方检验.xlsx", sheet = 5, col_names = FALSE)

data_validation3[c(2,4),2:4] %>%
  as.data.frame() %>%
  chisq.test()

data_validation3[c(2,4),6:8] %>%
  as.data.frame() %>%
  chisq.test()
