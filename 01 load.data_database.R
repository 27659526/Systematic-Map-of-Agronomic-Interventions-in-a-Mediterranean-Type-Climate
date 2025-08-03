library(readxl)

database <- read_excel(
  "[yourpath]/Supplementary Material_agr_intervention_database.xlsx",
  sheet = "Database",
  skip = 1
)
