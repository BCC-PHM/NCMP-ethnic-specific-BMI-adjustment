library(tidyverse)
library(readxl)
library(pool)
library(odbc)
library(DBI)

# set shape file paths and SQL server from global .Renviron
shape_file_path <- Sys.getenv("shape_file_path")
data_file_path <- Sys.getenv("data_file_path")
secure_drive_file_path <- Sys.getenv("secure_drive_file_path")

sql_server <- paste0(Sys.getenv("sql_server"), "\\", Sys.getenv("sql_server"))

# create connection to SQL server
conpool <- pool::dbPool(drv = odbc::odbc(), Driver="SQL Server", Server=sql_server, Database="PH_Evelyn", Trusted_Connection="True")           # ie setting up an ODBC connection to SQL Server.

# NCMP data from SQL
# Create a string variable containing SQL string
str_SQL_Code_NCMP <- "SELECT o.[SchoolYear] AS Year
      ,o.[DoH_URN]
      ,o.[School_Year]
      ,o.[Gender]
      ,o.[DateofBirth] AS DOB
      ,o.[PostcodeofPupil]
	  ,o.[PupilTier2LocalAuthority]
	  ,o.[SchoolTier2LocalAuthority]
      ,o.[DateofMeasurement]
      ,o.[Height_z]
      ,o.[Height_p]
      ,o.[Weight_z]
      ,o.[Weight_p]
      ,o.[BMI score] AS BMI_Score
      ,o.[BMI_z]
      ,o.[BMI_p]
      ,o.[Ethnic Description] AS Ethnicity
      ,o.[NCMPSystemId]
      ,o.[PupilReference]
      ,o.[NHSEthnicDescription] AS NHS_Ethnicity
      ,o.[ClinicalBMIcategory] AS BMI_Category_Clinical
      ,o.[GroupedClinicalBMIcategory] AS BMI_Category_Clinical_Grouped
      ,o.[PopulationBMIcategory] AS BMI_Category
      ,o.[GroupedPopulationBMIcategory] AS BMI_Category_Grouped
      ,o.[NonMeasurementReasonDescription]
      ,o.[PupilIndexOfMultipleDeprivationScore] AS Pupil_IMD_Score
      ,o.[PupilIndexOfMultipleDeprivationDecile] AS Pupil_IMD_Decile
      ,o.[ExtremeBmiWarning]
      ,p.[2011 Lower Layer Super Output Area] AS LSOA11CD
	    ,p.[2021 Lower Layer Super Output Area] AS LSOA21CD
	    ,p.[2021 Middle Layer Super Output Area] AS MSOA21CD
	    ,p.[2018 Ward code] AS WD18CD
	    ,w.[2018 Ward Name] AS WD18NM
	    ,p.[Westminster Parliamentary constituency] AS PCON24CD
  FROM [PH_Obesity].[dbo].[tblMainTable] AS o
  INNER JOIN [PH_LookUps].[dbo].[vw_tblNWWMClusterFullPostcodeFile] AS p
  ON o.[PostcodeofPupil] = p.[Postcod8] COLLATE SQL_Latin1_General_CP1_CI_AS
  INNER JOIN [PH_LookUps].[dbo].[tlkpWardNames2018byBham] as w
  ON p.[2018 Ward code] = w.[2018 Ward Code]
  WHERE o.[SchoolYear] BETWEEN '2014/2015' AND '2024/2025'"

ncmp_data <- DBI::dbGetQuery(conpool, str_SQL_Code_NCMP)

# import IDACI quintiles
IDACI_quintiles <- read_excel(paste0(Sys.getenv("secure_drive_file_path"), "Intelligence New/Themes/Deprivation/IMD/IMD 2025/File_3_IoD2025_Supplementary_Indices_IDACI_and_IDAOPI.xlsx"),
                              sheet = "IoD2025 IDACI & IDAOPI") |> 
  filter(`Local Authority District code (2024)` == "E08000025") |> 
  select(`LSOA code (2021)`, `Income Deprivation Affecting Children Index (IDACI) Decile (where 1 is most deprived 10% of LSOAs)`) |> 
  rename(LSOA21CD = `LSOA code (2021)`,
         IDACI_2025_Decile = `Income Deprivation Affecting Children Index (IDACI) Decile (where 1 is most deprived 10% of LSOAs)`) |> 
  mutate(IDACI_2025_Quintile = case_when(IDACI_2025_Decile <= 2 ~ 1,
                                         IDACI_2025_Decile <= 4 ~ 2,
                                         IDACI_2025_Decile <= 6 ~ 3,
                                         IDACI_2025_Decile <= 8 ~ 4,
                                         IDACI_2025_Decile <= 10 ~ 5))

# Main dataset cleaning and manipulation ----------------------------------

# add IDACI decile and quintile by merging on LSOA21CD
ncmp_data <- ncmp_data |> 
  left_join(IDACI_quintiles,
            by = "LSOA21CD")

# put non-measurement rows into own df
no_measurement <- ncmp_data |> 
  filter(NonMeasurementReasonDescription != "" | is.na(BMI_Category))

# filter non-measurement rows out of main df
ncmp_data <- ncmp_data |> 
  filter(!is.na(BMI_Category))

# change values in some fields
ncmp_data <- ncmp_data |> 
  mutate(School_Year = case_when(School_Year == "Year 0" ~ "Reception",
                                 School_Year == "Year 6" ~ "Year 6"),
         # Gender = case_when(Gender == "M" ~ "Male",
         #                    Gender == "F" ~ "Female"),
         BMI_Category = case_when(BMI_Category == "underweight" ~ "Underweight",
                                  BMI_Category == "healthy weight" ~ "Healthy Weight",
                                  BMI_Category == "overweight" ~ "Overweight",
                                  BMI_Category == "very overweight" ~ "Obese"),
         BMI_Category_Grouped = case_when(BMI_Category_Grouped == "underweight" ~ "Underweight",
                                          BMI_Category_Grouped == "healthy weight" ~ "Healthy Weight",
                                          BMI_Category_Grouped == "overweight or very overweight" ~ "Overweight/Obese"),
         BMI_Category_Clinical = case_when(BMI_Category_Clinical == "underweight" ~ "Underweight",
                                           BMI_Category_Clinical == "healthy weight" ~ "Healthy Weight",
                                           BMI_Category_Clinical == "overweight" ~ "Overweight",
                                           BMI_Category_Clinical == "very overweight" ~ "Obese"),
         BMI_Category_Clinical_Grouped = case_when(BMI_Category_Clinical_Grouped == "underweight" ~ "Underweight",
                                                   BMI_Category_Clinical_Grouped == "healthy weight" ~ "Healthy Weight",
                                                   BMI_Category_Clinical_Grouped == "overweight or very overweight" ~ "Overweight/Obese"))

# import ethnicity lookup
ethnicity_hudda_lookup <- read_excel("data/ethnicity_hudda_lookup.xlsx")

# merge/rename some ethnicities to give White, South Asian, Black, Mixed/Other and Unknown
ncmp_data <- ncmp_data |> 
  left_join(ethnicity_hudda_lookup,
            by = "NHS_Ethnicity") |> 
  mutate(Ethnicity_Hudda = case_when(is.na(Ethnicity) ~ "Unknown",
                                     Ethnicity == "Not stated" ~ "Unknown",
                                     is.na(Ethnicity_Hudda) ~ "Mixed and Other",
                               .default = Ethnicity_Hudda)) |> 
  mutate(Ethnicity_Hudda = str_trim(Ethnicity_Hudda))


# factorise imd scores
ncmp_data <- ncmp_data |> 
  mutate(IDACI_2025_Decile = factor(IDACI_2025_Decile,
                                    levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
         IDACI_2025_Quintile = factor(IDACI_2025_Quintile,
                                      levels = c("1", "2", "3", "4", "5")))