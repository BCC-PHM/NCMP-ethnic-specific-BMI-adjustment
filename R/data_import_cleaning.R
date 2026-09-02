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
  mutate(Gender = case_when(Gender == "M" ~ "Male",
                            Gender == "F" ~ "Female"),
         School_Year = case_when(School_Year == "Year 0" ~ "Reception",
                                 School_Year == "Year 6" ~ "Year 6"),
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

# add short stature column based on height_p
# need to calculate height_p using height_z for 2013/14 and 2014/15
ncmp_data <- ncmp_data |>
  mutate(Height_p = case_when(Year == "2013/2014" ~ pnorm(Height_z),
                              Year == "2014/2015" ~ pnorm(Height_z),
                              .default = Height_p),
         short_stature = case_when(Height_p <= 0.02 ~ TRUE,
                                   .default = FALSE))


# add locality based on constituency
ncmp_data <- ncmp_data |> 
  mutate(Locality = case_when(PCON24CD == "E14001535" ~ "North", # Sutton Coldfield
                              PCON24CD == "E14001093" ~ "North", # Erdington
                              PCON24CD == "E14001098" ~ "West", # Perry Barr
                              PCON24CD == "E14001096" ~ "West", # Ladywood
                              PCON24CD == "E14001095" ~ "East", # Hodge Hill
                              PCON24CD == "E14001100" ~ "East", # Yardley
                              PCON24CD == "E14001092" ~ "South", # Edgbaston
                              PCON24CD == "E14001097" ~ "South", # Northfield
                              PCON24CD == "E14001094" ~ "Central", # Hall Green
                              PCON24CD == "E14001099" ~ "Central")) # Selly Oak

# put years into a vector
years <- unique(ncmp_data$Year)

# remove 2020/2021
years <- years[!years %in% c("2020/2021")]

# create df with three years of data (first three years i.e. oldest)
ncmp_data_period_1 <- ncmp_data |> 
  filter(Year == years[1]|Year == years[2]|Year == years[3]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[1]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[3])))

# same again but with next three year period
ncmp_data_period_2 <- ncmp_data |> 
  filter(Year == years[2]|Year == years[3]|Year == years[4]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[2]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[4])))

# and so on
ncmp_data_period_3 <- ncmp_data |> 
  filter(Year == years[3]|Year == years[4]|Year == years[5]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[3]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[5])))

ncmp_data_period_4 <- ncmp_data |> 
  filter(Year == years[4]|Year == years[5]|Year == years[6]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[4]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[6])))

ncmp_data_period_5 <- ncmp_data |> 
  filter(Year == years[5]|Year == years[6]|Year == years[7]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[5]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[7])))

ncmp_data_period_6 <- ncmp_data |> 
  filter(Year == years[6]|Year == years[7]|Year == years[8]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[6]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[8])))

ncmp_data_period_7 <- ncmp_data |> 
  filter(Year == years[7]|Year == years[8]|Year == years[9]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[7]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[9])))

# most recent period
ncmp_data_period_8 <- ncmp_data |> 
  filter(Year == years[8]|Year == years[9]|Year == years[10]) |> 
  mutate(Year = paste0(sub("(20)(.*?)(20)", "\\1\\2", years[8]), " to ", sub("(20)(.*?)(20)", "\\1\\2", years[10])))

# bind all into one df
ncmp_data_3_years_combined <- bind_rows(ncmp_data_period_1,
                                        ncmp_data_period_2,
                                        ncmp_data_period_3,
                                        ncmp_data_period_4,
                                        ncmp_data_period_5,
                                        ncmp_data_period_6,
                                        ncmp_data_period_7,
                                        ncmp_data_period_8)

periods <- unique(ncmp_data_3_years_combined$Year)

# get a list of periods in format 20XX-XX
# where most recent period is periods[8]
periods_short <- sub("/([A-Za-z0-9]+( [A-Za-z0-9]+)+)/", "-", unique(ncmp_data_3_years_combined$Year))



