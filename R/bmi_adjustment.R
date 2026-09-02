library(tidyverse)
library(readxl)
library(writexl)

# import BMI adjustment data for black children ----------------------------------------------

black_bmi_adjustment <- read_excel("data/hudda_bmi_adjustment_table_black.xlsx") |> 
  janitor::clean_names()

black_bmi_adjustment <- black_bmi_adjustment |> 
  pivot_longer(cols = boys_4_6_years:girls_10_12_years,
               names_to = "group",
               values_to = "adjusted_bmi") |> 
  separate_wider_delim(group,
                       delim = "_",
                       names = c("Gender", "School_Year"),
                       too_many = "merge") |> 
  filter(School_Year != "7_9_years") |> 
  mutate(Gender = case_when(Gender == "boys" ~ "M",
                            Gender == "girls" ~ "F"),
         School_Year = case_when(School_Year == "4_6_years" ~ "Reception",
                                 School_Year == "10_12_years" ~ "Year 6")) |> 
  rename(BMI_Score = unadjusted_bmi_kg_m2,
         Adjusted_BMI_Score = adjusted_bmi)

# build a linear regression model for each group, then find coefficients to transform unadjusted BMI
# this gets round the fact that adjusted BMIs are only provided at 0.1kgm2 intervals
# and only provide adjustment for a limited range of BMIs

black_bmi_adjustment_males_r <- black_bmi_adjustment |> 
  filter(Gender == "M",
         School_Year == "Reception")

black_bmi_adjustment_males_r_mod <- lm(Adjusted_BMI_Score ~ BMI_Score, data = black_bmi_adjustment_males_r)
coef(black_bmi_adjustment_males_r_mod)

black_bmi_adjustment_males_y6 <- black_bmi_adjustment |> 
  filter(Gender == "M",
         School_Year == "Year 6")

black_bmi_adjustment_males_y6_mod <- lm(Adjusted_BMI_Score ~ BMI_Score, data = black_bmi_adjustment_males_y6)
coef(black_bmi_adjustment_males_y6_mod)

black_bmi_adjustment_females_r <- black_bmi_adjustment |> 
  filter(Gender == "F",
         School_Year == "Reception")

black_bmi_adjustment_females_r_mod <- lm(Adjusted_BMI_Score ~ BMI_Score, data = black_bmi_adjustment_females_r)
coef(black_bmi_adjustment_females_r_mod)

black_bmi_adjustment_females_y6 <- black_bmi_adjustment |> 
  filter(Gender == "F",
         School_Year == "Year 6")

black_bmi_adjustment_females_y6_mod <- lm(Adjusted_BMI_Score ~ BMI_Score, data = black_bmi_adjustment_females_y6)
coef(black_bmi_adjustment_females_y6_mod)

# put all intercepts into one vector
black_bmi_adjustment_intercepts <- c(coef(black_bmi_adjustment_males_r_mod)[1],
                                     coef(black_bmi_adjustment_males_y6_mod)[1],
                                     coef(black_bmi_adjustment_females_r_mod)[1],
                                     coef(black_bmi_adjustment_females_y6_mod)[1])
black_bmi_adjustment_intercepts <- unname(black_bmi_adjustment_intercepts)

# put all slopes into one vector
black_bmi_adjustment_slopes <- c(coef(black_bmi_adjustment_males_r_mod)[2],
                                 coef(black_bmi_adjustment_males_y6_mod)[2],
                                 coef(black_bmi_adjustment_females_r_mod)[2],
                                 coef(black_bmi_adjustment_females_y6_mod)[2])
black_bmi_adjustment_slopes <- unname(black_bmi_adjustment_slopes)

# combine into a coefficient lookup
black_bmi_adjustment_coefs <- tibble(Ethnicity_Hudda = rep("Black", times = 4),
                                     Gender = c("M", "M", "F", "F"),
                                     School_Year = c("Reception", "Year 6", "Reception", "Year 6"),
                                     Intercept = black_bmi_adjustment_intercepts,
                                     Slope = black_bmi_adjustment_slopes)


# Put BMI adjustment for South Asian children into a table ----------------

# adjustments for South Asian children are constant across age groups and BMI
# so slope is 1 and the intercept is the only thing that changes between male and female

south_asian_adjustment_coefs <- tibble(Ethnicity_Hudda = rep("South Asian", times = 4),
                                       Gender = c("M", "M", "F", "F"),
                                       School_Year = c("Reception", "Year 6", "Reception", "Year 6"),
                                       Intercept = c(1.12, 1.12, 1.07, 1.07),
                                       Slope = rep(1, times = 4))


# Create BMI adjustment table for other ethnicities -----------------------

# other ethnicity BMIs are not adjusted
# but need a table which states this

other_ethnicity_adjustment_coefs <- tibble(Ethnicity_Hudda = c(rep("White", times = 4), rep("Mixed and Other", times = 4), rep("Unknown", times = 4)),
                                           Gender = rep(c("M", "M", "F", "F"), times = 3),
                                           School_Year = rep(c("Reception", "Year 6", "Reception", "Year 6"), times = 3),
                                           Intercept = rep(0, times = 12),
                                           Slope = rep(1, times = 12))


# Combine BMI adjustment tables and merge with main df -------------------------------------------

adjustment_coefs <- rbind(black_bmi_adjustment_coefs,
                          south_asian_adjustment_coefs) |> 
  rbind(other_ethnicity_adjustment_coefs)

ncmp_data <- ncmp_data |> 
  left_join(adjustment_coefs,
            by = join_by(Ethnicity_Hudda, Gender, School_Year)) |> 
  mutate(BMI_Score_Adjusted = Slope * BMI_Score + Intercept)


# Get BMI z scores ---------------------------------------------------------

# Need to convert adjusted BMI score to BMI_z and weight category which can only be done with LMSGrowth extension in Excel

write_xlsx(ncmp_data,
           path = "data/ncmp_data_adjusted_bmi.xlsx")

# reimport with z scores and calculate p scores and weight categories

ncmp_data <- read_excel("data/ncmp_data_adjusted_bmi.xlsx") |> 
  rename(BMI_z_Adjusted = SDS_BMI) |> 
  mutate(BMI_p_Adjusted = pnorm(BMI_z_Adjusted),
         BMI_Category_Adjusted = case_when(BMI_p_Adjusted >= 0.95 ~ "Obese",
                                           BMI_p_Adjusted >= 0.85 ~ "Overweight",
                                           BMI_p_Adjusted > 0.02 ~ "Healthy Weight",
                                           BMI_p_Adjusted <= 0.02 ~ "Underweight"))

# Group into three year combined periods ----------------------------------

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


