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
  mutate(Gender = case_when(Gender == "boys" ~ "Male",
                            Gender == "girls" ~ "Female"),
         School_Year = case_when(School_Year == "4_6_years" ~ "Reception",
                                 School_Year == "10_12_years" ~ "Year 6")) |> 
  rename(BMI_Score = unadjusted_bmi_kg_m2,
         Adjusted_BMI_Score = adjusted_bmi)

# build a linear regression model for each group, then find coefficients to transform unadjusted BMI
# this gets round the fact that adjusted BMIs are only provided at 0.1kgm2 intervals
# and only provide adjustment for a limited range of BMIs

black_bmi_adjustment_males_r <- black_bmi_adjustment |> 
  filter(Gender == "Male",
         School_Year == "Reception")

black_bmi_adjustment_males_r_mod <- lm(Adjusted_BMI_Score ~ BMI_Score, data = black_bmi_adjustment_males_r)
coef(black_bmi_adjustment_males_r_mod)

black_bmi_adjustment_males_y6 <- black_bmi_adjustment |> 
  filter(Gender == "Male",
         School_Year == "Year 6")

black_bmi_adjustment_males_y6_mod <- lm(Adjusted_BMI_Score ~ BMI_Score, data = black_bmi_adjustment_males_y6)
coef(black_bmi_adjustment_males_y6_mod)

black_bmi_adjustment_females_r <- black_bmi_adjustment |> 
  filter(Gender == "Female",
         School_Year == "Reception")

black_bmi_adjustment_females_r_mod <- lm(Adjusted_BMI_Score ~ BMI_Score, data = black_bmi_adjustment_females_r)
coef(black_bmi_adjustment_females_r_mod)

black_bmi_adjustment_females_y6 <- black_bmi_adjustment |> 
  filter(Gender == "Female",
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
                                     Gender = c("Male", "Male", "Female", "Female"),
                                     School_Year = c("Reception", "Year 6", "Reception", "Year 6"),
                                     Intercept = black_bmi_adjustment_intercepts,
                                     Slope = black_bmi_adjustment_slopes)


# Put BMI adjustment for South Asian children into a table ----------------

# adjustments for South Asian children are constant across age groups and BMI
# so slope is 1 and the intercept is the only thing that changes between male and female

south_asian_adjustment_coefs <- tibble(Ethnicity_Hudda = rep("South Asian", times = 4),
                                       Gender = c("Male", "Male", "Female", "Female"),
                                       School_Year = c("Reception", "Year 6", "Reception", "Year 6"),
                                       Intercept = c(1.12, 1.12, 1.07, 1.07),
                                       Slope = rep(1, times = 4))


# Create BMI adjustment table for other ethnicities -----------------------

# other ethnicity BMIs are not adjusted
# but need a table which states this

other_ethnicity_adjustment_coefs <- tibble(Ethnicity_Hudda = c(rep("White", times = 4), rep("Mixed and Other", times = 4), rep("Unknown", times = 4)),
                                           Gender = rep(c("Male", "Male", "Female", "Female"), times = 3),
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