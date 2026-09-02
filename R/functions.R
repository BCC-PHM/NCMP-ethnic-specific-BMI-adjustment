# function to round an integer to nearest 5
# for data suppression purposes: https://digital.nhs.uk/data-and-information/find-data-and-publications/statement-of-administrative-sources/methodological-changes/changes-to-the-national-child-measurement-programme-2019-20-publication-content-and-disclosure-control-methodology#simple-calculations
round_5 <- function(x) {
  return(round(x / 5) * 5)
}

# function to summarise a patient level dataset by the columns given
# .by_count contains columns to summarise by
# .by_denom contains columns to create denominator
# returns a dataframe containing count, denominator and percentage with 95% CIs
# optionally include overweight/obese category (for BMI_Category data only)
# optionally recalculate percentage using count and denom rounded to nearest 5
ncmp_summarise <- function(.data, 
                           .by_count, 
                           .by_denom, 
                           .include_oo,
                           .round_5){
  if(.include_oo == TRUE){
    temp <- .data |> 
      dplyr::summarise(count = n(), .by = {{ .by_count }}) |> 
      dplyr::mutate(denominator = sum(count), .by = {{ .by_denom }})
    
    temp_oo <- .data |> 
      dplyr::mutate(BMI_Category = dplyr::case_when(BMI_Category == "Obese" ~ "Overweight/Obese",
                                      BMI_Category == "Overweight" ~ "Overweight/Obese",
                                      .default = BMI_Category)) |> 
      dplyr::summarise(count = n(), .by = {{ .by_count }}) |> 
      dplyr::mutate(denominator = sum(count), .by = {{ .by_denom }}) |> 
      dplyr::filter(BMI_Category == "Overweight/Obese")
    
    temp <- rbind(temp, temp_oo)
    
    if(.round_5 == TRUE){
      temp <- PHEindicatormethods::phe_proportion(temp,
                                                  x = count,
                                                  n = denominator,
                                                  multiplier = 100) |> 
        dplyr::mutate(value = (round_5(count)/round_5(denominator))*100) |> 
        dplyr::rename(pct = value,
               lower_ci = lowercl,
               upper_ci = uppercl)
    }
    
    if(.round_5 == FALSE){
      temp <- PHEindicatormethods::phe_proportion(temp,
                                                  x = count,
                                                  n = denominator,
                                                  multiplier = 100) |> 
        dplyr::rename(pct = value,
               lower_ci = lowercl,
               upper_ci = uppercl)
    }
  }
  
  if(.include_oo == FALSE){
    temp <- .data |> 
      dplyr::summarise(count = n(), .by = {{ .by_count }}) |> 
      dplyr::mutate(denominator = sum(count), .by = {{ .by_denom }})
    
    if(.round_5 == TRUE){
      temp <- PHEindicatormethods::phe_proportion(temp,
                                                  x = count,
                                                  n = denominator,
                                                  multiplier = 100) |> 
        dplyr::mutate(value = (round_5(count)/round_5(denominator))*100) |> 
        dplyr::rename(pct = value,
               lower_ci = lowercl,
               upper_ci = uppercl)
    }
    
    if(.round_5 == FALSE){
      temp <- PHEindicatormethods::phe_proportion(temp,
                                                  x = count,
                                                  n = denominator,
                                                  multiplier = 100) |> 
        dplyr::rename(pct = value,
               lower_ci = lowercl,
               upper_ci = uppercl)
    }
  }
  temp
}
