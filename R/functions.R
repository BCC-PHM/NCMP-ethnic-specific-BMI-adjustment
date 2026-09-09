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

# function to generate an interactive sankey diagram showing movement between BMI classes
# .Ethnicity_Hudda is ethnicity to plot
#. School_Year is school year to plot
bmi_adj_sankey <- function(.data,
                           .Ethnicity_Hudda,
                           .School_Year){
  
  sankey_links <- .data |>
    dplyr::filter(Ethnicity_Hudda == .Ethnicity_Hudda,
           School_Year == .School_Year) |> 
    dplyr::select(BMI_Category, BMI_Category_Adjusted, count, pct, lower_ci, upper_ci) |>
    dplyr::rename(source = BMI_Category,
           target = BMI_Category_Adjusted,
           value  = count) |>
    dplyr::mutate(
      linkgroup = source,
      linklabel = dplyr::case_when(
        source == "Underweight"     & target == "Underweight"     ~ "Remained underweight",
        source == "Healthy Weight"  & target == "Healthy Weight"  ~ "Remained healthy weight",
        source == "Overweight"      & target == "Overweight"      ~ "Remained overweight",
        source == "Obese"           & target == "Obese"           ~ "Remained obese",
        .default = paste0("Moved to ", str_to_lower(target))),
      source = paste0(source, "_unadj"),
      target = paste0(target, "_adj"),
      colour = dplyr::case_when(
        linkgroup == "Underweight"    ~ paste0(BMI_Category_colours["Underweight"], "99"),
        linkgroup == "Healthy Weight" ~ paste0(BMI_Category_colours["Healthy Weight"], "99"),
        linkgroup == "Overweight"     ~ paste0(BMI_Category_colours["Overweight"], "99"),
        linkgroup == "Obese"          ~ paste0(BMI_Category_colours["Obese"], "99"))) |>
    dplyr::filter(value > 7)
  
  # Explicit node order (left then right)
  ordered_nodes <- c("Underweight_unadj", "Healthy Weight_unadj", "Overweight_unadj", "Obese_unadj",
                     "Underweight_adj", "Healthy Weight_adj", "Overweight_adj", "Obese_adj")
  
  # Build nodes with consistent labels and colours
  sankey_nodes <- tibble::tibble(name = ordered_nodes,
                         # category without suffix, for color mapping
                         nodegroup = sub("_(unadj|adj)$", "", ordered_nodes),
                         colour = dplyr::case_when(
                           nodegroup == "Underweight"    ~ BMI_Category_colours["Underweight"],
                           nodegroup == "Healthy Weight" ~ BMI_Category_colours["Healthy Weight"],
                           nodegroup == "Overweight"     ~ BMI_Category_colours["Overweight"],
                           nodegroup == "Obese"          ~ BMI_Category_colours["Obese"]))
  
  # Labels for nodes
  node_labels <- sub("_(unadj|adj)$", "", sankey_nodes$name)
  
  # Map links to node indices based on explicit node order
  sankey_links$IDsource <- match(sankey_links$source, sankey_nodes$name) - 1
  sankey_links$IDtarget <- match(sankey_links$target, sankey_nodes$name) - 1
  
  # Plot
  plotly::plot_ly(type = "sankey",
          arrangement = "snap",
          orientation = "h",
          node = list(label = node_labels,          # length 8; no recycling
                      color = sankey_nodes$colour,
                      pad = 15,
                      thickness = 20,
                      line = list(color = "black", width = 0.5),
                      hoverinfo = "none"),
          link = list(source = sankey_links$IDsource,
                      target = sankey_links$IDtarget,
                      value  = sankey_links$value,
                      label  = sankey_links$linklabel,
                      color  = sankey_links$colour,
                      customdata = sprintf("%.1f%% (%.1f%% - %.1f%%)", sankey_links$pct, sankey_links$lower_ci, sankey_links$upper_ci),
                      hovertemplate = paste0("%{label}<br>%{customdata}<extra></extra>"))) |> 
    add_annotations(x = 0, y = 1.01, xref = "paper", yref = "paper",
                    text = "Unadjusted BMI score", showarrow = FALSE,
                    xanchor = "left", yanchor = "bottom",
                    font = list(size = 16, color = "black")) |> 
    add_annotations(x = 1, y = 1.01, xref = "paper", yref = "paper",
                    text = "Adjusted BMI score", showarrow = FALSE,
                    xanchor = "right", yanchor = "bottom",
                    font = list(size = 16, color = "black")) |> 
    layout(margin = list(t = 40)) |> 
    config(modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'zoomIn', 'zoomOut', 'lasso2d', 'autoScale', 'resetScale'))
}


