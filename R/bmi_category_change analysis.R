library(tidyverse)
library(PHEindicatormethods)
library(plotly)

# summarise unadjusted BMI categories by ethnicity

bmi_category_unadj_by_ethnicity <- ncmp_summarise(ncmp_data_period_8,
                                                  .by_count = c(School_Year, Ethnicity_Hudda, BMI_Category),
                                                  .by_denom = c(School_Year, Ethnicity_Hudda),
                                                  .include_oo = T,
                                                  .round_5 = T) |> 
  mutate(bmi_adj = "Unadjusted")

# summarise adjusted BMI categories by ethnicity

bmi_category_adj_by_ethnicity <- ncmp_data_period_8 |> 
  select(-BMI_Category) |> 
  rename(BMI_Category = BMI_Category_Adjusted) |> 
  ncmp_summarise(.by_count = c(School_Year, Ethnicity_Hudda, BMI_Category),
                 .by_denom = c(School_Year, Ethnicity_Hudda),
                 .include_oo = T,
                 .round_5 = T) |> 
  mutate(bmi_adj = "Adjusted")

# combine into one table

bmi_category_by_ethnicity = rbind(bmi_category_adj_by_ethnicity,
                                  bmi_category_unadj_by_ethnicity)

bcc_colours <- c("#D00070", "#FFAD00", "#75BC22", "#84329B", "#00A9E0", "#3c3c3b")

bmi_category_by_ethnicity |>
  filter(School_Year == "Year 6",
         BMI_Category != "Overweight/Obese") |> 
  mutate(BMI_Category = factor(BMI_Category,
                               levels = c("Obese", "Overweight", "Healthy Weight", "Underweight"))) |> 
  ggplot(aes(x = pct,
             y = bmi_adj,
             fill = BMI_Category)) +
  geom_col() +
  scale_fill_manual(values = c(bcc_colours[4:2], bcc_colours[5])) +
  facet_wrap(~Ethnicity_Hudda)

bmi_category_by_ethnicity |>
  filter(School_Year == "Reception",
         BMI_Category == "Overweight/Obese") |> 
  ggplot(aes(x = Ethnicity_Hudda,
             y = pct,
             fill = bmi_adj)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  scale_fill_manual(values = bcc_colours[1:2])

bmi_category_by_ethnicity |>
  filter(School_Year == "Year 6",
         BMI_Category != "Overweight/Obese") |> 
  mutate(BMI_Category = factor(BMI_Category,
                               levels = c("Obese", "Overweight", "Healthy Weight", "Underweight"))) |> 
  ggplot(aes(x = BMI_Category,
             y = pct,
             fill = bmi_adj)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  scale_fill_manual(values = c(bcc_colours[4:2], bcc_colours[5])) +
  facet_wrap(~Ethnicity_Hudda)

bmi_category_by_ethnicity |> 
  pivot_wider(names_from = bmi_adj,
              values_from = c(pct, lower_ci, upper_ci, count)) |> 
  mutate(pct_diff = pct_Adjusted - pct_Unadjusted,
         count_diff = count_Adjusted - count_Unadjusted) |> 
  View()


# calculate proportion of children within ethnicity/school year who fit into each BMI category/adjusted category combo
bmi_category_change_by_ethnicity_ <- ncmp_summarise(ncmp_data_period_8,
               .by_count = c(School_Year, Ethnicity_Hudda, BMI_Category, BMI_Category_Adjusted),
               .by_denom = c(School_Year, Ethnicity_Hudda),
               .include_oo = F,
               .round_5 = T) |> 
  mutate(category_change = case_when(BMI_Category == BMI_Category_Adjusted ~ F,
                                     .default = T))

# calculate proportion of children within ethncicity/school year/bmi category who fit into each adjusted category
adjusted_bmi_by_ethnicity_bmi_category <- ncmp_summarise(ncmp_data_period_8,
                                                         .by_count = c(School_Year, Ethnicity_Hudda, BMI_Category, BMI_Category_Adjusted),
                                                         .by_denom = c(School_Year, Ethnicity_Hudda, BMI_Category),
                                                         .include_oo = F,
                                                         .round_5 = T) |> 
  mutate(category_change = case_when(BMI_Category == BMI_Category_Adjusted ~ F,
                                     .default = T))


# sankey chart
sankey_links <- adjusted_bmi_by_ethnicity_bmi_category |>
  filter(Ethnicity_Hudda == "Black",
         School_Year == "Year 6") |> 
  select(BMI_Category, BMI_Category_Adjusted, count, pct, lower_ci, upper_ci) |>
  rename(source = BMI_Category,
         target = BMI_Category_Adjusted,
         value  = count) |>
  mutate(
    linkgroup = source,
    linklabel = case_when(
      source == "Underweight"     & target == "Underweight"     ~ "Remained underweight",
      source == "Healthy Weight"  & target == "Healthy Weight"  ~ "Remained healthy weight",
      source == "Overweight"      & target == "Overweight"      ~ "Remained overweight",
      source == "Obese"           & target == "Obese"           ~ "Remained obese",
      .default = paste0("Moved to ", str_to_lower(target))),
    source = paste0(source, "_unadj"),
    target = paste0(target, "_adj"),
    colour = case_when(
      linkgroup == "Underweight"    ~ "#00A9E099",
      linkgroup == "Healthy Weight" ~ "#FFAD0099",
      linkgroup == "Overweight"     ~ "#75BC2299",
      linkgroup == "Obese"          ~ "#84329B99")) |>
  filter(value > 7)

# Explicit node order (left then right)
ordered_nodes <- c("Underweight_unadj", "Healthy Weight_unadj", "Overweight_unadj", "Obese_unadj",
                   "Underweight_adj", "Healthy Weight_adj", "Overweight_adj", "Obese_adj")

# Build nodes with consistent labels and colours
sankey_nodes <- tibble(name = ordered_nodes,
                       # category without suffix, for color mapping
                       nodegroup = sub("_(unadj|adj)$", "", ordered_nodes),
                       colour = case_when(
                         nodegroup == "Underweight"    ~ "#00A9E0",
                         nodegroup == "Healthy Weight" ~ "#FFAD00",
                         nodegroup == "Overweight"     ~ "#75BC22",
                         nodegroup == "Obese"          ~ "#84329B"))

# Labels for nodes
node_labels <- sub("_(unadj|adj)$", "", sankey_nodes$name)

# Map links to node indices based on explicit node order
sankey_links$IDsource <- match(sankey_links$source, sankey_nodes$name) - 1
sankey_links$IDtarget <- match(sankey_links$target, sankey_nodes$name) - 1


# Plot
plot_ly(type = "sankey",
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
