#School Data Summary
school_data_summary <- school_data_temp |>
  filter(year != 2023) |>
  #drop_na(net_opeb_assets, net_opeb_liability, net_pension_assets, net_pension_liability,
   #       expenses, total_liabilities, revenues, total_assets) |>
  mutate(enrollment = case_when(
    year == 2020 ~ enrollment_20,
    year == 2021 ~ enrollment_21,
    year == 2022 ~ enrollment_22,
    TRUE ~ NA_real_  # Assign NA for any other years, if applicable
  )) |> 
  mutate(
    debt_ratio = total_liabilities / total_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
    net_net_pension_liability = net_pension_liability - net_pension_assets,
  ) |>
  group_by(year) |>
  summarise(
    net_net_pension_liability_sum = sum(net_net_pension_liability, na.rm = TRUE),
    net_net_opeb_liability_sum = sum(net_net_opeb_liability, na.rm = TRUE),
    expenses_sum = sum(expenses, na.rm = TRUE),
    total_liabilities_sum = sum(total_liabilities, na.rm = TRUE),
    revenues_sum = sum(revenues, na.rm = TRUE),
    total_assets_sum = sum(total_assets, na.rm = TRUE),
    debt_ratio = total_liabilities_sum / total_assets_sum,
    net_net_pension_liability_pc = net_net_pension_liability_sum / sum(enrollment[!is.na(net_net_pension_liability_sum)], na.rm = TRUE),
    net_net_opeb_liability_pc = net_net_opeb_liability_sum / sum(enrollment[!is.na(net_net_opeb_liability_sum)], na.rm = TRUE),
    expenses_pc = expenses_sum / sum(enrollment[!is.na(expenses_sum)], na.rm = TRUE),
    total_liabilities_pc = total_liabilities_sum / sum(enrollment[!is.na(total_liabilities_sum)], na.rm = TRUE),
    revenues_pc = revenues_sum / sum(enrollment[!is.na(revenues_sum)], na.rm = TRUE),
    total_assets_pc = total_assets_sum / sum(enrollment[!is.na(total_assets_sum)], na.rm = TRUE)
  ) |>
  select(
    year,
    debt_ratio,
    net_net_pension_liability_sum,
    net_net_pension_liability_pc,
    net_net_opeb_liability_sum,
    net_net_opeb_liability_pc,
    expenses_sum,
    expenses_pc,
    total_liabilities_sum,
    total_liabilities_pc,
    revenues_sum,
    revenues_pc,
    total_assets_sum,
    total_assets_pc
  ) |>
  rename_with(~ str_replace(., "_sum", ""), ends_with("_sum"))
