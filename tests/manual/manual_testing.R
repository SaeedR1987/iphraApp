test_protocol <- phr::IPHRAProtocol$new()

test_protocol$framework$modified_indicator_bank

test_protocol$get_allowable_tools()

test_protocol$add_tools("tool_household_iphra_v2")

test_protocol$remove_tools("tool_household_iphra_v2")

test_protocol$.tool_household_iphra

test_protocol$tools$tool_household_iphra_v2$filter_survey_by_indicator(indicator_codes = )

library(dplyr)

test_protocol$tools$tool_household_iphra_v2$survey %>%
  dplyr::group_by(indicator_code) %>%
  dplyr::summarise(time = sum(as.numeric(time_seconds))) %>%
  dplyr::mutate(time_minutes = time / 60) %>%
  dplyr::filter(time > 0) %>% View()
