test_protocol <- phr::IPHRAProtocol$new()

test_protocol$framework$modified_objectives_schema

test_protocol$framework$modified_indicator_bank

test_protocol$framework$modify_adjusted_schema(objective_codes = c("101", "102", "103"))

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




objs <- test_protocol$framework$modified_objectives_schema[
  , c("sector", "pillar", "sub_pillar", "objective_code")
]
objs



test_protocol$sample_object$add_stratum(
  stratum_id = "Population A",
  stratum_name = "Population A",
  pop_indicator = "Food Consumption Score",
  population_size = 45000,
  pop_design_effect = 1.5,
  pop_precision = 10,
  pop_expected_prevalence = 50,
  pop_nonresponse = 10,
  ind_indicator = "wasting_prevalence",
  ind_expected_prevalence = 15,
  ind_precision = 5,
  ind_nonresponse = 10,
  ind_design_effect = 1.5,
  ind_avg_hh_size = 5.2,
  ind_subpop_prop = 20,
  rate_indicator = "crude_death_rate",
  rate_expected_rate = 0.5,
  rate_precision = 0.5,
  rate_avg_hh_size = 5.2,
  rate_design_effect = 2,
  rate_fpc = FALSE,
  rate_nonresponse = 10,
  teams = 5,
  enumerators_per_team = 1,
  start_time = "10:00",
  end_time = "18:00",
  clusters_per_day = 2,
  avg_interview_time = 30,
  avg_rest_time = 30,
  avg_travel_time = 60,
  sampling_method_site = "systematic",
  sampling_method_hh = "systematic",
  n_sites = 10
)



test_protocol$sampling_frame$set(field = "log_df", value = read.csv("inst/resources/example_sampling_frame.csv") |>
                                   dplyr::mutate(
  inclusion = ifelse(inclusion == "True", TRUE, FALSE)))

a <- test_protocol$sampling_frame$get(field = "log_df")
str(a)

test_protocol$sampling_frame$draw_sample(strata_table = test_protocol$sample_object$sample_table, seed = 678)

test_protocol$sampling_frame$drawn_sample

test_protocol$sampling_frame$get(field = "drawn_sample_full")

test_protocol$get_allowable_tools()

test_protcol$add_tools(tool_name = "tool_household_iphra_v2")

test_protocol$metadata$audience_matric <- "test"




test_protocol$metadata$research_cycle_id <- "RC-2025-001"
test_protocol$metadata$country <- "Switzerland"
test_protocol$metadata$release_date <- Sys.Date()
test_protocol$metadata$version_number <- "1.1"
test_protocol$metadata$type_emergency <- "Protracted"
test_protocol$metadata$type_crisis <- "Conflict"
test_protocol$metadata$population <- "Internally Displaced Persons"
test_protocol$metadata$rationale <- "Recent population movements from conflict area, populations not served."
test_protocol$metadata$date_pilot_training <- "2025-04-15"
test_protocol$metadata$date_data_collection_start <- "2025-05-01"
test_protocol$metadata$date_data_collection_end <- "2025-05-15"
test_protocol$metadata$date_data_analysis <- "2025-05-20"
test_protocol$metadata$date_data_validation <- "2025-05-18"
test_protocol$metadata$date_preliminary_presentation <- "2025-05-25"
test_protocol$metadata$date_outputs_validation <- "2025-05-30"
test_protocol$metadata$date_outputs_publication <- "2025-06-05"
test_protocol$metadata$date_final_presentation <- "2025-06-10"
test_protocol$metadata$audience_type_cluster <- "Life-Saving Clusters"
test_protocol$metadata$expected_output_cluster <- "Preliminary Presentation, Technical Report"
test_protocol$metadata$expected_output_donor <- "Brief"
test_protocol$metadata$expected_output_operational_actor <- "Technical Report, Factsheet"
test_protocol$metadata$expected_output_other <- "Not applicable"
test_protocol$metadata$dissemination_strategy_cluster <- "In-Person, Email"
test_protocol$metadata$dissemination_strategy_donor <- "Email"
test_protocol$metadata$dissemination_strategy_operational_actor <- "Remote, Email"
test_protocol$metadata$dissemination_strategy_other <- "Not applicable"
test_protocol$metadata$access_cluster <- "Public"
test_protocol$metadata$access_donor <- "Bilateral, Restricted"
test_protocol$metadata$access_operational_actor <- "Restricted"
test_protocol$metadata$access_other <- "Not applicable"
test_protocol$metadata$visibility_cluster <- "Public"
test_protocol$metadata$visibility_donor <- "Restricted"
test_protocol$metadata$visibility_operational_actor <- "Restricted"
test_protocol$metadata$visibility_other <- "Not applicable"

test_protocol$metadata$month_year <- "June 2025"
test_protocol$metadata$country_name <- "Switzerland"
test_protocol$metadata$assessment_title <- "Integrated Public Health Rapid Assessment - Switzerland"
test_protocol$metadata$protocol_version <- "1.0"
test_protocol$metadata$version <- 1L
# Text metadata fields
test_protocol$metadata$mandating_body <- "IMPACT Initiatives"
test_protocol$metadata$project_code <- "98BSY"

test_protocol$metadata$research_cycle_id <- "RC-2025-001"
test_protocol$metadata$country <- "Switzerland"
test_protocol$metadata$release_date <- Sys.Date()
test_protocol$metadata$version_number <- "1.1"
test_protocol$metadata$type_emergency <- "Protracted"
test_protocol$metadata$type_crisis <- "Conflict"
test_protocol$metadata$population <- "Internally Displaced Persons"
test_protocol$metadata$rationale <- "Recent population movements from conflict area, populations not served."
test_protocol$metadata$date_pilot_training <- "2025-04-15"
test_protocol$metadata$date_data_collection_start <- "2025-05-01"
test_protocol$metadata$date_data_collection_end <- "2025-05-15"
test_protocol$metadata$date_data_analysis <- "2025-05-20"
test_protocol$metadata$date_data_validation <- "2025-05-18"
test_protocol$metadata$date_preliminary_presentation <- "2025-05-25"
test_protocol$metadata$date_outputs_validation <- "2025-05-30"
test_protocol$metadata$date_outputs_publication <- "2025-06-05"
test_protocol$metadata$date_final_presentation <- "2025-06-10"
test_protocol$metadata$audience_type_cluster <- "Life-Saving Clusters"
test_protocol$metadata$expected_output_cluster <- "Preliminary Presentation, Technical Report"
test_protocol$metadata$expected_output_donor <- "Brief"
test_protocol$metadata$expected_output_operational_actor <- "Technical Report, Factsheet"
test_protocol$metadata$expected_output_other <- "Not applicable"
test_protocol$metadata$dissemination_strategy_cluster <- "In-Person, Email"
test_protocol$metadata$dissemination_strategy_donor <- "Email"
test_protocol$metadata$dissemination_strategy_operational_actor <- "Remote, Email"
test_protocol$metadata$dissemination_strategy_other <- "Not applicable"
test_protocol$metadata$access_cluster <- "Public"
test_protocol$metadata$access_donor <- "Bilateral, Restricted"
test_protocol$metadata$access_operational_actor <- "Restricted"
test_protocol$metadata$access_other <- "Not applicable"
test_protocol$metadata$visibility_cluster <- "Public"
test_protocol$metadata$visibility_donor <- "Restricted"
test_protocol$metadata$visibility_operational_actor <- "Restricted"
test_protocol$metadata$visibility_other <- "Not applicable"

test_protocol$metadata$month_year <- "June 2025"
test_protocol$metadata$country_name <- "Switzerland"
test_protocol$metadata$assessment_title <- "Integrated Public Health Rapid Assessment - Switzerland"
test_protocol$metadata$protocol_version <- "1.0"
test_protocol$metadata$version <- 1L
# Text metadata fields
test_protocol$metadata$mandating_body <- "IMPACT Initiatives"
test_protocol$metadata$project_code <- "98BSY"

test_protocol$access_nested(
  field = "framework",
  member = "add_secondary_data_source",
  objective = 105,
  source = "UNHCR Population Statistics",
  purpose = "To provide context on population movements and displacement trends."
)

test_protocol$generate_quarto_doc()


