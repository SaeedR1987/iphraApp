# ────────────────────────────────────────────────────────────────────────────────
# IPHRA Schema Management Utilities
# ────────────────────────────────────────────────────────────────────────────────
#
# This module provides utilities for managing different types of schemas used
# in the IPHRA application. Schemas define the structure and validation rules
# for data, quality checks, analysis, and integrated analysis.
#
# Schema Types:
# - Data Schema: Defines expected data structure for imports
# - Quality Schema: Defines quality check rules and thresholds
# - Analysis Schema: Defines analysis parameters and indicators
# - Integrated Analysis Schema: Defines integrated analysis framework
#
# ────────────────────────────────────────────────────────────────────────────────

# ────────────────────────────────────────────────────────────────────────────────
# Schema Type Constants
# ────────────────────────────────────────────────────────────────────────────────

#' @title Schema Types
#' @description Available schema types in the IPHRA application
#' @noRd
IPHRA_SCHEMA_TYPES <- c("data", "quality", "analysis", "integrated_analysis")

#' @title Schema Sectors
#' @description Available sectors for schema management
#' @noRd
IPHRA_SCHEMA_SECTORS <- c("FSL", "Health", "WASH", "Nutrition", "MUAC", "Mortality")

# ────────────────────────────────────────────────────────────────────────────────
# Default Schema Definitions
# ────────────────────────────────────────────────────────────────────────────────

#' @title Get Default Data Schema
#' @description Returns the default data schema for a specific sector
#' @param sector Character string specifying the sector (FSL, Health, WASH, etc.)
#' @return A data frame representing the default data schema
#' @noRd
iphra_get_default_data_schema <- function(sector = "FSL") {
  # Base schema structure common to all sectors
  base_schema <- data.frame(
    variable_name = character(),
    variable_label = character(),
    data_type = character(),
    required = logical(),
    allowed_values = character(),
    validation_rule = character(),
    description = character(),
    stringsAsFactors = FALSE
  )

  # Sector-specific schemas
  schemas <- list(
    FSL = data.frame(
      variable_name = c("uuid", "admin1", "admin2", "hh_size", "fcs_score",
                        "hdds_score", "rcsi_score", "lcs_category", "fies_score"),
      variable_label = c("Unique ID", "Admin Level 1", "Admin Level 2",
                         "Household Size", "Food Consumption Score",
                         "Household Dietary Diversity Score",
                         "Reduced Coping Strategies Index",
                         "Livelihood Coping Strategy Category",
                         "Food Insecurity Experience Scale Score"),
      data_type = c("character", "character", "character", "numeric",
                    "numeric", "numeric", "numeric", "character", "numeric"),
      required = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
      allowed_values = c("", "", "", "1-50", "0-112", "0-12", "0-56",
                         "None|Stress|Crisis|Emergency", "0-8"),
      validation_rule = c("not_null", "not_null", "not_null", "range:1:50",
                          "range:0:112", "range:0:12", "range:0:56",
                          "choice", "range:0:8"),
      description = c("Unique identifier for each record",
                      "First administrative level",
                      "Second administrative level",
                      "Number of household members",
                      "FCS composite score",
                      "HDDS composite score",
                      "rCSI composite score",
                      "LCS classification category",
                      "FIES raw score"),
      stringsAsFactors = FALSE
    ),
    Health = data.frame(
      variable_name = c("uuid", "admin1", "admin2", "hh_size", "sick_2weeks",
                        "healthcare_access", "vaccination_status", "chronic_illness"),
      variable_label = c("Unique ID", "Admin Level 1", "Admin Level 2",
                         "Household Size", "Illness in Past 2 Weeks",
                         "Healthcare Access", "Vaccination Status",
                         "Chronic Illness Present"),
      data_type = c("character", "character", "character", "numeric",
                    "logical", "character", "character", "logical"),
      required = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
      allowed_values = c("", "", "", "1-50", "TRUE|FALSE",
                         "Yes|No|Sometimes", "Complete|Partial|None", "TRUE|FALSE"),
      validation_rule = c("not_null", "not_null", "not_null", "range:1:50",
                          "logical", "choice", "choice", "logical"),
      description = c("Unique identifier for each record",
                      "First administrative level",
                      "Second administrative level",
                      "Number of household members",
                      "Any household member sick in past 2 weeks",
                      "Ability to access healthcare services",
                      "Vaccination completion status",
                      "Presence of chronic illness in household"),
      stringsAsFactors = FALSE
    ),
    WASH = data.frame(
      variable_name = c("uuid", "admin1", "admin2", "hh_size", "water_source",
                        "water_treatment", "latrine_type", "handwashing_facility"),
      variable_label = c("Unique ID", "Admin Level 1", "Admin Level 2",
                         "Household Size", "Main Water Source",
                         "Water Treatment Method", "Latrine Type",
                         "Handwashing Facility"),
      data_type = c("character", "character", "character", "numeric",
                    "character", "character", "character", "character"),
      required = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
      allowed_values = c("", "", "", "1-50",
                         "Piped|Borehole|Well|Surface|Other",
                         "Boiling|Chlorine|Filter|None",
                         "Flush|Pit|VIP|None|Other",
                         "Fixed|Mobile|None"),
      validation_rule = c("not_null", "not_null", "not_null", "range:1:50",
                          "choice", "choice", "choice", "choice"),
      description = c("Unique identifier for each record",
                      "First administrative level",
                      "Second administrative level",
                      "Number of household members",
                      "Primary drinking water source",
                      "Method used to treat drinking water",
                      "Type of sanitation facility",
                      "Type of handwashing facility"),
      stringsAsFactors = FALSE
    ),
    Nutrition = data.frame(
      variable_name = c("uuid", "child_id", "age_months", "sex", "weight_kg",
                        "height_cm", "muac_mm", "oedema", "waz", "haz", "whz"),
      variable_label = c("Unique ID", "Child ID", "Age in Months", "Sex",
                         "Weight (kg)", "Height (cm)", "MUAC (mm)", "Bilateral Oedema",
                         "Weight-for-Age Z-score", "Height-for-Age Z-score",
                         "Weight-for-Height Z-score"),
      data_type = c("character", "character", "numeric", "character",
                    "numeric", "numeric", "numeric", "logical",
                    "numeric", "numeric", "numeric"),
      required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                   FALSE, FALSE, FALSE),
      allowed_values = c("", "", "0-59", "M|F", "0.5-30", "45-130",
                         "80-200", "TRUE|FALSE", "-6:6", "-6:6", "-6:6"),
      validation_rule = c("not_null", "not_null", "range:0:59", "choice",
                          "range:0.5:30", "range:45:130", "range:80:200",
                          "logical", "range:-6:6", "range:-6:6", "range:-6:6"),
      description = c("Unique identifier for each record",
                      "Child identifier within household",
                      "Age of child in months",
                      "Sex of child",
                      "Child weight in kilograms",
                      "Child height/length in centimeters",
                      "Mid-upper arm circumference in millimeters",
                      "Presence of bilateral pitting oedema",
                      "Weight-for-age z-score",
                      "Height-for-age z-score",
                      "Weight-for-height z-score"),
      stringsAsFactors = FALSE
    ),
    MUAC = data.frame(
      variable_name = c("uuid", "child_id", "age_months", "sex", "muac_mm",
                        "muac_category", "oedema"),
      variable_label = c("Unique ID", "Child ID", "Age in Months", "Sex",
                         "MUAC (mm)", "MUAC Category", "Bilateral Oedema"),
      data_type = c("character", "character", "numeric", "character",
                    "numeric", "character", "logical"),
      required = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
      allowed_values = c("", "", "6-59", "M|F", "80-200",
                         "SAM|MAM|At Risk|Normal", "TRUE|FALSE"),
      validation_rule = c("not_null", "not_null", "range:6:59", "choice",
                          "range:80:200", "choice", "logical"),
      description = c("Unique identifier for each record",
                      "Child identifier within household",
                      "Age of child in months",
                      "Sex of child",
                      "Mid-upper arm circumference in millimeters",
                      "MUAC-based malnutrition category",
                      "Presence of bilateral pitting oedema"),
      stringsAsFactors = FALSE
    ),
    Mortality = data.frame(
      variable_name = c("uuid", "admin1", "admin2", "recall_days",
                        "hh_members_start", "hh_members_end", "births", "joins",
                        "deaths", "left", "death_cause", "death_location"),
      variable_label = c("Unique ID", "Admin Level 1", "Admin Level 2",
                         "Recall Period (days)", "HH Members at Start",
                         "HH Members at End", "Births", "Joins",
                         "Deaths", "Left Household", "Cause of Death",
                         "Location of Death"),
      data_type = c("character", "character", "character", "numeric",
                    "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "character", "character"),
      required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
                   FALSE, FALSE, FALSE, FALSE),
      allowed_values = c("", "", "", "1-365", "1-50", "0-50",
                         "0-10", "0-20", "0-20", "0-20",
                         "Violence|Disease|Accident|Other|Unknown",
                         "Home|Health Facility|Other"),
      validation_rule = c("not_null", "not_null", "not_null", "range:1:365",
                          "range:1:50", "range:0:50", "range:0:10",
                          "range:0:20", "range:0:20", "range:0:20",
                          "choice", "choice"),
      description = c("Unique identifier for each record",
                      "First administrative level",
                      "Second administrative level",
                      "Number of days in recall period",
                      "Household members at start of recall",
                      "Household members at end of recall",
                      "Number of births during recall",
                      "Number who joined household",
                      "Number of deaths during recall",
                      "Number who left household",
                      "Primary cause of death",
                      "Location where death occurred"),
      stringsAsFactors = FALSE
    )
  )

  if (sector %in% names(schemas)) {
    return(schemas[[sector]])
  } else {
    return(base_schema)
  }
}

#' @title Get Default Quality Schema
#' @description Returns the default quality schema for a specific sector
#' @param sector Character string specifying the sector (FSL, Health, WASH, etc.)
#' @return A data frame representing the default quality schema
#' @noRd
iphra_get_default_quality_schema <- function(sector = "FSL") {
  # Base quality schema structure
  base_schema <- data.frame(
    check_id = character(),
    check_name = character(),
    check_type = character(),
    threshold_warning = numeric(),
    threshold_error = numeric(),
    description = character(),
    enabled = logical(),
    stringsAsFactors = FALSE
  )

  # Sector-specific quality schemas
  schemas <- list(
    FSL = data.frame(
      check_id = c("fsl_q1", "fsl_q2", "fsl_q3", "fsl_q4", "fsl_q5"),
      check_name = c("FCS Range Check", "FCS Zero Values", "HDDS Completeness",
                     "rCSI Consistency", "LCS Logic"),
      check_type = c("range", "frequency", "completeness", "consistency", "logic"),
      threshold_warning = c(5, 10, 90, 5, 5),
      threshold_error = c(10, 20, 80, 10, 10),
      description = c("Check FCS scores within valid range (0-112)",
                      "Flag high frequency of zero FCS values",
                      "Ensure HDDS components are complete",
                      "Check rCSI components sum correctly",
                      "Verify LCS category logic is consistent"),
      enabled = c(TRUE, TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    Health = data.frame(
      check_id = c("health_q1", "health_q2", "health_q3", "health_q4"),
      check_name = c("Age Validation", "Treatment Logic", "Symptom Consistency",
                     "Vaccination Logic"),
      check_type = c("range", "logic", "consistency", "logic"),
      threshold_warning = c(5, 5, 10, 5),
      threshold_error = c(10, 10, 15, 10),
      description = c("Validate age values within acceptable range",
                      "Check treatment sought when illness reported",
                      "Verify symptom and diagnosis consistency",
                      "Check vaccination status logic"),
      enabled = c(TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    WASH = data.frame(
      check_id = c("wash_q1", "wash_q2", "wash_q3", "wash_q4"),
      check_name = c("Water Quantity Check", "Sanitation Logic", "Hygiene Consistency",
                     "Treatment Validation"),
      check_type = c("range", "logic", "consistency", "logic"),
      threshold_warning = c(5, 5, 10, 5),
      threshold_error = c(10, 10, 15, 10),
      description = c("Check water quantity within reasonable range",
                      "Verify sanitation facility and usage logic",
                      "Check hygiene practice consistency",
                      "Validate water treatment logic"),
      enabled = c(TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    Nutrition = data.frame(
      check_id = c("nut_q1", "nut_q2", "nut_q3", "nut_q4", "nut_q5"),
      check_name = c("Age Range Check", "Anthropometric Flags", "Z-score Range",
                     "MUAC Consistency", "Oedema Logic"),
      check_type = c("range", "smart_flags", "range", "consistency", "logic"),
      threshold_warning = c(5, 10, 5, 5, 1),
      threshold_error = c(10, 15, 10, 10, 5),
      description = c("Check child age within 0-59 months",
                      "Flag anthropometric measurements using SMART flags",
                      "Check z-scores within biologically plausible range",
                      "Verify MUAC and weight/height consistency",
                      "Check oedema reporting logic"),
      enabled = c(TRUE, TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    MUAC = data.frame(
      check_id = c("muac_q1", "muac_q2", "muac_q3"),
      check_name = c("MUAC Range Check", "Age Eligibility", "Category Assignment"),
      check_type = c("range", "range", "logic"),
      threshold_warning = c(5, 5, 5),
      threshold_error = c(10, 10, 10),
      description = c("Check MUAC values within valid range (80-200mm)",
                      "Verify child age eligibility (6-59 months)",
                      "Validate MUAC category assignment logic"),
      enabled = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    Mortality = data.frame(
      check_id = c("mort_q1", "mort_q2", "mort_q3", "mort_q4"),
      check_name = c("Recall Period", "Demographic Accounting", "Death Rate Range",
                     "Cause of Death"),
      check_type = c("range", "balance", "range", "completeness"),
      threshold_warning = c(5, 5, 5, 80),
      threshold_error = c(10, 10, 10, 70),
      description = c("Validate recall period is appropriate",
                      "Check demographic accounting balance",
                      "Flag unusually high/low death rates",
                      "Ensure cause of death is recorded"),
      enabled = c(TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    )
  )

  if (sector %in% names(schemas)) {
    return(schemas[[sector]])
  } else {
    return(base_schema)
  }
}

#' @title Get Default Analysis Schema
#' @description Returns the default analysis schema for a specific sector
#' @param sector Character string specifying the sector (FSL, Health, WASH, etc.)
#' @return A data frame representing the default analysis schema
#' @noRd
iphra_get_default_analysis_schema <- function(sector = "FSL") {
  # Base analysis schema structure
  base_schema <- data.frame(
    indicator_id = character(),
    indicator_name = character(),
    calculation_type = character(),
    numerator = character(),
    denominator = character(),
    disaggregation = character(),
    classification = character(),
    enabled = logical(),
    stringsAsFactors = FALSE
  )

  # Sector-specific analysis schemas
  schemas <- list(
    FSL = data.frame(
      indicator_id = c("fsl_a1", "fsl_a2", "fsl_a3", "fsl_a4", "fsl_a5", "fsl_a6"),
      indicator_name = c("FCS Poor", "FCS Borderline", "FCS Acceptable",
                         "rCSI Mean", "HDDS Mean", "LCS Emergency"),
      calculation_type = c("proportion", "proportion", "proportion",
                           "mean", "mean", "proportion"),
      numerator = c("fcs_score < 21.5", "fcs_score >= 21.5 & fcs_score < 35.5",
                    "fcs_score >= 35.5", "rcsi_score", "hdds_score",
                    "lcs_category == 'Emergency'"),
      denominator = c("all", "all", "all", "all", "all", "all"),
      disaggregation = c("admin1|admin2", "admin1|admin2", "admin1|admin2",
                         "admin1|admin2", "admin1|admin2", "admin1|admin2"),
      classification = c("outcome", "outcome", "outcome",
                         "coping", "consumption", "coping"),
      enabled = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    Health = data.frame(
      indicator_id = c("health_a1", "health_a2", "health_a3", "health_a4"),
      indicator_name = c("Morbidity Rate", "Healthcare Access Rate",
                         "Treatment Seeking Rate", "Vaccination Coverage"),
      calculation_type = c("proportion", "proportion", "proportion", "proportion"),
      numerator = c("sick_2weeks == TRUE", "healthcare_access == 'Yes'",
                    "treatment_sought == TRUE", "vaccination_status == 'Complete'"),
      denominator = c("all", "all", "sick_2weeks == TRUE", "eligible_children"),
      disaggregation = c("admin1|admin2|sex|age_group", "admin1|admin2",
                         "admin1|admin2|sex", "admin1|admin2"),
      classification = c("morbidity", "access", "utilization", "coverage"),
      enabled = c(TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    WASH = data.frame(
      indicator_id = c("wash_a1", "wash_a2", "wash_a3", "wash_a4", "wash_a5"),
      indicator_name = c("Improved Water Source", "Water Treatment",
                         "Improved Sanitation", "Open Defecation",
                         "Handwashing with Soap"),
      calculation_type = c("proportion", "proportion", "proportion",
                           "proportion", "proportion"),
      numerator = c("water_source %in% c('Piped', 'Borehole')",
                    "water_treatment != 'None'",
                    "latrine_type %in% c('Flush', 'VIP')",
                    "latrine_type == 'None'",
                    "handwashing_facility != 'None' & soap_available == TRUE"),
      denominator = c("all", "all", "all", "all", "all"),
      disaggregation = c("admin1|admin2", "admin1|admin2", "admin1|admin2",
                         "admin1|admin2", "admin1|admin2"),
      classification = c("water", "water", "sanitation", "sanitation", "hygiene"),
      enabled = c(TRUE, TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    Nutrition = data.frame(
      indicator_id = c("nut_a1", "nut_a2", "nut_a3", "nut_a4", "nut_a5"),
      indicator_name = c("GAM by WHZ", "SAM by WHZ", "Stunting", "Underweight",
                         "GAM by MUAC"),
      calculation_type = c("proportion", "proportion", "proportion",
                           "proportion", "proportion"),
      numerator = c("whz < -2 | oedema == TRUE", "whz < -3 | oedema == TRUE",
                    "haz < -2", "waz < -2",
                    "muac_mm < 125 | oedema == TRUE"),
      denominator = c("children_6_59m", "children_6_59m", "children_0_59m",
                      "children_0_59m", "children_6_59m"),
      disaggregation = c("admin1|admin2|sex", "admin1|admin2|sex",
                         "admin1|admin2|sex|age_group",
                         "admin1|admin2|sex|age_group", "admin1|admin2|sex"),
      classification = c("acute", "acute", "chronic", "composite", "acute"),
      enabled = c(TRUE, TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    MUAC = data.frame(
      indicator_id = c("muac_a1", "muac_a2", "muac_a3", "muac_a4"),
      indicator_name = c("SAM by MUAC", "MAM by MUAC", "At Risk by MUAC",
                         "Normal by MUAC"),
      calculation_type = c("proportion", "proportion", "proportion", "proportion"),
      numerator = c("muac_mm < 115 | oedema == TRUE",
                    "muac_mm >= 115 & muac_mm < 125",
                    "muac_mm >= 125 & muac_mm < 135",
                    "muac_mm >= 135"),
      denominator = c("children_6_59m", "children_6_59m",
                      "children_6_59m", "children_6_59m"),
      disaggregation = c("admin1|admin2|sex", "admin1|admin2|sex",
                         "admin1|admin2|sex", "admin1|admin2|sex"),
      classification = c("acute", "acute", "risk", "normal"),
      enabled = c(TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    Mortality = data.frame(
      indicator_id = c("mort_a1", "mort_a2", "mort_a3", "mort_a4"),
      indicator_name = c("Crude Death Rate", "Under-5 Death Rate",
                         "Death Rate by Cause", "Proportional Mortality"),
      calculation_type = c("rate", "rate", "rate", "proportion"),
      numerator = c("total_deaths", "under5_deaths",
                    "deaths_by_cause", "deaths_by_cause"),
      denominator = c("person_days", "under5_person_days",
                      "person_days", "total_deaths"),
      disaggregation = c("admin1|admin2", "admin1|admin2",
                         "admin1|admin2|cause", "admin1|admin2|cause"),
      classification = c("mortality", "child_mortality", "cause_specific", "cause_specific"),
      enabled = c(TRUE, TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    )
  )

  if (sector %in% names(schemas)) {
    return(schemas[[sector]])
  } else {
    return(base_schema)
  }
}

#' @title Get Default Integrated Analysis Schema
#' @description Returns the default integrated analysis schema
#' @return A data frame representing the default integrated analysis schema
#' @noRd
iphra_get_default_integrated_analysis_schema <- function() {
  data.frame(
    framework_id = c("ia_1", "ia_2", "ia_3", "ia_4", "ia_5", "ia_6"),
    framework_element = c("Mortality Outcomes", "Nutrition Outcomes",
                          "Health Status", "Food Security Status",
                          "WASH Status", "Contributing Factors"),
    data_sources = c("Mortality survey data", "Anthropometry data",
                     "Health module data", "FSL module data",
                     "WASH module data", "All sector data"),
    key_indicators = c("CDR|U5DR", "GAM|SAM|Stunting",
                       "Morbidity|Healthcare Access", "FCS|HDDS|rCSI|LCS",
                       "Water|Sanitation|Hygiene", "All indicators"),
    integration_method = c("triangulation", "triangulation",
                           "triangulation", "triangulation",
                           "triangulation", "framework_synthesis"),
    weight = c(25, 25, 15, 15, 10, 10),
    classification_thresholds = c("CDR>=2|U5DR>=4:Critical",
                                   "GAM>=15:Critical|GAM>=10:Serious",
                                   "Morbidity>=30:High",
                                   "FCS_Poor>=20:Crisis",
                                   "OD>=25:High",
                                   "Multiple:Compound"),
    enabled = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

# ────────────────────────────────────────────────────────────────────────────────
# Schema Access Functions
# ────────────────────────────────────────────────────────────────────────────────

#' @title Get Schema
#' @description Retrieve a schema based on type and sector
#' @param schema_type Character: "data", "quality", "analysis", or "integrated_analysis"
#' @param sector Character: Sector name (not used for integrated_analysis)
#' @param session Shiny session object for accessing custom schemas
#' @return A data frame representing the requested schema
#' @export
iphra_get_schema <- function(schema_type, sector = NULL,
                              session = shiny::getDefaultReactiveDomain()) {
  # Validate schema type
  if (!schema_type %in% IPHRA_SCHEMA_TYPES) {
    iphra_warning(
      paste("Invalid schema type:", schema_type),
      origin = "iphra_get_schema"
    )
    return(NULL)
  }

  # Check for custom schema in session first
  if (!is.null(session)) {
    custom_schema <- iphra_get_custom_schema(schema_type, sector, session)
    if (!is.null(custom_schema)) {
      return(custom_schema)
    }
  }

  # Return default schema
  switch(schema_type,
         "data" = iphra_get_default_data_schema(sector),
         "quality" = iphra_get_default_quality_schema(sector),
         "analysis" = iphra_get_default_analysis_schema(sector),
         "integrated_analysis" = iphra_get_default_integrated_analysis_schema(),
         NULL)
}

#' @title Get Custom Schema
#' @description Retrieve a custom schema from session storage
#' @param schema_type Character: Schema type
#' @param sector Character: Sector name
#' @param session Shiny session object
#' @return Custom schema data frame or NULL if not found
#' @noRd
iphra_get_custom_schema <- function(schema_type, sector = NULL,
                                     session = shiny::getDefaultReactiveDomain())
{
  if (is.null(session) || is.null(session$userData$schemas)) {
    return(NULL)
  }

  schema_key <- if (schema_type == "integrated_analysis") {
    "integrated_analysis"
  } else {
    paste(schema_type, sector, sep = "_")
  }

  active_schema <- session$userData$schemas$active[[schema_key]]
  if (!is.null(active_schema) && active_schema != "default") {
    return(session$userData$schemas$custom[[schema_key]][[active_schema]])
  }

  return(NULL)
}

#' @title Save Custom Schema
#' @description Save a custom schema to session storage
#' @param schema_type Character: Schema type
#' @param sector Character: Sector name (NULL for integrated_analysis)
#' @param schema_name Character: Name for the custom schema
#' @param schema_data Data frame: The schema to save
#' @param session Shiny session object
#' @return Invisibly returns TRUE on success
#' @export
iphra_save_custom_schema <- function(schema_type, sector = NULL, schema_name,
                                      schema_data,
                                      session = shiny::getDefaultReactiveDomain())
{
  if (is.null(session)) {
    iphra_warning("No session available for saving schema", origin = "iphra_save_custom_schema")
    return(invisible(FALSE))
  }

  # Initialize schemas storage if needed
  if (is.null(session$userData$schemas)) {
    session$userData$schemas <- shiny::reactiveValues(
      custom = list(),
      active = list()
    )
  }

  schema_key <- if (schema_type == "integrated_analysis") {
    "integrated_analysis"
  } else {
    paste(schema_type, sector, sep = "_")
  }

  # Initialize nested list if needed
  if (is.null(session$userData$schemas$custom[[schema_key]])) {
    session$userData$schemas$custom[[schema_key]] <- list()
  }

  # Save the schema
  session$userData$schemas$custom[[schema_key]][[schema_name]] <- schema_data

  iphra_message(
    paste("Custom schema saved:", schema_name, "for", schema_key),
    origin = "iphra_save_custom_schema"
  )

  return(invisible(TRUE))
}

#' @title Set Active Schema
#' @description Set which schema (default or custom) is active
#' @param schema_type Character: Schema type
#' @param sector Character: Sector name
#' @param schema_name Character: Schema name ("default" or custom name)
#' @param session Shiny session object
#' @return Invisibly returns TRUE on success
#' @export
iphra_set_active_schema <- function(schema_type, sector = NULL, schema_name,
                                     session = shiny::getDefaultReactiveDomain())
{
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  if (is.null(session$userData$schemas)) {
    session$userData$schemas <- shiny::reactiveValues(
      custom = list(),
      active = list()
    )
  }

  schema_key <- if (schema_type == "integrated_analysis") {
    "integrated_analysis"
  } else {
    paste(schema_type, sector, sep = "_")
  }

  session$userData$schemas$active[[schema_key]] <- schema_name

  iphra_message(
    paste("Active schema set to:", schema_name, "for", schema_key),
    origin = "iphra_set_active_schema"
  )

  return(invisible(TRUE))
}

#' @title List Available Schemas
#' @description List all available schemas (default + custom) for a type/sector
#' @param schema_type Character: Schema type
#' @param sector Character: Sector name
#' @param session Shiny session object
#' @return Character vector of available schema names
#' @export
iphra_list_schemas <- function(schema_type, sector = NULL,
                                session = shiny::getDefaultReactiveDomain()) {
  schemas <- c("default")

  if (!is.null(session) && !is.null(session$userData$schemas$custom)) {
    schema_key <- if (schema_type == "integrated_analysis") {
      "integrated_analysis"
    } else {
      paste(schema_type, sector, sep = "_")
    }

    custom_names <- names(session$userData$schemas$custom[[schema_key]])
    if (length(custom_names) > 0) {
      schemas <- c(schemas, custom_names)
    }
  }

  return(schemas)
}

#' @title Validate Schema
#' @description Validate a schema structure
#' @param schema_data Data frame: The schema to validate
#' @param schema_type Character: Schema type for validation rules
#' @return List with valid (logical) and messages (character vector)
#' @export
iphra_validate_schema_structure <- function(schema_data, schema_type) {
  messages <- character()
  valid <- TRUE

  # Check it's a data frame
  if (!is.data.frame(schema_data)) {
    return(list(valid = FALSE, messages = "Schema must be a data frame"))
  }

  # Check for minimum rows
  if (nrow(schema_data) == 0) {
    messages <- c(messages, "Schema has no rows")
    valid <- FALSE
  }

  # Schema-specific column requirements
  required_cols <- switch(schema_type,
                           "data" = c("variable_name", "data_type", "required"),
                           "quality" = c("check_id", "check_name", "check_type", "enabled"),
                           "analysis" = c("indicator_id", "indicator_name", "calculation_type", "enabled"),
                           "integrated_analysis" = c("framework_id", "framework_element", "enabled"),
                           character())

  missing_cols <- setdiff(required_cols, names(schema_data))
  if (length(missing_cols) > 0) {
    messages <- c(messages, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    valid <- FALSE
  }

  return(list(valid = valid, messages = messages))
}

#' @title Reset Schema to Default
#' @description Reset a schema to its default values
#' @param schema_type Character: Schema type
#' @param sector Character: Sector name
#' @param session Shiny session object
#' @return The default schema data frame
#' @export
iphra_reset_schema_to_default <- function(schema_type, sector = NULL,
                                           session = shiny::getDefaultReactiveDomain())
{
  # Set active schema to default
  iphra_set_active_schema(schema_type, sector, "default", session)

  # Return the default schema
  default_schema <- switch(schema_type,
                            "data" = iphra_get_default_data_schema(sector),
                            "quality" = iphra_get_default_quality_schema(sector),
                            "analysis" = iphra_get_default_analysis_schema(sector),
                            "integrated_analysis" = iphra_get_default_integrated_analysis_schema(),
                            NULL)

  iphra_message(
    paste("Schema reset to default for:", schema_type,
          if (!is.null(sector)) paste("-", sector) else ""),
    origin = "iphra_reset_schema_to_default"
  )

  return(default_schema)
}

# ────────────────────────────────────────────────────────────────────────────────
# Schema Initialization
# ────────────────────────────────────────────────────────────────────────────────

#' @title Initialize Schema Storage
#' @description Initialize the schema storage structure in session
#' @param session Shiny session object
#' @return Invisibly returns the schemas reactiveValues
#' @export
iphra_init_schemas <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return(invisible(NULL))
  }

  if (is.null(session$userData$schemas)) {
    session$userData$schemas <- shiny::reactiveValues(
      custom = list(),
      active = list()
    )
    iphra_message("Schema storage initialized", origin = "iphra_init_schemas")
  }

  return(invisible(session$userData$schemas))
}
