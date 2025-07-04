## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(admiraldev)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(dplyr)
library(admiral)
library(pharmaversesdtm)
library(admiraldev)
library(admiralophtha)

## -----------------------------------------------------------------------------
data("oe_ophtha")
data("admiral_adsl")

# Add STUDYEYE to ADSL to simulate an ophtha dataset
adsl <- admiral_adsl %>%
  as.data.frame() %>%
  mutate(STUDYEYE = sample(c("LEFT", "RIGHT"), n(), replace = TRUE)) %>%
  convert_blanks_to_na()

oe <- convert_blanks_to_na(oe_ophtha) %>%
  ungroup()

# Lookup table
param_lookup <- tibble::tribble(
  ~OETESTCD, ~OECAT, ~OESCAT, ~AFEYE, ~PARAMCD, ~PARAM, ~PARAMN,
  "VACSCORE", "BEST CORRECTED VISUAL ACUITY", "OVERALL EVALUATION", "Study Eye", "SBCVA", "Study Eye Visual Acuity Score (letters)", 1, # nolint
  "VACSCORE", "BEST CORRECTED VISUAL ACUITY", "OVERALL EVALUATION", "Fellow Eye", "FBCVA", "Fellow Eye Visual Acuity Score (letters)", 2, # nolint
)

# SBCVA and FBCVA definition list
definition_bcva <- exprs(
  ~PARAMCD, ~condition, ~AVALCA1N, ~AVALCAT1,
  "SBCVA", AVAL >= 0 & AVAL <= 3, 1000, "< 20/800",
  "FBCVA", AVAL >= 0 & AVAL <= 3, 1000, "< 20/800",
  "SBCVA", AVAL >= 4 & AVAL <= 8, 800, "20/800",
  "FBCVA", AVAL >= 4 & AVAL <= 8, 800, "20/800",
  "SBCVA", AVAL >= 9 & AVAL <= 13, 640, "20/640",
  "FBCVA", AVAL >= 9 & AVAL <= 13, 640, "20/640",
  "SBCVA", AVAL >= 14 & AVAL <= 18, 500, "20/500",
  "FBCVA", AVAL >= 14 & AVAL <= 18, 500, "20/500",
  "SBCVA", AVAL >= 19 & AVAL <= 23, 400, "20/400",
  "FBCVA", AVAL >= 19 & AVAL <= 23, 400, "20/400",
  "SBCVA", AVAL >= 24 & AVAL <= 28, 320, "20/320",
  "FBCVA", AVAL >= 24 & AVAL <= 28, 320, "20/320",
  "SBCVA", AVAL >= 29 & AVAL <= 33, 250, "20/250",
  "FBCVA", AVAL >= 29 & AVAL <= 33, 250, "20/250",
  "SBCVA", AVAL >= 34 & AVAL <= 38, 200, "20/200",
  "FBCVA", AVAL >= 34 & AVAL <= 38, 200, "20/200",
  "SBCVA", AVAL >= 39 & AVAL <= 43, 160, "20/160",
  "FBCVA", AVAL >= 39 & AVAL <= 43, 160, "20/160",
  "SBCVA", AVAL >= 44 & AVAL <= 48, 125, "20/125",
  "FBCVA", AVAL >= 44 & AVAL <= 48, 125, "20/125",
  "SBCVA", AVAL >= 49 & AVAL <= 53, 100, "20/100",
  "FBCVA", AVAL >= 49 & AVAL <= 53, 100, "20/100",
  "SBCVA", AVAL >= 54 & AVAL <= 58, 80, "20/80",
  "FBCVA", AVAL >= 54 & AVAL <= 58, 80, "20/80",
  "SBCVA", AVAL >= 59 & AVAL <= 63, 63, "20/63",
  "FBCVA", AVAL >= 59 & AVAL <= 63, 63, "20/63",
  "SBCVA", AVAL >= 64 & AVAL <= 68, 50, "20/50",
  "FBCVA", AVAL >= 64 & AVAL <= 68, 50, "20/50",
  "SBCVA", AVAL >= 69 & AVAL <= 73, 40, "20/40",
  "FBCVA", AVAL >= 69 & AVAL <= 73, 40, "20/40",
  "SBCVA", AVAL >= 74 & AVAL <= 78, 32, "20/32",
  "FBCVA", AVAL >= 74 & AVAL <= 78, 32, "20/32",
  "SBCVA", AVAL >= 79 & AVAL <= 83, 25, "20/25",
  "FBCVA", AVAL >= 79 & AVAL <= 83, 25, "20/25",
  "SBCVA", AVAL >= 84 & AVAL <= 88, 20, "20/20",
  "FBCVA", AVAL >= 84 & AVAL <= 88, 20, "20/20",
  "SBCVA", AVAL >= 89 & AVAL <= 93, 16, "20/16",
  "FBCVA", AVAL >= 89 & AVAL <= 93, 16, "20/16",
  "SBCVA", AVAL >= 94 & AVAL <= 97, 12, "20/12",
  "FBCVA", AVAL >= 94 & AVAL <= 97, 12, "20/12",
  "SBCVA", AVAL >= 98, 1, "> 20/12",
  "FBCVA", AVAL >= 98, 1, "> 20/12"
)

## -----------------------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P, STUDYEYE)

adbcva <- oe %>%
  filter(
    OETESTCD %in% c("VACSCORE")
  ) %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  )

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  mutate(
    AVAL = OESTRESN,
    AVALU = "letters",
    DTYPE = NA_character_
  ) %>%
  derive_var_afeye(loc_var = OELOC, lat_var = OELAT)

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  derive_vars_merged(
    dataset_add = param_lookup,
    new_vars = exprs(PARAM, PARAMCD),
    by_vars = exprs(OETESTCD, AFEYE),
    filter_add = PARAMCD %in% c("SBCVA", "FBCVA")
  )

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  derive_param_computed(
    by_vars = c(
      get_admiral_option("subject_keys"),
      exprs(VISIT, VISITNUM, OEDY, OEDTC, AFEYE, !!!adsl_vars)
    ),
    parameters = c("SBCVA"),
    set_values_to = exprs(
      AVAL = convert_etdrs_to_logmar(AVAL.SBCVA),
      PARAMCD = "SBCVALOG",
      PARAM = "Study Eye Visual Acuity LogMAR Score",
      DTYPE = NA_character_,
      AVALU = "LogMAR"
    )
  ) %>%
  derive_param_computed(
    by_vars = c(
      get_admiral_option("subject_keys"),
      exprs(VISIT, VISITNUM, OEDY, OEDTC, AFEYE, !!!adsl_vars)
    ),
    parameters = c("FBCVA"),
    set_values_to = exprs(
      AVAL = convert_etdrs_to_logmar(AVAL.FBCVA),
      PARAMCD = "FBCVALOG",
      PARAM = "Fellow Eye Visual Acuity LogMAR Score",
      DTYPE = NA_character_,
      AVALU = "LogMAR"
    )
  ) %>%
  mutate(AVALC = as.character(AVAL)) %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = OEDTC,
    flag_imputation = "none"
  ) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  mutate(
    VISIT = ifelse(PARAMCD %in% c("SBCVALOG", "FBCVALOG"), NA_character_, VISIT),
    VISITNUM = ifelse(PARAMCD %in% c("SBCVALOG", "FBCVALOG"), NA, VISITNUM),
    OEDY = ifelse(PARAMCD %in% c("SBCVALOG", "FBCVALOG"), NA, OEDY),
    OEDTC = ifelse(PARAMCD %in% c("SBCVALOG", "FBCVALOG"), NA_character_, OEDTC)
  )

## -----------------------------------------------------------------------------
data("admiralophtha_adbcva")

adbcva <- admiralophtha_adbcva %>%
  select(-starts_with("CRIT"), -starts_with("AVALCA"))

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  derive_vars_cat(
    definition = definition_bcva,
    by_vars = exprs(PARAMCD)
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adbcva %>% filter(USUBJID == "01-701-1015"),
  display_vars = exprs(
    USUBJID, PARAMCD, AVAL, AVALCAT1, AVALCA1N
  )
)

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  restrict_derivation(
    derivation = call_derivation,
    filter = PARAMCD %in% c("SBCVA", "FBCVA"),
    args = params(
      derivation = derive_vars_crit_flag,
      variable_params = list(
        params(crit_nr = 1, condition = CHG >= 0 & CHG <= 5, description = "0 <= CHG <= 5"),
        params(crit_nr = 2, condition = CHG >= -5 & CHG <= -1, description = "-5 <= CHG <= -1"),
        params(crit_nr = 3, condition = CHG >= 10 & CHG <= 15, description = "10 <= CHG <= 15"),
        params(crit_nr = 4, condition = CHG <= -20, description = "CHG <= -20"),
        params(crit_nr = 5, condition = CHG <= 5, description = "CHG <= 5"),
        params(crit_nr = 6, condition = CHG <= 10, description = "CHG <= 10"),
        params(crit_nr = 7, condition = CHG >= -15, description = "CHG >= -15"),
        params(crit_nr = 8, condition = CHG >= 15, description = "CHG >= 15")
      ),
      values_yn = TRUE
    )
  ) %>%
  arrange(USUBJID, DOMAIN, PARAMCD)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adbcva %>%
    filter(USUBJID == "01-701-1015") %>%
    select(USUBJID, PARAMCD, AVAL, CHG, starts_with("CRIT"))
)

