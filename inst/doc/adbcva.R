## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
link <- function(text, url) {
  return(
    paste0(
      "[", text, "]",
      "(", url, ")"
    )
  )
}
dyn_link <- function(text,
                     base_url,
                     relative_url = "",
                     # Change to TRUE when admiral adopts multiversion docs
                     is_multiversion = FALSE,
                     multiversion_default_ref = "main") {
  url <- paste(base_url, relative_url, sep = "/")
  if (is_multiversion) {
    url <- paste(
      base_url,
      Sys.getenv("BRANCH_NAME", multiversion_default_ref),
      relative_url,
      sep = "/"
    )
  }
  return(link(text, url))
}
# Other variables
admiral_homepage <- "https://pharmaverse.github.io/admiral"
library(admiraldev)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(dplyr)
library(admiral)
library(admiral.test)
library(admiraldev)
library(admiralophtha)

## -----------------------------------------------------------------------------
data("admiral_oe")
data("admiral_adsl")

# Add STUDYEYE to ADSL to simulate an ophtha dataset
adsl <- admiral_adsl %>%
  as.data.frame() %>%
  mutate(STUDYEYE = sample(c("LEFT", "RIGHT"), n(), replace = TRUE)) %>%
  convert_blanks_to_na()

oe <- convert_blanks_to_na(admiral_oe) %>%
  ungroup()

# ---- Lookup table ----
param_lookup <- tibble::tribble(
  ~OETESTCD, ~OELAT, ~STUDYEYE, ~PARAMCD, ~PARAM, ~PARAMN,
  "VACSCORE", "RIGHT", "RIGHT", "SBCVA", "Study Eye Visual Acuity Score", 1,
  "VACSCORE", "LEFT", "LEFT", "SBCVA", "Study Eye Visual Acuity Score", 1,
  "VACSCORE", "RIGHT", "LEFT", "FBCVA", "Fellow Eye Visual Acuity Score", 2,
  "VACSCORE", "LEFT", "RIGHT", "FBCVA", "Fellow Eye Visual Acuity Score", 2
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
    by_vars = exprs(STUDYID, USUBJID)
  )

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  mutate(
    AVAL = OESTRESN,
    AVALU = "letters",
    DTYPE = NA_character_
  )

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  derive_vars_merged(
    dataset_add = param_lookup,
    new_vars = exprs(PARAM, PARAMCD),
    by_vars = exprs(OETESTCD, OELAT, STUDYEYE),
    filter_add = PARAMCD %in% c("SBCVA", "FBCVA")
  )

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  derive_param_computed(
    by_vars = c(exprs(STUDYID, USUBJID, VISIT, VISITNUM, OEDY, OEDTC), adsl_vars),
    parameters = c("SBCVA"),
    analysis_value = convert_etdrs_to_logmar(AVAL.SBCVA),
    set_values_to = exprs(
      PARAMCD = "SBCVALOG",
      PARAM = "Study Eye Visual Acuity LogMAR Score",
      DTYPE = "DERIVED",
      AVALU = "LogMAR"
    )
  ) %>%
  derive_param_computed(
    by_vars = c(exprs(STUDYID, USUBJID, VISIT, OEDY, OEDTC), adsl_vars),
    parameters = c("FBCVA"),
    analysis_value = convert_etdrs_to_logmar(AVAL.FBCVA),
    set_values_to = exprs(
      PARAMCD = "FBCVALOG",
      PARAM = "Fellow Eye Visual Acuity LogMAR Score",
      DTYPE = "DERIVED",
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
avalcat_lookup <- tibble::tribble(
  ~PARAMCD, ~AVALCA1N, ~AVALCAT1,
  "SBCVA", 1000, "< 20/800",
  "SBCVA", 800, "20/800",
  "SBCVA", 640, "20/640",
  "SBCVA", 500, "20/500",
  "SBCVA", 400, "20/400",
  "SBCVA", 320, "20/320",
  "SBCVA", 250, "20/250",
  "SBCVA", 200, "20/200",
  "SBCVA", 160, "20/160",
  "SBCVA", 125, "20/125",
  "SBCVA", 100, "20/100",
  "SBCVA", 80, "20/80",
  "SBCVA", 63, "20/63",
  "SBCVA", 50, "20/50",
  "SBCVA", 40, "20/40",
  "SBCVA", 32, "20/32",
  "SBCVA", 25, "20/25",
  "SBCVA", 20, "20/20",
  "SBCVA", 16, "20/16",
  "SBCVA", 12, "20/12",
  "SBCVA", 1, "> 20/12",
)

avalcat_lookup <- avalcat_lookup %>%
  mutate(PARAMCD = "FBCVA") %>%
  rbind(avalcat_lookup)

format_avalcat1n <- function(param, aval) {
  case_when(
    param %in% c("SBCVA", "FBCVA") & aval >= 0 & aval <= 3 ~ 1000,
    param %in% c("SBCVA", "FBCVA") & aval >= 4 & aval <= 8 ~ 800,
    param %in% c("SBCVA", "FBCVA") & aval >= 9 & aval <= 13 ~ 640,
    param %in% c("SBCVA", "FBCVA") & aval >= 14 & aval <= 18 ~ 500,
    param %in% c("SBCVA", "FBCVA") & aval >= 19 & aval <= 23 ~ 400,
    param %in% c("SBCVA", "FBCVA") & aval >= 24 & aval <= 28 ~ 320,
    param %in% c("SBCVA", "FBCVA") & aval >= 29 & aval <= 33 ~ 250,
    param %in% c("SBCVA", "FBCVA") & aval >= 34 & aval <= 38 ~ 200,
    param %in% c("SBCVA", "FBCVA") & aval >= 39 & aval <= 43 ~ 160,
    param %in% c("SBCVA", "FBCVA") & aval >= 44 & aval <= 48 ~ 125,
    param %in% c("SBCVA", "FBCVA") & aval >= 49 & aval <= 53 ~ 100,
    param %in% c("SBCVA", "FBCVA") & aval >= 54 & aval <= 58 ~ 80,
    param %in% c("SBCVA", "FBCVA") & aval >= 59 & aval <= 63 ~ 63,
    param %in% c("SBCVA", "FBCVA") & aval >= 64 & aval <= 68 ~ 50,
    param %in% c("SBCVA", "FBCVA") & aval >= 69 & aval <= 73 ~ 40,
    param %in% c("SBCVA", "FBCVA") & aval >= 74 & aval <= 78 ~ 32,
    param %in% c("SBCVA", "FBCVA") & aval >= 79 & aval <= 83 ~ 25,
    param %in% c("SBCVA", "FBCVA") & aval >= 84 & aval <= 88 ~ 20,
    param %in% c("SBCVA", "FBCVA") & aval >= 89 & aval <= 93 ~ 16,
    param %in% c("SBCVA", "FBCVA") & aval >= 94 & aval <= 97 ~ 12,
    param %in% c("SBCVA", "FBCVA") & aval >= 98 ~ 1
  )
}

adbcva <- adbcva %>%
  mutate(AVALCA1N = format_avalcat1n(param = PARAMCD, aval = AVAL)) %>%
  derive_vars_merged(
    avalcat_lookup,
    by = exprs(PARAMCD, AVALCA1N)
  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adbcva %>% filter(USUBJID == "01-701-1015"),
  display_vars = exprs(
    USUBJID, PARAMCD, AVAL, AVALCAT1, AVALCA1N
  )
)

## -----------------------------------------------------------------------------
adbcva <- adbcva %>% derive_var_bcvacritxfl(
  paramcds = c("SBCVA", "FBCVA"),
  bcva_ranges = list(c(5, 10)),
  bcva_uplims = list(25, -5),
  bcva_lowlims = list(15, -10)
)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
dataset_vignette(
  adbcva %>% filter(USUBJID == "01-701-1015"),
  display_vars = exprs(
    USUBJID, PARAMCD, AVAL, CHG, CRIT1, CRIT1FL, CRIT2, CRIT2FL, CRIT3, CRIT3FL, CRIT4, CRIT4FL, CRIT5, CRIT5FL
  )
)

## -----------------------------------------------------------------------------
adbcva <- adbcva %>%
  derive_var_bcvacritxfl(
    paramcds = c("SBCVA", "FBCVA"),
    bcva_ranges = list(c(5, 10)),
    critxfl_index = 10
  ) %>%
  derive_var_bcvacritxfl(
    paramcds = c("SBCVA", "FBCVA"),
    bcva_uplims = list(25, -5),
    critxfl_index = 20
  ) %>%
  derive_var_bcvacritxfl(
    paramcds = c("SBCVA", "FBCVA"),
    bcva_lowlims = list(15, -10),
    critxfl_index = 30
  )

