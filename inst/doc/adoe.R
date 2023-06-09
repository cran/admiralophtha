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

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~OETESTCD, ~AFEYE, ~PARAMCD, ~PARAM, ~PARAMN,
  "CSUBTH", "Study Eye", "SCSUBTH", "Study Eye Center Subfield Thickness (um)", 1,
  "CSUBTH", "Fellow Eye", "FCSUBTH", "Fellow Eye Center Subfield Thickness (um)", 2,
  "DRSSR", "Study Eye", "SDRSSR", "Study Eye Diabetic Retinopathy Severity", 3,
  "DRSSR", "Fellow Eye", "FDRSSR", "Fellow Eye Diabetic Retinopathy Severity", 4,
)

## -----------------------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P, STUDYEYE)

adoe <- oe %>%
  filter(
    OETESTCD %in% c("CSUBTH", "DRSSR")
  ) %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = exprs(STUDYID, USUBJID)
  )

