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

# ---- Lookup table ----

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~OETESTCD, ~OECAT, ~OESCAT, ~AFEYE, ~PARAMCD, ~PARAM, ~PARAMN,
  "CSUBTH", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Study Eye", "SCSUBTH", "Study Eye Center Subfield Thickness (um)", 1, # nolint
  "CSUBTH", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Fellow Eye", "FCSUBTH", "Fellow Eye Center Subfield Thickness (um)", 2, # nolint
  "DRSSR", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Study Eye", "SDRSSR", "Study Eye Diabetic Retinopathy Severity", 3, # nolint
  "DRSSR", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Fellow Eye", "FDRSSR", "Fellow Eye Diabetic Retinopathy Severity", 4, # nolint
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
    by_vars = get_admiral_option("subject_keys")
  )

## -----------------------------------------------------------------------------
adoe <- adoe %>%
  # Calculate AVAL, AVALC, AVALU and DTYPE
  mutate(
    AVAL = OESTRESN,
    AVALC = OESTRESC,
    AVALU = OESTRESU,
    DTYPE = NA_character_
  ) %>%
  # Derive AFEYE needed for PARAMCD derivation
  derive_var_afeye(loc_var = OELOC, lat_var = OELAT, loc_vals = c("EYE", "RETINA"))

