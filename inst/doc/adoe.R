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
library(stringr)

## -----------------------------------------------------------------------------
data("oe_ophtha")
data("admiral_adsl")

# Add STUDYEYE to ADSL to simulate an ophtha dataset
adsl <- admiral_adsl %>%
  as.data.frame() %>%
  mutate(STUDYEYE = sample(c("LEFT", "RIGHT"), n(), replace = TRUE)) %>%
  convert_blanks_to_na()

oe <- convert_blanks_to_na(oe_ophtha)

# Lookup table

# nolint start
param_lookup <- tibble::tribble(
  ~OETESTCD, ~OECAT, ~OESCAT, ~AFEYE, ~PARAMCD, ~PARAM, ~PARAMN,
  "CSUBTH", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Study Eye", "SCSUBTH", "Study Eye Center Subfield Thickness (um)", 1,
  "CSUBTH", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Fellow Eye", "FCSUBTH", "Fellow Eye Center Subfield Thickness (um)", 2,
  "DRSSR", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Study Eye", "SDRSSR", "Study Eye Diabetic Retinopathy Severity", 3,
  "DRSSR", "OPHTHALMIC ASSESSMENTS", "SD-OCT CST SINGLE FORM", "Fellow Eye", "FDRSSR", "Fellow Eye Diabetic Retinopathy Severity", 4,
  "IOP", "INTRAOCULAR PRESSURE", NA_character_, "Study Eye", "SIOP", "Study Eye IOP (mmHg)", 5,
  "IOP", "INTRAOCULAR PRESSURE", NA_character_, "Fellow Eye", "FIOP", "Fellow Eye IOP (mmHg)", 6
)
# nolint end

## -----------------------------------------------------------------------------
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P, STUDYEYE)

adoe <- oe %>%
  filter(
    OETESTCD %in% c("CSUBTH", "DRSSR", "IOP")
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

## -----------------------------------------------------------------------------
adoe <- adoe %>%
  # Add PARAM, PARAMCD from lookup table
  derive_vars_merged(
    dataset_add = param_lookup,
    new_vars = exprs(PARAM, PARAMCD),
    by_vars = exprs(OETESTCD, AFEYE)
  ) %>%
  # Derive visit, baseline flag info and BASETYPE
  mutate(
    ATPTN = OETPTNUM,
    ATPT = OETPT,
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN") ~ "Screening",
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = round(VISITNUM, 0),
    ABLFL = if_else(AVISIT == "Baseline", "Y", NA_character_)
    # In actual studies, ABLFL derivation will likely be more nuanced
    # and leverage derive_var_extreme_flag()
  )

## -----------------------------------------------------------------------------
adoe <- adoe %>%
  # Add derived parameter for difference between pre and post dose IOP
  call_derivation(
    derivation = derive_param_computed,
    by_vars = c(get_admiral_option("subject_keys"), !!adsl_vars, exprs(AVISIT, AVISITN)),
    variable_params = list(
      # Study eye
      params(
        parameters = exprs(
          SIOPPRE = PARAMCD == "SIOP" & ATPT == "PRE-DOSE",
          SIOPPOST = PARAMCD == "SIOP" & ATPT == "POST-DOSE"
        ),
        set_values_to = exprs(
          PARAMCD = "SIOPCHG",
          PARAM = "Study Eye IOP Pre to Post Dose Diff (mmHg)",
          PARAMN = 9,
          AVAL = AVAL.SIOPPOST - AVAL.SIOPPRE,
          AVALC = as.character(AVAL)
        )
      ),
      # Fellow eye
      params(
        parameters = exprs(
          FIOPPRE = PARAMCD == "FIOP" & ATPT == "PRE-DOSE",
          FIOPPOST = PARAMCD == "FIOP" & ATPT == "POST-DOSE"
        ),
        set_values_to = exprs(
          PARAMCD = "FIOPCHG",
          PARAM = "Fellow Eye IOP Pre to Post Dose Diff (mmHg)",
          PARAMN = 10,
          AVAL = AVAL.FIOPPOST - AVAL.FIOPPRE,
          AVALC = as.character(AVAL)
        )
      )
    )
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  adoe %>% arrange(USUBJID, AVISIT) %>% select(USUBJID, AVISIT, PARAMCD, AVAL),
  display_vars = exprs(USUBJID, PARAMCD, AVISIT, AVAL),
  filter = str_detect(PARAMCD, "IOP") & USUBJID == "01-701-1028" &
    AVISIT %in% c("Baseline", "Week 4")
)

## -----------------------------------------------------------------------------
adoe <- adoe %>%
  # Calculate BASE (do not derive for IOP change params)
  restrict_derivation(
    derivation = derive_var_base,
    args = params(
      by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, ATPT)),
      source_var = AVAL,
      new_var = BASE
    ),
    filter = !PARAMCD %in% c("SIOPCHG", "FIOPCHG")
  )

