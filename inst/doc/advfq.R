## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(dplyr)
library(admiral)
library(pharmaversesdtm)
library(admiraldev)
library(admiralophtha)

## -----------------------------------------------------------------------------
data("admiral_adsl")
data("qs_ophtha")
adsl <- admiral_adsl
qs <- qs_ophtha

qs <- qs %>% filter(QSTESTCD %in% c("VFQ1", "VFQ2", "VFQ3", "VFQ4"))

## ----eval=FALSE---------------------------------------------------------------
#  param_lookup <- tibble::tribble(
#    ~QSTESTCD, ~PARAMCD, ~PARAM, ~PARCAT1, ~PARCAT2,
#    "VFQ1", "VFQ1", "Overall Health", "NEI VFQ-25", "Original Response",
#    "VFQ2", "VFQ2", "Eyesight in Both Eyes", "NEI VFQ-25", "Original Response",
#    "VFQ3", "VFQ3", "Worry About Eyesight", "NEI VFQ-25", "Original Response",
#    "VFQ4", "VFQ4", "Pain in and Around Eyes", "NEI VFQ-25", "Original Response",
#    "QR01", "QR01", "Recoded Item - 01", "NEI VFQ-25", "General 01",
#    "QR02", "QR02", "Recoded Item - 02", "NEI VFQ-25", "General 01",
#    "QR03", "QR03", "Recoded Item - 03", "NEI VFQ-25", "General 02",
#    "QR04", "QR04", "Recoded Item - 04", "NEI VFQ-25", "General 02",
#    "QSG01", "QSG01", "General Score 01", "NEI VFQ-25", "Averaged Result",
#    "QSG02", "QSG02", "General Score 02", "NEI VFQ-25", "Averaged Result",
#    "QBCSCORE", "QBCSCORE", "Composite Score", "NEI VFQ-25", "Averaged Result"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)
#  
#  advfq <- derive_vars_merged(
#    qs,
#    dataset_add = adsl,
#    new_vars = adsl_vars,
#    by_vars = get_admiral_option("subject_keys")
#  )

## ----eval=FALSE---------------------------------------------------------------
#  advfq <- advfq %>%
#    ## Add PARAMCD only - add PARAM etc later ----
#    derive_vars_merged_lookup(
#      dataset_add = param_lookup,
#      new_vars = exprs(PARAMCD),
#      by_vars = exprs(QSTESTCD)
#    ) %>%
#    ## Calculate AVAL and AVALC ----
#    mutate(
#      AVAL = QSSTRESN,
#      AVALC = QSORRES
#    )

## ----eval=FALSE---------------------------------------------------------------
#  ## QR01 Recoded Item 01
#  # set to 100 if [advfq.AVAL] = 1
#  # else set to 75 if [advfq.AVAL] = 2
#  # else set to 50 if [advfq.AVAL] = 3
#  # else set to 25 if [advfq.AVAL] = 4
#  # else set to 0 if [advfq.AVAL] = 5
#  advfq <- advfq %>%
#    derive_summary_records(
#      dataset_add = advfq,
#      by_vars = c(
#        get_admiral_option("subject_keys"),
#        exprs(!!!adsl_vars, PARAMCD, VISITNUM, VISIT)
#      ),
#      filter_add = QSTESTCD == "VFQ1" & !is.na(AVAL),
#      set_values_to = exprs(
#        AVAL = identity(AVAL),
#        PARAMCD = "QR01"
#      )
#    ) %>%
#    mutate(AVAL = ifelse(PARAMCD == "QR01",
#      case_when(
#        AVAL == 1 ~ 100,
#        AVAL == 2 ~ 75,
#        AVAL == 3 ~ 50,
#        AVAL == 4 ~ 25,
#        AVAL >= 5 ~ 0
#      ),
#      AVAL
#    ))

## ----eval=FALSE---------------------------------------------------------------
#  ## Derive a new record as a summary record  ----
#  ## QSG01 General Score 01
#  # Average of QR01 and QR02 records
#  advfq <- advfq %>%
#    derive_summary_records(
#      dataset_add = advfq,
#      by_vars = c(
#        get_admiral_option("subject_keys"),
#        exprs(!!!adsl_vars, VISITNUM, VISIT, ADT, ADY)
#      ),
#      filter_add = PARAMCD %in% c("QR01", "QR02") & !is.na(AVAL),
#      set_values_to = exprs(
#        AVAL = mean(AVAL),
#        PARAMCD = "QSG01"
#      )
#    )

## ----eval=FALSE---------------------------------------------------------------
#  ## ANL01FL: Flag last result within an AVISIT for post-baseline records ----
#  advfq <- advfq %>%
#    restrict_derivation(
#      derivation = derive_var_extreme_flag,
#      args = params(
#        new_var = ANL01FL,
#        by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, AVISIT)),
#        order = exprs(ADT, AVAL),
#        mode = "last"
#      ),
#      filter = !is.na(AVISITN) & ONTRTFL == "Y"
#    )

## ----eval=FALSE---------------------------------------------------------------
#  ## Get ASEQ and PARAM  ----
#  advfq <- advfq %>%
#    # Calculate ASEQ
#    derive_var_obs_number(
#      new_var = ASEQ,
#      by_vars = get_admiral_option("subject_keys"),
#      order = exprs(PARAMCD, ADT, AVISITN, VISITNUM),
#      check_type = "error"
#    ) %>%
#    # Derive PARAM
#    derive_vars_merged(dataset_add = select(param_lookup, -QSTESTCD), by_vars = exprs(PARAMCD))

## ----eval=FALSE---------------------------------------------------------------
#  # Add all ADSL variables
#  advfq <- advfq %>%
#    derive_vars_merged(
#      dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
#      by_vars = get_admiral_option("subject_keys")
#    )

