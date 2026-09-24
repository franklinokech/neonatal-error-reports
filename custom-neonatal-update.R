
# Validate hospital and unique ids

if (exists("hosp_to_validate", envir = environment())) {
  if (!isTRUE(data_missing(hosp_to_validate))) {
    if (isTRUE(is_int(hosp_to_validate))) {
      hosp_to_validate = as.integer(hosp_to_validate)
      if (!isTRUE(data_missing(hosp_id))) {
        if (!isTRUE(hosp_id %in% hosp_to_validate)) {
          form__x2014cin = c(form__x2014cin, "Biodata")
          name__x2014cin = c(name__x2014cin, "hosp_id")
          msg__x2014cin = c (msg__x2014cin, "Invalid Hospital ID [hosp_id] (does not match hospital)!")
          sect__x2014cin = c(sect__x2014cin, "")
          entry__x2014cin = c(entry__x2014cin, hosp_id)
        }
      }
      if (!isTRUE(data_missing (id))) {
        if (isTRUE(nchar(id) > 6L)) {
          if (isTRUE(!as.integer(substr(id, 1L, 2L)) %in% hosp_to_validate)) {
            form__x2014cin = c(form__x2014cin, "Biodata")
            name__x2014cin = c(name__x2014cin, "id")
            msg__x2014cin = c(msg__x2014cin, "Invalid Unique ID [id] (does not match hospital)!")
            sect__x2014cin = c(sect__x2014cin, "")
            entry__x2014cin = c(entry__x2014cin, id)
          }
        } else {
          form__x2014cin = c(form__x2014cin, "Biodata")
          name__x2014cin = c(name__x2014cin, "id")
          msg__x2014cin = c(msg__x2014cin, "Invalid Unique ID [id] (too short)!")
          sect__x2014cin = c(sect__x2014cin, "")
          type__x2014cin = c(type__x2014cin, "")
          entry__x2014cin = c(entry__x2014cin, id)
        }
      }
    }
  }
}

# If Primary diagnosis checked, specify only one

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("pry_adm_diag", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_adm_diag_not_listed", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(clear_pry_adm_diag == 1)) {
  if (isTRUE(all(
    data_missing(pry_adm_diag)
    , other_adm_diag_not_listed != 1
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Baby's Admission Diagnoses")
      name__x2014cin = c(name__x2014cin, "pry_adm_diag")
      msg__x2014cin = c(msg__x2014cin, "Specify the Primary 'Admission diagnosis'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, pry_adm_diag)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# At least one admission diagnosis specified

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("adm_diag_1", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("adm_diag_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("adm_diag_3", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(clear_pry_adm_diag == 0)) {
  if (isTRUE(all(
    data_missing (adm_diag_1)
    , data_missing (adm_diag_2)
    , data_missing (adm_diag_3)
    , other_adm_diag_not_listed != 1
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Baby's Admission Diagnoses")
      name__x2014cin = c(name__x2014cin, "clear_pry_adm_diag")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one 'Admission diagnosis'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, clear_pry_adm_diag)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# At least one admission diagnosis specified

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_admission_diag_1", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_admission_diag_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_admission_diag_3", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_admission_diag_4", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_admission_diag_5", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_adm_diag_not_listed", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(other_admission_diag == 1)) {
  if (isTRUE(all(
    data_missing (other_admission_diag_1)
    , data_missing (other_admission_diag_2)
    , data_missing (other_admission_diag_3)
    , data_missing (other_admission_diag_4)
    , data_missing (other_admission_diag_5)
    , other_adm_diag_not_listed != 1
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Baby's Admission Diagnoses")
      name__x2014cin = c(name__x2014cin, "other_admission_diag")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one other 'Admission diagnosis'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, other_admission_diag)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# If other diagnosis checked, specify at least one

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("admisn_diag_not_listed", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(other_adm_diag_not_listed == 1)) {
  if (isTRUE(all(
    data_missing(admisn_diag_not_listed)
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Baby's Admission Diagnoses")
      name__x2014cin = c(name__x2014cin, "other_adm_diag_not_listed")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one other 'Admission diagnosis' not in the lookup list!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, other_adm_diag_not_listed)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# If other treatment checked, specify at least one

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_drugs_1", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_drugs_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_drugs_3", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_drugs_4", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_drugs_5", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_treatment_2", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(other_treatment == 1)) {
  if (isTRUE(all(
    data_missing(other_drugs_1)
    ,  data_missing(other_drugs_2)
    ,  data_missing(other_drugs_3)
    ,  data_missing(other_drugs_4)
    ,  data_missing(other_drugs_5)
    ,  data_missing(other_treatment_2)
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Drug Treatment")
      name__x2014cin = c(name__x2014cin, "other_treatment")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one 'other drug(s) prescribed'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, other_treatment)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# If other treatment checked, specify at least one

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("admisn_dx_not_listed", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(other_treatment_2 == 1)) {
  if (isTRUE(all(
    data_missing(admisn_dx_not_listed)
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Drug Treatment")
      name__x2014cin = c(name__x2014cin, "other_treatment_2")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one 'other drug(s) prescribed' not in the lookup list!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, other_treatment_2)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# # Specify the primary discharge diagnosis

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("primary_disch_diagnosis", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("any_other_disch_diag", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(dsc_dx1_primary == 1)) {
  if (isTRUE(all(
    data_missing(primary_disch_diagnosis)
    , any_other_disch_diag !=1
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Discharge Information")
      name__x2014cin = c(name__x2014cin, "dsc_dx1_primary")
      msg__x2014cin = c(msg__x2014cin, "Specify the 'Primary Discharge diagnosis'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, dsc_dx1_primary)
    }
  }
}
rm(.__update_date)
rm(.__is_update)


# Specify at least one discharge diagnosis

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("disch_diag_1", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("disch_diag_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("disch_diag_3", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("disch_diag_4", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("disch_diag_5", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("any_other_disch_diag", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(dsc_dx1_primary == 0)) {
  if (isTRUE(all(
    data_missing(disch_diag_1)
    , data_missing(disch_diag_2)
    , data_missing(disch_diag_3)
    , data_missing(disch_diag_4)
    , data_missing(disch_diag_5)
    , any_other_disch_diag != 1
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Discharge Information")
      name__x2014cin = c(name__x2014cin, "dsc_dx1_primary")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one 'Discharge diagnosis'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, dsc_dx1_primary)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# Specify at least one other discharge diagnosis
# Specify at least one discharge diagnosis

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_discharge_diag_1", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_discharge_diag_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_discharge_diag_3", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_discharge_diag_4", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_discharge_diag_5", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(other_discharge_diag == 1)) {
  if (isTRUE(all(
    data_missing(other_discharge_diag_1)
    , data_missing(other_discharge_diag_2)
    , data_missing(other_discharge_diag_3)
    , data_missing(other_discharge_diag_4)
    , data_missing(other_discharge_diag_5)
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Discharge Information")
      name__x2014cin = c(name__x2014cin, "other_discharge_diag")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one 'Other Discharge diagnosis'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, other_discharge_diag)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# If other diagnosis checked, specify at least one

.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_discharge_diag_unlisted", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_disch_diag_old", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(any_other_disch_diag == 1)) {
  if (isTRUE(all(
    data_missing (other_discharge_diag_unlisted),
    data_missing (other_disch_diag_old)
  ))) {
    if (!isTRUE(.__is_update & date_today <= .__update_date)) {
      form__x2014cin = c(form__x2014cin, "Discharge Information")
      name__x2014cin = c(name__x2014cin, "any_other_disch_diag")
      msg__x2014cin = c(msg__x2014cin, "Specify at least one other 'Discharge diagnosis' not in the lookup list!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, any_other_disch_diag)
    }
  }
}
rm(.__update_date)
rm(.__is_update)

# Check dates
.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_of_birth", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_adm", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_discharge", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (date_can_be_validated(date_adm) & date_can_be_validated(date_discharge) &
    as.Date(date_adm, "%Y-%m-%d") > as.Date(date_discharge, "%Y-%m-%d")){
  form__x2014cin = c(form__x2014cin, "Biodata")
  name__x2014cin = c(name__x2014cin, "date_adm")
  msg__x2014cin = c(msg__x2014cin, "'Admission Date' cannot be after 'Discharge/Death Date'")
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, date_adm)
}
if (isTRUE(date_can_be_validated(date_adm) && date_can_be_validated(date_of_birth))) {
  if (as.Date(date_adm, "%Y-%m-%d") < as.Date(date_of_birth, "%Y-%m-%d")) {
    form__x2014cin = c(form__x2014cin, "Biodata")
    name__x2014cin = c(name__x2014cin, "date_adm")
    msg__x2014cin = c(msg__x2014cin, "'Admission Date' cannot be before 'Date of Birth'")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_adm)
  }
}
if (isTRUE(date_can_be_validated(date_adm))) {
  if (isTRUE(as.Date(date_adm) > Sys.Date())) {
    form__x2014cin = c(form__x2014cin, "Biodata")
    name__x2014cin = c(name__x2014cin, "date_adm")
    msg__x2014cin = c(msg__x2014cin, "Admission Date' cannot be in the future!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_adm)
  }
}
if (isTRUE(date_can_be_validated(date_of_birth))) {
  if (isTRUE(as.Date(date_of_birth) > Sys.Date())) {
    form__x2014cin = c(form__x2014cin, "Biodata")
    name__x2014cin = c(name__x2014cin, "date_of_birth")
    msg__x2014cin = c(msg__x2014cin, "'Date of Birth' cannot be in the future!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, "")
  }
}
if (isTRUE(date_can_be_validated(date_discharge))) {
  if (isTRUE(as.Date(date_discharge) > Sys.Date())) {
    form__x2014cin = c(form__x2014cin, "Biodata")
    name__x2014cin = c(name__x2014cin, "date_discharge")
    msg__x2014cin = c(msg__x2014cin, "Discharge/Death Date' cannot be in the future!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_discharge)
  }
}
rm(.__update_date)
rm(.__is_update)

# Drugs Prescription dates: Penicillin, Genta
.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_prescribed", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_stopped", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_gentamycin_prescribed", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_gent_stopped", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_ampicillin_prescribed", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_amp_stopped", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_ceftriaxone_prescribe", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_ceftri_stopped", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("amikacin_date", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_amikacin_stopped", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("cefta_date", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("cefta_date_stopped", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("cpap_start", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("cpap_stop", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("blood_transf_pres_date", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_fluid_presc", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_feeds_prescribed", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_the_feeds_initiated", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("start_date_phototherapy", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("stop_date_phototherapy", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_discharge", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_discharge", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_discharge", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_of_birth", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)

# Penicillin
if (isTRUE(date_can_be_validated(date_prescribed))) {
  if (isTRUE((as.Date(date_prescribed) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_prescribed) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_stopped) && as.Date(date_prescribed) > as.Date(date_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_prescribed) > as.Date(date_discharge)) |
             (as.Date(date_prescribed) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_prescribed")
    msg__x2014cin = c(msg__x2014cin, "'Penicillin prescription Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_prescribed)
  }
}

if (isTRUE(date_can_be_validated(date_stopped))) {
  if (isTRUE((as.Date(date_stopped) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_stopped) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_prescribed) && as.Date(date_prescribed) > as.Date(date_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_stopped) > as.Date(date_discharge)) |
             (as.Date(date_stopped) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_stopped")
    msg__x2014cin = c(msg__x2014cin, "'Penicillin stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_stopped)
  }
}

# Genta
if (isTRUE(date_can_be_validated(date_gentamycin_prescribed))) {
  if (isTRUE((as.Date(date_gentamycin_prescribed) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_gentamycin_prescribed) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_gent_stopped) && as.Date(date_gentamycin_prescribed) > as.Date(date_gent_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_gentamycin_prescribed) > as.Date(date_discharge)) |
             (as.Date(date_gentamycin_prescribed) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_gentamycin_prescribed")
    msg__x2014cin = c(msg__x2014cin, "'Gentamicin prescription Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_gentamycin_prescribed)
  }
}

if (isTRUE(date_can_be_validated(date_gent_stopped))) {
  if (isTRUE((as.Date(date_gent_stopped) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_gent_stopped) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_gentamycin_prescribed) && as.Date(date_gentamycin_prescribed) > as.Date(date_gent_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_gent_stopped) > as.Date(date_discharge)) |
             (as.Date(date_gent_stopped) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_gent_stopped")
    msg__x2014cin = c(msg__x2014cin, "'Gentamicin stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_gent_stopped)
  }
}

# Ampicillin
if (isTRUE(date_can_be_validated(date_ampicillin_prescribed))) {
  if (isTRUE((as.Date(date_ampicillin_prescribed) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_ampicillin_prescribed) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_amp_stopped) && as.Date(date_ampicillin_prescribed) > as.Date(date_amp_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_ampicillin_prescribed) > as.Date(date_discharge)) |
             (as.Date(date_ampicillin_prescribed) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_ampicillin_prescribed")
    msg__x2014cin = c(msg__x2014cin, "'Ampicillin prescription Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_ampicillin_prescribed)
  }
}

if (isTRUE(date_can_be_validated(date_amp_stopped))) {
  if (isTRUE((as.Date(date_amp_stopped) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_amp_stopped) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_ampicillin_prescribed) && as.Date(date_ampicillin_prescribed) > as.Date(date_amp_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_amp_stopped) > as.Date(date_discharge)) |
             (as.Date(date_amp_stopped) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_amp_stopped")
    msg__x2014cin = c(msg__x2014cin, "'Ampicillin stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_amp_stopped)
  }
}

# Ceftriaxone
if (isTRUE(date_can_be_validated(date_ceftriaxone_prescribe))) {
  if (isTRUE((as.Date(date_ceftriaxone_prescribe) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_ceftriaxone_prescribe) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_ceftri_stopped) && as.Date(date_ceftriaxone_prescribe) > as.Date(date_ceftri_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_ceftriaxone_prescribe) > as.Date(date_discharge)) |
             (as.Date(date_ceftriaxone_prescribe) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_ceftriaxone_prescribe")
    msg__x2014cin = c(msg__x2014cin, "'ceftriaxone prescription Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_ceftriaxone_prescribe)
  }
}

if (isTRUE(date_can_be_validated(date_ceftri_stopped))) {
  if (isTRUE((as.Date(date_ceftri_stopped) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_ceftri_stopped) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_ceftriaxone_prescribe) && as.Date(date_ceftriaxone_prescribe) > as.Date(date_ceftri_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_ceftri_stopped) > as.Date(date_discharge)) |
             (as.Date(date_ceftri_stopped) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_ceftri_stopped")
    msg__x2014cin = c(msg__x2014cin, "'ceftriaxone stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_ceftri_stopped)
  }
}

# Amikacin
if (isTRUE(date_can_be_validated(amikacin_date))) {
  if (isTRUE((as.Date(amikacin_date) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(amikacin_date) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_amikacin_stopped) && as.Date(amikacin_date) > as.Date(date_amikacin_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(amikacin_date) > as.Date(date_discharge)) |
             (as.Date(amikacin_date) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "amikacin_date")
    msg__x2014cin = c(msg__x2014cin, "'Amikacin prescription Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, amikacin_date)
  }
}

if (isTRUE(date_can_be_validated(date_amikacin_stopped))) {
  if (isTRUE((as.Date(date_amikacin_stopped) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_amikacin_stopped) < as.Date(date_of_birth)) |
             (date_can_be_validated(amikacin_date) && as.Date(amikacin_date) > as.Date(date_amikacin_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(date_amikacin_stopped) > as.Date(date_discharge)) |
             (as.Date(date_amikacin_stopped) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "date_amikacin_stopped")
    msg__x2014cin = c(msg__x2014cin, "'Amikacin stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_amikacin_stopped)
  }
}

# Ceftazidime
if (isTRUE(date_can_be_validated(cefta_date))) {
  if (isTRUE((as.Date(cefta_date) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(cefta_date) < as.Date(date_of_birth)) |
             (date_can_be_validated(cefta_date_stopped) && as.Date(cefta_date) > as.Date(cefta_date_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(cefta_date) > as.Date(date_discharge)) |
             (as.Date(cefta_date) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "cefta_date")
    msg__x2014cin = c(msg__x2014cin, "'Ceftazidime prescription Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, cefta_date)
  }
}

if (isTRUE(date_can_be_validated(cefta_date_stopped))) {
  if (isTRUE((as.Date(cefta_date_stopped) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(cefta_date_stopped) < as.Date(date_of_birth)) |
             (date_can_be_validated(cefta_date) && as.Date(cefta_date) > as.Date(cefta_date_stopped)) |
             (date_can_be_validated(date_discharge) && as.Date(cefta_date_stopped) > as.Date(date_discharge)) |
             (as.Date(cefta_date_stopped) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "drug_treatment")
    name__x2014cin = c(name__x2014cin, "cefta_date_stopped")
    msg__x2014cin = c(msg__x2014cin, "'Ceftazidime stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, cefta_date_stopped)
  }
}

# CPAP
if (isTRUE(date_can_be_validated(cpap_start))) {
  if (isTRUE((as.Date(cpap_start) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(cpap_start) < as.Date(date_of_birth)) |
             (date_can_be_validated(cpap_stop) && as.Date(cpap_start) > as.Date(cpap_stop)) |
             (date_can_be_validated(date_discharge) && as.Date(cpap_start) > as.Date(date_discharge)) |
             (as.Date(cpap_start) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "cpap_start")
    msg__x2014cin = c(msg__x2014cin, "'CPAP start Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, cpap_start)
  }
}

if (isTRUE(date_can_be_validated(cpap_stop))) {
  if (isTRUE((as.Date(cpap_stop) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(cpap_stop) < as.Date(date_of_birth)) |
             (date_can_be_validated(cpap_start) && as.Date(cpap_start) > as.Date(cpap_stop)) |
             (date_can_be_validated(date_discharge) && as.Date(cpap_stop) > as.Date(date_discharge)) |
             (as.Date(cpap_stop) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "cpap_stop")
    msg__x2014cin = c(msg__x2014cin, "'ceftriaxone stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, cpap_stop)
  }
}


# Blood transfusion
if (isTRUE(date_can_be_validated(blood_transf_pres_date))) {
  if (isTRUE((as.Date(blood_transf_pres_date) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(blood_transf_pres_date) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_discharge) && as.Date(blood_transf_pres_date) > as.Date(date_discharge)) |
             (as.Date(blood_transf_pres_date) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "blood_transf_pres_date")
    msg__x2014cin = c(msg__x2014cin, "'Blood transfusion Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, blood_transf_pres_date)
  }
}

# Fluids
if (isTRUE(date_can_be_validated(date_fluid_presc))) {
  if (isTRUE((as.Date(date_fluid_presc) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_fluid_presc) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_discharge) && as.Date(date_fluid_presc) > as.Date(date_discharge)) |
             (as.Date(date_fluid_presc) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "date_fluid_presc")
    msg__x2014cin = c(msg__x2014cin, "'Fluid prescription Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_fluid_presc)
  }
}

# Feeds
if (isTRUE(date_can_be_validated(date_feeds_prescribed))) {
  if (isTRUE((as.Date(date_feeds_prescribed) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_feeds_prescribed) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_the_feeds_initiated) && as.Date(date_feeds_prescribed) > as.Date(date_the_feeds_initiated)) |
             (date_can_be_validated(date_discharge) && as.Date(date_feeds_prescribed) > as.Date(date_discharge)) |
             (as.Date(date_feeds_prescribed) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "date_feeds_prescribed")
    msg__x2014cin = c(msg__x2014cin, "'Feeds prescription Date' cannot be before date of admission/birth OR After start/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_feeds_prescribed)
  }
}

if (isTRUE(date_can_be_validated(date_the_feeds_initiated))) {
  if (isTRUE((as.Date(date_the_feeds_initiated) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(date_the_feeds_initiated) < as.Date(date_of_birth)) |
             (date_can_be_validated(date_feeds_prescribed) && as.Date(date_feeds_prescribed) > as.Date(date_the_feeds_initiated)) |
             (date_can_be_validated(date_discharge) && as.Date(date_the_feeds_initiated) > as.Date(date_discharge)) |
             (as.Date(date_the_feeds_initiated) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "date_the_feeds_initiated")
    msg__x2014cin = c(msg__x2014cin, "'Feeds Start Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_the_feeds_initiated)
  }
}

# Phototherapy
if (isTRUE(date_can_be_validated(start_date_phototherapy))) {
  if (isTRUE((as.Date(start_date_phototherapy) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(start_date_phototherapy) < as.Date(date_of_birth)) |
             (date_can_be_validated(stop_date_phototherapy) && as.Date(start_date_phototherapy) > as.Date(stop_date_phototherapy)) |
             (date_can_be_validated(date_discharge) && as.Date(start_date_phototherapy) > as.Date(date_discharge)) |
             (as.Date(start_date_phototherapy) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "start_date_phototherapy")
    msg__x2014cin = c(msg__x2014cin, "'Phototherapy Start Date' cannot be before date of admission/birth OR After stop/discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, start_date_phototherapy)
  }
}

if (isTRUE(date_can_be_validated(stop_date_phototherapy))) {
  if (isTRUE((as.Date(stop_date_phototherapy) < as.Date(date_adm)  && date_can_be_validated(date_adm)) |
             (date_can_be_validated(date_of_birth) && as.Date(stop_date_phototherapy) < as.Date(date_of_birth)) |
             (date_can_be_validated(start_date_phototherapy) && as.Date(start_date_phototherapy) > as.Date(stop_date_phototherapy)) |
             (date_can_be_validated(date_discharge) && as.Date(stop_date_phototherapy) > as.Date(date_discharge)) |
             (as.Date(stop_date_phototherapy) > Sys.Date()))){
    form__x2014cin = c(form__x2014cin, "supportive_care")
    name__x2014cin = c(name__x2014cin, "stop_date_phototherapy")
    msg__x2014cin = c(msg__x2014cin, "'Phototherapy Stop Date' cannot be before date of admission/birth/prescription date OR After discharge/in future")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, stop_date_phototherapy)
  }
}
rm(.__update_date)
rm(.__is_update)
#Check weight documentation section
.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_adm", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_discharge", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_doc", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_1", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_1", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (isTRUE(date_can_be_validated(date_adm)) & isTRUE(date_can_be_validated(date_discharge))) {
  if (isTRUE((as.Date(date_discharge) - as.Date(date_adm)) > 7)){
    if(as.numeric(birth_wt)<2.5 & is.na(weight_doc)){
      form__x2014cin = c(form__x2014cin, "weight_monitoring")
      name__x2014cin = c(name__x2014cin, "weight_doc")
      msg__x2014cin = c(msg__x2014cin, "Weight documented is missing")
      sect__x2014cin = c(sect__x2014cin, "weight_monitoring")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, weight_doc)
    }
  }
}
# Weight 1 documentations
if (isTRUE(!data_missing(weight_doc) & as.numeric(weight_doc)==1)) {
  if (isTRUE(abs(as.numeric(weight_1))!=1 & !(as.numeric(weight_1)>0.5 & as.numeric(weight_1)<6) &
             !(as.numeric(weight_1)>500 & as.numeric(weight_1)<6000))){
    form__x2014cin = c(form__x2014cin, "weight_monitoring")
    name__x2014cin = c(name__x2014cin, "weight_1")
    msg__x2014cin = c(msg__x2014cin, "No weight value indicated!")
    sect__x2014cin = c(sect__x2014cin, "weight_monitoring")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, weight_1)
  }
  if (isTRUE(data_missing(other_weight_2))){
    form__x2014cin = c(form__x2014cin, "weight_monitoring")
    name__x2014cin = c(name__x2014cin, 'other_weight_2')
    msg__x2014cin = c(msg__x2014cin, "other_weight_2 not checked!")
    sect__x2014cin = c(sect__x2014cin, "weight_monitoring")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, other_weight_2)
  }
  if (isTRUE(all(!as.Date(date_1, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01"),
                 !as.Date(date_1, "%Y-%m-%d")>=as.Date(date_adm), !as.Date(date_1, "%Y-%m-%d")<=(date_today)))){
    form__x2014cin = c(form__x2014cin, "weight_monitoring")
    name__x2014cin = c(name__x2014cin, 'date_1')
    msg__x2014cin = c(msg__x2014cin, paste("Invalide date_1 value!"))
    sect__x2014cin = c(sect__x2014cin, "weight_monitoring")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, date_1)
  }
}
rm(.__update_date)
rm(.__is_update)

# Check all other weight monitoring values
.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_3", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_4", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_5", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_6", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_7", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_8", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_9", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_10", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_11", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_12", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_13", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_14", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_15", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_16", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_17", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_18", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_19", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("other_weight_20", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_3", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_4", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_5", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_6", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_7", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_8", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_9", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_10", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_11", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_12", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_13", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_14", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_15", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_16", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_17", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_18", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_19", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("weight_20", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_2", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_3", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_4", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_5", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_6", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_7", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_8", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_9", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_10", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_11", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_12", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_13", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_14", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_15", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_16", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_17", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_18", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_19", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_20", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
sapply(paste0('other_weight_', c(2:20)), function(x){
  sapply(paste0('weight_', c(2:20)), function(y){
    if (isTRUE(as.numeric(x)==1)) {
      if (isTRUE(abs(as.numeric(y))!=1 & !(as.numeric(y)>0.5 & as.numeric(y)<6) &
                 !(as.numeric(y)>500 & as.numeric(y)<6000))){
        form__x2014cin = c(form__x2014cin, "weight_monitoring")
        name__x2014cin = c(name__x2014cin, y)
        msg__x2014cin = c(msg__x2014cin, paste("No",y,"value indicated!"))
        sect__x2014cin = c(sect__x2014cin, "weight_monitoring")
        type__x2014cin = c(type__x2014cin, "")
        entry__x2014cin = c(entry__x2014cin, y)
      }
      z1 = unlist(strsplit(x,"_"))[1:3]
      z = paste(z1[1:2], as.numeric(z1[3]), sep = '_')
      if (isTRUE(all(is.na(z), as.numeric(z1[3])<21))){
        form__x2014cin = c(form__x2014cin, "weight_monitoring")
        name__x2014cin = c(name__x2014cin, z)
        msg__x2014cin = c(msg__x2014cin, paste(z,"not checked!"))
        sect__x2014cin = c(sect__x2014cin, "weight_monitoring")
        type__x2014cin = c(type__x2014cin, "")
        entry__x2014cin = c(entry__x2014cin, z)
      }
    }
  })
  sapply(paste0('date_', c(2:20)), function(d){
    if (isTRUE(as.numeric(x)==1)) {
      if (isTRUE(!as.Date(d, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01") &
                 !(as.Date(d, "%Y-%m-%d")>=as.Date(date_adm) & as.Date(d)<=(date_discharge)))){
        form__x2014cin = c(form__x2014cin, "weight_monitoring")
        name__x2014cin = c(name__x2014cin, d)
        msg__x2014cin = c(msg__x2014cin, paste("Invalide",d,"value!"))
        sect__x2014cin = c(sect__x2014cin, "weight_monitoring")
        type__x2014cin = c(type__x2014cin, "")
        entry__x2014cin = c(entry__x2014cin, d)
      }
    }
  })
})
rm(.__update_date)
rm(.__is_update)

# Weight at admission
.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("wt_now", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("birth_wt", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("date_today", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("birth_wt_units", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("discharge_weight", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("discharge_wt_units", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)
if (all(!is.na(wt_now) , abs(as.numeric(wt_now))!=1 , !(as.numeric(wt_now)>0.5 & as.numeric(wt_now)<6),
        !(as.numeric(wt_now)>500 & as.numeric(wt_now)<6000))){
  form__x2014cin = c(form__x2014cin, "babys_history")
  name__x2014cin = c(name__x2014cin, 'wt_now')
  msg__x2014cin = c(msg__x2014cin, paste("Value for weight not is out of range!"))
  sect__x2014cin = c(sect__x2014cin, "babys_history")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, wt_now)
}

# Add the following to the custom script in the error validation folder
if (all(as.numeric(birth_wt)>0.5,!is.na(as.numeric(birth_wt)), as.Date(date_today)>=as.Date("2018-10-15"))){
  if (isTRUE(is.na(birth_wt_units))) {
    form__x2014cin = c(form__x2014cin, "Bio Data")
    name__x2014cin = c(name__x2014cin, "birth_wt_units")
    msg__x2014cin = c(msg__x2014cin, "Units for birth weight is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, birth_wt_units)
  }else{
  if (any(
    birth_wt_units == 1 & (as.numeric(birth_wt)<500 | as.numeric(birth_wt)>5500),
    birth_wt_units == 2 & (as.numeric(birth_wt)<0.5 | as.numeric(birth_wt)>5.5)
  )) {
    form__x2014cin = c(form__x2014cin, "Bio Data")
    name__x2014cin = c(name__x2014cin, "birth_wt")
    msg__x2014cin = c(msg__x2014cin, "Birth weight is out of range'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, birth_wt)
  }
}}

if (all(as.numeric(discharge_weight)>0.5,!is.na(as.numeric(discharge_weight)), as.Date(date_today)>=as.Date("2018-10-15"))) {
  if (is.na(discharge_wt_units)) {
    form__x2014cin = c(form__x2014cin, "Discharge Information")
    name__x2014cin = c(name__x2014cin, "discharge_wt_units")
    msg__x2014cin = c(msg__x2014cin, "Units for discharge weight is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, discharge_wt_units)
   }
    #else{
  #   if (any(
  #     discharge_wt_units == 1 & (as.numeric(discharge_weight) < 1700),
  #     discharge_wt_units == 2 & (as.numeric(discharge_weight) < 1.7)
  #   )){
  #     form__x2014cin = c(form__x2014cin, "Bio Data")
  #     name__x2014cin = c(name__x2014cin, "discharge_weight")
  #     msg__x2014cin = c(msg__x2014cin, "Discharge weight is out of range'!")
  #     sect__x2014cin = c(sect__x2014cin, "")
  #     type__x2014cin = c(type__x2014cin, "")
  #     entry__x2014cin = c(entry__x2014cin, discharge_weight)
  #     
  #   }
  # }
}
# ==================================================================================================================

if(all(!is.na(discharge_wt_units), as.numeric(discharge_wt_units) %in% c(1,2), as.Date(date_today)>=as.Date("2018-10-15"),
       ((birth_wt_units == 1 & !is.na(birth_wt) & abs(as.numeric(birth_wt)) > 0 & (as.numeric(birth_wt)<1500 & discharge_wt_units == 1 &
                                                                                   as.numeric(discharge_weight)<1700 & as.numeric(discharge_weight)>6500 & outcome != 2)) | 
        (birth_wt_units == 2 & !is.na(birth_wt) & abs(as.numeric(birth_wt)) > 0 & as.numeric(birth_wt)<1.5 & discharge_wt_units == 2 & 
         (as.numeric(discharge_weight)<1.7 | as.numeric(discharge_weight)>6.5) & outcome !=2)))){
  form__x2014cin = c(form__x2014cin, "Discharge Information")
  name__x2014cin = c(name__x2014cin, "discharge_weight")
  msg__x2014cin = c(msg__x2014cin, paste0("Discharge weight(",discharge_weight,") is out of range'!"))
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, discharge_weight)
}
rm(.__update_date)
rm(.__is_update)

# Time: t_birth, t_adm, t_discharge,cpap_start_time, cpap_end_time
.__update_date = c(
  Reduce(c, lapply(.__update, function(x) x$get_update_date("cpap_start_time", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("cpap_end_time", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("t_birth", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("t_adm", hosp_id))),
  Reduce(c, lapply(.__update, function(x) x$get_update_date("t_discharge", hosp_id)))
)
.__update_date = if (is.null(.__update_date)) NA else .__update_date
if (length(na.omit(.__update_date)) > 0)
  .__update_date = max(.__update_date, na.rm = T)
else
  .__update_date = NA
.__is_update = !is.na(.__update_date)

if(is.na(t_birth) | (!is.na(t_birth) & as.character(t_birth)!='-1'  & bba ==0 &
                     (!grepl('[0-9]:[0-9]',t_birth) | (!grepl('am$',tolower(t_birth)) & 
                                                       !grepl('pm$',tolower(t_birth)))))){
  form__x2014cin = c(form__x2014cin, "Biodata")
  name__x2014cin = c(name__x2014cin, "t_birth")
  msg__x2014cin = c (msg__x2014cin, "Time of Birth if invalid!")
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, t_birth)
}

if(is.na(t_adm) | (!is.na(t_adm) & as.character(t_adm)!='-1'  &
                   (!grepl('[0-9]:[0-9]',t_adm) | (!grepl('am$',tolower(t_adm)) & 
                                                   !grepl('pm$',tolower(t_adm)))))){
  form__x2014cin = c(form__x2014cin, "Biodata")
  name__x2014cin = c(name__x2014cin, "t_adm")
  msg__x2014cin = c (msg__x2014cin, "Time of Birth is invalid!")
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, t_adm)
}

if(is.na(t_seen) | (!is.na(t_seen) & as.character(t_seen)!='-1'  &
                   (!grepl('[0-9]:[0-9]',t_seen) | (!grepl('am$',tolower(t_seen)) & 
                                                   !grepl('pm$',tolower(t_seen)))))){
  form__x2014cin = c(form__x2014cin, "Biodata")
  name__x2014cin = c(name__x2014cin, "t_seen")
  msg__x2014cin = c (msg__x2014cin, "Time of Birth is invalid!")
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, t_seen)
}

if(!is.na(cpap) & as.numeric(cpap)==1  & 
   (is.na(cpap_start_time) | (!is.na(cpap_start_time) & as.character(cpap_start_time)!='-1'  &
   (!grepl('[0-9]:[0-9]',cpap_start_time) | (!grepl('am$',tolower(cpap_start_time)) & 
                                             !grepl('pm$',tolower(cpap_start_time))))))){
  form__x2014cin = c(form__x2014cin, "Biodata")
  name__x2014cin = c(name__x2014cin, "cpap_start_time")
  msg__x2014cin = c (msg__x2014cin, "CPAP start time is invalid!")
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, cpap_start_time)
}

if(!is.na(cpap) & as.numeric(cpap)==1 & 
   (is.na(cpap_end_time) | (!is.na(cpap_end_time) & as.character(cpap_end_time)!='-1'  &
   (!grepl('[0-9]:[0-9]',cpap_end_time) | (!grepl('am$',tolower(cpap_end_time)) & 
                                           !grepl('pm$',tolower(cpap_end_time))))))){
  form__x2014cin = c(form__x2014cin, "Biodata")
  name__x2014cin = c(name__x2014cin, "cpap_end_time")
  msg__x2014cin = c (msg__x2014cin, "CPAP end time is invalid!")
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, cpap_end_time)
}
if(is.na(t_discharge) | (!is.na(t_discharge) & as.character(t_discharge)!='-1'  &
                         (!grepl('[0-9]:[0-9]',t_discharge) | (!grepl('am$',tolower(t_discharge)) & 
                                                               !grepl('pm$',tolower(t_discharge)))))){
  form__x2014cin = c(form__x2014cin, "Biodata")
  name__x2014cin = c(name__x2014cin, "t_discharge")
  msg__x2014cin = c (msg__x2014cin, "Discharge time is invalid!")
  sect__x2014cin = c(sect__x2014cin, "")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, t_discharge)
}
rm(.__update_date)
rm(.__is_update)

# Post discharge Weight 1 documentations

# Add the following to the custom script in the error validation folder
if (all(as.numeric(postdischarge_weight_1)>0.5,!is.na(as.numeric(postdischarge_weight_1)), as.Date(date_today)>=as.Date("2018-10-15"))){
  if (isTRUE(is.na(post_weight_unit1))) {
    form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
    name__x2014cin = c(name__x2014cin, "post_weight_unit1")
    msg__x2014cin = c(msg__x2014cin, "Units for post discharge weight1 is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, post_weight_unit1)
  }else{
    if (any(
      post_weight_unit1 == 1 & (as.numeric(postdischarge_weight_1)<1750 | as.numeric(postdischarge_weight_1)>6500),
      post_weight_unit1 == 2 & (as.numeric(postdischarge_weight_1)<1.75 | as.numeric(postdischarge_weight_1)>6.5)
    )) {
      form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
      name__x2014cin = c(name__x2014cin, "postdischarge_weight_1")
      msg__x2014cin = c(msg__x2014cin, "postdischarge_weight_1 is out of range'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, postdischarge_weight_1)
    }
  }}

if (all(as.numeric(postdischarge_weight_2)>0.5,!is.na(as.numeric(postdischarge_weight_2)), as.Date(date_today)>=as.Date("2018-10-15"))){
  if (isTRUE(is.na(post_weight_unit2))) {
    form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
    name__x2014cin = c(name__x2014cin, "post_weight_unit2")
    msg__x2014cin = c(msg__x2014cin, "Units for post discharge weight1 is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, post_weight_unit2)
  }else{
    if (any(
      post_weight_unit2 == 1 & (as.numeric(postdischarge_weight_2)<1750 | as.numeric(postdischarge_weight_2)>6500),
      post_weight_unit2 == 2 & (as.numeric(postdischarge_weight_2)<1.75 | as.numeric(postdischarge_weight_2)>6.5)
    )) {
      form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
      name__x2014cin = c(name__x2014cin, "postdischarge_weight_2")
      msg__x2014cin = c(msg__x2014cin, "postdischarge_weight_2 is out of range'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, postdischarge_weight_2)
    }
  }}

if (all(as.numeric(postdischarge_weight_3)>0.5,!is.na(as.numeric(postdischarge_weight_3)), as.Date(date_today)>=as.Date("2018-10-15"))){
  if (isTRUE(is.na(post_weight_unit3))) {
    form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
    name__x2014cin = c(name__x2014cin, "post_weight_unit3")
    msg__x2014cin = c(msg__x2014cin, "Units for post discharge weight1 is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, post_weight_unit3)
  }else{
    if (any(
      post_weight_unit3 == 1 & (as.numeric(postdischarge_weight_3)<1750 | as.numeric(postdischarge_weight_3)>6500),
      post_weight_unit3 == 2 & (as.numeric(postdischarge_weight_3)<1.75 | as.numeric(postdischarge_weight_3)>6.5)
    )) {
      form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
      name__x2014cin = c(name__x2014cin, "postdischarge_weight_3")
      msg__x2014cin = c(msg__x2014cin, "postdischarge_weight_3 is out of range'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, postdischarge_weight_3)
    }
  }}

if (all(as.numeric(postdischarge_weight_4)>0.5,!is.na(as.numeric(postdischarge_weight_4)), as.Date(date_today)>=as.Date("2018-10-15"))){
  if (isTRUE(is.na(post_weight_unit4))) {
    form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
    name__x2014cin = c(name__x2014cin, "post_weight_unit4")
    msg__x2014cin = c(msg__x2014cin, "Units for post discharge weight1 is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, post_weight_unit4)
  }else{
    if (any(
      post_weight_unit4 == 1 & (as.numeric(postdischarge_weight_4)<1750 | as.numeric(postdischarge_weight_4)>6500),
      post_weight_unit4 == 2 & (as.numeric(postdischarge_weight_4)<1.75 | as.numeric(postdischarge_weight_4)>6.5)
    )) {
      form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
      name__x2014cin = c(name__x2014cin, "postdischarge_weight_4")
      msg__x2014cin = c(msg__x2014cin, "postdischarge_weight_4 is out of range'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, postdischarge_weight_4)
    }
  }}

if (all(as.numeric(postdischarge_weight_5)>0.5,!is.na(as.numeric(postdischarge_weight_5)), as.Date(date_today)>=as.Date("2018-10-15"))){
  if (isTRUE(is.na(post_weight_unit5))) {
    form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
    name__x2014cin = c(name__x2014cin, "post_weight_unit5")
    msg__x2014cin = c(msg__x2014cin, "Units for post discharge weight1 is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, post_weight_unit5)
  }else{
    if (any(
      post_weight_unit5 == 1 & (as.numeric(postdischarge_weight_5)<1750 | as.numeric(postdischarge_weight_5)>6500),
      post_weight_unit5 == 2 & (as.numeric(postdischarge_weight_5)<1.75 | as.numeric(postdischarge_weight_5)>6.5)
    )) {
      form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
      name__x2014cin = c(name__x2014cin, "postdischarge_weight_5")
      msg__x2014cin = c(msg__x2014cin, "postdischarge_weight_5 is out of range'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, postdischarge_weight_5)
    }
  }}

if (all(as.numeric(postdischarge_weight_6)>0.5,!is.na(as.numeric(postdischarge_weight_6)), as.Date(date_today)>=as.Date("2018-10-15"))){
  if (isTRUE(is.na(post_weight_unit6))) {
    form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
    name__x2014cin = c(name__x2014cin, "post_weight_unit6")
    msg__x2014cin = c(msg__x2014cin, "Units for post discharge weight1 is missing'!")
    sect__x2014cin = c(sect__x2014cin, "")
    type__x2014cin = c(type__x2014cin, "")
    entry__x2014cin = c(entry__x2014cin, post_weight_unit6)
  }else{
    if (any(
      post_weight_unit6 == 1 & (as.numeric(postdischarge_weight_6)<1750 | as.numeric(postdischarge_weight_6)>6500),
      post_weight_unit6 == 2 & (as.numeric(postdischarge_weight_6)<1.75 | as.numeric(postdischarge_weight_6)>6.5)
    )) {
      form__x2014cin = c(form__x2014cin, "Post Discharge Weight monitoring")
      name__x2014cin = c(name__x2014cin, "postdischarge_weight_6")
      msg__x2014cin = c(msg__x2014cin, "postdischarge_weight_6 is out of range'!")
      sect__x2014cin = c(sect__x2014cin, "")
      type__x2014cin = c(type__x2014cin, "")
      entry__x2014cin = c(entry__x2014cin, postdischarge_weight_6)
    }
  }}

# if (isTRUE(!is.na(postdischarge_weight_1) & !(as.numeric(postdischarge_weight_1)>1.75 & as.numeric(postdischarge_weight_1)<6.5) &
#            !(as.numeric(postdischarge_weight_1)>1750 & as.numeric(weight_1)<6500))){
#   form__x2014cin = c(form__x2014cin, "post_discharge_weights")
#   name__x2014cin = c(name__x2014cin, "postdischarge_weight_1")
#   msg__x2014cin = c(msg__x2014cin, "Post discharge weight out of range!")
#   sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
#   type__x2014cin = c(type__x2014cin, "")
#   entry__x2014cin = c(entry__x2014cin, postdischarge_weight_1)
# }
if (isTRUE(all(!as.Date(post_weight_date1, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01"),
               !as.Date(post_weight_date1, "%Y-%m-%d")>=as.Date(date_adm), !as.Date(post_weight_date1, "%Y-%m-%d")<=(date_today), post_weight_date1 !='' ))){
  form__x2014cin = c(form__x2014cin, "post_discharge_weights")
  name__x2014cin = c(name__x2014cin, 'post_weight_date1')
  msg__x2014cin = c(msg__x2014cin, paste("Invalide post_weight_date1 value!"))
  sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, post_weight_date1)
}

# if (isTRUE(!is.na(postdischarge_weight_2) & !(as.numeric(postdischarge_weight_2)>1.75 & as.numeric(postdischarge_weight_2)<6.5) &
#            !(as.numeric(postdischarge_weight_2)>1750 & as.numeric(weight_1)<6500))){
#   form__x2014cin = c(form__x2014cin, "post_discharge_weights")
#   name__x2014cin = c(name__x2014cin, "postdischarge_weight_2")
#   msg__x2014cin = c(msg__x2014cin, "Post discharge weight out of range!")
#   sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
#   type__x2014cin = c(type__x2014cin, "")
#   entry__x2014cin = c(entry__x2014cin, postdischarge_weight_2)
# }
if (isTRUE(all(!as.Date(post_weight_date2, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01"),
               !as.Date(post_weight_date2, "%Y-%m-%d")>=as.Date(date_adm), !as.Date(post_weight_date2, "%Y-%m-%d")<=(date_today), post_weight_date2 !='' ))){
  form__x2014cin = c(form__x2014cin, "post_discharge_weights")
  name__x2014cin = c(name__x2014cin, 'post_weight_date2')
  msg__x2014cin = c(msg__x2014cin, paste("Invalide post_weight_date2 value!"))
  sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, post_weight_date2)
}

# if (isTRUE(!is.na(postdischarge_weight_3) & !(as.numeric(postdischarge_weight_3)>1.75 & as.numeric(postdischarge_weight_3)<6.5) &
#            !(as.numeric(postdischarge_weight_3)>1750 & as.numeric(weight_1)<6500))){
#   form__x2014cin = c(form__x2014cin, "post_discharge_weights")
#   name__x2014cin = c(name__x2014cin, "postdischarge_weight_3")
#   msg__x2014cin = c(msg__x2014cin, "Post discharge weight out of range!")
#   sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
#   type__x2014cin = c(type__x2014cin, "")
#   entry__x2014cin = c(entry__x2014cin, postdischarge_weight_3)
# }
if (isTRUE(all(!as.Date(post_weight_date3, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01"),
               !as.Date(post_weight_date3, "%Y-%m-%d")>=as.Date(date_adm), !as.Date(post_weight_date3, "%Y-%m-%d")<=(date_today), post_weight_date3 !='' ))){
  form__x2014cin = c(form__x2014cin, "post_discharge_weights")
  name__x2014cin = c(name__x2014cin, 'post_weight_date3')
  msg__x2014cin = c(msg__x2014cin, paste("Invalide post_weight_date3 value!"))
  sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, post_weight_date3)
}

# if (isTRUE(!is.na(postdischarge_weight_4) & !(as.numeric(postdischarge_weight_4)>1.75 & as.numeric(postdischarge_weight_4)<6.5) &
#            !(as.numeric(postdischarge_weight_4)>1750 & as.numeric(weight_1)<6500))){
#   form__x2014cin = c(form__x2014cin, "post_discharge_weights")
#   name__x2014cin = c(name__x2014cin, "postdischarge_weight_4")
#   msg__x2014cin = c(msg__x2014cin, "Post discharge weight out of range!")
#   sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
#   type__x2014cin = c(type__x2014cin, "")
#   entry__x2014cin = c(entry__x2014cin, postdischarge_weight_4)
# }
if (isTRUE(all(!as.Date(post_weight_date4, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01"),
               !as.Date(post_weight_date4, "%Y-%m-%d")>=as.Date(date_adm), !as.Date(post_weight_date4, "%Y-%m-%d")<=(date_today), post_weight_date4 !='' ))){
  form__x2014cin = c(form__x2014cin, "post_discharge_weights")
  name__x2014cin = c(name__x2014cin, 'post_weight_date4')
  msg__x2014cin = c(msg__x2014cin, paste("Invalide post_weight_date4 value!"))
  sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, post_weight_date4)
}

# if (isTRUE(!is.na(postdischarge_weight_5) & !(as.numeric(postdischarge_weight_5)>1.75 & as.numeric(postdischarge_weight_5)<6.5) &
#            !(as.numeric(postdischarge_weight_5)>1750 & as.numeric(postdischarge_weight_5)<6500))){
#   form__x2014cin = c(form__x2014cin, "post_discharge_weights")
#   name__x2014cin = c(name__x2014cin, "postdischarge_weight_5")
#   msg__x2014cin = c(msg__x2014cin, "Post discharge weight out of range!")
#   sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
#   type__x2014cin = c(type__x2014cin, "")
#   entry__x2014cin = c(entry__x2014cin, postdischarge_weight_5)
# }
if (isTRUE(all(!as.Date(post_weight_date5, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01"),
               !as.Date(post_weight_date5, "%Y-%m-%d")>=as.Date(date_adm), !as.Date(post_weight_date5, "%Y-%m-%d")<=(date_today), post_weight_date5 !='' ))){
  form__x2014cin = c(form__x2014cin, "post_discharge_weights")
  name__x2014cin = c(name__x2014cin, 'post_weight_date5')
  msg__x2014cin = c(msg__x2014cin, paste("Invalide post_weight_date5 value!"))
  sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, post_weight_date5)
}

# if (isTRUE(!is.na(postdischarge_weight_6) & !(as.numeric(postdischarge_weight_6)>1.75 & as.numeric(postdischarge_weight_6)<6.5) &
#            !(as.numeric(postdischarge_weight_6)>1750 & as.numeric(postdischarge_weight_6)<6500))){
#   form__x2014cin = c(form__x2014cin, "post_discharge_weights")
#   name__x2014cin = c(name__x2014cin, "postdischarge_weight_6")
#   msg__x2014cin = c(msg__x2014cin, "Post discharge weight out of range!")
#   sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
#   type__x2014cin = c(type__x2014cin, "")
#   entry__x2014cin = c(entry__x2014cin, postdischarge_weight_6)
# }
if (isTRUE(all(!as.Date(post_weight_date6, "%Y-%m-%d") %in% as.Date("1914-01-01","1913-01-01"),
               !as.Date(post_weight_date6, "%Y-%m-%d")>=as.Date(date_adm), !as.Date(post_weight_date6, "%Y-%m-%d")<=(date_today), post_weight_date6 !='' ))){
  form__x2014cin = c(form__x2014cin, "post_discharge_weights")
  name__x2014cin = c(name__x2014cin, 'post_weight_date6')
  msg__x2014cin = c(msg__x2014cin, paste("Invalide post_weight_date6 value!"))
  sect__x2014cin = c(sect__x2014cin, "post_discharge_weights")
  type__x2014cin = c(type__x2014cin, "")
  entry__x2014cin = c(entry__x2014cin, post_weight_date6)
}


rm(.__update_date)
rm(.__is_update)
