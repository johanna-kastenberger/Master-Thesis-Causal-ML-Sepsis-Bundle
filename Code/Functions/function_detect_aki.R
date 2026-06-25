#' Identify Acute Kidney Injury
#'
#' Uses KDIGO Clinical practice guidline for acute kidney injury to
#' detect AKI events.
#'
#' Using Kidney Disease: Improving Global Outcomes (KDIGO)
#' guidelines, Acute Kidney Injury (AKI) are detected using this
#' function. Guidelines implemented can be found at[1].
#'
#' Note: if no reference index (RI_HIGH) is provided for a creatinine
#' value, a value will be generated using reference values found in
#' [2]. For ages under 18, the fractional polynomial value will be
#' used. This value may be used to categorize stage 3 AKI.
#'
#' 1. Selby NM, Hill R, Fluck RJ. Standardizing the early
#' identification of acute kidney injury: the NHS England ational
#' patient safety alert. \emph{Nephron.} 2015;131:113-117.
#'
#' 2. Ceriotti F, Boyd JC, Klein G, et al. Reference intervals for
#' serum creatinine concentrations: assessment of available data
#' for global application. \emph{Clinical Chemistry.}
#' 2008;54(3):559-566.
#'
#' @param df Data frame containing specific columns:
#'
#' \strong{ID:} Unique patient identification.
#'
#' \strong{COLLECTED_DT:} Dates when corresponding samples were
#' collected. Must be R datetime object.
#'
#' \strong{RESULT:} Creatinine value cooresponding to collected sample.
#' Units must be mg/dl.
#'
#' Optional:
#'
#' \strong{BIRTH_DT:} Birth date of patient. Must be R date or
#' datetime object. Only used for ages under 18.
#'
#' \strong{ULRI:} Reference index for upper limit of normal expected
#' creatinine levels. Only used for ages under 18. If no value is provided,
#' a value will be calculated based on age (see note in details). Units
#' must be mg/dl.
#'
#' @return Returns a data frame identifying acute kidney injury with specific
#' colums:
#'
#' \strong{AKI_STAGE:} Classifies the stage (1,2,3) of AKI event. 0 if there
#' is no AKI event and NA if classification is unknown.
#'
#' \strong{AKI_BASELINE:} Baseline used in detection of AKI_FLAG. Returns
#' NA if no baseline was established.
#'
#' @keywords kdigo, kidney
#' @export
detect_aki <- function(df) {
  #--------------------------------------------------------------------------------
  # PROCESS INPUT
  #--------------------------------------------------------------------------------
  creatinine <- dplyr::mutate(df,
                       COLLECTED_DT = as.numeric(df$COLLECTED_DT) / (24 * 3600),
                       BIRTH_DT = as.numeric(df$BIRTH_DT) / (24 * 3600),
                       RESULT = as.numeric(df$RESULT),
                       AKI_STAGE = as.numeric(NA),
                       AKI_BASELINE = as.numeric(NA),
                       CR_ROW = 1:nrow(df))
  if ("ULRI" %in% colnames(df)) {
    creatinine <- dplyr::mutate(creatinine, ULRI = df$ULRI)
  } else {
    creatinine <- dplyr::mutate(creatinine, ULRI = as.numeric(NA))
  }
  creatinine <- dplyr::filter(creatinine, !is.na(ID) & !is.na(RESULT) & !is.na(COLLECTED_DT))
  creatinine <- dplyr::arrange(creatinine, ID, COLLECTED_DT)
  creatinine <- dplyr::group_by(creatinine, ID)
  # define variable
  two_day_low <- NA


  #--------------------------------------------------------------------------------
  # ESSENTIAL FUNCTIONS
  #--------------------------------------------------------------------------------

  # use to establish substitute ULRI value
  # age in years
  find_ulri <- function(age) {
    return(-0.007297343 - 0.2164216 * log(age) + 0.3504704 * sqrt(age))
  }

  # determines whether there is a baseline and what that should be
  find_baseline <- function(previous, current) {
    differences <- creatinine$COLLECTED_DT[current] - creatinine$COLLECTED_DT[previous]
    two_days <- creatinine$RESULT[previous[differences <= 2]]
    week <- creatinine$RESULT[previous[differences <= 7]]
    year <- creatinine$RESULT[previous[differences <= 365 & differences > 7]]
    two_day_low <<- NA
    if (length(two_days) > 0) {
      two_day_low <<- min(two_days)
    }
    if (length(week) > 0 && length(year) > 0) {
      return(min(min(week), median(year)))
    } else if (length(week) > 0) {
      return(min(week))
    } else if (length(year) > 0) {
      return(median(year))
    } else {
      return(NA)
    }
  }

  # use calculated baseline to identify aki
  use_baseline <- function(index, baseline) {
    cr <- creatinine$RESULT[[index]]
    ratio <- cr / baseline
    age <- creatinine$COLLECTED_DT[[index]] - creatinine$BIRTH_DT[[index]]
    if (ratio >= 1.5) {
      if (!is.na(age) && age < 6570) {
        if (is.na(creatinine$ULRI[[index]])) {
          creatinine$ULRI[[index]] <<- find_ulri(age / 365)
        }
        if (cr > 3 * creatinine$ULRI[[index]]) {
          return(3)
        }
      }
      if (cr >= 4 || ratio >= 3) {
        return(3)
      } else if (ratio >= 2) {
        return(2)
      } else {
        return(1)
      }
    } else if (!is.na(two_day_low) && cr - two_day_low >= .3) {
      return(1)
    } else {
      return(0)
    }
  }


  #--------------------------------------------------------------------------------
  # FIND AKI EVENTS
  #--------------------------------------------------------------------------------
  groups <- attributes(creatinine)$groups$.rows
  for (i in seq_along(groups)) {
    for (j in seq_along(groups[[i]])) {
      index <- groups[[i]][[j]]
      if (j != 1) {
        baseline <- find_baseline(groups[[i]][1:(j - 1)], index)
        creatinine$AKI_BASELINE[[index]] <- baseline
        if (!is.na(baseline)) {
          creatinine$AKI_STAGE[[index]] <- use_baseline(index, baseline)
        }
      }
    }
  }


  #--------------------------------------------------------------------------------
  # GENERATE OUTPUT
  #--------------------------------------------------------------------------------
  creatinine <- dplyr::select(creatinine, CR_ROW, ULRI, AKI_STAGE, AKI_BASELINE)
  df <- dplyr::mutate(df,
                      AKI_STAGE = NA,
                      AKI_BASELINE = NA)
  df$AKI_STAGE[creatinine$CR_ROW] <- creatinine$AKI_STAGE
  df$AKI_BASELINE[creatinine$CR_ROW] <- creatinine$AKI_BASELINE
  return(df)
}


