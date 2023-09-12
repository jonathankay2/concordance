#' Converting 1987 SIC and NAICS Codes
#'
#' Concords Standard Industrial Classification Codes (1987SIC) to and from North American Industry Classification System codes (NAICS1997, NAICS2002 combined).
#'
#' @param sourcevar An input character vector of SIC or NAICS codes. The function accepts 2, 3, 4-digit codes for SIC and 2 to 6-digit codes for NAICS.
#' @param origin A string setting the input industry classification: "SIC1987", "SIC", "NAICS1997", "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined).
#' @param destination A string setting the output industry classification: "SIC1987", "SIC", "NAICS1997", "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 4, or 6 digits for HS and 2 to 6-digit codes for NAICS. The default is 6 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source SIC-NAICS concordance tables between 1987 and 1997, 2002 from the US Census <https://www.census.gov/naics/>.
#' @note Always include leading zeros in codes (e.g., use SIC code 01111 instead of 1111)---results may be buggy otherwise.
#' @examples
#' ## SIC combined to NAICS
#' # one input: one-to-one match
#' concord_sic_naics(sourcevar = "011",
#'                  origin = "SIC", destination = "NAICS",
#'                  all = FALSE)
#' concord_sic_naics(sourcevar = "011,
#'                  origin = "SIC", destination = "NAICS",
#'                  all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  all = FALSE)
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  all = TRUE)
#'
#' # two inputs: repeated
#' concord_sic_naics(sourcevar = c("0139", "0139"),
#'                  origin = "SIC", destination = "NAICS",
#'                  all = FALSE)
#'
#' # one to multiple matches????why there still two sourcevar codes with the origin of SIC?
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  all = TRUE)
#'
#' # if no match, will return NA and give warning message ????
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  all = FALSE)
#'
#' # 4-digit inputs
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  all = TRUE)
#'
#' # 4-digit outputs
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  dest.digit = 4, all = TRUE)
#'
#' ## SIC to NAICS
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  all = TRUE)
#'
#' concord_sic_naics(sourcevar = c("0139", "0161"),
#'                  origin = "SIC", destination = "NAICS",
#'                  dest.digit = 4, all = TRUE)
#'
#' ## NAICS to SIC
#' concord_sic_naics(sourcevar = c("111333", "111334"),
#'                  origin = "NAICS", destination = "SIC",
#'                  all = TRUE)
#'
#' concord_sic_naics(sourcevar = c("111333", "111334"),
#'                  origin = "NAICS", destination = "SIC",
#'                  dest.digit = 4, all = TRUE)

concord_sic_naics <- function (sourcevar,
                              origin,
                              destination,
                              dest.digit = 6,
                              all = FALSE) {
  
  # load specific conversion dictionary
  if ((origin == "SIC1987" & destination == "NAICS1997") | (origin == "NAICS1997" & destination == "SIC1987")) {
    
    dictionary <- concordance::sic87_naics97
    
    # load version codes
    naics.vec <- concordance::naics1997_desc
    
    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()
    
    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS1997_6d = .data$NAICS_6d,
             NAICS1997_5d = .data$NAICS_5d,
             NAICS1997_4d = .data$NAICS_4d,
             NAICS1997_3d = .data$NAICS_3d,
             NAICS1997_2d = .data$NAICS_2d) %>%
     select(.data$SIC_4d, .data$SIC_3d, .data$SIC_2d,
             .data$NAICS1997_6d, .data$NAICS1997_5d, .data$NAICS1997_4d, .data$NAICS1997_3d, .data$NAICS1997_2d) %>%
      distinct()
    
  } else if ((origin == "SIC1987" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "SIC1987")) {
    
    dictionary <- concordance::sic87_naics02
    
    # load version codes
    naics.vec <- concordance::naics2002_desc
    
    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()
    
    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$SIC_4d, .data$SIC_3d, .data$SIC_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()

  } else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}

  # check whether input codes have the same digits
  # NAICS code has some unusual 2-digit codes, exclude them when counting digits
  exempt.naics <- c("31-33", "44-45", "48-49")
  sourcevar.sub <- sourcevar[!sourcevar %in% exempt.naics]

  # avoid errors in the case where users only put in unusual 2-digit codes
  if(all(length(sourcevar.sub) == 0 & sourcevar %in% exempt.naics)) {

    sourcevar.sub <- "31"

  }

  # get the number of unique digits, excluding NAs
  digits <- unique(nchar(sourcevar.sub))
  digits <- digits[!is.na(digits)]

  # check whether input codes have the same digits
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits for inputs and outputs
  if (str_detect(origin, "SIC") & str_detect(destination, "NAICS")){

    origin.digits <- c(2, 3, 4)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 3, 4-digit inputs for SIC codes.")}

    destination.digits <- seq(2, 6, 1)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2 to 6-digit outputs for NAICS codes.")}

  } else if (str_detect(origin, "NAICS") & str_detect(destination, "SIC")) {

    origin.digits <- seq(2, 6, 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2 to 6-digit inputs for NAICS codes.")}

    destination.digits <- c(2, 3, 4)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 3, 4-digit outputs for SIC codes.")}

  } else {

    stop("Concordance not supported.")

  }

  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)

  # attach digit number to var
  origin.var <- paste(toupper(origin), "_", digits, "d", sep = "")
  destination.var <- paste(toupper(destination), "_", dest.digit, "d", sep = "")

  if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
  if (!destination.var %in% destination.codes){stop("Destination code not supported.")}

  # check if concordance is available for sourcevar
  all.origin.codes <- dictionary %>%
    pull(!!as.name(origin.var))

  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste("Matches for ", str_extract(origin.var, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

  }

  # match
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- dictionary[matches, c(origin.var, destination.var)]

  # calculate weights for matches
  dest.var <- dest.var %>%
    rename(!!origin := 1,
           !!destination := 2) %>%
    # if input is NA then match should be NA
    mutate(!!as.name(destination) := if_else(is.na(!!as.name(origin)), NA_character_, !!as.name(destination))) %>%
    group_by(!!as.name(origin), !!as.name(destination)) %>%
    mutate(n = length(!!as.name(destination)),
           n = ifelse(is.na(!!as.name(destination)), NA, n)) %>%
    distinct() %>%
    filter(!(is.na(n) & sum(!is.na(n)) > 0)) %>%
    group_by(!!as.name(origin)) %>%
    mutate(n_sum = sum(.data$n, na.rm = TRUE),
           weight = .data$n/.data$n_sum) %>%
    arrange(dplyr::desc(.data$weight)) %>%
    ungroup() %>%
    select(-n, -.data$n_sum) %>%
    rename(match = !!as.name(destination))

  # keep info on all matches and weights?
  if (all == TRUE){

    # merge matches/weights according to input
    out.merge <- nest_join(tibble(!!origin := sourcevar),
                           dest.var,
                           by = origin)

    names(out.merge$dest.var) <- sourcevar

    # fill NAs when there is no match
    out <- map(out.merge$dest.var, function(x){

      if(nrow(x) == 0){

        out.sub <- list(match = NA_character_,
                        weight = NA)

      } else {

        out.sub <- list(match = x$match,
                        weight = x$weight)

      }

    })

  } else {

    # keep match with largest weight
    # if multiple matches have the same weights, keep first match
    dest.var.sub <- dest.var %>%
      group_by(!!as.name(origin)) %>%
      slice(1) %>%
      ungroup() %>%
      select(-.data$weight)

    # handle repeated inputs
    out <- dest.var.sub[match(sourcevar, dest.var.sub %>% pull(!!as.name(origin))), "match"] %>%
      pull(match)

  }

  return(out)

}

