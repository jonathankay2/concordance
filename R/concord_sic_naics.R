#' Converting 1987 SIC codes to 2002 NAICS
#'
#' Concords 1987 Standard Industrial Classification Codes to and from the 2002 North American Industry Classification System codes (NAICS2002). Primarily intended as 
#' a helper function for the more comprehensive SIC (2002)<->NAICS (all years) converter below.
#'
#' @param sourcevar An input character vector of SIC or NAICS codes. The function accepts 2, 3, 4-digit codes for SIC and 2 to 6-digit codes for NAICS.
#' @param origin A string setting the input industry classification: "SIC1987" or "NAICS2002".
#' @param destination A string setting the output industry classification: "SIC1987" or "NAICS2002".
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 3, or 4 digits for SIC and 2 to 6-digit codes for NAICS. The default is 6 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source SIC-NAICS concordance tables between 1987 and 2002 from the US Census <https://www.census.gov/naics/>.
#' @note Always include leading zeros in codes (e.g., use SIC code 01111 instead of 1111)---results may be buggy otherwise.
#' @examples
#' # One item, one-to-one match
#' concord_sic87_naics02(sourcevar = "0111",
#'                       origin = "SIC1987", destination = "NAICS2002",
#'                       dest.digit = 6, all = TRUE)
#'
#' concord_sic87_naics02(sourcevar = "3821",
#'                       origin = "SIC1987", destination = "NAICS2002",
#'                       dest.digit = 6, all = TRUE)
#' 
#' # One item, one-to-many match
#' concord_sic87_naics02(sourcevar = "0119",
#'                       origin = "SIC1987", destination = "NAICS2002",
#'                       dest.digit = 6, all = TRUE)
#' 
#' # Multiple items, multiple and overlapping matches
#' concord_sic87_naics02(sourcevar = c("2599", "3821", "3841"), 
#'                       origin = "SIC1987", destination = "NAICS2002",
#'                       dest.digit = 6, all = TRUE)
#'

concord_sic87_naics02 <- function(sourcevar,
                                  origin,
                                  destination,
                                  dest.digit = 6,
                                  all = FALSE) {
    # Load conversion dictionary
    dictionary <- concordance::sic87_naics02

    # subset and clean
    dictionary <- dictionary %>%
        rename(
            SIC1987_4d = .data$SIC_4d,
            SIC1987_3d = .data$SIC_3d,
            SIC1987_2d = .data$SIC_2d,
            NAICS2002_6d = .data$NAICS_6d,
            NAICS2002_5d = .data$NAICS_5d,
            NAICS2002_4d = .data$NAICS_4d,
            NAICS2002_3d = .data$NAICS_3d,
            NAICS2002_2d = .data$NAICS_2d
        ) %>%
        distinct()

    # sanity check
    if (length(sourcevar) == 0) {
        return(character(0))
    }

    # check whether input codes have the same digits
    # NAICS code has some unusual 2-digit codes, exclude them when counting digits
    exempt.naics <- c("31-33", "44-45", "48-49")
    sourcevar.sub <- sourcevar[!sourcevar %in% exempt.naics]

    # avoid errors in the case where users only put in unusual 2-digit codes
    if (all(length(sourcevar.sub) == 0 & sourcevar %in% exempt.naics)) {
        sourcevar.sub <- "31"
    }

    # get the number of unique digits, excluding NAs
    digits <- unique(nchar(sourcevar.sub))
    digits <- digits[!is.na(digits)]

    # check whether input codes have the same digits
    if (length(digits) > 1) {
        stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")
    }

    # set acceptable digits for inputs and outputs
    if (str_detect(origin, "SIC") & str_detect(destination, "NAICS")) {
        origin.digits <- c(2, 3, 4)

        if (!(digits %in% origin.digits)) {
            stop("'sourcevar' only accepts 2, 3, 4-digit inputs for SIC codes.")
        }

        destination.digits <- seq(2, 6, 1)

        if ((!dest.digit %in% destination.digits)) {
            stop("'dest.digit' only accepts 2 to 6-digit outputs for NAICS codes.")
        }
    } else if (str_detect(origin, "NAICS") & str_detect(destination, "SIC")) {
        origin.digits <- seq(2, 6, 1)

        if (!(digits %in% origin.digits)) {
            stop("'sourcevar' only accepts 2 to 6-digit inputs for NAICS codes.")
        }

        destination.digits <- c(2, 3, 4)

        if ((!dest.digit %in% destination.digits)) {
            stop("'dest.digit' only accepts 2, 3, 4-digit outputs for SIC codes.")
        }
    } else {
        stop("Concordance not supported.")
    }

    # get column names of dictionary
    origin.codes <- names(dictionary)
    destination.codes <- names(dictionary)

    # attach digit number to var
    origin.var <- paste(toupper(origin), "_", digits, "d", sep = "")
    destination.var <- paste(toupper(destination), "_", dest.digit, "d", sep = "")

    if (!origin.var %in% origin.codes) {
        stop("Origin code not supported.")
    }
    if (!destination.var %in% destination.codes) {
        stop("Destination code not supported.")
    }

    # check if concordance is available for sourcevar
    all.origin.codes <- dictionary %>%
        pull(!!as.name(origin.var))

    if (!all(sourcevar %in% all.origin.codes)) {
        no.code <- sourcevar[!sourcevar %in% all.origin.codes]
        no.code <- paste0(no.code, collapse = ", ")

        warning(paste("Matches for ", str_extract(origin.var, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))
    }

    # match
    matches <- which(all.origin.codes %in% sourcevar)
    dest.var <- dictionary[matches, c(origin.var, destination.var)]

    # calculate weights for matches
    dest.var <- dest.var %>%
        rename(
            !!origin := 1,
            !!destination := 2
        ) %>%
        # if input is NA then match should be NA
        mutate(!!as.name(destination) := if_else(is.na(!!as.name(origin)), NA_character_, !!as.name(destination))) %>%
        group_by(!!as.name(origin), !!as.name(destination)) %>%
        mutate(
            n = length(!!as.name(destination)),
            n = ifelse(is.na(!!as.name(destination)), NA, n)
        ) %>%
        distinct() %>%
        filter(!(is.na(n) & sum(!is.na(n)) > 0)) %>%
        group_by(!!as.name(origin)) %>%
        mutate(
            n_sum = sum(.data$n, na.rm = TRUE),
            weight = .data$n / .data$n_sum
        ) %>%
        arrange(dplyr::desc(.data$weight)) %>%
        ungroup() %>%
        select(-n, -.data$n_sum) %>%
        rename(match = !!as.name(destination))

    # keep info on all matches and weights?
    if (all == TRUE) {
        # merge matches/weights according to input
        out.merge <- nest_join(tibble(!!origin := sourcevar),
            dest.var,
            by = origin
        )

        names(out.merge$dest.var) <- sourcevar

        # fill NAs when there is no match
        out <- map(out.merge$dest.var, function(x) {
            if (nrow(x) == 0) {
                out.sub <- list(
                    match = NA_character_,
                    weight = NA
                )
            } else {
                out.sub <- list(
                    match = x$match,
                    weight = x$weight
                )
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

#' Converting 1987 SIC and NAICS Codes
#'
#' Concords Standard Industrial Classification Codes (1987SIC) to and from North American Industry Classification System codes (NAICS1997, NAICS2002, NAICS2007, NAICS2012, NAICS2017) by
#' first converting from 1987 SIC to 2002 NAICS and then from 2002 NAICS to other NAICS editions as needed, aggregating weights as appropriate.
#'
#' @param sourcevar An input character vector of SIC or NAICS codes. The function accepts 2, 3, 4-digit codes for SIC and 2 to 6-digit codes for NAICS.
#' @param origin A string setting the input industry classification: "SIC1987", "NAICS1997", "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017".
#' @param destination A string setting the output industry classification: "SIC1987", "SIC", "NAICS1997", "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 3, or 4 digits for SIC and 2 to 6-digit codes for NAICS. The default is 6 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source SIC-NAICS concordance tables between 1987 and 2002 from the US Census <https://www.census.gov/naics/>.
#' @note Always include leading zeros in codes (e.g., use SIC code 01111 instead of 1111)---results may be buggy otherwise.
#' @examples
#' # One input: one-to-one match
#' concord_sic_naics(sourcevar = "0111",
#'                   origin = "SIC1987", destination = "NAICS2002",
#'                   dest.digit = 6, all = TRUE)
#' 
#' # One input: one-to-many match
#' concord_sic_naics(sourcevar = "3821",
#'                   origin = "SIC1987", destination = "NAICS2007",
#'                   dest.digit = 6, all = TRUE)
#'
#' concord_sic_naics(sourcevar = "0119",
#'                   origin = "SIC1987", destination = "NAICS2007",
#'                   dest.digit = 6, all = TRUE)
#'
#' # One input: one-to-many match, weighting appropriately among family categories
#' concord_sic_naics(sourcevar = "3821",
#'                   origin = "SIC1987", destination = "NAICS2007",
#'                   dest.digit = 4, all = TRUE)
#'
#' # An instructive example for understanding how weighting works with multiple splits and overlapping codes:
#' concord_sic_naics(sourcevar = "2599",
#'                   origin = "SIC1987", destination = "NAICS2007",
#'                   dest.digit = 6, all = TRUE)
#' # This is equivalent to first converting from SIC1987 to NAICS2002 and then from NAICS2002 to NAICS2007.
#' # The first hop produces two NAICS codes, 337127 and 339111, each given equal weight.
#' # concord_sic87_naics02(sourcevar = "2599",
#' #                       origin = "SIC1987", destination = "NAICS2002",
#' #                       dest.digit = 6, all = TRUE)
#' # In the second hop, 337127 is "converted" one-to-one to the same code, 337127. However, 339111 gets converted to seven different codes. 
#' # concord_naics(sourcevar = c("337127", "339111"),
#' #               origin = "NAICS2002", destination = "NAICS2007",
#' #               dest.digit = 6, all = TRUE)
#' # One of these seven codes is 337127. So the final weight for 337127 in the SIC1987->NAICS2007 conversion is 0.5*(1/1)+0.5*(1/7) = .571.
#' 
#' # We can also convert the other way, from NAICS2007 to SIC1987.
#' concord_sic_naics(sourcevar = "337127",
#'                   origin = "NAICS2007", destination = "SIC1987",
#'                   dest.digit = 4, all = TRUE)

concord_sic_naics <- function(sourcevar,
                              origin,
                              destination,
                              dest.digit = 6,
                              all = FALSE) {

    origin <- toupper(origin)
    destination <- toupper(destination)

    # sanity check
    if (length(sourcevar) == 0) {return(character(0))}

    # If we're converting between SIC 1987 and NAICS 2002, can use the standard dictionary-based conversion. Otherwise, need to take the multi-stage approach.
    if ((origin == "SIC1987" && destination == "NAICS2002") ||
        (origin == "NAICS2002" && destination == "SIC1987")) {
            out <- concord_sic87_naics02(sourcevar, origin, destination, dest.digit, all)
            return(out)
    } else if (origin == "SIC1987" && grepl("^NAICS", destination) ||
               grepl("^NAICS", origin) && destination == "SIC1987") {
        # Check whether we're converting to or from SIC.
        if(origin == "SIC1987"){
            # 1) First hop: convert from SIC 1987 to NAICS 2002 (at 6-digit level for fidelity and including all matches).
            hop1 <- concord_sic87_naics02(sourcevar, "SIC1987", "NAICS2002", dest.digit = 6, all = TRUE)

            # Collect all mid-codes we need to expand in hop 2
            mids_all <- unique(unlist(lapply(hop1, `[[`, "match")))
            mids_all <- mids_all[!is.na(mids_all)]

            # 2) Second hop: Convert our mid-codes from 2002 NAICS to the destination NAICS using the existing NAICS converter. 
            # Again, stick to the 6-digit level for fidelity and include all matches.
            hop2 <- concord_naics(mids_all, "NAICS2002", destination, 6, TRUE)
        } else {
            hop1 <- concord_naics(sourcevar, origin, "NAICS2002", dest.digit = 6, all = TRUE)
            mids_all <- unique(unlist(lapply(hop1, `[[`, "match")))
            mids_all <- mids_all[!is.na(mids_all)]
            
            hop2 <- concord_sic87_naics02(mids_all, "NAICS2002", "SIC1987", dest.digit = 4, all = TRUE) # 4 digits is most granular for SIC.
        }

        # 3) For each origin code, multiply the two stages of weights and sum duplicates.
        out_list <- map(sourcevar, function(src) {
          # Get hop1 matches and handle NA/null cases
          h1 <- hop1[[src]]
          if (is.null(h1) || is.null(h1$match) || all(is.na(h1$match))) {
            return(list(match = NA_character_, weight = NA_real_))
          }
          
          # Create dataframe of all hop1 matches and weights
          h1_df <- tibble(mid_code = h1$match, w1 = h1$weight) %>%
            filter(!is.na(mid_code), !is.na(w1))
          # 
          if (nrow(h1_df) == 0) {
            return(list(match = NA_character_, weight = NA_real_))
          }
          
          # Get all hop2 matches and weights
          results_df <- h1_df %>%
            # Join with hop2 results
            mutate(h2 = map(mid_code, ~hop2[[.x]])) %>%
            # Unnest the hop2 results
            unnest_wider(h2) %>%
            # Unnest the match and weight vectors
            unnest(c(match, weight)) %>%
            # Calculate combined weights
            mutate(
              wprod = w1 * weight,
              dcode = match
            ) %>%
            # Group by destination code and sum weights
            group_by(dcode) %>%
            summarise(weight = sum(wprod)) %>%
            # Normalize weights
            mutate(weight = weight / sum(weight))
          
          if (nrow(results_df) == 0) {
            return(list(match = NA_character_, weight = NA_real_))
          }
          
          # Handle digit truncation if needed
          if (dest.digit < 6) {
            results_df <- results_df %>%
              mutate(dcode = substr(as.character(dcode), 1, dest.digit)) %>%
              group_by(dcode) %>%
              summarise(weight = sum(weight)) %>%
              mutate(weight = weight / sum(weight))
          }
          
          list(match = results_df$dcode, weight = results_df$weight)
        })
        
        names(out_list) <- sourcevar

        # 5) If all = TRUE, return all matches; otherwise, pick the best match by highest weight, breaking ties by lexical order.
        if (all) {
            return(out_list)
        } else {
            best <- vapply(out_list, function(el) {
                if (is.null(el$match) || all(is.na(el$match))) {
                    return(NA_character_)
                }
                ord <- order(-el$weight, el$match)
                el$match[ord][1]
            }, FUN.VALUE = character(1))
            names(best) <- NULL
            return(best)
        }
    }
}
