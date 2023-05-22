

# using: https://www.scrapingdog.com/blog/web-scraping-r/
# link <- "https://www.mil.gov.ua/en/news/2023/03/08/the-total-combat-losses-of-the-enemy-from-24-02-2022-to-08-03-2023/"
# page = read_html(link, as_html = FALSE)
# lines <- read_html(link)

library(tidyverse)
library(glue)
library(xml2)

#' Extract the casualty count from a line in the report based on a regex search
#'
#'
#'
#' @param text The text of the web page that contains the casualty report.
#' @param phrase The regex that will find the casualty number.
#'
#' @return The casualty number.
#' @export
#'
#' @examples
#' x <- str_extract(text, "(?<=personnel [‒-] about )\\d*")
extract_number  <-  function(text, phrase = "(?<=personnel [‒-] about )\\d*") {
  x <- str_extract(text, phrase)
  if (is.na(x)) {
    if (!str_detect(text, phrase)) stop(paste0(phrase, " not found"))
    warning(paste0("number not found:", text()))
    return(NA_real_)
  }
  as.numeric(x)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Notes on scraping and parsing the data on Russian losses from the
# Ukraine Ministry of Defense web site:
#
# April 12, 2022 is earliest Russian casualty count
#
# Some dates that are missing data: "2022-06-16", "2022-06-18" (others in June)
#
# at this point doing two digit date for start of war and end
# https://www.mil.gov.ua/en/news/2023/02/23/the-total-combat-losses-of-the-enemy-from-24-02-22-to-23-02-23/
#
#   back then two digit years
# https://www.mil.gov.ua/en/news/2022/12/16/the-total-combat-losses-of-the-enemy-from-24-02-to-16-12/
#
#   https://www.mil.gov.ua/en/news/2022/12/31/the-total-combat-losses-of-the-enemy-from-24-02-to-31-12/
#
#   3/3/2023 seems to be start of using 4-digit year
#
# wrong:
#   https://www.mil.gov.ua/en/news/2022/09/23/the-total-combat-losses-of-the-enemy-from-24-02-to-23-09/
#
#   On January 1, started doing two digit dates
# https://www.mil.gov.ua/en/news/2023/01/03/the-total-combat-losses-of-the-enemy-from-24-02-22-to-03-01-23/
#
#   works
# https://www.mil.gov.ua/en/news/2023/02/20/the-total-combat-losses-of-the-enemy-from-24-02-22-to-20-02-23/
#
#   does not work:
#   https://www.mil.gov.ua/en/news/23/02/23/the-total-combat-losses-of-the-enemy-from-24-02-22-to-23-02-23/
#
#   does note work:
#   https://www.mil.gov.ua/en/news/23/02/20/the-total-combat-losses-of-the-enemy-from-24-02-22-to-20-02-23/
#
#   works:
#   https://www.mil.gov.ua/en/news/2022/05/09/the-total-combat-losses-of-the-enemy-from-24-02-to-09-05/
#
#   works:
#   https://www.mil.gov.ua/en/news/2022/06/25/the-total-combat-losses-of-the-enemy-from-24-02-to-25-06/
#
#   works:
#   https://www.mil.gov.ua/en/news/2022/04/16/the-total-combat-losses-of-the-enemy-from-24-02-to-16-04/
#
#   first phrase changed between 2022-04-16 and 2022-04-17
# "personnel ‒ about" to
#
#
# wrong:
#   https://www.mil.gov.ua/en/news/2022/07/12/the-total-combat-losses-of-the-enemy-from-24-02-to-12-07/
#
#   wrong:
#   https://www.mil.gov.ua/en/news/2022/06/20/the-total-combat-losses-of-the-enemy-from-24-02-to-20-06/
#
#   More than one line found 2022-06-06 and that goes until 2022-07-26 when hits error
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


#' Create a URL that goes to a standard Russian casualty report
#'
#' @param adate The Date for the report.
#'
#' @return String with the URL for the casualty report.
#' @export
#'
#' @examples
create_ukr_mod_link <- function(adate) {
  if (!is.Date(adate)) adate <- as_date(adate)
  # must have this form:   (4 digit years)
  # https://www.mil.gov.ua/en/news/2023/03/08/the-total-combat-losses-of-the-enemy-from-24-02-2022-to-08-03-2023/
  if (adate == as_date("2023-03-30")) link <-  "https://www.mil.gov.ua/en/news/2023/03/30/blizko-173-tis-osib-znishheno-ponad-6970-bojovih-bronovanih-mashin-voroga-–-genshtab-zsu/"
  else if (adate >= as_date("2023-03-03")) link <- glue("https://www.mil.gov.ua/en/news/", format(adate, "%Y/%m/%d"), "/the-total-combat-losses-of-the-enemy-from-24-02-2022-to-", format(adate, "%d-%m-%Y"), "/")
  else if (adate >= as_date("2023-01-01")) link <- glue("https://www.mil.gov.ua/en/news/", format(adate, "%Y/%m/%d"), "/the-total-combat-losses-of-the-enemy-from-24-02-22-to-", format(adate, "%d-%m-%y"), "/")
  else link <- glue("https://www.mil.gov.ua/en/news/", format(adate, "%Y/%m/%d"), "/the-total-combat-losses-of-the-enemy-from-24-02-to-", format(adate, "%d-%m"), "/")
  link
}

#' Extract the lines from a Ukraine Ministry of Defense page that contain the Russian casualty counts
#'
#' @param adate Date of casualty report on Ukraine Ministry of Defense web site
#'
#' @return List of text lines that contain the casualty reports, each category on different line.
#' @export
#'
#' @examples
#' fetch_mod_text("2023-03-29")

fetch_ukr_mod_text <- function(adate) {
  if (!is.Date(adate)) adate <- as_date(adate)
  if (adate < ymd("2022-04-12")) return(NA_character_)
  if (adate > today()) return(NA_character_)

  link <- create_mod_link(adate)
  x <- read_lines(link)
  #the_line <- x[x |> str_detect("personnel ‒ about")]
  # the_line <- x[x |> str_detect("The total combat losses of the enemy from")]
  if (adate <= ymd("2022-04-16")) the_line <- x[str_detect(x, "personnel ‒ about")]
  if (adate > ymd("2022-04-16")) the_line <- x[str_detect(x, "persons were liquidated")]
  if (length(the_line) == 0) {
    warning(paste0("Info line not found for ", adate))
    return(NA_character_)
  }
  # 2023-04-23 not read because text of report appears twice on the page
  if (length(the_line) > 1) {
    warning(paste0("More than one line found ", adate, "\n", link))
    return(NA_character_)
  }
  if (adate <= as_date("2022-04-16")) first_line <- which(str_detect(x, "personnel ‒ about"))
  if (adate > as_date("2022-04-16"))  first_line <- which(str_detect(x, "persons were liquidated"))
  last_line <- which(str_detect(x, "special equipment"))
  if (is.na(last_line) || (length(last_line) == 0)) last_line <- which(str_detect(x, "vehicles"))
  if (is.na(last_line) || (length(last_line) == 0)) last_line <- which(str_detect(x, "UAV"))
  if (is.na(last_line) || (length(last_line) == 0)) last_line <- first_line + 10
  # the_line
  str_flatten(c(as.character(adate), x[first_line:last_line]))
}

#' Parse the Russian casualty info from Ukraine MOD summary pages.
#'
#' The text from each web page is in the report column. The extract_number
#' function is used to find the casualties for each item, based on a regular expression
#' to search for that type of item.
#'
#' @param mod_df A df which has a report column that contains the text from casualty web pages.
#'
#' @return The same df is returned, but with numeric columns added for each type of casualty.
#' @export
#'
#' @examples
parse_ukr_mod_text <- function(mod_df) {
  mod_df$personnel = map_dbl(str_replace_all(mod_df$report, intToUtf8(160), ""), extract_number, phrase = "(?<=personnel ?[‒-–-] ? ?about )\\d*" )
  mod_df$tanks = map_dbl(mod_df$report, extract_number, phrase = "(?<=tanks [‒-–-] )\\d*" )
  mod_df$apv = map_dbl(mod_df$report, extract_number, phrase = "(?<=APV [‒-–-] )\\d*" )
  mod_df$artillery = map_dbl(mod_df$report, extract_number, phrase = "(?<=artillery systems [‒-–-] )\\d*" )
  mod_df$mlrs = map_dbl(mod_df$report, extract_number, phrase = "(?<=MLRS [‒-–-] )\\d*" )
  mod_df$aa = map_dbl(mod_df$report, extract_number, phrase = "(?<=Anti-aircraft warfare systems [‒-–-] )\\d*" )
  mod_df$aircraft = map_dbl(mod_df$report, extract_number, phrase = "(?<=aircraft [‒-–-] )\\d*" )
  mod_df$helicopters = map_dbl(mod_df$report, extract_number, phrase = "(?<=helicopters [‒-–-] )\\d*" )
  mod_df$uav = map_dbl(mod_df$report, extract_number, phrase = "(?<=UAV operational-tactical level [‒-–-] )\\d*" )
  mod_df$vehicles = map_dbl(mod_df$report, extract_number, phrase = "(?<=vehicles and fuel tanks [‒-–-] )\\d*" )
  # mod_df$special = map_dbl(mod_df$report, extract_number, phrase = "(?<=special equipment [‒-–-] )\\d*" )
  mod_df$warships = map_dbl(mod_df$report, extract_number, phrase = "(?<=warships / boats [‒-–-] )\\d*" )
  mod_df
}

#' Update the df that contains Ukraine MOD Russian casualty stats
#'
#' @param fname_ukr_mod_df file name of RData file that contains the stats
#'
#' @return Returns the updated df but also saves it to an RData file as a side effect.
#' @export
#'
#' @examples
#' update_ukr_mod_df("ukr_mod_df.RData", save_fname_ukr_mod_df = "test.RData")
#'
update_ukr_mod_df <- function(fname_ukr_mod_df, from_date = NULL, to_date = NULL,
                              days_previous = 20,
                              save_fname_ukr_mod_df = NULL) {
  load(fname_ukr_mod_df)



  ############################################################################
  # Here's the orginal sequence of dates that was used to initialize ukr_mod_df.
  # the_dates <- seq(from = ymd("2022-04-15"), to = today(), by = "1 day")
  ############################################################################

  # dates that need to be added to ukr_mod_df
  if (is.null(from_date)) from_date <-  today() - days_previous
  if (is.null(to_date)) to_date <- today()

  additional_dates <- seq(from = from_date, to = to_date, by = "1 day")
  abunch <- map_chr(additional_dates, fetch_ukr_mod_text, .progress = TRUE)
  abunch <- abunch[!is.na(abunch)]

  # ukr_mod_df <- tibble(report = abunch, date = ymd(str_sub(report, start = 1, end = 10)))
  if (length(abunch > 0)) {
    ukr_mod_page_additions <- tibble(report = abunch, date = ymd(str_sub(report, start = 1, end = 10)))
    ukr_mod_additions <- parse_mod_text(ukr_mod_page_additions)
  }

  # use overlap_dates to check whether MOD has updated recent daata
  # overlap_dates <- bind_rows(ukr_mod_df |> filter((ukr_mod_df$date %in% ukr_mod_additions$date)),
  #                            ukr_mod_additions |> filter(ukr_mod_additions$date %in% ukr_mod_df$date)) |>
  #   arrange(date) # |> View()

  # save_ukr_mod__df <- ukr_mod_df
  ukr_mod_df <- bind_rows(ukr_mod_df |> filter(!(ukr_mod_df$date %in% ukr_mod_additions$date)), ukr_mod_additions)

  # calculate the number of days between each row
  ukr_mod_df <- ukr_mod_df |> mutate(gap = as.numeric(date - lag(date, default = ukr_mod_df$date[1] - 1)))

  # by default, save it back into the save file that it was loaded from
  if (is.null(save_fname_ukr_mod_df)) save_fname_ukr_mod_df <- fname_ukr_mod_df
  save(ukr_mod_df, file = save_fname_ukr_mod_df)
  return(ukr_mod_df)
}
