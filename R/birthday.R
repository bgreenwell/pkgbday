#' Determine an Installed Packages Birthday
#'
#' Determine the birthday of a particular installed package. The birthdate is
#' simply the package's very first appearance on CRAN.
#'
#' @param package Character string specifying the name of the package.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @return A character string representing the birthday (i.e.,
#' \code{"mm/dd/yyyy"}) of the package.
#'
#' @export
#'
#' @examples
#' pkg_bday("rpart")
pkg_bday <- function(package, ...) {
  # @param repos Character string(s) specifying the URL(s) of the repositories to
  # search. Default is \code{getOption("repos")}.
  # info <- pkg.info(package, repos)
  # if (is.null(info)) {
  #   NaN
  # } else {
  #   birthday <- min(info$mtime)
  #   strftime(birthday, format = "%m/%d/%Y")
  # }
  url <- paste0("https://cran.r-project.org/src/contrib/Archive/", package, "/")
  td <- tryCatch(rvest::html_nodes(xml2::read_html(url),
                                   css = "td:nth-child(3)"),
                 error = function(e) NULL)
  if (is.null(td)) {
    return(NA)
  }
  PATTERN <- "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]"
  dates <- na.omit(stringi::stri_extract_first_regex(td, pattern = PATTERN))
  as.character(min(strptime(dates, format = "%Y-%m-%d %H:%M")))
}


#' Determine the Age of An Installed Package
#'
#' Determine the age of a particular installed package. The age is simply the
#' time difference (using \code{difftime}) between today's date and the
#' package's very first appearance on CRAN.
#'
#' @param package Character string specifying the name of the package.
#'
#' @param units character string specifying the units (e.g., \code{"days"}). Can
#' be abbreviated.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{pkg_bday}.
#'
#' @return A \code{"difftime"} object representing the age of the package. Can
#' be coerced to a double using \code{as.double}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pkg_age("rpart")
#' cat("rpart is", as.double(pkg.age("rpart")) / 365, "years old today!\n")
#' }
pkg_age <- function(package, units = c("auto", "secs", "mins",
                                       "hours", "days", "weeks"), ...) {
  units <- match.arg(units)
  bday <- as.Date(pkg_bday(package, ...), format = "%Y-%m-%d %H:%M")
  difftime(Sys.time(), bday, units = units)
}


#' All Installed Package Birthdays
#'
#' List the birthdays of all your installed packages.
#'
#' @param decreasing Logical indicating whether or not to list the birthdays
#' in decreasing order (i.e., youngest to oldest). Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{pkg_bday}.
#'
#' @return A data frame with two columns: \code{package} and \code{birthday}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' all_bdays()
#' }
all_bdays <- function(decreasing = FALSE, ...) {
  pkgs <- unname(utils::installed.packages()[, "Package"])
  bdays <- plyr::laply(pkgs, .fun = pkg_bday, ..., .progress = "text")
  # closeAllConnections()
  bdays <- strptime(bdays, format = "%Y-%m-%d %H:%M")
  res <- stats::na.omit(data.frame("package" = pkgs, "birthday" = bdays,
                                   stringsAsFactors = FALSE))
  res[order(res$birthday, decreasing = decreasing), ]
}


#' All Installed Package Ages
#'
#' List the age of all your installed packages.
#'
#' @param units character string specifying the units (e.g., \code{"days"}). Can
#' be abbreviated.
#'
#' @param decreasing Logical indicating whether or not to list the ages
#' in decreasing order (i.e., youngest to oldest). Default is \code{FALSE}.
#'
#' @param ... Additional optional arguments to be passed on to
#' \code{pkg.age}.
#'
#' @return A data frame with two columns: \code{package} and \code{age}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' all_ages(units = "weeks", decreasing = TRUE)
#' }
all_ages <- function(units = c("auto", "secs", "mins", "hours", "days",
                               "weeks"),
                     decreasing = FALSE, ...) {
  units <- match.arg(units)
  pkgs <- unname(utils::installed.packages()[, "Package"])
  ages <- plyr::laply(pkgs, .fun = function(x, ...) as.double(pkg.age(x, ...)),
                      units = units, .progress = "text")
  # closeAllConnections()
  res <- stats::na.omit(data.frame("package" = pkgs, "age" = ages,
                                   stringsAsFactors = FALSE))
  res[order(res$age, decreasing = decreasing), ]
}


#' Packages with Upcoming Birthdays
#'
#' Determine which of your installed packages have an upcoming birthday.
#'
#' @param days Integer specifying the number of days out from the current date
#' to consider.
#'
#' @return A data frame with two columns: \code{package} and \code{birthday}
#'
#' @note Based on a modified version of the internal
#' \code{\link[devtools]{devtools}} function \code{package_find_repo}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' upcoming_bdays()
#' }
upcoming_bdays <- function(days = 30) {
  tod <- format(Sys.Date(), format = "%Y-%m-%d")
  fut <- format(Sys.Date() + days, format = "%Y-%m-%d")
  current.year <- format(Sys.time(), format = "%Y")
  res <- all_bdays()
  bdays <- paste0(current.year, "-", format(bdays$birthday, format = "%m-%d"))
  id <- which(bdays >= tod & bdays <= fut)
  res[id, ]
}
