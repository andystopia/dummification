# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'



#' Create Dummy Variables From Substrings
#'
#' Extract substrings from a given column, creating
#' dummy variables in the format of colName.DUMMY_NAME,
#' where DUMMY_NAME is the corresponding value from
#' (@param dummyVarNames), for a given option.
#'
#' Provide this function a dataframe to work with,
#' a column name, the substrings that you would like extract from
#' the column, and a list that corresponds to what those substring's
#' should map to in the dummy variables.
#'
#' Strips away punctation _around_ substrings, and provides an other column back out.
#'
#' Note: This package matches by a pattern of "longest-first",
#' so "anteater" is more important to match than "ant", but if "anteater"
#' is unavailable, then "ant" is matched.
#'
#' @param column the column of variables to dummify
#' @param prefix the prefix of the dummy variable columns
#' @param options the substrings that we would like to extract.
#' @param dummyVarNames the dummy variable names (should correspond with options)
#'
#' @return a dataframe containing the column split out
#'
#' @examples
#'
#' df <- data.frame(
#'  a = c("ant", "anteater", "bear", "no"),
#'  b = c("anteater", "ant", "hi", "bear")
#' )
#'
#' df |> mutate(substr_dummify(a, "animal", b, b))
#'
#' @export
substr_dummify <- function(column, prefix, options, dummyVarNames, otherCol="OTHER") {
  ordering <- order(-nchar(options))
  options <- options[ordering]
  dummyVarNames <- dummyVarNames[ordering]
  df <- data.frame(matrix(vector(), nrow = length(column), ncol = 0))

  purrr::map2(options, dummyVarNames, \(option, dummy) {
    df[[glue::glue("{prefix}.{dummy}")]] <<- grepl(option, column, fixed=T)
    column <<- column |> map(\(item) stringr::str_remove_all(item, fixed(option)))
  });

  other_col <- column |> purrr::map(\(col) {
    v <- rlang::duplicate(col)
    options |> purrr::map(\(opt) {
      v <<- stringr::str_remove_all(v, stringr::fixed(opt))
    })
    # remove whitespace and punctuation
    stringr::str_remove_all(v, stringr::regex("((^[\\.\\,\\s]+)|([\\.\\,\\s]+$))"))
  });
  df[[glue::glue("{prefix}.{otherCol}")]] <- other_col
  df
}
