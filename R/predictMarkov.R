#' Predict next page based on all page views in the current session
#'
#' @param session_urls an array of all urls visited during the current session. In sequence.
#' @export
#' @import markovchain
predictNextPageMarkov <- function(session_urls){

  session_urls <- session_urls[!grepl("undefined", session_urls)]

  message("Predicting next page for ", session_urls)

  markovList <-  mcfL$estimate
  out <- try(predict(markovList, newdata = session_urls), silent = TRUE)

  if(inherits(out, "try-error")){

    ## try just with last page
    ll <- length(session_urls)
    retry_urls <- session_urls[ll]
    out <- try(predict(markovList, newdata = retry_urls), silent = TRUE)

    if(inherits(out, "try-error")){
      message("No prediction available")
      return(NULL)
    }
  }

  out
}

#' Predict next page based on all page views in the current session
#'
#' @param current_page The current page that the user is visiting. We will predict their next page by looking at the previous page data in GA
#' @export
#' @import
getNextPageFromPrevPage <- function(current_page)
{
  pages <- data[ga.data$previousPagePath == prevPage & ga.data$pagePath != prevPage, ]
  out <- pages[1,c("pagePath")]
  out
  #TODO also output the probability based on sum of pageviews
  #TODO handle NA output
}

#' Test function
#'
#' @param test_text
#' @export
#' @import
test <- function(test_text)
{
  test_text
}
