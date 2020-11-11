#'
#' @import tidyverse
#' @import here
#' @import hrbrthemes
#' @import extrafont
#' @import twitteR
#'
#'
#'
#'
#'


user_tweets <- function(user, n) {
  assign(paste(user, "tweets"), map_df(userTimeline(user, n), tibble), envir = .GlobalEnv)
}


search_hashtag <- function(hashtag, n) {
  assign(paste(hashtag, "tweets"), map_df(searchTwitter(hashtag, n), tibble))
}




