#'
#' @import tidyverse
#' @import here
#' @import hrbrthemes
#' @import extrafont
#' @import rtweet
#' @import twitteR
#' @import wordcloud2
#' @import qdapRegex
#' @import tm
#' @import emo
#' @import ggbeeswarm
#' @import plotly
#'
#' @title Twitter User Data
#'
#' @param user User's handle
#' @param n Number of tweets to retrieve
#'
#' @return
#' @export
#'
#' @examples
#' user_tweets("realDonaldTrump", 3200)
user_tweets <- function(user, n) {
  assign(paste0(user, "tweets"), get_timelines(user, n), envir = .GlobalEnv)
}

#' @title Twitter Hashtag Data
#'
#' @param hashtag Hashtag to search
#' @param n Number of tweets to retrieve
#'
#' @return
#' @export
#'
#' @examples
#' search_hashtag("data science", 3200)
search_hashtag <- function(hashtag, n) {
  assign(paste0(hashtag, "tweets"), search_tweets(paste0("#", hashtag), n), envir = .GlobalEnv)
}

#' @title User Wordcloud Generator
#'
#' @param username Twitter handle
#'
#' @return
#' @export
#'
#' @examples
#' tweets_to_wordcloud("gates_duncan")
tweets_to_wordcloud <- function(username){

  tweets <- get_timelines(username, n = 3200)

  # Clean the data
  text <- str_c(tweets$text, collapse = "") %>%
    str_remove("\\n") %>%                   # remove linebreaks
    rm_twitter_url() %>%                    # Remove URLS
    rm_url() %>%
    str_remove_all("#\\S+") %>%             # Remove any hashtags
    str_remove_all("@\\S+") %>%             # Remove any @ mentions
    removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
    removeNumbers() %>%
    stripWhitespace() %>%
    removeWords(c("amp"))                   # Final cleanup of other small changes

  # Convert the data into a summary table
  textCorpus <-
    Corpus(VectorSource(text)) %>%
    TermDocumentMatrix() %>%
    as.matrix()

  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

  wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
  return(wordcloud)
}

#' @title User Tweet Number Chart
#'
#' @param username Twitter handle
#'
#' @return
#' @export
#'
#' @examples
#' number_tweets("realDonaldTrump")
number_tweets <- function(username) {
  get_timelines(username, n = 3200) %>% count(week = lubridate::floor_date(created_at, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line() +
  labs(title = paste0("@", username, " tweets per week, 2020"),
       caption = "source: Twitter Analytics, retrieved 2020-11-12", y = "Number of Tweets", x = "Week") +
  hrbrthemes::theme_ipsum_ps()
}

#' @title User Tweet Length Chart
#'
#' @param username Twitter handle
#'
#' @return
#' @export
#'
#' @examples
#' tweet_length("realDonaldTrump")
tweet_length <- function(username) {
  get_timelines(username, n = 3200) %>%
    mutate(`Tweet Length` = nchar(text)) %>%
    ggplot(aes(lubridate::make_date(created_at), `Tweet Length`)) +
    geom_hline(yintercept = 140, linetype = "dotted", size = 0.25, color = "#2b2b2b") + # this far and no further
    geom_quasirandom(size = 1, shape = 21, color = "slategray", stroke = 0.1, groupOnX = TRUE) +
    labs(x = NULL, title = "Tweet Length Distribution") +
    theme_ipsum_rc(grid = "Y")
}

#' @title Emoji Usage
#'
#' @param username Twitter handle
#'
#' @return
#' @export
#'
#' @examples
#' tweet_emojis("realDonaldTrump")
tweet_emojis <- function(username) {
  get_timelines(username, n = 3200) %>%
    group_by(created_at) %>%
    mutate(emojis = ji_count(text)) %>%
    summarise(emojis = sum(emojis)) %>%
    ggplot(aes(created_at, emojis)) +
    geom_smooth(method = "loess", formula = y ~ x) +
    geom_point() +
    labs(title = paste0("@", username, "daily emoji count 2020")) +
    theme_ipsum_ps()
}

#' @title Emoji Chart Plot
#'
#' @param username Twitter handle
#'
#' @return
#' @export
#'
#' @examples
#' tweet_emoji_chart("realDonaldTrump")
tweet_emoji_chart <- function(username) {
      tweet_emoji <- get_timelines(username, n = 3200) %>%
      pull(text) %>%
      str_extract_all(emo::ji_rx) %>%
      flatten_chr() %>%
      table() %>%
      enframe(name = "emoji") %>%
      left_join( emo::jis, by = "emoji" ) %>%
      mutate(code = tolower(runes)) %>%
      mutate(emoji_name = gsub(" ", "-", name)) %>%
      arrange( desc(value) )

    # emoji as factor
    tweet_emoji$name <- factor(tweet_emoji$name, levels = tweet_emoji$name[order(-tweet_emoji$value)])
    tweet_emoji <- tweet_emoji %>%
      mutate(emoji_image = paste(emoji_name, "_", code, ".png", sep = ""))
    tweet_emoji <- tweet_emoji %>%
      mutate(emoji_url = paste('https://emojipedia-us.s3.amazonaws.com/thumbs/240/apple/118/', emoji_image, sep = ""))


    df.plot <- top_n(tweet_emoji, 15, value)
    df.plot %>%
      plotly::plot_ly(y = df.plot$value, x = df.plot$name) %>%
      plotly::add_text(
        text = ~emoji,
        hovertext = ~name,
        hoverinfo = "text",
        size = I(20))
}


