library(data.table)
library(stringr)
library(purrr)
library(igraph)
library(echarts4r)

tweets <- read.csv("p12/tweets.csv") |> as.data.table()
users <- read.csv("p12/users.csv") |> as.data.table()

#random tweet

random_row <- sample(nrow(tweets), 1, replace = T)


#data exploration:
# - example tweet and user
# - node distribution
# - graph presentation: centrality
# hashtags distiribution

# clustering:
# - using retweets connectino
# - xgboost with all data
# comparison

retweets <- tweets[!is.na(retweeted_status_id)]

retweets_connections <- merge(
    unique(tweets[, .(user_id, tweet_id)]),
    unique(retweets[, .(user_id, retweeted_status_id)]),
    by.x = "tweet_id",
    by.y = "retweeted_status_id",
    suffixes = c(".tweet", ".retweet")
)[!is.na(user_id.retweet) &
      !is.na(user_id.tweet)][, n.interactions := .N , by = .(user_id.tweet, user_id.retweet)]
setnames(retweets_connections, "user_id.tweet", "to")
setnames(retweets_connections, "user_id.retweet", "from")
retweets_connections$to <- as.character(retweets_connections$to)
retweets_connections$from <- as.character(retweets_connections$from)
users$id <- as.character(users$id)
users$followers_count_log <- log(users$followers_count)
users$followers_count_log[is.na(users$followers_count_log)] <- 1

g <- graph_from_data_frame(retweets_connections)
e_charts() |>
    e_graph_gl() |>
    e_graph_nodes(users, id, followers_count_log, followers_count_log, verified) |>
    e_graph_edges(retweets_connections, from, to, n.interactions)
degs <- data.frame(names(degree(g)), degree(g))
colnames(degs) <- c("id", "degree")
degs |>
    e_charts() |>
    e_histogram(degree, name = "histogram", breaks = 100) |>
    e_density(degree, areaStyle = list(opacity = .4), smooth = TRUE, name = "density", y_index = 1) |>
    e_tooltip(trigger = "axis")

hashtags <- tweets$hashtags |>
    str_replace_all("\\[", "") |>
    str_replace_all("\\]", "") |>
    str_replace_all('"', "") |> str_split(pattern = ",")

unique_hashtags <-
    hashtags |> unlist() |> discard(\(x) x == "") |> table()

pdf <- pdftools::pdf_text(pdf = "https://democrats-intelligence.house.gov/uploadedfiles/exhibit_b.pdf")
bots <- map(pdf, \(line) str_split_1(line, "[\n| ]") |> discard(\(x) x == "")) |> unlist()
bots_ids <- bots[seq(2, length(bots), 2)]
