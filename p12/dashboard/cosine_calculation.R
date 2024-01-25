tweets <- fread("../tweets.csv")
users <- fread("../users.csv")
pdf <-
    pdftools::pdf_text(pdf = "https://democrats-intelligence.house.gov/uploadedfiles/exhibit_b.pdf")
bots <-
    map(pdf, \(line) str_split_1(line, "[\n| ]") |> discard(\(x) x == "")) |> unlist()
bots_ids <- bots[seq(2, length(bots), 2)]


users$id <- as.character(users$id)
users$bot <- F
users$bot[users$id %in% bots_ids] <- T
users$followers_count_log <- log(users$followers_count)
users$followers_count_log[is.na(users$followers_count_log)] <- 1
users$verified[users$verified == ""] <- "Verified"
users$verified[users$verified == "false"] <- "Not verified"
random_rows <- sample(nrow(tweets), 5, replace = T)
random_users <- sample(nrow(users), 5, replace = T)
striped_hashtags <- strip_hashtags(tweets$hashtags)
all_hashtags <- striped_hashtags |> unlist() |>
    discard(\(x) x == "") |>
    table() |>
    sort(decreasing = T) |>
    data.frame()
colnames(all_hashtags) <- c("Hashtag", "Frequency")
popular_hashtags <- all_hashtags$Hashtag[1:50]

for (hashtag in popular_hashtags) {
    values <- map_int(striped_hashtags, \(x) hashtag %in% x)
    tweets[, (as.character(hashtag)) := values]
}

columns_needed <- c("user_id", as.character(popular_hashtags))
user_hashtags <- copy(tweets[!is.na(user_id), ..columns_needed])
user_hashtags <-
    user_hashtags[, lapply(.SD, sum, na.rm = TRUE), by = user_id]

user_connections <-
    data.frame(t(combn(unique(users[!is.na(id)]$id), 2)))
colnames(user_connections) <- c("from", "to")

cosine_sim <- pmap_dbl(user_connections, \(from, to){
    lsa::cosine(t(as.matrix(user_hashtags[user_id %in% c(from, to),
                                          2:(length(popular_hashtags) +1)])))[2]
})
cosine_sim[is.na(cosine_sim)] <- 0

user_connections$weight <- cosine_sim

write.csv(user_connections, "../cosine_similiarity.csv", row.names = F)
