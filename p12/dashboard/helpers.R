strip_hashtags <- function(hashtags) {
    hashtags |>
        str_replace_all("\\[", "") |>
        str_replace_all("\\]", "") |>
        str_replace_all('"', "") |> str_split(pattern = ",")
}

example_tweets <- function(tweets, indexes) {
    fluid_rows <- c()
    for (random_row in indexes) {
        tweet <- tweets[random_row,]
        fluid_rows <- c(fluid_rows, fluidRow(
            box(
                title = tweet$user_key,
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                tweet$text,
                HTML("<br/><b>", paste0(
                    "#", unlist(strip_hashtags(tweet$hashtags)), collapse = " "
                ), "</b>")
            ),
        ))
    }
    return(fluid_rows[!names(fluid_rows) %in% c("name", "attribs")])
    
}

example_users <- function(users, indexes) {
    fluid_rows <- c()
    for (random_row in indexes) {
        user <- users[random_row,]
        fluid_rows <- c(fluid_rows, fluidRow(
            box(
                title = user$name,
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                user$description,
                shiny::br(),
                "Location: ",
                user$location,
                shiny::br(),
                "Followers: ",
                user$followers_count,
                shiny::br(),
                "Posts: ",
                user$statuses_count,
                shiny::br(),
                "Friends: ",
                user$friends_count,
                shiny::br()
            ),
        ))
    }
    return(fluid_rows[!names(fluid_rows) %in% c("name", "attribs")])
    
}

create_plotly_graph <- function(G, colors_) {

    L <- layout_with_graphopt(G)
    
    vs <- V(G)
    es <- as.data.frame(get.edgelist(G))
    
    Nv <- length(vs)
    Ne <- length(es[1]$V1)
    
    Xn <- L[, 1]
    Yn <- L[, 2]
    
    network <-
        plot_ly(
            x = ~ Xn,
            y = ~ Yn,
            mode = "markers",
            text = vs$name,
            hoverinfo = "text",
            split = ~ colors_,
            type = "scatter"
        )
    
    edge_shapes <- list()
    for (i in 1:Ne) {
        v0 <- es[i, ]$V1
        v1 <- es[i, ]$V2
        Xn
        
        edge_shape = list(
            type = "line",
            line = list(color = "#030303", width = 0.3),
            x0 = Xn[which(vs$name == v0)],
            y0 = Yn[which(vs$name == v0)],
            x1 = Xn[which(vs$name == v1)],
            y1 = Yn[which(vs$name == v1)]
        )
        
        edge_shapes[[i]] <- edge_shape
    }
    
    axis <-
        list(
            title = "",
            showgrid = FALSE,
            showticklabels = FALSE,
            zeroline = FALSE
        )
    
    p <- layout(
        network,
        title = 'Network',
        #Changed the title
        shapes = edge_shapes,
        xaxis = axis,
        yaxis = axis
    )
    return(p)
}

retweets_links <- function(tweets, users){
    #retweets networks
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
    retweets_connections$from <-
        as.character(retweets_connections$from)
    retweets_connections[,tweet_id := NULL]
    return(unique(retweets_connections))
}

reply_links <- function(tweets, users){
    #retweets networks
    replies <- tweets[!is.na(in_reply_to_status_id)]
    
    replies_connections <- merge(
        unique(tweets[, .(user_id, tweet_id)]),
        unique(replies[, .(user_id, in_reply_to_status_id)]),
        by.x = "tweet_id",
        by.y = "in_reply_to_status_id",
        suffixes = c(".tweet", ".retweet")
    )[!is.na(user_id.retweet) &
          !is.na(user_id.tweet)][, n.interactions := .N , by = .(user_id.tweet, user_id.retweet)]
    setnames(replies_connections, "user_id.tweet", "to")
    setnames(replies_connections, "user_id.retweet", "from")
    replies_connections$to <- as.character(replies_connections$to)
    replies_connections$from <-
        as.character(replies_connections$from)
    replies_connections[,tweet_id := NULL]
    return(unique(replies_connections))
}

both_links <- function(tweets, users){
    rt <- retweets_links(tweets, users)
    rp <- reply_links(tweets, users)
    rtp <- rbindlist(list(rt,rp))
    rtp[,n.interactions := sum(n.interactions), by = .(to, from)]
    return(rtp)
}

messed_users <- function(mod_membership = modularity_$membership, mod_names = modularity_$names, users){
    unique_clusters <-
        unique(paste(
            ifelse(mod_names %in% users[users$bot,]$id, "T", "F"),
            mod_membership
        )) |> substr(3, 10) |> 
        table()
    
    messed_clusters <- unique_clusters[unique_clusters > 1]
    membership_table <- mod_membership |> table()
    return(sum(membership_table[messed_clusters])/sum(membership_table))
}