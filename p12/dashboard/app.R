## app.R ##
library(shinydashboard)
library(data.table)
library(stringr)
library(purrr)
library(reactable)
library(echarts4r)
library(plotly)
library(igraph)

source("helpers.R")

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

user_connections <- fread("../cosine_similiarity.csv")

ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "Tweeter analysis"),
    ## Sidebar content
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Example tweets",
            tabName = "example_tweets",
            icon = icon("twitter")
        ),
        menuItem(
            "Example users",
            tabName = "example_users",
            icon = icon("user")
        ),
        menuItem("EDA", tabName = "eda", icon = icon("th")),
        menuItem(
            "Network analysis",
            tabName = "retweets_network",
            icon = icon("hive")
        )
    )),
    ## Body content
    dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
        # First tab content
        tabItem(tabName = "example_tweets",
                example_tweets(tweets, random_rows),),
        tabItem(tabName = "example_users",
                example_users(users, random_users),),
        tabItem(
            tabName = "eda",
            fluidRow(
                valueBox(
                    nrow(users),
                    "Users",
                    icon = icon("user"),
                    color = "purple"
                ),
                valueBox(
                    nrow(tweets),
                    "Tweets",
                    icon = icon("pen"),
                    color = "purple"
                ),
                valueBox(
                    nrow(all_hashtags),
                    "Unique hashtags",
                    icon = icon("hashtag"),
                    color = "purple"
                ),
                
            ),
            fluidRow(box(
                reactable(all_hashtags, searchable = TRUE)
            ),
            box(reactable(users[, .(name, location, followers_count)], searchable = TRUE)))
        ),
        
        
        # Second tab content
        tabItem(
            tabName = "retweets_network",
            fluidRow(
                column(4, selectInput(
                    "connection_select",
                    "Select connection:",
                    c("Retweet",
                      "Reply",
                      "Reply + retweet",
                      "Hashtag similiarity")
                )),
                column(
                    4,
                    selectInput(
                        "retweets_select",
                        "Select clustering algorithm:",
                        c("Random walk",
                          "Spectral")
                    )
                ),
                column(
                    2,
                    sliderInput(
                        "threshold",
                        "Select threshold for cosine similiarity:",
                        0, 1,
                        0.5
                    )
                ),
                column(2, actionButton("retweets_calculate", "Calculate"))
            ),
            
            fluidRow(
                box(shinycssloaders::withSpinner(echarts4rOutput(
                    "histogram_retweets"
                ))),
                box(shinycssloaders::withSpinner(plotlyOutput("network_retweets")))
            ),
            fluidRow(
                valueBoxOutput("n_clusters_box"),
                valueBoxOutput("mudularity_val_box"),
                valueBoxOutput("centrality_val_box")
            ),
            fluidRow(
                valueBoxOutput("pctg_all_bots_val_box"),
                valueBoxOutput("pctg_here_bots_val_box"),
                valueBoxOutput("overlaping_bots_val_box")
            )
        )
        
    ))
)

server <- function(input, output) {
    
    output$network_retweets <- renderPlotly({
        plot_ly(data.frame(1, 1))
    })
    output$histogram_retweets <- renderEcharts4r({
        e_charts()
    })
    
    
    n_clusters <- reactiveVal(0)
    mudularity_val <- reactiveVal(0)
    centrality_val <- reactiveVal(0)
    pctg_all_bots_val <- reactiveVal(0)
    pctg_here_bots_val <- reactiveVal(0)
    overlaping_bots_val <- reactiveVal(0)
    
    output$n_clusters_box <- renderValueBox({
        valueBox(
            n_clusters(),
            "Number of clusters",
            icon = icon("circle-nodes"),
            color = "purple"
        )
    })
    output$mudularity_val_box <- renderValueBox({
        valueBox(
            round(mudularity_val(), 5),
            "Modularity",
            icon = icon("sliders"),
            color = "purple"
        )
    })
    output$centrality_val_box <- renderValueBox({
        valueBox(
            round(centrality_val(), 5),
            "Centrality",
            icon = icon("centercode"),
            color = "purple"
        )
    })
    
    output$pctg_all_bots_val_box <- renderValueBox({
        valueBox(
            paste0(round(pctg_all_bots_val(), 2) * 100, "%"),
            "% of all bots present here",
            icon = icon("robot"),
            color = "purple"
        )
    })
    output$pctg_here_bots_val_box <- renderValueBox({
        valueBox(
            paste0(round(pctg_here_bots_val(), 2) * 100, "%"),
            "% of users here that are bots",
            icon = icon("bots"),
            color = "purple"
        )
    })
    output$overlaping_bots_val_box <- renderValueBox({
        valueBox(
            paste0(round(overlaping_bots_val(), 2) * 100, "%"),
            "% of bots and normal users here mixed in one cluster",
            icon = icon("mixer"),
            color = "purple"
        )
    })
    
    
    observeEvent(input$connection_select, {
        shinyjs::toggle(id = 'threshold', 
                        condition = input$connection_select == "Hashtag similiarity")
    })
    
    
    observeEvent(input$retweets_calculate, ignoreInit = T, {
        conn_selected <- isolate(input$connection_select)
        
        if (conn_selected == "Retweet") {
            data_graph <- retweets_links(tweets, users)
        } else if (conn_selected == "Reply") {
            data_graph <- reply_links(tweets, users)
        } else if (conn_selected == "Reply + retweet") {
            data_graph <- both_links(tweets, users)
        } else if (conn_selected == "Hashtag similiarity") {
            data_graph <- copy(user_connections)[weight > input$threshold]
        }
        
        
        pctg_all_bots_val(nrow(users[users$bot &
                                         users$id %in% c(data_graph$to, data_graph$from), ]) / nrow(users[users$bot]))
        pctg_here_bots_val(nrow(users[users$bot &
                                          users$id %in% c(data_graph$to, data_graph$from),]) / length(unique(
                                              c(data_graph$to, data_graph$from)
                                          )))
        
        
        
        G <- graph_from_data_frame(data_graph[, .(to, from)])
        degs <- data.frame(names(degree(G)), degree(G))
        colnames(degs) <- c("id", "degree")
        output$histogram_retweets <- renderEcharts4r({
            degs |>
                e_charts() |>
                e_histogram(degree, name = "histogram", breaks = 100) |>
                e_density(
                    degree,
                    areaStyle = list(opacity = .4),
                    smooth = TRUE,
                    name = "density",
                    y_index = 1
                ) |>
                e_tooltip(trigger = "axis")
        })
        
        
        output$network_retweets <- renderPlotly({
            algorithm_selected <- isolate(input$retweets_select)
            
            if (algorithm_selected == "Random walk") {
                modularity_ <- igraph::walktrap.community(G)
                n_clusters(length(modularity_))
                mudularity_val(igraph::modularity(G, modularity_$membership))
            } else if (algorithm_selected == "Spectral") {
                modularity_ <- rSpectral::spectral_igraph_membership(G)
                n_clusters(length(unique(modularity_$membership)))
                mudularity_val(igraph::modularity(G, modularity_$membership))
            }
            
            overlaping_bots_val(messed_users(modularity_$membership, modularity_$names, users))
            centrality_val(mean(harmonic_centrality(G)))
            create_plotly_graph(G, modularity_$membership)
        })
        
        
    })
    
}

shinyApp(ui, server)
