## app.R ##
library(shinydashboard)
library(data.table)
library(stringr)
library(purrr)
library(glue)
library(recommenderlab)

movies_titles <-
    read.csv("../data/movie_titles.csv", col.names = c("id", "year", "title"))

models <- readRDS("../models.rds")
colnames_m <- readRDS("../colnames_m.rds")
movies_titles <- movies_titles[movies_titles$id %in% colnames_m, ]

n_movies <- length(colnames_m)

ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Movie recommender"),
    dashboardSidebar(collapsed = TRUE,
                     sidebarMenu()),
    ## Body content
    dashboardBody(
        shinyjs::useShinyjs(),
        column(4,
               purrr::map(1:5, \(i){
                   fluidRow(
                       column(8, selectizeInput(glue("movie{i}"), glue("Movie {i}"), 
                                                choices = NULL, width = "100%")),
                       column(
                           4,
                           numericInput(
                               glue("movie{i}_rating"),
                               "Rating",
                               3,
                               min = 1,
                               max = 5,
                               step = 1,
                               width = "50%"
                           )
                       )
                   )
               }),
               
               fluidRow(
                   column(8, selectInput("select_model", "Select model", 
                                         choices = c("UBCF", "IBCF", "SVD", "LIBMF", "POPULAR")))
                ),
               fluidRow(
                   column(8,
                          actionButton("run", "Recommend!"))
               )
               

               ),
        
        column(8,
               fluidRow(
                   valueBoxOutput("rmse"),
                   valueBoxOutput("mse"),
                   valueBoxOutput("mae"),
                   
               ),
               box(width = 12, tableOutput("recommendations"))
               
               
               )
    )
)

server <- function(input, output, session) {
    
    purrr::walk(1:5, \(i){
        updateSelectizeInput(session,
                             glue('movie{i}'),
                             choices = movies_titles$title,
                             selected = movies_titles$title[i],
                             server = TRUE)
        
    })
    
    rmse_ <- reactiveVal(0)
    mse_ <- reactiveVal(0)
    mae_ <- reactiveVal(0)
    
    observeEvent(input$select_model, {
        results <- models[[input$select_model]]$results
        rmse_(results[1])
        mse_(results[2])
        mae_(results[3])
    })
    
    
    observeEvent(input$run, {
        
        movies <- purrr::map_chr(1:5, \(i){
            input[[glue("movie{i}")]]
        })
        
        ratings <- purrr::map_int(1:5, \(i){
            input[[glue("movie{i}_rating")]]
        }) 
        ratings_df <- data.frame(title = movies, ratings)
        ids_df <- movies_titles[movies_titles$title %in% movies,] |> merge(ratings_df, by = "title")
        
        predict_this <- rep(NA, n_movies)
        for (i in 1:length(ids_df$id)) {
            id <- ids_df$id[i]
            rating <- ids_df$ratings[i]
            predict_this[which(colnames_m == id)] <- rating
        }

        predict_this <- as(t(matrix(predict_this)), "realRatingMatrix")
        pred <- predict(models[[input$select_model]]$model, predict_this, 
                        n = 10)

        predicted_ratings <- pred@ratings[[1]]
        predicted_ratings <- pmin(5, predicted_ratings)
        predicted_ratings <- pmax(1, predicted_ratings)
        items <- colnames_m[pred@items[[1]]]
        ratings_df <- data.frame(id = items, predicted_ratings)
        
        proposition_df <- movies_titles[movies_titles$id %in% items,] |> 
            merge(ratings_df, by = "id")
        colnames(proposition_df) <- c("ID", "Year", "Title", "Predicted rating")
        proposition_df <- proposition_df[order(proposition_df$`Predicted rating`, decreasing = T),]
        output$recommendations <- renderTable(proposition_df, width = "100%")
        
    })

    
    output$rmse <- renderValueBox({
        valueBox(
            round(rmse_(), 2),
            "RMSE",
            icon = icon("circle-nodes"),
            color = "purple"
        )
    })
    output$mse <- renderValueBox({
        valueBox(
            round(mse_(), 2),
            "MSE",
            icon = icon("sliders"),
            color = "purple"
        )
    })
    output$mae <- renderValueBox({
        valueBox(
            round(mae_(), 2),
            "MAE",
            icon = icon("centercode"),
            color = "purple"
        )
    })
    
}

shinyApp(ui, server)
