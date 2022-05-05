#
# shiny app for helping with wordle
#

# load packages
list.of.packages <- c("shiny", "shinyjs", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinyjs)
library(DT)
source("./functions.R")


wordle_grid = data.frame(
    letter1 = rep("",6),
    letter2 = rep("",6),
    letter3 = rep("",6),
    letter4 = rep("",6),
    letter5 = rep("",6)
)

position_grid = data.frame(
    pos1 = rep(0,6),
    pos2 = rep(0,6),
    pos3 = rep(0,6),
    pos4 = rep(0,6),
    pos5 = rep(0,6)
)

combined_grid <- cbind(wordle_grid,position_grid)


# Define UI for application
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      body {
        background-color: white;
        color: black;
      }
      h1 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }
      table, th, td {
        background-clip: padding-box;
        font-family: 'Yusei Magic', sans-serif;
        font-size: 150%;
        border-radius: 5px;
        color: black;
        height: 50px;
        border: 2px solid white;
      }"))
    ),
    
    useShinyjs(),
    
    # Application title
    titlePanel("Wordle Assistant"),

    sidebarLayout(
        sidebarPanel(
            div(
                id = "words",
                textInput("word1",
                          "1st word:"),
                textInput("word2",
                          "2nd word:"),
                textInput("word3",
                          "3rd word:"),
                textInput("word4",
                          "4th word:"),
                textInput("word5",
                          "5th word:"),
                textInput("word6",
                          "6th word:")
            ),
            actionButton("resetWords", "Reset words"),
            actionButton("resetPositions", "Reset colours")
        ),

        mainPanel(
            h1("Type in your wordle attempt and match the colours"),
            DTOutput("wordleGrid", height = "40em"),
            h1("Possible words in our dictionary"),
            textOutput("text")
        )
    ),

    uiOutput("footer")
)

server <- function(input, output, session) {
    
    # function to update grid using word input
    update_wordle_grid <- function(new_word, row_n, max_char = 5){
        if(nchar(new_word)>0){
            wordle_edit <- NULL
            wordle_edit$row <- rep(row_n, min(max_char, nchar(new_word)))
            wordle_edit$col <- 0:min(max_char-1, nchar(new_word)-1)
            wordle_edit$value <- head(unlist(strsplit(new_word, "")), max_char)
            wordle_edit <- data.frame(wordle_edit)
            
            combined_grid <<- editData(combined_grid, wordle_edit, "wordleGrid", rownames = FALSE)
        } else {
            combined_grid <- combined_grid
        }
        return()
    }
    
    # main grid
    output$wordleGrid <- renderDT({
        datatable(combined_grid,
                  rownames = FALSE,
                  colnames = NULL,
                  select="none",
                  options = list(
                      dom = 't',
                      ordering = FALSE,
                      autoWidth = TRUE,
                      columnDefs = list(
                          list(className = 'dt-center',
                               targets = 0:4),
                          list(visible = FALSE,
                               targets = c(5,6,7,8,9)),
                          list(
                              width = '70px', 
                              targets = "_all"
                          )
                        )
                      )
                 
                  ) %>%
            formatStyle(
                names(wordle_grid),
                valueColumns = names(position_grid),
                fontWeight = "bold",
                color = "black",
                background = styleEqual(c("0","1","2"), c('grey', 'yellow', 'green'))
            )
    })
    
    # all listeners
    toListen <- reactive({
        list(
            input$word1,
            input$word2,
            input$word3,
            input$word4,
            input$word5,
            input$word6,
            input$resetWords,
            input$resetPositions,
            input$wordleGrid_cell_clicked
             )
    })
    
    # read updates
    observeEvent(input$word1,{
        # limit to 5 char
        if(nchar(input$word1)>5){
            word_trim <- str_sub(input$word1,1,5)
            updateTextInput(session, "word1", value = word_trim)
        }
        update_wordle_grid(input$word1, 1)
    })
    observeEvent(input$word2,{
        # limit to 5 char
        if(nchar(input$word2)>5){
            word_trim <- str_sub(input$word2,1,5)
            updateTextInput(session, "word2", value = word_trim)
        }
        update_wordle_grid(input$word2, 2)
    })
    observeEvent(input$word3,{
        # limit to 5 char
        if(nchar(input$word3)>5){
            word_trim <- str_sub(input$word3,1,5)
            updateTextInput(session, "word3", value = word_trim)
        }
        update_wordle_grid(input$word3, 3)
    })
    observeEvent(input$word4,{
        # limit to 5 char
        if(nchar(input$word4)>5){
            word_trim <- str_sub(input$word4,1,5)
            updateTextInput(session, "word4", value = word_trim)
        }
        update_wordle_grid(input$word4, 4)
    })
    observeEvent(input$word5,{
        # limit to 5 char
        if(nchar(input$word5)>5){
            word_trim <- str_sub(input$word5,1,5)
            updateTextInput(session, "word5", value = word_trim)
        }
        update_wordle_grid(input$word5, 5)
    })
    observeEvent(input$word6,{
        # limit to 5 char
        if(nchar(input$word6)>5){
            word_trim <- str_sub(input$word6,1,5)
            updateTextInput(session, "word6", value = word_trim)
        }
        update_wordle_grid(input$word6, 6)
    })
    
    # reset buttons
    observeEvent(input$resetWords, {
        reset("words")
        
        #reset grid
        wordle_edit <- NULL
        wordle_edit$row <- rep(1:6,5)
        wordle_edit$col <- rep(0:4,each=6)
        wordle_edit$value <- ""
        wordle_edit <- data.frame(wordle_edit)
        
        combined_grid <<- editData(combined_grid, wordle_edit, "wordleGrid", rownames = FALSE)
    })
    
    observeEvent(input$resetPositions, {
        #reset grid colours
        position_edit <- NULL
        position_edit$row <- rep(1:6,5)
        position_edit$col <- rep(5:9,each=6)
        position_edit$value <- 0
        position_edit <- data.frame(position_edit)
        
        combined_grid <<- editData(combined_grid, position_edit, "wordleGrid", rownames = FALSE)
    })
    
    # colour click listener
    observeEvent(input$wordleGrid_cell_clicked,{
        req(length(input$wordleGrid_cell_clicked) > 0)

        position_edit <- NULL
        # note: some parts are zero indexed
        position_edit$row <- input$wordleGrid_cell_clicked$row
        position_edit$col <- input$wordleGrid_cell_clicked$col + 5
        position_edit$value <- (combined_grid[position_edit$row,
                                              position_edit$col+1] + 1) %%3

        combined_grid <<- editData(combined_grid, position_edit, 'wordleGrid', rownames = FALSE)
    })
    

    # run wordle help
    observeEvent(toListen(), {
        print("try to eval")
        word_list <- NULL
        position_list <- NULL

        total_entries <- sum(rowSums(combined_grid[,1:5]=="")==0)

        print(total_entries)
        if(total_entries>0){
            # only operate on non empty words
            for (word_n in 1:total_entries){
                # extract info from grid
                word_list <- c(word_list,
                               paste(combined_grid[word_n,1:5],collapse=""))
                position_list <- c(position_list,
                                   paste(combined_grid[word_n,6:10],collapse=""))
            }
            print(word_list)
            print(position_list)


            output$text <- renderText({
                paste(wordle_help_multi(tolower(word_list),
                                        position_list),
                      collapse = ", ")
                })
        }
    })
    
    # footer
    url <- a("/kevinchtsang", href="https://github.com/kevinchtsang/wordle_assist")
    output$footer <- renderUI({
      tagList("Developed by ", url)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
