## Load libraries
library(shiny)
library(shinythemes)
library(magick)
library(stringr)
library(lubridate)
library(readr)
library(tesseract)

## 4 Letter words
words <- read_table("data/4letterwords.csv", col_names = FALSE)$X1

## Set the appropriate resource paths
addResourcePath(prefix = "images", directoryPath = "images/")
addResourcePath(prefix = "js", directoryPath = "js/")

## Define UI
ui <- navbarPage(id = "top-nav", title = div(img(src = "images/OAG_CLR_web_small.png", width = "60px")), windowTitle = "OCR App", theme = shinytheme("cosmo"),
     tabPanel("App", icon = icon("area-chart", "fa-2x"),
              sidebarLayout(
                  
                  ## Sidebar Panel containing controls
                  sidebarPanel(
                      h3(strong("OCR App")),
                      
                      p("This shiny app creates a canvas that captures your mouse strokes. Just tap on to begin and end strokes. When you are finished click 'Perform OCR' and see how the Tesseract 4 OCR engine interprets your handwriting. When you are ready to play our game, where we give you a random word, and it's your job to write it clearly enough for the algorithm to detect."),
                      
                      hr(),
                      
                      radioButtons("mode", label =NULL,choices = c("Free Draw Mode","Game Mode"),selected ="Free Draw Mode",inline=FALSE),
                      
                      fluidRow(
                          conditionalPanel(condition = "input.mode == 'Free Draw Mode'",column(3, offset = 0,actionButton("reset", "Reset Canvas",icon = icon("trash")))),
                          conditionalPanel(condition = "input.mode == 'Game Mode'",column(2, offset = 0,actionButton("replay", "Replay ",icon = icon("undo")))),
                          column(4, offset = 0),
                          conditionalPanel(condition = "input.mode == 'Free Draw Mode'",column(3, offset = 0,actionButton("feed", "Perform OCR",icon = icon("search")))),
                          conditionalPanel(condition = "input.mode == 'Game Mode'",column(3, offset = 0,actionButton("test", "Submit and Score",icon = icon("text-width"))))
                      ),
                      
                      hr(),
                      
                      conditionalPanel(condition = "input.mode == 'Free Draw Mode'",verbatimTextOutput("ocr_txt")),
                      conditionalPanel(condition = "input.mode == 'Game Mode'",verbatimTextOutput("game_res_txt")),
                      
                      textOutput("eventTimeRemaining"),
                      
                      tags$head(tags$style("#ocr_txt{color: blue;font-size: 20px;font-style: bold;}")),
                      tags$head(tags$style("#game_res_txt{color: blue;font-size: 20px;font-style: bold;}")),
                      tags$style(HTML(".radio-inline {margin-right: 50px;}"))
                  ),
                  
                  ## Main panel containing drawing pad
                  mainPanel(
                      plotOutput( "plot", width = "800px", height = "600px",
                                  hover=hoverOpts(id = "hover", delay = 300, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                  click="click"
                      )
                  )
              )),
     tabPanel("Home", icon = icon("home", "fa-2x"), value = "http://omnianalytics.io"),
     tabPanel("Other Apps", icon = icon("code-fork", "fa-2x"), value = "http://labs.omnianalytics.io"),
     footer = tagList(includeScript("js/top-nav-links.js"))
)

## Define the Server
server <- function(input, output, session) {
    
    ## Defining reactive values to store app data
    vals = reactiveValues(x=NULL, y=NULL, starttime=NULL)
    txt_holder = reactiveValues(txt=NULL,game_txt=sample(words,1),g_txt=NULL,res_txt=NULL)
    draw = reactiveVal(FALSE)
    
    ########################################################################################################
    ## Drawing Pad Area Codebase
    ########################################################################################################
    
    ## Canvas handler
    observeEvent(input$click,handlerExpr = {
        ## Change the boolean draw
        temp <- draw()
        draw(!temp)
        if(!draw())
        {
            vals$x <- c(vals$x, NA)
            vals$y <- c(vals$y, NA)
        }
    })
    
    observeEvent(input$hover, {
        if (draw())
        {
            vals$x <- c(vals$x, input$hover$x)
            vals$y <- c(vals$y, input$hover$y)
        }
    })
    
    ## Reset Canvas
    observeEvent(c(input$reset,input$replay), handlerExpr = {
        vals$x <- NULL
        vals$y <- NULL
        txt_holder$txt <- NULL
        txt_holder$g_txt <- NULL
        txt_holder$game_txt <- sample(words,1)
        txt_holder$res_txt <- NULL
    })
    
    ########################################################################################################
    ########################################################################################################
    
    ########################################################################################################
    ## OCR Job and Game Algo
    ########################################################################################################
    observeEvent(c(input$feed,input$test), {
        img <- image_graph()
        plot(x=vals$x, y=vals$y, xlim=c(0, 28), ylim=c(0, 28), ylab="", xlab="", xaxt='n', yaxt='n', type="l", axes=F,lwd=8)#input$mywidth)
        if(input$mode=="Free Draw Mode") txt_holder$txt <- str_trim(image_ocr(img))
        if(input$mode=="Game Mode")
        {
            vals$starttime <- NULL
            g_text <- str_trim(image_ocr(img))
            txt_holder$g_txt <- g_text
            txt_holder$res_txt <- ifelse(identical(g_text,txt_holder$game_txt),"👍 \nYou got that Right",paste0("👎\nRequired: ",txt_holder$game_txt,"\nSubmission: ",g_text))
        }
    })
    
    observeEvent(c(input$mode), handlerExpr = {
        vals$x <- NULL
        vals$y <- NULL
        txt_holder$txt <- NULL
        txt_holder$g_txt <- NULL
        txt_holder$game_txt <- sample(words,1)
        txt_holder$res_txt <- NULL
    })
    
    observe({
        if (input$mode == "Game Mode") {
            vals$starttime <- Sys.time() + seconds(7 * nchar(txt_holder$game_txt))
        } else {
            vals$starttime <- NULL
        }
    })
    
    ########################################################################################################
    ########################################################################################################
    
    
    ########################################################################################################
    ## Output Plot and texts
    ########################################################################################################
    output$plot <- renderPlot({
        plot(x = vals$x, y = vals$y, 
             xlim = c(0, 28), ylim = c(0, 28), 
             ylab = "", xlab = "", 
             xaxt = 'n', yaxt = 'n', type = "l", lwd = 8)
    })
    
    output$ocr_txt <- renderText({
        if (is.null(txt_holder$txt)) return(NULL)
        if (txt_holder$txt == "") return(NULL)
        
        paste0("You Wrote \n", txt_holder$txt)
    })
    
    output$game_res_txt= renderText({
        return(ifelse(is.null(txt_holder$res_txt),paste0("Try writing the word\n",txt_holder$game_txt),txt_holder$res_txt))
    })
    
    output$eventTimeRemaining <- renderText({
        invalidateLater(100, session)
        
        if (input$mode == 'Free Draw Mode' || is.null(vals$starttime)) return(NULL)
        
        res <- paste("Time remaining:", max(0, round(difftime(vals$starttime, Sys.time(), units='secs'), digits = 1)), 'seconds')
        
        return(res)
    })
    ########################################################################################################
    ########################################################################################################
}

shinyApp(ui, server)
