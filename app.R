library(shiny)
library(shinythemes)
library(shinyWidgets)
# max file limit 100mb
options(shiny.maxRequestSize = 100*1024^2)

ui <- fluidPage(
    theme = shinytheme("readable"),
    includeCSS("app.css"),
    titlePanel(h1("Univariate Report Generator"),
               windowTitle = "Univariate Report Generator"),
    ##############################################################    
    fluidRow(
        column(
            wellPanel(# input for uploading of files
                fileInput("file", h5("Upload Data File"),
                          accept = c(".csv"), placeholder = "...", 
                          buttonLabel = "Browse Files"))
            ,width = 4),
        column(
            wellPanel(textInput("NAlab", label = h5("Label For Missing Cases"), value = "NA"))
            ,width = 4),
        column(
            wellPanel("Placeholder")
            ,width = 4)
    ),
    ##############################################################
    fluidRow(
        column(
            wellPanel(
                h2("Likert Scales"),
                "Select variables which represent responses to Likert Scales, 
                these responses should consist of integer numbers.",
                "Histograms, Frequency Counts and Basic Descriptive Statistics will be computed.",
                uiOutput("ui.likert")
            ),
            width = 4),
        column(
            wellPanel(
                h2("Categorical Variables"),
                "Select the variables which are Categorical Variables, responses to these variables should consist of .",
                uiOutput("ui.cat")
            ),
            width = 4),
        column(
            wellPanel(
                h2("Continuous Variables"),
                "Select the variables which are Continuous Variables.",
                uiOutput("ui.num")
            ),
            width = 4)
    ),
    fluidRow(
        column(
            wellPanel(
                h2("Scales"),
                "Use the slider to input the number of Scales.",
                numericInput("numscales", label = h3("Number of Scales"), min = 0, 
                             max = 20, value = 0),
                uiOutput("scales")
            ),
            width = 9),
        column(
            wellPanel(
                h5("Click on ", tags$em("Generate Report"), " after selecting the variables below."),
                downloadButton("report", "Generate Report")
            ),
        width = 3)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # get address of the data using the address provided by the user
    mydata <- reactive({
        inFile <- input$file
        # if no response, return null
        if (is.null(inFile))
        {
            return(NULL)
        }
        # read the dataset and assign it to mydata
        return(read.csv(inFile$datapath))
    })
    # the UI for choosing likert scales
    output$ui.likert <- renderUI({
        pickerInput(
            inputId = "likert", 
            choices = names(mydata()),
            options = list(`actions-box` = TRUE,
                           `deselect-all-text` = "Deselect All",
                           `select-all-text` = "Select All",
                           `none-selected-text` = "No Variables Selected",
                           `selected-text-format`= "count",
                           `count-selected-text` = "{0} Variables Selected"), 
            multiple = TRUE
        )
    })
    # the UI for choosing categorical variables
    output$ui.cat <- renderUI({
        pickerInput(
            inputId = "cat", 
            choices = names(mydata()),
            options = list(`actions-box` = TRUE,
                           `deselect-all-text` = "Deselect All",
                           `select-all-text` = "Select All",
                           `none-selected-text` = "No Variables Selected",
                           `selected-text-format`= "count",
                           `count-selected-text` = "{0} Variables Selected"), 
            multiple = TRUE
        )
    })
    # the UI for choosing numeric
    output$ui.num <- renderUI({
        pickerInput(
            inputId = "num", 
            choices = names(mydata()),
            options = list(`actions-box` = TRUE,
                           `deselect-all-text` = "Deselect All",
                           `select-all-text` = "Select All",
                           `none-selected-text` = "No Variables Selected",
                           `selected-text-format`= "count",
                           `count-selected-text` = "{0} Variables Selected"), 
            multiple = TRUE
        )
    })
    
    # the UI for choosing the likert scales that make up a scale
    output$scales <- renderUI({
        if(input$numscales == 0)
        {
            return(NULL)
        }
        
        lapply(1:input$numscales, function(i){
                wellPanel(
                    checkboxGroupInput(paste0("scale", i), 
                              label = paste0("Scale ", i, " Components"), 
                              choices = input$likert)
                    )
        })
    })
    
    
    # generating the report to download
    output$report <- downloadHandler(
        filename = "report.pdf",
        content = function(file) {
            # set up parameters to pass to Rmd
            params <- list(data = paste0(input$file$datapath),
                           likert = input$likert,
                           cat = input$cat,
                           num = input$num,
                           NAlab = input$NAlab,
                           scale1 = input$scale1,
                           scale2 = input$scale2,
                           scale3 = input$scale3,
                           scale4 = input$scale4,
                           scale5 = input$scale5,
                           scale6 = input$scale6,
                           scale7 = input$scale7,
                           scale8 = input$scale8,
                           scale9 = input$scale9,
                           scale10 = input$scale10,
                           scale11 = input$scale11,
                           scale12 = input$scale12,
                           scale13 = input$scale13,
                           scale14 = input$scale14,
                           scale15 = input$scale15,
                           scale16 = input$scale16,
                           scale17 = input$scale17,
                           scale18 = input$scale18,
                           scale19 = input$scale19,
                           scale20 = input$scale20)
            # knit
            rmarkdown::render("report.Rmd", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

# run the app 
shinyApp(ui = ui, server = server)
