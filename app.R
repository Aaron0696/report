library(shiny)
library(shinythemes)
# max file limit 100mb
options(shiny.maxRequestSize = 100*1024^2)

ui <- fluidPage(
    theme = shinytheme("readable"),
    tags$script(src = "fileInput_text.js"),
    titlePanel(h1("Univariate Report Generator"),
               windowTitle = "Univariate Report Generator"),
##############################################################    
    fluidRow(
        column(
            wellPanel(# input for uploading of files
                fileInput("file", h5("Upload Data File"),
                          accept = c(".csv"), placeholder = "...", 
                          buttonLabel = "Browse Files"),
                textInput("NAlab", label = h5("Label For Missing Cases"), value = "NA"),
                h5("Click on ", tags$em("Generate Report"), " after selecting the variables below."),
                downloadButton("report", "Generate Report")
            ),
            width = 12)
    ),
##############################################################
    fluidRow(
        column(
            wellPanel(
                h2("Likert Scales"),
                "Select the variables which are Likert Scales.",
                uiOutput("ui.likert")
            ),
            width = 4),
        column(
            wellPanel(
                h2("Categorical Variables"),
                "Select the variables which are Categorical Variables.",
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
    fluidRow(column(
        wellPanel(
            h2("Scales"),
            "Use the slider to input the number of Scales.",
            sliderInput("numscales", label = h3("Number of Scales"), min = 0, 
                        max = 20, value = 0),
            uiOutput("scales")
        ),
        width = 4))
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
        
        checkboxGroupInput("likert", label = NULL, 
                           choices = names(mydata()),
                           selected = NULL)
    })
    # the UI for choosing categorical variables
    output$ui.cat <- renderUI({
        
        checkboxGroupInput("cat", label = NULL, 
                           choices = names(mydata()),
                           selected = NULL)
    })
    # the UI for choosing numeric
    output$ui.num <- renderUI({
        
        checkboxGroupInput("num", label = NULL, 
                           choices = names(mydata()),
                           selected = NULL)
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
