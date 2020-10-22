# ui of page_input

body_input <- shinydashboard::dashboardBody(
  # box(
  #   fileInput(inputId = "input_file",
  #             label = "Choose .xlsx file",
  #             multiple = FALSE,
  #             accept = c(".xlsx"),
  #             placeholder = "No file selected. Select .xlsx-file.")
  # ),
  fluidRow(
    box(title = "Templates",
        status = "success",
        solidHeader = FALSE,
        downloadButton("btn_botrytis", label = "Botrytis",
                       style="color: #ffff; background-color: #393839; border-color: #28572A"),
        downloadButton("btn_production", label = "Productie",
                       style="color: #ffff; background-color: #393839; border-color: #28572A"),
        downloadButton("btn_sortering", label = "Sortering",
                       style="color: #ffff; background-color: #393839; border-color: #28572A"),
        downloadButton("btn_generaltemplate", label = "General template",
                       style="color: #ffff; background-color: #393839; border-color: #28572A"),
    ),
    box(title = "Load data",
        status = "success",
        solidHeader = TRUE,
        actionButton("load", label = "Load file",icon("file-upload"), 
                     style="color: #ffff; background-color: #446F35; border-color: #28572A")
    )
  ),
  fluidRow(
    box(title = "Select file",
        status= "success",
        solidHeader = FALSE,
        actionButton("btn_selectfile", label = "Select file", 
                     style="color: #ffff; background-color: #393839; border-color: #28572A"),
        br(),
        br(),
        textOutput("outputpath"),
    ),
    
    box(title = "Select input type",
        status= "success",
        solidHeader = FALSE,
        radioButtons("datatype",label = "",
                     choices = list("Botrytis" = 1,
                                    #"Uitval" = 2,
                                    #"Drukplekgevoeligheid" = 3,
                                    "Productie"= 4,
                                    "Sortering"= 5,
                                    #"Aantal bloemtakken" = 6,
                                    #"Gewaslengte" = 7,
                                    "Algemene proef" = 100), 
                     selected = 100))
  ),
  fluidRow(
    DT::dataTableOutput("loadedtable")
)

#   box(
#     actionButton("btn_selectfile", label = "Select file")
#   ),
#   box(title = "",
#       textOutput("outputpath")
#   ),
#   box(
#     actionButton("load", label = "Load file")
#   ),
#   box(
#     radioButtons("datatype", label = h3("Choose datatype"),
#                  choices = list("Botrytis" = 1,
#                                 "Uitval" = 2,
#                                 "Drukplekgevoeligheid" = 3,
#                                 "Productie"= 4,
#                                 "Sortering"= 5), 
#                  selected = 1)
#   ),
# fluidRow(
#   box(title = "",
#       textOutput("outputpath")),
# ),
# fluidRow(
#   DT::dataTableOutput("loadedtable")
# )
)

page_input <- shinydashboard::dashboardPage(
  title = "Input",
  header = shinydashboard::dashboardHeader(disable = TRUE),
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  body = body_input
)

