# ui of page_statistics

body_statistics <- shinydashboard::dashboardBody(
  fluidRow(
    box(title = "Statistical tests",
        status = "success",
        solidHeader = FALSE,
        "Placeholder for table with overview of differnt statistal tests.",
        
    ),
    box(title = "Run Statistics",
        status = "success",
        solidHeader = TRUE,
        actionButton("btn_statisticsrun", label = "Run statistics",icon("rocket"), 
                     style="color: #ffff; background-color: #446F35; border-color: #28572A")
    )
  ),
  
  tabsetPanel(
    id = "tables_stats",
    tabPanel("Boxplot", plotOutput("summary_boxplot")),
    tabPanel("Descriptive stats", DT::dataTableOutput("descriptivestats"))
  ),
  
  fluidRow(
    DT::dataTableOutput("sigletters")
  )
  
)

page_statistics <- shinydashboard::dashboardPage(
  title = "Statistics",
  header = shinydashboard::dashboardHeader(disable = TRUE),
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  body = body_statistics
)