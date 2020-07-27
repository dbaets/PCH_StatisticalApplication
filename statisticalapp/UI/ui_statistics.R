# ui of page_statistics

body_statistics <- shinydashboard::dashboardBody(
  fluidRow(
    box(title = "Statistical tests",
        status = "success",
        solidHeader = FALSE,
        "table with statistical tests and post-hoc tests",
        
    ),
    box(title = "Run Statistics",
        status = "success",
        solidHeader = TRUE,
        actionButton("btn_statisticsrun", label = "Run statistics",icon("rocket"), 
                     style="color: #ffff; background-color: #446F35; border-color: #28572A")
    )
  ),
  fluidRow(
    box(title = "P-value",
        textOutput("p_value")
        ),
    box(
      actionButton("statistic_run", label = "Run statistics")
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