# ui - output -------------------------------------------------------------

body_output <- shinydashboard::dashboardBody(
  fluidRow(
    box(title = "Output results",
        status = "success",
        solidHeader = FALSE,
        "In order to download the output excel pleas press the green Output Results button."
    ),
    box(title = "Export results to Excel",
        status = "success",
        solidHeader = TRUE,
        actionButton("btn_output", label = "Output Results", icon("file-download"),
                     style="color: #ffff; background-color: #446F35; border-color: #28572A")
    )
  )
)

page_output <- shinydashboard::dashboardPage(
  title = "Output",
  header = shinydashboard::dashboardHeader(disable = TRUE),
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  body = body_output
)
