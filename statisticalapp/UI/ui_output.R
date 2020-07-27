# ui - output -------------------------------------------------------------

body_output <- shinydashboard::dashboardBody(
  fluidRow(
    box(title = "Some output title",
        status = "success",
        solidHeader = FALSE,
        "Some output text"
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
