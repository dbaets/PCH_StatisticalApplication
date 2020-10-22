# ui of page_about

body_about <- shinydashboard::dashboardBody(
  fluidRow(
    box(title = "About PCH statistical application",
        status = "success",
        solidHeader = FALSE,
        "The statistical application is especially build to make statistical
         analysis easier for all researchers at Proefcentrum Hoogstraten.
        By automating the process of choosing the right statistical test the time spent on data analysis is drastically reduced.",
        br(),
        "If you want more information on how to use this application please vistit the ",
        tags$a(href = "http://github.com/dbaets/PCH_StatisticalApplication","Github page of Dieter Baets"),
        "where you can find the source code (MIT license) and a manual.",
        hr(),
        img(src = "logopch_colour.png",
            width = "100px", height = "70px"),
        "Please visit the website of PCH for more information: ",
        tags$a(href="http://www.proefcentrum.be", "www.proefcentrum.be"),
        br()
    ),
    box(title = "R environment and packages",
        status = "success",
        solidHeader = FALSE,
        "Folowwing R packages were used to build this shiny application:",
        p("Made with", a("Shiny",href = "http://shiny.rstudio.com"), "."),
        img(src = "shinylogo.png",
            width = "70px", heigt = "70px")
      
    )
  )
)

page_about <- shinydashboard::dashboardPage(
  title = "About",
  header = shinydashboard::dashboardHeader(disable = TRUE),
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  body = body_about
)
