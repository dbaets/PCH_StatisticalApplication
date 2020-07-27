# ui of statistical app for Proefcentrum Hoogstraten ----------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/png", href = "logopch_black.png"),
    tags$title("PCH - Statistical application")
  ),
  
  navbarPage(
    title = tags$div(img(src="image.png", height = '30px', width = '40px'),"Statistical application"),
    id = "mainnavbarpage",
    collapsible = TRUE,
    fluid = TRUE,
    footer = p("(C) 2020 - Dieter Baets (", a("Github",href = "http://github.com/dbaets"), ") for ",
               a("Proefcentrum Hoogstraten", href = "http://www.proefcentrum.be"),"."),
    inverse = TRUE,
    windowTitle = "Statistical application Proefcentrum Hoogstraten",
    tabPanel("Input", page_input, value = "page_input"),
    tabPanel("Statistics", page_statistics, value = "page_statistics"),
    tabPanel("Output", page_output, value = "page_output"),
    tabPanel("About", page_about, value = "page_about")
  )
)

