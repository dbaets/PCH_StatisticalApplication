# Global Variables --------------------------------------------------------
# global.no_levels <- numeric()
# global.anova <- NULL
# global.kruskalwallis <- NULL
# global.t_test <- NULL
# global.mannwhitney <- NULL
# global.model <- NULL
# global.sigletters <- NULL


server <- function(input, output,session) {
  # reactive values, global environment
  rv <- reactiveValues(path=NULL,
                       fileselected = FALSE,
                       datatype = NULL,
                       t_input= NULL,
                       t_summary = NULL,
                       no_levels = NULL,
                       anova = NULL,
                       kruskalwallis = NULL,
                       t_test = NULL,
                       mannwhitney = NULL,
                       model = NULL,
                       sigletters = NULL,
                       p_value = NULL
  )
  
  
  # Loading template files --------------------------------------------------
  global.templatepath <- "examplefile\\"
  global.template_botrytis <- read.xlsx(xlsxFile = paste0(global.templatepath,"template_botrytis.xlsx"),sheet = "statistiek")
  global.template_generaltrial <- read.xlsx(xlsxFile = paste0(global.templatepath,"template_generaltrial.xlsx"),sheet = "statistiek")
  
  # Input panel: ------------------------------------------------------------
  
  # templates
  output$btn_botrytis <- downloadHandler(filename = "template_botrytis.xlsx",
                                                content = function(file){
                                                  main_colour <- "#6BA13C"
                                                  options("openxlsx.borderColour" = main_colour)
                                                  headerstyle <- createStyle(
                                                    textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
                                                    fontName = "Arial Narrow", fgFill = main_colour
                                                  )
                                                  output_list <- list("statistiek" = global.template_botrytis)
                                                  write.xlsx(output_list,
                                                             file = file,
                                                             headerStyle = headerstyle,
                                                             startCol = 1, startRow = 1,
                                                             colWidths = c("auto"),
                                                             asTable = c(TRUE),
                                                             withFilter = c(FALSE))
                                                })
  output$btn_generaltemplate <- downloadHandler(filename = "template_generaltrial.xlsx",
                                         content = function(file){
                                           main_colour <- "#6BA13C"
                                           options("openxlsx.borderColour" = main_colour)
                                           headerstyle <- createStyle(
                                             textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
                                             fontName = "Arial Narrow", fgFill = main_colour
                                           )
                                           output_list <- list("statistiek" = global.template_generaltrial)
                                           write.xlsx(output_list,
                                                      file = file,
                                                      headerStyle = headerstyle,
                                                      startCol = 1, startRow = 1,
                                                      colWidths = c("auto"),
                                                      asTable = c(TRUE),
                                                      withFilter = c(FALSE))
                                         })
  
  # selecting file
  observeEvent(input$btn_selectfile,{
    rv$path <- choose.files(default = "C:\\Users\\dbaets\\Desktop\\MaartenHofkens\\21_20 Rassenproef doorteelt",
                            caption = "Select .xlsx file to perform statistical analysis",
                            multi = FALSE)
    if (is.null(rv$path)){
      message("No file selected, please select a file")
      rv$fileselected <- FALSE
    } else{
      rv$fileselected <- TRUE
    }
  })
  
  # selected input file
  output$outputpath <- renderPrint({
    if(is.null(rv$path)) { 
      return("")
    } else{
      seperatedstring <- str_split(string = rv$path, pattern = "\\\\")
      outputstring <- seperatedstring[[1]][length(seperatedstring[[1]])]
      return(paste0("Selected .xlsx-file: ",outputstring))
    }
    
  })
  
  # loading file into program
  observeEvent(input$load,{
    rv$datatype <- input$datatype
    
    if (rv$fileselected){
      t_input_raw <- f_loadingdata(path = rv$path, datatype = input$datatype)
      
      if (is.null(t_input_raw)) {
        showNotification(ui = "The datatype does not match your loaded file. Please select new other datatype",
                         closeButton = TRUE,
                         type = "error",
                         duration = NULL)
        rv$t_input <- NULL
      } else {
        rv$t_input <- preprocessing(df = t_input_raw, datatype = input$datatype)
        updateNavbarPage(session = session,inputId = "mainnavbarpage", selected = "page_statistics")
      }  
      
    } else{
      rv$t_input <- NULL
      showNotification(ui = "No file selected, please select a file.",
                       closeButton = TRUE,
                       type = "error",
                       duration = NULL)
    }
  })
  
  # table output
  output$loadedtable <- DT::renderDataTable({
    DT::datatable(data = rv$t_input)
  })
  
  
  
  # Statistics panel: -------------------------------------------------------
  
  observeEvent(input$btn_statisticsrun,{
    f_processing(df = rv$t_input, datatype = rv$datatype)
    
    print(rv$model$p.value)
    print(rv$sigletters)
    
    updateNavbarPage(session = session,inputId = "mainnavbarpage", selected = "page_output")
    # rv$sigletters <- global.sigletters
    # rv$p_value <- global.model$p.value
  })
  
  # output$p_value <- renderPrint({
  #   if(is.null(rv$p_value)) { 
  #     return("")
  #   } else{
  #     return(paste0("p = ",rv$p_value))
  #   }
  # })
  
  # output$sigletters <- DT::renderDataTable({
  #   
  #   DT::datatable(data = rv$sigletters)
  # })
  
  output$summary_boxplot <- renderPlot({
    # t_boxplot_input <- f_table_boxplot(rv$t_input)
    # p_boxplot <- ggplot(t_boxplot_input,aes())
    ggplot(global.summary, aes(x = object, y = mean))+
      geom_bar(stat ="identity",fill = "#28572A", width = 0.5) + # #28572A (dark green) / #6BA13C (light green)
      geom_errorbar(aes(ymin =  mean-SEM, ymax = mean+SEM),width = 0.2)+
      ylab("gemiddelde")+
      xlab("objectnummer") +
      theme_classic()
    
  })
  
  output$descriptivestats <- DT::renderDataTable({
    DT::datatable(global.summary)
  })
  
  
  
  
  # output panel ------------------------------------------------------------
  observeEvent(input$btn_output,{
    
    # t_teststatistic <- f_combining_output()
    
    
    f_output_to_excel(inputdata = data.frame(rv$t_input),
                      teststatistic = data.frame(global.teststatistic),
                      summary = data.frame(global.summary),
                      sigletters = data.frame(global.sigletters),
                      inputpath = rv$path)
    
    updateNavbarPage(session = session,inputId = "mainnavbarpage", selected = "page_about")
    
  })
  
  # about panel -------------------------------------------------------------
  
  
  
  
  
}

