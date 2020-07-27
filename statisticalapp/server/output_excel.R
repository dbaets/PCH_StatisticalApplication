# Output to excel ---------------------------------------------------------

# file name ---------------------------------------------------------------
f_filename <- function(path, sep_pattern = "\\\\", filename = "statistics"){
  all_locations <- str_locate_all(string = path, pattern = sep_pattern)
  last_location <- all_locations[[1]][length(all_locations[[1]])]
  
  firstpart <- str_sub(string = path, start = 1,end = last_location)
  
  trial_number <- str_sub(string = path, start = last_location+1, end = last_location+1+5)
  trial_number <- str_trim(trial_number,side = "right")
  
  # adjust output file name to analysis type
  analysis_name <- ""
  if (global.botrytis) {
    analysis_name <- "botrytis"
  } else if (global.uitval) {
    analysis_name <- "uitval"
  } else if (global.drukplek) {
    analysis_name <- "drukplek"
  }else if (global.yield) {
    analysis_name <- "productie"
  }else if (global.grading) {
    analysis_name <- "sortering"
  }else{
    analysis_name <- "ERROR"
  }
  
  outputpath <- paste0(firstpart,trial_number,"_",filename,"_",analysis_name,".xlsx")
  
  return(outputpath)

}

# write .xlsx file --------------------------------------------------------
f_output_to_excel <- function(inputdata, teststatistic, sigletters, summary, inputpath){
  # excel output options
  main_colour <- "#6BA13C"
  options("openxlsx.borderColour" = main_colour)
  headerstyle <- createStyle(
    textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
    fontName = "Arial Narrow", fgFill = main_colour
  )
  # output df to different sheets
  output_list <- list("InputData" = inputdata,
                      "TestStatistic" = teststatistic,
                      "Sigletters" = sigletters,
                      "Summary" = summary)
  # make filename and outputpath
  output_path <- f_filename(path = inputpath)
  # write xlsx file
  write.xlsx(output_list,
             file = output_path,
             headerStyle = headerstyle,
             startCol = 1, startRow = 1,
             colWidths = c("auto","auto","auto"),
             asTable = c(TRUE, FALSE, TRUE),
             withFilter = c(TRUE, FALSE, FALSE)
  )
  print("Excel saved to directory.")
}



# Combining statistics output ---------------------------------------------

f_combining_output <- function(){
  # empty df
  
  t_teststatistic <- data.frame("variable_name" = character(6),
                            "value" = numeric(6),
                            stringsAsFactors = FALSE)
  
  if (global.anova){
    t_teststatistic$variable_name[1] <- global.test_name
    t_teststatistic$value[1] <- NA
    t_teststatistic$variable_name[2] <- "DF object"
    t_teststatistic$value[2] <- global.df_object
    t_teststatistic$variable_name[3] <- "DF residuals"
    t_teststatistic$value[3] <- global.df_residuals
    t_teststatistic$variable_name[4] <- "F"
    t_teststatistic$value[4] <- global.F
    t_teststatistic$variable_name[5] <- "P-value"
    t_teststatistic$value[5] <- global.p
    t_teststatistic$variable_name[6] <- paste0("F(",global.df_object,",",
                                               global.df_residuals,")=",
                                               global.F)
    t_teststatistic$value[6] <- NA
    
  } else if(global.kruskalwallis){
    
    t_teststatistic$variable_name[1] <- global.test_name
    t_teststatistic$value[1] <- NA
    t_teststatistic$variable_name[2] <- "DF"
    t_teststatistic$value[2] <- global.df
    t_teststatistic$variable_name[3] <- "F"
    t_teststatistic$value[3] <- global.F
    t_teststatistic$variable_name[4] <- "P-value"
    t_teststatistic$value[4] <- global.p
    t_teststatistic$variable_name[5] <- NA
    t_teststatistic$value[5] <- NA
    t_teststatistic$variable_name[6] <- NA
    t_teststatistic$value[6] <- NA
    
  } else if(global.t_test){
    
  } else if(global.mannwhitney){
    
  } else{
    print("Error output: no statistical test performed.")
  }
  
  
  return(t_teststatistic)
  
}



