# All functions related to preprocessing
# preprocessing of botrytis data ------------------------------------------
f_preprocessing_botrytis <- function(df){                                     # for all dates
  # calculate McKinnon Index for all dates not necessairy in current setup - performed in excel
  df$totalno_fruit <- df$aantasting_geen + 
    df$aantasting_25 + df$aantasting_50 + 
    df$aantasting_75 + df$aantasting_100
  
  
  df$incidence <- (df$aantasting_25 +
                     df$aantasting_50 +
                     df$aantasting_75 + 
                     df$aantasting_100) / df$totalno_fruit
  
  # Mc Kinney Index
  df$severity <- (df$aantasting_geen * 0 + 
                    df$aantasting_25 * 1 +
                    df$aantasting_50 * 2 + 
                    df$aantasting_75 * 3 +
                    df$aantasting_100 * 4) / (df$totalno_fruit * 4)
  
  return(df)
  
}


# preprocessing of uitval data --------------------------------------------
f_preprocessing_uitval <- function(df){
  return(df)
}


# preprocessing of drukplek data ------------------------------------------
f_preprocessing_drukplek <- function(df){
  return(df)
}


# preprocessing of yield data ---------------------------------------------
f_preprocessing_yield <- function(df){
  return(df)
}


# preprocessing of grading data -------------------------------------------
f_preprocessing_grading <- function(df){
  return(df)
}


# preprocessing of aantal bloemtakken data --------------------------------
f_preprocessing_bloemtakken <- function(df){
  return(df)
}

# preprocessing of gewaslengte data ---------------------------------------
f_preprocessing_gewaslengte <- function(df){
  return(df)
}

# preprocessing of general trial data -------------------------------------
f_preprocessing_generaltrial <- function(df){
  return(df)
}




# General preprocessing function ------------------------------------------
preprocessing <- function(df, datatype){
  global.botrytis <<- FALSE
  global.uitval <<- FALSE
  global.drukplek <<- FALSE
  global.yield <<- FALSE
  global.grading <<- FALSE
  global.bloemtakken <<- FALSE
  global.gewaslengte <<- FALSE
  global.generaltrial <<- FALSE
  
  if(datatype == 1){ # botrytis
    data_preproc <- f_preprocessing_botrytis(df = df)
    print("Botrytis file loaded.")
    global.botrytis <<- TRUE
  } else if(datatype == 2){ # uitval
    data_preproc <- f_preprocessing_uitval(df = df)
    print("Uitval file loaded")
    global.uitval <<- TRUE
  } else if(datatype == 3){ # drukplekgevoeligheid
    data_preproc <- f_preprocessing_drukplek(df = df)
    print("drukplekgevoeligheid loaded")
    global.drukplek <<- TRUE
  } else if(datatype == 4){ # productie
    data_preproc <- f_preprocessing_yield(df = df)
    global.yield <<- TRUE
    print("yield file loaded.")
  } else if(datatype == 5){ # sortering
    data_preproc <- f_preprocessing_grading(df =df)
    global.grading <<- TRUE
    print("production file loaded.")
  } else if(datatype == 6){ # aantal bloemtakken
    data_preproc <- f_preprocessing_bloemtakken(df =df)
    global.bloemtakken <<- TRUE
    print("aantal bloemtakken file loaded.")
  } else if(datatype == 7){ # gewaslengte
    data_preproc <- f_preprocessing_gewaslengte(df =df)
    global.gewaslengte <<- TRUE
    print("gewaslengte file loaded.")
  } else if(datatype == 100){ # algemene proef
    data_preproc <- f_preprocessing_generaltrial(df =df)
    global.generaltrial <<- TRUE
    print("algemene proef file loaded.")
  } else{
    data_preproc <- NULL
    print("Error_preprocessing: datatype input does not excist. Can't load correct file.")
    showNotification(ui = "Error: Can't load the selected file, pleas load correct file.",
                     closeButton = TRUE,
                     type = "error",
                     duration = NULL)
  }
  return(data_preproc)
}
