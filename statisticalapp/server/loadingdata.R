# All functions related to loading the data

# # loading xlsx file

# f_loading_xlsx <- function(path, sheetname = "statistiek"){
#   
#   raw_import_data <- read.xlsx(xlsxFile = path,sheet = sheetname)
#   return(raw_import_data)
# }

f_loading_xlsx <- function(path, sheetname = "statistiek") {
  out <- tryCatch(
    {
      raw_import_data <- read.xlsx(xlsxFile = path,sheet = sheetname)
    },
    error=function(cond) {
      message(paste("The Excel sheet does not seem to exist: ", sheetname))
      return(NULL)
    },
    warning=function(cond) {
      message(paste("The Excel sheet does not seem to exist: ", sheetname))
      return(NULL)
    },
    finally={
      
    }
  )    
  return(out)
}

# General loading function ------------------------------------------------
f_loadingdata <- function(path, datatype){
  if(datatype == 1){ # botrytis
    # load statistiek sheet
    loaded_data <- f_loading_xlsx(path = path)
  } else if(datatype == 2){ # uitval
    loaded_data <- f_loading_xlsx(path = path)
  } else if(datatype == 3){ # drukplekgevoeligheid
    loaded_data <- f_loading_xlsx(path = path)
  } else if(datatype == 4){ # productie
    loaded_data <- f_loading_xlsx(path = path, sheetname = "statistiek_productie")
  } else if(datatype == 5){ # sortering
    loaded_data <- f_loading_xlsx(path = path, sheetname = "statistiek_sortering")
  } else if(datatype == 100){ # general trial
    loaded_data <- f_loading_xlsx(path = path)
  } else{
    loaded_data <- NULL
  }
  return(loaded_data)
}

