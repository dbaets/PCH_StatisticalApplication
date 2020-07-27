###########################################################################
# Statistical shiny application for Proefcentrum Hoogstraten, Belgium
# Company site:       https://www.proefcentrum.be
#
# Release date:       27/07/2020
# Last modified:      27/07/2020
#
# Author:             Dieter Baets
# Contact:            dieter.baets@proefcentrum.be or dieterbaets@gmail.com
# Github repository:  https://github.com/dbaets/PCH_StatisticalApplication
#
# License:            This application is shared under to Creative Commons
#                     Attribution-ShareAlike 4.0 International license
#                     (CC BY-SA 4.0)
#                     (https://creativecommons.org/licenses/by-sa/4.0/))'
#                     + MIT-license
# 
###########################################################################

# Statistical application Proefcentrum Hoogstraten - Dieter Baets ---------

# Loading packages --------------------------------------------------------
# shiny packages
library(shiny)
library(shinydashboard)

#logic packages
library(openxlsx)
library(readxl)
library(tidyverse)
library(stringr)
library(emmeans)
library(multcomp)
library(car)

# Sourcing ui and server files --------------------------------------------
# UI
source("UI/ui_input.R", local = TRUE)
source("UI/ui_statistics.R", local = TRUE)
source("UI/ui_output.R", local = TRUE)
source("UI/ui_about.R", local = TRUE)
source("UI/ui.R", local = TRUE)

# SERVER
source("server/loadingdata.R", local = TRUE)
source("server/preprocessing.R", local = TRUE)
source("server/processing.R",local = TRUE)
source("server/output_excel.R",local = TRUE)
source("server/output_excel.R",local = TRUE)
source("server/server.R", local = TRUE)

# Shiny application -------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
