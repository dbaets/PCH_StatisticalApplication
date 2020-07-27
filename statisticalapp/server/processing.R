# Processing functions

# Summary statistics ------------------------------------------------------
# input table is table with two columns (named "group" and "testvariable")
f_summary_stat <- function(summarytable,group = "group 1"){
  t_summary <- summarytable %>% 
    group_by(object) %>% 
    summarise(grouping = group,
              count = n(),
              mean = mean(testvariable,na.rm = TRUE),
              sd = sd(testvariable, na.rm = TRUE), # amount of variability
              SEM = (sd(testvariable,na.rm = TRUE))/(sqrt(n())), # how far the sample mean of the data is likely to be from the true population mean
              median = median(testvariable, na.rm = TRUE),
              IQR = IQR(testvariable,na.rm = TRUE))
  return(t_summary)
}

# PARAMETRIC --------------------------------------------------------------
# Two groups: T-Test ------------------------------------------------------
f_parametric_t_test <- function(test_variable, two_objects){
  # two_objects is factor vector with two levels
  t_test <- t.test(test_variable ~ two_objects)
  return(t_test)
}

# > two groups: ANOVA -----------------------------------------------------
# anova test
f_anova <- function(test_variable, objectname, datatable){
  aov_model <- aov(test_variable ~ objectname, data = datatable)
  return(aov_model)
}

# significant letters
# adds group to output --> possible to seperate data out of one table
f_anova_sigletters <- function(aov_model, group = "group 1"){
  marginal <- lsmeans(aov_model, ~objectname)
  t_sigletters <- cld(marginal,
                      alpha = 0.05,
                      Letters = letters,
                      adjust = "tukey")
  t_sigletters$Group <- group
  colnames(t_sigletters) <- c("Objectnaam", "lsmean", "SE", "df", "lower_CL", "upper_CL", "sig_letter","Group")
  t_sigletters <- t_sigletters %>%
    dplyr::select(Group, Objectnaam, lsmean, SE, df, lower_CL, upper_CL, sig_letter)
  t_sigletters <- t_sigletters %>%
    arrange(Objectnaam)
  t_sigletters # return ordened dataframe with significant letters and grouping label
}

f_anova_assumptions <- function(aov_model, datatable, test_variable, group_variable){
  assumption_normaldata <- FALSE
  assumption_normalresiduals <- FALSE
  normality <- FALSE
  
  # normal distribution of data (levene test)
  test_levene <- leveneTest(test_variable ~ as.factor(group_variable), data = datatable)
  # normal distribution of residuals (Shapiro test)
  aov_residuals <- residuals(object = aov_model)
  test_shapiro <- shapiro.test(x=aov_residuals)
  
  # check assumptions
  if (test_levene$`Pr(>F)`[1]>=0.05) {
    assumption_normaldata <- TRUE
  } else{
    assumption_normaldata <- FALSE
  }
  if(test_shapiro$p.value >= 0.05){
    assumption_normalresiduals <- TRUE
  } else{
    assumption_normalresiduals <- FALSE
  }
  
  if(assumption_normaldata & assumption_normalresiduals){
    normality <- TRUE
  } else{
    normality <- FALSE
  }
  return(normality) #return TRUE or FALSE depending on assumptions of normality
}

# NON-PARAMETRIC ----------------------------------------------------------

# Two groups: Mann-Whitney test -------------------------------------------
f_nonparametric_mannwhithney <- function(testvariable, objectname){
  #objectname needs to be binary!!
  mannwhitney <- wilcox.test(testvariable ~ objectname)
  # two tailed test
  return(mannwhitney)
}

# > two groups: Kruskal Wallis test ---------------------------------------
f_nonparametric_kruskalwallis <- function(test_variable, objectname, datatable){
  kruskaltest <- kruskal.test(test_variable ~ objectname, data = datatable)
  return(kruskaltest)
}

f_kruskalwallis_groupdifference <- function(test_variable, objectname){
  wilcox <- pairwise.wilcox.test(test_variable,objectname,p.adjust.method = "BH")
  return(wilcox)
}


# Gegevens met scores: non parametric -------------------------------------
# in development

# Multivariate Analyse: GLM model -----------------------------------------
# ask Dieter for advice to develop GLM model

# Repeated measures -------------------------------------------------------
# in development



# Checknumber of objects --------------------------------------------------
# redundant
f_number_of_objects <- function(df, datatype){
  no_objects <- levels(as_factor(df$objectnr))
  
  if (no_objects<2) {
    print("only one object detected. Change input file")
  }else if(no_objects == 2){
    print("two objects detected. Checking normality...")
    f_processing_2Objects(df=df, datatype=datatype)
  }else{
    print("More than two objects detected. Checking normality...")
    f_processing_moreObjects(df=df, datatype=datatype)
  }
  
}

# Check normality two objects ---------------------------------------------
f_normality_2objects <- function(test_vector){
  normality <- FALSE
  normality_shapiro <- shapiro.test(test_vector)
  if (normality_shapiro$p.value>0.05) {
    normality <- TRUE
  } else{
    normality <- FALSE
  }
  return(normality)
}

# SigLetters output - two objects -----------------------------------------
f_sigletters_two_objects <- function(p_value,objectlevels,group = "group"){
  objectlevels <- levels(as.factor(objectlevels))
  objects <- c(objectlevels[1],
               objectlevels[2])
  grouping <- c(group,
                group)
  
  if (p_value>0.05) {
    value <- c("a",
               "a")
  } else{
    value <- c("a",
               "b")
  }
  sigletters <- tibble(grouping = grouping,
                       objectnr = objects,
                       measurement = value)
  return(sigletters)
  
}

###########################################################################
# Processing logic for all datatypes --------------------------------------
###########################################################################

# Processing function for two objects -------------------------------------
# redundant
f_processing_2Objects <- function(df, datatype){
  
  
}



# General processing function ---------------------------------------------
f_processing <- function(df, datatype){
  # global variables
  global.anova <<- FALSE
  global.kruskalwallis <<- FALSE
  global.t_test <<- FALSE
  global.mannwhitney <<- FALSE
  
  global.test_name <<- "no test"
  global.df <<- -1
  global.F <<- -1
  global.p <<- -1
  global.chi <<- -1
  
  
  # Botrytis ----------------------------------------------------------------
  
  if(datatype == 1){ # botrytis
    # factorize input
    df$datum <- as_factor(df$datum)
    df$objectnr <- as_factor(df$objectnr)
    df$objectnaam <- as_factor(df$objectnaam)
    
    # check number of objects
    global.no_levels <<- length(levels(df$objectnaam))
    
    # which test
    if (global.no_levels>2) {
      # ANOVA
      m_model <- f_anova(test_variable = df$severity,
                         objectname = df$objectnr,
                         datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model,
                                                    datatable = df,
                                                    test_variable = df$severity,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model)
        
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value_botrytis <- c("ANOVA, Tukey Post-Hoc",
                            summary_anova[[1]]$Df[1], #df_object
                            summary_anova[[1]]$Df[2], #df_residuals
                            summary_anova[[1]]$`F value`[1], #F-value
                            summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics <- tibble(variable_name = variables,
                             botrytis = value_botrytis)
        
        m_letters <- f_anova_sigletters(aov_model = m_model, group = "botrytis")
        
        print("statistical test: ANOVA, botrytis")
        
      } else{
        global.kruskalwallis <<- TRUE
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$severity,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$severity,
                                                        objectname = df$objectnr)
        m_letters <- letters_temp$p.value
        # add rownames to output
        m_letters <- data.frame(m_letters)
        m_letters$object <- row.names(m_letters)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value_botrytis <- c(m_model$method,
                            as.numeric(m_model$parameter), #df
                            as.numeric(m_model$statistic), #F-value
                            m_model$p.value) #p-value
        statistics <- tibble(variable_name = variables,
                             botrytis = value_botrytis)
        
        print("statistical test: Kruskalwallis, botrytis")
      }
      
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$severity)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$severity,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics <- tibble(variable_name = variables,
                             botrytis = value)
        m_letters <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)))
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$severity,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics <- tibble(variable_name = variables,
                             botrytis = value)
        m_letters <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)))
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough object levels to perform statistics.")
    }
    
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,severity)
    colnames(summarytable_input) <- c("object", "testvariable")
    
    # global output
    global.summary <<- f_summary_stat(summarytable = summarytable_input)
    global.sigletters <<- m_letters
    global.teststatistic <<- statistics
    
    print("Botrytis file processed")
    
    
    # Uitval ------------------------------------------------------------------
    
  } else if(datatype == 2){ # uitval
    
    print("Uitval file processed")
    
    
    # Drukplekgevoeligheid ----------------------------------------------------
    
  } else if(datatype == 3){ # drukplekgevoeligheid
    
    print("Drukplekgevoeligheid file processed")
    
    
    # Yield -------------------------------------------------------------------
    
  } else if(datatype == 4){ # productie
    # factorize input
    df$objectnr <- as_factor(df$objectnr)
    df$objectnaam <- as_factor(df$objectnaam)
    
    # check number of objects
    global.no_levels <<- length(levels(df$objectnaam))
    
    #_________#
    ## kg_pl ## 
    #_________#
    
    if (global.no_levels>2) {
      # ANOVA
      m_model1 <- f_anova(test_variable = df$kg_pl,
                          objectname = df$objectnr,
                          datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model1,
                                                    datatable = df,
                                                    test_variable = df$kg_pl,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model1)
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value_kgpl <- c("ANOVA, Tukey Post-Hoc",
                        summary_anova[[1]]$Df[1], #df_object
                        summary_anova[[1]]$Df[2], #df_residuals
                        summary_anova[[1]]$`F value`[1], #F-value
                        summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics_kgpl <- tibble(variable_name = variables,
                                  kg_pl = value_kgpl)
        
        m_letters1 <- f_anova_sigletters(aov_model = m_model1,group = "kg_pl")
        
        print("statistical test: ANOVA, kg_pl")
        
      } else{
        global.kruskalwallis <<- TRUE
        
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$kg_pl,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$kg_pl,
                                                        objectname = df$objectnr)
        m_letters1 <- letters_temp$p.value
        # add rownames to output
        m_letters1 <- data.frame(m_letters1)
        m_letters1$object <- row.names(m_letters1)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value_kgpl <- c(m_model$method,
                        as.numeric(m_model$parameter), #df
                        as.numeric(m_model$statistic), #F-value
                        m_model$p.value) #p-value
        statistics_kgpl <- tibble(variable_name = variables,
                                  kg_pl = value_kgpl)
        
        print("statistical test: Kruskalwallis, kg_pl")
      }
      
      # Kruskall Wallis
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$kg_pl)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$kg_pl,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_kgpl <- tibble(variable_name = variables,
                             kg_pl = value)
        m_letters1 <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "kg_pl")
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$kg_pl,objectname = objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_kgpl <- tibble(variable_name = variables,
                             kg_pl = value)
        m_letters1 <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "kg_pl")
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough levels to perform statistics.")
    }
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,kg_pl)
    colnames(summarytable_input) <- c("object", "testvariable")
    t_summary_kgpl <<- f_summary_stat(summarytable = summarytable_input, group = "kg_pl")
    
    #________#
    # kg_m2 ##
    #________#
    
    if (global.no_levels>2) {
      # ANOVA
      m_model2 <- f_anova(test_variable = df$kg_m2,
                          objectname = df$objectnr,
                          datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model2,
                                                    datatable = df,
                                                    test_variable = df$kg_m2,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model2)
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value_kgm2 <- c("ANOVA, Tukey Post-Hoc",
                        summary_anova[[1]]$Df[1], #df_object
                        summary_anova[[1]]$Df[2], #df_residuals
                        summary_anova[[1]]$`F value`[1], #F-value
                        summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics_kgm2 <- tibble(variable_name = variables,
                                  kg_m2 = value_kgm2)
        
        m_letters2 <- f_anova_sigletters(aov_model = m_model1,group = "kg_m2")
        
        print("statistical test: ANOVA, kg_m2")
        
      } else{
        global.kruskalwallis <<- TRUE
        
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$kg_m2,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$kg_m2,
                                                        objectname = df$objectnr)
        m_letters2 <- letters_temp$p.value
        # add rownames to output
        m_letters2 <- data.frame(m_letters2)
        m_letters2$object <- row.names(m_letters2)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value_kgm2 <- c(m_model$method,
                        as.numeric(m_model$parameter), #df
                        as.numeric(m_model$statistic), #F-value
                        m_model$p.value) #p-value
        statistics_kgm2 <- tibble(variable_name = variables,
                                  kg_kgm2 = value_kgm2)
        
        print("statistical test: Kruskalwallis, kg_m2")
      }
      
      # Kruskall Wallis
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$kg_m2)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$kg_m2,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_kgm2 <- tibble(variable_name = variables,
                             kg_m2 = value)
        m_letters2 <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "kg_m2")
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$kg_m2,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_kgm2 <- tibble(variable_name = variables,
                             kg_m2 = value)
        m_letters2 <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "kg_m2")
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough levels to perform statistics.")
    }
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,kg_m2)
    colnames(summarytable_input) <- c("object", "testvariable")
    t_summary_kgm2 <<- f_summary_stat(summarytable = summarytable_input, group = "kg_m2")
    
    # combine both outputs + global variables
    global.sigletters <<- bind_rows(m_letters1,m_letters2)
    global.summary <<- bind_rows(t_summary_kgpl,t_summary_kgm2)
    global.teststatistic <<- full_join(x = statistics_kgpl,
                                       y = statistics_kgm2,
                                       by = "variable_name",
                                       copy = TRUE)
    
    
    print("Yield file processed")
    
    
    # Grading -----------------------------------------------------------------
    
  } else if(datatype == 5){ # sortering
    # factorize input
    df$objectnr <- as_factor(df$objectnr)
    df$objectnaam <- as_factor(df$objectnaam)
    
    # check number of objects
    global.no_levels <<- length(levels(df$objectnaam))
    
    #__________#
    # groot2A ##
    #__________#
    if (global.no_levels>2) {
      # ANOVA
      m_model <- f_anova(test_variable = df$groot2A,
                         objectname = df$objectnr,
                         datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model,
                                                    datatable = df,
                                                    test_variable = df$groot2A,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model)
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value <- c("ANOVA, Tukey Post-Hoc",
                   summary_anova[[1]]$Df[1], #df_object
                   summary_anova[[1]]$Df[2], #df_residuals
                   summary_anova[[1]]$`F value`[1], #F-value
                   summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics_groot2A <- tibble(variable_name = variables,
                                     groot2A = value)
        
        m_letters_groot2A <- f_anova_sigletters(aov_model = m_model,group = "groot2A")
        
        print("statistical test: ANOVA, groot 2A")
        
      } else{
        global.kruskalwallis <<- TRUE
        
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$groot2A,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$groot2A,
                                                        objectname = df$objectnr)
        m_letters_groot2A <- letters_temp$p.value
        # add rownames to output
        m_letters_groot2A <- data.frame(m_letters_groot2A)
        m_letters_groot2A$object <- row.names(m_letters_groot2A)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value) #p-value
        statistics_groot2A <- tibble(variable_name = variables,
                                     groot2A = value)
        
        print("statistical test: Kruskalwallis, groot 2A")
      }
      
      # Kruskall Wallis
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$groot2A)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$groot2A,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_groot2A <- tibble(variable_name = variables,
                                     groot2A = value)
        m_letters_groot2A <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Groot2A")
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$groot2A,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_groot2A <- tibble(variable_name = variables,
                                     groot2A = value)
        m_letters_groot2A <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Groot2A")
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough levels to perform statistics.")
    }
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,groot2A)
    colnames(summarytable_input) <- c("object", "testvariable")
    t_summary_groot2A <<- f_summary_stat(summarytable = summarytable_input, group = "groot2A")
    
    
    #__________#
    # grootA  ##
    #__________#
    if (global.no_levels>2) {
      # ANOVA
      m_model <- f_anova(test_variable = df$grootA,
                         objectname = df$objectnr,
                         datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model,
                                                    datatable = df,
                                                    test_variable = df$grootA,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model)
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value <- c("ANOVA, Tukey Post-Hoc",
                   summary_anova[[1]]$Df[1], #df_object
                   summary_anova[[1]]$Df[2], #df_residuals
                   summary_anova[[1]]$`F value`[1], #F-value
                   summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics_grootA <- tibble(variable_name = variables,
                                    grootA = value)
        
        m_letters_grootA <- f_anova_sigletters(aov_model = m_model,group = "grootA")
        
        print("statistical test: ANOVA, groot A")
        
      } else{
        global.kruskalwallis <<- TRUE
        
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$grootA,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$grootA,
                                                        objectname = df$objectnr)
        m_letters_grootA <- letters_temp$p.value
        # add rownames to output
        m_letters_grootA <- data.frame(m_letters_grootA)
        m_letters_grootA$object <- row.names(m_letters_grootA)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value) #p-value
        statistics_grootA <- tibble(variable_name = variables,
                                    grootA = value)
        
        print("statistical test: Kruskalwallis, groot A")
      }
      
      # Kruskall Wallis
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$grootA)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$grootA,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_grootA <- tibble(variable_name = variables,
                                    grootA = value)
        m_letters_grootA <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "GrootA")
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$grootA,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_grootA <- tibble(variable_name = variables,
                                    grootA = value)
        m_letters_grootA <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "GrootA")
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough levels to perform statistics.")
    }
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,grootA)
    colnames(summarytable_input) <- c("object", "testvariable")
    t_summary_grootA <<- f_summary_stat(summarytable = summarytable_input, group = "grootA")
    
    
    #__________#
    # klein   ##
    #__________#
    if (global.no_levels>2) {
      # ANOVA
      m_model <- f_anova(test_variable = df$klein,
                         objectname = df$objectnr,
                         datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model,
                                                    datatable = df,
                                                    test_variable = df$klein,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model)
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value <- c("ANOVA, Tukey Post-Hoc",
                   summary_anova[[1]]$Df[1], #df_object
                   summary_anova[[1]]$Df[2], #df_residuals
                   summary_anova[[1]]$`F value`[1], #F-value
                   summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics_klein <- tibble(variable_name = variables,
                                   klein = value)
        
        m_letters_klein <- f_anova_sigletters(aov_model = m_model,group = "klein")
        
        print("statistical test: ANOVA, klein")
        
      } else{
        global.kruskalwallis <<- TRUE
        
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$klein,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$klein,
                                                        objectname = df$objectnr)
        m_letters_klein <- letters_temp$p.value
        # add rownames to output
        m_letters_klein <- data.frame(m_letters_klein)
        m_letters_klein$object <- row.names(m_letters_klein)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value) #p-value
        statistics_klein <- tibble(variable_name = variables,
                                   klein = value)
        
        print("statistical test: Kruskalwallis, klein")
      }
      
      # Kruskall Wallis
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$klein)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$klein,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_klein <- tibble(variable_name = variables,
                                   klein = value)
        m_letters_klein <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Klein")
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$klein,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_klein <- tibble(variable_name = variables,
                                   klein = value)
        m_letters_klein <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Klein")
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough levels to perform statistics.")
    }
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,klein)
    colnames(summarytable_input) <- c("object", "testvariable")
    t_summary_klein <<- f_summary_stat(summarytable = summarytable_input, group = "klein")
    
    
    #__________#
    # misvormd##
    #__________#
    if (global.no_levels>2) {
      # ANOVA
      m_model <- f_anova(test_variable = df$misvormd,
                         objectname = df$objectnr,
                         datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model,
                                                    datatable = df,
                                                    test_variable = df$misvormd,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model)
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value <- c("ANOVA, Tukey Post-Hoc",
                   summary_anova[[1]]$Df[1], #df_object
                   summary_anova[[1]]$Df[2], #df_residuals
                   summary_anova[[1]]$`F value`[1], #F-value
                   summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics_misvormd <- tibble(variable_name = variables,
                                      misvormd = value)
        
        m_letters_misvormd <- f_anova_sigletters(aov_model = m_model,group = "misvormd")
        
        print("statistical test: ANOVA, misvormd")
        
      } else{
        global.kruskalwallis <<- TRUE
        
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$misvormd,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$misvormd,
                                                        objectname = df$objectnr)
        m_letters_misvormd <- letters_temp$p.value
        # add rownames to output
        m_letters_misvormd <- data.frame(m_letters_misvormd)
        m_letters_misvormd$object <- row.names(m_letters_misvormd)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value) #p-value
        statistics_misvormd <- tibble(variable_name = variables,
                                      misvormd = value)
        
        print("statistical test: Kruskalwallis, misvormd")
      }
      
      # Kruskall Wallis
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$misvormd)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$misvormd,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_misvormd <- tibble(variable_name = variables,
                                      misvormd = value)
        m_letters_misvormd <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Misvormd")
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$misvormd,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_misvormd <- tibble(variable_name = variables,
                                      misvormd = value)
        m_letters_misvormd <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Misvormd")
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough levels to perform statistics.")
    }
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,misvormd)
    colnames(summarytable_input) <- c("object", "testvariable")
    t_summary_misvormd <<- f_summary_stat(summarytable = summarytable_input, group = "misvormd")
    
    
    #__________#
    # rot     ##
    #__________#
    if (global.no_levels>2) {
      # ANOVA
      m_model <- f_anova(test_variable = df$rot,
                         objectname = df$objectnr,
                         datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model,
                                                    datatable = df,
                                                    test_variable = df$rot,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model)
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value <- c("ANOVA, Tukey Post-Hoc",
                   summary_anova[[1]]$Df[1], #df_object
                   summary_anova[[1]]$Df[2], #df_residuals
                   summary_anova[[1]]$`F value`[1], #F-value
                   summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics_rot <- tibble(variable_name = variables,
                                 rot = value)
        
        m_letters_rot <- f_anova_sigletters(aov_model = m_model,group = "rot")
        
        print("statistical test: ANOVA, rot")
        
      } else{
        global.kruskalwallis <<- TRUE
        
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$rot,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$rot,
                                                        objectname = df$objectnr)
        m_letters_rot <- letters_temp$p.value
        # add rownames to output
        m_letters_rot <- data.frame(m_letters_rot)
        m_letters_rot$object <- row.names(m_letters_rot)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value) #p-value
        statistics_rot <- tibble(variable_name = variables,
                                 rot = value)
        
        print("statistical test: Kruskalwallis, rot")
      }
      
      # Kruskall Wallis
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$rot)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$rot,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_rot <- tibble(variable_name = variables,
                                 rot = value)
        m_letters_rot <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Rot")
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$rot,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "W-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #T-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics_rot <- tibble(variable_name = variables,
                                 rot = value)
        m_letters_rot <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)), group = "Rot")
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough levels to perform statistics.")
    }
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,rot)
    colnames(summarytable_input) <- c("object", "testvariable")
    t_summary_rot <<- f_summary_stat(summarytable = summarytable_input, group = "rot")
    
    
    # combine all outputs and save in global variable
    global.sigletters <<- bind_rows(m_letters_groot2A,
                                    m_letters_grootA,
                                    m_letters_klein,
                                    m_letters_misvormd,
                                    m_letters_rot)
    global.summary <<- bind_rows(t_summary_groot2A,
                                 t_summary_grootA,
                                 t_summary_klein,
                                 t_summary_misvormd,
                                 t_summary_rot)
    
    temp_teststatistic <- full_join(x = statistics_groot2A,
                                    y = statistics_grootA,
                                    by = "variable_name",
                                    copy = TRUE)
    temp_teststatistic <- full_join(x = temp_teststatistic,
                                    y = statistics_klein,
                                    by = "variable_name",
                                    copy = TRUE)
    temp_teststatistic <- full_join(x = temp_teststatistic,
                                    y = statistics_misvormd,
                                    by = "variable_name",
                                    copy = TRUE)
    temp_teststatistic <- full_join(x = temp_teststatistic,
                                    y = statistics_rot,
                                    by = "variable_name",
                                    copy = TRUE)
    
    global.teststatistic <<- temp_teststatistic
    
    
    print("Grading file processed")
    
   

# General Trial -----------------------------------------------------------

  } else if(datatype == 100){
    # factorize input
    df$objectnr <- as_factor(df$objectnr)
    df$objectnaam <- as_factor(df$objectnaam)
    
    # check number of objects
    global.no_levels <<- length(levels(df$objectnaam))
    
    # which test
    if (global.no_levels>2) {
      # ANOVA
      m_model <- f_anova(test_variable = df$meting,
                         objectname = df$objectnr,
                         datatable = df)
      check_assumption_anova <- f_anova_assumptions(aov_model = m_model,
                                                    datatable = df,
                                                    test_variable = df$meting,
                                                    group_variable = df$objectnr)
      # check anova assumptions
      if(check_assumption_anova == TRUE){
        global.anova <<- TRUE
        
        # get anova summary
        summary_anova <- summary.aov(m_model)
        
        variables <- c("Test statistic",
                       "DF object",
                       "DF residuals",
                       "F-value",
                       "p-value")
        value <- c("ANOVA, Tukey Post-Hoc",
                            summary_anova[[1]]$Df[1], #df_object
                            summary_anova[[1]]$Df[2], #df_residuals
                            summary_anova[[1]]$`F value`[1], #F-value
                            summary_anova[[1]]$`Pr(>F)`[1]) #p-value
        statistics <- tibble(variable_name = variables,
                             botrytis = value)
        
        m_letters <- f_anova_sigletters(aov_model = m_model, group = "algemene proef")
        
        print("statistical test: ANOVA, algemene proef")
        
      } else{
        global.kruskalwallis <<- TRUE
        m_model <- f_nonparametric_kruskalwallis(test_variable = df$meting,
                                                 objectname = df$objectnr,
                                                 datatable = df)
        
        letters_temp <- f_kruskalwallis_groupdifference(test_variable = df$meting,
                                                        objectname = df$objectnr)
        m_letters <- letters_temp$p.value
        # add rownames to output
        m_letters <- data.frame(m_letters)
        m_letters$object <- row.names(m_letters)
        
        variables <- c("Test statistic",
                       "DF",
                       "F-value",
                       "p-value")
        value <- c(m_model$method,
                            as.numeric(m_model$parameter), #df
                            as.numeric(m_model$statistic), #F-value
                            m_model$p.value) #p-value
        statistics <- tibble(variable_name = variables,
                             botrytis = value)
        
        print("statistical test: Kruskalwallis, algemene proef")
      }
      
    } else if(global.no_levels == 2){
      # check normality
      normality_check <- f_normality_2objects(test_vector = df$meting)
      if(normality_check){
        # t-test
        global.t_test <<- TRUE
        m_model <- f_parametric_t_test(test_variable = df$meting,two_objects = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   as.numeric(m_model$parameter), #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics <- tibble(variable_name = variables,
                             botrytis = value)
        m_letters <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)))
        
        print("statistical test: T-Test")
      } else {
        # Mann-Whitney U test
        global.mannwhitney <<- TRUE
        m_model <- f_nonparametric_mannwhithney(testvariable = df$meting,objectname = df$objectnr)
        variables <- c("Test statistic",
                       "DF",
                       "T-value",
                       "p-value",
                       "side")
        value <- c(m_model$method,
                   "NO DF", #df
                   as.numeric(m_model$statistic), #F-value
                   m_model$p.value, #p-value
                   m_model$alternative) 
        statistics <- tibble(variable_name = variables,
                             botrytis = value)
        m_letters <- f_sigletters_two_objects(p_value = m_model$p.value,objectlevels = levels(as.factor(df$objectnr)))
        
        print("statistical test: MannWhitney")
      }
    } else {
      print("Error: not enough object levels to perform statistics.")
    }
    
    
    summarytable_input <- df %>% 
      dplyr::select(objectnr,meting)
    colnames(summarytable_input) <- c("object", "testvariable")
    
    # global output
    global.summary <<- f_summary_stat(summarytable = summarytable_input)
    global.sigletters <<- m_letters
    global.teststatistic <<- statistics
    
    print("general trial file processed")
  
     
    
  } else{
    message("Error, datatype not found")
    
  }
  # return
  
}



