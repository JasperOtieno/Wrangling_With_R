#PROGRAM  HEADER ---------------------------------------------------------------
#SCRIPT TITLE           : Install Packages  
#PROJECT                : Package Installation
#TASK                   : Load packages for data wrangling
#CREATED BY             : Jasper         
#DATE CREATED           : 25/Jan/2025                         
#DATE LAST MODIFIED     :                                                                                                 
#LAST MODIFIED BY       : 
#REASON for MODIFICATION:                                                                                            
#-------------------------------------------------------------------------------

  #Load packages needed for most R data tasks
  
  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(
  
    tidyverse,  #for general data cleaning tasks
    readxl,     #for reading excel files
    writexl,    # for saving excel files
    janitor,    # for data manipulation, contains get_dupes for checking duplicates
    plotly,     #for interactive graphs
    here,       #for reading specified file paths
    arsenal,    #for comparing data sets
    esquisse,
    inspectdf,
    babynames,
    table1,
    haven
  )