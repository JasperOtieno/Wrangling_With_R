# PROGRAM  HEADER ##############################################################

#SCRIPT TITLE           : stidata_cleaning_script  
#PROJECT                : Data Cleaning
#TASK                   : Import and wrangle the STI dataset for analysis
#CREATED BY             : Jasper         
#DATE CREATED           : 25/Jan/2025                         
#DATE LAST MODIFIED     :                                                                                                 
#LAST MODIFIED BY       : Jasper                                                                                                   
#REASON for MODIFICATION: 

################################################################################

#Load necessary packages

source("scripts/install_packages.R")

#read the excel datasets from the directory

  df <- read_xls(here("DataRaw/STIData.xls"))
  #names(df)
  #dim(df)
  #glimpse(df)

#Clean the sex variable: two vars with some different values

  unique(df$Sex...35)
  table(df$Sex...35, exclude=NULL)

  unique(df$Sex...47)
  table(df$Sex...47, exclude=NULL)
  
#check if the two sex vars are exact match and correctly replace if possible(confirm correct gender with data collection team)
  df1 <- df %>% 
    relocate(Sex...35, .before = Sex...47) %>% 
    dplyr::mutate (Sexdif= ifelse(df$Sex...47 == df$Sex...35, "Same", "Different"), #check if exact match
                   Sex= case_when(Sexdif %in% c("Same", "Different") ~df$Sex...47,  #reassign gender
                                  !is.na(df$Sex...47) & is.na(df$Sex...35) ~df$Sex...47,
                                  !is.na(df$Sex...35) & is.na(df$Sex...47) ~df$Sex...35,
                                  TRUE ~"No sex" #reassign values
                               ))
 

#output cases where sex is different or missing
  
  sex_diff <- df1 %>% 
    select(IdNumber, A1Age, Sex...35, Sex...47, Sexdif, Sex) %>% 
    filter(df1$Sexdif=="Different" |is.na(df1$Sexdif) | Sex=="No sex")


#remove the unwanted sex variables after creating clean one 
  
  df1<- df1 %>% 
    select(-c(Sex...35,Sex...47,Sexdif)) 
  
  
#Check for duplicates on all columns and output key vars
  
  dups_by_all_vars <- janitor::get_dupes(df1) %>% 
    select(IdNumber,dupe_count, Date, A1Age, A2Occupation, Weight, Height)
  
#Check for duplicates on id number and output key vars
  
  dups_by_id <- janitor::get_dupes(df1,IdNumber) %>% 
    select(IdNumber, Date, A1Age, A2Occupation, Weight, Height,dupe_count) 
  
  #get total number of duplicated id numbers
  sum(duplicated(df1$IdNumber)) 
  #sort(unique(df1$IdNumber))
  
#Reassign ID number for subject 51, when age ==  23
  
  df1 <- df1 %>%
    dplyr::mutate(IdNumber2= case_when(df1$IdNumber==51 & df1$A1Age == 23 ~ 227,
                              TRUE ~ df1$IdNumber)) %>% 
          select(-c(IdNumber)) %>% 
          rename(IdNumber = IdNumber2) %>% 
          relocate(IdNumber, .before = CaseStatus)
  
#check again for duplicates
  
  dupID2<-janitor::get_dupes(df1,IdNumber) %>% 
    select(IdNumber,dupe_count, Date, A1Age, A2Occupation, Weight, Height)
  
#Convert ID number to character if needed
  
  df1$IdNumber <- as.character(df1$IdNumber)
  
#Check CaseStatus and clean(confirm correct status value with data collection team)
  
  table(df1$CaseStatus)
  
  #output cases where casestatus is 3  
  case3 <- df1 %>% 
    filter(CaseStatus==3) %>% 
    select(IdNumber:A2Occupation)
  
  df1 <- df1 %>%
    dplyr::mutate(CaseStatus2= case_when(df1$CaseStatus==3 & df1$IdNumber == 31 ~ 1,
                                         df1$CaseStatus==3 & df1$IdNumber == 1 ~ 2,
                                         TRUE~df1$CaseStatus)) %>% 
            select(-c(CaseStatus)) %>% 
            rename(CaseStatus=CaseStatus2) %>% 
            relocate(CaseStatus, .after = IdNumber)

#Convert CaseStatus to a factor and label if it's numeric
  class(df1$CaseStatus)
  
  df1$CaseStatus <- factor(df1$CaseStatus,
                                 levels = 1:2,
                                 labels = c("Positive", "Negative")) #assign labels
  
#confirm cleaned status
  table(df1$CaseStatus)
  
#Check for dates and clean Date

#check for age and clean A1Age/ aggregate

  df1<- df1 %>% 
    mutate(AgeCat= case_when(A1Age < 18 ~ "Child",
                             A1Age >= 18 & A1Age <= 29 ~ "Youth",
                             A1Age >= 30 & A1Age <= 39 ~ "Young Adult",
                             A1Age >= 40 & A1Age <= 49 ~ "Adult",
                             A1Age >= 50 & A1Age <= 59 ~ "Senior Adult",
                             A1Age >= 60  ~ "Elderly",
                             TRUE ~ "Not categorized")) %>% 
    relocate(AgeCat, .after=A1Age)
  
  #Confirm age categories
  table(df1$AgeCat)
  
#extract the leading digits into a new variable of code and remove special characters too
 #create a vector of variables of interest
  
  my_vars <- c() 
  
  for (col_name in names(df1)){
    if(any(grepl("^\\d+([[:punct:]]\\s|\\s)\\w", df[[col_name]]))){
      my_vars <- c(my_vars, col_name)
    }
    
    if (any(my_vars == col_name)){
        
    #remove leading digits to be codes for the values
        
    new_col_name <- paste0(col_name, "_code") #create a new column
    df1[[new_col_name]] = as.numeric(str_extract(df1[[col_name]],"^\\d+")) 
    df1[[col_name]] = str_trim(str_to_sentence(str_replace_all(df1[[col_name]], "\\d\\W", "")))
    } 
 
  }

#reorder the new columns next to corresponding column
  variables <- names(df1)
  
  # identify the unique pattern and remove them
  prefixes <- unique(sub("_code$","",variables))
  
  # initialize an ordered vector
  
  ordered_vector <- c()
  
  # loop through each prefix
  for (prefix in prefixes){
    pattern <- paste0("^",prefix, "_code$|^", prefix,"$" )
    matched_vars <- variables[grepl(pattern, variables)] 
    
    # add the matched vars to the ordered list
  ordered_vector <- c(ordered_vector, matched_vars)
  }
 
  df1 <- df1 %>%
    select(all_of(ordered_vector)) %>% 
    relocate (Sex, .after = IdNumber)
  
#format date to dd-MMM-yyyy
  df1$Date <- format(df1$Date, "%d-%b-%Y") 
    

#Export Clean data to correct repository for analysis in csv or xlsx format
  #write_csv(df1, "DataClean/STIData_Cleaned.csv", append=FALSE, col_names = TRUE)
  
  writexl::write_xlsx(df1, "DataClean/STIData_Cleaned.xlsx")

  
  
 
  
  