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
    mutate(AgeCat= case_when(A1Age < 18 ~ "Below 18 Years",
                             A1Age >= 18 & A1Age <= 29 ~ "18 to 29 Years",
                             A1Age >= 30 & A1Age <= 39 ~ "30 to 39 Years",
                             A1Age >= 40 & A1Age <= 49 ~ "40 to 49 Years",
                             A1Age >= 50 ~ "Above 50 Years",
                             TRUE ~ "Not categorized")) %>% 
    relocate(AgeCat, .after=A1Age)
  
  #Confirm age categories
  table(df1$AgeCat)
  
#remove the leading digits into a new variable or remove special characters
  my_vars <- c("A2Occupation","A3Church","A4LevelOfEducation","A5MaritalStatus","D2Group1","D2Group2","E8WhyhaveSTI",
               "N10givereceiveforsex","N11Usedcondom","N12UseCondom","N13TakenAlcohol","Typeofsti","N9Relationship")
 

  df2 <- df1 
    
    for (col_name in names(df2)) {
      
      if (any(my_vars == col_name)){
        
      #remove leading digits to be codes for the values
        
      new_col_name <- paste0(col_name, "_code")} #create a new column
      df2[[new_col_name]] = str_extract(df2[[col_name]],"^\\d+") 
      df2[[col_name]] = str_trim(str_to_sentence(str_replace_all(df2[[col_name]], "\\d\\W", "")))
      relocate(df2[[new_col_name]], .before= df2[[col_name]])
      } 
 
    }

#reorder the new columns next to corresponding column
  # 
  # df3 <- df2 %>%
  #   select(matches("^.*_code$"))
  # 
  # 
  # my_code_vars <- names(df3)
  # 
  # 
  # for(col_name in my_vars){
  #   if(any(my_code_vars==df2[matches("^.*_code$")])){
  #     relocate(col_name, .after= df2[matches("^.*_code$")])
  #   }
  # }

  # Rename variables 
  df2<-df2 %>%
    dplyr::rename(Age = A1Age,
                  Occupation = A2Occupation,
                  Church = A3Church,
                  Level_Of_Education = A4LevelOfEducation,
                  Marital_Status=A5MaritalStatus,
                  STI_Yes_No=C3StiYesno,
                  Burial_Society=D1BurialSociety,
                  Religious_Group=D1religiousgrp,
                  Savings_Club=D1savingsClub,
                  Traders_Association=D1tradersAssoc,
                  Group1=D2Group1,
                  Group2=D2Group2,
                  Funeral_Assistance=D3FuneralAssistance,
                  Health_Services=D3HealthServices,
                  Duration_Of_Illness=DurationOfillness,
                  Reason_for_STI=E8WhyhaveSTI,
                  Give_Receive_for_sex=N10givereceiveforsex,
                  Used_Condom=N11Usedcondom,
                  Uses_Condom=N12UseCondom,
                  Taken_Alcohol=N13TakenAlcohol,
                  Type_of_STI=Typeofsti,
                  Relationship=N9Relationship,
                  Had_An_STI=N3HadAnSti,
                  Do_You_Have=N14DoYouHave,
                  Living_Together=N15LivingTogether,
                  How_Old_Is=N16HowOldIs,
                  Receive_Credit=D3receivecredit,
                  Habitation_Status=HabitationStatus,
                  Age_First_Sex=AgeFirstSex,
                  Alcohol_Use=AlcoholUse,
                  Sex_Partner_1year=SexPartner1year,
                  Sex_Partner_3month=SexPartner3month,
                  Last_Partner_Spouse=LastPartnerSpouse)
  
#Export Clean data to correct repository for analysis in csv or xlsx format
  #write_csv(df2, "DataClean/STIData_Cleaned.csv", append=FALSE, col_names = TRUE)
  
  writexl::write_xlsx(df2, "DataClean/STIData_Cleaned.xlsx")

  
  
 
  
  