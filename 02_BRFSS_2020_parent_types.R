install.packages('tidyverse')
install.packages('rmarkdown')
library(tidyverse)
library(dplyr)
library(readr)
library(rmarkdown)

#loading the file
BRFSS_2020 <- read_csv("C:/Users/dell/Desktop/AREC/GRA Materials/Codes/BRFSS_2020.csv")
View(BRFSS_2020)


#preliminary works
limited_dataframe <- BRFSS_2020%>%
  select("_SEX", BIRTHSEX, CHILDREN, MARITAL) %>% 
  rename(SEX = "_SEX")

View (limited_dataframe)

#transformation of data into groups 
limited_dataframe$MARITAL[limited_dataframe$MARITAL==2]<-"single_parent"
limited_dataframe$MARITAL[limited_dataframe$MARITAL==3]<-"single_parent"
limited_dataframe$MARITAL[limited_dataframe$MARITAL==4]<-"single_parent"
limited_dataframe$MARITAL[limited_dataframe$MARITAL==5]<-"single_parent"
limited_dataframe$MARITAL[limited_dataframe$MARITAL==1]<-"double_parent"
limited_dataframe$MARITAL[limited_dataframe$MARITAL==6]<-"double_parent"
limited_dataframe$MARITAL[limited_dataframe$MARITAL==9]<-"refused"
limited_dataframe$CHILDREN[limited_dataframe$CHILDREN==88]<-0
limited_dataframe$CHILDREN[limited_dataframe$CHILDREN==99]<-NA
limited_dataframe$SEX[limited_dataframe$SEX==2]<-"female"
limited_dataframe$SEX[limited_dataframe$SEX==1]<-"male"


new_column <- limited_dataframe %>% 
  mutate(parent_type = case_when(MARITAL == "single_parent" & SEX == "male" ~ "father_only",
                             MARITAL == "single_parent" & SEX == "female"  ~ "mother_only", 
                             MARITAL == "refused" ~ "refused", 
                             MARITAL == "double_parent" ~ "double_parent"))
  
dataframe <- new_column %>%
  select(CHILDREN, parent_type) 


categorical <- dataframe %>%
  group_by(parent_type) %>%
  summarise(CHILDREN = sum(CHILDREN, na.rm = TRUE)) %>% 
  drop_na(parent_type)


percentage <- categorical%>%
  mutate(total= sum(CHILDREN)) %>% 
  mutate(percentage=CHILDREN/total*100)
  

