#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(rfm)
library(caret)
library(partykit)
library(randomForest)
library(e1071)
library(nnet)


#### DIRECTORIES ####
working_dir = "C:\\Users\\Laura\\Google Drive\\MAGISTRALE\\1 anno\\2 SEMESTRE\\digital marketing\\progetto Digital Marketing\\SCRIPTS DATASCIENCE LAB DIGITAL MARKETING 01"
data_dir = "C:\\Users\\Laura\\Google Drive\\MAGISTRALE\\1 anno\\2 SEMESTRE\\digital marketing\\progetto Digital Marketing\\2020_DS_lab_digital_mrkt_data_pt1"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####

PIPELINE_scripts <- c(
  'B01_ingestion.R'
  , 'C01_preparation_df1.R'
  , 'C02_preparation_df2.R'
  , 'C03_preparation_df3.R'
  , 'C04_preparation_df4.R'
  , 'C05_preparation_df5.R'
  , 'C06_preparation_df6.R'
  , 'C07_preparation_df7.R'
  , 'modello_RFM.R'
  , 'modello_CHURN.R'
  ## add other scripts
  )

for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
}
#source causes R to accept its input from the named file or URL or connection or expressions directly. 
#Input is read and parsed from that file until the end of the file is reached, then the parsed 
#expressions are evaluated sequentially in the chosen environment.