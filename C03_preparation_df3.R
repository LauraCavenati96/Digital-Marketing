#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))
#cioè seleziono solo quelli che hanno tutte e tre le feature diverse da nan!!!!


#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### EXPLORE COLUMNS of df_3 ####

#-----------------------------------------------------------------
#### ???? TO DO df_3 ???? ####
# EXPLORE the df_3_cli_address_clean relevant variables

#suddivisione degli indirizzi per regione
df_3_cli_address_reg <- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarise(TOT_REG = n())

plot_df_3_cli_address_reg <- (
  ggplot(data=df_3_cli_address_reg
         , aes(x=REGION, y=TOT_REG)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
)

plot_df_3_cli_address_reg


#suddivisione degli indirizzi per provincia
df_3_cli_address_prv <- df_3_cli_address_clean %>%
  group_by(REGION, PRV) %>%
  summarise(TOT_PRV = n())

plot_df_3_cli_address_prv <- (
  ggplot(data=df_3_cli_address_prv
         , aes(x=PRV, y=TOT_PRV)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
)

plot_df_3_cli_address_prv
#-------------------------------------------------------------



#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)