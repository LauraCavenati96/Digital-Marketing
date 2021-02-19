#### FIRST LOOK of df_1 ####

str(df_1_cli_fid)
summary(df_1_cli_fid)

#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())
#un cliente può sottoscrivrere più di un abbonamento fedeltà 
#(nel corso del tempo può attivarne uno nuovo e quello più vecchio lo disattiva). 
#può sottoscriverli nella stessa data o in date diverse
#ci sono 369472 clienti, 367925 sottoscrizioni alla carta fedeltà e 370135 coppie CLI-FID (dimensione del dataset)


#!!! NOTE:  no duplicates for combination CLI-FID !!!#
#mentre ci sono duplicati per ID_CLI e ID_FID!!!
#ID_CLI 788871 ID_FID 764499
#ID_CLI 788871 ID_FID 777389

#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####




## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
            )

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#

## let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 3)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
#magari perchè ha perso la carta, oppure la password...

# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%  #arrange() for reordering the rows
  filter(row_number() == 1) %>% #cioè prendi quelli che hanno fatto solo il primo abbonamento fatto nella prima data
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

#### EXPLORE COLUMNS of df_1 ####

### variable LAST_COD_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
         ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

#### ???? TO DO df_1 ???? ####
# EXPLORE the remaining df_1_cli_fid_clean relevant variables

#numero di abbonamenti sottoscritti da ogni negozio

df_1_cli_fid_id_negozio <- df_1_cli_fid_clean %>%
  group_by(FIRST_ID_NEG) %>%
  summarise(TOT_ID_NEG = n())%>%
  mutate(PERCENT = TOT_ID_NEG/sum(TOT_ID_NEG)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

plot_df_1_cli_fid_id_negozio  <- (
  ggplot(data=df_1_cli_fid_id_negozio 
         , aes(x=FIRST_ID_NEG, y=TOT_ID_NEG)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
)

plot_df_1_cli_fid_id_negozio 
#negozio 1 fa molti abbonamenti!!
#------------------------------------------------------------------
 




#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)