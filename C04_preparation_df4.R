#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

#---------------------------------------------------------------
#### ???? TO DO df_4 ???? ####
# EXPLORE the df_4_cli_privacy_clean relevant variables

#suddivisione dei clienti in base alla variabile flag_privacy_1
df_dist_flag_privacy_1 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarise(TOT_FLAG_PRIVACY_1 = n())
  
plot_df_dist_flag_privacy_1 <- (
  ggplot(data=df_dist_flag_privacy_1
         , aes(x=FLAG_PRIVACY_1, y=TOT_FLAG_PRIVACY_1)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_dist_flag_privacy_1



#suddivisione dei clienti in base alla variabile flag_privacy_2
df_dist_flag_privacy_2 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarise(TOT_FLAG_PRIVACY_2 = n())

plot_df_dist_flag_privacy_2 <- (
  ggplot(data=df_dist_flag_privacy_2
         , aes(x=FLAG_PRIVACY_2, y=TOT_FLAG_PRIVACY_2)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_dist_flag_privacy_2



#suddivisione dei clienti in base alla variabile flag_direct_mkt
df_dist_flag_direct_mkt <- df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarise(TOT_FLAG_DIRECT_MKT = n())

plot_df_dist_flag_direct_mkt <- (
  ggplot(data=df_dist_flag_direct_mkt
         , aes(x=FLAG_DIRECT_MKT, y=TOT_FLAG_DIRECT_MKT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_dist_flag_direct_mkt
#----------------------------------------------------------------------


#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)

