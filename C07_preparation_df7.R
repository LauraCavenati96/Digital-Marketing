#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo


## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#--------------------------------------------------------------------------------
#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO


## compute aggregate
df7_dist_importosconto_byrep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_byrep <- df7_dist_importosconto_byrep %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_byrep

## plot aggregate
plot_df7_dist_importo_byrep <- (
  ggplot(data=df7_dist_importosconto_byrep,
           aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram( fill="white", alpha=0.5, bins=20) +
    theme_minimal()
)

plot_df7_dist_importo_byrep

## plot aggregate
plot_df7_dist_sconto_byrep <- (
  ggplot(data=df7_dist_importosconto_byrep,
           aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5, bins=20) +
    theme_minimal()
)

plot_df7_dist_sconto_byrep
#mia interpretazione: ci sono reparti in cui la spesa (sia importo lordo che sconto!) è molto maggiore rispetto ad altri



# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

## compute aggregate
df7_dist_importosconto_byart<- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO) %>%
  summarize(num_TICs = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_importosconto_byart


# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

## compute aggregate
df7_dist_importosconto_bycli<- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = mean(IMPORTO_LORDO), SCONTO = mean(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_bycli <- df7_dist_importosconto_bycli %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_bycli

## plot aggregate
plot_df7_dist_importo_bycli <- (
  ggplot(data=df7_dist_importosconto_bycli %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5, bins=10) +
    theme_minimal()
)

plot_df7_dist_importo_bycli





## plot aggregate
plot_df7_dist_sconto_bycli <- (
  ggplot(data=df7_dist_importosconto_bycli %>%
           filter((SCONTO > -75) & (SCONTO < 150))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5, bins=10) +
    theme_minimal()
)

plot_df7_dist_sconto_bycli
#la maggior parte dei clienti hanno speso in totale poco



# compute the distribution of customers by number of purchases (as described in the slides)
df7_customers_bynumberof_purchases<- df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%
  summarize(num_purch = n_distinct(ID_SCONTRINO)) %>%
  group_by(num_purch) %>%
  summarize(NUM_CLI =  n_distinct(ID_CLI)) %>%
  mutate(num_purch_clean = if_else(num_purch < 15, num_purch, 15L)) %>%
  group_by(num_purch_clean) %>%
  summarise(NUM_CLI=sum(NUM_CLI)) %>%
  arrange(desc(num_purch_clean)) %>%
  mutate(SUM_CLI=cumsum(NUM_CLI)) %>%
  arrange(num_purch_clean) %>%
  mutate(SUM_CLI_diff=c(0, SUM_CLI[-1]/lag(SUM_CLI)[-1]))

  
df7_customers_bynumberof_purchases


plot_customers_bynumberof_purchases <- (
  ggplot() +
    geom_bar(data=df7_customers_bynumberof_purchases,
             aes(x=num_purch_clean, y=NUM_CLI), stat="identity", fill="steelblue") +
    geom_point(data=df7_customers_bynumberof_purchases,
              aes(x=num_purch_clean, y=SUM_CLI_diff*70599), size = 2, color = "red")+
    scale_y_continuous(name = "num cli", 
                       sec.axis=sec_axis(~./70599, name="% confirmed")) + 
    theme(
      axis.title.y = element_text(color = "grey"),
      axis.title.y.right = element_text(color = "red"))
)
plot_customers_bynumberof_purchases




# compute the days for next purchase curve (as described in the slides)
data<-df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, ID_CLI, TIC_DATE) %>%
  summarize(IMPORTO_LORDO=sum(IMPORTO_LORDO)) %>%
  arrange(ID_CLI, TIC_DATE)

difftime<-ddply(data, "ID_CLI", transform, inter.time = c(NA, diff(TIC_DATE)))

#faccio la media per ogni customer 
difftimegroup<-difftime %>%
  group_by(ID_CLI) %>%
  summarize(N = n(), mediatime=mean(inter.time, na.rm = TRUE)) %>%
  filter(!is.na(mediatime))

difftimegroup

plot_difftimegroup <- (
  ggplot(data=difftimegroup,
         aes( x=mediatime)) +
    stat_ecdf() +
    coord_fixed(ratio = 100, xlim =c(0, 100), expand=FALSE) +
    labs(x="giorni medi al prossimo acquisto", y="% clienti", title ="curva di riacquisto") +
    theme_light()
)

plot_difftimegroup
#nota bene: considero solo i clienti che hanno fatto almeno due acquisti!! per gli altri (giustamente) non ha senso parlare di media!

#--------------------------------------------------------------------------------

#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)

