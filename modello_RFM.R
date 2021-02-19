#### MODELLO RFM ####

#Reference date per suddividere clienti in active e inactive
PurchaseTimeScaleRFM<-as.Date('2018-11-01')


df_last_purchase<- df_7_tic_clean %>%
  group_by(ID_CLI) %>%
  summarize(TIC_DATE = max(TIC_DATE))

#### clienti inactive ####
df_inactive<- df_last_purchase %>%
  filter(TIC_DATE < PurchaseTimeScaleRFM)

#### clienti active ####
df_active<- df_last_purchase %>%
  filter(TIC_DATE >= PurchaseTimeScaleRFM)
  
rfm_data<-df_7_tic_clean_final %>%
  filter (ID_CLI %in% df_active$ID_CLI) %>%
  group_by(ID_SCONTRINO, ID_CLI, TIC_DATE) %>%
  summarize(IMPORTO_LORDO=sum(IMPORTO_LORDO))


#### RFM model 1 (imposto il parametro numero di bins=3) ####
#data in cui eseguo le mie analisi
analysis_date <- as.Date('2019-04-30')

#suddivido i clienti attivi attraverso un modello RFM preimpostato in R
rfm3<-rfm_table_order(rfm_data , ID_CLI, TIC_DATE, IMPORTO_LORDO, analysis_date,
                      recency_bins = 3, frequency_bins = 3, monetary_bins = 3)

rfm3_result<-rfm3$rfm %>%
  mutate(segment=0)

#rfm score e relativa classe di apartenenza
Diamond<-c(333, 233)
Gold<-c(232, 332, 133)
Silver<-c(231, 331, 132, 223, 323)
Bronze<-c(131, 222, 322, 113, 123)
Copper<-c(221, 321, 112, 122, 213, 313)
Tin<-c(111, 121, 212, 312)
Cheap<-c(211, 311)

rfm3_result$segment[which(rfm3_result$rfm_score %in% Diamond)] = "Diamond"
rfm3_result$segment[which(rfm3_result$rfm_score %in% Gold)] = "Gold"
rfm3_result$segment[which(rfm3_result$rfm_score %in% Silver)] = "Silver"
rfm3_result$segment[which(rfm3_result$rfm_score %in% Bronze)] = "Bronze"
rfm3_result$segment[which(rfm3_result$rfm_score %in% Copper)] = "Copper"
rfm3_result$segment[which(rfm3_result$rfm_score %in% Tin)] = "Tin"
rfm3_result$segment[which(rfm3_result$rfm_score %in% Cheap)] = "Cheap"

#numero di clienti in ogni gruppo 
rfm3_result %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

#alcuni grafici a barre per rappresentare la distribuzione dei customers tra i segmenti
rfm_plot_median_recency(rfm3_result)
rfm_plot_median_frequency(rfm3_result)
rfm_plot_median_monetary(rfm3_result)





#### RFM model 2 (imposto il parametro numero di bins=5) ####

#suddivido i clienti attivi attraverso un modello RFM preimpostato in R
rfm5<-rfm_table_order(rfm_data , ID_CLI, TIC_DATE, IMPORTO_LORDO, analysis_date)
rfm5_result<-rfm5$rfm

rfm_fm_plot(rfm5)
rfm_rm_plot(rfm5)
rfm_rf_plot(rfm5)

rfm_bar_chart(rfm5)

#rfm score e relativa classe di apartenenza
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist", "New Customers", "Promising", "Need Attention",
                   "About To Sleep", "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segments5<-rfm_segment(rfm5, segment_names, recency_lower, recency_upper,
            frequency_lower, frequency_upper, monetary_lower, monetary_upper)

segments5 %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

#alcuni grafici a barre per rappresentare la distribuzione dei customers tra i segmenti
rfm_plot_median_recency(segments5)
rfm_plot_median_frequency(segments5)
rfm_plot_median_monetary(segments5)
