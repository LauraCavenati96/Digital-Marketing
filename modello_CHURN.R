#### modello di churn ####

#voglio calcolare i giorni tra due acquisti consecutivi di uno stesso id
#per poi farne la media e calcolare  il PURCHASE TIME SCALE

scontrino_data<-df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, ID_CLI, TIC_DATE) %>%
  summarise(IMPORTO_LORDO=sum(IMPORTO_LORDO)) %>%
  arrange(ID_CLI, TIC_DATE)
#può succedere che uno acquista e poi restituisce lo stesso giorno. Bisogna tenerne conto facendo la media!!

difftime<-ddply(scontrino_data, "ID_CLI", transform, inter.time = c(NA, diff(TIC_DATE)))

#faccio la media per ogni customer 
difftimegroup<-difftime %>%
  group_by(ID_CLI) %>%
  summarize(mediatime=mean(inter.time, na.rm = TRUE))

#trovo il 90% percentile
purchasetimescale<-quantile(difftimegroup$mediatime, .9, na.rm = TRUE)
purchasetimescale

#A customer is churned at a certain reference date if the customer does not purchase in the interval after the
#reference date corresponding to the Purchase Time Scale (=4 mesi)
#scelgo come holdout un periodo di 4 mesi basandomi sui valori della curva di riacquisto 
#e perchè in questo modo il 27% dei clienti risulta churner, valore accettabile.

#holdout period= 4 mesi
referencedate=as.Date('2019-01-01')

#lookback period before the reference date=8 mesi
startlookbackdate<-as.Date('2018-05-01')
 

data_last_purch<-df_7_tic_clean_final %>%
  group_by(ID_CLI) %>%
  summarize(LAST_DATE=max(TIC_DATE)) 

churn_data<-df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, ID_CLI, TIC_DATE) %>%
  summarize(IMPORTO_LORDO=sum(IMPORTO_LORDO))

churn_data <- churn_data %>%
  left_join(data_last_purch
              , by = 'ID_CLI') %>%
  mutate(Churn = ifelse(LAST_DATE< referencedate, 1, 0))

#individuo quanti clienti sono churn
churn_data %>%
  group_by(Churn) %>%
  count()
# 
  
#divido il dataset in lookback e holdout
holdout_data1<-churn_data %>%
  filter(TIC_DATE>=referencedate)

lookback_data1<-churn_data %>%
  filter(TIC_DATE>=startlookbackdate & TIC_DATE<referencedate)

churn_predictors_data<-lookback_data1 %>%
  group_by(ID_SCONTRINO, ID_CLI, TIC_DATE) %>%
  summarize(IMPORTO_LORDO=sum(IMPORTO_LORDO))

#costruisco il modello RFM che mi permette di ottenere le variabili transaction_count, amount e recency_days
#che mi serviranno a costruire il modello di churn
lookback_data1_rfm<-rfm_table_order(lookback_data1 , ID_CLI, TIC_DATE, IMPORTO_LORDO, referencedate)
lookback_data1_rfm_result<-lookback_data1_rfm$rfm

lookback_data1_rfm_result<-lookback_data1_rfm_result %>%
  left_join(lookback_data1 %>%
              select(ID_CLI, Churn), by = c("customer_id"="ID_CLI"))


intrain <- createDataPartition(lookback_data1_rfm_result$Churn, p = 0.7, list = FALSE)
training <- lookback_data1_rfm_result[intrain, ]
testing <- lookback_data1_rfm_result[- intrain, ]



#### Fitting the Logistic Regression Model ####
LogModel <- glm(Churn ~ transaction_count+amount+recency_days, family = binomial(link = "logit"), data = training)
print(summary(LogModel))

log_pred <- predict(LogModel, newdata = testing, type = "response")
log_pred <- ifelse(log_pred > 0.5, 1, 0)
log_t<-table(testing$Churn, log_pred)

log_accuracy<-sum(diag(log_t))/(sum(log_t))
log_precision<-(log_t[2, 2])/(log_t[2, 2]+log_t[1, 2])
log_recall<-(log_t[2, 2])/(log_t[2, 2]+log_t[2, 1])
log_Fmeasure<-(2*log_precision*log_recall)/(log_precision+log_recall)



#### Fitting the Decision Tree Model ####
tree <- ctree(Churn ~ transaction_count+amount+recency_days, training)
tree_pred <- predict(tree, testing)
tree_pred <- ifelse(tree_pred > 0.5, 1, 0)
tree_t<-table(Predicted = tree_pred, Actual = testing$Churn)

tree_accuracy<-sum(diag(tree_t))/(sum(tree_t))
tree_precision<-(tree_t[2, 2])/(tree_t[2, 2]+tree_t[1, 2])
tree_recall<-(tree_t[2, 2])/(tree_t[2, 2]+tree_t[2, 1])
tree_Fmeasure<-(2*tree_precision*tree_recall)/(tree_precision+tree_recall)



#### Fitting the Random Forest Model ####
#MOLTO LENTO!!!(non sono mai riuscita a farlo finire!!!)
if(FALSE){
rfModel <- randomForest(Churn ~ transaction_count+amount+recency_days, data = training)
forest_pred <- predict(rfModel, testing)
forest_pred <- ifelse(forest_pred > 0.5, 1, 0)
forest_t<-table(Predicted = forest_pred, Actual = testing$Churn)

forest_accuracy<-sum(diag(forest_t))/(sum(forest_t))
forest_precision<-(forest_t[2, 2])/(forest_t[2, 2]+forest_t[1, 2])
forest_recall<-(forest_t[2, 2])/(forest_t[2, 2]+forest_t[2, 1])
forest_Fmeasure<-(2*forest_precision*forest_recall)/(forest_precision+forest_recall)
}



#### Fitting the Naive Bayes Model ####
naiveModel <- naiveBayes(as.factor(Churn) ~ transaction_count+amount+recency_days, data = training)
naive_pred <- predict(naiveModel, testing)
naive_t<-table(Predicted = naive_pred, Actual = testing$Churn)

naive_accuracy<-sum(diag(naive_t))/(sum(naive_t))
naive_precision<-(naive_t[2, 2])/(naive_t[2, 2]+naive_t[1, 2])
naive_recall<-(naive_t[2, 2])/(naive_t[2, 2]+naive_t[2, 1])
naive_Fmeasure<-(2*naive_precision*naive_recall)/(naive_precision+naive_recall)



#### Fitting the Neural Network Model ####
nnModel <- nnet(Churn ~ transaction_count+amount+recency_days, data = training, size = 3)
neural_pred <- predict(nnModel, testing)
neural_pred <- ifelse(neural_pred > 0.5, 1, 0)
neural_t<-table(Predicted = neural_pred, Actual = testing$Churn)

neural_accuracy<-sum(diag(neural_t))/(sum(neural_t))
neural_precision<-(neural_t[2, 2])/(neural_t[2, 2]+neural_t[1, 2])
neural_recall<-(neural_t[2, 2])/(neural_t[2, 2]+neural_t[2, 1])
neural_Fmeasure<-(2*neural_precision*neural_recall)/(neural_precision+neural_recall)