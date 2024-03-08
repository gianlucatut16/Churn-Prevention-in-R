############ LIBRARIES #############
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(visdat)
library(tidyr)
library(naniar)
library(corrplot)
library(reshape2)
library(psych)
library(randomForest)
library(caret)
library(devtools)
library(factoextra)
library(FactoMineR)
library(ggpubr)
library(doParallel)
library(nestedcv)

setwd("C:/Users/gltut/Desktop/Corsi/Statistical Data Analisys/FATER")

doParallel::registerDoParallel(cores = 7)

############ FUNCTIONS #############
# Function for data visualization (now only to check the missing data)

# Function needed by missing_values
toBinaryMatrix <- function(df){
  m<-c()
  for(i in colnames(df)){
    x<-sum(is.na(df[,i]))
    # missing value count
    m<-append(m,x)
    # non-missing value count
    m<-append(m,nrow(df)-x) 
  }
  
  # adding column and row names to matrix
  a<-matrix(m,nrow=2)
  rownames(a)<-c("TRUE","FALSE")
  colnames(a)<-colnames(df)
  
  return(a)
}

# Function to visualizee NA values
# Il problema per cui non fa 3 plot si risolverebbe salvando i tre diversi plot
missing_values <- function(x){
  if (sum(is.na(x) != 0)){
  print(dim(x))
  print('The number of missing values for each column:')
  print(colSums(is.na(x)))
  binMat = toBinaryMatrix(x)
  
  
  barplot(binMat,
          main = "Missing values in all features",xlab = "Frequency",
          col = c("#ff9999","#4dffd2"))
  
  gg_miss_var(x, show_pct = TRUE)
  
  }
  else{
    print('No missing values in this dataframe')
  }
}



######### DATASETS #########
anagrafica <- read_csv("data/anagrafica.csv", 
                       col_types = cols(Provincia = col_skip(), 
                       SiglaProvincia = col_skip(), Comune = col_skip()))

accessi_app <- read_csv("data/accessi_app.csv",
                        col_types = cols(source = col_skip()))

conversione_ean_prodotto <- read_csv("data/conversione_ean_prodotto.csv", 
                                     col_types = cols(REFERENZA_DES = col_skip(), 
                                                      SEGMENTO_DES = col_skip()))

missioni_players <- read_csv("data/missioni_players.csv", 
                             col_types = cols(type = col_skip()))

premi_mamme <- read_csv("data/premi_mamme.csv", 
                        col_types = cols(deliveryMode = col_skip()))

prodotti_caricati <- read_csv('data/prodotti_caricati.csv')


##### DATA VISUALIZATION ON DATASETS ######

## Anagrafica normalization

if (sum(is.na(anagrafica)) != 0){
  gg_miss_upset(anagrafica)
  gg_miss_var(anagrafica, show_pct = TRUE)
  vis_miss(anagrafica)
}

# Survey time limits
survey_start = as.Date(min(accessi_app$updated_at))
survey_end = as.Date(max(accessi_app$updated_at))
survey_period = survey_end - survey_start   # 180 gg

## accessi_app normalization 
missing_values(accessi_app)

# accessi per mese
accessi_app_month = accessi_app
accessi_app_month$updated_at = month(accessi_app_month$updated_at)

accessi_month = accessi_app_month %>%
  group_by(updated_at) %>%
  summarise(accessi_mese = n())

ggplot(data=accessi_month, aes(x=updated_at, y=accessi_mese)) +
  geom_bar(stat="identity", width=0.5, fill = 'black')

## conversione_ean_prodotto normalization
missing_values(conversione_ean_prodotto)

# Barplot OCCUSO_DES per mostrare che si parla principalmente di pannolini
ggplot(conversione_ean_prodotto,
       aes(x=reorder(OCCUSO_DES, OCCUSO_DES, function(x)-length(x)))) +
       geom_bar(color = 'black', fill = 'white') +
       labs(title = 'Plot type of product', x = 'Type', y = 'Frequency')

graph<-ggplot(active_users, aes(x=num_prod, y = points_prodotti))+
  geom_point()+
  labs(title = 'Linear regression cost EU vs Type of course', x = 'Type of course', y = 'Cost EU')
graph <- graph + geom_smooth(method="lm", col="blue")
graph <- graph +
  stat_regline_equation(label.x = 1, label.y = 1)
graph
# Barplot per frequenza delle diverse TIER
ggplot(conversione_ean_prodotto,
       aes(x = TIER)) +
  geom_bar() +
  labs(title = 'Plot TIER of uploaded products', x = 'TIER', y = 'Frequency')

## missioni_players normalization
missing_values(missioni_players)
missioni_players = filter(missioni_players, created_at >= survey_start)
missioni_players$created_at = as.Date(missioni_players$created_at)

# Istogramma per distribuzioni dei punti delle missioni effettuate
ggplot(missioni_players, aes(x = points)) +   
       geom_histogram(color = 'black', fill = 'white')+
       labs(title = 'Histogram of mission points', x = 'Points', y = 'Frequency')      

## premi_mamme normalization
missing_values(premi_mamme)
premi_mamme = filter(premi_mamme, datarichiestapremio >= survey_start)

# barplot per frequency di tipo di premio
ggplot(premi_mamme,               
       aes(x = tipopremio))+
  geom_bar(color = 'black', fill = 'grey')+
  theme_classic()        

# Boxplot per mostrare la distribuzione dei puti che ci voglioni per i diversi tipi di premio
ggplot(premi_mamme, aes(x = tipopremio, y = puntipremio))+ 
      geom_boxplot(color = 'black', fill = 'grey')+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
      theme_classic()+
      labs(title = 'Boxplot punti premi rispetto al tipo', x = 'Type', y = 'Punti')



## prodotti_caricati normalization
#missing_values(prodotti_caricati)
prodotti_caricati = filter(prodotti_caricati, created_at >= survey_start)

# Istogramma per mostrare la distribuzione dei punti ottenuti per caricamento prodotti
ggplot(prodotti_caricati, aes(x = points))+    
  geom_histogram(color = 'black', fill = 'white')+
  labs(title = 'Histogram of uploaded products points', x = 'Points', y = 'Frequency')

sum(is.na(prodotti_caricati))





################### DATA PREPROCESSING ###################


# Merge prodotti_caricati e conversione_ean_prodotti
prodotti = merge(prodotti_caricati, conversione_ean_prodotto, all.x = TRUE)

ggplot(subset(prodotti, !is.na(TIER)), aes(x = reorder(TIER, points), y = points)) + 
  geom_boxplot(color = 'black', fill = 'grey', width = 0.3, na.rm = TRUE) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  labs(title = 'Boxplot punti prodotti', x = 'TIER', y = 'Punti')

## Filtro il dataset anagrafica prendendo solo i player che hanno effettuato accessi durante la raccolta dati

# Last access of every player
last_access = accessi_app %>%
group_by(id_player)%>%
summarise(last_access = max(updated_at))

# Only players that have made at least one access
active_users = merge(last_access, anagrafica, all.x = TRUE)

### Aggiungo le informazioni degli altri dataset al dataset di utenti attivi

## Data ultimo accesso rispetto fine survey   
active_users$last_time =  survey_end - as.Date(active_users$last_access) 
active_users$last_time = as.double(active_users$last_time)

# Differenza primo e ultimo accesso per ogni player (da accessi_app)
app_time = accessi_app %>%
  group_by(id_player) %>%
  summarise(tempo_app = as.Date(max(updated_at)) - as.Date(min(updated_at)))

# agiungo la variabile tempo app e la converto in double 
active_users = merge(active_users, app_time, all.x = TRUE)
active_users$tempo_app = as.double(active_users$tempo_app)

## Numero di prodotti caricati da ogni player
prod_per_user = prodotti_caricati %>%
  group_by(id_player)%>%
  summarise(num_prod = n())

active_users = merge(active_users, prod_per_user, all.x = TRUE)


## Oltre al numero dei prodotti aggiungo 4 colonne TIER1, TIER2, TIER3 per 
## mostrare i sottogruppi di prodotti caricati per ogni player

# Variabile categorica TIER
TIER_num = prodotti %>%
  group_by(id_player, TIER) %>%
  summarise(num = n())

TIER1_num = filter(TIER_num, TIER == 'TIER1')   # metti tutto dentro funzione
colnames(TIER1_num)[3] = 'TIER1'
TIER1_num = TIER1_num[-2]


TIER2_num = filter(TIER_num, TIER == 'TIER2')
colnames(TIER2_num)[3] = 'TIER2'
TIER2_num = TIER2_num[-2]


TIER3_num = filter(TIER_num, TIER == 'TIER3')
colnames(TIER3_num)[3] = 'TIER3'
TIER3_num = TIER3_num[-2]

Type_num = prodotti %>%
  group_by(id_player, OCCUSO_DES) %>%
  summarise(num = n())

Pannolini_num = filter(Type_num, OCCUSO_DES == 'Pannolini')
colnames(Pannolini_num)[3] = 'Pannolini'
Pannolini_num = Pannolini_num[-2]

Wipes_num = filter(Type_num, OCCUSO_DES == 'Wipes')
colnames(Wipes_num)[3] = 'Wipes'
Wipes_num = Wipes_num[-2]

active_users  = merge(active_users, TIER1_num, all.x = TRUE)
active_users  = merge(active_users, TIER2_num, all.x = TRUE)
active_users  = merge(active_users, TIER3_num, all.x = TRUE)
active_users = merge(active_users, Pannolini_num, all.x = TRUE)
active_users = merge(active_users, Wipes_num, all.x = TRUE)

## Per ogni player aggiungo la variabile che indica quanti premi ha richiesto (da premi_mamme)

rewards = premi_mamme %>%
  group_by(id_player)%>%
  summarise(num_reward = n())

active_users = merge(active_users, rewards, all.x = TRUE)

## Oltre al numero dei premi aggiungo 3 colonne gift, special, gift per 
## mostrare i sottogruppi dei premi richiesti da ogni player

tipo_premio = premi_mamme %>%
  group_by(id_player, tipopremio) %>%
  summarise(num = n())

basic = filter(tipo_premio, tipopremio == 'basic')
colnames(basic)[3] = 'basic'
basic = basic[-2]

special = filter(tipo_premio, tipopremio == 'special')
colnames(special)[3] = 'special'
special = special[-2]

active_users  = merge(active_users, basic, all.x = TRUE)
active_users  = merge(active_users, special, all.x = TRUE)

## Per ogni player prendo il numero di accessi nel periodo di survey
access_freq = accessi_app %>%
  group_by(id_player)%>%
  summarise(num_access = n())

active_users = merge(active_users, access_freq, all.x = TRUE)

## Numero di missioni per ogni player
missions_num = missioni_players %>%
  group_by(id_player)%>%
  summarise(num_missioni = n())

active_users = merge(active_users, missions_num, all.x = TRUE)

misstype_num = missioni_players %>%
  group_by(id_player, subType) %>%
  summarise(num = n())

cib_num = filter(misstype_num, subType == 'cib')   # metti tutto dentro funzione
colnames(cib_num)[3] = 'cib'
cib_num = cib_num[-2]


double_num = filter(misstype_num, subType == 'double')
colnames(double_num)[3] = 'double'
double_num = double_num[-2]


ticket_num = filter(misstype_num, subType == 'ticket-punti')
colnames(ticket_num)[3] = 'ticket-punti'
ticket_num = ticket_num[-2]

active_users  = merge(active_users, cib_num, all.x = TRUE)
active_users  = merge(active_users, double_num, all.x = TRUE)
active_users  = merge(active_users, ticket_num, all.x = TRUE)



## Somma punti ottenuti dai prodotti caricati
points_num = prodotti_caricati %>%
  group_by(id_player)%>%
  summarise(points_prodotti = sum(points))

active_users = merge(active_users, points_num, all.x = TRUE)

## Somma punti ottenuti dalle missioni
points_mission = missioni_players %>%
  group_by(id_player)%>%
  summarise(points_missioni = sum(points))

active_users = merge(active_users, points_mission, all.x = TRUE)

## Somma punti spesi per richiesta premi
points_expense = premi_mamme %>%
  group_by(id_player)%>%
  summarise(points_premi = sum(puntipremio))

active_users = merge(active_users, points_expense, all.x = TRUE)

# Frequenze

missing_values(test)
active_users = filter(active_users, tempo_app > 7 & !is.na(ETA_MM_BambinoTODAY))
active_users[8:length(active_users)][is.na(active_users[8:length(active_users)])] = 0


active_users$week_access = round(active_users$num_access / (active_users$tempo_app/7), 2)

# Missioni
active_users$week_mission = round(active_users$num_missioni / (active_users$tempo_app/7), 2)
active_users$week_cib = round(active_users$cib / (active_users$tempo_app/7), 2)
active_users$week_double = round(active_users$double / (active_users$tempo_app/7), 2)
active_users$week_ticket = round(active_users$ticket / (active_users$tempo_app/7), 2)
# Prodotti caricati
active_users$week_prod = round(active_users$num_prod/(active_users$tempo_app/7), 2)
active_users$week_TIER1 = round(active_users$TIER1/(active_users$tempo_app/7), 2)
active_users$week_TIER2 = round(active_users$TIER2/(active_users$tempo_app/7), 2)
active_users$week_TIER3 = round(active_users$TIER3/(active_users$tempo_app/7), 2)

# Premi richiesti
active_users$week_reward = round(active_users$num_reward/(active_users$tempo_app/7), 2)
active_users$week_special = round(active_users$special/(active_users$tempo_app/7), 2)
active_users$week_basic = round(active_users$basic/(active_users$tempo_app/7), 2)

# Punti ottenuti e spesi
active_users$week_prodpoints = round((active_users$points_prodotti/100)/(active_users$tempo_app/7), 2)
active_users$week_misspoints = round((active_users$points_missioni/100)/(active_users$tempo_app/7), 2)
active_users$week_rewpoints = round((active_users$points_premi/100)/(active_users$tempo_app/7), 2)

############# UNSUPERVISED LEARNING #############
# test = active_users[, c(1,5,8,9,10,14,17,18,22,23,24)]
#  
# pca.data <- PCA(test[, -1], scale.unit = TRUE, graph = FALSE)
# summary(pca.data)
# res.hcpc <- HCPC(pca.data, graph = FALSE)
# 
# data <- active_users
# data$clust <- res.hcpc$data.clust$clust
# 
# dim(data)

data = filter(data, ETA_MM_BambinoTODAY >-1 | num_prod != 0 | num_missioni != 0 | num_reward != 0)

nord_italia = c("VALLE D'AOSTA", "PIEMONTE", "LOMBARDIA", "EMILIA-ROMAGNA", "TRENTINO-ALTO ADIGE", "LIGURIA", "VENETO", "FRIULI-VENEZIA GIULIA")
centro_italia = c("UMBRIA", "LAZIO", "MARCHE", "TOSCANA")
sud_italia = c("ABRUZZO", "CAMPANIA", "BASILICATA", "CALABRIA", "PUGLIA", "MOLISE", "SICILIA", "SARDEGNA")


data$Regione[data$Regione %in% nord_italia] = "NORD"
data$Regione[data$Regione %in% centro_italia] = "CENTRO"
data$Regione[data$Regione %in% sud_italia] = "SUD"
# 
# 
# ################## BOXPLOTS CLUSTERS ###################
# # Età bambino
# ggplot(data, aes(x = clust, y = ETA_MM_BambinoTODAY-6)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # last_time
# ggplot(data, aes(x = clust, y = last_time)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # tempo_app
# ggplot(data, aes(x = clust, y = tempo_app)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
#  
# # Num accessi
# ggplot(data, aes(x = clust, y = num_access)) +
#   geom_boxplot() +
#   ylim(0,200) +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
#  
# # Num missioni
# ggplot(data, aes(x = clust, y = num_missioni)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # Punti prodotti
# ggplot(data, aes(x = clust, y = points_prodotti)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # Punti missioni
# ggplot(data, aes(x = clust, y = points_missioni)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # Punti premi
# ggplot(data, aes(x = clust, y = points_premi)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # Media accessi
# ggplot(data, aes(x = clust, y = week_access)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # week_access
# ggplot(data, aes(x = clust, y = week_mission)) +
#   geom_boxplot() +
#   ylim(0,1) +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # week_prod
# ggplot(data, aes(x = clust, y = week_prod)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # week_reward
# ggplot(data, aes(x = clust, y = week_reward)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # points_prodotti
# ggplot(data, aes(x = clust, y = week_prodpoints)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # points_missioni
# ggplot(data, aes(x = clust, y = week_misspoints)) +
#   geom_boxplot() +
#   ylim(0,10) +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # points_premi
# ggplot(data, aes(x = clust, y = week_rewpoints)) +
#   geom_boxplot() +
#   ylim(0,10) +
#   stat_summary(fun.y=mean, geom="point", shape=10, size=4)
# 
# # num_prod e TIER
# dfm_prod_hcpc <- melt(data[,c('clust','num_prod', 'TIER1','TIER2', 'TIER3')],id.vars = 1)
# 
# ggplot(dfm_prod_hcpc, aes(x = clust,y = value, color = variable)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", position = position_dodge(0.75), shape=23, size=4)
# 
# # num_prod e TIER in frequenza
# dfm_prod_hcpc <- melt(data[,c('clust','week_prod', 'week_TIER1','week_TIER2', 'week_TIER3')],id.vars = 1)
# 
# ggplot(dfm_prod_hcpc, aes(x = clust,y = value, color = variable)) +
#   geom_boxplot() +
#   ylim(0,10) +
#   stat_summary(fun.y=mean, geom="point", position = position_dodge(0.75), shape=23, size=4)
# 
# # num_reward and types
# dfm_rew_hcpc <- melt(data[,c('clust','num_reward', 'basic', 'special')],id.vars = 1)
# 
# ggplot(dfm_rew_hcpc, aes(x = clust,y = value, color = variable)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", position = position_dodge(0.75), shape=23, size=4)
# 
# # num_reward and types in frequency
# dfm_rew_hcpc <- melt(data[,c('clust','week_reward', 'week_basic', 'week_special')],id.vars = 1)
# 
# ggplot(dfm_rew_hcpc, aes(x = clust,y = value, color = variable)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", position = position_dodge(0.75), shape=23, size=4)

############# CHURN #############

data$days_toaccess = round(data$tempo_app / data$num_access, 2)

data$churn = 0

for (i in 1:dim(data)[1]){
  ultimo_acc  = data$last_time[i]
  media_accessi = data$days_toaccess[i] + sd(data$days_toaccess)
  if ( ultimo_acc > media_accessi){
    data$churn[i] = "CHURN"
  }
  else{
    data$churn[i] = 'NOCHURN'
  }
}

# a = table(data$clust, data$churn)
# 
# a
# pct = c(a[1,2]/(a[1,1] + a[1,2]), a[2,2]/(a[2,1] + a[2,2]), a[3,2]/(a[3,1] + a[3,2]))
# pct = round(pct,2)
# pct

########## ARMANDO ############
# dfm_rew_hcpc <- melt(data[,c('clust','num_prod', 'Pannolini', 'Wipes')],id.vars = 1)
# 
# ggplot(dfm_rew_hcpc, aes(x = clust,y = value, color = variable)) +
#   geom_boxplot() +
#   stat_summary(fun.y=mean, geom="point", position = position_dodge(0.75), shape=23, size=4)
# 
# ggplot(data, aes(x = Wipes))+
#   geom_histogram(binwidth = 1, color = 'black', fill = 'white')
# 
# 
# 
# 
# ######################
# View(data)

# globali = c("last_access", "ETA_MM_BambinoTODAY", "Regione","last_time", "tempo_app", "num_prod", "TIER1", "TIER2", "TIER3", "num_reward", "basic", "special", "num_access", "num_missioni", "cib", "double", "ticket-punti", "points_prodotti", "points_missioni", "points_premi", "days_toaccess", "churn")
# globals_data = select(data, globali)
# 
# # write.csv(globals_data, 'globals_data')
# 
# globali_general = c("last_access", "ETA_MM_BambinoTODAY", "Regione","last_time", "tempo_app", "num_prod", "num_reward", "num_access", "num_missioni", "points_prodotti", "points_missioni", "points_premi", "days_toaccess", "churn")
# globals_general_data = select(data, globali_general)
# # write.csv(globals_general_data, 'globals_general_data')
# 
# frequenze= c("week_access", "week_mission", "week_cib", "week_double", "week_ticket", "week_prod", "week_TIER1", "week_TIER2", "week_TIER3", "week_reward", "week_special", "week_basic", "week_prodpoints", "week_misspoints", "week_rewpoints", "churn")
# frequencies_data = select(data, frequenze)
# # write.csv(frequencies_data, 'frequencies_data')

frequenze_general = c("week_access", "week_mission", "week_prod", "week_reward", "week_prodpoints", "week_misspoints", "week_rewpoints", "churn")
frequencies_general_data = select(data, frequenze_general)

pairs.panels(frequencies_general_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             smooth = FALSE,
             ellipses = FALSE)


library(mlbench)
library(caret)
library(ranger)
library(tidymodels)
library(yardstick)
library(glmnet)
library(dplyr)
library(forcats)

# Load Dataset
dataset <- frequencies_general_data

dataset$churn = as.factor(dataset$churn)
x <- dataset[,1:7]
y <- dataset[,8]




folds <- 5
bal_acc_rf=c()
bal_acc_dt=c()
bal_acc_lr=c()
#Definizione dei dataset dell'outer loop
cvIndex <- createFolds(y, folds, returnTrain = T)


for (i in 1:length(cvIndex)) {
  
  #Definizione dei dataset da usare nell'inner loop
  dataset_train_outer=dataset[cvIndex[[i]],]
  dataset_test_outer=dataset[-cvIndex[[i]],]
  x_train_outer=x[cvIndex[[i]],]
  x_test_outer=x[-cvIndex[[i]],]
  y_train_outer=y[cvIndex[[i]]]
  y_test_outer=y[-cvIndex[[i]]]
  
  
  ####################### Random Forest  ########################
  
  #Inner loop
  print('Tuning forest...')
  
  rf_spec <- rand_forest(
    mtry = tune(), trees = tune(), min_n = tune()
  ) %>%
    set_engine(
      "ranger", num.threads = 7, importance = "impurity"
    ) %>%
    set_mode("classification")
  
  recipe <- recipe(churn~., data=dataset_train_outer)
  
  rf_Wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(rf_spec)
  
  rf_grid <-
    grid_latin_hypercube(
      min_n(),
      mtry(range = c(4, 7)),
      trees(),
      size = 2)
  
  
  folds <- vfold_cv(dataset_train_outer, strata = churn, v = 5)
  set.seed(123)
  tune_res <-
    rf_Wf %>%
    tune_grid(
      resamples = folds, grid = rf_grid,
      metrics = metric_set(yardstick::bal_accuracy)
    )
  
  best_rf <- tune_res %>%
    select_best("bal_accuracy")
  
  rf <- rand_forest(mtry=best_rf$mtry, trees = best_rf$trees, min_n = best_rf$min_n, mode = 'classification')
  
  rf_fit <-
    rf %>%
    set_engine("ranger") %>%
    fit(churn ~., data = dataset_train_outer)
  predictions_rf=predict(rf_fit, dataset_test_outer)
  
  #valore di metrica dell'outer loop
  bal_acc_rf[i]=bal_accuracy_vec(y_test_outer,predictions_rf$.pred_class)
  
  
  ####################### Decision Tree  ########################
  print('Tuning tree...')
  
  dt_spec <-
    decision_tree(
      cost_complexity = tune(),
      tree_depth = tune()
    ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  tree_grid <- grid_regular(cost_complexity(),
                            tree_depth(),
                            levels = 5)
  
  tree_wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(dt_spec)
  
  tree_res <-
    tree_wf %>%
    tune_grid(
      resamples = folds,
      grid = tree_grid,
      metrics = metric_set(yardstick::bal_accuracy)
    )
  best_tree <- tree_res %>%
    select_best("bal_accuracy")
  
  best_tree
  
  dt <- decision_tree(cost_complexity = best_tree$cost_complexity, tree_depth=best_tree$tree_depth, mode = 'classification')
  
  dt_fit <-
    dt %>%
    set_engine("rpart") %>%
    fit(churn ~., data = dataset_train_outer)
  predictions_dt=predict(dt_fit, dataset_test_outer)
  bal_acc_dt[i]=bal_accuracy_vec(y_test_outer,predictions_dt$.pred_class)
  
  
  ####################### Logistic Regression  ########################
  print('Tuning glm...')
  
  lg <- logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet")
  
  
  lg_grid <- data.frame(mixture = seq(0, 0.1, 0.05),
                        penalty = 1)
  
  
  lg_wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(lg)
  
  lg_res <- 
    lg_wf %>% 
    tune_grid(
      resamples = folds,
      grid = lg_grid, 
      metrics = metric_set(yardstick::bal_accuracy)
    )
  best_lg <- lg_res %>%
    select_best("bal_accuracy")
  
  best_lg
  
  lr <- logistic_reg(mixture = best_lg$mixture, penalty = best_lg$penalty)
  
  lr_fit <-
    lr %>%
    set_engine("glmnet") %>%
    fit(churn ~., data = dataset_train_outer)
  predictions_lr=predict(lr_fit, dataset_test_outer)
  bal_acc_lr[i]=bal_accuracy_vec(y_test_outer,predictions_lr$.pred_class)
  
}

bal_acc_rf_final=mean(bal_acc_rf)
bal_acc_dt_final=mean(bal_acc_dt)
bal_acc_lr_final=mean(bal_acc_lr)

bal_acc_rf_final
bal_acc_dt_final
bal_acc_lr_final


