---
title: "GPR#2 Exploratory"
author: "Samuel Benedict"
output:
  html_document:
    df_print: paged
---

# File to Clean Data
 
## Import Libraries
```{r, warning=FALSE, error=FALSE, message=FALSE, output=FALSE}
library(tidyquant)
library(tidyverse)
library(timetk)
library(tibbletime)
library(scales)
library(broom)
library(highcharter)
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library(rpart)
library(rpart.plot)
```


```{r}
# Load data
data <- read_excel("C:/Sam/Files/4B/AFM 423/data_ml.xlsx")

# Rename date column
names(data)[2] <- "date"
```

## Transform Data
$$log(\frac{P_t}{P_{t-1}}) = log(\frac{P_{t}-P_{t-1}+P_{t-1}}{P_{t-1}}) = log(\f

## Import Data
```{r}rac{P_t-P_{t-1}}{P_{t-1}} +1) = log(R1M_{USD} + 1) $$

# Metadata preprocessing
```{r}
data_new <- subset(data, 
                   select = -c(R3M_Usd, R6M_Usd, R12M_Usd))

data_new <- data_new %>%
  na.omit() %>%              # delete NAs
  filter(R1M_Usd < 5) %>%    # delete data where 1M return is >500%
  mutate(R1M_Usd = log(R1M_Usd + 1))   # log transform

# get stocks that appear in every observation (date)
stock_occurence <- table(data_new$stock_id)
stock_ids_short <- as.numeric(names(which(stock_occurence == max(stock_occurence))))
length(stock_ids_short)

# only stocks that appear in every obs
data_new <- data_new %>%
  filter(stock_id %in% stock_ids_short)  

```


# Check return distribution
```{r}
hist(data_new$R1M_Usd, breaks='FD')
```


# Divide into quintiles to form factor regression
```{r}
quintile_smb <- data_new %>%
  select(stock_id, date, Mkt_Cap_6M_Usd, R1M_Usd) %>%
  arrange(date, Mkt_Cap_6M_Usd) %>%
  group_by(date) %>%
  mutate(quantilegroup = ntile(Mkt_Cap_6M_Usd, 5)) %>%
  group_by(date, quantilegroup) %>%
  summarise(quant_return = mean(R1M_Usd), 
            mkt_cap = mean(Mkt_Cap_6M_Usd))

head(quintile_smb, 5)
```

```{r}
quintile_hml <- data_new %>%
  select(stock_id, date, Pb, R1M_Usd) %>%
  arrange(date, Pb) %>%
  group_by(date) %>%
  mutate(quantilegroup = ntile(Pb, 5)) %>%
  group_by(date, quantilegroup) %>%
  summarise(quant_return = mean(R1M_Usd))

quintile_wml <- data_new %>%
  select(stock_id, date, Mom_Sharp_5M_Usd, R1M_Usd) %>%
  arrange(date, Mom_Sharp_5M_Usd) %>%
  group_by(date) %>%
  mutate(quantilegroup = ntile(Mom_Sharp_5M_Usd, 5)) %>%
  group_by(date, quantilegroup) %>%
  summarise(quant_return = mean(R1M_Usd))

quintile_rmw <- data_new %>%
  select(stock_id, date, Ebitda_Margin, R1M_Usd) %>%
  arrange(date, Ebitda_Margin) %>%
  group_by(date) %>%
  mutate(quantilegroup = ntile(Ebitda_Margin, 5)) %>%
  group_by(date, quantilegroup) %>%
  summarise(quant_return = mean(R1M_Usd))

quintile_cma <- data_new %>%
  select(stock_id, date, Capex_Sales, R1M_Usd) %>%
  arrange(date, Capex_Sales) %>%
  group_by(date) %>%
  mutate(quantilegroup = ntile(Capex_Sales, 5)) %>%
  group_by(date, quantilegroup) %>%
  summarise(quant_return = mean(R1M_Usd))
```

# Average monthly returns
```{r}
month_ret <- data_new %>%
  select(stock_id, date, R1M_Usd) %>%
  group_by(date) %>%
  summarize(R1M_Usd = mean(R1M_Usd))

month_ret
```

# Factor returns
```{r}
factor_ret <- data.frame(
  date = unique(quintile_smb$date),
  smb_return = 
    quintile_smb[(quintile_smb$quantilegroup == 1),]$quant_return -
    quintile_smb[(quintile_smb$quantilegroup == 5),]$quant_return,
  hml_return =
    quintile_hml[(quintile_hml$quantilegroup == 1),]$quant_return -
    quintile_hml[(quintile_hml$quantilegroup == 5),]$quant_return,
  wml_return =
    quintile_wml[(quintile_wml$quantilegroup == 5),]$quant_return -
    quintile_wml[(quintile_wml$quantilegroup == 1),]$quant_return,
  rmw_return =
    quintile_rmw[(quintile_rmw$quantilegroup == 5),]$quant_return -
    quintile_rmw[(quintile_rmw$quantilegroup == 1),]$quant_return,
  cma_return =
    quintile_cma[(quintile_cma$quantilegroup == 1),]$quant_return -
    quintile_cma[(quintile_cma$quantilegroup == 5),]$quant_return,
  return_1M = month_ret$R1M_Usd
)
factor_ret
```

# Export CPI Data and Ken French Library
```{r}
cpi <- read.csv("C:/Sam/Files/4B/AFM 423/CPIAUCSL.csv")
ff5 <- read.csv("C:/Sam/Files/4B/AFM 423/F-F_Research_Data_5_Factors_2x3.csv")

cpi = subset(cpi, select = -c(DATE))
ff5 = subset(ff5, select = -c(orig_date))

colnames(cpi)[1] <- "date"
colnames(ff5)[1] <- "date"

cpi <- cpi %>%
  mutate(date = as.Date(as.character(date), format="%m/%d/%Y")) %>%
  subset(select = -c(CPIAUCSL))
ff5_n <- ff5 %>%
  mutate(date = as.Date(as.character(date), format="%m/%d/%Y"))
ff5 <- ff5 %>%
  mutate(date = as.Date(as.character(date), format="%m/%d/%Y")) %>%
  subset(select = c(date, Mkt.RF, RF))

```

# Join with Metadata
```{r}
factor_ret <- factor_ret %>%
  left_join(cpi, by="date") %>%
  subset(select = -c(X12M_log_Inflation)) %>%
  left_join(ff5, by="date") %>%
  subset(select = -c(RF)) %>%
  mutate(Mkt.RF = Mkt.RF/100)

factor_ret
```

# Check the time series of returns
```{r, fig.width=5, fig.height=5}
factor_ret %>%
  gather(key = factor, value = value, - date) %>%
  ggplot(aes(x=date, y=value, color=factor)) +
  geom_line() +  theme_light()
```


# Perform factor diagnostics (FF factor regression)
```{r}
factor_model <- lm(data=factor_ret, 
                   return_1M ~ smb_return + hml_return +
                               wml_return + rmw_return +
                               cma_return + X1M_log_inflation +
                               Mkt.RF)
summary(factor_model)
```

# Backwards selection
```{r}
factor_model2 <- lm(data=factor_ret, 
                   return_1M ~ smb_return + hml_return +
                               wml_return + rmw_return +
                               cma_return +
                               Mkt.RF)
summary(factor_model2)

factor_model3 <- lm(data=factor_ret, 
                   return_1M ~ smb_return + hml_return +
                               wml_return + rmw_return +
                               cma_return)
summary(factor_model3)

```


# Use KF data to perform factor diagnostics
```{r}
fr_kf <- data.frame(
  date = data_new$date,
  return = data_new$R1M_Usd
)
fr_kf <- fr_kf %>%
  group_by(date) %>%
  summarize(avg_ret = mean(return)) %>%
  left_join(ff5_n, by="date") %>%
  left_join(cpi, by='date') %>%
  subset(select = -c(X12M_log_Inflation,RF)) %>%
  mutate(Mkt.RF = Mkt.RF/100)


fm_kf <- lm(data=fr_kf, avg_ret ~ Mkt.RF + SMB + HML + RMW +
                                  CMA + X1M_log_inflation)
summary(fm_kf)
```



# Try it on a specific train/test split (for tuning and testing if the model works)

## Reuniformize data

```{r}
# Reapply uniformization
# Transformation function
norm_unif <- function(v){
  v <- v %>% as.matrix()
  return (ecdf(v)(v))
}

# Create uniformized data
data_unif <- data_new %>%
  group_by(date) %>%          # For each date
  # Apply uniformization
  mutate(Mkt_Cap_3M_Usd = norm_unif(Mkt_Cap_3M_Usd)) %>%  
  mutate(Pb = norm_unif(Pb)) %>%  
  mutate(Mom_Sharp_5M_Usd = norm_unif(Mom_Sharp_5M_Usd)) %>%  
  mutate(Ebitda_Margin = norm_unif(Ebitda_Margin)) %>%  
  mutate(Debtequity = norm_unif(Debtequity)) %>%  
  filter(stock_id %in% stock_ids_short) %>%
  ungroup()
```


## Transform train/test split 
```{r}
sorted_date <- sort(unique(data_unif$date))

data_train <- data_unif %>%
  select(date, stock_id, R1M_Usd, Mkt_Cap_3M_Usd, Pb, Mom_5M_Usd,
         Ebitda_Margin, Capex_Sales) %>%
  filter(date %in% sorted_date[1:185]) %>%
  left_join(ff5, by='date') %>%
  left_join(cpi, by="date") %>%
  arrange(date, stock_id) %>%
  select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
  mutate(Mkt.RF = Mkt.RF/100) 

data_train_x <- data_train %>%
  select(-c(R1M_Usd)) %>%
  as.matrix()
data_train_y <- data_train$R1M_Usd

data_test <- data_unif %>%
  select(date, stock_id, R1M_Usd, Mkt_Cap_3M_Usd, Pb, Mom_5M_Usd,
         Ebitda_Margin, Capex_Sales) %>%
  filter(date %in% sorted_date[186]) %>%
  left_join(ff5, by='date') %>%
  left_join(cpi, by="date") %>%
  arrange(date, stock_id) %>%
  select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
  mutate(Mkt.RF = Mkt.RF/100)

data_test_actual <- data_test$R1M_Usd
data_test_mtx <- data_test %>%
  select(-c(R1M_Usd)) %>% 
  as.matrix()

nb_stocks <- length(unique(data_unif$stock_id))
nb_feats <- length(ncol(data_train_x))
nb_dates <- 185

```



# Linear model
```{r}
lr_model_1 <- lm(R1M_Usd ~ . ,data = data_train)
summary(lr_model_1)
pred_lr_1 <- predict(lr_model_1, newdata = data_test)
mean((pred_lr_1 - data_test_actual)^2)
mean(pred_lr_1 * data_test_actual > 0)
```

# MSE and MAE of actual returns
```{r}
# compare error of returns instead of log returns
mean((exp(pred_lr_1) - exp(data_test_actual))^2)
mean((exp(pred_lr_1)-1) * (exp(data_test_actual)-1) > 0)
```

# LASSO tuning
```{r}
library(glmnet)
lambdas <- 10^seq(2, -3, -0.1)
las_cv_1 <- cv.glmnet(data_train_x, data_train_y, alpha = 1,
                         lambda = lambdas, nfolds = 5)
lambda_best <- las_cv_1$lambda.min
lambda_best
```

# LASSO prediction
```{r}
las_model_1 <- glmnet(data_train_x, data_train_y, alpha = 1,
                      lambda = lambda_best)
coef(las_model_1, s=lambda_best)
pred_las_1 <- predict(las_model_1, s= lambda_best, newx = data_test_mtx)

mean((pred_las_1 - data_test_actual)^2)
mean(pred_las_1 * data_test_actual > 0)
```

```{r}
# compare error of returns instead of log returns
mean((exp(pred_las_1) - exp(data_test_actual))^2)
mean((exp(pred_las_1)-1) * (exp(data_test_actual)-1) > 0)
```

# Ridge Tuning
```{r}
library(glmnet)
lambdas <- 10^seq(2, -3, -0.1)
rid_cv_1 <- cv.glmnet(data_train_x, data_train_y, alpha = 0,
                         lambda = lambdas, nfolds = 5)
lambda_bestr <- rid_cv_1$lambda.min
lambda_bestr
```

# Ridge Prediction
```{r}
rid_model_1 <- glmnet(data_train_x, data_train_y, alpha = 0,
                      lambda = lambda_bestr)
coef(rid_model_1, s=lambda_bestr)
pred_rid_1 <- predict(rid_model_1, s= lambda_bestr, newx = data_test_mtx)
mean((pred_rid_1 - data_test_actual)^2)
mean(pred_rid_1 * data_test_actual > 0)

# compare error of returns instead of log returns
mean((exp(pred_rid_1) - exp(data_test_actual))^2)
mean((exp(pred_rid_1)-1) * (exp(data_test_actual)-1) > 0)
```


# Random Forest Tuning
```{r}
library(randomForest)
library(caret)

#longggg
control <- trainControl(method="repeatedcv", number = 5, 
                        repeats = 3, search = "grid")
tunegrid <- expand.grid(.mtry=c(2,3,4))
rf_gridsearch <- train(R1M_Usd ~ ., data = data_train, method="rf",
                       tuneGrid = tunegrid, 
                       trControl = control)
print(rf_gridsearch)

```

# Random Forest Fit
```{r}
library(randomForest)
library(caret)
rf_model_1 <- randomForest(R1M_Usd ~ ., data = data_train,
                           maxnodes = 4,
                           mtry = 3,
                           cp = 0.001)
pred_rf_1 <- predict(rf_model_1, data_test)

mean((pred_rf_1 - data_test_actual)^2)
mean(pred_rf_1 * data_test_actual > 0)
# compare error of returns instead of log returns
mean((exp(pred_rf_1) - exp(data_test_actual))^2)
mean((exp(pred_rf_1)-1) * (exp(data_test_actual)-1) > 0)
varImpPlot(rf_model_1)
#(rf_model_1, data_train, Pb)
```


# XGBoost Tuning
```{r}
control <- trainControl(method="repeatedcv", number = 5, 
                        repeats = 1, search = "grid")
tunegrid <- expand.grid(.eta=0.5,
                        .gamma=0.01,
                        .nrounds=50,
                        .max_depth = c(3, 5, 7),
                        .colsample_bytree = c(0.5, 0.75, 1),
                        .min_child_weight = 0,
                        .subsample = 0.5)
xgb_gridsearch <- train(R1M_Usd ~ ., data = data_train,
                       method="xgbTree",
                       tuneGrid = tunegrid, 
                       trControl = control,
                       verbose = FALSE)
print(xgb_gridsearch)

```

# XGBoost Fit
```{r}
library(xgboost)
dtrain_xgb <- xgb.DMatrix(data=data_train_x, label=data_train_y)

xgb_model_1 <- xgb.train(data = dtrain_xgb,
                         eta = 0.5,
                         objective = "reg:squarederror",
                         max_depth = 3,
                         lambda = 1,
                         gamma = 0.1,
                         nrounds = 50)
# monotonicity?

pred_xgb_1 <- predict(xgb_model_1, data_test_mtx)
mean((pred_xgb_1 - data_test_actual)^2)
mean(pred_xgb_1 * data_test_actual > 0)
# compare error of returns instead of log returns
mean((exp(pred_xgb_1) - exp(data_test_actual))^2)
mean((exp(pred_xgb_1)-1) * (exp(data_test_actual)-1) > 0)

```

# Neural Networks Model Building & Fitting
```{r}
library(keras)
nn_model_1 <- keras_model_sequential()

nn_model_1 %>% 
  layer_dense(units = 8, activation = 'tanh', 
              input_shape = ncol(data_train_x)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 4, activation = 'tanh') %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 2, activation = 'tanh') %>%
  layer_dense(units = 1) 

# Compile model and provide summary
nn_model_1%>% 
  compile(              
    loss = 'mean_squared_error',
    optimizer = "adam",                              
    metrics = c('mean_absolute_error')
    )
summary(nn_model_1)
```

```{r}
fit_nn_1 <- nn_model_1 %>%
  fit(data_train_x, data_train_y,
      epochs = 10, 
      batch_size = nb_stocks
      )
pred_nn_1 <- predict(nn_model_1, data_test_mtx)
mean((pred_nn_1 - data_test_actual)^2)
mean(pred_nn_1 * data_test_actual > 0)
# compare error of returns instead of log returns
mean((exp(pred_nn_1) - exp(data_test_actual))^2)
mean((exp(pred_nn_1)-1) * (exp(data_test_actual)-1) > 0)
```

# RNN GRU model building & fitting (discontinued)
```{r}
 data_train <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[1:180]) %>%
    left_join(ff5, by='date') %>% # add Mkt-nn
    left_join(cpi, by="date") %>% # add inflation
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100) 

  data_train_x <- data_train %>%
    select(-c(R1M_Usd)) %>%
    as.matrix()
  data_train_y <- data_train$R1M_Usd

  data_test <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[170:181]) %>%
    left_join(ff5, by='date') %>%
    left_join(cpi, by="date") %>%
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100)
  
  data_test_y <- data_test$R1M_Usd[3994:4356]
  data_test <- data_test %>% select(-c(R1M_Usd))
  data_test_x <- data_test %>% as.matrix()

rnn_train_features <- 
  array(data_train_x, dim=c(nb_dates, nb_stocks, nb_feats))%>%
  aperm(c(2,1,3))
rnn_test_features <- 
  array(data_test_x, dim=c(12, nb_stocks, nb_feats))%>%
  aperm(c(2,1,3))
rnn_train_labels <- 
  array(data_train_y,dim=c(nb_dates, nb_stocks, 1))%>% 
  aperm(c(2,1,3))

rnn_model_1 <- keras_model_sequential() %>%
  layer_gru(units = 16, 
            batch_input_shape = c(nb_stocks, nb_dates, nb_feats),
            activation = "tanh",
            return_sequences = TRUE,
            dropout = 0.25) %>%
  layer_gru(units = 4, 
            batch_input_shape = c(nb_stocks, nb_dates, nb_feats),
            activation = "tanh",
            return_sequences = TRUE) %>%
  layer_dense(units = 1)

rnn_model_1 <- rnn_model_1 %>%
  compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_rmsprop(),                              
    metrics = c('mean_absolute_error')
    )

summary(rnn_model_1)
```

```{r}
fit_rnn_1 <- rnn_model_1 %>%
  fit(rnn_train_features,
      rnn_train_labels,
      epochs = 20,
      batch_size = nb_stocks)

pred_rnn_1 <- predict(rnn_model_1, rnn_test_features)

```

```{r}
mean((pred_rnn_1[3994:4356] - data_test_y)^2)
# compare error of returns instead of log returns
mean((exp(pred_rnn_1[3994:4356]) - exp(data_test_y))^2)
mean((exp(pred_rnn_1[3994:4356])-1) * (exp(data_test_y)-1) > 0)
```



# Autoencoder Model Building & Fitiing
```{r}
nb_dates <- 185

data_short <- data_unif %>%
  select(stock_id, date, Mkt_Cap_3M_Usd, Pb, Mom_5M_Usd,
         Ebitda_Margin, Capex_Sales, R1M_Usd) %>%
  filter(date %in% sorted_date[1:186]) %>%
  left_join(ff5, by="date") %>%
  left_join(cpi, by="date") %>%
  arrange(date, stock_id) %>%
  select(-c(RF, X12M_log_Inflation)) %>%
  mutate(Mkt.RF = Mkt.RF/100) 
  

factor_data_train <- data_short %>%
  filter(date %in% sorted_date[1:185]) %>%
  dplyr::select(date, stock_id, R1M_Usd) %>%
  spread(key = stock_id, value = R1M_Usd) %>%
  dplyr::select(-date) %>%
  as.matrix()

beta_data_train <- array(unlist(
        data_short %>% 
        filter(date <= sorted_date[1:185]) %>%  
        dplyr::select(-stock_id, -date, -R1M_Usd)),
                     dim = c(nb_stocks, nb_dates, nb_feats)) %>%
        aperm(c(2,1,3))

factor_data_test <- data_short %>%
  filter(date %in% sorted_date[186]) %>%
  dplyr::select(date, stock_id, R1M_Usd) %>%
  spread(key = stock_id, value = R1M_Usd) %>%
  dplyr::select(-date) %>%
  as.matrix()

beta_data_test <- array(unlist(
        data_short %>% 
        filter(date %in% sorted_date[185]) %>%  
        dplyr::select(-stock_id, -date, -R1M_Usd)),
                     dim = c(nb_stocks, 1, nb_feats)) %>%
        aperm(c(2,1,3))
```

```{r}
# Create NN specifications
main_input <- layer_input(shape = c(nb_stocks), name = "main_input")   

# Definition of factor side network
factor_network <- main_input %>%                                    
  layer_dense(units = 8, activation = "relu", name = "layer_1_r") %>%
  layer_dense(units = 4, activation = "tanh", name = "layer_2_r")

# Auxiliary input: characteristics
aux_input <- layer_input(shape = c(nb_stocks, nb_feats), name = "aux_input")         
# Definition of beta side network
beta_network <- aux_input %>%                                       
  layer_dense(units = 8, activation = "relu", name = "layer_1_l") %>%
  layer_dense(units = 4, activation = "tanh", name = "layer_2_l") %>%
  layer_permute(dims = c(2,1), name = "layer_3_l") # Permutation!

# Product of 2 networks
main_output <- layer_dot(c(beta_network, factor_network),        
                         axes = 1, name = "main_output")

# Autoencoder model
model_ae <- keras_model(                                        
  inputs = c(main_input, aux_input),
  outputs = c(main_output)
)

# Compile the model
model_ae %>%
    compile(
      optimizer = "rmsprop", 
      loss = "mse",
      metrics = c("mean_absolute_error")
      )

# See autoencoder model details
summary(model_ae)
```

```{r}
fit_ae_1 <- model_ae %>% 
  fit(                                                            
    x = list(main_input = factor_data_train, 
             aux_input = beta_data_train),
    y = list(main_output = factor_data_train),
    epochs = 30,
    batch_size = nb_stocks
    )
```

```{r}
pred_ae_1 <- predict(model_ae, 
                     list(factor_data_test, 
                          beta_data_test))
mean((pred_ae_1 - data_test_actual)^2)
mean(pred_ae_1 * data_test_actual > 0)
```



# Sample Portfolio Construction for Various Predictors
```{r}
stock_ids_long_lr <- as.integer(names(pred_lr_1[which(pred_lr_1 >= quantile(pred_lr_1, probs=seq(0,1,1/5))[5])]))
stock_ids_short_lr <- as.integer(names(pred_lr_1[which(pred_lr_1 <= quantile(pred_lr_1, probs=seq(0,1,1/5))[2])]))
# NOT stock id, but stock index
mean(data_test_actual[stock_ids_long_lr])
mean(data_test_actual[stock_ids_short_lr])
# actual returns
mean(exp(data_test_actual[stock_ids_long_lr])-1)
mean(exp(data_test_actual[stock_ids_short_lr])-1)
```

```{r}
stock_ids_long_las <- which(pred_las_1 >= quantile(pred_las_1, probs=seq(0,1,1/5))[5])
stock_ids_short_las <- which(pred_las_1 <= quantile(pred_las_1, probs=seq(0,1,1/5))[2])
# NOT stock id, but stock index
mean(data_test_actual[stock_ids_long_las]) 
mean(data_test_actual[stock_ids_short_las])
# actual returns 
mean(exp(data_test_actual[stock_ids_long_las])-1)
mean(exp(data_test_actual[stock_ids_short_las])-1)
```

```{r}
stock_ids_long_nn <- as.integer(names(pred_nn_1[which(pred_nn_1 >= quantile(pred_nn_1, probs=seq(0,1,1/5))[5])]))
stock_ids_short_nn <- as.integer(names(pred_nn_1[which(pred_nn_1 <= quantile(pred_nn_1, probs=seq(0,1,1/5))[2])]))
# NOT stock id, but stock index
mean(data_test_actual[stock_ids_long_nn]) 
mean(data_test_actual[stock_ids_short_nn])
# actual returns 
mean(exp(data_test_actual[stock_ids_long_nn])-1)
mean(exp(data_test_actual[stock_ids_short_nn])-1)
```

```{r}
stock_ids_long_nn <- which(pred_nn_1 >= quantile(pred_nn_1, probs=seq(0,1,1/5))[5])
stock_ids_short_nn <- which(pred_nn_1 <= quantile(pred_nn_1, probs=seq(0,1,1/5))[2])
# NOT stock id, but stock index
mean(data_test_actual[stock_ids_long_nn]) 
mean(data_test_actual[stock_ids_short_nn])
# actual returns 
mean(exp(data_test_actual[stock_ids_long_nn])-1)
mean(exp(data_test_actual[stock_ids_short_nn])-1)
```

```{r}
stock_ids_long_rnn <- which(pred_rnn_1 >= quantile(pred_rnn_1, probs=seq(0,1,1/5))[5])
stock_ids_short_rnn <- which(pred_rnn_1 <= quantile(pred_rnn_1, probs=seq(0,1,1/5))[2])
# NOT stock id, but stock index
mean(data_test_actual[stock_ids_long_rnn]) 
mean(data_test_actual[stock_ids_short_rnn])
# actual returns 
mean(exp(data_test_actual[stock_ids_long_rnn])-1)
mean(exp(data_test_actual[stock_ids_short_rnn])-1)
```

```{r}
stock_ids_long_xgb <- which(pred_xgb_1 >= quantile(pred_xgb_1, probs=seq(0,1,1/5))[5])
stock_ids_short_xgb <- which(pred_xgb_1 <= quantile(pred_xgb_1, probs=seq(0,1,1/5))[2])
# NOT stock id, but stock index
mean(data_test_actual[stock_ids_long_xgb]) 
mean(data_test_actual[stock_ids_short_xgb])
# actual returns 
mean(exp(data_test_actual[stock_ids_long_xgb])-1)
mean(exp(data_test_actual[stock_ids_short_xgb])-1)
```

```{r}

which(pred_xgb_1 >= quantile(pred_xgb_1, probs=seq(0,1,1/5))[5])
```






################## ACTUAL PREDICTION ###########################


# Uniformize data

```{r}
# Reapply uniformization
# Transformation function
norm_unif <- function(v){
  v <- v %>% as.matrix()
  return (ecdf(v)(v))
}

# Create uniformized data
data_unif <- data_new %>%
  group_by(date) %>%          # For each date
  # Apply uniformization
  mutate(Mkt_Cap_3M_Usd = norm_unif(Mkt_Cap_3M_Usd)) %>%  
  mutate(Pb = norm_unif(Pb)) %>%  
  mutate(Mom_Sharp_5M_Usd = norm_unif(Mom_Sharp_5M_Usd)) %>%  
  mutate(Ebitda_Margin = norm_unif(Ebitda_Margin)) %>%  
  mutate(Debtequity = norm_unif(Debtequity)) %>%  
  mutate(Op_Margin = norm_unif(Op_Margin)) %>%  
  mutate(Capex_Sales = norm_unif(Capex_Sales)) %>%  
  filter(stock_id %in% stock_ids_short) %>%
  ungroup()
```


# Regression Predictions 
```{r, warning=FALSE}
sorted_date <- sort(unique(data_unif$date))

library(glmnet)

MSE_logr_lr <- rep(0,65)
MAE_logr_lr <- rep(0,65)
HR_logr_lr <- rep(0,65)
MSE_ret_lr <- rep(0,65)
MAE_ret_lr <- rep(0,65)
HR_ret_lr <- rep(0,65)
long_ret_lr <- rep(1,66)
ls_ret_lr <- rep(1,66)
lev_ret_lr <- rep(1,66)
long_avg_lr <- rep(0,65)
short_avg_lr <- rep(0,65)

MSE_logr_las <- rep(0,65)
MAE_logr_las <- rep(0,65)
HR_logr_las <- rep(0,65)
MSE_ret_las <- rep(0,65)
MAE_ret_las <- rep(0,65)
HR_ret_las <- rep(0,65)
long_ret_las <- rep(1,66)
ls_ret_las <- rep(1,66)
lev_ret_las <- rep(1,66)
long_avg_las <- rep(0,65)
short_avg_las <- rep(0,65)

MSE_logr_rid <- rep(0,65)
MAE_logr_rid <- rep(0,65)
HR_logr_rid <- rep(0,65)
MSE_ret_rid <- rep(0,65)
MAE_ret_rid <- rep(0,65)
HR_ret_rid <- rep(0,65)
long_ret_rid <- rep(1,66)
ls_ret_rid <- rep(1,66)
lev_ret_rid <- rep(1,66)
long_avg_rid <- rep(0,65)
short_avg_rid <- rep(0,65)

for (i in 1:65) {
  # form sliding train/test splits
  data_train <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i:i+179]) %>%
    left_join(ff5, by='date') %>% # add Mkt-nn
    left_join(cpi, by="date") %>% # add inflation
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100) 

  data_train_x <- data_train %>%
    select(-c(R1M_Usd)) %>%
    as.matrix()
  data_train_y <- data_train$R1M_Usd

  data_test <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i+180]) %>%
    left_join(ff5, by='date') %>%
    left_join(cpi, by="date") %>%
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100)
  
  data_test_y <- data_test$R1M_Usd
  data_test <- data_test %>% select(-c(R1M_Usd))
  data_test_x <- data_test %>% as.matrix()
  
  # Linear Regression
  lr_model_1 <- lm(R1M_Usd ~ . ,data = data_train)
  pred_lr_1 <- predict(lr_model_1, newdata = data_test)
  
  # Prediction accuracy of log returns
  MSE_logr_lr[i] <- mean((pred_lr_1 - data_test_y)^2)
  MAE_logr_lr[i] <- mean(abs(pred_lr_1 - data_test_y))
  HR_logr_lr[i] <- mean(pred_lr_1 * data_test_y > 0)
    
  # Prediction accuracy of returns
  MSE_ret_lr[i] <- mean((exp(pred_lr_1) - exp(data_test_y))^2)
  MAE_ret_lr[i] <- mean(abs(exp(pred_lr_1) - exp(data_test_y)))
  HR_ret_lr[i] <- mean((exp(pred_lr_1)-1) * (exp(data_test_y)-1) > 0)
  
  # Stocks that are in top/bottom prediction quintile  
  stock_ids_long_lr <- as.integer(names(pred_lr_1[which(pred_lr_1 >= quantile(pred_lr_1, probs=seq(0,1,1/5))[5])]))
  stock_ids_short_lr <- as.integer(names(pred_lr_1[which(pred_lr_1 <= quantile(pred_lr_1, probs=seq(0,1,1/5))[2])]))
  
    
  # Actual return accuracy and portfolio return
  long_avg_lr[i] <- mean(exp(data_test_y[stock_ids_long_lr])-1)
  short_avg_lr[i] <- mean(exp(data_test_y[stock_ids_short_lr])-1)
  long_ret_lr[i+1] <- long_ret_lr[i] * (1 + long_avg_lr[i])
  ls_ret_lr[i+1] <- ls_ret_lr[i] * (1 + long_avg_lr[i] - short_avg_lr[i])
  lev_ret_lr[i+1] <- lev_ret_lr[i] * (1 + 1.5*long_avg_lr[i] - 0.5*short_avg_lr[i])
  
  
  # LASSO
  lambdas <- 10^seq(2, -3, -0.1)
  las_cv_1 <- cv.glmnet(data_train_x, data_train_y, alpha = 1,
                           lambda = lambdas, nfolds = 5)
  lambda_best <- las_cv_1$lambda.min
  
  las_model_1 <- glmnet(data_train_x, data_train_y, alpha = 1,
                      lambda = lambda_best)
  pred_las_1 <- predict(las_model_1, s= lambda_best, 
                        newx = data_test_x)
  
  MSE_logr_las[i] <- mean((pred_las_1 - data_test_y)^2)
  MAE_logr_las[i] <- mean(abs(pred_las_1 - data_test_y))
  HR_logr_las[i] <- mean(pred_las_1 * data_test_y > 0)
    
  MSE_ret_las[i] <- mean((exp(pred_las_1) - exp(data_test_y))^2)
  MAE_ret_las[i] <- mean(abs(exp(pred_las_1) - exp(data_test_y)))
  HR_ret_las[i] <- mean((exp(pred_las_1)-1) * (exp(data_test_y)-1) > 0)
    
  stock_ids_long_las <- which(pred_las_1 >= quantile(pred_las_1, probs=seq(0,1,1/5))[5])
  stock_ids_short_las <- which(pred_las_1 <= quantile(pred_las_1, probs=seq(0,1,1/5))[2])
  
  # actual returns
  long_avg_las[i] <- mean(exp(data_test_y[stock_ids_long_las])-1)
  short_avg_las[i] <- mean(exp(data_test_y[stock_ids_short_las])-1)
  long_ret_las[i+1] <- long_ret_las[i] * (1 + long_avg_las[i])
  ls_ret_las[i+1] <- ls_ret_las[i] * (1 + long_avg_las[i] - short_avg_las[i])
  lev_ret_las[i+1] <- lev_ret_las[i] * (1 + 1.5*long_avg_las[i] - 0.5*short_avg_las[i])
  
  
  # Ridge
  lambdas <- 10^seq(2, -3, -0.1)
  rid_cv_1 <- cv.glmnet(data_train_x, data_train_y, alpha = 0,
                           lambda = lambdas, nfolds = 5)
  lambda_best <- rid_cv_1$lambda.min
  
  rid_model_1 <- glmnet(data_train_x, data_train_y, alpha = 0,
                      lambda = lambda_best)
  pred_rid_1 <- predict(rid_model_1, s= lambda_best, 
                        newx = data_test_x)
  
  MSE_logr_rid[i] <- mean((pred_rid_1 - data_test_y)^2)
  MAE_logr_rid[i] <- mean(abs(pred_rid_1 - data_test_y))
  HR_logr_rid[i] <- mean(pred_rid_1 * data_test_y > 0)
    
  MSE_ret_rid[i] <- mean((exp(pred_rid_1) - exp(data_test_y))^2)
  MAE_ret_rid[i] <- mean(abs(exp(pred_rid_1) - exp(data_test_y)))
  HR_ret_rid[i] <- mean((exp(pred_rid_1)-1) * (exp(data_test_y)-1) > 0)
    
  stock_ids_long_rid <- which(pred_rid_1 >= quantile(pred_rid_1, probs=seq(0,1,1/5))[5])
  stock_ids_short_rid <- which(pred_rid_1 <= quantile(pred_rid_1, probs=seq(0,1,1/5))[2])
  
  # actual returns
  long_avg_rid[i] <- mean(exp(data_test_y[stock_ids_long_rid])-1)
  short_avg_rid[i] <- mean(exp(data_test_y[stock_ids_short_rid])-1)
  long_ret_rid[i+1] <- long_ret_rid[i] * (1 + long_avg_rid[i])
  ls_ret_rid[i+1] <- ls_ret_rid[i] * (1 + long_avg_rid[i] - short_avg_rid[i])
  lev_ret_rid[i+1] <- lev_ret_rid[i] * (1 + 1.5*long_avg_rid[i] - 0.5*short_avg_rid[i])
  
}

```

# Market and RF Characteristics
```{r}
mkt_ret <- rep(1,66)
rf_ret <- rep(1,66)
mr <- rep(0,65)
rf <- rep(0,65)

for (i in 1:65) {
  data_mkt <- ff5 %>%
    filter(date == sorted_date[i+180]) %>%
    mutate(Mkt.RF = Mkt.RF/100) %>%
    mutate(RF = RF/100)
  
  rf[i] <- data_mkt[[3]]
  mr[i] <- data_mkt[[2]] + rf[i]
  
  mkt_ret[i+1] <- mkt_ret[i] * (1 + mr[i])
  rf_ret[i+1] <- rf_ret[i] * (1 + rf[i])
}

rf_ret <- unlist(rf_ret)
mkt_ret <- unlist(mkt_ret)
mkt_sd <- sd(mr) * sqrt(12)
rf_sd <- sd(rf) * sqrt(12)
mkt_annr <- mkt_ret[66]^(1/5.5) - 1
rf_annr <- rf_ret[66]^(1/5.5) - 1

month_cum_ret <- rep(1,66)
for (i in 1:65) {
  month_cum_ret[i+1] = month_cum_ret[i] * 
    exp(month_ret$R1M_Usd[i]) 
}
ew_sd <- sd(exp(month_ret$R1M_Usd)-1) * sqrt(12)
ew_annr <- exp(month_cum_ret[66]-1)^(1/5.5) - 1

cat("Risk-Free Return: ", rf_annr, "\n")
cat("Risk-Free Volatility: ", rf_sd, "\n \n")
cat("Market Return: ", mkt_annr, "\n")
cat("Market Volatility: ", mkt_sd, "\n")
cat("Market Sharpe: ", (mkt_annr - rf_annr)/mkt_sd, "\n \n")
cat("Equal-Weight (1/N) Return: ", ew_annr, "\n")
cat("Equal-Weight (1/N) Volatility: ", ew_sd, "\n")
cat("Equal-weight Sharpe: ", (ew_annr - rf_annr)/ew_sd, "\n \n")
```

# Accuracy & Return Analysis
```{r}
long_annr_lr <- long_ret_lr[66]^(1/5.5) - 1 
ls_annr_lr <- ls_ret_lr[66]^(1/5.5) - 1
lev_annr_lr <- lev_ret_lr[66]^(1/5.5) - 1

long_sd_lr <- sd(long_avg_lr) * sqrt(12)
ls_sd_lr <- sd(long_avg_lr - short_avg_lr) * sqrt(12)
lev_sd_lr <- sd(1.5*long_avg_lr - 0.5*short_avg_lr) * sqrt(12)

long_sharpe_lr <- (long_annr_lr - rf_annr)/long_sd_lr
ls_sharpe_lr <- (ls_annr_lr - rf_annr)/ls_sd_lr
lev_sharpe_lr <- (lev_annr_lr - rf_annr)/lev_sd_lr
  
cat("Linear Regression Summary: \n\n")

cat("MSE: ", mean(MSE_ret_lr), "\n")
cat("MAE: ", mean(MAE_ret_lr), "\n")
cat("HR: ", mean(HR_ret_lr), "\n \n")

cat("Long Return: ", long_annr_lr, "\n")
cat("L/S Return: ", ls_annr_lr, "\n")
cat("Lev L/S Return: ", lev_annr_lr, "\n \n")

cat("Long Vol: ", long_sd_lr, "\n")
cat("L/S Vol: ", ls_sd_lr, "\n")
cat("Lev L/S Vol: ", lev_sd_lr, "\n \n")

cat("Long Sharpe: ", long_sharpe_lr, "\n")
cat("L/S Sharpe: ", ls_sharpe_lr, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_lr, "\n \n")

```

```{r}
long_annr_las <- long_ret_las[66]^(1/5.5) - 1 
ls_annr_las <- ls_ret_las[66]^(1/5.5) - 1
lev_annr_las <- lev_ret_las[66]^(1/5.5) - 1

long_sd_las <- sd(long_avg_las) * sqrt(12)
ls_sd_las <- sd(long_avg_las - short_avg_las) * sqrt(12)
lev_sd_las <- sd(1.5*long_avg_las - 0.5*short_avg_las) * sqrt(12)

long_sharpe_las <- (long_annr_las - rf_annr)/long_sd_las
ls_sharpe_las <- (ls_annr_las - rf_annr)/ls_sd_las
lev_sharpe_las <- (lev_annr_las - rf_annr)/lev_sd_las

cat("LASSO Regression Summary: \n\n")

cat("MSE: ", mean(MSE_ret_las), "\n")
cat("MAE: ", mean(MAE_ret_las), "\n")
cat("HR: ", mean(HR_ret_las), "\n \n")

cat("Long Return: ", long_annr_las, "\n")
cat("L/S Return: ", ls_annr_las, "\n")
cat("Lev L/S Return: ", lev_annr_las, "\n \n")

cat("Long Vol: ", long_sd_las, "\n")
cat("L/S Vol: ", ls_sd_las, "\n")
cat("Lev L/S Vol: ", lev_sd_las, "\n \n")

cat("Long Sharpe: ", long_sharpe_las, "\n")
cat("L/S Sharpe: ", ls_sharpe_las, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_las, "\n \n")
```

```{r}
long_annr_rid <- long_ret_rid[66]^(1/5.5) - 1 
ls_annr_rid <- ls_ret_rid[66]^(1/5.5) - 1
lev_annr_rid <- lev_ret_rid[66]^(1/5.5) - 1

long_sd_rid <- sd(long_avg_rid) * sqrt(12)
ls_sd_rid <- sd(long_avg_rid - short_avg_rid) * sqrt(12)
lev_sd_rid <- sd(1.5*long_avg_rid - 0.5*short_avg_rid) * sqrt(12)

long_sharpe_rid <- (long_annr_rid - rf_annr)/long_sd_rid
ls_sharpe_rid <- (ls_annr_rid - rf_annr)/ls_sd_rid
lev_sharpe_rid <- (lev_annr_rid - rf_annr)/lev_sd_rid

cat("Ridge Regression Summary: \n\n")

cat("MSE: ", mean(MSE_ret_rid), "\n")
cat("MAE: ", mean(MAE_ret_rid), "\n")
cat("HR: ", mean(HR_ret_rid), "\n \n")

cat("Long Return: ", long_annr_rid, "\n")
cat("L/S Return: ", ls_annr_rid, "\n")
cat("Lev L/S Return: ", lev_annr_rid, "\n \n")

cat("Long Vol: ", long_sd_rid, "\n")
cat("L/S Vol: ", ls_sd_rid, "\n")
cat("Lev L/S Vol: ", lev_sd_rid, "\n \n")

cat("Long Sharpe: ", long_sharpe_rid, "\n")
cat("L/S Sharpe: ", ls_sharpe_rid, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_rid, "\n \n")
```

# Plot Cumulative Returns
```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_lr, type="l", lwd=2,
     ylim=c(0.75,1.9), col="green2",
     main="Growth of $1 with Linear Regression",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_lr, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_lr, col="pink2", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], month_cum_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market", "Equal-Weight"),
       col = c("green3", "pink3",  "red", "blue", "lightblue"), 
       pch=20)
```


```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_las, type="l", lwd=2,
     ylim=c(0.7,1.9), col="green2",
     main="Growth of $1 with LASSO Linear Regression",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_las, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_las, col="pink3", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], month_cum_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market", "Equal-Weight"),
       col = c("green3", "pink3",  "red", "blue", "lightblue"), 
       pch=20)
```


```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_rid, type="l", lwd=2,
     ylim=c(0.7,1.9), col="green2",
     main="Growth of $1 with Ridge Regression",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_rid, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_rid, col="pink3", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], month_cum_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market", "Equal-Weight"),
       col = c("green3", "pink3",  "red", "blue", "lightblue"), 
       pch=20)
```



# Repeat the prediction process for Random Forest & XGBoost
```{r, warning=FALSE}
library(randomForest)
library(caret)
library(xgboost)

sorted_date <- sort(unique(data_unif$date))

MSE_logr_rf <- rep(0,65)
MAE_logr_rf <- rep(0,65)
HR_logr_rf <- rep(0,65)
MSE_ret_rf <- rep(0,65)
MAE_ret_rf <- rep(0,65)
HR_ret_rf <- rep(0,65)
long_ret_rf <- rep(1,66)
ls_ret_rf <- rep(1,66)
lev_ret_rf <- rep(1,66)
long_avg_rf <- rep(0,65)
short_avg_rf <- rep(0,65)

MSE_logr_xgb <- rep(0,65)
MAE_logr_xgb <- rep(0,65)
HR_logr_xgb <- rep(0,65)
MSE_ret_xgb <- rep(0,65)
MAE_ret_xgb <- rep(0,65)
HR_ret_xgb <- rep(0,65)
long_ret_xgb <- rep(1,66)
ls_ret_xgb <- rep(1,66)
lev_ret_xgb <- rep(1,66)
long_avg_xgb <- rep(0,65)
short_avg_xgb <- rep(0,65)

for (i in 1:65) {
  data_train <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i:i+179]) %>%
    left_join(ff5, by='date') %>% # add Mkt-nn
    left_join(cpi, by="date") %>% # add inflation
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100) 

  data_train_x <- data_train %>%
    select(-c(R1M_Usd)) %>%
    as.matrix()
  data_train_y <- data_train$R1M_Usd

  data_test <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i+180]) %>%
    left_join(ff5, by='date') %>%
    left_join(cpi, by="date") %>%
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100)
  
  data_test_y <- data_test$R1M_Usd
  data_test <- data_test %>% select(-c(R1M_Usd))
  data_test_x <- data_test %>% as.matrix()

  rf_model_1 <- randomForest(R1M_Usd ~ ., data = data_train,
                             maxdepth = 5,
                             mtry = 3,
                             replace = TRUE,
                             ntree = 2000)
  pred_rf_1 <- predict(rf_model_1, data_test)
  
  # rf
  MSE_logr_rf[i] <- mean((pred_rf_1 - data_test_y)^2)
  MAE_logr_rf[i] <- mean(abs(pred_rf_1 - data_test_y))
  HR_logr_rf[i] <- mean(pred_rf_1 * data_test_y > 0)
    
  MSE_ret_rf[i] <- mean((exp(pred_rf_1) - exp(data_test_y))^2)
  MAE_ret_rf[i] <- mean(abs(exp(pred_rf_1) - exp(data_test_y)))
  HR_ret_rf[i] <- mean((exp(pred_rf_1)-1) * (exp(data_test_y)-1) > 0)
    
  stock_ids_long_rf <- as.integer(names(pred_rf_1[which(pred_rf_1 >= quantile(pred_rf_1, probs=seq(0,1,1/5))[5])]))
  stock_ids_short_rf <- as.integer(names(pred_rf_1[which(pred_rf_1 <= quantile(pred_rf_1, probs=seq(0,1,1/5))[2])]))
  
  # actual returns
  long_avg_rf[i] <- mean(exp(data_test_y[stock_ids_long_rf])-1)
  short_avg_rf[i] <- mean(exp(data_test_y[stock_ids_short_rf])-1)
  long_ret_rf[i+1] <- long_ret_rf[i] * (1 + long_avg_rf[i])
  ls_ret_rf[i+1] <- ls_ret_rf[i] * (1 + long_avg_rf[i] - short_avg_rf[i])
  lev_ret_rf[i+1] <- lev_ret_rf[i] * (1 + 1.5*long_avg_rf[i] - 0.5*short_avg_rf[i])


  dtrain_xgb <- xgb.DMatrix(data=data_train_x, label=data_train_y)

  xgb_model_1 <- xgb.train(data = dtrain_xgb,
                           eta = 0.5,
                           objective = "reg:squarederror",
                           max_depth = 3,
                           lambda = 1,
                           gamma = 0.1,
                           nrounds = 75)
  pred_xgb_1 <- predict(xgb_model_1, data_test_x)
  
  # XGB
  MSE_logr_xgb[i] <- mean((pred_xgb_1 - data_test_y)^2)
  MAE_logr_xgb[i] <- mean(abs(pred_xgb_1 - data_test_y))
  HR_logr_xgb[i] <- mean(pred_xgb_1 * data_test_y > 0)
    
  MSE_ret_xgb[i] <- mean((exp(pred_xgb_1) - exp(data_test_y))^2)
  MAE_ret_xgb[i] <- mean(abs(exp(pred_xgb_1) - exp(data_test_y)))
  HR_ret_xgb[i] <- mean((exp(pred_xgb_1)-1) * (exp(data_test_y)-1) > 0)
    
  stock_ids_long_xgb <- which(pred_xgb_1 >= quantile(pred_xgb_1, probs=seq(0,1,1/5))[5])
  stock_ids_short_xgb <- which(pred_xgb_1 <= quantile(pred_xgb_1, probs=seq(0,1,1/5))[2])
  
  # actual returns
  long_avg_xgb[i] <- mean(exp(data_test_y[stock_ids_long_xgb])-1)
  short_avg_xgb[i] <- mean(exp(data_test_y[stock_ids_short_xgb])-1)
  long_ret_xgb[i+1] <- long_ret_xgb[i] * (1 + long_avg_xgb[i])
  ls_ret_xgb[i+1] <- ls_ret_xgb[i] * (1 + long_avg_xgb[i] - short_avg_xgb[i])
  lev_ret_xgb[i+1] <- lev_ret_xgb[i] * (1 + 1.5*long_avg_xgb[i] - 0.5*short_avg_xgb[i])

}

```


```{r}
long_annr_rf <- long_ret_rf[66]^(1/5.5) - 1 
ls_annr_rf <- ls_ret_rf[66]^(1/5.5) - 1
lev_annr_rf <- lev_ret_rf[66]^(1/5.5) - 1

long_sd_rf <- sd(long_avg_rf) * sqrt(12)
ls_sd_rf <- sd(long_avg_rf - short_avg_rf) * sqrt(12)
lev_sd_rf <- sd(1.5*long_avg_rf - 0.5*short_avg_rf) * sqrt(12)

long_sharpe_rf <- (long_annr_rf - rf_annr)/long_sd_rf
ls_sharpe_rf <- (ls_annr_rf - rf_annr)/ls_sd_rf
lev_sharpe_rf <- (lev_annr_rf - rf_annr)/lev_sd_rf

cat("Random Forest Summary: \n\n")

cat("MSE: ", mean(MSE_ret_rf), "\n")
cat("MAE: ", mean(MAE_ret_rf), "\n")
cat("HR: ", mean(HR_ret_rf), "\n \n")

cat("Long Return: ", long_annr_rf, "\n")
cat("L/S Return: ", ls_annr_rf, "\n")
cat("Lev L/S Return: ", lev_annr_rf, "\n \n")

cat("Long Vol: ", long_sd_rf, "\n")
cat("L/S Vol: ", ls_sd_rf, "\n")
cat("Lev L/S Vol: ", lev_sd_rf, "\n \n")

cat("Long Sharpe: ", long_sharpe_rf, "\n")
cat("L/S Sharpe: ", ls_sharpe_rf, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_rf, "\n \n")
```

```{r}
long_annr_xgb <- long_ret_xgb[66]^(1/5.5) - 1 
ls_annr_xgb <- ls_ret_xgb[66]^(1/5.5) - 1
lev_annr_xgb <- lev_ret_xgb[66]^(1/5.5) - 1

long_sd_xgb <- sd(long_avg_xgb) * sqrt(12)
ls_sd_xgb <- sd(long_avg_xgb - short_avg_xgb) * sqrt(12)
lev_sd_xgb <- sd(1.5*long_avg_xgb - 0.5*short_avg_xgb) * sqrt(12)

long_sharpe_xgb <- (long_annr_xgb - rf_annr)/long_sd_xgb
ls_sharpe_xgb <- (ls_annr_xgb - rf_annr)/ls_sd_xgb
lev_sharpe_xgb <- (lev_annr_xgb - rf_annr)/lev_sd_xgb

cat("XGBoost Summary: \n\n")

cat("MSE: ", mean(MSE_ret_xgb), "\n")
cat("MAE: ", mean(MAE_ret_xgb), "\n")
cat("HR: ", mean(HR_ret_xgb), "\n \n")

cat("Long Return: ", long_annr_xgb, "\n")
cat("L/S Return: ", ls_annr_xgb, "\n")
cat("Lev L/S Return: ", lev_annr_xgb, "\n \n")

cat("Long Vol: ", long_sd_xgb, "\n")
cat("L/S Vol: ", ls_sd_xgb, "\n")
cat("Lev L/S Vol: ", lev_sd_xgb, "\n \n")

cat("Long Sharpe: ", long_sharpe_xgb, "\n")
cat("L/S Sharpe: ", ls_sharpe_xgb, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_xgb, "\n \n")
```



```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_rf, type="l", lwd=2,
     ylim=c(0.8,2.2), col="green3",
     main="Growth of $1 with Random Forest",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_rf, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_rf, col="pink3", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], month_cum_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market", "Equal-Weight"),
       col = c("green3", "pink3",  "red", "blue", "lightblue"), 
       pch=20)
```

```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_xgb, type="l", lwd=2,
     ylim=c(0.8,1.9), col="green3",
     main="Growth of $1 with XGBoost",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_xgb, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_xgb, col="pink3", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], month_cum_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market", "Equal-Weight"),
       col = c("green3", "pink3",  "red", "blue", "lightblue"), 
       pch=20)
```



# and Neural Networks
```{r, warning=FALSE}
library(keras)

sorted_date <- sort(unique(data_unif$date))

MSE_logr_nn <- rep(0,65)
MAE_logr_nn <- rep(0,65)
HR_logr_nn <- rep(0,65)
MSE_ret_nn <- rep(0,65)
MAE_ret_nn <- rep(0,65)
HR_ret_nn <- rep(0,65)
long_ret_nn <- rep(1,66)
ls_ret_nn <- rep(1,66)
lev_ret_nn <- rep(1,66)
long_avg_nn <- rep(0,65)
short_avg_nn <- rep(0,65)

for (i in 1:65) {
  data_train <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i:i+179]) %>%
    left_join(ff5, by='date') %>% # add Mkt-nn
    left_join(cpi, by="date") %>% # add inflation
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100) 

  data_train_x <- data_train %>%
    select(-c(R1M_Usd)) %>%
    as.matrix()
  data_train_y <- data_train$R1M_Usd

  data_test <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i+180]) %>%
    left_join(ff5, by='date') %>%
    left_join(cpi, by="date") %>%
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100)
  
  data_test_y <- data_test$R1M_Usd
  data_test <- data_test %>% select(-c(R1M_Usd))
  data_test_x <- data_test %>% as.matrix()
  
  nn_model_1 <- keras_model_sequential()

  nn_model_1 %>% 
    layer_dense(units = 16, activation = 'tanh', 
                input_shape = ncol(data_train_x)) %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 4, activation = 'tanh') %>%
    layer_dense(units = 1) 

  # Compile model and provide summary
  nn_model_1%>% 
    compile(              
      loss = 'mean_squared_error',
      optimizer = "sgd",                              
      metrics = c('mean_absolute_error')
      )
  fit_nn_1 <- nn_model_1 %>%
  fit(data_train_x, data_train_y,
      epochs = 10, 
      batch_size = nb_stocks,
      verbose = 0
      )
  pred_nn_1 <- predict(nn_model_1, data_test_x)
  
  MSE_logr_nn[i] <- mean((pred_nn_1 - data_test_y)^2)
  MAE_logr_nn[i] <- mean(abs(pred_nn_1 - data_test_y))
  HR_logr_nn[i] <- mean(pred_nn_1 * data_test_y > 0)
    
  MSE_ret_nn[i] <- mean((exp(pred_nn_1) - exp(data_test_y))^2)
  MAE_ret_nn[i] <- mean(abs(exp(pred_nn_1) - exp(data_test_y)))
  HR_ret_nn[i] <- mean((exp(pred_nn_1)-1) * (exp(data_test_y)-1) > 0)
    
  stock_ids_long_nn <- which(pred_nn_1 >= quantile(pred_nn_1, probs=seq(0,1,1/5))[5])
  stock_ids_short_nn <- which(pred_nn_1 <= quantile(pred_nn_1, probs=seq(0,1,1/5))[2])
  
  # actual returns
  long_avg_nn[i] <- mean(exp(data_test_y[stock_ids_long_nn])-1)
  short_avg_nn[i] <- mean(exp(data_test_y[stock_ids_short_nn])-1)
  long_ret_nn[i+1] <- long_ret_nn[i] * (1 + long_avg_nn[i])
  ls_ret_nn[i+1] <- ls_ret_nn[i] * (1 + long_avg_nn[i] - short_avg_nn[i])
  lev_ret_nn[i+1] <- lev_ret_nn[i] * (1 + 1.5*long_avg_nn[i] - 0.5*short_avg_nn[i])
}
```

```{r}
long_annr_nn <- long_ret_nn[66]^(1/5.5) - 1 
ls_annr_nn <- ls_ret_nn[66]^(1/5.5) - 1
lev_annr_nn <- lev_ret_nn[66]^(1/5.5) - 1

long_sd_nn <- sd(long_avg_nn) * sqrt(12)
ls_sd_nn <- sd(long_avg_nn - short_avg_nn) * sqrt(12)
lev_sd_nn <- sd(1.5*long_avg_nn - 0.5*short_avg_nn) * sqrt(12)

long_sharpe_nn <- (long_annr_nn - rf_annr)/long_sd_nn
ls_sharpe_nn <- (ls_annr_nn - rf_annr)/ls_sd_nn
lev_sharpe_nn <- (lev_annr_nn - rf_annr)/lev_sd_nn

cat("Neural Networks Summary: \n\n")

cat("MSE: ", mean(MSE_ret_nn), "\n")
cat("MAE: ", mean(MAE_ret_nn), "\n")
cat("HR: ", mean(HR_ret_nn), "\n \n")

cat("Long Return: ", long_annr_nn, "\n")
cat("L/S Return: ", ls_annr_nn, "\n")
cat("Lev L/S Return: ", lev_annr_nn, "\n \n")

cat("Long Vol: ", long_sd_nn, "\n")
cat("L/S Vol: ", ls_sd_nn, "\n")
cat("Lev L/S Vol: ", lev_sd_nn, "\n \n")

cat("Long Sharpe: ", long_sharpe_nn, "\n")
cat("L/S Sharpe: ", ls_sharpe_nn, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_nn, "\n \n")
```

```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_nn, type="l", lwd=2,
     ylim=c(0.8,1.9), col="green3",
     main="Growth of $1 with Neural Networks",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_nn, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_nn, col="pink3", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], month_cum_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market", "Equal-Weight"),
       col = c("green3", "pink3",  "red", "blue", "lightblue"), 
       pch=20)
```



# Tried GRU (RNN) but doesnt work well and doesnt fit as expected
# This predictor is discontinued
```{r, warning=FALSE, message=FALSE}
library(keras)

MSE_logr_rnn <- rep(0,65)
MAE_logr_rnn <- rep(0,65)
HR_logr_rnn <- rep(0,65)
MSE_ret_rnn <- rep(0,65)
MAE_ret_rnn <- rep(0,65)
HR_ret_rnn <- rep(0,65)
long_ret_rnn <- rep(1,66)
ls_ret_rnn <- rep(1,66)
lev_ret_rnn <- rep(1,66)

nb_stocks <- length(unique(data_unif$stock_id))
nb_feats <- 7
nb_dates <- 180


for (i in 1:12) {
  data_train <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i:i+221]) %>%
    left_join(ff5, by='date') %>% # add Mkt-nn
    left_join(cpi, by="date") %>% # add inflation
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100) 
  
  data_train_x <- data_train %>%
    select(-c(R1M_Usd)) %>%
    as.matrix()
  data_train_y <- data_train$R1M_Usd

  data_test <- data_unif %>%
    select(date, stock_id, R1M_Usd, Mkt_Cap_6M_Usd, Pb,
           Mom_Sharp_5M_Usd, Ebitda_Margin, Capex_Sales) %>%
    filter(date %in% sorted_date[i+222:i+233]) %>%
    left_join(ff5, by='date') %>%
    left_join(cpi, by="date") %>%
    arrange(date, stock_id) %>%
    select(-c(date, stock_id, RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100)
  
  data_test_y <- data_test$R1M_Usd[3994:4356]
  data_test_x <- data_test %>% 
    select(-c(R1M_Usd)) %>%
    as.matrix()
  nb_datetest <- 12
  
  rnn_train_features <- 
    array(data_train_x, dim=c(nb_dates, nb_stocks, nb_feats))%>%
    aperm(c(2,1,3))
  rnn_test_features <- 
    array(data_test_x, dim=c(nb_datetest, nb_stocks, nb_feats))%>%
    aperm(c(2,1,3))
  rnn_train_labels <- 
    array(data_train_y,dim=c(nb_dates, nb_stocks, 1))%>% 
    aperm(c(2,1,3))
  
  rnn_model_1 <- keras_model_sequential() %>%
    layer_gru(units = 16, 
              batch_input_shape = c(nb_stocks, nb_dates, nb_feats),
              activation = "tanh",
              return_sequences = TRUE,
              dropout = 0.25) %>%
    #layer_gru(units = 4, 
    #          batch_input_shape = c(nb_stocks, nb_dates, nb_feats),
    #          activation = "tanh",
    #          return_sequences = TRUE) %>%
    layer_dense(units = 1)
  
  rnn_model_1 <- rnn_model_1 %>%
    compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_rmsprop(),                              
      metrics = c('mean_absolute_error')
      )
  
  fit_rnn_1 <- rnn_model_1 %>%
    fit(rnn_train_features,
        rnn_train_labels,
        epochs = 20,
        batch_size = nb_stocks,
        verbose = 0
      )
  
  new_rnn <- keras_model_sequential() %>%
    layer_gru(units = 16, 
              batch_input_shape = c(nb_stocks, nb_dates, nb_feats),
              activation = "tanh",
              return_sequences = TRUE,
              dropout = 0.25) %>%
    layer_dense(units = 1)
  new_rnn %>% keras::set_weights(keras::get_weights(rnn_model_1))

  pred_rnn_1 <- predict(new_rnn, rnn_test_features)[3994:4356]
  
  MSE_logr_rnn[i] <- mean((pred_rnn_1 - data_test_y)^2)
  MAE_logr_rnn[i] <- mean(abs(pred_rnn_1 - data_test_y))
  HR_logr_rnn[i] <- mean(pred_rnn_1 * data_test_y > 0)
    
  MSE_ret_rnn[i] <- mean((exp(pred_rnn_1) - exp(data_test_y))^2)
  MAE_ret_rnn[i] <- mean(abs(exp(pred_rnn_1) - exp(data_test_y)))
  HR_ret_rnn[i] <- mean((exp(pred_rnn_1)-1) * (exp(data_test_y)-1) > 0)
    
  stock_ids_long_rnn <- which(pred_rnn_1 >= quantile(pred_rnn_1, probs=seq(0,1,1/5))[5])
  stock_ids_short_rnn <- which(pred_rnn_1 <= quantile(pred_rnn_1, probs=seq(0,1,1/5))[2])
  
  # actual returns
  long_avg_rnn <- mean(exp(data_test_y[stock_ids_long_rnn])-1)
  short_avg_rnn <- mean(exp(data_test_y[stock_ids_short_rnn])-1)
  long_ret_rnn[i+1] <- long_ret_rnn[i] * (1 + long_avg_rnn)
  ls_ret_rnn[i+1] <- ls_ret_rnn[i] * (1 + long_avg_rnn - short_avg_rnn)
  lev_ret_rnn[i+1] <- lev_ret_rnn[i] * (1 + 1.5*long_avg_rnn - 0.5*short_avg_rnn)
  
  MSE_logr_rnn[i] <- mean((pred_rnn_1 - data_test_y)^2)
  MAE_logr_rnn[i] <- mean(abs(pred_rnn_1 - data_test_y))
  HR_logr_rnn[i] <- mean(pred_rnn_1 * data_test_y > 0)
    
  MSE_ret_rnn[i] <- mean((exp(pred_rnn_1) - exp(data_test_y))^2)
  MAE_ret_rnn[i] <- mean(abs(exp(pred_rnn_1) - exp(data_test_y)))
  HR_ret_rnn[i] <- mean((exp(pred_rnn_1)-1) * (exp(data_test_y)-1) > 0)
    
  stock_ids_long_rnn <- which(pred_rnn_1 >= quantile(pred_rnn_1, probs=seq(0,1,1/5))[5])
  stock_ids_short_rnn <- which(pred_rnn_1 <= quantile(pred_rnn_1, probs=seq(0,1,1/5))[2])
  
  # actual returns
  long_avg_rnn <- mean(exp(data_test_y[stock_ids_long_rnn])-1)
  short_avg_rnn <- mean(exp(data_test_y[stock_ids_short_rnn])-1)
  long_ret_rnn[i+1] <- long_ret_rnn[i] * (1 + long_avg_rnn)
  ls_ret_rnn[i+1] <- ls_ret_rnn[i] * (1 + long_avg_rnn - short_avg_rnn)
  lev_ret_rnn[i+1] <- lev_ret_rnn[i] * (1 + 1.5*long_avg_rnn - 0.5*short_avg_rnn)
}
```


```{r}
long_annr_rnn <- long_ret_rnn[25]^(1/2) - 1 
ls_annr_rnn <- ls_ret_rnn[25]^(1/2) - 1
lev_annr_rnn <- lev_ret_rnn[25]^(1/2) - 1
long_sharpe_rnn <- mean(long_ret_rnn - rf_ret)/sd(long_ret_rnn)
ls_sharpe_rnn <- mean(ls_ret_rnn - rf_ret)/sd(ls_ret_rnn)
lev_sharpe_rnn <- mean(lev_ret_rnn - rf_ret)/sd(lev_ret_rnn)

cat("MSE: ", mean(MSE_ret_rnn), "\n")
cat("MAE: ", mean(MAE_ret_rnn), "\n")
cat("HR: ", mean(HR_ret_rnn), "\n \n")

cat("Long Return: ", long_annr_rnn, "\n")
cat("L/S Return: ", ls_annr_rnn, "\n")
cat("Lev L/S Return: ", lev_annr_rnn, "\n \n")

cat("Long Vol: ", sd(long_ret_rnn), "\n")
cat("L/S Vol: ", sd(ls_ret_rnn), "\n")
cat("Lev L/S Vol: ", sd(lev_ret_rnn), "\n \n")

cat("Long Sharpe: ", long_sharpe_rnn, "\n")
cat("L/S Sharpe: ", ls_sharpe_rnn, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_rnn, "\n \n")
```

```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_rnn, type="l", lwd=2,
     ylim=c(0.8,2.2), col="green3",
     main="Growth of $1 with Recurrent Neural Networks",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_rnn, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_rnn, col="pink3", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], rf_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market Return", "Risk-Free Return"),
       col = c("green3", "red", "pink3", "blue", "lightblue"), 
       pch=20)
```


# .... and finally, Autoencoders!
```{r}
library(keras)

nb_dates <- 180
sorted_date <- sort(unique(data_unif$date))
nb_stocks <- length(unique(data_unif$stock_id))
nb_feats <- 7


MSE_logr_ae <- rep(0,65)
MAE_logr_ae <- rep(0,65)
HR_logr_ae <- rep(0,65)
MSE_ret_ae <- rep(0,65)
MAE_ret_ae <- rep(0,65)
HR_ret_ae <- rep(0,65)
long_ret_ae <- rep(1,66)
ls_ret_ae <- rep(1,66)
lev_ret_ae <- rep(1,66)
long_avg_ae <- rep(0,65)
short_avg_ae <- rep(0,65)

for (i in 1:65) {
  data_short <- data_unif %>%
    select(stock_id, date, Mkt_Cap_6M_Usd, Pb, Mom_Sharp_5M_Usd,
           Op_Margin, Capex_Sales, R1M_Usd) %>%
    filter(date %in% sorted_date[i:(i+179)]) %>%
    left_join(ff5, by="date") %>%
    left_join(cpi, by="date") %>%
    arrange(date, stock_id) %>%
    select(-c(RF, X12M_log_Inflation)) %>%
    mutate(Mkt.RF = Mkt.RF/100) 
    
  
  factor_data_train <- data_short %>%
    dplyr::select(date, stock_id, R1M_Usd) %>%
    spread(key = stock_id, value = R1M_Usd) %>%
    dplyr::select(-date) %>%
    as.matrix()
  
  beta_data_train <- array(unlist(
          data_short %>% 
          dplyr::select(-stock_id, -date, -R1M_Usd)),
                       dim = c(nb_stocks, nb_dates, nb_feats)) %>%
          aperm(c(2,1,3))
  
  factor_data_test <- data_unif %>%
    filter(date %in% sorted_date[i+180]) %>%
    dplyr::select(date, stock_id, R1M_Usd) %>%
    spread(key = stock_id, value = R1M_Usd) %>%
    dplyr::select(-date) %>%
    as.matrix()
  
  beta_data_test <- array(unlist(
          data_short %>% 
          filter(date %in% sorted_date[i+179]) %>%  
          dplyr::select(-stock_id, -date, -R1M_Usd)),
                       dim = c(nb_stocks, 1, nb_feats)) %>%
          aperm(c(2,1,3))
  
  # Create NN specifications
  main_input <- layer_input(shape = c(nb_stocks), name = "main_input")   
  
  # Definition of factor side network
  factor_network <- main_input %>%                                    
    layer_dense(units = 16, activation = "tanh", name = "layer_1_r") %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 4, activation = "tanh", name = "layer_2_r")
  
  # Auxiliary input: characteristics
  aux_input <- layer_input(shape = c(nb_stocks, nb_feats), name = "aux_input")         
  # Definition of beta side network
  beta_network <- aux_input %>%                                       
    layer_dense(units = 16, activation = "tanh", name = "layer_1_l") %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 4, activation = "tanh", name = "layer_2_l") %>%
    layer_permute(dims = c(2,1), name = "layer_3_l") # Permutation!
  
  # Product of 2 networks
  main_output <- layer_dot(c(beta_network, factor_network),        
                           axes = 1, name = "main_output")
  
  # Autoencoder model
  model_ae <- keras_model(                                        
    inputs = c(main_input, aux_input),
    outputs = c(main_output)
  )
  
  # Compile the model
  model_ae %>%
      compile(
        optimizer = "rmsprop", 
        loss = "mse",
        metrics = c("mean_absolute_error")
        )
  
  fit_ae_1 <- model_ae %>% 
    fit(                  
      x = list(main_input = factor_data_train, 
               aux_input = beta_data_train),
      y = list(main_output = factor_data_train),
      epochs = 25,
      batch_size = nb_stocks,
      verbose = 0
      )
  
  pred_ae_1 <- predict(model_ae, 
                     list(factor_data_test, 
                          beta_data_test))
  
  MSE_logr_ae[i] <- mean((pred_ae_1 - factor_data_test)^2)
  MAE_logr_ae[i] <- mean(abs(pred_ae_1 - factor_data_test))
  HR_logr_ae[i] <- mean(pred_ae_1 * factor_data_test > 0)
    
  MSE_ret_ae[i] <- mean((exp(pred_ae_1) - exp(factor_data_test))^2)
  MAE_ret_ae[i] <- mean(abs(exp(pred_ae_1) - exp(factor_data_test)))
  HR_ret_ae[i] <- mean((exp(pred_ae_1)-1) * (exp(factor_data_test)-1) > 0)
    
  stock_ids_long_ae <- which(pred_ae_1 >= quantile(pred_ae_1, probs=seq(0,1,1/5))[5])
  stock_ids_short_ae <- which(pred_ae_1 <= quantile(pred_ae_1, probs=seq(0,1,1/5))[2])
  
  # actual returns
  long_avg_ae[i] <- mean(exp(factor_data_test[stock_ids_long_ae])-1)
  short_avg_ae[i] <- mean(exp(factor_data_test[stock_ids_short_ae])-1)
  long_ret_ae[i+1] <- long_ret_ae[i] * (1 + long_avg_ae[i])
  ls_ret_ae[i+1] <- ls_ret_ae[i] * (1 + long_avg_ae[i] - short_avg_ae[i])
  lev_ret_ae[i+1] <- lev_ret_ae[i] * (1 + 1.5*long_avg_ae[i] - 0.5*short_avg_ae[i])

}

```


```{r}
long_annr_ae <- long_ret_ae[66]^(1/5.5) - 1 
ls_annr_ae <- ls_ret_ae[66]^(1/5.5) - 1
lev_annr_ae <- lev_ret_ae[66]^(1/5.5) - 1

long_sd_ae <- sd(long_avg_ae) * sqrt(12)
ls_sd_ae <- sd(long_avg_ae - short_avg_ae) * sqrt(12)
lev_sd_ae <- sd(1.5*long_avg_ae - 0.5*short_avg_ae) * sqrt(12)

long_sharpe_ae <- (long_annr_ae - rf_annr)/long_sd_ae
ls_sharpe_ae <- (ls_annr_ae - rf_annr)/ls_sd_ae
lev_sharpe_ae <- (lev_annr_ae - rf_annr)/lev_sd_ae

cat("Autoencoder Summary: \n\n")

cat("MSE: ", mean(MSE_ret_ae), "\n")
cat("MAE: ", mean(MAE_ret_ae), "\n")
cat("HR: ", mean(HR_ret_ae), "\n \n")

cat("Long Return: ", long_annr_ae, "\n")
cat("L/S Return: ", ls_annr_ae, "\n")
cat("Lev L/S Return: ", lev_annr_ae, "\n \n")

cat("Long Vol: ", long_sd_ae, "\n")
cat("L/S Vol: ", ls_sd_ae, "\n")
cat("Lev L/S Vol: ", lev_sd_ae, "\n \n")

cat("Long Sharpe: ", long_sharpe_ae, "\n")
cat("L/S Sharpe: ", ls_sharpe_ae, "\n")
cat("Lev L/S Sharpe: ", lev_sharpe_ae, "\n \n")
```

```{r, fig.width=3, fig.height=2.5}
plot(sorted_date[180:245], long_ret_ae, type="l", lwd=2,
     ylim=c(0.8,2), col="green3",
     main="Growth of $1 with Autoencoder",
     xlab="Year", ylab="Return (base $1)")
lines(sorted_date[180:245], ls_ret_ae, col="red", lwd=2)
lines(sorted_date[180:245], lev_ret_ae, col="pink3", lwd=2)
lines(sorted_date[180:245], mkt_ret, col="blue", lwd=2)
lines(sorted_date[180:245], month_cum_ret, col="lightblue", lwd=2)
par(mar=c(10,0,0,0), xpd=TRUE)
legend("topleft", inset=c(0, 0), cex=0.85,
       legend = c("Long Only", "Long/Short", "Leveraged L/S", 
                  "Market", "Equal-Weight"),
       col = c("green3", "pink3",  "red", "blue", "lightblue"), 
       pch=20)
```


