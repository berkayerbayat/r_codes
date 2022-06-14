library(keras)
library(tensorflow)
library(ggplot2)
library(stats)
library(readr)
library(dplyr)
library(forecast)
library(Metrics)

# Data Processing
RSXFSN <- read_csv("C:/Users/SAMSUNG/Desktop/zaman serisi/RSXFSN (1).csv")
RSXFSN$DATE <- as.Date(RSXFSN$DATE, "%Y-%M-%D")
ggplot(RSXFSN, aes(x=DATE, y = RSXFSN)) + geom_line()
dateID <- RSXFSN$DATE[2:nrow(RSXFSN)]
diff <- diff(RSXFSN$RSXFSN, differences = 1)


#Data normalization
scale_data <- function(train, test, feature_range = c(0,1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = (x - min(x)) / (max(x) - min(x))
  std_test = (test - min(x)) / (max(x) - min(x))
  
  scaled_train = std_train * (fr_max - fr_min) + fr_min
  scaled_test = std_test * (fr_max - fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}

#Reverse Scale
reverse_scaling <- function(scaled, scaler, feature_range = c(0,1)) {
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for(i in 1:t) {
    X = (scaled[i] - mins) / (maxs - mins)
    rawValues = X * (max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}


#######################################
mae_df <- data.frame(row.names = c("LAG","MAE"))
mae_df <- as.data.frame(t(mae_df))

a <- 1
while (a < 37) {
  print(a)
  supervised <- as.data.frame(cbind(lag(diff,a), diff))
  supervised[is.na(supervised)] <- 0
  
  #Data Split
  N = nrow(supervised)
  n_= round(N *0.7, digits = 0)
  
  train <- supervised[1:n_, ]
  test <- supervised[(n_+1):N,]
  train_id <- dateID[1:n_]
  test_id <- dateID[(n_+1):N]
  
  
  #scaled data, retrieved test&train
  Scaled = scale_data(train, test, c(-1, 1))
  y_train = Scaled$scaled_train[, 2]
  x_train = Scaled$scaled_train[, 1]
  y_test = Scaled$scaled_test[, 2]
  x_test = Scaled$scaled_test[, 1]
  
  #Modeling
  dim(x_train) <- c(length(x_train), 1,1)
  X_shape2 <- dim(x_train)[2]
  X_shape3 <- dim(x_train)[3]
  batch_size <- 1
  units <- 1
  
  model <- keras_model_sequential()
  model %>% 
    layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful = TRUE) %>%
    layer_dense(units = 1)
  
  #Compile
  model %>% 
    compile(loss = 'mean_squared_error',
            optimizer = optimizer_adam(learning_rate = 0.03, decay = 1e-6),
            metrics = c('accuracy')
    )
  
  Epochs = 50   
  e <- 1
  for(i in 1:Epochs ){
    model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
    model %>% reset_states()
    print(e)
    e <- e + 1
  }
  
  #Predictions
  L = length(x_test)
  scaler = Scaled$scaler
  predictions = numeric(L)
  Series=RSXFSN$RSXFSN
  
  for(i in 1:L){
    X = x_test[i]
    dim(X) = c(1,1,1)
    set.seed(1)
    yhat = model %>% predict(X, batch_size=batch_size)
    # invert scaling
    yhat = reverse_scaling(yhat, scaler,  c(-1, 1))
    # invert differencing
    yhat  = yhat + Series[(n_+i)]
    # store
    predictions[i] <- yhat
  }
  
  summary(model)
  mae_df[a,1] <- a
  mae_df[a,2] <- mae(RSXFSN$RSXFSN[86:121], predictions)
  a <- a + 1
}

diff <- diff(RSXFSN$RSXFSN, differences = 1)
supervised <- as.data.frame(cbind(lag(diff,1), diff))
supervised[is.na(supervised)] <- 0

#Data Split
N = nrow(supervised)
n_= round(N *0.7, digits = 0)

train <- supervised[1:n_, ]
test <- supervised[(n_+1):N,]
train_id <- dateID[1:n_]
test_id <- dateID[(n_+1):N]


#scaled data, retrieved test&train
Scaled = scale_data(train, test, c(-1, 1))
y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]
y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

#Modeling
dim(x_train) <- c(length(x_train), 1,1)
X_shape2 <- dim(x_train)[2]
X_shape3 <- dim(x_train)[3]
batch_size <- 1
units <- 1

set.seed(1)
model <- keras_model_sequential()
model %>% 
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful = TRUE) %>%
  layer_dense(units = 1)

#Compile
set.seed(1)
model %>% 
  compile(loss = 'mean_squared_error',
          optimizer = optimizer_adam(learning_rate = 0.03, decay = 1e-6),
          metrics = c('accuracy')
  )

set.seed(1)
Epochs = 50
e <- 1
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
  print(e)
  e <- e + 1
}

#Predictions
L = length(x_test)
scaler = Scaled$scaler
predictions = numeric(L)
Series=RSXFSN$RSXFSN

j <- 1
#forecast <- as.data.frame(RSXFSN$RSXFSN)
while (j < 25) {
  print(j)
  
  forecast <- as.data.frame(forecast)
  forecast <- rbind(forecast,0)
  forecast <- as_tibble(forecast)
  tail(forecast)
  
  diff <- diff(forecast$`RSXFSN$RSXFSN`, differences = 1)
  supervised <- as.data.frame(cbind(lag(diff,12), diff))
  supervised[is.na(supervised)] <- 0
  tail(supervised)
  #Data Split
  N = nrow(supervised)
  n_= N-1
  
  train <- supervised[1:n_, ]
  test <- supervised[(n_+1):N,]
  
  #scaled data, retrieved test&train
  Scaled = scale_data(train, test, c(-1, 1))
  y_train = Scaled$scaled_train[, 2]
  x_train = Scaled$scaled_train[, 1]
  y_test = Scaled$scaled_test[, 2]
  x_test = Scaled$scaled_test[, 1]
  
  #Modeling
  dim(x_train) <- c(length(x_train), 1,1)
  X_shape2 <- dim(x_train)[2]
  X_shape3 <- dim(x_train)[3]
  batch_size <- 1
  units <- 1
  
  set.seed(1)
  model <- keras_model_sequential()
  model %>% 
    layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful = TRUE) %>%
    layer_dense(units = 1)
  
  #Compile
  set.seed(1)
  model %>% 
    compile(loss = 'mean_squared_error',
            optimizer = optimizer_adam(learning_rate = 0.03, decay = 1e-6),
            metrics = c('accuracy')
    )
  
  set.seed(1)
  Epochs = 500
  e <- 1
  for(i in 1:Epochs ){
    model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
    model %>% reset_states()
    print(e)
    e <- e + 1
  }
  
  #Predictions
  L = length(x_test)
  scaler = Scaled$scaler
  predictions = numeric(L)
  Series=forecast$`RSXFSN$RSXFSN`
  
  for(i in 1:L){
    X = x_test[i]
    dim(X) = c(1,1,1)
    set.seed(1)
    yhat = model %>% predict(X, batch_size=batch_size)
    # invert scaling
    yhat = reverse_scaling(yhat, scaler,  c(-1, 1))
    # invert differencing
    yhat  = yhat + Series[(n_+i)]
    # store
    predictions[i] <- yhat
  }
  predictions
  forecast[121+j,] <- predictions
  j <- j + 1
}
forecast
plot.ts(as.data.frame(forecast))
abline(v = 121, col = "red", lty = 5)
abline(v = 133, col = "blue", lty = 5)
############################################################

RSXFSN_arima <- RSXFSN %>% select(RSXFSN) %>% ts()
arimaFit <- auto.arima(RSXFSN_arima)
arimaFitted <- arimaFit$fitted
arimaFitted <- as.data.frame(cbind(arimaFitted, RSXFSN$DATE))
arimaFitted <- arimaFitted[-1,]
arimaFitted <- cbind(arimaFitted, RSXFSN[-1,])
arimaFitted$set <- "train"
arimaFitted$set[arimaFitted$"RSXFSN$DATE" > 15765] <- "test"
arimaFitted$arimaFitted[arimaFitted$set == "train"] <- NA
arimaPlot <- plot_ly(arimaFitted, x = ~DATE) %>%
  add_trace(y = ~RSXFSN, mode = "lines", name = "Interest Rate") %>%
  add_trace(y = ~arimaFitted, mode = "lines", name = "Interest Rate") %>%
  layout(title="ARIMA")
arimaMAE <- arimaFitted %>% filter(set == "test") %>% select(RSXFSN, arimaFitted)
arimaMAE <- mae(arimaMAE$RSXFSN, arimaMAE$arimaFitted)
arimaMAE
arimaFit
