library(ggplot2)
library(tidyverse)
library(knitr)
library(tm)
library(plotly)
library(ggExtra)
library(patchwork)
library(tensorflow)
library(keras)

#Carcgar los datos
df <- read.csv('summaries_train.csv')

#Función para limpieza de texto
clean <- function(txt){
  txt_new <- tolower(txt)
  txt_new <- gsub("[0-9]", "", txt_new)
  txt_new <- gsub("[[:punct:]]", "", txt_new)
  txt_new <- iconv(txt_new, "UTF-8", "ASCII//TRANSLIT")
  txt_new <- gsub("\\s+", " ", txt_new)
  return(txt_new)
  
}

df$text <- clean(df$text)


#Dividir el conjunto de datos en train y test
set.seed(pi)
muestra_train <- sample(1:dim(df)[1],
                        round(dim(df)[1]*.80),
                        replace = FALSE)

df_train <- df[muestra_train,]
df_test <- df[-muestra_train,]

y_train <- df$content[muestra_train]
y_test <- df$content[-muestra_train]

#----------------------Tfidf-------------------------
sklearn_f <- import("sklearn.feature_extraction.text")
vectorizador <- sklearn_f$TfidfVectorizer
vectorizador <- vectorizador()
#Entrenar con el conjunto de prueba
vectorizador <- vectorizador$fit(df_train$text)


#----Extraer las el valor Tfidf para train y test---------
X_train <- as.matrix(vectorizador$transform(df_train$text))
colnames(X_train) <- vectorizador$get_feature_names_out()

X_test <- as.matrix(vectorizador$transform(df_test$text))
colnames(X_test) <- vectorizador$get_feature_names_out()



#------------Modelo-------------------
#-------------------------------------
shape = dim(X_train)[2]

#Definir la estructura de la red
model <- keras_model_sequential(input_shape = shape) %>%
  layer_flatten() %>%
  layer_dense(5, activation = "relu") %>%
  #layer_dropout(0.2) %>%
  layer_dense(5, activation = "relu") %>%
  layer_dense(1)

#Compilar el modelo
model %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = "mse"
)

#Entrenar el modelo
model %>% fit(X_train,y_train, epochs = 20,
              validation_data = list(X_test, y_test)
)


#--------------Evaluación-------------------

#------Con el conjunto de train--------------
sklearn_metrics <- import("sklearn.metrics")
mean_squared_error <- sklearn_metrics$mean_squared_error
mean_absolute_error <- sklearn_metrics$mean_absolute_error
r2_score <- sklearn_metrics$r2_score


y_pred <- model %>% predict(X_train)

MSE <- mean_squared_error(y_train,y_pred,squared=TRUE) #MSE
RMSE <- mean_squared_error(y_train,y_pred,squared=FALSE) #RMSE
MAE <- mean_absolute_error(y_train,y_pred) #MAE
R2 <- r2_score(y_train,y_pred)

df_eval <- data.frame(y_train,y_pred)

ggplot(df_eval) +
  geom_density(aes(x = y_train, 
                   fill = "Datos de entrenamiento"), 
               alpha = 0.5, color = "green") +
  geom_density(aes(x = y_pred, fill = "Predicciones"),
               alpha = 0.5, color = "red") +
  
  # Personalizar colores en la leyenda y etiquetas
  scale_fill_manual(
    values = c("Datos de entrenamiento" = "green", 
               "Predicciones" = "red"),
    labels = c("Datos de entrenamiento", "Predicciones")
  ) +
  
  guides(fill = guide_legend(title = "")) +
  labs(
    title = "Densidad de Datos Reales vs. Predicciones",
    x = "Content",
    y = "Densidad",
    fill = "Variable"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = df_eval) + 
  geom_point(aes(x = y_pred,y = y_train),
             color = "#1B96B5") + 
  labs(title = "Datos de entrenamiento vs predicciónes",
       x = "Predicciones", y = "Content")  + 
  theme(plot.title = element_text(hjust = 0.5))


#------Con el conjunto de test--------------
y_pred <- model %>% predict(X_test)

MSE <- mean_squared_error(y_test,y_pred,squared=TRUE) #MSE
RMSE <- mean_squared_error(y_test,y_pred,squared=FALSE) #RMSE
MAE <- mean_absolute_error(y_test,y_pred) #MAE
R2 <- r2_score(y_test,y_pred)

df_eval <- data.frame(y_test,y_pred)

ggplot(df_eval) +
  geom_density(aes(x = y_test, 
                   fill = "Datos de prueba"), 
               alpha = 0.5, color = "green") +
  geom_density(aes(x = y_pred, fill = "Predicciones"),
               alpha = 0.5, color = "red") +
  
  # Personalizar colores en la leyenda y etiquetas
  scale_fill_manual(
    values = c("Datos de prueba" = "green", 
               "Predicciones" = "red"),
    labels = c("Datos de prueba", "Predicciones")
  ) +
  
  guides(fill = guide_legend(title = "")) +
  labs(
    title = "Densidad de Datos Reales vs. Predicciones",
    x = "Content",
    y = "Densidad",
    fill = "Variable"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = df_eval) + 
  geom_point(aes(x = y_pred,y = y_test),
             color = "#1B96B5") + 
  labs(title = "Datos de prueba vs predicciónes",
       x = "Predicciones", y = "Content")  + 
  theme(plot.title = element_text(hjust = 0.5))


