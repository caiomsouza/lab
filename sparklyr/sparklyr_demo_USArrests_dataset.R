
library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local")

# Load data
data("USArrests")
my_data <- USArrests
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
my_data <- scale(my_data)
# View the firt 3 rows
head(my_data, n = 3)

my_data

my_data_tbl <- copy_to(sc, my_data)


# Train and Test
sets <- sparklyr::sdf_partition(my_data_tbl, training=0.7, test = 0.3, seed = 1234)
train <- sets$training
test <- sets$test

train
test


# K-Means

kmeans_model <- train %>%
#  select(!is.na(Murder), !is.na(Assault), !is.na(UrbanPop), !is.na(Rape)) %>%
  select(Murder, Assault, UrbanPop, Rape) %>%
  ml_kmeans(centers = 5)

# print our model fit
print(kmeans_model)


# predict the associated class
predicted <- sdf_predict(kmeans_model, test) %>%
  collect
table(predicted$Murder, predicted$prediction)


# plot cluster membership
sdf_predict(kmeans_model) %>%
  collect() %>%
  ggplot(aes(Murder, Assault, UrbanPop, Rape)) +
  geom_point(aes(Murder, Assault, UrbanPop, Rape, col = factor(prediction + 1)),
             size = 2, alpha = 0.5) + 
  geom_point(data = kmeans_model$centers, aes(Murder, Assault, UrbanPop, Rape),
             col = scales::muted(c("red", "green", "blue")),
             pch = 'x', size = 12) +
  scale_color_discrete(name = "Predicted Cluster",
                       labels = paste("Cluster", 1:5)) +
  labs(
    x = "Petal Length",
    y = "Petal Width",
    title = "K-Means Clustering",
    subtitle = "Use Spark.ML to predict cluster membership with the iris dataset."
  )


# 
