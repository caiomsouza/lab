
# Install 
install.packages("sparklyr")

# Load sparklyr library
library(sparklyr)

# Install Spark
spark_install(version = "1.6.2")

Sys.getenv("SPARK_HOME")

sc <- spark_connect(master = "local")

sc <- spark_connect(master = "local", spark_home = "/BigData/Spark/spark-1.6.2-bin-hadoop2.6")


Sys.setenv(SPARK_HOME="/BigData/Spark/spark-1.6.2-bin-hadoop2.6")
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))

library(sparklyr)
library(DBI)

sc <- spark_connect(master = "local", config = list())




install.packages(c("nycflights13", "Lahman"))


#install.packages(c("digest", "htmltools", "httpuv", "xtable"))
#devtools::install_github("rstudio/sparklyr")
library(sparklyr)
packageVersion("sparklyr")
options("sparkapi.ports.file" = "ports.out")
sc <- spark_connect(master = "local")

sessionInfo()

# https://github.com/rstudio/sparklyr/issues/86

#https://github.com/caiomsouza/data-science/blob/master/SparkR/src/sparkr-example-00.R


spark_install_dir()

spark_installed_versions()

spark_available_versions()

spark_install(logging = "INFO", verbose = interactive())

#http://spark.rstudio.com/reference/sparklyr/latest/spark_install.html

library(dplyr)
iris_tbl <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")



library(DBI)
iris_preview <- dbGetQuery(sc, "SELECT * FROM iris LIMIT 10")
iris_preview


# copy mtcars into spark
mtcars_tbl <- copy_to(sc, mtcars)

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

# fit a linear model to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))

fit

summary(fit)


spark_web(sc)

spark_log(sc, n = 10)

spark_disconnect(sc)


# http://spark.rstudio.com/h2o.html
install.packages("rsparkling")
options(rsparkling.sparklingwater.version = "1.6.2")

# Load libraries
library(rsparkling)
library(h2o)
library(dplyr)

# Open the Spark connection
sc <- spark_connect("local", version = "1.6.2")

# Load mtcars to Spark memory
mtcars_tbl <- copy_to(sc, mtcars, "mtcars")

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)


training <- as_h2o_frame(sc, partitions$training, strict_version_check = FALSE)
test <- as_h2o_frame(sc, partitions$test, strict_version_check = FALSE)


# fit a linear model to the training dataset
glm_model <- h2o.glm(x = c("wt", "cyl"), 
                     y = "mpg", 
                     training_frame = training,
                     lambda_search = TRUE)

print(glm_model)


library(ggplot2)

# compute predicted values on our test dataset
pred <- h2o.predict(glm_model, newdata = test)
# convert from H2O Frame to Spark DataFrame
predicted <- as_spark_dataframe(sc, pred, strict_version_check = FALSE)

# extract the true 'mpg' values from our test dataset
actual <- partitions$test %>%
  select(mpg) %>%
  collect() %>%
  `[[`("mpg")

# produce a data.frame housing our predicted + actual 'mpg' values
data <- data.frame(
  predicted = predicted,
  actual    = actual
)
# a bug in data.frame does not set colnames properly; reset here 
names(data) <- c("predicted", "actual")

# plot predicted vs. actual values
ggplot(data, aes(x = actual, y = predicted)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Actual Fuel Consumption",
    y = "Predicted Fuel Consumption",
    title = "Predicted vs. Actual Fuel Consumption"
  )




