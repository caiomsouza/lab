
# http://spark.rstudio.com/

# Error: tar: Failed to set default locale
system("defaults write org.R-project.R force.LANG en_US.UTF-8")

install.packages("sparklyr")

devtools::install_github("rstudio/sparklyr")

options(rsparkling.sparklingwater.version = "1.6.8")
library(sparklyr)

spark_install(logging = "INFO", verbose = interactive())

sc <- spark_connect(master = "local")

install.packages(c("nycflights13", "Lahman"))

library(dplyr)
iris_tbl <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
src_tbls(sc)

# filter by departure delay and print the first few records
flights_tbl %>% filter(dep_delay == 2)

delay <- flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect

# plot delays
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

batting_tbl %>%
  select(playerID, yearID, teamID, G, AB:H) %>%
  arrange(playerID, yearID, teamID) %>%
  group_by(playerID) %>%
  filter(min_rank(desc(H)) <= 2 & H > 0)

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








# H20 - http://spark.rstudio.com/h2o.html
install.packages("rsparkling")

spark_installed_versions()

options(rsparkling.sparklingwater.version = "2.0.3")

# Because Spark version '2.0.1' will be used, set Sparkling Water to verion '2.0.3'  - See 'Installation' for more info
options(rsparkling.sparklingwater.version = "2.0.3")

options(rsparkling.sparklingwater.version = "1.6.8")

# Because Spark version '2.0.1' will be used, set Sparkling Water to verion '2.0.3'  - See 'Installation' for more info
options(rsparkling.sparklingwater.version = "2.0.3")


# Load libraries
library(rsparkling)
library(h2o)
library(dplyr)

# Open the Spark connection
sc <- spark_connect("local", version = "2.0.1")

spark_disconnect_all()

sc <- spark_connect("local", version = "1.6.2")

# Load mtcars to Spark memory
mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

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


iris_tbl <- copy_to(sc, iris, "iris", overwrite = TRUE)
iris_tbl

iris_hf <- as_h2o_frame(sc, iris_tbl, strict_version_check = FALSE)


