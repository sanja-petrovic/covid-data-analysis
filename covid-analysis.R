library(sparklyr)
library(dplyr)
library(ggplot2)

sc <- spark_connect(master = "local")
spark_get_java()

covid.basic <- spark_read_csv(sc, name="covid", path=".")