library(tidyverse)
library(sparklyr)

sc <- spark_connect(master = "local")

df1 <- as_tibble(iris)

df <- copy_to(sc, "df1")

class(df1)
class(df)

head(df1)
head(df)

df %>% select(Sepal_Length,Species) %>% head %>% print

df %>% filter(Sepal_Length>5.5) %>% head %>% print

df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head %>%
 print

df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length),
 count = n()) %>% head %>% print

df2 %>% arrange(Species) %>% head %>% print
