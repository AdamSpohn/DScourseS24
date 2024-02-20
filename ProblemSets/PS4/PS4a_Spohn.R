library(jsonlite)
library(dplyr)

system('wget "https://www.vizgr.org/historical-events/search.php?format=json&begin_da
te=00000101&end_date=20240209&lang=en" -O dates.json', intern = TRUE)

system("cat dates.json")

mylist <- fromJSON("dates.json")

mydf <- bind_rows(mylist$result[-1])

class(mydf$date)

head(mydf, n = 10)
