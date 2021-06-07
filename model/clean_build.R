library(tidyverse)
library(ggplot2)

clean_data <- readRDS(file = "./data/subregion_agg.rds")

clean_data %>%
  filter(date >= "2020-03-01") %>%
  select(date, "subregion1_name", "new_confirmed") %>%
  arrange(date)
  
view(clean_data)  

ggplot(
  data = clean_data,
  aes(y = new_confirmed, x = date, color = subregion1_name)
) + geom_line() + labs(color="Province")