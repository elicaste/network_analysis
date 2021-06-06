library(data.table)
library(tidyverse)
library(dtplyr)

mammalia <- fread("gbif_november_mammalia.csv")
mammalia <- lazy_dt(mammalia)

mammalia <- mammalia %>% 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>% 
  select(order,
         family,
         genus,
         species,
         scientificName,
         decimalLongitude,
         decimalLatitude,
         coordinateUncertaintyInMeters,
         day,
         month,
         year,
         basisOfRecord)

mammalia <- as.data.frame(mammalia)

fwrite(mammalia, "data/gbif_november_mammalia_clean.csv")

sample_n <- round(nrow(mammalia) * 0.001)
sample_index <- sample(1:nrow(mammalia), sample_n, replace = FALSE)

mammalia_sample <- mammalia[sample_index, ]
fwrite(mammalia_sample, "gbif_november_mammalia_sample.csv")
