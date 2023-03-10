library(tidyverse)
library(janitor)
decathalon <- read_rds(here::here("raw_data/decathlon.rds"))
names(decathalon)
glimpse(decathalon)

decathalon_first_clean <- clean_names(decathalon) # cleans file names
names(decathalon_first_clean)
glimpse(decathalon_first_clean)

# `100m` -> x100m
# Long.jump -> long_jump

# my_data %>% 
#  rename(
#   sepal_length = Sepal.Length,
#    sepal_width = Sepal.Width
#  )

#decathalon_first_clean %>% 
#  rename(
#    100m = 100m,
#    400m = x400m,
#    110m_hurdle = x110m_hurdle,
#    1500m = x1500m
#  )

# All caps for Decastar competition
# All small letters for OlympicG

d <- decathalon_first_clean
names <- rownames(d)
rownames(d) <- NULL
decathalon_row_remove <- cbind(names,d)  # change row names to columns










