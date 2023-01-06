library(tidyverse)

df_students <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")
head(df_students)

df_world_tile <- readr::read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv") %>% 
  mutate(
    ## Namibias two-digit country code is handled as `NA` - let us fix that
    alpha.2 = if_else(name == "Namibia", "NA", alpha.2),
    ## We are going to split "Americas" into "North America" and "Sout America"
    region = if_else(region == "Americas", sub.region, region),
    region = if_else(region %in% c("Northern America", "Central America", "Caribbean"), 
                     "North America", region),
    region = if_else(region == "Southern America", "South America", region),
    ## to join both data sets, we need a id column
    country_code = alpha.3
  )
head(df_world_tile)


df_ratios <- df_students %>% 
  ## Let's keep only the most recent data per country
  group_by(country, indicator) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  # Create `NA`s for countries which do not have any data 2012-2018
  complete(indicator, nesting(country, country_code)) %>% 
  ## Let's focus on primary education and keep only countries (coded by letters)
  filter(
    indicator == "Primary Education",
    str_detect(country_code, "[A-Z]")
  ) %>% 
  ## merge with world tile map data
  full_join(df_world_tile) %>%
  filter(
    !is.na(region),
    !is.na(indicator)
  ) %>% 
  group_by(region) %>% 
  mutate(student_ratio_region = median(student_ratio, na.rm = T)) %>% 
  ungroup()

head(df_ratios)

#New data.frame with only data needed for analysis
df_ratio2 <- df_ratios %>% 
  select(c("indicator", "country", "region","student_ratio", "student_ratio_region"))

table(df_ratio2)
head(df_ratio2)

#Removed the rows which contain NA
ratios_3 <- na.omit(df_ratio2)

df_group <- df_ratio2 %>% 
  group_by(region)
df_group
