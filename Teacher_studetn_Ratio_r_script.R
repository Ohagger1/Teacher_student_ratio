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

#Initial box plot of data
ggplot(ratios_3, aes(x = region, y = student_ratio)) +
  geom_boxplot()

#Arrange the box plots in ascending to descending order
df_sorted <-
  ratios_3 %>%
  mutate(region = fct_reorder(region, student_ratio_region))

#Wrong order so rearrange to correct order
df_sorted2 <-
  ratios_3 %>%
  mutate(region = fct_reorder(region, -student_ratio_region))

ggplot(df_sorted2,mapping = aes(x=region,y=student_ratio))+ geom_boxplot()

#Violin plot
ggplot(df_sorted2,mapping = aes(x=region,y=student_ratio))+ geom_violin()

#Switch x and y axis
ggplot(df_sorted2,mapping = aes(x=student_ratio,y=region))+ geom_violin()

#Changed the x axis length and expand axis
ggplot(df_sorted2,mapping = aes(x=student_ratio,y=region))+ geom_violin() + scale_x_continuous(limits = c(0,90),expand = c(0.02, 0.02))

ggplot(df_sorted2,mapping = aes(x=student_ratio,y=region,color = region))+ geom_violin() + scale_x_continuous(limits = c(0,90),expand = c(0.02, 0.02)) 


#Set theme of the graph
theme_set(theme_light(base_size = 18, base_family = "Poppins"))

#Sort out the variables of the graphs
g <-
  ggplot(df_sorted2, aes(x = student_ratio, y = region, color = region)) +
  scale_x_continuous(limits = c(0, 90), expand = c(0.02, 0.02)) +

  labs(y = NULL, x = "Student to teacher ratio") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )

#Point of the graph
g+geom_point(size = 3,alpha = 0.15)

#
outlier.alpha = 0
set.seed(2019)
g + geom_jitter(size = 2, alpha = 0.25,height = 0.25)+stat_summary(fun = mean, geom = "point", size = 5)

world_avg <-
  df_sorted2 %>%
  summarize(avg = mean(student_ratio, na.rm = TRUE)) %>%
  pull(avg)

#
g + geom_jitter(size = 2, alpha = 0.35,height = 0.2)+
  stat_summary(fun = mean, geom = "point", size = 5)+
  geom_vline(aes(xintercept=world_avg, colour="grey"))



