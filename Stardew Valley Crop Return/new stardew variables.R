# stardew valley crop simulations
# contains libraries and variables used throughout the scripts

# libraries ----
library(data.table)
library(gghighlight)
library(tidyverse)
library(ggpattern)
library(ggbeeswarm)
library(ggplot2)
library(glue)

# initial variables ----
days_season <- 28
select_scare_crow <- 0
season_select <- 'Spring'
fertilizer_select <- 3
level_select <- 14

# dummy data  ----
rand_crops <- c('Blue Jazz', 'Cauliflower', 'Garlic', 'Green Bean', 'Kale', 'Parsnip', 'Potato', 'Tulip', 'Blueberry', 'Corn', 'Hops', 'Hot Pepper', 'Melon', 'Poppy', 'Radish', 'Red Cabbage', 'Summer Spangle', 'Sunflower', 'Tomato', 'Amaranth', 'Artichoke', 'Bok Choy', 'Cranberries', 'Eggplant', 'Fairy Rose', 'Grape', 'Pumpkin', 'Yam', 'Wheat')

farm_levels <- read.csv('~/farm levels and fertilizer.csv') %>%
  filter(fertilizer == fertilizer_select,
         level == level_select) %>%
  select(regular,
         silver,
         gold,
         iridium)

crop_growth <- read.csv('~/crop days growth.csv') %>% 
  filter(!is.na(season), season == season_select)

crop_total_grown <- read.csv('~/crops grown.csv') %>%
  filter(season != '', 
         season == season_select) %>%
  mutate(total_grown = round(runif(nrow(.), 10, 10)))

crop_additional <- read.csv('~/additional crops.csv') %>% 
  filter(season != '', 
         season == season_select)

crop_sell <- read.csv('~/crop selling price.csv') %>% 
  filter(season != '', 
         season == season_select) %>%
  pivot_longer(cols = c('regular', 'silver', 'gold', 'iridium'),
               names_to = 'quality',
               values_to = 'revenue') %>%
  as.data.table()

crop_produces <- read.csv('~/additional crops.csv') %>%
  select(crop) %>%
  mutate(produces = ifelse(crop == 'Cranberries', 2, 1))

crop_cost <- data.table(crop = rand_crops) %>%
  mutate(pierre = round(runif(nrow(.), 5, 10)),
         joja_mart = round(runif(nrow(.), 11, 21 )))

# final variables ----
total_crops <- sum(crop_total_grown$total_grown)
crow_loss <- ifelse(total_crops < 15 | select_scare_crow == 0, 0.08, 0)

select_crops <- sample(crop_growth$crop,8)

filter_crops_growth <- dplyr::filter(crop_growth, crop %in% select_crops)
filter_crop_produces <- dplyr::filter(crop_produces, crop %in% select_crops)
filter_crops_total_grown <- dplyr::filter(crop_total_grown, crop %in% select_crops)
filter_crops_additional <- dplyr::filter(crop_additional, crop %in% select_crops)
filter_crops_cost <- dplyr::filter(crop_cost, crop %in% select_crops)
filter_crops_sell <- dplyr::filter(crop_sell, crop %in% select_crops)
