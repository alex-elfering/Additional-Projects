# this script contains dummy data that was previously used to test stardew valley functions

crop_growth <- data.table(crop = rand_crops) %>% 
  mutate(days_to_growth = round(runif(nrow(.), 3,7)),
         regrowth = round(runif(nrow(.), 0, 1)),
         regrowth_days = ifelse(regrowth == 1, round(runif(nrow(.), 2,4)), 0),
         total_harvests = case_when(regrowth == 0 ~ 1,
                                    regrowth == 1 ~ floor((days_season-(days_to_growth+1))/(regrowth_days+1) ) + floor(days_season/(days_to_growth+1)) ))

crop_total_grown <- data.table(crop = rand_crops) %>%
  mutate(total_grown = round(runif(nrow(.), 20, 20)))

crop_additional <- data.table(crop = rand_crops) %>%
  mutate(additional_crop = round(runif(nrow(.), 0, 1)),
         additional_crop_prob = ifelse(additional_crop == 1, 
                                       round(runif(nrow(.), 0,0.25), 2), 
                                       NA),
         additional_crop_tot = ifelse(additional_crop == 1, 
                                      round(runif(nrow(.), 1,1.6)), 
                                      NA))

crop_sell <- data.table(crop = rand_crops) %>%
             mutate(regular = round(runif(nrow(.), 30, 40)),
                    silver = round(runif(nrow(.), 60, 70)),
                    gold = round(runif(nrow(.), 100, 120)),
                    iridium = round(runif(nrow(.), 160, 200))) %>%
  pivot_longer(cols = c('regular', 'silver', 'gold', 'iridium'),
               names_to = 'quality',
               values_to = 'revenue') %>%
  as.data.table()

crop_produces <- data.table(crop = rand_crops) %>%
  mutate(produces = round(runif(nrow(.), 0.6, 1.6)))

