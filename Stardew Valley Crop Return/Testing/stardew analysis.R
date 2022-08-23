# the goal of this script is to test the output of the stardew valley data and functions to ensure it makes sense

source("~/GitHub/Additional-Projects/Stardew Valley Crop Return/new stardew variables.R")
source("~/GitHub/Additional-Projects/Stardew Valley Crop Return/new stardew functions.R")

return_crop_printer <- rbindlist(crop_printer(i = select_crops, n = 10000))
return_crop_quality <- crop_quality(return_crop_printer, fertilizer_level = fertilizer_select)
return_additional <- additional_crops(return_crop_quality)
return_crop_revenue <- crop_revenue(return_additional)

return_crop_revenue %>%
  group_by()

return_crop_revenue %>%
  group_by(iter,
           quality) %>%
  summarise(crops = sum(crops)) %>%
  ungroup() %>%
  group_by(iter) %>%
  mutate(pct = round(crops/sum(crops),3)) %>%
  ungroup() %>%
  group_by(quality) %>%
  summarise(min_p = min(pct),
            max_p = max(pct))


crop_pct_range <- return_crop_revenue %>%
  group_by(iter, crop) %>%
  mutate(total_revenue = init_revenue + (additional *add_revenue),
         revenue_additional = additional * add_revenue) %>%
  summarise(init_revenue= sum(init_revenue), 
            revenue_additional = sum(revenue_additional),
            total_revenue = sum(total_revenue, na.rm = TRUE),
            pct = revenue_additional/total_revenue) %>%
  ungroup() %>%
  group_by(iter) %>%
  mutate(total_pct = total_revenue/sum(total_revenue)) %>%
  ungroup() %>%
  arrange(desc(pct)) %>%
  group_by(crop) %>%
  summarise(min_total_pct = min(total_revenue),
            max_total_pct = max(total_revenue),
            average_pct = mean(total_revenue)) %>%
  ungroup() %>%
  arrange(desc(average_pct))

gg_crop_range <- crop_pct_range %>%
  ggplot() + 
  geom_segment(mapping = aes(x = reorder(crop, average_pct),
                             xend = reorder(crop, average_pct),
                             y = min_total_pct,
                             yend = max_total_pct),
               size = 7,
               alpha = 0.2,
               color = 'steelblue') +
  geom_point(mapping = aes(x = reorder(crop, average_pct),
                           y = average_pct),
             size = 7,
             color = 'steelblue') +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(scales::comma(x), "g")) +
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 16, color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.y = element_blank()) 

ts <- gsub(':', '', Sys.time())
ggsave(gg_crop_range, file = glue('gg_crop_range {ts}.png'), width = 12, height = 6)


compare_fert <- list()
for(fert in 0:3){
  
  farm_levels <- read.csv('~/farm levels and fertilizer.csv') %>%
    filter(fertilizer == fert,
           level == level_select) %>%
    select(regular,
           silver,
           gold,
           iridium)
  
  return_crop_quality <- crop_quality(return_crop_printer, fertilizer_level = fert)
  return_additional <- additional_crops(return_crop_quality)
  return_crop_revenue <- crop_revenue(return_additional)
  
  crop_pct_range <- return_crop_revenue %>%
    mutate(total_revenue = init_revenue + (additional *add_revenue)) %>%
    group_by(iter) %>%
    summarise(total_revenue = sum(total_revenue)) %>%
    ungroup() %>%
    mutate(fertilizer = fert)
  
  compare_fert[[fert+1]] <- crop_pct_range
  
}

rbindlist(compare_fert) %>%
  #filter(fertilizer == 3) %>%
  ggplot(aes(total_revenue)) + 
  geom_histogram(mapping = aes(color = factor(fertilizer),
                               fill = factor(fertilizer)),
                 bins = 50,
                 alpha = 0.5) +
  facet_wrap(~fertilizer,
             scales = 'free')

compare_level <- list()
for(level_v in 0:14){
  
  farm_levels <- read.csv('~/farm levels and fertilizer.csv') %>%
    filter(fertilizer == 3,
           level == level_v) %>%
    select(regular,
           silver,
           gold,
           iridium)
  
  return_crop_quality <- crop_quality(return_crop_printer, fertilizer_level = fert)
  return_additional <- additional_crops(return_crop_quality)
  return_crop_revenue <- crop_revenue(return_additional)
  
  crop_pct_range <- return_crop_revenue %>%
    group_by(iter, 
             crop) %>%
    mutate(total_revenue = init_revenue + (additional *add_revenue),
           revenue_additional = additional * add_revenue) %>%
    summarise(init_revenue= sum(init_revenue), 
              revenue_additional = sum(revenue_additional),
              total_revenue = sum(total_revenue, na.rm = TRUE),
              pct = revenue_additional/total_revenue) %>%
    ungroup() %>%
    group_by(iter) %>%
    mutate(total_pct = total_revenue/sum(total_revenue)) %>%
    ungroup() %>%
    arrange(desc(pct)) %>%
    group_by(crop) %>%
    summarise(min_rev = min(total_revenue),
              max_rev = max(total_revenue),
              avg_rev = mean(total_revenue),
              median_rev = median(total_revenue)) %>%
    ungroup() %>%
    arrange(desc(avg_rev)) %>%
    mutate(level = level_v)
  
  compare_level[[level_v+1]] <- crop_pct_range
  
}

rbindlist(compare_fert) %>%
  select(fertilizer,
         crop,
         median_rev) %>%
  arrange(crop) %>%
  group_by(crop) %>%
  mutate(base = ifelse(fertilizer == 0, median_rev, NA)) %>%
  fill(base) %>%
  mutate(med_gain = (median_rev-base)/base) %>%
  filter(fertilizer > 0) %>%
  group_by(fertilizer) %>%
  summarise(min_gain = min(med_gain),
            max_gain = max(med_gain),
            median_gain = median(med_gain)) %>%
  ungroup() %>%
  mutate(label = case_when(fertilizer == 1 ~ 'Basic',
                           fertilizer == 2 ~ 'Quality',
                           fertilizer == 3 ~ 'Deluxe'),
         label = factor(label, levels = c('Basic', 'Quality', 'Deluxe'))) %>%
  ggplot() + 
  geom_segment(mapping = aes(x = label,
                             xend = label,
                             y = min_gain,
                             yend = max_gain),
               size = 5,
               alpha = 0.4,
               color = 'darkorange') +
  geom_point(mapping = aes(x = label,
                           y = median_gain),
             size = 5,
             color = 'darkorange') +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0('+', scales::percent(x))) +
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 16, color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.y = element_blank()) 


rbindlist(compare_level) %>%
  select(level,
         crop,
         median_rev) %>%
  arrange(crop) %>%
  group_by(crop) %>%
  mutate(base = ifelse(level == 0, median_rev, NA)) %>%
  fill(base) %>%
  mutate(med_gain = (median_rev-base)/base) %>%
  filter(level > 0) %>%
  group_by(level) %>%
  summarise(min_gain = min(med_gain),
            max_gain = max(med_gain),
            median_gain = median(med_gain)) %>%
  ungroup() %>%
  ggplot() + 
  geom_segment(mapping = aes(x = level,
                             xend = level,
                             y = min_gain,
                             yend = max_gain),
               size = 5,
               alpha = 0.4,
               color = 'darkorange') +
  geom_point(mapping = aes(x = level,
                           y = median_gain),
             size = 5,
             color = 'darkorange') +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0('+', scales::percent(x))) +
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(size = 14),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 16, color = '#969696'),
        axis.text.x.bottom = element_text(size = 12, color = 'black'),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.y = element_blank()) 

fert_list <- list()
for(fert_v in 0:3){
  
  level_list <- list()
  for(level_v in 0:14){
    
    farm_levels <- read.csv('~/farm levels and fertilizer.csv') %>%
      filter(fertilizer == fert_v,
             level == level_v) %>%
      select(regular,
             silver,
             gold,
             iridium)
    
    return_crop_quality <- crop_quality(return_crop_printer, fertilizer_level = fert)
    return_additional <- additional_crops(return_crop_quality)
    return_crop_revenue <- crop_revenue(return_additional)
    
    crop_pct_range <- return_crop_revenue %>%
      group_by(iter, 
               crop) %>%
      mutate(total_revenue = init_revenue + (additional *add_revenue),
             revenue_additional = additional * add_revenue) %>%
      summarise(init_revenue= sum(init_revenue), 
                revenue_additional = sum(revenue_additional),
                total_revenue = sum(total_revenue, na.rm = TRUE),
                pct = revenue_additional/total_revenue) %>%
      ungroup() %>%
      group_by(iter) %>%
      mutate(total_pct = total_revenue/sum(total_revenue)) %>%
      ungroup() %>%
      arrange(desc(pct)) %>%
      group_by(crop) %>%
      summarise(min_rev = min(total_revenue),
                max_rev = max(total_revenue),
                avg_rev = mean(total_revenue),
                median_rev = median(total_revenue)) %>%
      ungroup() %>%
      arrange(desc(avg_rev)) %>%
      mutate(level = level_v)
    
    level_list[[level_v+1]] <- crop_pct_range

  }
  
  fert_df <- rbindlist(level_list) %>% mutate(fertilizer = fert_v)
  
  fert_list[[fert_v+1]] <- fert_df
}

rbindlist(fert_list) %>%
  #filter(crop == 'Hops') %>%
  ggplot() + 
  geom_tile(mapping = aes(x = level,
                          y = fertilizer,
                          fill = median_rev),
            color = 'white') +
  facet_wrap(~crop)


