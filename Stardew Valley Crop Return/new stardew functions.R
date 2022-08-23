# stardew valley crop simulations
# functions script

crop_printer <- function(i = select_crops, n = 10000){
  # this function takes a vector of selected crops and prints a row for each crop grown 'n' simulations
  # inputs:
  #   i is the vector of crops
  #   n is the number of simulations
  # example: if you were growing 8 tomatoes, then the output would be 8 rows of tomatoes 10,000 times for each simulation
  
  crop_list <- list()
  for(v in i){
    
    crop_vars_grown <- dplyr::filter(filter_crops_total_grown, crop == v)
    crop_vars_harvests <- dplyr::filter(filter_crops_growth, crop == v)
    
    init <- data.table(crop = do.call("rbind", replicate(crop_vars_grown$total_grown, v, simplify = FALSE)), keep.rownames = TRUE) %>%
      rename(crop = 1)
    
    list <- replicate(crop_vars_harvests$total_harvests, init, simplify = FALSE)
    
    list_index <- do.call("rbind", Map(cbind, list, grow_cycle = seq_along(list)))
    
    rep_n <- replicate(n, list_index, simplify = FALSE)
    rep_index <- do.call("rbind", Map(cbind, rep_n, iter = seq_along(rep_n)))
    crop_list[[v]] <- rep_index
    
  }
  
  return(crop_list)
}
crop_quality <- function(df, fertilizer_level = 0){
  # this function simulates the probability of obtaining a quality of the crop based on the fertilizer selected
  # inputs:
  #   df is the data frame of crops
  #   fertilizer_level determines the probability of obtaining a quality of crop (0-3)
  
  df_quality <- data.table()
  
  if(fertilizer_level < 3){
    
    df_quality <- return_crop_printer
    
    df_quality[, rand_int:=runif(.N, 0, 1), list(iter, grow_cycle)]
    
    df_fert<-cbind(df_quality, farm_levels)
    
    df_fert[, is_gold := ifelse(rand_int <= gold, 1, 0)]
    df_fert[, is_silver := ifelse(rand_int <= silver & is_gold == 0, 1, 0)]
    df_fert[, is_regular := ifelse(is_gold == 0 & is_silver == 0, 1, 0)]
    df_fert[,rand_int:=NULL]
    df_fert[,iridium:=NULL]
    df_fert[,gold:=NULL]
    df_fert[,silver:=NULL]
    df_fert[,regular:=NULL]
    
    df_melt <- melt(df_fert, id=c("crop","grow_cycle", "iter"))
    
    setnames(df_melt, "variable", "quality")
    setnames(df_melt, "value", "crops")
    
    df_melt[,quality := gsub('\\is_', '', quality)]
    
    df_filter <- df_melt[crops!=0]
    df_filter[,crops :=ifelse(crop == 'Cranberries', 2, 1)]
    
  }else{
    
    df_quality <- return_crop_printer
    
    df_quality[, rand_int:=runif(.N, 0, 1), list(iter, grow_cycle)]
    
    df_fert<-cbind(df_quality, farm_levels)
    
    df_fert[, is_iridium := ifelse(rand_int <= iridium, 1, 0)]
    df_fert[, is_gold := ifelse(rand_int <= gold & is_iridium == 0, 1, 0)]
    df_fert[, is_silver := ifelse(is_gold == 0 & is_iridium == 0, 1, 0)]
    df_fert[,rand_int:=NULL]
    df_fert[,iridium:=NULL]
    df_fert[,gold:=NULL]
    df_fert[,silver:=NULL]
    df_fert[,regular:=NULL]
    
    df_melt <- melt(df_fert, id=c("crop","grow_cycle", "iter"))
    
    setnames(df_melt, "variable", "quality")
    setnames(df_melt, "value", "crops")
    
    df_melt[,quality := gsub('\\is_', '', quality)]
    
    df_filter <- df_melt[crops!=0]
    df_filter[,crops :=ifelse(crop == 'Cranberries', 2, 1)]
    
  }
  
  print(df_filter)
  return(df_filter)
  
}
additional_crops <- function(df){
  # this function simulates the probability of a crop producing an additional crop, if relevant
  # input:
  #   df is the data frame of crops
  
  crop_additional_prob <- merge(df, filter_crops_additional, all=FALSE)
    
    crop_additional_prob[, rand_int:=runif(.N, 0, 1), list(iter, grow_cycle)]
    
    crop_additional_prob[, additional := ifelse(rand_int <= additional_crop_prob, additional_crop_tot, 0) ]
    crop_additional_prob[, additional := ifelse(is.na(additional), 0, additional) ]
    
    crop_additional_prob[,additional_crop_prob:=NULL]
    crop_additional_prob[,additional_crop_tot:=NULL]
    crop_additional_prob[,additional_crop:=NULL]
    crop_additional_prob[,rand_int:=NULL]
    
  
  print(crop_additional_prob)
  return(crop_additional_prob)
  
}
crows <- function(df){
  
  crop_rows <- nrow(df)
  
  df[, rand_int := runif(crop_rows, 0, 1)]
  
  df[, crow_eats := ifelse(rand_int <= crow_loss, 1, 0) ]
  
  df[,rand_int:=NULL]
  
  print(df)
  return(df)
  
}
crop_revenue <- function(df){
  # this function measures the revenue returned based on the quality of crop from crop_quality()
  # additional crops are always regular quality
  # inputs:
  #   df is the data frame of crops      
  
  regular_sell <- crop_sell[quality == 'regular']
  regular_sell[,quality := NULL]
  
  return_crows_join <- return_additional[crop_sell, on = .(crop = crop, quality = quality, season = season), nomatch = NULL]
  setnames(return_crows_join, "revenue", "init_revenue")
  additional_revenue <- return_crows_join[regular_sell, on = .(crop = crop, season=season), nomatch = NULL]
  additional_revenue[,revenue := ifelse(additional != 0, revenue, 0)]
  setnames(additional_revenue, "revenue", "add_revenue")
  additional_revenue[,init_revenue := init_revenue*crops]
  
  #return_crows_join[, real_revenue := ifelse(crow_eats == 1, 0, revenue)]
  
  print(additional_revenue)
  return(additional_revenue)
  
}
