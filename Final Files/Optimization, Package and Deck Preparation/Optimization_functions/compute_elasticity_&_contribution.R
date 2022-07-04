##Function to get the media target value for a given spend rolled up to quarterly level
get_target <- function(spend)
{
    ###Get the media GA for the given spend matrix
    res <- gross_add_weekly(solution = spend,
                 min_media_mat = min_media_mat,
                 max_media_mat = max_media_mat,
                 ratio_dma = spend_activity_ratio_dma_df,
                 y_max = target_max_vec,
                 prospect_mat_media = prospect_mat_media,
                 beta_media_df = beta_media_mat,
                 K = K,
                 S=S)
    
    media_spend_names_only <- media_names_vec[!(media_names_vec %in% activity_based_media)]

    
    ## Roll up the media GA to quarterly level
    quarterly_res <- data.frame(nbr_period_q*res) %>%
                     mutate_at(.vars = vars(media_names_vec),.funs = ~round(.)) %>%
                     mutate(spend_media_total = rowSums(.[media_spend_names_only]),
                            Total = rowSums(.),
                            dma = dma_names_vec) %>%
                     select(dma,everything())
    
  
    ## DMA level GA - includes Optimum also
    dma_res <- quarterly_res %>%
                    gather(key = media,value = 'Media_GA', -dma) %>%
                    mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.))) %>%
                    mutate(Media_GA = as.numeric(Media_GA))

                
    ## Suddenlink level GA
    sdl_res <- quarterly_res %>%
                   filter(!(dma%in%optimum_dma)) %>%
                   summarize_at(.vars = c(media_names_vec,'spend_media_total','Total'),.funs = sum) %>%
                   gather(key = media,value = 'Media_GA') %>%
                   mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.))) %>%
                   mutate(Media_GA = as.numeric(Media_GA))
    
    ## Altice level GA
    altice_res <- quarterly_res %>%
                      summarize_at(.vars = c(media_names_vec,'spend_media_total','Total'),.funs = sum) %>%
                      gather(key = media,value = 'Media_GA') %>%
                      mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.))) %>%
                      mutate(Media_GA = as.numeric(Media_GA))
    
    
    return(list('DMA' = dma_res,
                'Suddenlink' = sdl_res,
                'Altice' = altice_res))    
}


##Compute Elasticity & Media Contribution at Footprint level & DMA level
compute_elasticity_and_contribution <- function(model_params,
                              mean_qy_spend,
                              spend_change = 0.1,
                              display_order = media_names_vec,
                              activity_based_media = NULL){
    
    
    
    ##Extract the params needed to compute the elasticity
    beta_media_mat <<- model_params$media_params$beta_media_mat
    K <<- model_params$media_params$K
    S <<- model_params$media_params$S
    spend_activity_ratio_dma_df <<- model_params$media_params$spend_activity_ratio_dma_df
    min_media_mat <<- model_params$media_params$min_media_mat
    max_media_mat <<- model_params$media_params$max_media_mat
    media_names_vec <<- model_params$media_params$media_names_vec
    cleaned_names_list <<- model_params$media_params$cleaned_names_list
    prospect_mat_media <<- model_params$media_params$prospect_mat_media
    media_spend_cols <<- model_params$media_params$spend_cols_vec

    ##--------------------------------Control parameters ----------------------------------
    beta_control_mat <<- model_params$control_params$beta_control_mat
    control_mat <<- model_params$control_params$control_mat
    control_names_vec <<- model_params$control_params$control_names_vec

    ##-------------------------------Target parameters ------------------------------------
    target_max_vec <<- model_params$target_params$target_max_vec
    target_var <<- model_params$target_params$target_var
    target_capita_norm_col <<- model_params$target_params$target_capita_norm_col

    ##--------------------------------Other parameters -------------------------------------
    nbr_period_q <<- model_params$other_params$nbr_period_q
    #prospect_vec <<- model_params$other_params$prospect_vec
    dma_names_vec <<- model_params$other_params$dma_names_vec
    optimum_dma <<- model_params$other_params$optimum_dma
    
    activity_based_media <<- activity_based_media
    
    media_spend_cols_only <<- media_spend_cols[!(media_spend_cols %in% activity_based_media)]
    
    
    ##Compute the spend matrix from input quarterly mean spend
    mean_qy_spend_mat <- mean_qy_spend %>%
                            arrange(dma,dma_names_vec) %>%
                            rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                            select(media_names_vec) %>%
                            as.matrix(.)
    
    ##Get the long format of spend by DMA
     mean_spend_long_dma <- mean_qy_spend %>%
                                   mutate(spend_media_total = rowSums(.[media_spend_cols_only]),
                                          Total = rowSums(.[media_spend_cols])) %>%
                                   mutate_at(.vars = c(media_spend_cols,'spend_media_total','Total'),.funs=~nbr_period_q*(.)) %>%
                                   rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                                   gather(key = media,value = mean_spend, -dma) %>%
                                   mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                                   arrange(dma, match(media,display_order))
    
    ##Get the long format of spend for SDL
     mean_spend_long_sdl <- mean_qy_spend %>%
                                   mutate(spend_media_total = rowSums(.[media_spend_cols_only]),
                                                 Total = rowSums(.[media_spend_cols])) %>%
                                   filter(!(dma%in%optimum_dma)) %>%
                                   summarize_at(.vars = c(media_spend_cols,'spend_media_total','Total'),.funs=~nbr_period_q*sum(.)) %>%
                                   rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                                   gather(key = media,value = mean_spend) %>%
                                   mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                                   arrange(match(media,display_order))
    
    ##Get the long format of spend for Altice
     mean_spend_long_altice <- mean_qy_spend %>%
                                   mutate(spend_media_total = rowSums(.[media_spend_cols_only]),
                                          Total = rowSums(.[media_spend_cols])) %>%
                                   summarize_at(.vars = c(media_spend_cols,'spend_media_total','Total'),.funs=~nbr_period_q*sum(.)) %>%
                                   rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                                   gather(key = media,value = mean_spend) %>%
                                   mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                                   arrange(match(media,display_order))
    
    
    ##Compute the media GA from the model results
    mean_quarterly_trg <- get_target(mean_qy_spend_mat)
    
    dma_trg <- mean_quarterly_trg$DMA
    sdl_trg <- mean_quarterly_trg$Suddenlink
    altice_trg <- mean_quarterly_trg$Altice
    
    
    ##Join the spend and target 
    dma_spend_trg <- mean_spend_long_dma %>%
                         dplyr::left_join(dma_trg, by = c('dma','media')) %>%
                         dplyr::ungroup()
    
    sdl_spend_trg <- mean_spend_long_sdl %>%
                         dplyr::left_join(sdl_trg, by = 'media') %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(dma = 'SUDDENLINK') %>%
                         dplyr::select(dma, everything())
    
    altice_spend_trg <- mean_spend_long_altice %>%
                         dplyr::left_join(altice_trg, by = 'media') %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(dma = 'ALTICE') %>%
                         dplyr::select(dma, everything())
    
    
    for(change in spend_change){
        list_name <- paste('change',change,sep = '_')
        
        ##Compute the incremental and decremental spend matrices
        increment <- 1 + change
        decrement <- 1 - change

        mean_p_spend_mat <- increment*mean_qy_spend_mat 
        mean_b_spend_mat <- decrement*mean_qy_spend_mat 

        ##Get the media target for the incremental and decremental spends
        mean_p_target = get_target(mean_p_spend_mat)
        mean_b_target = get_target(mean_b_spend_mat)
        
        
        ##Get the spend for DMA       
        change_spend_dma    <-  mean_spend_long_dma%>% 
                                    mutate('mean_spend_up' =  round(increment*mean_spend,0), #increment*mean_spend,
                                           'mean_spend_down' = round(decrement*mean_spend,0))  #decrement*mean_spend)
        mean_spend_dma_up   <- change_spend_dma %>%
                                    dplyr::select(dma, media,mean_spend_up)
        mean_spend_dma_down <- change_spend_dma %>%
                                    dplyr::select(dma, media,mean_spend_down)
        
        ##Get the spend for Suddenlink
        change_spend_sdl    <-  mean_spend_long_sdl%>%
                                    mutate('mean_spend_up' = round(increment*mean_spend,0),
                                           'mean_spend_down' = round(decrement*mean_spend,0)) 
        mean_spend_sdl_up   <- change_spend_sdl %>%
                                    dplyr::select(media,mean_spend_up)
        mean_spend_sdl_down <- change_spend_sdl %>%
                                    dplyr::select(media,mean_spend_down)
        
        ##Get the spend for Altice
        change_spend_altice    <-  mean_spend_long_altice%>%
                                        mutate('mean_spend_up' = round(increment*mean_spend,0),
                                               'mean_spend_down' = round(decrement*mean_spend,0)) 
        mean_spend_altice_up   <- change_spend_altice %>%
                                       dplyr::select(media,mean_spend_up)
        mean_spend_altice_down <- change_spend_altice %>%
                                       dplyr::select(media,mean_spend_down)
        
    
        ##Increased & Decreased spend and GA at DMA level    
        incr_dma_spend_ga <- mean_spend_dma_up %>%
                             dplyr::left_join(mean_p_target$DMA, by = c('dma','media')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(dma, media, mean_spend_up,Media_GA) %>%
                             dplyr::mutate(mean_spend_up = as.numeric(mean_spend_up),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_up',change,sep = '_') := mean_spend_up,
                                           !!paste('Media_GA_up',change,sep = '_') := Media_GA) 
        
        decr_dma_spend_ga <- mean_spend_dma_down %>%
                             dplyr::left_join(mean_b_target$DMA, by = c('dma','media')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(dma, media, mean_spend_down,Media_GA) %>%
                             dplyr::mutate(mean_spend_down = as.numeric(mean_spend_down),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_down',change,sep = '_') := mean_spend_down,
                                           !!paste('Media_GA_down',change,sep = '_') := Media_GA)
        
        ##Increased & Decreased spend and GA at Suddenlink level 
        incr_sdl_spend_ga <- mean_spend_sdl_up %>%
                             dplyr::left_join(mean_p_target$Suddenlink, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'SUDDENLINK') %>%
                             dplyr::select(dma, media, mean_spend_up,Media_GA) %>%
                             dplyr::mutate(mean_spend_up = as.numeric(mean_spend_up),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_up',change,sep = '_') := mean_spend_up,
                                           !!paste('Media_GA_up',change,sep = '_') := Media_GA)
        
        decr_sdl_spend_ga <- mean_spend_sdl_down %>%
                             dplyr::left_join(mean_b_target$Suddenlink, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'SUDDENLINK') %>%
                             dplyr::select(dma, media, mean_spend_down,Media_GA) %>%
                             dplyr::mutate(mean_spend_down = as.numeric(mean_spend_down),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_down',change,sep = '_') := mean_spend_down,
                                           !!paste('Media_GA_down',change,sep = '_') := Media_GA)
        
        ##Increased & Decreased spend and GA at Altice level 
        incr_altice_spend_ga <- mean_spend_altice_up %>%
                             dplyr::left_join(mean_p_target$Altice, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'ALTICE') %>%
                             dplyr::select(dma, media, mean_spend_up,Media_GA) %>%
                             dplyr::mutate(mean_spend_up = as.numeric(mean_spend_up),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_up',change,sep = '_') := mean_spend_up,
                                           !!paste('Media_GA_up',change,sep = '_') := Media_GA)
        
        decr_altice_spend_ga <- mean_spend_altice_down %>%
                             dplyr::left_join(mean_b_target$Altice, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'ALTICE') %>%
                             dplyr::select(dma, media, mean_spend_down,Media_GA) %>%
                             dplyr::mutate(mean_spend_down = as.numeric(mean_spend_down),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_down',change,sep = '_') := mean_spend_down,
                                           !!paste('Media_GA_down',change,sep = '_') := Media_GA) 
        
        
        ##Add Incr and Decr dataframes to Actual dataframe - DMA level
        dma_spend_trg <- dma_spend_trg %>%
                             dplyr::left_join(incr_dma_spend_ga, by = c('dma','media')) %>%
                             dplyr::left_join(decr_dma_spend_ga, by = c('dma','media')) %>%
                             dplyr::ungroup()
        
        ##Add Incr and Decr dataframes to Actual dataframe - Suddenlink level
        sdl_spend_trg <- sdl_spend_trg %>%
                             dplyr::left_join(incr_sdl_spend_ga, by = c('dma','media')) %>%
                             dplyr::left_join(decr_sdl_spend_ga, by = c('dma','media')) %>%
                             dplyr::ungroup()
        
        ##Add Incr and Decr dataframes to Actual dataframe - Altice level
        altice_spend_trg <- altice_spend_trg %>%
                             dplyr::left_join(incr_altice_spend_ga, by = c('dma','media')) %>%
                             dplyr::left_join(decr_altice_spend_ga, by = c('dma','media')) %>%
                             dplyr::ungroup()
         
    }
   
    ## Combine DMA level, Suddenlink level, Altice level dataframes into 1 single dataframe
    final_spend_trg <-  altice_spend_trg %>% rbind(sdl_spend_trg, 
                                                   dma_spend_trg %>% filter(dma == 'NEW YORK, NY'),
                                                   dma_spend_trg %>% filter(dma != 'NEW YORK, NY')) %>%
                                            dplyr::mutate(dma = ifelse(as.character(dma) == "NEW YORK, NY", "OPTIMUM", as.character(dma)))
    
    ##Compute the control GA averaging across the latest of each quarters
    control_ga_Q1 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q1',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q2 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q2',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q3 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q3',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q4 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q4',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)
    
    
    ##Compute the cumulative baseline (control) target by DMA, Suddenlink, ALtice
    control_ga_sum_dma    <- rowSums(control_ga_Q1+control_ga_Q2+control_ga_Q3+control_ga_Q4)
    mean_baseline_trg_dma <- round(control_ga_sum_dma/4)
    control_ga_dma        <- data.frame(dma = dma_names_vec, control_ga = mean_baseline_trg_dma)
    
    control_ga_sum_sdl    <- sum(control_ga_Q1[-17,]+control_ga_Q2[-17,]+control_ga_Q3[-17,]+control_ga_Q4[-17,])
    mean_baseline_trg_sdl <- round(control_ga_sum_sdl/4)
    control_ga_sdl        <- data.frame(dma = 'SUDDENLINK', control_ga = mean_baseline_trg_sdl)
    
    control_ga_sum_altice    <- sum(control_ga_Q1+control_ga_Q2+control_ga_Q3+control_ga_Q4)
    mean_baseline_trg_altice <- round(control_ga_sum_altice/4)
    control_ga_altice        <- data.frame(dma = 'ALTICE', control_ga = mean_baseline_trg_altice)
    
    ## Combine DMA level, Suddenlink level, Altice level dataframes into 1 single dataframe
    final_control_ga <-  control_ga_altice %>% rbind(control_ga_sdl, 
                                                   control_ga_dma %>% filter(dma == 'NEW YORK, NY'),
                                                   control_ga_dma %>% filter(dma != 'NEW YORK, NY')) %>%
                                                dplyr::mutate(dma = ifelse(as.character(dma) == "NEW YORK, NY", "OPTIMUM", as.character(dma)))
    
    final_spend_trg_controlga <- final_spend_trg %>%
                                     dplyr::left_join(final_control_ga, by = c('dma')) %>%
                                     dplyr::group_by(dma) %>%
                                     dplyr::mutate(Media_contribution = Media_GA*100/Media_GA[media == "Total"],
                                                   Media_contribution = ifelse(media == 'Total', Media_GA*100/(Media_GA+control_ga), Media_contribution)) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::mutate_at(.vars = vars('Media_contribution'),.funs = ~round(.,2))
    
    Elasticity_list <- list()
    desired_order <- final_control_ga$dma
    
    Media_contribution <- final_spend_trg_controlga %>%
                                    dplyr::filter(media != 'spend_media_total') %>%
                                    dplyr::select(dma, media, Media_contribution) %>%
                                    dplyr::mutate(dma = factor(dma,levels = desired_order)) %>%
                                    tidyr::spread(media, Media_contribution) %>%
                                    dplyr::select(dma, display_order, Total)
                        
    
    for(change in spend_change){
        
         list_name_up <- paste('Elasticity_up',change,sep = '_')
         list_name_down <- paste('Elasticity_down',change,sep = '_')
        
         var_mean_spend_up <-  paste('mean_spend_up',change,sep='_')
         var_Media_GA_up <-  paste('Media_GA_up',change,sep='_')  
         var_Elasticity_up <- paste('Elasticity_up',change,sep='_')
        
         var_mean_spend_down <-  paste('mean_spend_down',change,sep='_')
         var_Media_GA_down <-  paste('Media_GA_down',change,sep='_')  
         var_Elasticity_down <- paste('Elasticity_down',change,sep='_')

         final_spend_trg_controlga <- final_spend_trg_controlga %>% 
               group_by(dma) %>% 
               mutate(
                      Spend_change_pct_up = ifelse(media %in% activity_based_media,
                                              (UQ(rlang::sym(var_mean_spend_up)) - mean_spend)*100/mean_spend,
                                              (UQ(rlang::sym(var_mean_spend_up)) - mean_spend)*100/mean_spend[media == "spend_media_total"]),
                      GA_change_pct_up    = (UQ(rlang::sym(var_Media_GA_up)) - Media_GA)*100/(Media_GA[media == 'Total'] + control_ga[media == 'Total']),
                      Total_Elasticity_up = GA_change_pct_up/Spend_change_pct_up,
                      
                      Spend_change_pct_down = ifelse(media %in% activity_based_media,
                                                 (UQ(rlang::sym(var_mean_spend_down)) - mean_spend)*100/mean_spend,
                                                 (UQ(rlang::sym(var_mean_spend_down)) - mean_spend)*100/mean_spend[media == "spend_media_total"]),
                      GA_change_pct_down    = (UQ(rlang::sym(var_Media_GA_down)) - Media_GA)*100/(Media_GA[media == 'Total'] + control_ga[media == 'Total']),
                      Total_Elasticity_down = GA_change_pct_down/Spend_change_pct_down) %>% 
               ungroup() %>%
               mutate_at(.vars = vars(Spend_change_pct_up, GA_change_pct_up, Total_Elasticity_up,
                                      Spend_change_pct_down, GA_change_pct_down, Total_Elasticity_down),
                         .funs = ~round(.,2)) %>%
               rename(!!paste('Spend_change_pct_up',change,sep='_')   := Spend_change_pct_up,
                      !!paste('GA_change_pct_up',change,sep='_')      := GA_change_pct_up,
                      !!paste('Elasticity_up',change,sep='_')         := Total_Elasticity_up,
                      !!paste('Spend_change_pct_down',change,sep='_') := Spend_change_pct_down,
                      !!paste('GA_change_pct_down',change,sep='_')    := GA_change_pct_down,
                      !!paste('Elasticity_down',change,sep='_')       := Total_Elasticity_down) %>%
               select(dma, media, mean_spend, Media_GA, Media_contribution, everything(), -starts_with('Spend_change'), -starts_with('GA_change'))

        
          Elasticity_list[[list_name_up]] <- list(final_spend_trg_controlga %>%
                                                dplyr::filter(media != 'Total') %>%
                                                dplyr::select('dma', 'media', var_Elasticity_up) %>%
                                                dplyr::mutate(dma = factor(dma,levels = desired_order)) %>%
                                                tidyr::spread(media, !!paste('Elasticity_up',change,sep='_')) %>%
                                                dplyr::select(dma, display_order, spend_media_total)
                                               )
        
          Elasticity_list[[list_name_down]] <- list(final_spend_trg_controlga %>%
                                                dplyr::filter(media != 'Total') %>%
                                                dplyr::select('dma', 'media', var_Elasticity_down) %>%
                                                dplyr::mutate(dma = factor(dma,levels = desired_order)) %>%
                                                tidyr::spread(media, !!paste('Elasticity_down',change,sep='_')) %>%
                                                dplyr::select(dma, display_order, spend_media_total)
                                               )
    }
    
#     final_spend_trg_controlga <- final_spend_trg_controlga %>% filter(media != 'Total')
    
    
    cat('Order of media:\n')
    cat(paste(c(1:(length(display_order)+1)),c(display_order,'total'),collapse = '\n', sep ='.'))
    
    return(list('Control_GA' = final_control_ga,
                'All_metrics_detailed' = final_spend_trg_controlga,
                'Elasticity' =  Elasticity_list,
                'Media_contribution' = Media_contribution))
    
    
    
}