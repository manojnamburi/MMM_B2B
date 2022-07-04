
#source('alt_optimizer.R')
budget_adjustment <- function(weekly_spend_df,
                              weekly_target_df,
                              media_list,
                              id_opt_case,
                              media_for_adjustment)

{
  #'Budget Adjustments for interacting media
  #'
  #'@description
  #'Function to take the evaluation results from optimizer and adjust budgets to smoothen out the sudden spikes and dips in budget allocation
  #'at various spend levels
  #'
  #'@details
  #'This code is currently used only for OPTIMUM
  #'It computes the share of budget between media (1 & 2), (3 & 4)
  #'Using this share it reallocates the budget between media (1 & 2), (3 & 4)
  #'
  #'
  #'@param weekly_spend_df Output from the test_case function which has budget mix at various spend levels
  #'@param weekly_target_df Output from the test_case function which has GA at various spend levels
  #'@param media_list media list
  #'@param id_opt_case 17 for OPTIMUM, -17 for SDL
  #'@param media_for_adjustment vector of media which needs to be adjusted. (1 & 2), (3 & 4) will be treated seperately
  #'
  #'@return adjusted weekly_spend_df, adjusted weekly_target_df
  #'
  

  weekly_spend_df_alt  <- weekly_spend_df
  weekly_target_df_alt <- weekly_target_df
  idxdma_list          <- c(1:length(dma_names_vec[id_opt_case]))
   
    for(idxdma in idxdma_list){
      DMA = dma_names_vec[id_opt_case][idxdma]
      # print(DMA)
      dma_res <- weekly_spend_df_alt %>% 
                 dplyr::filter(dma == DMA) %>%
                 dplyr::mutate(
                              '{media_for_adjustment[1]}_{media_for_adjustment[2]}' := !!rlang::sym(media_for_adjustment[1])+!!rlang::sym(media_for_adjustment[2]),  

                              '{media_for_adjustment[1]}_pct' := sum(!!rlang::sym(media_for_adjustment[1]))/(sum(!!rlang::sym(media_for_adjustment[1]))+sum(!!rlang::sym(media_for_adjustment[2]))), 
                              '{media_for_adjustment[2]}_pct' := sum(!!rlang::sym(media_for_adjustment[2]))/(sum(!!rlang::sym(media_for_adjustment[1]))+sum(!!rlang::sym(media_for_adjustment[2]))), 

                              '{media_for_adjustment[1]}'     := ifelse(is.na(!!rlang::sym(paste0(media_for_adjustment[1],'_pct'))),
                                                                        !!rlang::sym(media_for_adjustment[1]),
                                                                        !!rlang::sym(paste0(media_for_adjustment[1],'_pct'))*(!!rlang::sym(media_for_adjustment[1])+!!rlang::sym(media_for_adjustment[2]))),

                              '{media_for_adjustment[2]}'     := ifelse(is.na(!!rlang::sym(paste0(media_for_adjustment[2],'_pct'))),
                                                                        !!rlang::sym(media_for_adjustment[2]),
                                                                        !!rlang::sym(paste0(media_for_adjustment[1],'_',media_for_adjustment[2])) - !!rlang::sym(media_for_adjustment[1])
                                                                       ),

                              # ------------  tried this but gave wrong output --------------- 
                              # '{media_for_adjustment[2]}'     := ifelse(is.na(!!rlang::sym(paste0(media_for_adjustment[2],'_pct'))),
                              #                                           !!rlang::sym(media_for_adjustment[2]),
                              #                                           !!rlang::sym(paste0(media_for_adjustment[2],'_pct'))*(!!rlang::sym(media_for_adjustment[1])+!!rlang::sym(media_for_adjustment[2]))),

                              '{media_for_adjustment[3]}_{media_for_adjustment[4]}' := !!rlang::sym(media_for_adjustment[3])+!!rlang::sym(media_for_adjustment[4]),

                              '{media_for_adjustment[3]}_pct' := sum(!!rlang::sym(media_for_adjustment[3]))/(sum(!!rlang::sym(media_for_adjustment[3]))+sum(!!rlang::sym(media_for_adjustment[4]))), 
                              '{media_for_adjustment[4]}_pct' := sum(!!rlang::sym(media_for_adjustment[4]))/(sum(!!rlang::sym(media_for_adjustment[3]))+sum(!!rlang::sym(media_for_adjustment[4]))), 

                              '{media_for_adjustment[3]}'     := ifelse(is.na(!!rlang::sym(paste0(media_for_adjustment[3],'_pct'))),
                                                                        !!rlang::sym(media_for_adjustment[3]),
                                                                        !!rlang::sym(paste0(media_for_adjustment[3],'_pct'))*(!!rlang::sym(media_for_adjustment[3])+!!rlang::sym(media_for_adjustment[4]))),

                              '{media_for_adjustment[4]}'     := ifelse(is.na(!!rlang::sym(paste0(media_for_adjustment[4],'_pct'))),
                                                                        !!rlang::sym(media_for_adjustment[4]),
                                                                        !!rlang::sym(paste0(media_for_adjustment[3],'_',media_for_adjustment[4])) - !!rlang::sym(media_for_adjustment[3])
                                                                       )

                              # ------------  tried this but gave wrong output --------------- 
                              # '{media_for_adjustment[4]}'     := ifelse(is.na(!!rlang::sym(paste0(media_for_adjustment[4],'_pct'))),
                              #                                           !!rlang::sym(media_for_adjustment[4]),
                              #                                           !!rlang::sym(paste0(media_for_adjustment[4],'_pct'))*(!!rlang::sym(media_for_adjustment[3])+!!rlang::sym(media_for_adjustment[4])))
                              )                                         


        
        input_qtr_budget <- dma_res['Input_quarterly_budget']

        for(i in c(1:as.numeric(count(input_qtr_budget))))
        {
          corrected_weekly_spend = 0*beta_media_mat
          colnames(corrected_weekly_spend) <- media_names_vec
          
          # print(corrected_weekly_spend)
          ##Reference: https://stackoverflow.com/questions/54919400/dplyr-filter-using-dynamic-column-name-and-dynamic-value/54919698#54919698
          ##Using dynamic name of the spend_variable to filter
          new_mix <- dma_res %>% 
                          filter(Input_quarterly_budget==input_qtr_budget[i,]) %>% 
                          select(media_names_vec)

          media_list_dma = media_list[[idxdma]]

          ##Assign the corrected spend of the respective DMA to the matrix
          id_dma <-  which(dma_names_vec==DMA)
          corrected_weekly_spend[id_dma,] <- as.numeric(new_mix)


          optimized_ga = gross_add_weekly(corrected_weekly_spend,
                                          min_media_mat = min_media_mat,
                                          max_media_mat = max_media_mat,
                                          ratio_dma = spend_activity_ratio_dma_df,
                                          prospect_mat_media = prospect_mat_media,
                                          y_max = target_max_vec,
                                          beta_media_df = beta_media_mat,K,S, target_capita_norm_col)
          
          dma_weekly_ga = apply(optimized_ga,sum,MARGIN = 1)
          
          corrected_ga <- dma_weekly_ga
          
          ##Apply the correction to the dma_res object
          dma_res[i,target_var]      <- corrected_ga[id_dma]
          dma_res[i,media_names_vec] <- corrected_weekly_spend[id_dma,media_names_vec]
          
          ##Apply the correction to the target as well
          weekly_target_df_alt[i,media_names_vec] <- optimized_ga[id_dma,media_names_vec]
          weekly_target_df_alt[i,target_var]      <- corrected_ga[id_dma]
        }

      ##Drop the intermediate columns used to compute the adjustments
      dma_res <- dma_res %>% select(colnames(weekly_spend_df_alt))

      ##Assign the corrected df back to the weekly_spend_df
      weekly_spend_df_alt[weekly_spend_df_alt$dma==DMA,] <- dma_res
      
      return(list('corrected_spend_df' = weekly_spend_df_alt,
                  'corrected_target_df' = weekly_target_df_alt)
            )
    }
}