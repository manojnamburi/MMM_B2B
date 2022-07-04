
###############################################################################################################################################
# cobyla All DMAs
###############################################################################################################################################
gross_add_cobyla <- function(media, idxdma, media_list){
  #'
  #'Compute the Weekly Media Gross Adds for the DMA
  #'
  #'@description
  #'Function to compute the Media Gross Adds
  #'
  #'@details
  #'Compute the weekly gross adds for given weekly spend. Uses the shape
  #'parameters K and S obtained from the model to compute the media gross adds. Used to
  #'compute the objective function for the optimization
  #'
  #'@return 
  #'Returns the media gross adds for a given DMA
  #'
  #'@param media The media matrix of the given DMA
  #'@param idxdma The id of the DMA for which the optimization is being run
  #'@param media_list List of media to optimized
  #'
  
  
  media_GA = c(rep(0, length(media_list)))
  
  idx = 0
  for(idxmedia in media_list){
    idx = idx +1
    #normalize the media to evaluate the Hill  
    dma = dma_names_vec[idxdma]
    channel = media_names_vec[idxmedia]
    media_norm = normalize_x_dma(x=media[idx],min_media_mat,max_media_mat,prospect_mat_media,media = channel,dma = dma)
    if(media_norm <=0){
      media_GA[idx]=0
    } else {
      media_GA[idx] = beta_media_mat[[idxdma,idxmedia]]*
        hill(media_norm, K = K[[channel]], S = S[[channel]])*
        target_max_vec[idxdma]*prospect_mat_media[[idxdma,idxmedia]]
    }
    
  }#idxmedia
  media_GA = as.numeric(media_GA)
  #names(media_GA) <- media_names_prospect
  media_GA
  
}

#objective function for idx period pred time
eval_f0_opt <- function(spend, B, dma, ratio, media_list){
  #'
  #'Objective function of the optimization
  #'
  #'@description 
  #'Function to specify the objective function of the optimization
  #'
  #'@details
  #'This function specifies the objective of the optimization which is to maximize the weekly
  #'Gross Adds. We use the weekly budget to compute the media GA and the objective is to maximize
  #'the media Gross Adds
  #'
  #'@return
  #'Returns the media Gross Adds for the DMA. Negative of the Gross Adds is computed as nloptr 
  #'minimizes the objective function by default.
  #'
  #'@param spend Weekly spend of DMA
  #'@param B Quarterly budget allotted to DMA
  #'@param ratio Ratio of activity to spend
  #'@param constraint Direct Mail constraint variable
  #'@param media_list List of media to be optimized
  #'
  #'@references 
  #'nloptr package: https://cran.r-project.org/web/packages/nloptr/nloptr.pdf
  #' 
  media = spend*ratio
  ga = sum(gross_add_cobyla(media, idxdma=dma, media_list))
  #message(ga)
  return(- ga )
}

#constraint function
eval_g0_opt <- function(spend, B, dma, ratio, media_list){
  #'Constraint function to the optimizer
  #'
  #'@description
  #'Function to specify the budget constraint of the optimization
  #'
  #'@details
  #'Function that defines the budget constraint of the optimizer. 
  #'Constraint: Total weekly spend*number of weeks in the period = Input budget
  #'This is specified by the DMA. Currently as we are using the COBYLA algorithm
  #'under the 'nloptr' package. This does not support equality constraint, so, the
  #'equality constraint is broken down into two inequality constraints to ensure that
  #'the sum(weekly spend)*number of weeks = budget[dma]
  #'
  #'@return 
  #'Returns a vector of two opposite inequalities that translate to the constraint of the budget
  #'
  #'@param spend Weekly spend of DMA
  #'@param B Quarterly budget allotted to DMA
  #'@param ratio Ratio of activity to spend
  #'@param constraint Direct Mail constraint variable
  #'@param media_list List of media that need to be optimized
  #'
  #'@references 
  #'nloptr package: https://cran.r-project.org/web/packages/nloptr/nloptr.pdf
  #'
  eq_constraint = c(0,0)
  eq_constraint[1] = sum(spend) - B
  eq_constraint[2] = B - sum(spend)
  #message(spend)
  #message(contraint)
  return(  eq_constraint)
}

optimize_MMM_weekly <- function(idxdma,
                                dma_budget,
                                media_list_dma,
                                lb_opt_dma,
                                ub_opt_dma,
                                ratio_dma_opt,
                                print_nloptr_log = print_nloptr_log,
                                print_dma_opt = print_dma_opt,
                                prev_solution_dma = NULL){
  #'Get the optimized weekly budget spending by DMA 
  #'
  #'@description
  #'Function that outputs the optimized weekly budget spending based on the input budget
  #'for each DMA. Objective is to maximize the weekly GA.
  #'
  #'@details
  #'Get the optimized budget spend for each dma based on the input quarterly budget specified 
  #'through the optimizer. Takes the budget and list of media by DMA as inputs and calculates 
  #'the optimal media mix by channel.Leverages nloptr package and COBYLA method currently for 
  #'calculation of the optimal spend mix.
  #'The starting point for the optimizer is set to the lower bound of the spending. The lower bound
  #'is computed and inputed in from the optimizer.
  #'For smoothening, the starting point is set to the previous solution point rather than the lower bound.
  #'
  #'
  #'@return
  #'Optimized budget vector by DMA
  #'
  #'@param idxdma ID of the DMA for which the optimization is being run for
  #'@param dma_budget Input quarterly budget for the DMA
  #'@param media_list_dma List of the media that need to be optimized for the DMA
  #'@param lb_opt_dma Lower bound of optimization for the DMA by media
  #'@param ub_opt_dma Upper bound of optimization for the DMA by media
  #'@param ratio_dma_opt Spend/activity ratio by DMA for activity based models to convert activity metrics to spend
  #'@param prev_solution_dma Solution of the previous budget point. Previous budget point is current budget - step_size. Used for smoothening
  #'
  
  
  
  ##Set up the parameters you need for the optimization
  media_opt_dma = media_list_dma
  dma_name = dma_names_vec[idxdma]
  x0_opt = lb_opt_dma
  ##Initialize the starting point of optimization as the lower bound
  if(is.null(prev_solution_dma)==F){
    x0_opt = prev_solution_dma
  }
  
  
  
  cobyla_opt = nloptr(
    ratio = ratio_dma_opt,
    dma = idxdma,
    x0 = x0_opt,
    eval_f= eval_f0_opt,
    lb = lb_opt_dma,
    ub = ub_opt_dma,
    eval_g_ineq = eval_g0_opt,
    media_list = media_opt_dma,
    B = dma_budget,
    opts = list("algorithm"="NLOPT_LN_COBYLA", 
                ftol_rel = 1e-10,
                xtol_rel = 1e-10, 
                maxeval = 1e4
    )
  )
  if(print_nloptr_log){
    cat(paste('NLOPTR fit for',dma_name))
    cat("\n")
    cat(print(cobyla_opt))
  }
  
  if(print_dma_opt){
  cat(paste(dma_name,
            " Budget ", format_money(dma_budget,0),
            " Solution" , format_money(sum(cobyla_opt$solution),0), 
            sep = " "))
  cat('\n')
  flush.console()
  }
  weekly_optimized_solution_dma = cobyla_opt$solution
  
  return(weekly_optimized_solution_dma)
}