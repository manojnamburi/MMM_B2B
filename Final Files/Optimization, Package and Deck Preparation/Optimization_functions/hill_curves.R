if((!require(tidyverse))){install.packages("tidyverse",repos="http://cran.us.r-project.org")}
if((!require(ggdark))){install.packages("ggdark",repos="http://cran.us.r-project.org")}
if((!require(gridExtra))){install.packages("gridExtra",repos="http://cran.us.r-project.org")}

source('Optimization_functions/common_functions_mmm.R')
###Get the take-off point and inflection point by DMA and media
getSpendRegion<- function(hill_df,
                          dma_name,
                          media_name,
                          min_media_mat,
                          max_media_mat,
                          prospect_mat_media,
                          K,S){
    
    media_id = which(media_name == media_names_vec)
    dma_id = which(dma_name == dma_names_vec)    
    K_media = K[media_id]
    S_media = S[media_id]
    ##Locate the starting and ending point of possible inflection point
    ##Round the hill value to 3rd decimal place
    hill_df['hill_value'] = round(hill_df$hill_value,3)
    
    start_point = hill_df[max(which(hill_df$rate_of_slope>0)),'x']
    end_point = hill_df[min(which(hill_df$rate_of_slope<0)),'x']
    
    start_point_norm = normalize_x_dma(x = start_point,min_media_mat = min_media_mat,max_media_mat = max_media_mat,
                                   prospect_mat_media = prospect_mat_media,media = media_name,dma = dma_name)
    end_point_norm = normalize_x_dma(x = end_point,min_media_mat = min_media_mat,max_media_mat = max_media_mat,
                                   prospect_mat_media = prospect_mat_media,media = media_name,dma = dma_name)
    
    
    vector_x = linspace(x1 = start_point_norm,x2 = end_point_norm,n = 1000)
    
    
    xroot_norm = uniroot(f = hill_second_derivative,lower = start_point_norm, upper = end_point_norm, K = K_media, S = S_media)$root
    
    
    
    if(length(xroot_norm)<2){
        spend_start = min(hill_df[which(round(hill_df$hill_value,3)>0),'x'])
        inflection_point = (xroot_norm*(max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id])+min_media_mat[dma_id,media_id])*prospect_mat_media[dma_id,media_id]
    }else if(length(xroot_norm)==2){
        spend_start_norm = xroot_norm[1]
        spend_end_norm = xroot_norm[2]
        spend_start = (spend_start_norm*(max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id])+min_media_mat[dma_id,media_id])*prospect_mat_media[dma_id,media_id]
        inflection_point = (spend_end_norm*(max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id])+min_media_mat[dma_id,media_id])*prospect_mat_media[dma_id,media_id]
    }
    spend_region = c('start_spend' = spend_start,
                     'end_spend' = inflection_point)
    
    return(spend_region)
}

###All media hill curves
plotHill <- function(K,S,
                     cleaned_names_vec,
                     media_spend_df = NULL,
                     x_normal_show = F,
                     lim_x = NULL, lim_y = 0.7){
  ##Input a n*media_names_vec data frame to get the hill curves for all the media
  ##Compute the result matrix for the media
  #cleaned_names_vec = c("Radio","TV","Print","DM","Audio","Display","Video","BrandedSearch", "UnbrandedSearch", "Social")
  cat('Make sure the cleaned_names_vec has the same order as media_names_vec')
  #if(length(cleaned_names_vec)!=length(media_names_vec)){stop(cat('Please ensure the cleaned names has the same length as that of the media names vector.'))}
  if(is.null(media_spend_df)==T){
    x <- list()
    for(i in c(1:length(cleaned_names_vec))){
      x[[i]] <- linspace(0,1)
    }
    x_df <- matrix(unlist(x), nrow=100, byrow=F)
    colnames(x_df) <- cleaned_names_vec
    media_matrix_norm <- x_df
    x_normal_show = T
  }else{
    media_matrix <- as.matrix(media_spend_df)
    colnames(media_matrix) <- cleaned_names_vec
    
    media_matrix_norm <- (media_matrix - min(media_matrix))/(max(media_matrix)-min(media_matrix))
  }
  
  hill_x <- t(apply(media_matrix_norm,FUN = function(x){hill(x,K,S)},MARGIN = 1))
  
  if(x_normal_show){
    x_melt <- melt(media_matrix_norm,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = " "
  }else{
    x_melt <- melt(media_matrix,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = "Spend($)"
  }
  
  
  plot_y_data <- melt(hill_x, value.name = "y", varnames = c("id","media"),as.is = T)
  
 
  plot_data <- cbind.data.frame(x_melt,'y' = plot_y_data$y)
  plot_data$id <- NULL
  
  if(is.null(lim_x)==T){
    lim_x = max(plot_data$x)
  }
  
  display_table = data.frame('Media' = cleaned_names_vec,
                             'K' = K,
                             'S' = S)
  #lim_y = plot_data %>% filter(x <= lim_x) %>% summarize(max = max(y)) %>% pull()
  
         plt <- ggplot(data =plot_data, aes(x=x, y=y, colour=media)) +geom_line(size = 1) +
         xlim(0,lim_x) + ylim(0,lim_y) +
          dark_theme_gray() +
         ggtitle('Hill curve - Media') +
         theme(legend.position="top",
          legend.text = element_text(size=12,face = 'bold'),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
              ) +
           scale_color_brewer(palette = "Paired") + xlab(x_label) + ylab("Hill(x)")
  
         tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
         tbl <- tableGrob(display_table, rows=NULL, theme=tt)
          
          grid.arrange(plt, tbl,
                       nrow=2,
                       as.table=TRUE,
                       heights=c(3,1))
         
  #list('Plot' = plt,"Plot Data" = plot_data)
  
}

##Hill curves by DMA

plotHillDMA <- function(K,S,beta_media_mat,
                     cleaned_names_vec,
                     media_spend_df = NULL,
                     x_normal_show = F,
                     lim_x = NULL, lim_y = 0.7){
  ##Input a n*media_names_vec data frame to get the hill curves for all the media
  ##Compute the result matrix for the media
  #cleaned_names_vec = c("Radio","TV","Print","DM","Audio","Display","Video","BrandedSearch", "UnbrandedSearch", "Social") 
  cat('Make sure the cleaned_names_vec has the same order as media_names_vec')
  cat('\n')
  dma_names_vec1 <- c('Optimum','Suddenlink','Altice',dma_names_vec)
  ##To ensure the betas are aligned to the media names vector names.
  #beta_media_mat <- beta_media_mat[,media_names_vec]  
  beta_df = data.frame(beta_media_mat)
  colnames(beta_df) <- cleaned_names_vec
  #if(length(cleaned_names_vec)!=length(media_names_vec)){stop(cat('Please ensure the cleaned names has the same length as that of the media names vector.'))}
  if(is.null(media_spend_df)==T){
    x <- list()
    for(i in c(1:length(cleaned_names_vec))){
      x[[i]] <- linspace(0,1)
    }
    x_df <- matrix(unlist(x), nrow=100, byrow=F)
    colnames(x_df) <- cleaned_names_vec
    media_matrix_norm <- x_df
    x_normal_show = T
  }else{
    media_matrix <- as.matrix(media_spend_df)
    colnames(media_matrix) <- cleaned_names_vec
    
    media_matrix_norm <- (media_matrix - min(media_matrix))/(max(media_matrix)-min(media_matrix))
  }
    
  output_plot_list = list()  
    
  
  for(DMA in dma_names_vec1){
  ##Get the beta values of all media for the DMA selected
  if(DMA == 'Optimum')
  {
      dma_id = which(dma_names_vec %in% optimum_dma)
      dma_beta_vec = as.numeric(beta_df[dma_id,])
  }else if(DMA == 'Suddenlink'){
      cat('For Suddenlink DMA\n')
      cat('Using the mean beta values across DMAs\n')
      dma_id = -1*which(dma_names_vec %in% optimum_dma)
      beta_subset = beta_df[dma_id,, drop = FALSE]
      dma_beta_vec = as.numeric(apply(beta_subset, FUN = mean, MARGIN = 2))
      
  }else if(DMA == 'Altice'){
      cat('For Altice DMA\n')
      cat('Using the mean beta values across DMAs\n')
      dma_beta_vec = as.numeric(apply(beta_df, FUN = mean, MARGIN = 2))
  }else{
      dma_id = which(dma_names_vec %in% DMA)
      dma_beta_vec = as.numeric(beta_df[dma_id,])
      
  }
  hill_x <- t(apply(media_matrix_norm,FUN = function(x){hill(x,K,S)},MARGIN = 1)*dma_beta_vec)
  
  if(x_normal_show){
    x_melt <- melt(media_matrix_norm,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = " "
  }else{
    x_melt <- melt(media_matrix,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = "Spend($)"
  }
  
  
  plot_y_data <- melt(hill_x, value.name = "y", varnames = c("id","media"),as.is = T)
  
 
  plot_data <- cbind.data.frame(x_melt,'y' = plot_y_data$y)
  plot_data$id <- NULL
  
  if(is.null(lim_x)==T){
    lim_x = max(plot_data$x)
  }
  
  display_table = data.frame('Media' = cleaned_names_vec,
                             'K' = K,
                             'S' = S,
                             'beta' = dma_beta_vec
                             )
  #lim_y = plot_data %>% filter(x <= lim_x) %>% summarize(max = max(y)) %>% pull()
  
         plt <- ggplot(data =plot_data, aes(x=x, y=y, colour=media)) +geom_line(size = 1) +
         xlim(0,lim_x) + ylim(0,lim_y) +
          dark_theme_gray() +
         ggtitle(sprintf('Hill curve - Media (%s)',DMA)) +
         theme(legend.position="top",
          legend.text = element_text(size=12,face = 'bold'),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
              ) +
           scale_color_brewer(palette = "Paired") + xlab(x_label) + ylab(expression(beta*"Hill(x)"))
  
         tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
         tbl <- tableGrob(display_table, rows=NULL, theme=tt)
          
          final_plt <- gridExtra::arrangeGrob(plt, tbl,
                       nrow=2,
                       as.table=TRUE,
                       heights=c(3,1))
      temp_list = list(final_plt)
      temp_list = setNames(temp_list,DMA)
      output_plot_list = append(output_plot_list,temp_list) 
  #list('Plot' = plt,"Plot Data" = plot_data)
  }
  names(output_plot_list) = dma_names_vec1
  return(output_plot_list)
}




###Hill curves by DMA and media
plot_hill_media <- function(data_XyZ,
                            media_names_spend,
                            media_names_vec,
                            cleaned_names_vec,
                            min_media_mat,
                            max_media_mat,
                            ratio_dma,prospect_mat_media,
                            target_max_vec,
                            dma_name,media_name,beta_media_mat,
                            target_name_var,
                            K,S,
                            var_optimization_lb = 0.7,
                            var_optimization_ub = 1.3)
{
    ##If the data doesnt contain the quarter details, add the necessary columns
  quarter_cols <- c('quarter','qy')
  
  if(!(all(quarter_cols%in%colnames(data_XyZ)))){
    data_XyZ <- get_quarters(data_XyZ)
  }
  x0_usa_spend = data_XyZ %>% 
    dplyr::group_by(dma) %>% 
    select(media_names_spend) %>%  
    dplyr::summarize_at(.vars = media_names_spend,.funs = exclude_zero_spend_weeks) %>%  
    dplyr::ungroup() %>%
    dplyr::rename_at(media_names_spend,~media_names_vec) %>% select(media_names_vec) %>%
    as.matrix()
  
  
  cleaned_names_list = as.list(cleaned_names_vec)
  names(cleaned_names_list) = media_names_vec 
  
  qy_spend_summary = data_XyZ %>%
                     dplyr::group_by(dma,qy) %>% 
                     dplyr::select(media_names_spend) %>% 
                     dplyr::summarise_all(mean) %>% 
                     dplyr::ungroup() %>%
                     dplyr::rename_at(media_names_spend,~media_names_vec) %>% 
                     select(dma,qy,media_names_vec)
  
  dma_id = which(dma_names_vec==dma_name)
  media_id = which(media_names_vec==media_name)
  min_media_spend <- 0
  #min_media_spend <- 600000
  max_media_spend <- 2*max(x0_usa_spend[dma_id,])
  
  
  
  ratio_dma_opt = as.numeric(ratio_dma[dma_id,])
  names(ratio_dma_opt) <- media_names_vec
  
  #alternate_min <- (x0_usa_spend*var_optimization_lb)[dma_id,2]
  
  x0 <- seq(min_media_spend,max_media_spend,length.out = 100)
    
  media_norm <- normalize_x_dma(x0,min_media_mat,max_media_mat,prospect_mat_media,media_name,dma_name)
  
  hill_y =  hill(media_norm, K = K[media_id], S = S[media_id])
  
  hill_y[is.na(hill_y)] <- 0
  
  y = beta_media_mat[dma_id,media_id]*hill_y*target_max_vec[dma_id]*target_capita_norm_col[dma_id]
  
  
  
  ##Calculate the specific value of hill at the current spend value
  current_spend <- x0_usa_spend[dma_id,media_id]
  
  current_spend_norm <- normalize_x_dma(current_spend,min_media_mat,max_media_mat,prospect_mat_media,media_name,dma_name)
  
  y_current <- beta_media_mat[dma_id,media_id]*hill(current_spend_norm, K = K[media_id], S = S[media_id])*target_max_vec[dma_id]*target_capita_norm_col[dma_id][1]
  y_current <- ifelse(is.na(y_current),0,y_current)
  ##Calculate the hill function value for the average quarterly spend 
  quarterly_spend <- qy_spend_summary %>% filter(dma == dma_name) %>% select(qy,media_name) 
  
  quarterly_spend_norm <- normalize_x_dma(quarterly_spend[media_name],min_media_mat,max_media_mat,prospect_mat_media,media_name,dma_name)
  
  y_quarter <- beta_media_mat[dma_id,media_id]*hill(quarterly_spend_norm, K = K[media_id], S = S[media_id])*target_max_vec[dma_id]*target_capita_norm_col[dma_id]
  y_quarter[is.na(y_quarter)] <- 0
  
  x_vec <- unlist(quarterly_spend[media_name])
  y_vec <- unlist(y_quarter)
  color_vec <- unlist(quarterly_spend['qy'])
  
  y_hat = hill_derivative(media_norm, K = K[media_id], S = S[media_id])
  
  y_sec_der = hill_second_derivative(media_norm, K = K[media_id], S = S[media_id])
  
  hill_df <- data.frame("x" = x0, "hill_value" = hill_y, "hill_deriv" = y_hat, "rate_of_slope" = y_sec_der)
  
  plot_data <- data.frame("x" = x0,"y" = y)
  
  ##Compute point of influxion
  ideal_spend_region = getSpendRegion(hill_df,dma_name,media_name,min_media_mat,max_media_mat,prospect_mat_media,K,S)
    
  cleaned_media_name <- cleaned_names_list[media_name]
  
  display_table = data.frame('TypeOfSpend' = c('Current spend','Infection point'),
                             'Spend' = c(format_money(current_spend),format_money(ideal_spend_region[2])))
  
  ##Projected target
  plt<- ggplot(data = plot_data,aes(x=x0,y=y)) + geom_line() + 
    geom_line(aes(x=current_spend, color = 'Current spend'),size = 1) +
    geom_line(aes(x=current_spend*var_optimization_ub,color = 'Optimization region'),size = 1) +
    geom_line(aes(x=current_spend*var_optimization_lb,color = 'Optimization region'),size = 1)+
    #annotate("segment", x = tv_spend_region[2], xend = tv_spend_region[2], y = max(plot_data$y)+25, yend = max(plot_data$y), colour = "green", size=1, alpha=0.6, arrow=arrow()) + 
    #geom_line(aes(x = ideal_spend_region[1],color = 'Region of maximum return'),size = 1)+
    geom_line(aes(x = ideal_spend_region[2],color = 'Inflection point'),size = 1) +
    annotate("text",x = ideal_spend_region[2],y = max(plot_data$y)/2,label = 'Inflection point',angle = 90,vjust = -0.5)+
    #annotate("rect", xmin=ideal_spend_region[1], xmax=ideal_spend_region[2], ymin=0, ymax=max(plot_data$y), alpha=0.2, fill="green")+
    annotate("rect", xmin=current_spend*var_optimization_lb, xmax=current_spend*var_optimization_ub, ymin=0, ymax=max(plot_data$y), alpha=0.2, fill="yellow")+
    #geom_point(data = quarterly_spend,aes(x=x_vec, y = y_vec, color = color_vec),size = 5)+
    ggtitle(sprintf('%s projection for %s,%s',target_name_var,cleaned_media_name,dma_name))+
    dark_theme_gray() +
    scale_color_manual(values = c('red','green','yellow'))+
    xlab('Spend($)') +
    ylab(sprintf('%s',Caps(target_var))) +
    theme(legend.position="top",
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(display_table, rows=NULL, theme=tt)
  
  a1 <- grid.arrange(plt, tbl,
               nrow=2,
               as.table=TRUE,
               heights=c(3,1))
  
  ##Hill curve
  a2<- ggplot(data = hill_df, aes(x = x0,y = hill_y)) + geom_line() +
    geom_line(aes(x=current_spend),color = "red",size = 2) +
    geom_line(aes(x=current_spend*var_optimization_ub),color = 'yellow',size = 2) +
    geom_line(aes(x=current_spend*var_optimization_lb),color = 'yellow',size = 2) +
    dark_theme_gray() +
    xlab('Spend($)') +
    ylab('Hill(x)') +
    ggtitle(sprintf('Hill curve for %s,%s',cleaned_media_name,dma_name))+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
         )
  
  #ggsave(sprintf('%s_%s_hill_curve_%s.png',dma_name,media_name,model_type))
  
  hill_df$diff <- hill_df$hill_deriv - dplyr::lag(hill_df$hill_deriv,n=1)
  ideal_spend_region_mid <- mean(ideal_spend_region)

  b <- ggplot(data = hill_df, aes(x = x0)) + 
    geom_line(aes(y = hill_deriv,colour = "First derivative")) +
    geom_line(aes(x=current_spend, y = rate_of_slope/3,colour = 'Current spend'),size = 1) +
    geom_line(aes(y = rate_of_slope/3,colour = "Second derivative")) +
    scale_y_continuous(sec.axis = sec_axis(~.*3,name = 'Rate of slope change(Sec derivative)'))+
    geom_point(aes(x = ideal_spend_region[2],y=0))+
    annotate("text",x = ideal_spend_region[2],y = 0,label = 'Inflection point',vjust = -1)+
    #annotate("rect", xmin=ideal_spend_region[1], xmax=ideal_spend_region[2], ymin=-Inf, ymax=max(hill_df$hill_deriv,na.rm = T), alpha=0.2,color = 'green', fill="green")+
    #annotate("text",x = ideal_spend_region_mid,y = (max(hill_df$hill_deriv,na.rm = T)),label = 'Region of maximum return',vjust = -0.5) +
    #geom_line(aes(x=current_spend*var_optimization_lb),color = 'yellow',size = 2) +
    dark_theme_gray() +
    xlab('Spend($)') +
    ylab('Hill deriv(x)') +
    scale_color_manual(values = c('red','green','blue')) + 
    ggtitle(sprintf('Deriv Hill curve for %s,%s',cleaned_media_name,dma_name)) +
    theme(
        legend.position="top",
        legend.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()
         )
  
  #ggsave(sprintf('%s_%s_hill_curve_deriv_%s.png',dma_name,media_name,model_type))
  
  c <- ggplot(data = hill_df, aes(x = x0,y = y_sec_der)) + geom_line() +
    geom_line(aes(x=current_spend),color = "red",size = 2) +
    geom_line(aes(x=current_spend*var_optimization_ub),color = 'yellow',size = 2) +
    geom_line(aes(x=current_spend*var_optimization_lb),color = 'yellow',size = 2) +
    dark_theme_gray() +
    xlab('Spend($)') +
    ylab('Hill sec deriv(x)') +
    ggtitle(sprintf('Sec.derivative Hill curve for %s,%s',cleaned_media_name,dma_name)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
         )
  
  #ggsave(sprintf('%s_%s_hill_curve_sec_deriv_%s.png',dma_name,media_name,model_type))
  
  return_list <- list('Projected_target' = a1,
                      'Hill_curve' = a2,
                      'Hill_deriv_curve' = b,
                      'Hill_df' = hill_df)
  
  return(return_list)
}