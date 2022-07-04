#'Get the Optimization Outputs for the Given Inputs
#'
#'
#'@description
#'Function to get the distribution of media from the Quarterly Media Mix by footprint
#'
#'@details
#'Function that calls the get_quarterly_mix function and is used to expose the API for
#'obtaining the media mix.
#'
#'Generates a summary table by footprint and formatted output by media
#'
#'The input is input which is a generic JSON which is treated as a dataframe for generation
#'of the optimal mix.
#'
#'@return For output - Footprint level metrics like target and spend
#'
#'@param input JSON object containing all the inputs associated with the particular MMM
#'
#'@section Note:
#'For MMM Acquisition, for this function to work, please specify the following inputs:
#'
#'Input_OPT_budget, \cr
#'Input_SDL_budget, \cr
#'Input_OPT_crosschannel_imp, \cr
#'Input_SDL_crosschannel_imp, \cr
#'Input_OPT_email_sent, \cr
#'Input_SDL_email_sent, \cr
#'input_period, \cr
#'
#'@export


run_b2b_app <- function(input){

  ##-------------------Input initialization and tests--------------------------------

  ##Accept the inputs either as csv or as a JSON call
  input_data <- if(is.character(input) && file.exists(input))
  {
    read.csv(input)
  } else {
    as.data.frame(input)
  }

  ##Required inputs
  stopifnot("Input_OPT_budget" %in% names(input_data))
  stopifnot("Input_SDL_budget" %in% names(input_data))

  stopifnot("Input_OPT_crosschannel_imp" %in% names(input_data))
  stopifnot("Input_SDL_crosschannel_imp" %in% names(input_data))

  #stopifnot("Input_OPT_email_sent" %in% names(input_data))
  #stopifnot("Input_SDL_email_sent" %in% names(input_data))

  stopifnot("input_period" %in% names(input_data))

  ##Additional flight level levers by footprint
  stopifnot("flight_flag_radio_OPT" %in% names(input_data))
  stopifnot("flight_flag_radio_SDL" %in% names(input_data))

  stopifnot("flight_flag_DRTV_OPT" %in% names(input_data))
  stopifnot("flight_flag_DRTV_SDL" %in% names(input_data))

  ##Adding the downloadable csv similar to acquisition as an additional input
  stopifnot("output_type" %in% names(input_data))


  ###The datasets are loaded from the Data folder when the package is called

  ##-------------------Initialize parameters needed for the running of the app---------
  ##--------------------------------Media parameters ----------------------------------
  ##Using <<- to make the variables accessible to the Global environment which is the parent
  ##environment for all functional environments and also since these are not modified anywhere
  ##we fix these values at the Global level
  ##Sai Yadalam: Commenting out Email as of 04/07/2022 as email is now a control variable

  beta_media_mat <<- model_params$media_params$beta_media_mat
  K <<- model_params$media_params$K
  S <<- model_params$media_params$S
  spend_activity_ratio_dma_df <<- model_params$media_params$spend_activity_ratio_dma_df
  min_media_mat <<- model_params$media_params$min_media_mat
  max_media_mat <<- model_params$media_params$max_media_mat
  media_names_vec <<- model_params$media_params$media_names_vec
  cleaned_names_list <<- model_params$media_params$cleaned_names_list


  ##Correcting Media names
  cleaned_names_list['cross_channel_imp'] = 'Cross Channel Imp'
  cleaned_names_list['dm'] = 'Direct Mail'
  cleaned_names_list['digital'] = 'Digital'
  cleaned_names_list['DRTV_w'] = 'DRTV'
  cleaned_names_list['radio_w'] = 'Radio'
  cleaned_names_list['social'] = 'Social'
  cleaned_names_list['paid_search'] = 'Paid Search'
  #cleaned_names_list['email_sent_w'] = 'Email Vol sent'
  cleaned_names_list <<- cleaned_names_list

  # Media spend names only
  #media_names_spend_only = media_names_vec[!(media_names_vec %in% c('cross_channel_imp','email_sent_w'))]
  media_names_spend_only = media_names_vec[!(media_names_vec %in% c('cross_channel_imp'))]
  
  prospect_mat_media <<- model_params$media_params$prospect_mat_media

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
  dma_names_vec <<- model_params$other_params$dma_names_vec
  optimum_dma <<- model_params$other_params$optimum_dma

  ##--------------------------------Input parameters -------------------------------------
  var_OPT_budget <- input_data$Input_OPT_budget
  var_SDL_budget <- input_data$Input_SDL_budget

  var_OPT_crosschannel_imp <- input_data$Input_OPT_crosschannel_imp
  var_SDL_crosschannel_imp <- input_data$Input_SDL_crosschannel_imp

  #var_OPT_email_sent <- input_data$Input_OPT_email_sent
  #var_SDL_email_sent <- input_data$Input_SDL_email_sent

  var_period     <- input_data$input_period

  flight_flag_radio_OPT <- input_data$flight_flag_radio_OPT
  flight_flag_radio_SDL <- input_data$flight_flag_radio_SDL

  flight_flag_DRTV_OPT <- input_data$flight_flag_DRTV_OPT
  flight_flag_DRTV_SDL <- input_data$flight_flag_DRTV_SDL

  var_output_type <- input_data$output_type


  ##Default parameters
  var_flight_media_OPT    <- c('dm')
  #var_constr_media_OPT    <- c('cross_channel_imp','email_sent_w')
  var_constr_media_OPT    <- c('cross_channel_imp')

  var_flight_media_SDL    <- c('dm')
  #var_constr_media_SDL    <- c('cross_channel_imp','email_sent_w')
  var_constr_media_SDL    <- c('cross_channel_imp')

  #constr_media_budget_OPT <- c(var_OPT_crosschannel_imp, var_OPT_email_sent)
  constr_media_budget_OPT <- c(var_OPT_crosschannel_imp)
  #constr_media_budget_SDL <- c(var_SDL_crosschannel_imp, var_SDL_email_sent)
  constr_media_budget_SDL <- c(var_SDL_crosschannel_imp)

  ##Based on the flags set by footprint, change the flight media and constrained media

  if(flight_flag_radio_OPT & flight_flag_DRTV_OPT){
    opt_res <- opt_all_on
    var_n_flight_period_OPT <- 6
    var_flight_media_OPT <- append(var_flight_media_OPT,c('DRTV_w','radio_w'))
  }else if(flight_flag_radio_OPT){
    opt_res <- opt_drtv_off
    var_n_flight_period_OPT <- 6
    var_flight_media_OPT <- append(var_flight_media_OPT,c('radio_w'))
    constr_media_budget_OPT <- append(constr_media_budget_OPT,c(0))
    var_constr_media_OPT <- append(var_constr_media_OPT,c('DRTV_w'))
  }else if(flight_flag_DRTV_OPT){
    opt_res <- opt_radio_off
    var_n_flight_period_OPT <- 6
    var_flight_media_OPT <- append(var_flight_media_OPT,c('DRTV_w'))
    constr_media_budget_OPT <- append(constr_media_budget_OPT,c(0))
    var_constr_media_OPT <- append(var_constr_media_OPT,c('radio_w'))
  }else{
    opt_res <- opt_drtv_off_radio_off
    var_n_flight_period_OPT <- 6
    constr_media_budget_OPT <- append(constr_media_budget_OPT,c(0,0))
    var_constr_media_OPT <- append(var_constr_media_OPT,c('DRTV_w','radio_w'))
  }


  if(flight_flag_radio_SDL & flight_flag_DRTV_SDL){
    sdl_res <- sdl_all_on
    var_n_flight_period_SDL <- 6
    var_flight_media_SDL <- append(var_flight_media_SDL,c('DRTV_w','radio_w'))
  }else if(flight_flag_radio_SDL){
    sdl_res <- sdl_drtv_off
    var_n_flight_period_SDL <- 6
    var_flight_media_SDL <- append(var_flight_media_SDL,c('radio_w'))
    constr_media_budget_SDL <- append(constr_media_budget_SDL,c(0))
    var_constr_media_SDL <- append(var_constr_media_SDL,c('DRTV_w'))
  }else if(flight_flag_DRTV_SDL){
    sdl_res <- sdl_radio_off
    var_n_flight_period_SDL <- 6
    var_flight_media_SDL <- append(var_flight_media_SDL,c('DRTV_w'))
    constr_media_budget_SDL <- append(constr_media_budget_SDL,c(0))
    var_constr_media_SDL <- append(var_constr_media_SDL,c('radio_w'))
  }else{
    sdl_res <- sdl_drtv_off_radio_off
    var_n_flight_period_SDL <- 6
    constr_media_budget_SDL <- append(constr_media_budget_SDL,c(0,0))
    var_constr_media_SDL <- append(var_constr_media_SDL,c('DRTV_w','radio_w'))
  }


  var_id_opt <<- which(dma_names_vec %in% optimum_dma)

  ##-------------------------------Main code section -------------------------------------


  res <- get_quarterly_mix(input_quarterly_budget_opt = var_OPT_budget,
                           input_quarterly_budget_sdl = var_SDL_budget,
                           constr_media_vec_OPT = var_constr_media_OPT,
                           constr_media_vec_SDL = var_constr_media_SDL,
                           constr_media_budget_OPT = constr_media_budget_OPT,
                           constr_media_budget_SDL = constr_media_budget_SDL,
                           smoothened_res_OPT = T,
                           smoothened_res_SDL = F,
                           sim_results_opt = opt_res,
                           sim_results_sdl = sdl_res,
                           control_beta_mat = beta_control_mat,
                           control_mat = control_mat,
                           control_names_vec = control_names_vec,
                           flight_media_vec_OPT = var_flight_media_OPT,
                           flight_media_vec_SDL = var_flight_media_SDL,
                           var_n_flight_period_OPT = var_n_flight_period_OPT,
                           var_n_flight_period_SDL = var_n_flight_period_SDL,
                           var_period = var_period,
                           target_capita_norm_col = target_capita_norm_col,
                           spend_media_vec = media_names_spend_only
  )


  ##------------------------Format output for the app----------------------------

  # Showing Media Spend mix and TGA in the UI
  media_spend_df <- res$Formatted$Footprint$Spend
  tga_df   <- res$Formatted$Footprint$TGA

  # Recoding footprint names
  recode_vec <- c('USA','EAST','WEST')
  names(recode_vec) <- c('Altice USA','Optimum','Suddenlink')

  # Compute actual cross channel impressions which will be shown in Summary table
  total_cc_imp <- var_OPT_crosschannel_imp + var_SDL_crosschannel_imp
  cc_imp_df <- data.frame('Footprint' = recode_vec,
                          'cc_imp' = c(total_cc_imp,var_OPT_crosschannel_imp,var_SDL_crosschannel_imp),
                          stringsAsFactors = F) %>%
    dplyr::mutate_at(.vars = vars('cc_imp'),.funs = ~format(.,big.mark = ','))

  # # Compute actual email sent which will be shown in Summary table
  # total_email_sent <- var_OPT_email_sent + var_SDL_email_sent
  # email_vol_df <- data.frame('Footprint' = recode_vec,
  #                            'Email_Volume' = c(total_email_sent,var_OPT_email_sent,var_SDL_email_sent),
  #                            stringsAsFactors = F) %>%
  #   dplyr::mutate_at(.vars = vars('Email_Volume'),.funs = ~format(.,big.mark = ','))

  # Media channel names in descending order based on their spend with respect to USA
  media_order_desc <- media_spend_df %>%
    #dplyr::select(-c('Cross Channel Imp', 'Email Vol sent')) %>%
    dplyr::select(-c('Cross Channel Imp')) %>%
    dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec)) %>%
    dplyr::filter(Footprint == 'USA') %>%
    dplyr::mutate_at(.vars = vars(-'Footprint'),.funs = ~as.numeric(gsub('[$,]', '', .))) %>%
    tidyr::gather(key=media, value=spend, -c(Footprint, Total)) %>%
    dplyr::arrange(desc(spend)) %>%
    dplyr::select(media) %>%
    pull()


  if(var_output_type == 'json'){

    ## spend table
    spend_df <- media_spend_df %>%
      #dplyr::select(-c('Cross Channel Imp', 'Email Vol sent')) %>%
      dplyr::select(-c('Cross Channel Imp')) %>%
      dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec)) %>%
      dplyr::select('Footprint', 'Total', media_order_desc) %>%
      dplyr::mutate_at(.vars = vars(-'Footprint'),.funs = ~format_money(as.numeric(gsub('[$,]', '', .)),0)) %>%
      dplyr::rename('Total Spend $(000)' = 'Total', 'name' = 'Footprint') %>%
      dplyr::mutate(id = row_number(),
                    "__parent" = ifelse(name =='USA','parent','1'),
                    "__color"  = ifelse(name == 'USA', '#000000',
                                        ifelse(name == 'EAST', '#0084d6', '#ff7f32')),
                    "expanded" = ifelse(name =='USA','true','false')
      )

    ## summary table
    summary_df <- media_spend_df %>%
      dplyr::left_join(tga_df, by = "Footprint") %>%
      dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec)) %>%
      dplyr::left_join(cc_imp_df,by = 'Footprint') %>%
      #dplyr::left_join(email_vol_df,by = 'Footprint') %>%
      #dplyr::select('Footprint', 'cc_imp', 'Email_Volume', 'TGA') %>%
      dplyr::select('Footprint', 'cc_imp', 'TGA') %>%
      #dplyr::rename('name' = "Footprint", 'Cross Channel Impressions' = 'cc_imp', 'Email Volume' = 'Email_Volume', 'Gross Adds' = 'TGA') %>%
      dplyr::rename('name' = "Footprint", 'Cross Channel Impressions' = 'cc_imp', 'Gross Adds' = 'TGA') %>%
      dplyr::mutate(id = row_number(),
                    "__parent" = ifelse(name =='USA','parent','1'),
                    "__color"  = ifelse(name == 'USA', '#000000',
                                        ifelse(name == 'EAST', '#0084d6', '#ff7f32')),
                    "expanded" = ifelse(name =='USA','true','false')
      )

    ##Returns summary & spend dataframes
    output_list = list('summary' = summary_df,
                       'spend' =  spend_df)


  }else if(var_output_type == 'csv'){

    ## CSV output
    footprint_df <- media_spend_df %>%
      dplyr::left_join(tga_df, by = "Footprint") %>%
      dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec)) %>%
      dplyr::left_join(cc_imp_df,by = 'Footprint') %>%
      #dplyr::left_join(email_vol_df,by = 'Footprint') %>%
      #dplyr::select('Footprint', 'Total', media_order_desc, 'TGA', 'cc_imp', 'Email_Volume') %>%
      dplyr::select('Footprint', 'Total', media_order_desc, 'TGA', 'cc_imp') %>%
      #dplyr::mutate_at(.vars = vars(-c('Footprint','TGA','cc_imp', 'Email_Volume')),.funs = ~format_money(as.numeric(gsub('[$,]', '', .)),0)) %>%
      dplyr::mutate_at(.vars = vars(-c('Footprint','TGA','cc_imp')),.funs = ~format_money(as.numeric(gsub('[$,]', '', .)),0)) %>%
      #dplyr::rename('Cross Channel Impressions' = 'cc_imp', 'Email Volume' = 'Email_Volume', 'Gross Adds' = 'TGA', 'Total Spend $(000)' = 'Total') %>%
      dplyr::rename('Cross Channel Impressions' = 'cc_imp', 'Gross Adds' = 'TGA', 'Total Spend $(000)' = 'Total') %>%
      dplyr::rename_at(.vars = vars('Total Spend $(000)'), .funs = ~paste(.,sprintf('[%s]',var_period)))

    ##Return the footprint_df for download
    output_list = footprint_df

  }else{
    stop(cat('Wrong output type specified. Use either json or csv as output_type param\n'))
  }


  return(output_list)

}
