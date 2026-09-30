contains <- function(x, keyword){
  
# we want to see if a string contains a substring
# we apply this on a vector of strings
# we return a vector of booleans. Each result corresponds with the ith string
# in the vector of strings
#   
# -------------------------------------------------------------
# input:
#   - x (vector of strings): string of interest
#   - keyword (string): substring of interest
#   
# output:
#   - end_array (vector of booleans): end_array[i] is TRUE if keyword is located in x[i]
# -------------------------------------------------------------
  
  
  end_array <- c()
  for(i in x){
    end_array <- append(end_array, grepl(keyword, i))
  }
  end_array
}


ends_with <- function(x, keyword){
  
  # we want to see if a string ends with a substring
  # we apply this on a vector of strings
  # we return a vector of booleans. Each result corresponds with the ith string
  # in the vector of strings
  # 
  # -------------------------------------------------------------
  # input:
  #   - x (vector of strings): string of interest
  #   - keyword (string): substring of interest
  # 
  # output:
  #   - end_array (vector of booleans): end_array[i] is TRUE if x[i] ends with the keyword
  # -------------------------------------------------------------
  
  keyword_chars <- nchar(keyword)
  end_array <- c()
  for(i in x){
    candidate_chars <- nchar(i)
    if(candidate_chars >= keyword_chars){
      
      # extract the ending chracters
      end_slice <- substr(i, candidate_chars - keyword_chars + 1, candidate_chars)
      
      if(end_slice == keyword){ #if the slice is equal to the keyword
        end_array <- append(end_array, T)
      } else{
        end_array <- append(end_array, F)
      }
    }
    else{ #if the string of interest is already smaller than the keyword, it must be false
      end_array <- append(end_array, F)
    }
  }
  end_array
}

#I want to be able to get the loess curve for y ~ x stratified on a factor z with 3 levels

loess_stratify <- function(df, x_var, y_var, stratify_var, three_timepoints){
  
  # I want to be able to get the loess curve for y ~ x stratified on a factor z with 3 levels  
  #
  # ---------------------------------------------------------------
  # input:
  #   - df (dataframe):
  #   - x_var (string): name of the x variable 
  #   - y_var (string): name of the y variable
  #   - stratify_var (string): name of the stratifying variable
  #   - three_timepoints (boolean):  TRUE if we want three timepoints (2008 summer, 2016 spring, 2016 summer)
  #                                  FALSE if we only want two timepoints (2008 summer, 2016 summer)
  #                                  
  # output:
  #   - df2 (dataframe): df input dataframe with columns named 'loess', 'lower', and 'upper' for the loess curve
  #
  # ---------------------------------------------------------------
  
  loess_eq <- paste(y_var, x_var, sep = ' ~ ')
  
  if(three_timepoints){
    
    # create three dataframes for all 3 timepoints
    df_a <- df %>% filter(Index == 1)
    df_b <- df %>% filter(Index == 2)
    df_c <- df %>% filter(Index == 3)
    
    # create loess and CI values for all 3 timepoints
    temp <- predict(loess(formula(loess_eq), data = df_a, span = 0.75), se=TRUE)
    df_a$loess <- temp$fit
    df_a$lower <- temp$fit - 1.96 * temp$se.fit
    df_a$upper <- temp$fit + 1.96 * temp$se.fit
    
    temp <- predict(loess(formula(loess_eq), data = df_b, span = 0.75), se=TRUE)
    df_b$loess <- temp$fit
    df_b$lower <- temp$fit - 1.96 * temp$se.fit
    df_b$upper <- temp$fit + 1.96 * temp$se.fit

    temp <- predict(loess(formula(loess_eq), data = df_c, span = 0.75), se=TRUE)
    df_c$loess <- temp$fit
    df_c$lower <- temp$fit - 1.96 * temp$se.fit
    df_c$upper <- temp$fit + 1.96 * temp$se.fit        
    
    df2 <- rbind(df_a, df_b)
    df2 <- rbind(df2, df_c)
    
  } else{
    
    # create two dataframes 
    df_a <- df %>% filter(Index == 1)
    df_c <- df %>% filter(Index == 3)
    
    # create loess and CI values for all 2 timepoints
    
    temp <- predict(loess(formula(loess_eq), data = df_a, span = 0.75), se=TRUE)
    df_a$loess <- temp$fit
    df_a$lower <- temp$fit - 1.96 * temp$se.fit
    df_a$upper <- temp$fit + 1.96 * temp$se.fit
    
    temp <- predict(loess(formula(loess_eq), data = df_c, span = 0.75), se=TRUE)
    df_c$loess <- temp$fit
    df_c$lower <- temp$fit - 1.96 * temp$se.fit
    df_c$upper <- temp$fit + 1.96 * temp$se.fit        
    
    df2 <- rbind(df_a, df_c)
  }
  
  return(df2)
  
}

is_significant <- function(df){
  pos_sig <- apply(df, 1, function(x){all(x > 0)})
  neg_sig <- apply(df, 1, function(x){all(x < 0)})
  
  return(pos_sig | neg_sig)
}


# I want to create a graph for stratified analysis
# I supply estimates and CI's and classes
# group_names <- c('cortisol', 'testosterone', 'estradiol', 'dhea')
# strat_levels <- c('0-25%ile', '25-50%ile', '50-75%ile', '75-100%ile')

# additional_arguments is a list of:

create_CI_graph <- function(df_est, df_lower, df_upper, group_names, strat_levels, 
                           additional_arguments){
  
  # load additional arguments
  xlab_name <- additional_arguments[[1]]
  ylab_name <- additional_arguments[[2]]
  the_title <- additional_arguments[[3]]
  the_colors <- additional_arguments[[4]]
  y_adj <- additional_arguments[[5]]
  asterisk_size <- additional_arguments[[6]]
  file_name <- additional_arguments[[7]]
  
  
  n_groups <- length(group_names)
  n_strat <- length(strat_levels)
  
  df_graph <- data.frame(t(df_est), t(df_lower), t(df_upper))
  
  df_graph <- cbind(df_graph, is_significant(df_graph))
  
  colnames(df_graph) <- c('est', 'lower', 'upper', 'significant')
  df_graph <- df_graph %>% mutate(stratif = rep(strat_levels, n_groups)) %>%
    mutate(hormone = rep(group_names, each = n_strat))
  
  signif_vec <- rep('', nrow(df_graph))
  signif_vec[df_graph$significant == T] <- '*'
  df_graph$signif <- signif_vec
  
  # significance
  
  
  df_test <- data.frame(rep(0, n_groups), group_names)
  colnames(df_test) <- c('int', 'groups')
  
  
  g_stratif <- ggplot(data = df_graph) +
    geom_point(aes(x = hormone, y = est, color = stratif),position = position_dodge(width = 0.5) ) +
    geom_errorbar(position = position_dodge(width = 0.5), width = 0.25,
                  aes(x = hormone,
                      ymin = lower,
                      ymax = upper,
                      color = stratif)) +
    geom_hline(data = df_test, aes(yintercept = 0), linetype = "dashed") +
    scale_x_discrete(guide = guide_axis(angle = 30)) +
    xlab(xlab_name) +
    ylab(ylab_name) +
    ggtitle(the_title) +
    scale_color_manual(values = the_colors) +
    guides(color=guide_legend("Stratified Category")) +
    theme_bw() +
    geom_text(aes(x = hormone,
                  y = upper + y_adj,
                  group = stratif,
                  label = signif),
              position = position_dodge(width = 0.5),
              size = asterisk_size)
  
  file_path <- paste('Figures', file_name, sep = '/')
  
  plot_dir <- paste(substr(dir,1, nchar(dir)-4), file_path, sep = '')
  
  pdf(plot_dir)
  print(g_stratif)
  dev.off()
  
  return(g_stratif)
}
