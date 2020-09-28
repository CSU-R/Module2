padded_data.frame <- function(l, ...){
  # pad with NAs
  na.pad <- function(x,len){
    x[1:len]
  }
  # make data frame
  maxlen <- max(sapply(l, length))
  ret <- data.frame(lapply(l, na.pad, len = maxlen), ...)
  return(ret)
}

stack_predictions <- function(preds_wide){
  tmp1 <- preds_wide %>%
    select(., id, name_pt = home, starts_with('home')) %>%
    set_names(., str_remove_all(names(.), 'home_')) %>%
    mutate(., at_home = TRUE)
  
  tmp2 <- preds_wide %>%
    select(., id, name_pt = road, starts_with('road')) %>%
    set_names(., str_remove_all(names(.), 'road_')) %>%
    mutate(., at_home = FALSE)
  
  ret <- tmp1 %>%
    bind_rows(., tmp2) %>%
    arrange(id, desc(at_home)) %>%
    rename(., home = at_home) %>%
    select(., -id)
  
  return(ret)
}

get_events <- function(bov_json){
  require(tidyverse)
  
  events <- bov_json %>%
    list.clean(., is.list) %>%
    as_tibble() %>%
    mutate(., startTime = as_datetime(startTime/1000, tz = 'MST'),
           lastModified = as_datetime(lastModified/1000, tz = 'MST')) %>%
    select(., contains('id'), contains('link'), contains('description'), 
           contains('startTime'), contains('live'), contains('type'), 
           contains('lastModified'), -competitionId)
  
  events <- unname(split(events, seq(nrow(events))))
  names(events) <- NULL
  
  return(events)
}

get_competitors <- function(bov_json){
  competitors <- bov_json %>%
    .[['competitors']] %>%
    map(., ~as_tibble(.x)) %>%
    map(., ~tryCatch({separate(.x, col = 'id', into = c('id', 'team_id'), sep = '-')},
                     error = function(cond) {
                       return(NULL)
                     }))
  
  return(competitors)
}

get_bets <- function(bov_json, competitors, quietly){
  
  # function to extract bet types
  get_bet_types <- function(y){
    
    # logic
    bet_types <- function(x){
      tmp <- x %>%
        filter(., defaultType) %>%
        select(., contains('markets')) %>%
        unnest(., markets)
      
      res <- tmp %>%
        select(., bet_type = description, outcomes) %>%
        bind_cols(., tmp$period) %>%
        filter(., abbreviation == 'M') %>%
        select(., contains('bet_type'), contains('outcomes'))
      
      names(res$outcomes) <- res$bet_type
      
      return(res$outcomes)
    }
    
    # wrap logic in tryCatch
    out <- tryCatch({bet_types(y)},
                    error = function(cond) {
                      return(NULL)
                    }
    )
    
    return(out)
  }
  
  # get bets in raw form
  bets <- bov_json %>%
    .[['displayGroups']] %>%
    map(., ~get_bet_types(.x))
  
  # clean up bets
  bov_bets <- list()
  for(j in 1:length(competitors)){
    ret <- list()
    if(!is.null(bets[[j]])){
      for(i in 1:length(bets[[j]])){
        if(!quietly) cat(j, ':', i, '\n')
        # check if 
        # anything to parse?
        if(is.null(names(bets[[j]][i])) | is_empty(bets[[j]][[i]])){
          res <- NULL
        }else{
          # parse totals?
          if(names(bets[[j]])[i] == 'Total'){
            res <- bets[[j]][[i]] %>%
              bind_cols(., .$price, .name_repair = 'unique') %>%
              select(., description, handicap, decimal) %>%
              pivot_wider(., id_cols = c('handicap', 'description'), names_from = 'description', values_from = 'decimal') %>%
              select(., handicap_total = handicap, juice_over = Over, juice_under = Under)
            res <- competitors[[j]] %>%
              select(., contains('id'), description = 'name') %>%
              bind_cols(., res)
          }
          # parse moneyline or spread?
          else{
            res <- bets[[j]][[i]] %>%
              bind_cols(., .$price, .name_repair = 'unique') %>%
              select(., -price, -starts_with('id')) %>%
              separate(., col = 'competitorId', into = c('id', 'team_id'), sep = '-') %>%
              select(., contains('id'), contains('description'), contains('handicap'), 
                     contains('decimal'))
          }
          
          # set names
          if(names(bets[[j]][i]) == 'Moneyline'){
            res <- rename(res, juice_money = decimal)
          }else if(names(bets[[j]][i]) == 'Point Spread'){
            res <- rename(res, handicap_spread = handicap, juice_spread = decimal)
          }
        }
        
        # save to return list
        ret[[i]] <- res
      }
    }else{
      ret <- NULL
    }
    
    # combine results of return list
    if(!is_empty(ret)){
      ret_df <- ret %>%
        list.clean(.) %>% 
        reduce(., left_join)
    }else{
      ret_df <- NULL
    }
    
    bov_bets[[j]] <- ret_df
  }
  
  bov_bets <- bov_bets %>%
    bind_rows() %>%
    rename(name = description)
  
  return(bov_bets)
}

get_bovada_lines <- function(bov_json, quietly = TRUE){
  require(tidyverse)
  require(rlist)
  
  bov_json <- bov_json %>%
    .[['events']] %>%
    unlist(., recursive = FALSE)
  
  logic <- function(bov_json = bov_json){
    # get all events on bovada
    events <- get_events(bov_json = bov_json)
    # get all competitors on bovada
    competitors <- get_competitors(bov_json = bov_json)
    # get all bet types
    bets <- get_bets(bov_json = bov_json, competitors = competitors, quietly = quietly)
    
    bov_lines <- bind_rows(events) %>%
      left_join(., bind_rows(competitors)) %>%
      left_join(., bind_rows(bets)) %>%
      filter(., type == 'GAMEEVENT') %>%
      mutate(., name = str_replace(name, " \\(.*\\)", "")) %>%
      mutate_at(., vars(starts_with('juice_')), as.numeric) %>%
      mutate_at(., vars(starts_with('handicap_')), as.numeric)
    
    return(bov_lines)
  }
  
  suppress_text <- function(x) suppressMessages(suppressWarnings(x))
  
  suppress_text(logic(bov_json = bov_json))
}

