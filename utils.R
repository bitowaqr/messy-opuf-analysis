# utility functions to extract data

# retrievePUFs(raw_json)
# retrieve from raw JSON files :
# - lvl ratings, 
# - dim weights, 
# - anchorings, 
# - 0-1 scaled PUFs
# - anchored PUFs (QALY scale)
# - own eq-5d-5l state
# - own vas
retrievePUFs = function(raw_jsons, strict_cleaning = F, eligible_ids=NULL,excluded_ids=NULL) {
  
  require(eq5d)
  
  ratings_df = weights_df = anchor_df = puf_anchored = puf_anchored_trimmed = c()
  own_state_df = own_vas_df = c()
  groups_df = time_df = c()
  xconsistency_df = c()
  puf_scaled = list()
  feedback = c()
  total_n = falsy_users = pit_or_dead_equal_fh = dim_weight_error = n_pufs = 0
  
  
  for (i in seq_along(raw_jsons)) {
    # cat("\r", round(i / length(raw_jsons), 2) * 100,"%")
    user_id = raw_jsons[[i]]$userId[[1]]
    if(!is.null(eligible_ids)){
      if(!(raw_jsons[[i]]$userId %in% eligible_ids)){
        next
      } else {
      }
    }
    
    dim_weight_error_i = F
    total_n = total_n+1
    ignore_user = F
    
    state_i = unlist(unlist(unlist(raw_jsons[[i]]$ownState$state)))
    own_state_i = paste0(as.numeric(substr(state_i,4,4))+1,collapse = "")
    own_vas_i = raw_jsons[[i]]$ownState$overallHealthLevel
    
    
    ds = raw_jsons[[i]]$ds
    total_weight = sum(unlist(lapply(ds, \(x)x$swingWeight[[1]])))
    weights_df_i = ratings_df_i = c()
    puf_scaled_i = list()
    
    dim_index = 1
    for (dim in ds) {
      dim_weight = dim$swingWeight[[1]] / total_weight
      
      if(is.null(dim$swingWeight[[1]])) {
        ignore_user = T
        dim_weight_error_i = T
      }
      
      weights_df_i = rbind(
        weights_df_i,
        cbind(
          id = user_id,
          dim_id = dim$id[[1]],
          value = ifelse(!is.null(dim$swingWeight[[1]]),dim$swingWeight[[1]],NA)
        )
      )
      dim_levels = dim$levelRating
      
      levels_ids = unlist(lapply(dim_levels, \(level) {
        rating_raw = level$id
      }))
      levels_rating_raw = unlist(lapply(dim_levels, \(level) {
        rating_raw = level$rating[[1]]
      }))
      
      if(sum(is.na(levels_rating_raw))>0) {
        print("level rating error")
        ignore_user = T
      }
      
      ratings_df_i = rbind(
        ratings_df_i,
        cbind(
          id = user_id,
          lvl_id = levels_ids,
          value = levels_rating_raw
        )
      )
      
      levels_rating_anchored = unlist(lapply(dim_levels, \(level) {
        rating_anchored = (dim_weight * (100 - level$rating[[1]])) / 100
        
        return(rating_anchored)
      }))
      puf_scaled_i[[dim_index]]  = levels_rating_anchored
      dim_index = dim_index + 1
    }
    
    if(ncol(ratings_df_i)==2){
      ignore_user = T
      rating_error = rating_error+1
    }
    
    if(dim_weight_error_i){
      dim_weight_error = dim_weight_error_i+1
    }
    
    if(raw_jsons[[i]]$AnchorVas$vasValueOfPreferred=="100.00"){
      pits_value = NA
      ignore_user = T
      pit_or_dead_equal_fh = pit_or_dead_equal_fh+1
    } else {
    pits_value = 1/(1-raw_jsons[[i]]$AnchorVas$pitStateUtility)
      
    
    if(length(pits_value) ==0){
      pits_value = NA
      ignore_user = T
      pit_or_dead_equal_fh = pit_or_dead_equal_fh+1
    }
    if(pits_value== Inf){
      pits_value = NA
      ignore_user = T
      pit_or_dead_equal_fh = pit_or_dead_equal_fh+1
    }
    }
    
    
    
    anchor_i = 1 / pits_value
    puf_anchored_i = lapply(puf_scaled_i, \(dim) {
      dim / pits_value
    })
    puf_anchored_trimmed_i = lapply(puf_scaled_i, \(dim) {
      pits_value = ifelse(1/pits_value > 2, 1/2, pits_value) # pits <= -1 
      dim / pits_value
    })
    
    puf_anchored_i_as_row = unlist(puf_anchored_i)
    puf_anchored_trimmed_i_as_row = unlist(puf_anchored_trimmed_i)
    puf_scaled_i_mat = do.call(cbind, puf_scaled_i)
    
    secs_taken_i = sum(unlist(lapply(raw_jsons[[i]]$taskTimeTracked, \(x) x$time[[1]])))
    
    groups_i = list()
    for(q_index in seq_along(raw_jsons[[i]]$otherQuestions[[1]])){
      q = raw_jsons[[i]]$otherQuestions[[1]][q_index]
      group = names(q)
      if(class(q)=="list"){
        for(resp in q){
          value = resp
          if(length(value)>1){
            value = paste0(unlist(value), collapse = ", ")
          }
          if(is.null(value)){
            value = NA
          }
          groups_i[[length(groups_i)+1]] = c(id = user_id, group = group, value = value)
        }
      } else {
        value = q
        if(length(value)>1){
          value = paste0(unlist(value), collapse = ", ")
        }
        if(is.null(value)){
          value = NA
        }
        groups_i[[length(groups_i)+1]] = c(id = user_id, group = group, value = value)
      }
    }
    
    groups_i = do.call(rbind, groups_i)
    
    groups_j = lapply(raw_jsons[[i]]$screener,\(q){
      group = q$id
      value = q$response[[1]]
      if(length(value)>1){
        value = paste0(unlist(value), collapse = ", ")
      }
      if(is.null(value)){
        value = NA
      }
      return(c(id = user_id, group = group, value = value))
    })
    groups_j = do.call(rbind, groups_j)
    
    
    # append cross user mats
    # (suboptimal code implementation, but working)
    if(ignore_user){
      falsy_users = falsy_users+1
      excluded_ids = c(excluded_ids, user_id)
    } else {
      ratings_df = rbind(ratings_df, ratings_df_i)
      weights_df = rbind(weights_df, weights_df_i)
      anchor_df = rbind(anchor_df, cbind(id  = user_id, anchor = anchor_i))
      time_df = rbind(time_df, cbind(id = user_id, seconds = secs_taken_i))
      groups_df = rbind(groups_df, groups_i, groups_j)
      own_state_df = rbind(own_state_df,cbind(id = user_id, state = own_state_i))
      own_vas_df = rbind(own_vas_df, cbind(id = user_id, vas = own_vas_i))
      puf_scaled[[i]] = puf_scaled_i
      n_pufs = n_pufs+1
      puf_anchored = rbind(puf_anchored, puf_anchored_i_as_row)
      rownames(puf_anchored)[length(rownames(puf_anchored))] = user_id 
      puf_anchored_trimmed = rbind(puf_anchored_trimmed, puf_anchored_trimmed_i_as_row)
      rownames(puf_anchored_trimmed)[length(rownames(puf_anchored_trimmed))] = user_id 
    }
  }
  
  colToNum = function(mat, cols = c("value")){
    df = data.frame(mat)
    for(col in cols){
      df[,col] = as.numeric(df[,col])
    }
    return(df)
  }
  
  ratings_df = colToNum(ratings_df)
  weights_df = colToNum(weights_df)
  anchor_df = colToNum(anchor_df,"anchor")
  time_df = colToNum(time_df, "seconds")
  own_vas_df = colToNum(own_vas_df, "vas")
  own_state_df = data.frame(own_state_df)
  groups_df = data.frame(groups_df)
  
  return(
    list(
      ratings_df = ratings_df,
      weights_df = weights_df,
      anchor_df = anchor_df,
      pufs = puf_anchored,
      pufs_trimmed = puf_anchored_trimmed,
      groups_df = groups_df,
      time_df = time_df,
      feedback = feedback,
      own_state_df=own_state_df,
      own_vas_df=own_vas_df,
      puf_scaled = puf_scaled,
      falsy_users = falsy_users,
      pit_or_dead_equal_fh = pit_or_dead_equal_fh,
      dim_weight_error = dim_weight_error,
      n_pufs = n_pufs,
      total_n = total_n
    )
  )
}