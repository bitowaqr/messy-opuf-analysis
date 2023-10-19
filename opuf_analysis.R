rm(list=ls())

# load libraries
library(jsonlite)
library(ggplot2)
library(reshape2)

# load utility functions
source("./utils.R")

ds_dims = c("MO","UA","EX","LO","CT","AX","SD","CO","PA")

# load raw json files
# ----> @Aisha, you obviously need to chang the fiel path. 
# If this is tricky, you can use the function below to get the right path
# file.choose()
# you could even do read_json(file.choose()) 
# I don't recommend it though
ra_jsons = read_json("responses_raw.json")

ra = retrievePUFs(raw_jsons = ra_jsons,strict_cleaning = F, excluded_ids = NULL)

# descriptive stats
# total completions
ra$total_n
# failures
ra$dim_weight_error  # because dim weight collection error
ra$pit_or_dead_equal_fh # because pit or dead set equal to full health
# included
ra$n_pufs

# Demographics
dem_vars = c("age","sex","education1","education2","experienceIllness","illnessDiagnosed","caringChildren","caringHealth")
sapply(dem_vars, \(group){
  t_ = table(ra$groups_df$value[ra$groups_df$group == group])
  tn_ = names(t_)
  res = paste(t_, " (", round(t_/ra$n_pufs,3)*100,"%)", sep ="")
  names(res) = tn_
  return(res)
})

dems = c()
for(var in dem_vars){
  t_ = table(ra$groups_df$value[ra$groups_df$group == var])
  if(length(t_)==0){
    return(NULL)
  }
  tn_ = names(t_)
  res = paste(t_, " (", round(t_/ra$n_pufs,3)*100,"%)", sep ="")
  dems = rbind(dems,cbind(category = var, group = tn_, value = res))
}
# write.csv(dems, file = "./dems.csv", row.names = F)

# time taken
sum((ra$time_df$seconds / 60) < 5)
ra$time_df$seconds[ra$time_df$seconds < (60*5)]/60
min(ra$time_df$seconds)/60
mean(ra$time_df$seconds)/60
sd(ra$time_df$seconds)/60
summary(round(ra$time_df$seconds/60,1))
quantile(round(ra$time_df$seconds/60),0.5)
quantile(round(ra$time_df$seconds/60),c(0.1,0.9))
hist(ra$time_df$seconds/60, breaks = 120, xlim = c(0,60), xlab = "Mins")


ggplot() +
  geom_histogram(aes(ra$time_df$seconds/60), bins = 70, col="gray", fill = "cadetblue") +
  xlim(c(0,50)) +
  xlab("Survey duration (in mins)") +
  theme_minimal()

# Own health state
# full health
sum(ra$own_state_df$state == "111111111")
sum(ra$own_state_df$state == "111111111")/ra$n_pufs
only_minor = grepl("2",ra$own_state_df$state) &!grepl("3",ra$own_state_df$state) & !grepl("4",ra$own_state_df$state) & !grepl("5",ra$own_state_df$state)
sum(only_minor)
sum(only_minor)/ra$n_pufs
# severe or extreme
severe_extreme = grepl("4",ra$own_state_df$state) | grepl("5",ra$own_state_df$state)
sum(severe_extreme)
sum(severe_extreme)/ra$n_pufs

own_states_mat = do.call(rbind,strsplit(ra$own_state_df$state,split = ""))
apply(own_states_mat,2,\(x) sum(as.numeric(x)>1))
apply(own_states_mat,2,\(x) mean(as.numeric(x)))
apply(own_states_mat,2,\(x) sd(as.numeric(x)))
apply(own_states_mat,2,\(x) median(as.numeric(x)))

# misery index
miseryScores = unlist(lapply(ra$own_state_df$state, \(x){
  y =as.numeric(unlist(strsplit(x,"")))
  sum(as.numeric(unlist(y)))
}))
summary(miseryScores)


# Own VAS
mean(ra$own_vas_df$vas);sd(ra$own_vas_df$vas);
quantile(ra$own_vas_df$vas, c(0,0.25,0.5,0.75,1))

ggplot() +
  geom_histogram(aes(ra$own_vas_df$vas), col = "lightgray", fill = "cadetblue", binwidth = 5) +
  theme_minimal() +
  xlab("EQ-VAS")


# OPUF RESPONSES
# Dim weights
dimWeights = by(ra$weights_df$value, ra$weights_df$dim_id, \(dim){
  c(
    paste(round(mean(dim),1), " (",round(sd(dim),1),")",sep = ""),
    paste(round(median(dim),1), " (",paste0(round(quantile(dim,c(0.25,0.75))),collapse = "; "),")",sep = "")
  )
})

dimLabs = levels(as.factor(ra$weights_df$dim_id))
dimWeightsTbl = c()
for(i in seq_along(dimWeights)){
  dimWeightsTbl = rbind(dimWeightsTbl, c(dimLabs[i], dimWeights[[i]]))
}
colnames(dimWeightsTbl) = c("Dimension", "Mean (SD)", "Median (Q25-Q75)")
write.csv(dimWeightsTbl, "dimWeightsTbl.csv",row.names = F)

ggplot(ra$weights_df) +
  geom_histogram(aes(value, fill = dim_id), col = "lightgray", binwidth = 10) +
  facet_wrap(~dim_id, ncol = 2) +
  theme_minimal() +
  theme(legend.position="none")

# level ratings
lapply(ds_dims,\(dim){
  s_ = grepl(dim,ra$ratings_df$lvl_id) & !grepl("_4",ra$ratings_df$lvl_id) & !grepl("_0",ra$ratings_df$lvl_id)
  df_ = ra$ratings_df[s_,]
  by(df_$value,df_$lvl_id,\(lvl){
    c(
      paste(round(mean(lvl),1), " (",round(sd(lvl),1),")",sep = ""),
      paste(round(median(lvl),1), " (",paste0(round(quantile(lvl,c(0.25,0.75))),collapse = "; "),")",sep = "")
    )
  })
})

## number of people who assigned 100 levels
lvl100 = lapply(ds_dims,\(dim){
  s_ = grepl(dim,ra$ratings_df$lvl_id) & !grepl("_3",ra$ratings_df$lvl_id) & !grepl("_0",ra$ratings_df$lvl_id)
  df_ = ra$ratings_df[s_,]
  by(df_$value,df_$lvl_id,\(lvl){
    lvl == 100
  })
})

# sum(unlist(lvl100))

# level ratings
lvl_ratings = lapply(ds_dims,\(dim){
  s_ = grepl(dim,ra$ratings_df$lvl_id) & !grepl("_4",ra$ratings_df$lvl_id) & !grepl("_0",ra$ratings_df$lvl_id)
  df_ = ra$ratings_df[s_,]
  by(df_$value,df_$lvl_id,\(lvl){
    c(paste0(round(mean(lvl),1), " (",round(sd(lvl),1),")",collapse = ""))
  })
})
lvl_ratings = rbind(
  paste0(rep(100,9)," (0)"),
  do.call(cbind,lvl_ratings),
  paste0(rep(0,9)," (0)")
)
colnames(lvl_ratings) = ds_dims
rownames(lvl_ratings) = paste0("lvl_",1:5)
lvl_ratings = t(lvl_ratings)
write.csv(lvl_ratings, "./lvl_ratings.csv")

# error check
ra$ratings_df$dim = gsub("_.*","",ra$ratings_df$lvl_id)
ra$ratings_df$lvl = 1+as.numeric(gsub(".*_","",ra$ratings_df$lvl_id))

# count the number of times per id that a rating for a lower level is higher than a rating for a higher level
uniq_ids = unique(ra$ratings_df$id)
err = c()
for (id in uniq_ids){
  s_ = ra$ratings_df$id == id
  df_ = ra$ratings_df[s_,]
  id_err = 0
  for (dim in ds_dims){
    dim_err = F
    s_ = df_$dim == dim
    df_dim = df_[s_,]
    for (lvl in 1:4){
      s_ = df_dim$lvl == lvl
      s2_ = df_dim$lvl > lvl
      df_lvl = df_dim[s_,]
      df_lvl2 = df_dim[s2_,]
      if(sum(df_lvl$value < df_lvl2$value) > 0){
        dim_err = T
      }
    }
     if(dim_err){
       id_err = id_err + 1
     }
  }
  err = c(err,id_err)
}

errTbl = table(err)
errTblPerc = round(errTbl/ra$n_pufs,2)
lvlErrorTbl = cbind(unlist(lapply(1:length(errTbl),\(i){
  paste0(errTbl[i]," (",errTblPerc[i]*100,"%)")
})),
round(cumsum(round(table(err)/ra$n_pufs,3))*100)
)
write.csv(lvlErrorTbl, "./lvlErrorTbl.csv")
# 


intermediateLevels = ra$ratings_df[ra$ratings_df$lvl %in% 2:4,]
sum(intermediateLevels$value==100)/length(intermediateLevels$value)


# anchor
# n (%) BTD/WTD
sum((1-ra$anchor_df$anchor)==0); sum((1-ra$anchor_df$anchor)==0)/ra$n_pufs
sum((1-ra$anchor_df$anchor)>0); sum((1-ra$anchor_df$anchor)>0)/ra$n_pufs
sum((1-ra$anchor_df$anchor)<0); sum((1-ra$anchor_df$anchor)<0)/ra$n_pufs
sum((1-ra$anchor_df$anchor)< -1); sum((1-ra$anchor_df$anchor)< -1)/ra$n_pufs


sum((1-ra$anchor_df$anchor)> 0.9); sum((1-ra$anchor_df$anchor) > 0.9)/ra$n_pufs

1-mean(ra$anchor_df$anchor);sd(ra$anchor_df$anchor);
1-quantile(ra$anchor_df$anchor, c(0,0.25,0.5,0.75,1))


hist(as.numeric(unlist(lapply(ra_jsons, \(x) x$AnchorVas$vasValueOfPreferred))), breaks = 15)


  


ra_trimmed = ra$anchor_df$anchor
sum(ra_trimmed> 2); sum(ra_trimmed> 2)/ra$n_pufs
ra_trimmed[ra_trimmed> 2] = 2
1-mean(ra_trimmed);sd(ra_trimmed);

ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_histogram(aes(1-ra_trimmed), col = "black", fill = "lightblue", alpha = 0.9, binwidth = 0.1) +
  theme_minimal() +
  xlab("Utility of 555555555 (censoring at -1)")




# PUF stats
puf_stats = formatC(apply(ra$pufs_trimmed,2,quantile,probs = c(0.5,0.25,.75)),digits = 3, format = "f")
puf_stats = t(puf_stats)
puf_stats = matrix(paste0(puf_stats[,1]," (",puf_stats[,2],"; ",puf_stats[,3],")"),ncol = 9)[-1,]


# Social preference model -----
ra_mean_est = formatC(apply(ra$pufs_trimmed,2,mean),digits = 3, format = "f")
ra_mean_boot = replicate(10000,apply(ra$pufs_trimmed[sample(1:nrow(ra$pufs_trimmed),size = nrow(ra$pufs),replace = T),],2,mean))
ra_mean_boot = formatC(apply(ra_mean_boot,1,quantile,probs = c(0.025,0.975)),digits = 3, format = "f")
ra_m_95 = sapply(1:45, \(i) paste(ra_mean_est[i]," (",ra_mean_boot[1,i],"; ",ra_mean_boot[2,i],")",sep = ""))
ra_m_95 = matrix(ra_m_95, ncol = 9)[-1,]
colnames(ra_m_95) = ds_dims
rownames(ra_m_95) = paste("lvl_",2:5)
ra_m_95 = t(ra_m_95)
write.csv(ra_m_95, file = "./group-puf.csv", row.names = T)


# social value set
social_model = matrix(apply(ra$pufs_trimmed,2,mean), ncol = 9)
ds_expanded = expand.grid(1:5, 1:5, 1:5, 1:5, 1:5,1:5,1:5,1:5,1:5)
ds_str = apply(ds_expanded,1,\(x) paste0(x,collapse=""))
ra_vs = c()
# for(i in 1:nrow(ds_expanded)){
#   v_ = 1
#   for(j in seq_along(ds_expanded[i,])){
#     v_ = v_ - social_model[ds_expanded[i,j],j]
#   }
#   ra_vs = c(ra_vs, v_)
# }
# 
# ra_hs_ranking = order(-ra_vs)
# 
# ggplot() +
#   geom_hline(yintercept = 0, col = "gray", size = 0.5) +
#   geom_line(aes(x = 1:3125, y = ra_vs[ra_hs_ranking])) +
#   ylab("HRQoL / Utility") +
#   xlab("EQ-5D-5L Health states - ranked from best to worst") +
#   theme_minimal()

# Example utilitirs
examples = rbind(
  c(2,1,1,1,1,1,1,1,1),
  c(1,2,1,1,1,1,1,1,1),
  
    c(2,2,2,2,2,2,2,2,2),
      c(3,3,3,3,3,3,3,3,3),
        c(4,4,4,4,4,4,4,4,4),
          c(5,5,5,5,5,5,5,5,5)
  )
ds_str = apply(examples,1,\(x) paste0(x,collapse=""))
ra_vs = c()
for(i in 1:nrow(examples)){
  v_ = 1
  for(j in seq_along(examples[i,])){
    v_ = v_ - social_model[examples[i,j],j]
  }
  ra_vs = c(ra_vs, v_)
}

ra_hs_ranking = order(-ra_vs)

exampleTbl = cbind(
  apply(examples,1,paste0, collapse=""), round(ra_vs,3))
colnames(exampleTbl)= c("Health state","utility")
write.csv(exampleTbl, "exampleTbl.csv", row.names = F)


# feedback ------


dem_vars = c("easyToUnderstand"  ,"easyToAnswer"     , "duration" )

feedback1 = c()
for(var in dem_vars){
  t_ = table(ra$groups_df$value[ra$groups_df$group == var])
  if(length(t_)==0){
    return(NULL)
  }
  tn_ = names(t_)
  res = paste(t_, " (", round(t_/ra$n_pufs,3)*100,"%)", sep ="")
  feedback1 = rbind(feedback1,cbind(category = var, group = tn_, value = res))
}
write.csv(feedback1, file = "./feedback1.csv", row.names = F)


feedback_generalImpression = ra$groups_df$value[ra$groups_df$group=="generalImpression"]
feedback_generalImpression = feedback_generalImpression[!is.na(feedback_generalImpression)]
feedback_generalImpression = feedback_generalImpression[feedback_generalImpression != ""]
feedback_generalImpression = feedback_generalImpression[feedback_generalImpression != " "]
feedback_generalImpression = feedback_generalImpression[feedback_generalImpression != "preferNotToSay"]
feedback_generalImpression

write.csv(feedback_generalImpression, "./feedback_generalImpression.csv")

# install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(dplyr)


docs <- unlist(strsplit(feedback_generalImpression, " "))
docs <- tolower(docs)
docs <- removeWords(docs,stopwords("english"))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


set.seed(12345) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          scale = c(1.5,.5),
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2")
          )


### general other feedback

feedback = ra$groups_df$value[ra$groups_df$group=="feedback"]
feedback = feedback[!is.na(feedback)]
feedback = feedback[feedback != ""]
feedback = feedback[feedback != " "]
feedback = feedback[feedback != "preferNotToSay"]
feedback

write.csv(feedback, "./feedback.csv")


# docs <- unlist(strsplit(feedback, " "))
# docs <- tolower(docs)
# docs <- removeWords(docs,stopwords("english"))
# dtm <- TermDocumentMatrix(docs) 
# matrix <- as.matrix(dtm) 
# words <- sort(rowSums(matrix),decreasing=TRUE) 
# df <- data.frame(word = names(words),freq=words)
# 
# set.seed(1234) # for reproducibility 
# wordcloud(words = df$word, freq = df$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35,
#           colors=brewer.pal(8, "Dark2")
# )
