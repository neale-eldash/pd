######################################################
############## Likely voter model functions - Alabama

generate_lv_df <- function(df=NULL,party,lv,weights,cuts=seq(0.05,0.95,length.out = 19),keep.all=FALSE){
  
  party <- enquo(party)
  lv <- enquo(lv)
  weights <- enquo(weights)
  
  if (keep.all==TRUE){
    df <- df %>% select(!!party,!!lv,!!weights,everything())
  } else {
    df <- df %>% select(!!party,!!lv,!!weights)
  }
  df <- df %>% arrange(-!!lv)
  df <- df %>% mutate(rank = cumsum(weights) / sum(weights))
  df.cuts <- as.data.frame(map(cuts,~ifelse(df$rank <= .,1,0)))
  names(df.cuts) <- paste0('top.',cuts)
  df <- bind_cols(df,df.cuts)
  
}

generate_lv_graph <- function(df,name="Turnout tipping point analysis (Registered Voters)"){
  
  title <- name
  
  df <- df %>% gather(cut,value,starts_with('top')) %>% filter(value==1)
  df$cut <- as.numeric(str_replace(df$cut,'top.',''))
  df <- df %>% group_by(party,cut) %>% summarise(vote=sum(weights),n=n())
  df <- df %>% group_by(cut) %>% mutate(n=sum(n))
  df <- df %>% group_by(cut) %>% mutate(vote=round(100*vote/sum(vote),1)) %>% arrange(party)
  df <- df %>% mutate(moe=100*sqrt(1/n),low=vote - 100*sqrt(1/n),high=vote + 100*sqrt(1/n))
  
  #adding colour
  cands <- names(xtabs(~df$party))
  myColors <- 1:length(cands)
  myColors[str_which(cands,'(R|r)ep|//((R|r)//)')] <- 'red'
  myColors[str_which(cands,'(D|d)em|//((D|d)//)')] <- 'blue'
  myColors[str_which(cands,'(O|o)ther|(N|n)one')] <- 'gray'
  names(myColors) <- cands
  colScale <- scale_colour_manual(name = 'party',values = myColors)
  filScale <- scale_fill_manual(name = 'party',values = myColors)
  
  gg <- ggplot(data=df) + geom_line(aes(x=cut,y=vote,group=party,color=party),size=1) +
    geom_ribbon(aes(x=cut,ymin=low,ymax=high,group=party,fill=party),alpha=0.1) +
    geom_text(aes(x=cut,y=vote,label=vote,color=party),size=3,vjust=2)+
    scale_x_reverse(lim=c(1,0),breaks=seq(0.1,0.9,0.1)) + labs(title=title,x="Turnout",y="Vote (%)") +
    coord_cartesian(ylim = c(0, 65))
  gg <- gg + colScale + filScale
  
  return(gg)
  
}

generate_lv_demos <- function(df,wgts='weights',reg.expr.dem='_wgts$',reg.expr.lv='^top'){
  
  df$weights <- df[,wgts]
  df <- df %>% select(weights,matches(reg.expr.dem),matches(reg.expr.lv))
  df <- df %>% gather(cut.var,cut.value,matches(reg.expr.lv))
  df <- df %>% filter(cut.value == 1) %>% select(-cut.value)
  df <- df %>% gather(demos,categ,matches(reg.expr.dem))
  df <- df %>% group_by(cut.var,demos,categ) %>% summarise(weights=sum(weights))
  df <- df %>% group_by(cut.var,demos) %>% mutate(weights=round(100*weights/sum(weights),1))
  df <- df %>% spread(cut.var,weights,fill = 0)
  
  return(df)
}

generate_lv_weights <- function(df,id=NA,targets=NA,reg.expr.lv='^top',reg.expr.dem='_wgts$'){
  
  #remove weights variable if alreadt exists
  #páu super irritante que demorou pra descobrir
  df <- df %>% select(-weights)
  
  #getting targets ready
  wgts <- names(xtabs(~targets$var))
  sample <- map(wgts,~as.formula(paste0('~',.)))
  
  population <- map(wgts,function(x){
    df <- ungroup(targets[targets$var == x,])
    df$var <- NULL
    df[,x] <- df$categ
    df$categ <- NULL
    return(df)
  })
  
  #getting data ready
  df$id <- df[,id]
  #df <- df %>% select(id,matches(reg.expr.dem),matches(reg.expr.lv))
  df <- df %>% gather(cut.var,cut.value,matches(reg.expr.lv))
  df <- df %>% filter(cut.value == 1) %>% select(-cut.value)
  df <- df %>% group_by(cut.var) %>% nest()
  
  #weighting the data
  rake_lv <- function(x){
    data.svy <- svydesign(id=~id,data = x);
    data.svy <- raking_svy(data.svy, sample=sample, population=population, control = list(maxit = 800))
    x$wgts <- weights(data.svy)
    x$wgts <- x$wgts * (nrow(x) / sum(x$wgts))
    return(x)
  }
  
  check_rake <- function(x){
    
    check <- x %>% select(wgts,matches(reg.expr.dem))
    check <- check %>% gather(var,categ,-wgts) %>% group_by(var,categ) %>% summarise(sample=n(),raw=n(),weights=sum(wgts))
    check <- full_join(check,targets,by=c('var','categ'))
    check <- check %>% mutate(raw=round(100*raw/sum(raw,na.rm = TRUE),1),weights=round(100*weights/sum(weights,na.rm = TRUE),1),pop=round(100*pop/sum(pop,na.rm = TRUE),1))
    check$diff <- check$weights - check$pop
    check <- check %>% arrange(var,categ)
    check <- as.data.frame(check)
    return(check)
  }
  
  df.wgts <- df %>% mutate(
    data.wgtd = map(data,~rake_lv(.)),
    check = map(data.wgtd,~check_rake(.)),
    sample = map_int(data.wgtd,~nrow(.)),
    max.abs.error = map_dbl(check,~max(abs(.$diff),na.rm = TRUE))
  )
  
  return(df.wgts)
  
}

generate_lv_graph_after_weights <- function(df,name="Turnout tipping point analysis (Weighted LV)"){
  
  title <- name
  df <- df %>% rename(cut=cut.var,weights=wgts)
  df$cut <- as.numeric(str_replace(df$cut,'top.',''))
  df <- df %>% group_by(party,cut) %>% summarise(vote=sum(weights),n=n())
  df <- df %>% group_by(cut) %>% mutate(n=sum(n))
  df <- df %>% group_by(cut) %>% mutate(vote=round(100*vote/sum(vote),1)) %>% arrange(party)
  df <- df %>% mutate(moe=100*sqrt(1/n),low=vote - 100*sqrt(1/n),high=vote + 100*sqrt(1/n))
  
  #adding colour
  cands <- names(xtabs(~df$party))
  myColors <- 1:length(cands)
  myColors[str_which(cands,'(R|r)ep|//((R|r)//)')] <- 'red'
  myColors[str_which(cands,'(D|d)em|//((D|d)//)')] <- 'blue'
  myColors[str_which(cands,'(O|o)ther|(N|n)one')] <- 'gray'
  names(myColors) <- cands
  colScale <- scale_colour_manual(name = 'party',values = myColors)
  filScale <- scale_fill_manual(name = 'party',values = myColors)
  
  gg <- ggplot(data=df) + geom_line(aes(x=cut,y=vote,group=party,color=party),size=1) +
    geom_ribbon(aes(x=cut,ymin=low,ymax=high,group=party,fill=party),alpha=0.1) +
    geom_text(aes(x=cut,y=vote,label=vote,color=party),size=3,vjust=2)+
    scale_x_reverse(lim=c(1,0),breaks=seq(0.1,0.9,0.1)) + labs(title=title,x="Turnout",y="Vote (%)") +
    coord_cartesian(ylim = c(0, 65))
  gg <- gg + colScale + filScale
  
  return(gg)
  
}

