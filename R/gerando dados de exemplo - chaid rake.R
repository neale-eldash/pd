# require(pd)
# require(tidyverse)
# library(rpart)
# library(rpart.plot)
#
# #########################################################################
# #########################################################################
#
# ######## WHAT IF I RUN THE TREE ON THE DATA WEIGHTED WITH TRADIONAL RAKING
# ######## THENIT SHOULD IMPROVE THE RESULTS
#
# ### e se usar regression trees ou random forest ao inves de chaid
# ### https://www.r-bloggers.com/how-random-forests-improve-simple-regression-trees/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29
#
# #########################################################################
# #########################################################################
#
#
# dir <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Ipsos USA\\Reuters\\Reuters 2017\\AAPOR 2017 - MRP for Reuters 2016 data"
# file.data <- paste0(dir,"\\Reuters Unified Poll - WEEK 201642 thru 201644.sav")
# file.turn <- paste0(dir,"\\2016 pres turnout.xlsx")
# dir.beta <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Ipsos USA\\Reuters\\Reuters 2016\\Presidential Election 2016\\Likely Voter model for Boost Sample"
# file.beta <- paste0(dir.beta,"\\Betas for Scoring LV model.sav")
# dir.cps <- "E:\\DADOS\\Bancos de Dados\\US Census\\Current Population Survey\\2016"
# file.cps <- paste0(dir.cps,"\\CPS 2016 June.sav")
# dir.gen2016 <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Polling Data\\PollingData2016\\US 2016"
# file.gen2016=paste0(dir.gen2016,"\\US2016 Results Gen.RData")
#
# ##########################################
# ##########################################
# ###### functions
# ##########################################
# ##########################################
#
#  get_common_vars <- function(df.x,df.y){
#
#    vars <- names(df.x)[names(df.x) %in% names(df.y)]
#    return(vars)
#
#  }
#
#  create_cells_df <- function(df,vars,weight=NA){
#
#    if (is.na(weight) == TRUE){
#      df$wgts <- 1
#    } else {
#      df <- df %>% dplyr::select(dplyr::one_of(vars),dplyr::one_of(weight))
#      df$wgts <- df[,weight]
#      df[,weight] <- NULL
#    }
#
#    df <- df %>% dplyr::group_by_(.dots=vars) %>% dplyr::summarise(wgts=sum(wgts),n=n())
#    return(df)
#
#  }
#
#  replicate_tree <- function (object, newdata,na.action = na.pass){
#
#    df.orig <- newdata
#    if (is.null(attr(newdata, "terms"))) {
#      Terms <- delete.response(object$terms)
#      newdata <- model.frame(Terms, newdata, na.action = na.action,
#                             xlev = attr(object, "xlevels"))
#      if (!is.null(cl <- attr(Terms, "dataClasses")))
#        .checkMFClasses(cl, newdata, TRUE)
#    }
#    df.orig$grp <- rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
#    df.orig
#  }
#
#  extract_names <- function(df.pop,vars){
#
#    df.names <- df.pop %>% dplyr::select(dplyr::one_of(vars),grp,strata)
#    df.names <- df.names %>% tidyr::gather(var,categ,-grp,-strata)
#    df.names <- df.names %>% dplyr::group_by(strata,grp,var,categ) %>% dplyr::summarise(n=n()) %>% dplyr::select(-n)
#    df.names <- df.names[complete.cases(df.names),]
#    df.names <- df.names %>% dplyr::group_by(strata,grp,var) %>% do(categs=paste(.$categ,collapse=";"))
#    df.names <- df.names %>% tidyr::spread(var,categs)
#    df.names <- df.names %>% dplyr::arrange(strata,grp)
#
#    return(df.names)
#  }
#
#  create_tree <- function(cells.svy,indep=NULL,dep=NULL,minbucket = 30,cp = 0.001){
#
#    #options that can be used with rpart
#    #to labels and visualize groups, for example
#
#    # bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
#    # tree.pruned <- prune(fit, cp = bestcp)
#    # prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)
#    # plot(tree.pruned); text(tree.pruned)
#    # df.tree <- tree.pruned$frame
#    # df.grp <- as.data.frame(tree.pruned$where)
#    # df.split <- as.data.frame(tree.pruned$splits)
#    # df.csplit <- as.data.frame(tree.pruned$csplit)
#    # ttt <- rpart.lists(tree.pruned)
#    # df.ttt <- rpart.rules(tree.pruned)
#    # df.ttt1 <- rpart.rules.table(tree.pruned)
#    # df.ttt2 <- rpart.subrules.table(tree.pruned)
#
#    if(any(is.null(vars),is.null(dep))){
#      stop("Dependent and Independent variables must be specified!")
#    }
#
#    #tree
#    frmla <- as.formula(paste(dep,paste(indep,collapse='+'),sep='~'))
#    tree <- rpart(frmla,method="anova", data=cells.svy,weights=wgts,control=rpart.control(minbucket = minbucket,cp = cp))
#
#    #prune tree
#    # graf <- prp(tree, faclen = 0, cex = 0.8, extra = 1)
#    # bestcp <- tree$cptable[which.min(fit.cells$cptable[,"xerror"]),"CP"]
#    # fit.cells <- prune(tree, cp = bestcp)
#    cells.svy$grp <- tree$where
#
#    saida <- list(cells.svy=cells.svy,tree=tree)
#    return(saida)
#
#  }
#
#  create_tree_grps <- function(df.pop,df.svy,strata=NULL,dep=NULL,wgt.pop=NULL,minbucket = 30,cp = 0.001){
#
#    vars <- get_common_vars(df.svy,df.pop)
#    if(length(vars) == 0){
#      stop("There are no common variable in the survey and population datasets!")
#    }
#
#    #test if strata is in both data sets and create formula
#    if (is.null(strata) == FALSE){
#      if (!(strata %in% vars)){
#        stop("Variable 'strata' has to be in both dataframes!")
#      }
#      vars <- setdiff(vars,strata)
#      df.pop$strata <- df.pop[,strata]
#      df.svy$strata <- df.svy[,strata]
#    } else {
#      df.pop$strata <- 1
#      df.svy$strata <- 1
#    }
#    indep.vars <- vars
#    vars <- c(vars,'strata')
#
#    #datasets - cells
#    cells.pop <- create_cells_df(df=df.pop,vars=vars,weight=wgt.pop)
#    cells.svy <- create_cells_df(df=df.svy,vars=c(vars,dep))
#
#    #generate tree
#    fit.cells <- cells.svy %>% dplyr::group_by(strata) %>% do(fit=create_tree(.,indep=indep.vars,dep=dep,minbucket = minbucket,cp = cp))
#    cells.svy <- purrr::map_df(fit.cells$fit,'cells.svy')
#
#    cells.pop.fit <- purrr::map2_df(fit.cells$strata,fit.cells$fit,
#                             function(x,y){
#                               df <- cells.pop %>% dplyr::filter(strata == x);
#                               df <- replicate_tree(y$tree,df)
#                             })
#    cells.pop <- dplyr::left_join(cells.pop,cells.pop.fit)
#    df.svy <- dplyr::left_join(df.svy,select(cells.svy,-wgts,-n),by=c(vars,dep))
#
#    #generate names
#    df.names <- extract_names(cells.svy,indep.vars)
#
#    saida <- list(df.svy=df.svy,cells.pop=cells.pop,grps=df.names,trees=fit.cells)
#  }
#
#  chaid_raking <- function(df.pop,df.svy,strata=NULL,id.var=NULL,dep=NULL,wgt.pop=NULL,minbucket = 30,cp = 0.001){
#
#    if(is.null(dep)){
#      stop("Dependent variable must be specified!")
#    }
#    if(is.null(wgt.pop)){
#      stop("Weight variable for population dataset must be specified!")
#    }
#    if(is.null(id.var)){
#      stop("The id variable must be specified!")
#    }
#
#    ### formating datasets
#    df.pop <- as.data.frame(df.pop)
#    df.svy <- as.data.frame(df.svy)
#    id.var <- as.formula(paste0('~',id.var))
#
#    #tree <- create_tree_grps(pnad,base,dep='Q4_1_dummy',wgt.pop='pop')
#    tree <- create_tree_grps(df.pop,df.svy,dep=dep,wgt.pop=wgt.pop,strata=strata,minbucket = minbucket,cp = cp)
#
#    #############################
#    #tree output
#
#    df.svy <- tree$df.svy
#    cells.pop <- tree$cells.pop
#
#    #############################
#    #targets
#
#    targets.pop <- cells.pop %>% ungroup() %>% dplyr::select(strata,grp,wgts) %>% dplyr::filter(is.na(grp)==FALSE)
#    targets.pop <- targets.pop %>% dplyr::group_by(strata,grp) %>% dplyr::summarise(wgts=sum(wgts))
#    targets.pop <- targets.pop %>% ungroup() %>% dplyr::mutate(wgts=round(100*wgts/sum(wgts),1))
#    targets.pop <- as.data.frame(targets.pop)
#    pop.tree <- xtabs(wgts~strata+grp,data=cells.pop)
#
#    targets.svy <- df.svy %>% ungroup() %>% dplyr::select(strata,grp) %>% dplyr::filter(is.na(grp)==FALSE)
#    targets.svy <- targets.svy %>% dplyr::group_by(strata,grp) %>% dplyr::summarise(n=n())
#    missing.grps <- anti_join(targets.svy,targets.pop,by=c('strata','grp'))
#    missing.grps <- missing.grps %>% dplyr::select(-n) %>% dplyr::mutate(tira='missing grp')
#
#    #############################
#    #giving weight 1 to grps missing in the population
#    df.svy <- dplyr::left_join(df.svy,missing.grps,by=c('strata','grp'))
#
#    #############################
#    #raking
#
#    data.svy <- survey::svydesign(id=id.var,data = df.svy[is.na(df.svy$tira) == TRUE,]);
#    data.svy <- raking_svy(data.svy, sample=list(~strata+grp), population=list(pop.tree), control = list(maxit = 800))
#    df.svy$weights <- 1
#    df.svy$weights[is.na(df.svy$tira) == TRUE] <- weights(data.svy)
#    df.svy$weights <- df.svy$weights * (dim(df.svy)[1] / sum(df.svy$weights))
#
#    #############################
#    #checks
#
#    check <- df.svy[,c('weights','strata','grp')]
#    check <- check %>% dplyr::group_by(strata,grp) %>% dplyr::summarise(svy.sample=n(),svy.raw=n(),svy.wgts=sum(weights))
#    check <- dplyr::full_join(check,dplyr::rename(targets.pop,pop.wgts=wgts),by=c('strata','grp'))
#    check <- check %>% ungroup() %>% dplyr::mutate(svy.raw=round(100*svy.raw/sum(svy.raw,na.rm = TRUE),1),svy.wgts=round(100*svy.wgts/sum(svy.wgts,na.rm = TRUE),1))
#    check$diff <- check$svy.wgts - check$pop.wgts
#    check <- as.data.frame(check)
#
#    saida <- list(df.svy=df.svy,cells.pop=cells.pop,check=check,trees=tree$trees,grps=tree$grps)
#    return(saida)
#
#  }
#
#
#  ##########################################
#  ##########################################
#  ###### REUTERS DATA - LAST 3 WEEKS - 201642 THRU 201644
#  ##########################################
#  ##########################################
#
#  data.spss <- read_sav(file.data)
#
#  #sample source: TEMP_EVENT
#  #1 – Omni
#  #2 – Cortex
#  #3 – State Poll
#  #NA – Tracker
#
#  vars <- c("RESPID","ID_REUTERS","INTEND","YEAR_WEEK","PD4_NEW_1","NEW_LVscore","PARTY_ID_","STATE","AGE_GRP","EDU","RACE_","INCOME2","GEODIV9","SEX","SVQ6","PD1","realloc2","temp_event","TM651Y15_SON_6","QMktSize_13_1","EarlyVote2016")
#  reuters <- data.spss[,vars]
#  reuters <- as_factor(reuters,only_labelled = TRUE,ordered=TRUE)
#
#  #dplyr::filter
#  reuters <- reuters %>% dplyr::filter(YEAR_WEEK %in% c("201642", "201643", "201644"))
#
#  reuters$date <- as.Date(reuters$INTEND)
#  reuters$MONTH <- format(reuters$date,"%m%Y")
#  reuters$source <- ifelse(is.na(reuters$temp_event)==TRUE,0,reuters$temp_event)
#  reuters$source <- factor(reuters$source,levels=0:2,labels=c("Tracker","Omni","Cortex"))
#  reuters$metro <- ifelse(as.numeric(reuters$QMktSize_13_1) <= 3,1,2)
#  reuters$metro <- factor(reuters$metro,levels=1:2,labels=c('Metro','NonMetro'))
#  reuters$metro2 <- factor(as.numeric(reuters$QMktSize_13_1),levels=1:4,labels=c('Less 1M','1M to 5M','5M+','NonMetro'))
#  reuters$QMktSize_13_1 <- NULL
#
#  #make sure that unemployment is compatible between CPS and Reuters
#  reuters$employed <- ifelse(as.numeric(reuters$SVQ6)<=4,1,3)
#  reuters$employed <- ifelse(as.numeric(reuters$SVQ6) == 7,2,reuters$employed)
#  reuters$employed <- factor(reuters$employed,levels=1:3,labels=c('Employed','Unemployed','Not in labour force'))
#
#  ##################################
#  #### Vote
#
#  reuters$vote <- reuters$TM651Y15_SON_6
#  reuters$lead <- 0
#  reuters$lead <- ifelse(as.numeric(reuters$vote) == 1,-1,reuters$lead)
#  reuters$lead <- ifelse(as.numeric(reuters$vote) == 2,1,reuters$lead)
#  reuters$lead <- ifelse(as.numeric(reuters$vote) == 3,0,reuters$lead)
#
#  ##########################################
#  ##########################################
#  ###### LV MODEL
#  ##########################################
#  ##########################################
#
#  betas <- read_sav(file.beta)
#  betas <- as_factor(betas,only_labelled = TRUE,ordered=TRUE)
#
#  reuters <- dplyr::left_join(reuters,betas,by=c('STATE','EDU','SEX','AGE_GRP','RACE_','INCOME2','PARTY_ID_'))
#  reuters$votoPD4 <- as.numeric(ifelse(as.numeric(reuters$PD4_NEW_1) > 10,0,reuters$PD4_NEW_1))
#  reuters$lv <- reuters$intercept + reuters$votoPD4 * reuters$betaPD4
#  reuters$lv <- exp(reuters$lv) / (1 + exp(reuters$lv))
#
#  #early voter - probability 1 for the LV model
#  reuters$lv.force <- ifelse(as.numeric(reuters$EarlyVote2016) %in% 5:7,1,0)
#  reuters$lv <- ifelse(as.numeric(reuters$EarlyVote2016) %in% 5:7,1,reuters$lv)
#
#  ##########################################
#  ##########################################
#  ###### CPS 2016 June data
#  ##########################################
#  ##########################################
#
#  ### loading CPS data
#
#  cps <- read_sav(file.cps)
#  cps <- as_factor(cps,only_labelled = TRUE,ordered=TRUE)
#
#  vars.cps <- c("HRHHID", "PWSSWGT","GESTCEN", "age_wgts", "edu_wgts", "race_wgts", "income_wgts","sex_wgts","employment_wgts","div9","GTMETSTA","GTCBSASZ")
#  cps <- cps[,vars.cps]
#  cps$STATE <- as.character(cps$GESTCEN)
#  cps$GESTCEN <- NULL
#  cps <- cps %>% dplyr::rename(employed=employment_wgts,AGE_GRP=age_wgts,EDU=edu_wgts,RACE_=race_wgts,INCOME2=income_wgts,SEX=sex_wgts,GEODIV9=div9)
#  #18+ dplyr::filter
#  cps <- cps[cps$AGE_GRP != "NaN",]
#  cps <- cps[cps$employed != "NaN",]
#  cps <- droplevels(cps)
#
#  #recodes / labels to match reuters data
#  aux.age <- names(xtabs(~reuters$AGE_GRP,drop.unused.levels = TRUE))
#  cps$AGE_GRP <- factor(as.numeric(cps$AGE_GRP),levels=1:5,labels=aux.age)
#  aux.edu <- names(xtabs(~reuters$EDU,drop.unused.levels = TRUE))
#  cps$EDU <- factor(as.numeric(cps$EDU),levels=1:3,labels=aux.edu)
#  aux.INC <- names(xtabs(~reuters$INCOME2,drop.unused.levels = TRUE))
#  cps$INCOME2 <- factor(as.numeric(cps$INCOME2),levels=1:4,labels=aux.INC)
#  cps$metro <- ifelse(as.numeric(cps$GTMETSTA) == 1,1,2)
#  cps$metro <- factor(cps$metro,levels=1:2,labels=c('Metro','NonMetro'))
#  cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 2:4,1,4)
#  cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 5:6,2,cps$metro2)
#  cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 7,3,cps$metro2)
#  cps$metro2 <- factor(cps$metro2,levels=1:4,labels=c('Less 1M','1M to 5M','5M+','NonMetro'))
#
#  #weight targets - national
#  pop.sex <- xtabs(PWSSWGT~SEX,data=cps)
#  pop.age <- xtabs(PWSSWGT~AGE_GRP,data=cps)
#  pop.edu <- xtabs(PWSSWGT~EDU,data=cps)
#  pop.race <- xtabs(PWSSWGT~RACE_,data=cps)
#  pop.geo9 <- xtabs(PWSSWGT~GEODIV9,data=cps)
#  pop.metro <- xtabs(PWSSWGT~metro,data=cps)
#  pop.metro2 <- xtabs(PWSSWGT~metro2,data=cps)
#
#  #weight targets - STATE vs demos
#  pop.sex.st <- xtabs(PWSSWGT~STATE+SEX,data=cps)
#  pop.age.st <- xtabs(PWSSWGT~STATE+AGE_GRP,data=cps)
#  pop.edu.st <- xtabs(PWSSWGT~STATE+EDU,data=cps)
#  pop.race.st <- xtabs(PWSSWGT~STATE+RACE_,data=cps)
#  pop.geo9.st <- xtabs(PWSSWGT~GEODIV9,data=cps)
#  pop.metro.st <- xtabs(PWSSWGT~STATE+metro,data=cps)
#  pop.metro2.st <- xtabs(PWSSWGT~STATE+metro2,data=cps)
#  #including at least population 1 in each cell
#  pop.metro.st[pop.metro.st == 0] <- 1
#  pop.metro2.st[pop.metro2.st == 0] <- 1
#
#  #saving data to library
#  svy.vote <- reuters %>% select(RESPID,vote,lead,lv,one_of(names(cps)))
#  cps <- cps %>% select(HRHHID,PWSSWGT,one_of(names(svy.vote)))
#  devtools::use_data(svy.vote,cps)
