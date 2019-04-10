require(pd)
require(tidyverse)
library(rpart)
library(rpart.plot)

#########################################################################
#########################################################################

######## WHAT IF I RUN THE TREE ON THE DATA WEIGHTED WITH TRADIONAL RAKING
######## THENIT SHOULD IMPROVE THE RESULTS

### e se usar regression trees ou random forest ao inves de chaid
### https://www.r-bloggers.com/how-random-forests-improve-simple-regression-trees/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29

#########################################################################
#########################################################################


dir <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Ipsos USA\\Reuters\\Reuters 2017\\AAPOR 2017 - MRP for Reuters 2016 data"
file.data <- paste0(dir,"\\Reuters Unified Poll - WEEK 201642 thru 201644.sav")
file.turn <- paste0(dir,"\\2016 pres turnout.xlsx")
dir.beta <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Ipsos USA\\Reuters\\Reuters 2016\\Presidential Election 2016\\Likely Voter model for Boost Sample"
file.beta <- paste0(dir.beta,"\\Betas for Scoring LV model.sav")
dir.cps <- "E:\\DADOS\\Bancos de Dados\\US Census\\Current Population Survey\\2016"
file.cps <- paste0(dir.cps,"\\CPS 2016 June.sav")
dir.gen2016 <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Polling Data\\PollingData2016\\US 2016"
file.gen2016=paste0(dir.gen2016,"\\US2016 Results Gen.RData")

 ##########################################
 ##########################################
 ###### REUTERS DATA - LAST 3 WEEKS - 201642 THRU 201644
 ##########################################
 ##########################################

 data.spss <- read_sav(file.data)

 #sample source: TEMP_EVENT
 #1 – Omni
 #2 – Cortex
 #3 – State Poll
 #NA – Tracker

 vars <- c("RESPID","ID_REUTERS","INTEND","YEAR_WEEK","PD4_NEW_1","NEW_LVscore","PARTY_ID_","STATE","AGE_GRP","EDU","RACE_","INCOME2","GEODIV9","SEX","SVQ6","PD1","realloc2","temp_event","TM651Y15_SON_6","QMktSize_13_1","EarlyVote2016")
 reuters <- data.spss[,vars]
 reuters <- as_factor(reuters,only_labelled = TRUE,ordered=TRUE)

 #dplyr::filter
 reuters <- reuters %>% dplyr::filter(YEAR_WEEK %in% c("201642", "201643", "201644"))

 reuters$date <- as.Date(reuters$INTEND)
 reuters$MONTH <- format(reuters$date,"%m%Y")
 reuters$source <- ifelse(is.na(reuters$temp_event)==TRUE,0,reuters$temp_event)
 reuters$source <- factor(reuters$source,levels=0:2,labels=c("Tracker","Omni","Cortex"))
 reuters$metro <- ifelse(as.numeric(reuters$QMktSize_13_1) <= 3,1,2)
 reuters$metro <- factor(reuters$metro,levels=1:2,labels=c('Metro','NonMetro'))
 reuters$metro2 <- factor(as.numeric(reuters$QMktSize_13_1),levels=1:4,labels=c('Less 1M','1M to 5M','5M+','NonMetro'))
 reuters$QMktSize_13_1 <- NULL

 #make sure that unemployment is compatible between CPS and Reuters
 reuters$employed <- ifelse(as.numeric(reuters$SVQ6)<=4,1,3)
 reuters$employed <- ifelse(as.numeric(reuters$SVQ6) == 7,2,reuters$employed)
 reuters$employed <- factor(reuters$employed,levels=1:3,labels=c('Employed','Unemployed','Not in labour force'))

 ##################################
 #### Vote

 reuters$vote <- reuters$TM651Y15_SON_6
 reuters$lead <- 0
 reuters$lead <- ifelse(as.numeric(reuters$vote) == 1,-1,reuters$lead)
 reuters$lead <- ifelse(as.numeric(reuters$vote) == 2,1,reuters$lead)
 reuters$lead <- ifelse(as.numeric(reuters$vote) == 3,0,reuters$lead)

 ##########################################
 ##########################################
 ###### LV MODEL
 ##########################################
 ##########################################

 betas <- read_sav(file.beta)
 betas <- as_factor(betas,only_labelled = TRUE,ordered=TRUE)

 reuters <- dplyr::left_join(reuters,betas,by=c('STATE','EDU','SEX','AGE_GRP','RACE_','INCOME2','PARTY_ID_'))
 reuters$votoPD4 <- as.numeric(ifelse(as.numeric(reuters$PD4_NEW_1) > 10,0,reuters$PD4_NEW_1))
 reuters$lv <- reuters$intercept + reuters$votoPD4 * reuters$betaPD4
 reuters$lv <- exp(reuters$lv) / (1 + exp(reuters$lv))

 #early voter - probability 1 for the LV model
 reuters$lv.force <- ifelse(as.numeric(reuters$EarlyVote2016) %in% 5:7,1,0)
 reuters$lv <- ifelse(as.numeric(reuters$EarlyVote2016) %in% 5:7,1,reuters$lv)

 ##########################################
 ##########################################
 ###### CPS 2016 June data
 ##########################################
 ##########################################

 ### loading CPS data

 cps <- read_sav(file.cps)
 cps <- as_factor(cps,only_labelled = TRUE,ordered=TRUE)

 vars.cps <- c("HRHHID", "PWSSWGT","GESTCEN", "age_wgts", "edu_wgts", "race_wgts", "income_wgts","sex_wgts","employment_wgts","div9","GTMETSTA","GTCBSASZ")
 cps <- cps[,vars.cps]
 cps$STATE <- as.character(cps$GESTCEN)
 cps$GESTCEN <- NULL
 cps <- cps %>% dplyr::rename(employed=employment_wgts,AGE_GRP=age_wgts,EDU=edu_wgts,RACE_=race_wgts,INCOME2=income_wgts,SEX=sex_wgts,GEODIV9=div9)
 #18+ dplyr::filter
 cps <- cps[cps$AGE_GRP != "NaN",]
 cps <- cps[cps$employed != "NaN",]

 #recodes / labels to match reuters data
 aux.age <- names(xtabs(~reuters$AGE_GRP,drop.unused.levels = TRUE))
 cps$AGE_GRP <- factor(as.numeric(cps$AGE_GRP),levels=1:5,labels=aux.age)
 aux.edu <- names(xtabs(~reuters$EDU,drop.unused.levels = TRUE))
 cps$EDU <- factor(as.numeric(cps$EDU),levels=1:3,labels=aux.edu)
 aux.INC <- names(xtabs(~reuters$INCOME2,drop.unused.levels = TRUE))
 cps$INCOME2 <- factor(ifelse(is.na(as.numeric(cps$INCOME2)),4,as.numeric(cps$INCOME2)),levels=1:4,labels=aux.INC)
 cps$metro <- ifelse(as.numeric(cps$GTMETSTA) == 1,1,2)
 cps$metro <- factor(cps$metro,levels=1:2,labels=c('Metro','NonMetro'))
 cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 2:4,1,4)
 cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 5:6,2,cps$metro2)
 cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 7,3,cps$metro2)
 cps$metro2 <- factor(cps$metro2,levels=1:4,labels=c('Less 1M','1M to 5M','5M+','NonMetro'))

 #saving data to library
 svy.vote <- reuters %>% select(RESPID,vote,lead,lv,one_of(names(cps)))
 cps <- cps %>% select(HRHHID,PWSSWGT,one_of(names(svy.vote)))
 cps <- cps %>% filter(!is.na(HRHHID))
 cps <- droplevels(cps)

 attributes(svy.vote$vote)$label <- NULL
 attributes(svy.vote$RESPID)$label <- NULL
 devtools::use_data(svy.vote,cps,overwrite = TRUE)
