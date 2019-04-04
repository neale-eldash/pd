raking_svy <- function (design, sample.margins, population.margins, control = list(maxit = 10,epsilon = 1, verbose = FALSE), compress = NULL){
  #' Function for running the raking algorithm from the survey package, ignoring missing marginals
  #'
  #' I created this function to avoid errors when running the raking algorithm with small
  #' samples.

  if (!missing(control)) {
    control.defaults <- formals(rake)$control
    for (n in names(control.defaults)) if (!(n %in% names(control)))
      control[[n]] <- control.defaults[[n]]
  }
  is.rep <- inherits(design, "svyrep.design")
  if (is.rep && is.null(compress))
    compress <- inherits(design$repweights, "repweights_compressed")
  if (is.rep)
    design$degf <- NULL
  if (length(sample.margins) != length(population.margins))
    stop("sample.margins and population.margins do not match.")
  nmar <- length(sample.margins)
  if (control$epsilon < 1)
    epsilon <- control$epsilon * sum(weights(design, "sampling"))
  else epsilon <- control$epsilon
  strata <- lapply(sample.margins, function(margin) if (inherits(margin,
                                                                 "formula")) {
    mf <- model.frame(margin, data = design$variables)
  })
  allterms <- unlist(lapply(sample.margins, all.vars))
  ff <- formula(paste("~", paste(allterms, collapse = "+"),
                      sep = ""))
  oldtable <- svytable(ff, design)
  if (control$verbose)
    print(oldtable)
  oldpoststrata <- design$postStrata
  iter <- 0
  converged <- FALSE
  while (iter < control$maxit) {
    design$postStrata <- NULL
    for (i in 1:nmar) {
      design <- postStratify(design, strata[[i]], population.margins[[i]],
                             compress = FALSE,partial = TRUE)
    }
    newtable <- svytable(ff, design)
    if (control$verbose)
      print(newtable)
    delta <- max(abs(oldtable - newtable))
    if (delta < epsilon) {
      converged <- TRUE
      break
    }
    oldtable <- newtable
    iter <- iter + 1
  }
  rakestrata <- design$postStrata
  if (!is.null(rakestrata)) {
    class(rakestrata) <- "raking"
    design$postStrata <- c(oldpoststrata, list(rakestrata))
  }
  design$call <- sys.call()
  if (is.rep && compress)
    design$repweights <- compressWeights(design$repweights)
  if (is.rep)
    design$degf <- degf(design)
  if (!converged)
    warning("Raking did not converge after ", iter, " iterations.\n")
  return(design)
}

check_categs <- function(x,y){
  df.x <- x %>% drop_na() %>% gather(var,categ) %>% group_by(var,categ) %>% count() %>% rename(n.x=n)
  df.y <- y %>% drop_na() %>% gather(var,categ) %>% group_by(var,categ) %>% count() %>% rename(n.y=n)
  df <- df.x %>% full_join(df.y)
  if (sum(is.na(df$n.x))>0 | sum(is.na(df$n.y))>0){
    print(df)
    stop("erro de codificação das variáveis.")
  }
  return(df)
}

rake_df <- function(df.svy=NA,df.pop=NA,reg.exp.vars=NA,reg.exp.cruz=NA,reg.exp.id=NA,reg.exp.wgts=NA){
  #' Rake sample to match population. Population input is a data frame of the \emph{population}.
  #'
  #' This function rakes the sample to match the population counts. This algorithm has 4 basic steps:
  #' \itemize{
  #'  \item \strong{Check variables}: checks that same variables with same labels are in both dataframes.
  #'  \item \strong{Population targets}: Calculates the population targets from the population dataframe.
  #'  \item \strong{Rake sample}: Uses a adjusted raking algorithm adapted from
  #'  in \code{\link[survey]{rake}}.
  #'  \item \strong{Check weights}: Compares the weights to the population targets to make sure the raking
  #'  worked.
  #' }
  #'
  #' @param df.svy The sample \emph{dataframe}, containing the variables to be used in the analysis (unique id,
  #' targets and cross-variable).
  #' @param df.pop The population \emph{dataframe}, containing the variables to be used in the analysis (weights,
  #' targets, cross-variable).
  #' @param reg.exp.vars A \emph{string} with the regular expression identifying the target variables (i.e., those
  #' variables that the sample total should match the population total). These variables should exist in both the
  #' sample and population dataframes.
  #' @param reg.exp.cruz A \emph{string} with the regular expression identifying the variable which the target
  #' variables are crossed by (usually reagion). The target variables will match the population within each label of
  #' the crossing variable. These variables should exist in both the sample and population dataframes.
  #' @param reg.exp.id A \emph{string} with the regular expression identifying the unique id variable. This variable
  #' needs to exist only in the sample dataframe.
  #' @param reg.exp.wgts A \emph{string} with the regular expression identifying the population weight variable.
  #' This variable needs to exist only in the population dataframe.
  #' @return A list with two components:
  #' \itemize{
  #'  \item \strong{weights}\emph{(dataframe)}: the original sample dataframe with the weights.
  #'  \item \strong{check.vars}\emph{(dataframe)}: comparison of all variables and labels used.
  #'  \item \strong{check.wgts}\emph{(dataframe)}: comparison of all weights and population totals.
  #' }
  #' @examples
  #'
  #' Raking \emph{without} crossing variable:
  #' weights <- rake_df(df.svy=dados,df.pop=base,reg.exp.vars="_cota$",reg.exp.cruz=NA,reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")
  #' Raking \emph{with} crossing variable:
  #' weights <- rake_df(df.svy=dados,df.pop=base,reg.exp.vars="_cota$",reg.exp.cruz="^regiao$",reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")

  reg.exp.vars <- ifelse(is.na(reg.exp.vars)," ",reg.exp.vars)
  reg.exp.cruz <- ifelse(is.na(reg.exp.cruz)," ",reg.exp.cruz)
  reg.exp.id <- ifelse(is.na(reg.exp.id)," ",reg.exp.id)
  reg.exp.wgts <- ifelse(is.na(reg.exp.wgts)," ",reg.exp.wgts)

  ####################################
  ############  CHECKS
  ####################################

  ############
  ### checando bases

  if (!("data.frame" %in% class(df.svy))){
    stop("Missing dataframe with survey data.")
  }
  if (!("data.frame" %in% class(df.pop))){
    stop("Missing dataframe with population data.")
  }

  ############
  ### checando variaveis cota

  check.vars.svy <- names(select(df.svy,matches(reg.exp.vars)))
  check.vars.pop <- names(select(df.pop,matches(reg.exp.vars)))
  vars_cota <- intersect(check.vars.svy,check.vars.pop)
  if (length(check.vars.svy) == 0 | length(check.vars.pop) == 0){
    stop("Mão foram encontradas variáveis de cota em uma das bases.")
  }
  if (length(vars_cota) == 0){
    stop("Não existem variáveis de cota comuns entre as bases.")
  } else if (length(vars_cota) == 1){
    if (vars_cota == "") {
      stop("Não existem variáveis de cota comuns entre as bases.")
    }
  }
  if (length(check.vars.svy) != length(check.vars.pop)){
    warning("O número de variáveis de cota em cada base diverge. Serão usadas essas variáveis:")
  } else {
    warning("As variáveis de cota utilizadas na ponderação serão:")
  }
  print(vars_cota)

  ############
  ### checando variavel cruzamento

  check.cruz.svy <- names(select(df.svy,matches(reg.exp.cruz)))
  check.cruz.pop <- names(select(df.pop,matches(reg.exp.cruz)))
  var_cruz <- intersect(check.cruz.svy,check.cruz.pop)
  if (length(var_cruz) == 0){
    warning("Não será utilizada variável de cruzamento.")
  } else if (var_cruz == "") {
    warning("Não será utilizada variável de cruzamento.")
  } else if (length(var_cruz) > 1) {
    stop("Mais de uma variável de cruzamento encontrada na base.")
  } else {
    warning("A variável de cruzamento utilizada será:")
    print(var_cruz)
    vars_cota <- setdiff(vars_cota,var_cruz)
  }

  ############
  ### checando id

  var_id <- names(select(df.svy,matches(reg.exp.id)))
  if (length(var_id) != 1){
    stop("Variável ID não foi corretamente definida.")
  } else if (var_id == "") {
    stop("Variável ID não foi corretamente definida.")
  }
  df.svy[,"id"] <- df.svy[,var_id]

  ############
  ### checando peso

  var_wgt <- names(select(df.pop,matches(reg.exp.wgts)))
  if (length(var_wgt) != 1){
    warning("Não será usada variável de ponderação pra definir os targets populacionais.")
  } else if (var_wgt == "") {
    warning("Não será usada variável de ponderação pra definir os targets populacionais.")
  }

  ############
  ### checando categs

  df.categs <- check_categs(select(df.svy,one_of(vars_cota,var_cruz)),select(df.pop,one_of(vars_cota,var_cruz)))

  ####################################
  ############  TARGETS
  ####################################

  #adding weights
  if (length(var_wgt) == 1){
    df.pop <- df.pop %>% rename(peso=var_wgt)
  } else {
    df.pop$peso <- 1
  }

  #adding cruzamento
  if (length(var_cruz) == 1){
    df.pop <- df.pop %>% rename(cruz=var_cruz)
    df.svy <- df.svy %>% rename(cruz=var_cruz)
  } else {
    df.pop$cruz <- "Total"
    df.svy$cruz <- "Total"
  }

  #agregating pop counts
  targets <- df.pop %>%
    select(one_of(vars_cota),cruz,peso) %>%
    gather(var,categ,one_of(vars_cota),na.rm = TRUE) %>%
    group_by(cruz,var,categ) %>%
    summarise(
      pop = sum(peso,na.rm = TRUE)
    )

  #adjusting for possible uneven NA
  targets <- targets %>% group_by(cruz,var) %>% mutate(
    pop.tot=sum(pop),
    pop = pop / pop.tot)
  targets <- targets %>% group_by(cruz) %>% mutate(pop.tot=max(pop.tot))
  targets <- targets %>% ungroup() %>% mutate(pop=pop * pop.tot) %>% select(-pop.tot)

  ####################################
  ############  PONDERAÇÃO
  ####################################

  wgts <- sort(vars_cota)
  sample <- map(wgts,~as.formula(paste0('~',.,'+cruz')))

  targets <- targets %>% arrange(cruz,var)

  population <- map(wgts,function(x){
    df.svy <- targets[targets$var == x,]
    df.svy$var <- NULL
    df.svy[,x] <- df.svy$categ
    df.svy$categ <- NULL
    return(df.svy)
  })

  #removing missings
  df.comp <- df.svy %>% select(id,cruz,one_of(vars_cota)) %>% drop_na()
  data.svy <- svydesign(id=~id,data = df.comp);
  data.svy <- raking_svy(data.svy, sample=sample, population=population, control = list(maxit = 800))
  df.comp$weights <- weights(data.svy)
  df.svy <- df.svy %>% left_join(select(df.comp,id,weights))

  #weight 1 for respondentes with missing (rescaled)
  df.svy$weights <- ifelse(is.na(df.svy$weights),1,df.svy$weights)
  df.svy$weights <- df.svy$weights * (nrow(df.svy) / sum(df.svy$weights))

  ####################################
  ############  CHECKS
  ####################################

  check <- df.svy[,c('weights','cruz',wgts)]
  check <- check %>% gather(var,categ,-weights,-cruz) %>% group_by(cruz,var,categ) %>% summarise(sample=n(),raw=n(),max.wgt = max(weights),min.wgt = min(weights),weights=sum(weights))
  check <- full_join(check,targets,by=c('cruz','var','categ'))
  var.tot <- check$var[1]
  check.tot <- check %>% ungroup() %>% filter(var == var.tot)
  check <- check %>% mutate(
    raw=round(100*raw/sum(raw,na.rm = TRUE),1),
    weights=round(100*weights/sum(weights,na.rm = TRUE),1),
    pop=round(100*pop/sum(pop,na.rm = TRUE),1),
    diff=weights - pop
  )

  check.tot <- check.tot %>% group_by(cruz) %>% summarise_at(vars(-cruz,-var,-categ,-ends_with('.wgt')),funs(sum(.)))
  check.tot <- check.tot %>% ungroup() %>% mutate(
    raw=round(100*raw/sum(raw,na.rm = TRUE),1),
    weights=round(100*weights/sum(weights,na.rm = TRUE),1),
    pop=round(100*pop/sum(pop,na.rm = TRUE),1),
    diff=weights - pop
  )
  check <- check %>% bind_rows(check.tot)

  check <- check %>% select(cruz:raw,weights:diff,ends_with('.wgt'))
  check <- check %>% rename(Cruzamento=cruz, Variável=var, Categoria=categ, Amostra=sample, Sem_Ponderar=raw, Ponderado=weights, População=pop,Diferença=diff)
  check <- as.data.frame(check)
  check <- check %>% filter(Amostra != nrow(df.svy))

  if (length(var_cruz) == 0){
    check$Cruzamento <- NULL
  }

  df.svy <- df.svy %>% select(-id,-cruz)
  saida <- list(weights=df.svy,check.vars=df.categs,check.wgts=check)

  return(saida)

}

# dados <- dados %>% rename(regiao=REGIAO)
# base <- base %>% rename(sexo_cota=sexo)
# base$regiao <- str_replace(base$regiao,"Centro Oeste","Centro-Oeste")
# teste.rake <- rake_df(df.svy=dados,df.pop=base,reg.exp.vars="_cota$",reg.exp.cruz=NA,reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")
# teste.rake.cruz <- rake_df(df.svy=dados,df.pop=base,reg.exp.vars="_cota$",reg.exp.cruz="^regiao$",reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")

# ##################################################
# ##################################################
# ##################################################
# ################ Raking
# ##################################################
# ##################################################
# ##################################################
#
#
# require(tidyverse)
# require(sampling)
# require(stringr)
# require(lubridate)
# require(haven)
# require(xlsx)
# require(survey)
# require(readxl)
# require(neale)
# require(rvest)
#
# ###################################
# ###################################
# ###### Parameters
# ###################################
# ###################################
#
# dir.cetic <- 'E:/DADOS/Bancos de Dados/Internet CETIC/2015'
# file.cetic.dom <- paste0(dir.cetic,'/ticdom_2015_domicilios_base_de_microdados_v1.0.csv')
# file.cetic.pes <- paste0(dir.cetic,'/ticdom_2015_individuos_base_de_microdados_v1.0.csv')
#
# dir <- 'E:/DADOS/CONSULTORIA/PESSOAS FÍSICAS/Paulo Cidade/Projeto iFood - 10-01-2018/Brasil/Dados'
# file.spss <- paste0(dir,'\\HAREBR_131942_20180306_(n2004).sav')
# file.check <- paste0(dir,'\\check pesos - 2004 casos.xlsx')
# file.peso <- paste0(dir,'\\Base iFood Brasil - Ponderada - 2004 casos.sav')
#
# ##############################################
# ######### SampleFrame - microdados PNAD
# ##############################################
#
# pnad.dir <- "E:\\DADOS\\Bancos de Dados\\PNAD\\Microdados PNAD 2014\\Dados"
#
# pnad <- get_spss(file=paste0(pnad.dir,"\\PNAD2014 - Pessoas - para R com cotas Paulo 2017.sav"))$sav
# pnad <- pnad %>% select(V4729,area,reg,sexo_cota,V8005,V0232)
# pnad <- pnad %>% rename(pop=V4729,cota_sexo=sexo_cota)
# # pnad <- pnad %>% mutate(
# #   fx_idade = cut(as.numeric(V8005),breaks = c(-1,11,16,21,36,56,71,150),labels = c('0-10','11-15','16-20','21-35','36-55','56-70','71+'))
# # )
# # pnad <- pnad %>% group_by(fx_idade) %>% summarise(
# #   pop=sum(pop)
# # )
# # pnad <- pnad %>% ungroup() %>% mutate(
# #   prop=round(100*pop/sum(pop),1)
# # )
# # write.table(pnad,'clipboard',sep='\t',row.names = FALSE,dec=".")
# pnad <- pnad %>% filter(V8005 %in% 16:70,V0232=='Sim')
#
# pnad.cota <- pnad %>% select(reg,area,pop) %>% gather(var,categ,area)
# pnad.cota <- pnad.cota %>% group_by(reg,var,categ) %>% summarise(pop=sum(pop))
#
#
#
# ##############################################
# ######### SampleFrame - microdados CETIC
# ##############################################
#
# cetic.dom <- read_delim(file.cetic.dom,delim=';',locale = locale(decimal_mark = ","))
# cetic.pes <- read_delim(file.cetic.pes,delim=';',locale = locale(decimal_mark = ","))
#
# pes <- cetic.pes %>% mutate(
#   cota_idade=cut(idade,breaks = c(15,20,35,55,70),labels = c('16-20','21-35','36-55','56-70')),
#   cota_sexo=factor(sexo,levels=1:2,labels=c("Masculino","Feminino")),
#   cota_edu=case_when(
#     grau_instrucao %in% 1:8 ~ 1,
#     grau_instrucao %in% 9:10 ~ 2,
#     grau_instrucao %in% 11:12 ~ 3
#   ),
#   cota_edu=factor(cota_edu,levels=1:3,labels=c('Fundamental','Médio','Superior')),
#   cota_pea=ifelse(pea == 5,2,1),
#   cota_pea=factor(cota_pea,levels=1:2,labels=c('Pea','Não Pea')),
#   cota_internet=factor(C3,levels=c(1:3,99),labels=c("Há menos de 3 meses","Entre 3 meses e 12 meses","Mais de 12 meses atrás","Não se aplica"))
# ) %>% select(id_domicilio,Peso,starts_with('cota'))
#
# dom <- cetic.dom %>% mutate(
#   cod_regiao=factor(cod_regiao,levels=1:5,labels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")),
#   classe_cb2015= factor(classe_cb2015,levels=1:4,labels=c("A","B","C","DE")),
#   area=factor(area,levels=1:2,labels=c("Urbana","Rural")),
#   Peso_dom=PESO
# ) %>% select(id_domicilio,Peso_dom,cod_regiao,classe_cb2015,area) %>% rename(reg=cod_regiao,classe=classe_cb2015)
#
# df.cetic <- pes %>% left_join(dom)
# df.cetic <- df.cetic %>% filter(cota_internet == "Há menos de 3 meses" & classe %in% c("A","B","C") & !is.na(cota_idade))
#
# cetic.cota <- df.cetic %>% select(reg,Peso,classe,starts_with('cota'),-cota_internet) %>% gather(var,categ,-reg,-Peso)
# cetic.cota <- cetic.cota %>% group_by(reg,var,categ) %>% summarise(pop=sum(Peso))
#
# total.cota <- bind_rows(pnad.cota,cetic.cota)
# total.cota$var <- str_replace(total.cota$var,'area','cota_area')
# total.cota$var <- str_replace(total.cota$var,'classe','cota_classe')
#
# ###################################
# ###################################
# ###### Survey data
# ###################################
# ###################################
#
# spss <- get_spss(file.spss)
# df <- spss$sav
# df.vars <- spss$vars
#
# df <- df %>% select(numericalId,RegionRecode,Q1,Q2,Q3,RM_INTRecode,EduRecode,PEARecode)
# df <- df %>% rename(reg=RegionRecode,cota_sexo=Q1,cota_idade=Q2,cota_classe=Q3,cota_area=RM_INTRecode,cota_edu=EduRecode,cota_pea=PEARecode)
# df <- df %>% mutate(
#   cota_sexo = factor(as.numeric(cota_sexo),levels=1:2,labels=levels(df.cetic$cota_sexo)),
#   cota_idade = factor(as.numeric(cota_idade) - 1,levels=1:4,labels=levels(df.cetic$cota_idade)),
#   cota_classe = case_when(
#     cota_classe %in% c('A') ~ 1,
#     cota_classe %in% c('B1','B2') ~ 2,
#     cota_classe %in% c('C1','C2') ~ 3
#   ),
#   cota_classe = factor(cota_classe,levels=1:3,labels=levels(df.cetic$classe)[1:3]),
#   cota_pea = factor(as.numeric(cota_pea),levels=1:2,labels=levels(df.cetic$cota_pea)),
#   reg = factor(as.numeric(reg),levels=1:5,labels=levels(df.cetic$reg))
# )
#
#
#
# ########################################
# ########################################
# ########################################
# #### Weights
# ########################################
# ########################################
# ########################################
#
# ########################################
# #Raking data frame
#
# wgts <- sort(names(df %>% select(starts_with('cota'))))
# sample <- map(wgts,~as.formula(paste0('~',.,'+reg')))
#
# targets <- total.cota %>% arrange(reg,var)
#
# population <- map(wgts,function(x){
#   df <- targets[targets$var == x,]
#   df$var <- NULL
#   df[,x] <- df$categ
#   df$categ <- NULL
#   return(df)
# })
#
# #removing missings
# df.comp <- df %>% drop_na(starts_with('cota'))
# data.svy <- svydesign(id=~numericalId,data = df.comp);
# data.svy <- raking_svy(data.svy, sample=sample, population=population, control = list(maxit = 800))
# df.comp$weights <- weights(data.svy)
# df <- df %>% left_join(select(df.comp,numericalId,weights))
#
# #weight 1 for respondentes with missing (relative to the whole population)
# #not an actal final weight of 1
# df$weights <- ifelse(is.na(df$weights),1,df$weights)
# df$weights <- df$weights * (nrow(df) / sum(df$weights))
#
# #############################
# #checks
#
# check <- df[,c('weights','reg',wgts)]
# check <- check %>% gather(var,categ,-weights,-reg) %>% group_by(reg,var,categ) %>% summarise(sample=n(),raw=n(),weights=sum(weights))
# check <- full_join(check,targets,by=c('reg','var','categ'))
# check.tot <- check %>% filter(var == 'cota_sexo') %>% group_by(reg) %>% select(-var,-categ) %>% summarise_all(funs(sum(.,na.rm = TRUE)))
# check.tot <- check.tot %>% rename(categ=reg)
# check.tot$var <- 'Reg'
# check <- check %>% bind_rows(check.tot)
# check <- check %>% arrange(reg,var,categ)
# check[,4:7] <- check[,4:7] %>% map_df(~ifelse(is.na(.),0,.))
# check <- check %>% mutate(raw=round(100*raw/sum(raw,na.rm = TRUE),1),weights=round(100*weights/sum(weights,na.rm = TRUE),1),pop=round(100*pop/sum(pop,na.rm = TRUE),1))
# check$diff <- check$weights - check$pop
# check <- as.data.frame(check)





#' ##################################################
#' ##################################################
#' ##################################################
#' ################ Tree Raking
#' ##################################################
#' ##################################################
#' ##################################################
#'
#'
#' require(neale)
#' require(xlsx)
#' require(ggplot2)
#' require(dplyr)
#' require(tidyr)
#' require(stringr)
#' require(survey)
#' require(haven)
#' require(stringr)
#' require(purrr)
#' library(rpart)
#' library(rpart.plot)
#' #library(rpart.utils)
#' options(stringsAsFactors = FALSE)
#'
#'
#' #########################################################################
#' #########################################################################
#'
#' ######## WHAT IF I RUN THE TREE ON THE DATA WEIGHTED WITH TRADIONAL RAKING
#' ######## THENIT SHOULD IMPROVE THE RESULTS
#'
#' ### e se usar regression trees ou random forest ao inves de chaid
#' ### https://www.r-bloggers.com/how-random-forests-improve-simple-regression-trees/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29
#'
#' #########################################################################
#' #########################################################################
#'
#'
#' dir <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Ipsos USA\\Reuters\\Reuters 2017\\AAPOR 2017 - MRP for Reuters 2016 data"
#' file.data <- paste0(dir,"\\Reuters Unified Poll - WEEK 201642 thru 201644.sav")
#' file.turn <- paste0(dir,"\\2016 pres turnout.xlsx")
#' dir.beta <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Ipsos USA\\Reuters\\Reuters 2016\\Presidential Election 2016\\Likely Voter model for Boost Sample"
#' file.beta <- paste0(dir.beta,"\\Betas for Scoring LV model.sav")
#' dir.cps <- "E:\\DADOS\\Bancos de Dados\\US Census\\Current Population Survey\\2016"
#' file.cps <- paste0(dir.cps,"\\CPS 2016 June.sav")
#' dir.gen2016 <- "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Polling Data\\PollingData2016\\US 2016"
#' file.gen2016=paste0(dir.gen2016,"\\US2016 Results Gen.RData")
#'
#' ##########################################
#' ##########################################
#' ###### functions
#' ##########################################
#' ##########################################
#'
#' raking_svy <- function (design, sample.margins, population.margins, control = list(maxit = 10,epsilon = 1, verbose = FALSE), compress = NULL){
#'   #' Function for running the raking algorithm from the survey package, ignoring missing marginals
#'   #'
#'   #' I created this function to avoid errors when running the raking algorithm with small
#'   #' samples.
#'
#'   if (!missing(control)) {
#'     control.defaults <- formals(rake)$control
#'     for (n in names(control.defaults)) if (!(n %in% names(control)))
#'       control[[n]] <- control.defaults[[n]]
#'   }
#'   is.rep <- inherits(design, "svyrep.design")
#'   if (is.rep && is.null(compress))
#'     compress <- inherits(design$repweights, "repweights_compressed")
#'   if (is.rep)
#'     design$degf <- NULL
#'   if (length(sample.margins) != length(population.margins))
#'     stop("sample.margins and population.margins do not match.")
#'   nmar <- length(sample.margins)
#'   if (control$epsilon < 1)
#'     epsilon <- control$epsilon * sum(weights(design, "sampling"))
#'   else epsilon <- control$epsilon
#'   strata <- lapply(sample.margins, function(margin) if (inherits(margin,
#'                                                                  "formula")) {
#'     mf <- model.frame(margin, data = design$variables)
#'   })
#'   allterms <- unlist(lapply(sample.margins, all.vars))
#'   ff <- formula(paste("~", paste(allterms, collapse = "+"),
#'                       sep = ""))
#'   oldtable <- svytable(ff, design)
#'   if (control$verbose)
#'     print(oldtable)
#'   oldpoststrata <- design$postStrata
#'   iter <- 0
#'   converged <- FALSE
#'   while (iter < control$maxit) {
#'     design$postStrata <- NULL
#'     for (i in 1:nmar) {
#'       design <- postStratify(design, strata[[i]], population.margins[[i]],
#'                              compress = FALSE,partial = TRUE)
#'     }
#'     newtable <- svytable(ff, design)
#'     if (control$verbose)
#'       print(newtable)
#'     delta <- max(abs(oldtable - newtable))
#'     if (delta < epsilon) {
#'       converged <- TRUE
#'       break
#'     }
#'     oldtable <- newtable
#'     iter <- iter + 1
#'   }
#'   rakestrata <- design$postStrata
#'   if (!is.null(rakestrata)) {
#'     class(rakestrata) <- "raking"
#'     design$postStrata <- c(oldpoststrata, list(rakestrata))
#'   }
#'   design$call <- sys.call()
#'   if (is.rep && compress)
#'     design$repweights <- compressWeights(design$repweights)
#'   if (is.rep)
#'     design$degf <- degf(design)
#'   if (!converged)
#'     warning("Raking did not converge after ", iter, " iterations.\n")
#'   return(design)
#' }
#'
#' get_common_vars <- function(df.x,df.y){
#'
#'   vars <- names(df.x)[names(df.x) %in% names(df.y)]
#'   return(vars)
#'
#' }
#'
#' create_cells_df <- function(df,vars,weight=NA){
#'
#'   if (is.na(weight) == TRUE){
#'     df$wgts <- 1
#'   } else {
#'     df <- df %>% select(one_of(vars),one_of(weight))
#'     df$wgts <- df[,weight]
#'     df[,weight] <- NULL
#'   }
#'
#'   df <- df %>% group_by_(.dots=vars) %>% summarise(wgts=sum(wgts),n=n())
#'   return(df)
#'
#' }
#'
#' replicate_tree <- function (object, newdata,na.action = na.pass){
#'
#'   df.orig <- newdata
#'   if (is.null(attr(newdata, "terms"))) {
#'     Terms <- delete.response(object$terms)
#'     newdata <- model.frame(Terms, newdata, na.action = na.action,
#'                            xlev = attr(object, "xlevels"))
#'     if (!is.null(cl <- attr(Terms, "dataClasses")))
#'       .checkMFClasses(cl, newdata, TRUE)
#'   }
#'   df.orig$grp <- rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
#'   df.orig
#' }
#'
#' extract_names <- function(df.pop,vars){
#'
#'   df.names <- df.pop %>% select(one_of(vars),grp,strata)
#'   df.names <- df.names %>% gather(var,categ,-grp,-strata)
#'   df.names <- df.names %>% group_by(strata,grp,var,categ) %>% summarise(n=n()) %>% select(-n)
#'   df.names <- df.names[complete.cases(df.names),]
#'   df.names <- df.names %>% group_by(strata,grp,var) %>% do(categs=paste(.$categ,collapse=";"))
#'   df.names <- df.names %>% spread(var,categs)
#'   df.names <- df.names %>% arrange(strata,grp)
#'
#'   return(df.names)
#' }
#'
#' create_tree <- function(cells.svy,indep=NULL,dep=NULL,minbucket = 30,cp = 0.001){
#'
#'   #options that can be used with rpart
#'   #to labels and visualize groups, for example
#'
#'   # bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
#'   # tree.pruned <- prune(fit, cp = bestcp)
#'   # prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)
#'   # plot(tree.pruned); text(tree.pruned)
#'   # df.tree <- tree.pruned$frame
#'   # df.grp <- as.data.frame(tree.pruned$where)
#'   # df.split <- as.data.frame(tree.pruned$splits)
#'   # df.csplit <- as.data.frame(tree.pruned$csplit)
#'   # ttt <- rpart.lists(tree.pruned)
#'   # df.ttt <- rpart.rules(tree.pruned)
#'   # df.ttt1 <- rpart.rules.table(tree.pruned)
#'   # df.ttt2 <- rpart.subrules.table(tree.pruned)
#'
#'   if(any(is.null(vars),is.null(dep))){
#'     stop("Dependent and Independent variables must be specified!")
#'   }
#'
#'   #tree
#'   frmla <- as.formula(paste(dep,paste(indep,collapse='+'),sep='~'))
#'   tree <- rpart(frmla,method="anova", data=cells.svy,weights=wgts,control=rpart.control(minbucket = minbucket,cp = cp))
#'
#'   #prune tree
#'   # graf <- prp(tree, faclen = 0, cex = 0.8, extra = 1)
#'   # bestcp <- tree$cptable[which.min(fit.cells$cptable[,"xerror"]),"CP"]
#'   # fit.cells <- prune(tree, cp = bestcp)
#'   cells.svy$grp <- tree$where
#'
#'   saida <- list(cells.svy=cells.svy,tree=tree)
#'   return(saida)
#'
#' }
#'
#' create_tree_grps <- function(df.pop,df.svy,strata=NULL,dep=NULL,wgt.pop=NULL,minbucket = 30,cp = 0.001){
#'
#'   vars <- get_common_vars(df.svy,df.pop)
#'   if(length(vars) == 0){
#'     stop("There are no common variable in the survey and population datasets!")
#'   }
#'
#'   #test if strata is in both data sets and create formula
#'   if (is.null(strata) == FALSE){
#'     if (!(strata %in% vars)){
#'       stop("Variable 'strata' has to be in both dataframes!")
#'     }
#'     vars <- setdiff(vars,strata)
#'     df.pop$strata <- df.pop[,strata]
#'     df.svy$strata <- df.svy[,strata]
#'   } else {
#'     df.pop$strata <- 1
#'     df.svy$strata <- 1
#'   }
#'   indep.vars <- vars
#'   vars <- c(vars,'strata')
#'
#'   #datasets - cells
#'   cells.pop <- create_cells_df(df=df.pop,vars=vars,weight=wgt.pop)
#'   cells.svy <- create_cells_df(df=df.svy,vars=c(vars,dep))
#'
#'   #generate tree
#'   fit.cells <- cells.svy %>% group_by(strata) %>% do(fit=create_tree(.,indep=indep.vars,dep=dep,minbucket = minbucket,cp = cp))
#'   cells.svy <- map_df(fit.cells$fit,'cells.svy')
#'
#'   cells.pop.fit <- map2_df(fit.cells$strata,fit.cells$fit,
#'                            function(x,y){
#'                              df <- cells.pop %>% filter(strata == x);
#'                              df <- replicate_tree(y$tree,df)
#'                            })
#'   cells.pop <- left_join(cells.pop,cells.pop.fit)
#'   df.svy <- left_join(df.svy,select(cells.svy,-wgts,-n),by=c(vars,dep))
#'
#'   #generate names
#'   df.names <- extract_names(cells.svy,indep.vars)
#'
#'   saida <- list(df.svy=df.svy,cells.pop=cells.pop,grps=df.names,trees=fit.cells)
#' }
#'
#' chaid_raking <- function(df.pop,df.svy,strata=NULL,id.var=NULL,dep=NULL,wgt.pop=NULL,minbucket = 30,cp = 0.001){
#'
#'   if(is.null(dep)){
#'     stop("Dependent variable must be specified!")
#'   }
#'   if(is.null(wgt.pop)){
#'     stop("Weight variable for population dataset must be specified!")
#'   }
#'   if(is.null(id.var)){
#'     stop("The id variable must be specified!")
#'   }
#'
#'   ### formating datasets
#'   df.pop <- as.data.frame(df.pop)
#'   df.svy <- as.data.frame(df.svy)
#'   id.var <- as.formula(paste0('~',id.var))
#'
#'   #tree <- create_tree_grps(pnad,base,dep='Q4_1_dummy',wgt.pop='pop')
#'   tree <- create_tree_grps(df.pop,df.svy,dep=dep,wgt.pop=wgt.pop,strata=strata,minbucket = minbucket,cp = cp)
#'
#'   #############################
#'   #tree output
#'
#'   df.svy <- tree$df.svy
#'   cells.pop <- tree$cells.pop
#'
#'   #############################
#'   #targets
#'
#'   targets.pop <- cells.pop %>% ungroup() %>% select(strata,grp,wgts) %>% filter(is.na(grp)==FALSE)
#'   targets.pop <- targets.pop %>% group_by(strata,grp) %>% summarise(wgts=sum(wgts))
#'   targets.pop <- targets.pop %>% ungroup() %>% mutate(wgts=round(100*wgts/sum(wgts),1))
#'   targets.pop <- as.data.frame(targets.pop)
#'   pop.tree <- xtabs(wgts~strata+grp,data=cells.pop)
#'
#'   targets.svy <- df.svy %>% ungroup() %>% select(strata,grp) %>% filter(is.na(grp)==FALSE)
#'   targets.svy <- targets.svy %>% group_by(strata,grp) %>% summarise(n=n())
#'   missing.grps <- anti_join(targets.svy,targets.pop,by=c('strata','grp'))
#'   missing.grps <- missing.grps %>% select(-n) %>% mutate(tira='missing grp')
#'
#'   #############################
#'   #giving weight 1 to grps missing in the population
#'   df.svy <- left_join(df.svy,missing.grps,by=c('strata','grp'))
#'
#'   #############################
#'   #raking
#'
#'   data.svy <- svydesign(id=id.var,data = df.svy[is.na(df.svy$tira) == TRUE,]);
#'   data.svy <- raking_svy(data.svy, sample=list(~strata+grp), population=list(pop.tree), control = list(maxit = 800))
#'   df.svy$weights <- 1
#'   df.svy$weights[is.na(df.svy$tira) == TRUE] <- weights(data.svy)
#'   df.svy$weights <- df.svy$weights * (dim(df.svy)[1] / sum(df.svy$weights))
#'
#'   #############################
#'   #checks
#'
#'   check <- df.svy[,c('weights','strata','grp')]
#'   check <- check %>% group_by(strata,grp) %>% summarise(svy.sample=n(),svy.raw=n(),svy.wgts=sum(weights))
#'   check <- full_join(check,rename(targets.pop,pop.wgts=wgts),by=c('strata','grp'))
#'   check <- check %>% ungroup() %>% mutate(svy.raw=round(100*svy.raw/sum(svy.raw,na.rm = TRUE),1),svy.wgts=round(100*svy.wgts/sum(svy.wgts,na.rm = TRUE),1))
#'   check$diff <- check$svy.wgts - check$pop.wgts
#'   check <- as.data.frame(check)
#'
#'   saida <- list(df.svy=df.svy,cells.pop=cells.pop,check=check,trees=tree$trees,grps=tree$grps)
#'   return(saida)
#'
#' }
#'
#'
#' ##########################################
#' ##########################################
#' ###### REUTERS DATA - LAST 3 WEEKS - 201642 THRU 201644
#' ##########################################
#' ##########################################
#'
#' data.spss <- read_sav(file.data)
#'
#' #sample source: TEMP_EVENT
#' #1 – Omni
#' #2 – Cortex
#' #3 – State Poll
#' #NA – Tracker
#'
#' vars <- c("RESPID","ID_REUTERS","INTEND","YEAR_WEEK","PD4_NEW_1","NEW_LVscore","PARTY_ID_","STATE","AGE_GRP","EDU","RACE_","INCOME2","GEODIV9","SEX","SVQ6","PD1","realloc2","temp_event","TM651Y15_SON_6","QMktSize_13_1","EarlyVote2016")
#' reuters <- data.spss[,vars]
#' reuters <- as_factor(reuters,only_labelled = TRUE,ordered=TRUE)
#'
#' #filter
#' reuters <- reuters %>% filter(YEAR_WEEK %in% c("201642", "201643", "201644"))
#'
#' reuters$date <- as.Date(reuters$INTEND)
#' reuters$MONTH <- format(reuters$date,"%m%Y")
#' reuters$source <- ifelse(is.na(reuters$temp_event)==TRUE,0,reuters$temp_event)
#' reuters$source <- factor(reuters$source,levels=0:2,labels=c("Tracker","Omni","Cortex"))
#' reuters$metro <- ifelse(as.numeric(reuters$QMktSize_13_1) <= 3,1,2)
#' reuters$metro <- factor(reuters$metro,levels=1:2,labels=c('Metro','NonMetro'))
#' reuters$metro2 <- factor(as.numeric(reuters$QMktSize_13_1),levels=1:4,labels=c('Less 1M','1M to 5M','5M+','NonMetro'))
#' reuters$QMktSize_13_1 <- NULL
#'
#' #make sure that unemployment is compatible between CPS and Reuters
#' reuters$employed <- ifelse(as.numeric(reuters$SVQ6)<=4,1,3)
#' reuters$employed <- ifelse(as.numeric(reuters$SVQ6) == 7,2,reuters$employed)
#' reuters$employed <- factor(reuters$employed,levels=1:3,labels=c('Employed','Unemployed','Not in labour force'))
#'
#' ##################################
#' #### Vote
#'
#' reuters$vote <- reuters$TM651Y15_SON_6
#' reuters$lead <- 0
#' reuters$lead <- ifelse(as.numeric(reuters$vote) == 1,-1,reuters$lead)
#' reuters$lead <- ifelse(as.numeric(reuters$vote) == 2,1,reuters$lead)
#' reuters$lead <- ifelse(as.numeric(reuters$vote) == 3,0,reuters$lead)
#'
#' ##########################################
#' ##########################################
#' ###### LV MODEL
#' ##########################################
#' ##########################################
#'
#' betas <- read_sav(file.beta)
#' betas <- as_factor(betas,only_labelled = TRUE,ordered=TRUE)
#'
#' reuters <- left_join(reuters,betas,by=c('STATE','EDU','SEX','AGE_GRP','RACE_','INCOME2','PARTY_ID_'))
#' reuters$votoPD4 <- as.numeric(ifelse(as.numeric(reuters$PD4_NEW_1) > 10,0,reuters$PD4_NEW_1))
#' reuters$lv <- reuters$intercept + reuters$votoPD4 * reuters$betaPD4
#' reuters$lv <- exp(reuters$lv) / (1 + exp(reuters$lv))
#'
#' #early voter - probability 1 for the LV model
#' reuters$lv.force <- ifelse(as.numeric(reuters$EarlyVote2016) %in% 5:7,1,0)
#' reuters$lv <- ifelse(as.numeric(reuters$EarlyVote2016) %in% 5:7,1,reuters$lv)
#'
#' ##########################################
#' ##########################################
#' ###### CPS 2016 June data
#' ##########################################
#' ##########################################
#'
#' ### loading CPS data
#'
#' cps <- read_sav(file.cps)
#' cps <- as_factor(cps,only_labelled = TRUE,ordered=TRUE)
#'
#' vars.cps <- c("HRHHID", "PWSSWGT","GESTCEN", "age_wgts", "edu_wgts", "race_wgts", "income_wgts","sex_wgts","employment_wgts","div9","GTMETSTA","GTCBSASZ")
#' cps <- cps[,vars.cps]
#' cps$STATE <- as.character(cps$GESTCEN)
#' cps$GESTCEN <- NULL
#' cps <- cps %>% rename(employed=employment_wgts,AGE_GRP=age_wgts,EDU=edu_wgts,RACE_=race_wgts,INCOME2=income_wgts,SEX=sex_wgts,GEODIV9=div9)
#' #18+ filter
#' cps <- cps[cps$AGE_GRP != "NaN",]
#' cps <- cps[cps$employed != "NaN",]
#' cps <- droplevels(cps)
#'
#' #recodes / labels to match reuters data
#' aux.age <- names(xtabs(~reuters$AGE_GRP,drop.unused.levels = TRUE))
#' cps$AGE_GRP <- factor(as.numeric(cps$AGE_GRP),levels=1:5,labels=aux.age)
#' aux.edu <- names(xtabs(~reuters$EDU,drop.unused.levels = TRUE))
#' cps$EDU <- factor(as.numeric(cps$EDU),levels=1:3,labels=aux.edu)
#' aux.INC <- names(xtabs(~reuters$INCOME2,drop.unused.levels = TRUE))
#' cps$INCOME2 <- factor(as.numeric(cps$INCOME2),levels=1:4,labels=aux.INC)
#' cps$metro <- ifelse(as.numeric(cps$GTMETSTA) == 1,1,2)
#' cps$metro <- factor(cps$metro,levels=1:2,labels=c('Metro','NonMetro'))
#' cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 2:4,1,4)
#' cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 5:6,2,cps$metro2)
#' cps$metro2 <- ifelse(as.numeric(cps$GTCBSASZ) %in% 7,3,cps$metro2)
#' cps$metro2 <- factor(cps$metro2,levels=1:4,labels=c('Less 1M','1M to 5M','5M+','NonMetro'))
#'
#' #weight targets - national
#' pop.sex <- xtabs(PWSSWGT~SEX,data=cps)
#' pop.age <- xtabs(PWSSWGT~AGE_GRP,data=cps)
#' pop.edu <- xtabs(PWSSWGT~EDU,data=cps)
#' pop.race <- xtabs(PWSSWGT~RACE_,data=cps)
#' pop.geo9 <- xtabs(PWSSWGT~GEODIV9,data=cps)
#' pop.metro <- xtabs(PWSSWGT~metro,data=cps)
#' pop.metro2 <- xtabs(PWSSWGT~metro2,data=cps)
#'
#' #weight targets - STATE vs demos
#' pop.sex.st <- xtabs(PWSSWGT~STATE+SEX,data=cps)
#' pop.age.st <- xtabs(PWSSWGT~STATE+AGE_GRP,data=cps)
#' pop.edu.st <- xtabs(PWSSWGT~STATE+EDU,data=cps)
#' pop.race.st <- xtabs(PWSSWGT~STATE+RACE_,data=cps)
#' pop.geo9.st <- xtabs(PWSSWGT~GEODIV9,data=cps)
#' pop.metro.st <- xtabs(PWSSWGT~STATE+metro,data=cps)
#' pop.metro2.st <- xtabs(PWSSWGT~STATE+metro2,data=cps)
#' #including at least population 1 in each cell
#' pop.metro.st[pop.metro.st == 0] <- 1
#' pop.metro2.st[pop.metro2.st == 0] <- 1
#'
#' ##########################################
#' ##########################################
#' ###### WEIGHTS
#' ##########################################
#' ##########################################
#'
#'
#' rake.chaid <- chaid_raking(cps,reuters,strata='STATE',id.var='RESPID',wgt.pop='PWSSWGT',dep='lead',minbucket = 40,cp = 0.000001)
#'
#' check <- rake.chaid$check
#' df.svy <- rake.chaid$df.svy
#' cells.pop <- rake.chaid$cells.pop
#' trees <- rake.chaid$trees
#' grps.descr <- rake.chaid$grps
#'
#' df.svy <- df.svy %>% rename(weights_chaid=weights)
#'
#' #NATIONAL
#' reuters.wgts <- svydesign(id=~ID_REUTERS,data = df.svy);
#' saida1 <- raking_svy(reuters.wgts, sample=list(~SEX,~AGE_GRP,~EDU,~RACE_,~metro,~GEODIV9), population=list(pop.sex,pop.age,pop.edu,pop.race,pop.metro,pop.geo9), control = list(maxit = 800))
#' pesos1 <- weights(saida1)
#' AAPOR_WEIGHTS <- dim(df.svy)[1] * (pesos1 / sum(pesos1))
#' df.svy$weights_nat <- AAPOR_WEIGHTS
#'
#' #STATE
#' saida2 <- raking_svy(reuters.wgts, sample=list(~STATE+SEX,~STATE+AGE_GRP,~STATE+EDU,~STATE+RACE_,~STATE+metro), population=list(pop.sex.st,pop.age.st,pop.edu.st,pop.race.st,pop.metro.st), control = list(maxit = 800))
#' pesos2 <- weights(saida2)
#' AAPOR_WEIGHTS_STATE <- dim(df.svy)[1] * (pesos2 / sum(pesos2))
#' df.svy$weights_state <- AAPOR_WEIGHTS_STATE
#'
#'
#' ############################################################
#' ############################################################
#' ##########  Checking weight targets
#' ############################################################
#' ############################################################
#'
#' check.dados <- select(df.svy,STATE,SEX,AGE_GRP,EDU,RACE_,GEODIV9,metro,weights_nat,weights_state) %>% gather(var,categ,SEX,AGE_GRP,EDU,RACE_,GEODIV9,metro)
#' check.dados <- check.dados %>% group_by(STATE,var,categ) %>% summarise(new.weights = sum(weights_nat,na.rm = TRUE),state.weights = sum(weights_state,na.rm = TRUE),n = n())
#' check.tot <- check.dados %>% group_by(var,categ) %>% summarise(new.weights = sum(new.weights,na.rm = TRUE),n = sum(n))
#' check.tot <- check.tot %>% group_by(var) %>% mutate(weights = round(100 *new.weights / sum(new.weights))) %>% select(-new.weights)
#' check.tot$STATE <- "US"
#' check.tot$type <- "NATIONAL WEIGHTS"
#' check.dados <- check.dados %>% group_by(STATE,var) %>% mutate(weights = round(100 *state.weights / sum(state.weights))) %>% select(-new.weights,-state.weights)
#' check.dados$type <- "STATE WEIGHTS"
#' check.dados <- bind_rows(check.tot,check.dados)
#'
#' check.cps <- select(cps,STATE,SEX,AGE_GRP,EDU,RACE_,GEODIV9,metro,PWSSWGT) %>% gather(var,categ,SEX,AGE_GRP,EDU,RACE_,GEODIV9,metro,na.rm = TRUE)
#' check.cps <- check.cps %>% group_by(STATE,var,categ) %>% summarise(cps = sum(PWSSWGT))
#' check.cpstot <- check.cps %>% group_by(var,categ) %>% summarise(cps = sum(cps))
#' check.cpstot <- check.cpstot %>% group_by(var) %>% mutate(cps = round(100 * cps / sum(cps)))
#' check.cpstot$STATE <- "US"
#' check.cpstot$type <-"NATIONAL WEIGHTS"
#' check.cps <- check.cps %>% group_by(STATE,var) %>% mutate(cps = round(100 * cps / sum(cps)))
#' check.cps$type <- "STATE WEIGHTS"
#' check.cps <- bind_rows(check.cpstot,check.cps)
#'
#' check.final <- left_join(check.dados,check.cps,by=c('STATE','type','var','categ'))
#' check.final$dif <- round(check.final$weights - check.final$cps,2)
#' check.final <- check.final %>% select(type,STATE,var,categ,n,cps,weights,dif)
#'
#' ##########################################
#' ##########################################
#' ###### Election Results - for benchmarking
#' ##########################################
#' ##########################################
#'
#' ### vote
#'
#' load(file.gen2016)
#' gen.2016 <- results.US2016.gen
#' gen.2016$lead <- 100*((gen.2016$Clinton / rowSums(gen.2016[,-c(1,2)])) - (gen.2016$Trump / rowSums(gen.2016[,-c(1,2)])))
#' gen.2016 <- gen.2016 %>% select(state,lead)
#' gen.2016 <- gen.2016 %>% rename(STATE=state)
#'
#' ### Turnout
#'
#' turn2016 <- read.xlsx(file.turn,sheetIndex = 1)
#' turn2016$turnout <- rowMeans(turn2016[,c('vep','vap')])
#' df.svy <- left_join(df.svy,select(turn2016,STATE,turnout),by='STATE')
#'
#' ############################################################
#' ############################################################
#' ##########  Comparing different weights
#' ############################################################
#' ############################################################
#'
#' #Cut point defined at state level
#' df.reuters <- df.svy  %>% gather(wgts.type,weights,starts_with('weights'))
#' df.reuters <- df.reuters %>% arrange(STATE,wgts.type,-lv)
#' df.reuters <- df.reuters %>% group_by(STATE,wgts.type) %>% mutate(rank.st = cumsum(weights) / sum(weights))
#' df.reuters$cut.lv <- ifelse(df.reuters$rank.st <= df.reuters$turnout,1,0)
#'
#' #filtering LV
#' df.reuters <- df.reuters %>% filter(cut.lv == 1)
#'
#' #vote estimate
#' df.reuters <- df.reuters %>% group_by(STATE,wgts.type) %>% summarise(lead=100*weighted.mean(lead,weights))
#' df.reuters <- df.reuters %>% spread(wgts.type,lead)
#' df.reuters <- left_join(df.reuters,gen.2016,by='STATE')
#' df.reuters <- df.reuters %>% rename(election=lead)
#' df.reuters$dif_chaid <- abs(df.reuters$weights_chaid - df.reuters$election)
#' df.reuters$dif_nat <- abs(df.reuters$weights_nat - df.reuters$election)
#' df.reuters$dif_state <- abs(df.reuters$weights_state - df.reuters$election)
#'
#' avg.error <- df.reuters %>% ungroup() %>% select(starts_with('dif')) %>% summarise_all(funs(mean(.)))
#'
#' df.reuters <- df.reuters %>% select(-starts_with('dif')) %>% gather(estimate,lead,-STATE)
#' df.error <- df.reuters %>% spread(estimate,lead)
#'
#' #################
#' ### graph
#'
#' g <- ggplot(data=df.reuters)
#' g <- g + geom_line(aes(x=STATE,y=lead,group=estimate,colour=estimate),size=1)
#' g
#'
#'
#' #################
#' ### see all trees
#'
#' pdf(paste0(dir,"\\Tree plots from Chaid-Raking.pdf"),paper = 'a4r', width = 12)
#'
#' nada <- map(trees$fit,~prp(.$tree, faclen = 0, cex = 0.8, extra = 1, main=.$cells.svy$strata[[1]]))
#'
#' dev.off()
#'
#'
#' #################
#' ### estados com problemas para checar
#'
#' df.key.states <- df.error %>% filter(STATE %in% c('OH','WI','FL','NC','SC'))
#'
#' # 1.       Ohio
#' # 2.       Wisconsin
#' # 3.       Florida
#' # 4.       North Carolina
#' # 5.       South Carolina
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
