##################################################
##################################################
##################################################
################ Dados
##################################################
##################################################
##################################################

#' Dados da Ambima
#'
#' Base de dados contendo perfil de investidores do Brasil, filtrada para classe AB e idade entre 25 e 70
#'
#'  @format Um dataframe com 967 casos e 6 variáveis:
#'  \describe{
#'  \item{nquest}{id do respondente}
#'  \item{pesoe}{peso do respondente}
#'  \item{regiao}{região do país}
#'  \item{sexo_cota}{sexo do respondente}
#'  \item{idade_cota}{idade do respondente}
#'  \item{classe_cota}{classe social do respondente}
#'  }
#'  @source \url{http://www.anbima.com.br/pt_br/especial/raio-x-do-investidor-2018.htm}
"pop"

#' Pesquisa online com investidores
#'
#' Base de dados contendo perfil dos respondentes de uma pesquisa online, filtrada para classe AB e idade entre 25 e 70
#'
#'  @format Um dataframe com 1032 casos e 5 variáveis:
#'  \describe{
#'  \item{numericalId}{id do respondente}
#'  \item{regiao}{região do país}
#'  \item{sexo_cota}{sexo do respondente}
#'  \item{idade_cota}{idade do respondente}
#'  \item{classe_cota}{classe social do respondente}
#'  }
"svy"

#' Pesquisa eleitoral online
#'
#' Base de dados de pesquisa online sobre intenção de voto nos EUA, filtrada para idade maior ou igual a 18 anos.
#'
#'  @format Um dataframe com 52.368 casos e 14 variáveis:
#'  \describe{
#'  \item{RESPID}{id do respondente}
#'  \item{vote}{intenção de voto declarada}
#'  \item{lead}{variável dummy assumindo valores 1 se o voto for Democrata, -1 se for Republicano e 0 caso contrário.}
#'  \item{lv}{probabilidade estimada de votar na eleição}
#'  \item{regiao}{região do país}
#'  \item{SEX}{sexo do respondente}
#'  \item{AGE_GRP}{idade do respondente}
#'  \item{INCOME2}{renda do respondente}
#'  \item{EDU}{escolaridade do respondente}
#'  \item{RACE_}{raça do respondente}
#'  \item{employed}{situação trabalhista do respondente}
#'  \item{metro}{tamanho cidade do respondente}
#'  \item{metro2}{tamanho cidade do respondente}
#'  \item{STATE}{estado do respondente}
#'  \item{GEODIV9}{divisão9 do respondente}
"svy.vote"

#' Base de dados da pesquisa CPS (Current Population Survey) do censo americano.
#'
#' Base de dados contendo perfil dos americanos, filtrada para idade maior ou igual a 18 anos.
#'
#'  @format Um dataframe com 104.715 casos e 12 variáveis:
#'  \describe{
#'  \item{HRHHID}{id do respondente}
#'  \item{PWSSWGT}{peso do respondente}
#'  \item{regiao}{região do país}
#'  \item{SEX}{sexo do respondente}
#'  \item{AGE_GRP}{idade do respondente}
#'  \item{INCOME2}{renda do respondente}
#'  \item{EDU}{escolaridade do respondente}
#'  \item{RACE_}{raça do respondente}
#'  \item{employed}{situação trabalhista do respondente}
#'  \item{metro}{tamanho cidade do respondente}
#'  \item{metro2}{tamanho cidade do respondente}
#'  \item{STATE}{estado do respondente}
#'  \item{GEODIV9}{divisão9 do respondente}
#'  }
#'  @source \url{https://www.census.gov/programs-surveys/cps.html}
"cps"


##################################################
##################################################
##################################################
################ Amostragem
##################################################
##################################################
##################################################

allocate_pts <- function(df,pts,min.pts){
  #' Distribuir amostra proporcionalmentem, levando em consideração o arredondamento.
  #'
  #' Essa função distribui a amostra proporcionalmente a população, controlando o arredondamento de forma
  #' que o menor erro de arredondamento possível seja cometido. Ela também permite definir um número mínimo
  #' de pontos por estrato.
  #'
  #' @param df \emph{Dataframe} com a população de cada estrato. Nessa base devem haver pelo menos duas
  #'  colunas. Uma chamada strata, com os nomes dos estratos, e outra chamada pop, com a contagem populacional
  #'   de cada estrato. Colunas extra serão ignoradas, porém mantidas.
  #' @param pts \emph{Inteiro} definindo o número de entevistas (ou pontos) a serem distribuídos.
  #' @param min.pts \emph{Inteiro} definindo o número mínimo de casos por estrato.
  #' @return Um \emph{Dataframe} com os dados originais, além do tamanho da amostra e algumas informações
  #' amostrais:
  #' \itemize{
  #'  \item \strong{ordem_show} \emph{(inteiro)}: ordem original dos estratos.
  #'  \item \strong{size.prop} \emph{(real)}: Tamanho relativo de cada estrato.
  #'  \item \strong{pts.prop} \emph{(real)}: Distribuição da amostra proporcional, sem arredondamento.
  #'  \item \strong{pts.arred} \emph{(Inteiro)}: Distribuição da amostra proporcional, com arredondamento.
  #'  \item \strong{pts} \emph{(Inteiro)}: Distribuição da amostra proporcional, com ajustes finais.
  #' }
  #' @examples
  #'
  #' data(svy)
  #' df <- svy %>% group_by(regiao) %>% summarise(pop=n()) %>% rename(strata=regiao)
  #'
  #' # Sem definir um minimo de pontos por estrato
  #' allocate_pts(df,pts = 20,min.pts = 0)
  #'
  #' #Definindo pelo menos 2 pontos por estrato
  #' allocate_pts(df,pts = 20,min.pts = 2)

  df$ordem_show <- 1:nrow(df)
  df.zero <- df %>% select(strata,pop,ordem_show) %>% filter(pop == 0)

  df <- df %>% filter(pop > 0)
  df <- df %>% mutate(
    size.prop = pop/sum(pop),
    pts.prop = round(size.prop*pts,2),
    pts.arred = round(pts.prop,0),
    pts = ifelse(pts.arred < min.pts,min.pts,pts.arred),
    pts.extra = pts-pts.prop
  )

  #aux.n <- ceiling((sum(df$pts) - pts) / min.pts)
  aux.n <- sum(df$pts) - pts
  df$pts.fim <- df$pts
  if (aux.n < 0 & abs(aux.n) <= nrow(df)){

    df <- df %>% arrange(pts.extra)
    df$pts.fim[1:abs(aux.n)] <- df$pts.fim[1:abs(aux.n)] +1

  } else if (aux.n < 0 & abs(aux.n) > nrow(df)){
    #se tem menos linhas do que pontos vai distribuir proporionalmente
    #ao número de pontos sobrando (com relação ao min.pts)
    df$sobra <- df$pts.fim - min.pts
    df$pts.fim <- df$pts.fim + round(aux.n * (df$sobra / sum(df$sobra)),0)
    df$pts.fim[1] <- df$pts.fim[1] + (sum(df$pts.fim) - pts)
    #warning("Alocação pode ter desbalanceado a amostra!")

  } else if (aux.n > 0){

    df <- df %>% arrange(desc(pts.extra))
    df$pts.status <- ifelse(df$pts.fim - 1 >= min.pts,TRUE,FALSE)
    df$acum.status <- cumsum(df$pts.status)

    if (max(df$acum.status) >= aux.n){
      df$pts.fim <- ifelse(df$acum.status <= aux.n & df$pts.status == TRUE,df$pts.fim - 1,df$pts.fim)
    } else if((max(df$acum.status) < aux.n) & max(df$acum.status) > 0) {
      #se tem menos linhas do que pontos vai distribuir proporionalmente
      #ao número de pontos sobrando (com relação ao min.pts)
      df$sobra <- df$pts.fim - min.pts
      df$pts.fim <- df$pts.fim + round(aux.n * (df$sobra / sum(df$sobra)),0)
      df$pts.fim[1] <- df$pts.fim[1] + (sum(df$pts.fim) - pts)
      #warning("Alocação pode ter desbalanceado a amostra!")
    }
  }

  if(nrow(df.zero) > 0){
    df <- df %>% bind_rows(df.zero)
    df <- df %>% mutate_at(vars(-strata),funs(ifelse(is.na(.),0,.)))
  }

  if ((sum((df$pts.fim < min.pts) & (df$pop > 0)) > 0) | (sum(df$pts.fim) > pts)){
    stop("Não foi possível alocar corretamente os pts da amostra!!!")
  }

  df <- df %>% arrange(ordem_show)
  df <- df %>% select(strata:pts.arred,pts.fim,ordem_show,-pts)
  df <- df %>% rename(pts=pts.fim)

  return(df)

}

##################################################
##################################################
##################################################
################ Raking
##################################################
##################################################
##################################################

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
  df.x <- x %>% tidyr::drop_na() %>% tidyr::gather(var,categ) %>% dplyr::group_by(var,categ) %>% dplyr::count() %>% dplyr::rename(n.x=n)
  df.y <- y %>% tidyr::drop_na() %>% tidyr::gather(var,categ) %>% dplyr::group_by(var,categ) %>% dplyr::count() %>% dplyr::rename(n.y=n)
  df <- df.x %>% dplyr::full_join(df.y)
  if (sum(is.na(df$n.y))>0){
    print(df)
    stop("erro de codificação das variáveis. Existem categorias na amostra que não existem na população!")
  }
  return(df)
}

check_targets <- function(x,t){

  df.x <- x %>% tidyr::drop_na() %>% tidyr::gather(var,categ) %>% dplyr::group_by(var,categ) %>% dplyr::count() %>% dplyr::rename(n.x=n)

  if ('cruz' %in% names(t)){
    df.y <- t %>% dplyr::select(-cruz) %>% dplyr::group_by(var,categ) %>% dplyr::summarise(n.y=sum(pop))
  } else {
    df.y <- t %>% dplyr::group_by(var,categ) %>% dplyr::summarise(n.y=sum(pop))
  }

  df <- df.x %>% dplyr::full_join(df.y)
  if (sum(is.na(df$n.y))>0){
    print(df)
    stop("erro de codificação das variáveis. Existem categorias na amostra que não existem na população!")
  }

  return(df)
}

rake_df <- function(df.svy=NA,df.pop=NA,reg.exp.vars=NA,reg.exp.cruz=NA,reg.exp.id=NA,reg.exp.wgts=NA){
  #' Rake sample to match population (Population input is a data frame of population).
  #'
  #' This function rakes the sample to match the population counts. Missing values are excluded from the
  #' raking and get value 1 before normalization. This algorithm has 4 basic steps:
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
  #' @param reg.exp.cruz [Optional] A \emph{string} with the regular expression identifying the variable which the target
  #' variables are crossed by (usually reagion). The target variables will match the population within each label of
  #' the crossing variable. These variables should exist in both the sample and population dataframes.
  #' @param reg.exp.id A \emph{string} with the regular expression identifying the unique id variable. This variable
  #' needs to exist only in the sample dataframe.
  #' @param reg.exp.wgts [Optional] A \emph{string} with the regular expression identifying the population weight variable.
  #' This variable needs to exist only in the population dataframe.
  #' @return A list with three components:
  #' \itemize{
  #'  \item \strong{weights}\emph{(dataframe)}: the original sample dataframe with the weights.
  #'  \item \strong{check.vars}\emph{(dataframe)}: comparison of all variables and labels used.
  #'  \item \strong{check.wgts}\emph{(dataframe)}: comparison of all weights and population totals.
  #' }
  #' @examples
  #'
  #' ###################
  #' ##Example 1
  #' ###################
  #'
  #' # Survey data
  #' data(svy)
  #' # Population data
  #' data(pop)
  #'
  #' ## Raking WITHOUT crossing variable:
  #' weights <- rake_df(df.svy=svy,df.pop=pop,reg.exp.vars="_cota$",reg.exp.cruz=NA,reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")
  #'
  #' ## Raking WITH crossing variable:
  #' weights <- rake_df(df.svy=svy,df.pop=pop,reg.exp.vars="_cota$",reg.exp.cruz="^regiao$",reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")
  #'
  #' ###################
  #' ##Example 2
  #' ###################
  #'
  #'# Survey data
  #'data(svy.vote)
  #'
  #'# Population data
  #'data(cps)
  #'
  #'#creating regular expression that includes all desired variables
  #'#INCOME2 removido por causa de missings
  #'vars_cotas <- c("AGE_GRP", "EDU", "RACE_","SEX", "employed","metro2")
  #'vars_cotas <- paste0("^",vars_cotas,"$")
  #'vars_cotas <- paste(vars_cotas,collapse = "|")
  #'vars_cotas <- paste0("(",vars_cotas,")")
  #'
  #'## Raking WITHOUT crossing variable:
  #'weights <- rake_df(df.svy=svy.vote,df.pop=cps,reg.exp.vars=vars_cotas,reg.exp.id="^RESPID$",reg.exp.wgts="^PWSSWGT$")
  #'
  #'## Raking WITH crossing variable:
  #'weights.cross <- rake_df(df.svy=svy.vote,df.pop=cps,reg.exp.vars=vars_cotas,reg.exp.cruz="^GEODIV9$",reg.exp.id="^RESPID$",reg.exp.wgts="^PWSSWGT$")

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

  check.vars.svy <- names(dplyr::select(df.svy,dplyr::matches(reg.exp.vars)))
  check.vars.pop <- names(dplyr::select(df.pop,dplyr::matches(reg.exp.vars)))
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
    warning("O número de variáveis de cota em cada base diverge.")
  }

  ############
  ### checando variavel cruzamento

  check.cruz.svy <- names(dplyr::select(df.svy,dplyr::matches(reg.exp.cruz)))
  check.cruz.pop <- names(dplyr::select(df.pop,dplyr::matches(reg.exp.cruz)))
  var_cruz <- intersect(check.cruz.svy,check.cruz.pop)
  if (length(var_cruz) == 0){
    warning("Não será utilizada variável de cruzamento.")
  } else if (var_cruz == "") {
    warning("Não será utilizada variável de cruzamento.")
  } else if (length(var_cruz) > 1) {
    stop("Mais de uma variável de cruzamento encontrada na base.")
  } else {
    vars_cota <- setdiff(vars_cota,var_cruz)
  }

  ############
  ### checando id

  var_id <- names(dplyr::select(df.svy,dplyr::matches(reg.exp.id)))
  if (length(var_id) != 1){
    stop("Variável ID não foi corretamente definida.")
  } else if (var_id == "") {
    stop("Variável ID não foi corretamente definida.")
  }
  df.svy[,"id"] <- df.svy[,var_id]
  if (length(unique(svy.vote$RESPID)) < nrow(svy.vote)){
    stop("Variável ID tem duplicidade.")
  }

  ############
  ### checando peso

  var_wgt <- names(dplyr::select(df.pop,dplyr::matches(reg.exp.wgts)))
  if (length(var_wgt) != 1){
    warning("Não será usada variável de ponderação pra definir os targets populacionais.")
  } else if (var_wgt == "") {
    warning("Não será usada variável de ponderação pra definir os targets populacionais.")
  }

  ############
  ### checando categs

  df.categs <- check_categs(dplyr::select(df.svy,dplyr::one_of(vars_cota,var_cruz)),dplyr::select(df.pop,dplyr::one_of(vars_cota,var_cruz)))

  ####################################
  ############  TARGETS
  ####################################

  #adding weights
  if (length(var_wgt) == 1){
    df.pop <- df.pop %>% dplyr::rename(peso=var_wgt)
  } else {
    df.pop$peso <- 1
  }

  #adding cruzamento
  if (length(var_cruz) == 1){
    df.pop <- df.pop %>% dplyr::rename(cruz=var_cruz)
    df.svy <- df.svy %>% dplyr::rename(cruz=var_cruz)
  } else {
    df.pop$cruz <- "Total"
    df.svy$cruz <- "Total"
  }

  #agregating pop dplyr::counts
  targets <- df.pop %>%
    dplyr::select(dplyr::one_of(vars_cota),cruz,peso) %>%
    tidyr::gather(var,categ,dplyr::one_of(vars_cota),na.rm = TRUE) %>%
    dplyr::group_by(cruz,var,categ) %>%
    dplyr::summarise(
      pop = sum(peso,na.rm = TRUE)
    )

  #adjusting for possible uneven NA
  targets <- targets %>% dplyr::group_by(cruz,var) %>% dplyr::mutate(
    pop.tot=sum(pop),
    pop = pop / pop.tot)
  targets <- targets %>% dplyr::group_by(cruz) %>% dplyr::mutate(pop.tot=max(pop.tot))
  targets <- targets %>% ungroup() %>% dplyr::mutate(pop=pop * pop.tot) %>% dplyr::select(-pop.tot)

  ####################################
  ############  PONDERAÇÃO
  ####################################

  wgts <- sort(vars_cota)
  sample <- purrr::map(wgts,~as.formula(paste0('~',.,'+cruz')))

  targets <- targets %>% dplyr::arrange(cruz,var)

  population <- purrr::map(wgts,function(x){
    df.svy <- targets[targets$var == x,]
    df.svy$var <- NULL
    df.svy[,x] <- df.svy$categ
    df.svy$categ <- NULL
    return(df.svy)
  })

  #removing missings
  df.comp <- df.svy %>% dplyr::select(id,cruz,dplyr::one_of(vars_cota)) %>% tidyr::drop_na()
  data.svy <- survey::svydesign(id=~id,data = df.comp);
  data.svy <- raking_svy(data.svy, sample=sample, population=population, control = list(maxit = 800))
  df.comp$weights <- weights(data.svy)
  df.svy <- df.svy %>% dplyr::left_join(dplyr::select(df.comp,id,weights))

  #weight 1 for respondentes with missing (rescaled)
  df.svy$weights <- ifelse(is.na(df.svy$weights),1,df.svy$weights)
  df.svy$weights <- df.svy$weights * (nrow(df.svy) / sum(df.svy$weights))

  ####################################
  ############  CHECKS
  ####################################

  check <- df.svy[,c('weights','cruz',wgts)]
  check <- check %>% tidyr::gather(var,categ,-weights,-cruz) %>% dplyr::group_by(cruz,var,categ) %>% dplyr::summarise(sample=n(),raw=n(),max.wgt = max(weights,na.rm = TRUE),min.wgt = min(weights,na.rm = TRUE),weights=sum(weights,na.rm = TRUE))
  check <- dplyr::full_join(check,targets,by=c('cruz','var','categ'))
  var.tot <- check$var[1]
  check.tot <- check %>% ungroup() %>% dplyr::filter(var == var.tot)
  check <- check %>% dplyr::mutate(
    raw=round(100*raw/sum(raw,na.rm = TRUE),1),
    weights=round(100*weights/sum(weights,na.rm = TRUE),1),
    pop=round(100*pop/sum(pop,na.rm = TRUE),1),
    diff=weights - pop
  )

  #check.tot <- check.tot %>% dplyr::group_by(cruz) %>% dplyr::summarise_at(vars(-cruz,-var,-categ,-ends_with('.wgt')),funs(sum(.,na.rm = TRUE)))
  check.tot <- check.tot %>% dplyr::group_by(cruz) %>% dplyr::summarise_at(vars(-var,-categ,-ends_with('.wgt')),funs(sum(.,na.rm = TRUE)))
  check.tot <- check.tot %>% ungroup() %>% dplyr::mutate(
    raw=round(100*raw/sum(raw,na.rm = TRUE),1),
    weights=round(100*weights/sum(weights,na.rm = TRUE),1),
    pop=round(100*pop/sum(pop,na.rm = TRUE),1),
    diff=weights - pop
  )
  check <- check %>% bind_rows(check.tot)

  check <- check %>% dplyr::select(cruz:raw,weights:diff,ends_with('.wgt'))
  check <- check %>% dplyr::rename(Cruzamento=cruz, Variavel=var, Categoria=categ, Amostra=sample, Sem_Ponderar=raw, Ponderado=weights, Populacao=pop,Diferenca=diff)
  check <- as.data.frame(check)
  check <- check %>% dplyr::filter(Amostra != nrow(df.svy))

  if (length(var_cruz) == 0){
    check$Cruzamento <- NULL
  }

  df.svy <- df.svy %>% dplyr::select(-id,-cruz)
  saida <- list(weights=df.svy,check.vars=df.categs,check.wgts=check)

  return(saida)

}

rake_target <- function(df.svy=NA,targets=NA,reg.exp.vars=NA,reg.exp.cruz=NA,reg.exp.id=NA){
  #' Rake sample to match population (Population input is a data frame of aggregated targets).
  #'
  #' This function rakes the sample to match the population counts. Missing values are excluded from the
  #' raking and get value 1 before normalization. This algorithm has 3 basic steps:
  #' \itemize{
  #'  \item \strong{Check variables}: checks that same variables with same labels are in both dataframes.
  #'  \item \strong{Rake sample}: Uses a adjusted raking algorithm adapted from
  #'  in \code{\link[survey]{rake}}.
  #'  \item \strong{Check weights}: Compares the weights to the population targets to make sure the raking
  #'  worked.
  #' }
  #'
  #' @param df.svy The sample \emph{dataframe}, containing the variables to be used in the analysis (unique id,
  #' targets and cross-variable).
  #' @param targets The population targets \emph{dataframe}, This df should have the following variables:
  #' var (variable names),categ (categories labels), pop (pop dplyr::count) and the optional 'cruz' which identifies
  #'  the crossing variable. Also, if there total pop dplyr::counts are different for each variable, this wont be
  #'  corrected. The df layout is as follows:
  #' \itemize{
  #'  \item \strong{var}: column with the names of the target variables.
  #'  \item \strong{categ}: column with the labels of the categories.
  #'  \item \strong{pop}: population dplyr::count for each combination.
  #'  \item \strong{var_cruz[Optional]}: identifier of each category of the crossing variable. The name of the
  #'  column should match the name of the cross variable in the survey dataframe.
  #' }
  #' @param reg.exp.vars A \emph{string} with the regular expression identifying the target variables (i.e., those
  #' variables that the sample total should match the population total). These variables should exist in both the
  #' sample and population dataframes.
  #' @param reg.exp.cruz [Optional] A \emph{string} with the regular expression identifying the variable which the target
  #' variables are crossed by (usually reagion). The target variables will match the population within each label of
  #' the crossing variable. These variables should exist in both the sample and population dataframes.
  #' @param reg.exp.id A \emph{string} with the regular expression identifying the unique id variable. This variable
  #' needs to exist only in the sample dataframe.
  #' @return A list with three components:
  #' \itemize{
  #'  \item \strong{weights}\emph{(dataframe)}: the original sample dataframe with the weights.
  #'  \item \strong{check.vars}\emph{(dataframe)}: comparison of all variables and labels used.
  #'  \item \strong{check.wgts}\emph{(dataframe)}: comparison of all weights and population totals.
  #' }
  #' @examples
  #' ##load data
  #' # Survey data
  #' data(svy)
  #' # Population data
  #' data(pop)
  #'
  #' ## Raking WITHOUT crossing variable:
  #' targets <- pop %>% dplyr::filter(!is.na(classe_cota),!is.na(idade_cota))
  #' targets <- targets %>% dplyr::select(pesoe,sexo_cota,idade_cota,classe_cota) %>% dplyr::rename(pop=pesoe)
  #' targets <- targets %>% tidyr::gather(var,categ,-pop)
  #' targets <- targets %>% dplyr::group_by(var,categ) %>% dplyr::summarise(pop=sum(pop))
  #' targets <- targets %>% dplyr::filter(is.na(categ) == FALSE)
  #' teste.targets <- rake_target(df.svy=svy,targets=targets,reg.exp.vars="_cota$",reg.exp.cruz=NA,reg.exp.id="^numericalId$")
  #'
  #' ## Raking WITH crossing variable:
  #' targets <- pop %>% dplyr::filter(!is.na(classe_cota),!is.na(idade_cota))
  #' targets <- targets %>% dplyr::select(pesoe,regiao,sexo_cota,idade_cota,classe_cota) %>% dplyr::rename(pop=pesoe)
  #' targets <- targets %>% tidyr::gather(var,categ,-regiao,-pop)
  #' targets <- targets %>% dplyr::group_by(regiao,var,categ) %>% dplyr::summarise(pop=sum(pop))
  #' targets.cruz <- targets %>% dplyr::filter(is.na(categ) == FALSE) %>% ungroup()
  #' teste.targets.cruz <- rake_target(df.svy=svy,targets=targets.cruz,reg.exp.vars="_cota$",reg.exp.cruz="^regiao$",reg.exp.id="^numericalId$")


  reg.exp.vars <- ifelse(is.na(reg.exp.vars)," ",reg.exp.vars)
  reg.exp.cruz <- ifelse(is.na(reg.exp.cruz)," ",reg.exp.cruz)
  reg.exp.id <- ifelse(is.na(reg.exp.id)," ",reg.exp.id)

  ####################################
  ############  CHECKS
  ####################################

  ############
  ### checando bases
  if (!("data.frame" %in% class(df.svy))){
    stop("Missing dataframe with survey data.")
  }
  if (sum(c("var", "categ", "pop") %in% names(targets)) !=3){
    stop("Targets dataframe is not specified correctly.")
  }

  ############
  ### checando variaveis cota
  check.vars.svy <- names(dplyr::select(df.svy,dplyr::matches(reg.exp.vars)))
  check.vars.pop <- unique(targets$var)
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
    warning("O número de variáveis de cota em cada base diverge.")
  }

  ############
  ### checando variavel cruzamento
  check.cruz.svy <- names(dplyr::select(df.svy,dplyr::matches(reg.exp.cruz)))
  check.cruz.pop <- names(dplyr::select(targets,dplyr::matches(reg.exp.cruz)))
  var_cruz <- intersect(check.cruz.svy,check.cruz.pop)
  if (length(var_cruz) == 0) {
    if (length(reg.exp.cruz) > 0){
      if (reg.exp.cruz != " "){
        stop("A variável de cruzamento não foi encontrada!")
      }
    }
  } else if (length(var_cruz) > 1) {
    stop("Mais de uma variável de cruzamento encontrada na base.")
  } else {
    df.svy <- df.svy %>% dplyr::rename(cruz=var_cruz)
    targets <- targets %>% dplyr::rename(cruz=var_cruz)
    vals_df <- unique(df.svy$cruz)
    vals_target <- unique(targets$cruz)
    if(sum(vals_df %in% vals_target) != length(vals_df)){
      stop("Amostra e targets são incompatíveis.")
    }
  }

  #adding cruzamento
  if (length(var_cruz) == 0){
    df.svy$cruz <- "Total"
    targets$cruz <- "Total"
  }
  ############
  ### checando id
  var_id <- names(dplyr::select(df.svy,dplyr::matches(reg.exp.id)))
  if (length(var_id) != 1){
    stop("Variável ID não foi corretamente definida.")
  } else if (var_id == "") {
    stop("Variável ID não foi corretamente definida.")
  }
  df.svy[,"id"] <- df.svy[,var_id]
  if (length(unique(svy.vote$RESPID)) < nrow(svy.vote)){
    stop("Variável ID tem duplicidade.")
  }

  ############
  ### checando categs
  df.categs <- check_targets(dplyr::select(df.svy,dplyr::one_of(vars_cota)),targets)

  ####################################
  ############  PONDERAÇÃO
  ####################################

  wgts <- sort(vars_cota)
  sample <- purrr::map(wgts,~as.formula(paste0('~',.,'+cruz')))
  targets <- targets %>% dplyr::arrange(cruz,var)
  population <- purrr::map(wgts,function(x){
    df.svy <- targets[targets$var == x,]
    df.svy$var <- NULL
    df.svy[,x] <- df.svy$categ
    df.svy$categ <- NULL
    return(df.svy)
  })
  #removing missings
  df.comp <- df.svy %>% dplyr::select(id,cruz,dplyr::one_of(vars_cota)) %>% tidyr::drop_na()
  data.svy <- survey::svydesign(id=~id,data = df.comp);
  data.svy <- raking_svy(data.svy, sample=sample, population=population, control = list(maxit = 800))
  df.comp$weights <- weights(data.svy)
  df.svy <- df.svy %>% dplyr::left_join(dplyr::select(df.comp,id,weights))
  #weight 1 for respondentes with missing (rescaled)
  df.svy$weights <- ifelse(is.na(df.svy$weights),1,df.svy$weights)
  df.svy$weights <- df.svy$weights * (nrow(df.svy) / sum(df.svy$weights))

  ####################################
  ############  CHECKS
  ####################################

  check <- df.svy[,c('weights','cruz',wgts)]
  check <- check %>% tidyr::gather(var,categ,-weights,-cruz) %>% dplyr::group_by(cruz,var,categ) %>% dplyr::summarise(sample=n(),raw=n(),max.wgt = max(weights,na.rm = TRUE),min.wgt = min(weights,na.rm = TRUE),weights=sum(weights,na.rm = TRUE))
  check <- dplyr::full_join(check,targets,by=c('cruz','var','categ'))
  var.tot <- check$var[1]
  check.tot <- check %>% ungroup() %>% dplyr::filter(var == var.tot)
  check <- check %>% dplyr::mutate(
    raw=round(100*raw/sum(raw,na.rm = TRUE),1),
    weights=round(100*weights/sum(weights,na.rm = TRUE),1),
    pop=round(100*pop/sum(pop,na.rm = TRUE),1),
    diff=weights - pop
  )

  #check.tot <- check.tot %>% dplyr::group_by(cruz) %>% dplyr::summarise_at(vars(-cruz,-var,-categ,-ends_with('.wgt')),funs(sum(.,na.rm = TRUE)))
  check.tot <- check.tot %>% dplyr::group_by(cruz) %>% dplyr::summarise_at(vars(-var,-categ,-ends_with('.wgt')),funs(sum(.,na.rm = TRUE)))
  check.tot <- check.tot %>% ungroup() %>% dplyr::mutate(
    raw=round(100*raw/sum(raw,na.rm = TRUE),1),
    weights=round(100*weights/sum(weights,na.rm = TRUE),1),
    pop=round(100*pop/sum(pop,na.rm = TRUE),1),
    diff=weights - pop
  )

  check <- check %>% bind_rows(check.tot)
  check <- check %>% dplyr::select(cruz:raw,weights:diff,ends_with('.wgt'))
  check <- check %>% dplyr::rename(Cruzamento=cruz, Variavel=var, Categoria=categ, Amostra=sample, Sem_Ponderar=raw, Ponderado=weights, Populacao=pop,Diferenca=diff)
  check <- as.data.frame(check)
  check <- check %>% dplyr::filter(Amostra != nrow(df.svy))

  if (length(var_cruz) == 0){
    check$Cruzamento <- NULL
  }
  df.svy <- df.svy %>% dplyr::select(-id,-cruz)

  saida <- list(weights=df.svy,check.vars=df.categs,check.wgts=check)
  return(saida)

}

##################################################
##################################################
##################################################
################ Tree Raking
##################################################
##################################################
##################################################

get_common_vars <- function(df.x,df.y){

  vars <- names(df.x)[names(df.x) %in% names(df.y)]
  return(vars)

}

create_cells_df <- function(df,vars,weight=NA){

  if (is.na(weight) == TRUE){
    df$wgts <- 1
  } else {
    df <- df %>% dplyr::select(dplyr::one_of(vars),dplyr::one_of(weight))
    df$wgts <- df[,weight]
    df[,weight] <- NULL
  }

  df <- df %>% dplyr::group_by_(.dots=vars) %>% dplyr::summarise(wgts=sum(wgts),n=n())
  return(df)

}

replicate_tree <- function (object, newdata,na.action = na.pass){

  df.orig <- newdata
  if (is.null(attr(newdata, "terms"))) {
    Terms <- delete.response(object$terms)
    newdata <- model.frame(Terms, newdata, na.action = na.action,
                           xlev = attr(object, "xlevels"))
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, newdata, TRUE)
  }
  df.orig$grp <- rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
  df.orig
}

extract_names <- function(df.pop,vars){

  df.names <- df.pop %>% dplyr::select(dplyr::one_of(vars),grp,strata)
  df.names <- df.names %>% tidyr::gather(var,categ,-grp,-strata)
  df.names <- df.names %>% dplyr::group_by(strata,grp,var,categ) %>% dplyr::summarise(n=n()) %>% dplyr::select(-n)
  df.names <- df.names[complete.cases(df.names),]
  df.names <- df.names %>% dplyr::group_by(strata,grp,var) %>% do(categs=paste(.$categ,collapse=";"))
  df.names <- df.names %>% tidyr::spread(var,categs)
  df.names <- df.names %>% dplyr::arrange(strata,grp)

  return(df.names)
}

create_tree <- function(cells.svy,indep=NULL,dep=NULL,minbucket = 30,cp = 0.001){

  #options that can be used with rpart
  #to labels and visualize groups, for example

  # bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
  # tree.pruned <- prune(fit, cp = bestcp)
  # prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)
  # plot(tree.pruned); text(tree.pruned)
  # df.tree <- tree.pruned$frame
  # df.grp <- as.data.frame(tree.pruned$where)
  # df.split <- as.data.frame(tree.pruned$splits)
  # df.csplit <- as.data.frame(tree.pruned$csplit)
  # ttt <- rpart.lists(tree.pruned)
  # df.ttt <- rpart.rules(tree.pruned)
  # df.ttt1 <- rpart.rules.table(tree.pruned)
  # df.ttt2 <- rpart.subrules.table(tree.pruned)

  if(any(is.null(vars),is.null(dep))){
    stop("Dependent and Independent variables must be specified!")
  }

  #tree
  frmla <- as.formula(paste(dep,paste(indep,collapse='+'),sep='~'))
  tree <- rpart(frmla,method="anova", data=cells.svy,weights=wgts,control=rpart.control(minbucket = minbucket,cp = cp))

  #prune tree
  # graf <- prp(tree, faclen = 0, cex = 0.8, extra = 1)
  # bestcp <- tree$cptable[which.min(fit.cells$cptable[,"xerror"]),"CP"]
  # fit.cells <- prune(tree, cp = bestcp)
  cells.svy$grp <- tree$where

  saida <- list(cells.svy=cells.svy,tree=tree)
  return(saida)

}

create_tree_grps <- function(df.pop,df.svy,strata=NULL,dep=NULL,wgt.pop=NULL,minbucket = 30,cp = 0.001){

  vars <- get_common_vars(df.svy,df.pop)
  if(length(vars) == 0){
    stop("There are no common variable in the survey and population datasets!")
  }

  #test if strata is in both data sets and create formula
  if (is.null(strata) == FALSE){
    if (!(strata %in% vars)){
      stop("Variable 'strata' has to be in both dataframes!")
    }
    vars <- setdiff(vars,strata)
    df.pop$strata <- df.pop[,strata]
    df.svy$strata <- df.svy[,strata]
  } else {
    df.pop$strata <- 1
    df.svy$strata <- 1
  }
  indep.vars <- vars
  vars <- c(vars,'strata')

  #datasets - cells
  cells.pop <- create_cells_df(df=df.pop,vars=vars,weight=wgt.pop)
  cells.svy <- create_cells_df(df=df.svy,vars=c(vars,dep))

  #generate tree
  fit.cells <- cells.svy %>% dplyr::group_by(strata) %>% do(fit=create_tree(.,indep=indep.vars,dep=dep,minbucket = minbucket,cp = cp))
  cells.svy <- purrr::map_df(fit.cells$fit,'cells.svy')

  cells.pop.fit <- purrr::map2_df(fit.cells$strata,fit.cells$fit,
                                  function(x,y){
                                    df <- cells.pop %>% dplyr::filter(strata == x);
                                    df <- replicate_tree(y$tree,df)
                                  })
  cells.pop <- dplyr::left_join(cells.pop,cells.pop.fit)
  df.svy <- dplyr::left_join(df.svy,select(cells.svy,-wgts,-n),by=c(vars,dep))

  #generate names
  df.names <- extract_names(cells.svy,indep.vars)

  saida <- list(df.svy=df.svy,cells.pop=cells.pop,grps=df.names,trees=fit.cells)
}

chaid_raking <- function(df.pop,df.svy,strata=NULL,id.var=NULL,dep=NULL,wgt.pop=NULL,minbucket = 30,cp = 0.001){
  #' CHAID Rake sample to match population (Population input is a data frame of population).
  #'
  #' This function CHAID rakes the sample to match the population counts. This raking strategy is based
  #' on CHAID trees. The main idea is to run a CHAID tree in the survey data, using a pre-defined dependent
  #' variable (such as voting intention), then using the resulting leafs of the tree as the
  #' cells for raking. This algorithm has 5 basic steps:
  #' \itemize{
  #'  \item \strong{Check variables}: checks that same variables with same labels are in both dataframes.
  #'  \item \strong{CHAID Tree}: Run the tree on the survey data, and create the resulting cells in both
  #'  the survey and population dataframes.
  #'  \item \strong{Population targets}: Calculates the population targets from the population dataframe.
  #'  \item \strong{Rake sample}: Uses a adjusted raking algorithm adapted from
  #'  in \code{\link[survey]{rake}}.
  #'  \item \strong{Check weights}: Compares the weights to the population targets to make sure the raking
  #'  worked.
  #' }
  #'
  #' @param df.svy The sample \emph{dataframe}, containing the variables to be used in the analysis (unique id,
  #' raking variable targets, strata variable and dependent variable to build the tree). Both raking and
  #' strata variables have to exist in both survey and population dataframe. The algorithm checks the existence
  #' of these variables, but does not check that they are coded correctly in both datasets.
  #' @param df.pop The population \emph{dataframe}, containing the variables to be used in the analysis (weights,
  #' raking variable targets and strata variable). Both raking and strata variables have to exist in both survey
  #' and population dataframe. The algorithm checks the existence of these variables, but does not check that
  #' they are coded correctly in both datasets.
  #' @param strata A \emph{string} with the name of the stratifying variable. If this variable is defined, raking
  #' will be performed within each stratum. This variable should exist in both the sample and population dataframes.
  #' @param dep A \emph{string} with the name of the dependent variable to be used in the CHAID analysis. This
  #' variable needs to exist only in the survey dataframe.
  #' @param id.var A \emph{string} with the name of the unique id variable. This variable needs to exist only
  #' in the survey dataframe.
  #' @param wgt.pop A \emph{string} with the name of the weight variable. THis variable will be used to calculate
  #' the population targets. If there is no weight variable in the population dataframe, create a constant variable.
  #' This variable needs to exist only in the population dataframe.
  #' @param minbucket[Optional] A \emph{integer} number representing the minimum number of sample units in each leaf
  #' of the CHAID Tree. Default value is 30.
  #' @param cp[Optional] A \emph{real} number representing the complexity of the CHAID Tree. Default value is 0.001.
  #' @return A list with three components:
  #' \itemize{
  #'  \item \strong{df.svy}\emph{(dataframe)}: the original sample dataframe with the weights.
  #'  \item \strong{cells.pop}\emph{(dataframe)}: the population target cells created by the CHAID Tree.
  #'  \item \strong{check}\emph{(dataframe)}: comparison of all weights and population totals.
  #'  \item \strong{trees}\emph{(dataframe)}: The output from all trees (per stratum).
  #'  \item \strong{grps}\emph{(dataframe)}: Description of all cells created by the algorithm.
  #' }
  #' @examples
  #'
  #' ##load data
  #' # Survey data
  #' data(svy.vote)
  #' # Population data
  #' data(cps)
  #'
  #' ## Raking WITHOUT strata variable:
  #' rake.chaid <- chaid_raking(cps,svy.vote,id.var='RESPID',wgt.pop='PWSSWGT',dep='lead',minbucket = 40,cp = 0.000001)
  #'
  #' ## Raking WITH strata variable:
  #' rake.chaid.strata <- chaid_raking(cps,svy.vote,strata='STATE',id.var='RESPID',wgt.pop='PWSSWGT',dep='lead',minbucket = 40,cp = 0.000001)
  #'
  #' ### save all trees - chaid raking with strata
  #' file <- "C://tree_raking.pdf"
  #' pdf(file,paper = 'a4r', width = 12)
  #' purrr::walk(rake.chaid.strata$trees$fit,~prp(.$tree, faclen = 0, cex = 0.8, extra = 1, main=.$cells.svy$strata[[1]]))
  #' dev.off()

  if(is.null(dep)){
    stop("Dependent variable must be specified!")
  }
  if(is.null(wgt.pop)){
    stop("Weight variable for population dataset must be specified!")
  }
  if(is.null(id.var)){
    stop("The id variable must be specified!")
  }

  ### formating datasets
  df.pop <- as.data.frame(df.pop)
  df.svy <- as.data.frame(df.svy)
  id.var <- as.formula(paste0('~',id.var))

  #tree <- create_tree_grps(pnad,base,dep='Q4_1_dummy',wgt.pop='pop')
  tree <- create_tree_grps(df.pop,df.svy,dep=dep,wgt.pop=wgt.pop,strata=strata,minbucket = minbucket,cp = cp)

  #############################
  #tree output

  df.svy <- tree$df.svy
  cells.pop <- tree$cells.pop

  #############################
  #targets

  targets.pop <- cells.pop %>% ungroup() %>% dplyr::select(strata,grp,wgts) %>% dplyr::filter(is.na(grp)==FALSE)
  targets.pop <- targets.pop %>% dplyr::group_by(strata,grp) %>% dplyr::summarise(wgts=sum(wgts))
  targets.pop <- targets.pop %>% ungroup() %>% dplyr::mutate(wgts=round(100*wgts/sum(wgts),1))
  targets.pop <- as.data.frame(targets.pop)
  pop.tree <- xtabs(wgts~strata+grp,data=cells.pop)

  targets.svy <- df.svy %>% ungroup() %>% dplyr::select(strata,grp) %>% dplyr::filter(is.na(grp)==FALSE)
  targets.svy <- targets.svy %>% dplyr::group_by(strata,grp) %>% dplyr::summarise(n=n())
  missing.grps <- anti_join(targets.svy,targets.pop,by=c('strata','grp'))
  missing.grps <- missing.grps %>% dplyr::select(-n) %>% dplyr::mutate(tira='missing grp')

  #############################
  #giving weight 1 to grps missing in the population
  df.svy <- dplyr::left_join(df.svy,missing.grps,by=c('strata','grp'))

  #############################
  #raking

  data.svy <- survey::svydesign(id=id.var,data = df.svy[is.na(df.svy$tira) == TRUE,]);
  data.svy <- raking_svy(data.svy, sample=list(~strata+grp), population=list(pop.tree), control = list(maxit = 800))
  df.svy$weights <- 1
  df.svy$weights[is.na(df.svy$tira) == TRUE] <- weights(data.svy)
  df.svy$weights <- df.svy$weights * (dim(df.svy)[1] / sum(df.svy$weights))

  #############################
  #checks

  check <- df.svy[,c('weights','strata','grp')]
  check <- check %>% dplyr::group_by(strata,grp) %>% dplyr::summarise(svy.sample=n(),svy.raw=n(),svy.wgts=sum(weights))
  check <- dplyr::full_join(check,dplyr::rename(targets.pop,pop.wgts=wgts),by=c('strata','grp'))
  check <- check %>% ungroup() %>% dplyr::mutate(svy.raw=round(100*svy.raw/sum(svy.raw,na.rm = TRUE),1),svy.wgts=round(100*svy.wgts/sum(svy.wgts,na.rm = TRUE),1))
  check$diff <- check$svy.wgts - check$pop.wgts
  check <- as.data.frame(check)

  saida <- list(df.svy=df.svy,cells.pop=cells.pop,check=check,trees=tree$trees,grps=tree$grps)
  return(saida)

}
