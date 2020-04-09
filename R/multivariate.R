######################################
##### segmentation - hierarquical + kmeans

#atualizar com o purrr e o broom
#https://cran.r-project.org/web/packages/broom/vignettes/kmeans.html

auto_cluster <- function(df=NULL,grps=3:6,name='kmeans',iter.max=100){
  #' Cluster Analysis combining Hierarquical and Kmeans cluster
  #'
  #' This function runs as Cluster Analysis. The first step is to run a Hierarquical Cluster,
  #' and then use centroids as starting point for the Kmeans Cluster. This function outputs the
  #' cluster ids for each line in the dataframe and a summary of the group sizes. Details of the
  #' analysis for each step are:
  #' \itemize{
  #'  \item \strong{Hierarquical}: Uses Euclidean distance and Ward's method. See more details
  #'   in \code{\link[stats]{hclust}}.
  #'  \item \strong{Kmeans}: Uses default options. See more details in \code{\link[stats]{kmeans}}.
  #' }
  #'
  #' @param df A \emph{dataframe} containing the variables to be used in the analysis.
  #' @param grps An \emph{array} or \emph{number} with the number of groups that should be created.
  #' @param name A \emph{string} with the name of the variables that will be created.
  #' @param iter.max A \emph{number} indicating the maximum number of iterations for the Kmeans cluster.
  #' @return A list with three components:
  #' \itemize{
  #'  \item \strong{grps}\emph{(dataframe)}: with the variables identifying the cluster each observation
  #'  belongs too.
  #'  \item \strong{summary}\emph{(dataframe)}: summary of the number of observations per cluster.
  #'  \item \strong{details}\emph{(dataframe)}: details for each iteration of the kmeans algorithm.
  #' }
  #' @examples
  #' df.cluster <- auto_cluster(df=df,grps=3:6,name='kmeans')
  #'

  df_ <- df

  details <- tibble(
    grupos=grps,
    tss = NA,
    details = NA
  )

  d <- dist(df, method = "euclidean")
  hrq <- hclust(d, method="ward")

  for (i in grps){
    grp <- cutree(hrq, k=i)
    aux.df <- cbind(grp,df)
    aux.df <- aux.df %>% dplyr::group_by(grp) %>% dplyr::summarise_each(funs(mean))
    aux <- as.matrix(aux.df[,-1])

    #Cluster kmeans
    cluster <- kmeans(df, aux, iter.max = iter.max)
    details$tss[details$grupos==i] <- cluster$tot.withinss
    details$details[details$grupos==i] <- list(cluster)
    df_[,paste0(name,i)] <- cluster$cluster
  }

  df_ <- df_ %>% dplyr::select(starts_with(name))
  df.seg <- df_ %>% tidyr::gather(var,seg) %>% dplyr::group_by(var,seg) %>% dplyr::summarise(n=n())
  df.seg <- as.data.frame(df.seg %>% tidyr::spread(var,n))

  return(list(grps=df_,summary=df.seg,details=details))
}

######################################
##### Factor analysis

factor_analysis <- function(df=NULL,n.fat=NULL,name="fator",sep=".",rotation="varimax",scores="regression",cut=0.2){
  #' Factor Analysis choosing number of factors automatically
  #'
  #' This function runs as Factor Analysis. The first step is to calculate the number of factors
  #' automatically. Once the number of factor is choosen, the Factor Analysis is run, and both
  #' the estimated factor variables and the loadings are calculated. Missing values aren't aloud. Details
  #' of the analysis for each step are:
  #' \itemize{
  #'  \item \strong{# of Factors}: Calculates the number of eigenvalues in the correlation matrix that
  #'  are larger then 1. The idea is that if the variance of a standardized variable is 1, then a factor
  #'  should only be maintained if it's variance is at least one. See more details in \code{\link[base]{eigen}}.
  #'  \item \strong{Factor Analysis}: The default options are \code{rotation='varimax'} and
  #'  \code{scores='regression'}. See more details in \code{\link[stats]{factanal}}.
  #' }
  #'
  #' @param df A \emph{dataframe} containing the variables to be used in the analysis.
  #' @param n.fat A \emph{number} with the number of factors to use. if \emph{NULL} then the
  #' number of factors is calculated automatically.
  #' @param name A \emph{string} with the name of the factor variables that will be created.
  #' @param sep A \emph{string} with the character to be used separating name of the factor
  #' from the number of the factor
  #' @param rotation A \emph{string} identifying the type of rotation to be performed.
  #' @param scores A \emph{string} identifying the type of scores to be extracted.
  #' @param cut A \emph{number} indicating the minimum size of factor loadings that should be kept.
  #' Loadings smaller then \emph{cut} will be dropped.
  #' @return A list with two components:
  #' \itemize{
  #'  \item \strong{factors}\emph{(dataframe)}: with the variables with the scores extrated from
  #'   the factor analysis.
  #'  \item \strong{loadings}\emph{(dataframe)}: with the extracted factor loadings.
  #'  \item \strong{details}\emph{(list)}: with all of the information returned from factanal.
  #' }
  #' @examples
  #' df_factan <- factor_analysis(df=df,cut=0.2)
  #'

  #number of factors
  if (is.null(n.fat)){
    n.fat <- eigen(cor(df,use = "pairwise.complete.obs"))
    n.fat <- sum(n.fat$values >= 1)
  }

  #if there are missings
  #cov.mat <- cov(df[,-1],use = "pairwise.complete.obs")
  #fit <- factanal(x=df[,-1], factors=n.fat, covmat=cov.mat, rotation="varimax",scores="regression")

  fit <- factanal(x=df, factors=n.fat, rotation=rotation,scores=scores)

  df.scores <- as.data.frame(fit$scores)
  names(df.scores) <- paste0(name,sep,1:n.fat)

  load <- as.data.frame(fit$loadings[])
  names(load) <- paste0(name,sep,1:n.fat)
  load$fator <- apply(abs(load),1,which.max)
  load$max <- apply(abs(load[,-dim(load)[2]]),1,max)
  load$var <- row.names(load)
  load <- load %>% tidyr::gather(factor,loading,-var,-fator,-max)
  load$loading <- ifelse(abs(load$loading) <= cut,NA,load$loading)
  load <- load %>% tidyr::spread(factor,loading)
  load <- load %>% dplyr::arrange(fator,-max)
  load <- load[,c('var','fator',paste0(name,sep,1:n.fat))]

  return(list(factors=df.scores,loadings=load,details=fit))
}


######################################
##### Correspondence Analysis
#
# library(mschart)
# library(officer)
# library(magrittr)
# require(ggplot2)
#
# mytheme <- mschart_theme(legend_position='n')
# my_barchart <- ms_barchart(data = browser_data,x = "browser", y = "value", group = "serie")
#
# doc <- read_pptx()
# #doc <- set_theme(doc, mytheme)
# doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
# doc <- ph_with_chart(doc, chart = my_barchart)
#
# my_barchart2 <- chart_settings( my_barchart, grouping = "stacked", gap_width = 50, overlap = 100 )
#
# doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
# doc <- ph_with_chart(doc, chart = my_barchart2)
#
# df <- mtcars
# df$nome <- row.names(mtcars)
#
# sc_01 <- ms_scatterchart(data =df, x = "disp",y = "drat", group = "nome")
# #sc_01 <- chart_ax_x(sc_01, cross_between = "midCat")
# sc_01 <- chart_settings(sc_01, vary_colors=FALSE,scatterstyle='marker')
# sc_01 <- sc_01 %>% chart_data_labels(show_serie_name = TRUE,show_legend_key = FALSE)
# sc_01 <- sc_01 %>% chart_theme(legend_position = "n",
#                                legend_text = fp_text(font.size = 8),
#                                main_title = fp_text(font.size = 8)
# )
# sc_01 <- sc_01 %>% chart_data_fill(values = "#6FA2FF")
# sc_01 <- sc_01 %>% chart_data_stroke(values = "#6FA2FF")
# sc_01 <- sc_01 %>% chart_data_size(values = 4)
# sc_01 <- sc_01 %>% chart_data_line_width(values = 5)
#
# sc_01 <- sc_01 %>% chart_labels(title = "Correspondencia", xlab = "Dim x",
#                                 ylab = "Dim y")
#
# doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
# doc <- ph_with_chart(doc, chart = sc_01)
#
# doc <- doc %>% add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_table(value = mtcars[1:6,], type = "body",
#                 last_row = FALSE, last_column = FALSE, first_row = TRUE)
#
# doc <- add_slide(doc, layout = "Title and Content",
#                  master = "Office Theme")
#
# gg_plot <- ggplot(data = iris ) +
#   geom_point(mapping = aes(Sepal.Length, Petal.Length), size = 3) +
#   theme_minimal()
#
# doc <- ph_with_gg_at(doc, value = gg_plot,height = 4, width = 8, left = 1, top = 1 )
#
#
# print(doc, target = "E:\\DADOS\\CONSULTORIA\\SLEEK DATA\\Ipsos USA\\Alabama Special Election - 27-11-2017\\Results\\teste.pptx")
