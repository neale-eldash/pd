#############
## GITHUB
#############

# ## getting your package on github
# #http://kbroman.org/pkg_primer/pages/github.html
#
#1- git remote add origin https://github.com/neale-eldash/pd.git
#2- git push -u origin master

#############
## R package
#############

### Gerar help das funções novamente
#devtools::document()

######################################
##### recode functions

dummy_all <- function(df=NULL,reg.exp=NULL,keep_all=TRUE){
  #' Creates an indicador variable for each category of the selected variables
  #'
  #' This function creates an indicador variable for each category of the selected variables
  #' that belong to classes \emph{factor} or \emph{character}. Variables are selected using a
  #' regular expression. The user can choose to keep all categories or to drop the reference
  #' category. The new indicathor variables are named with the category label/value - only
  #' alpha-numerical characters are kept, and all accents are removed. Empty categories (i.e.
  #' factor levels that are defined but not used) are dropped.
  #'
  #' @param df The dataframe containing the variables to be recoded.
  #' @param reg.exp A \emph{regular expression} identifying the variables that should be analysed.
  #' @param keep_all If \code{TRUE}, then for every category a dummy variable will be created. If
  #' \code{FALSE} then the reference category will be dropped.
  #' @return A \emph{(dataframe)} contaning only the dummy variables that were created.
  #'
  #' @examples
  #' df.dummies <- dummy_all(df,reg.exp='^P[0-9]+$',keep_all=TRUE)
  #'

  #dplyr::selecting variables
  df <- df %>% dplyr::select(matches(reg.exp))
  inds <- sapply(df,class) %in% c("factor","character")
  df <- df[,inds,drop=FALSE]
  if (sum(inds) > 0) warning("Only Factor and Character variables are kept!!!")
  if (sum(inds) == 0) stop("No variable of the specified type's were found in the data frame!")

  #creating names
  df.names <- df %>% tidyr::gather(var,categ)
  df.names <- df.names %>% dplyr::group_by(var,categ) %>% dplyr::summarise(freq=n())
  df.names <- df.names %>% dplyr::group_by(var) %>% dplyr::mutate(n=row_number())
  df.names$orig <- paste0(df.names$var,df.names$categ)
  df.names$orig <- iconv(df.names$orig,from='UTF-8', to='ASCII//TRANSLIT')
  df.names$name <- str_replace_all(df.names$categ,'[^[:alnum:]]','.')
  df.names$name <- str_replace_all(df.names$name,'\\.+','.')
  df.names$name <- str_replace_all(df.names$name,'\\.$','')
  df.names$name <- iconv(df.names$name,from='UTF-8', to='ASCII//TRANSLIT')
  df.names$new <- paste0(df.names$var,".",df.names$n,"_",df.names$name)
  df.names <- df.names %>% ungroup() %>% dplyr::select(orig,new)

  #setting option to include missing in model.matrix
  old.option <- options('na.action')
  options(na.action='na.pass')
  #creating dataframe of dummies
  if (keep_all==TRUE){
    form <- paste0("~ -1 + ",paste(names(df),collapse = " + "))
    form <- as.formula(form)
    df.dummy <- as.data.frame(model.matrix(form,df,contrasts.arg = lapply(df, contrasts, contrasts=FALSE)))
  } else {
    form <- paste0("~ ",paste(names(df),collapse = " + "))
    form <- as.formula(form)
    df.dummy <- as.data.frame(model.matrix(form,df))
    df.dummy <- df.dummy[,-1]
  }
  options(na.action=old.option)

  #dropping empty variables
  inds <- map_lgl(df.dummy,~sum(!(is.na(.))) > 0)
  df.dummy <- df.dummy[,inds]
  names.orig <- iconv(names(df.dummy),to='ASCII//TRANSLIT')
  df.merge <- data.frame(orig=names.orig,order=1:length(names.orig))
  df.merge <- dplyr::left_join(df.merge,df.names,by="orig")
  df.merge <- df.merge %>% dplyr::arrange(order)
  if(sum(is.na(df.merge$new)) > 0) stop("Something went wrong with the df labels!")
  names(df.dummy) <- df.merge$new



  return(df.dummy)

}

######################################
##### summarise functions

df_summary <- function(df,drop=FALSE){
  #' Summarises all variables in a dataframe
  #'
  #' This function calculates the following statistics for all variables in the dataframe:
  #' \itemize{
  #'  \item \strong{var}: Variable name
  #'  \item \strong{name}: Variable label
  #'  \item \strong{class}: Variable class
  #'  \item \strong{valid}: Number of valid cases
  #'  \item \strong{n.na}: Number of \code{NA}'s
  #'  \item \strong{n.distinct}: Number of distinct values
  #' }
  #'
  #' @param df The dataframe to be summarised.
  #' @param drop If \code{droplevels(df)} should be run before summarizing the dataframe.
  #' This can make the orginal dataframe and the one analyzied differ. We
  #' recomend you run \code{droplevels(df)} before calling this function if needed.
  #' @return A \emph{(dataframe)} with the description of all variables.
  #'
  #' @examples
  #' summary <- df_summary(df)
  #'

  if (drop == TRUE){
    df <- droplevels(df)
    warning('Function droplevels was used. Analised DF may be different than you expect!')
  }

  df.sum <- tibble(
    var = names(df),
    name = purrr::map_chr(df,~ifelse(is.null(attr(.,which="label")),NA,attr(.,which="label"))),
    class = purrr::map(df,~class(.)),
    valid = purrr::map_int(df,~sum(!is.na(.))),
    na = purrr::map_int(df,~sum(is.na(.))),
    n.distinct = purrr::map_int(df,~length(unique(.)))
  )

  return(df.sum)

}

tab_summary <- function(df=NULL,reg.exp_lin=NULL,reg.exp_col=NULL,wgt=NULL){
  #' Summary table is created, automatically adding marginal
  #'
  #' This function is still incomplete, for now it only does counts and column total. NEED TO UPDATE!
  #'
  #' @param df A \emph{dataframe} containing the variables to be used in the analysis.
  #' @param reg.exp_lin A \emph{regular expression} identifying the variables that should be used
  #' to create the \emph{lines} of the table.
  #' @param reg.exp_col A \emph{regular expression} identifying the variables that should be used
  #' to create the \emph{columns} of the table.
  #' @param wgt A \emph{string} identifying the weight variable. If \emph{NULL} weights are ignored.
  #' @return A \emph{(dataframe)} with the summary table.
  #' @examples
  #' df.tab <- tab_summary(df=df,reg.exp_lin='^P1[0-4]$',reg.exp_col='^P2[6-9]$',wgt=NULL)
  #'


  df <- as.data.frame(df)
  if (is.null(wgt)){
    df$peso <- 1
  } else {
    df$peso <- df[,wgt]
  }

  df.tab <- df %>% dplyr::select(peso,matches(reg.exp_lin),matches(reg.exp_col)) %>% tidyr::gather(var,categ,matches(reg.exp_lin)) %>% tidyr::gather(seg,grp,matches(reg.exp_col))
  df.tab <- df.tab %>% dplyr::group_by(seg,grp,var,categ) %>% dplyr::summarise(freq=sum(peso,na.rm = TRUE))
  df.tab <- df.tab %>% dplyr::group_by(seg,grp,var) %>% dplyr::mutate(freq=round(100*freq/sum(freq),1))
  df.tab <- df.tab %>% tidyr::unite(grp,seg,grp)
  df.tab <- df.tab %>% tidyr::spread(grp,freq)

  df.tot.lin <- df %>% dplyr::select(peso,matches(reg.exp_lin)) %>% tidyr::gather(var,categ,matches(reg.exp_lin))
  df.tot.lin <- df.tot.lin %>% dplyr::group_by(var,categ) %>% dplyr::summarise(total=sum(peso,na.rm = TRUE))
  df.tot.lin <- df.tot.lin %>% dplyr::group_by(var) %>% dplyr::mutate(total=round(100*total/sum(total),1))

  df.tab <- dplyr::left_join(df.tot.lin,df.tab,by=c("var","categ"))

  return(df.tab)
}

######################################
##### edited View function

#over-ridding utils View function
View_ <- function(df){
  #' Invoke a Data Viewer, but removes list-columns so that R doesn't crash.
  #'
  #' @param df The dataframe to be visualized.
  #' @return Invisible \emph{NULL}. The functions puts up a window and returns immediately: the window can be closed via its controls or menus.
  #'
  #' @examples
  #'
  #' #generate list-column dataframe
  #' df <- tibble(id=1:10,tab=map(1:10,~iris))
  #'
  #' #utils View function
  #' View(df)
  #'
  #' View_(df)

  var.change <- names(df)[purrr:::map_chr(df,~class(.)) == "list"]
  df[,var.change] <- "list (hidden)"
  View(df)

}

#rendering HTML code in RStudio viewer
view_html <- function(html_txt){
  #' render's HTML code in RStudio's Viewer.
  #'
  #' To render's HTML code in RStudio's Viewer, first this function creates a temporary file
  #' and then file is loaded to be displayed in the viewer.
  #'
  #' @param html_txt HTML code to be rendered.
  #' @return Invisible \emph{NULL}. The functions puts up a Viewer window and returns immediately.
  #'
  #' @examples
  #'
  #' #generate html
  #'   df <- tibble(
  #'     id = 1:8,
  #'     info = paste0("info jogo",id),
  #'     nome1 = paste0("time 1 - J",id),
  #'     nome2 = paste0("time 2 - J",id),
  #'     prob1 = paste0("prob 1 - J",id),
  #'     prob2 = paste0("prob 2 - J",id)
  #'   )
  #' html_txt <- pd:::tab_copa_america(df$info,df$nome1,df$nome2,df$prob1,df$prob2)
  #'
  #' #view it in RStudio's Viewer
  #' view_html(html_txt)
  #'

  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "test.html")
  writeLines(as.character(html_txt), htmlFile)
  rstudioapi::viewer(htmlFile)

}

#function used for example in view_html function
tab_copa_america <- function(info,nome1,nome2,prob1,prob2){
  paste0('
         <table style="font-size: 90%; margin:1em 2em 1em 1em;" border="0" cellspacing="0" cellpadding="0">
         <tbody><tr>
         <td height="6">
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f2f2f2" colspan="2">Quartas de final
         </td>
         <td colspan="2">
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f2f2f2" colspan="2">Semifinais
         </td>
         <td colspan="2">
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f2f2f2" colspan="2">Final
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td width="170">&nbsp;
         </td>
         <td width="50">&nbsp;
         </td>
         <td width="20">&nbsp;
         </td>
         <td width="25">&nbsp;
         </td>
         <td width="170">&nbsp;
         </td>
         <td width="50">&nbsp;
         </td>
         <td width="20">&nbsp;
         </td>
         <td width="25">&nbsp;
         </td>
         <td width="170">&nbsp;
         </td>
         <td width="50">&nbsp;
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="2" colspan="2">',info[1],' 27 de junho –<a title="Porto Alegre" href="/wiki/Porto_Alegre">Porto Alegre</a>
         </td>
         <td style="border-width:0 0 1px 0; border-style: solid;border-color:black;" rowspan="4">&nbsp;
         </td>
         <td style="border-width:0 0 1px 0; border-style: solid;border-color:black;" rowspan="7">&nbsp;
         </td>
         <td rowspan="3" colspan="2">
         </td>
         <td style="border-width:0 0 1px 0; border-style: solid;border-color:black;" rowspan="7">&nbsp;
         </td>
         <td style="border-width:0 0 1px 0; border-style: solid;border-color:black;" rowspan="13">&nbsp;
         </td>
         <td rowspan="9" colspan="2">
         </td></tr>
         <tr>
         <td height="6">
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[1],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[1],'</td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="2" colspan="2">',info[5],' 2 de julho –<a title="Belo Horizonte" href="/wiki/Belo_Horizonte">Belo Horizonte</a>
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[1],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[1],'
         </td>
         <td style="border-width:2px 3px 1px 0; border-style: solid;border-color:black;" rowspan="6">&nbsp;
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[5],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[5],'</td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="2" colspan="2">',info[2],' - 28 de junho –<a title="Rio de Janeiro" href="/wiki/Rio_de_Janeiro">Rio de Janeiro</a>
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border-width:2px 0 1px 0; border-style: solid;border-color:black;" rowspan="12">&nbsp;
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[5],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[5],'</td>
         <td style="border-width:2px 3px 1px 0; border-style: solid;border-color:black;" rowspan="12">&nbsp;
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[2],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[2],'</td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="6" colspan="2">
         </td>
         <td rowspan="2" colspan="2">',info[8],' 7 de julho –<a title="Rio de Janeiro" href="/wiki/Rio_de_Janeiro">Rio de Janeiro</a>
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[2],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[2],'</td>
         <td style="border-width:2px 0 1px 0; border-style: solid;border-color:black;" rowspan="6">&nbsp;
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[8],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[8],'
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="2" colspan="2">',info[3],' - 28 de junho –<a title="São Paulo" href="/wiki/S%C3%A3o_Paulo">São Paulo</a>
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border-width:2px 0 0 0; border-style: solid;border-color:black;" rowspan="11">&nbsp;
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[8],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[8],'
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[3],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[3],'</td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="2" colspan="2">',info[6],' 3 de julho –<a title="Porto Alegre" href="/wiki/Porto_Alegre">Porto Alegre</a>
         </td>
         <td rowspan="2" colspan="2">
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[3],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[3],'</td>
         <td style="border-width:2px 3px 1px 0; border-style: solid;border-color:black;" rowspan="6">&nbsp;
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[6],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[6],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f2f2f2" rowspan="2" colspan="2">3º lugar
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="2" colspan="2">',info[4],' - 29 de junho –<a title="Salvador" href="/wiki/Salvador">Salvador</a>
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border-width:2px 0 0 0; border-style: solid;border-color:black;" rowspan="5">&nbsp;
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[6],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[6],'
         </td>
         <td style="border-width:2px 0 0 0; border-style: solid;border-color:black;" rowspan="5">&nbsp;
         </td>
         <td colspan="2">
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[4],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[4],'</td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome1[7],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob1[7],'
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td rowspan="3" colspan="2">
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[4],'</td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[4],'</td>
         <td style="border-width:2px 0 0 0; border-style: solid;border-color:black;" rowspan="2">&nbsp;
         </td>
         <td style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',nome2[7],'
         </td>
         <td align="center" style="border:1px solid #aaa;" bgcolor="#f9f9f9" rowspan="2">',prob2[7],'
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td colspan="2">
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td colspan="8">
         </td>
         <td valign="top" rowspan="2" colspan="2">',info[7],' 6 de julho –<a title="São Paulo" href="/wiki/S%C3%A3o_Paulo">São Paulo</a>
         </td></tr>
         <tr>
         <td height="6">
         </td>
         <td colspan="8">
         </td>
         </tr></tbody>
         </table>                                                                                                                                                                                                                   </td></tr></tbody></table>
         ')}

