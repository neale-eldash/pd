xlsx_table <- function(df,wb,sheet,tit.sep = NA,lin.inic = 1,col.inic = 1,width.first = 30,width.cols = 20,
                       title_font='blue1',title_bg="lightcyan",col1_font='black',col1_bg="lightgrey"){
  #' Generate nice formated tables in excel.
  #'
  #' This function exports a dataframe to a formated table in excel. Format parameters are still hard-coded
  #' in function. Cells of the table title will be merged if needed.
  #'
  #' @param df The \emph{dataframe} to be exported.
  #' @param wb A \emph{workbook} object (created by \code{\link[openxlsx]{createWorkbook}}), identifying where the table will be created.
  #' @param sheet A \emph{intenger} identifying in which sheet index to create the table.
  #' @param tit.sep [Optional] A \emph{regular expression} identifying how variable names and values are separated in
  #' the dataframe title. For example, in "variable_category" the tit.sep parameter would be "_".
  #' @param lin.inic [Optional] A \emph{integer} identifying in which line to insert the table.
  #' @param col.inic [Optional] A \emph{integer} identifying in which column to insert the table.
  #' @param width.first [Optional] A \emph{integer} identifying the width of the first column in the table.
  #' @param width.cols [Optional] A \emph{integer} identifying  the width of the other columns in the table.
  #' @return \emph{NULL}: the table is written to the provided workbook, no object is returned.
  #' @examples
  #'
  #'### carregando dados
  #'data(svy)
  #'
  #'### formatando dados
  #'df = svy
  #'df$pop <- runif(nrow(df))
  #'df <- df %>% gather(var,categ,ends_with('cota'))
  #'df <- df %>% group_by(regiao,var,categ) %>% summarise(pop=sum(pop))
  #'df <- df %>% unite(var,var,categ,sep = "#")
  #'df1 <- df
  #'df <- df %>% spread(var,pop)
  #'
  #'### gerando tabelas
  #'file <- "C:\\tabela_xlsx.xlsx"
  #'wb <- createWorkbook()
  #'addWorksheet(wb, "Exemplos")
  #'# Tabela com 1 linha no título
  #'xlsx_table(df1,wb=wb,sheet=1,lin.inic = 1,col.inic = 1)
  #'# Tabela com 2 linhas no título
  #'xlsx_table(df,wb=wb,sheet=1,tit.sep = "#",lin.inic = 1,col.inic = 5)
  #'saveWorkbook(wb, file, overwrite = TRUE)

  ### estilos

  style_title_top <- createStyle(fontColour=title_font,halign = 'center', valign = 'center', textDecoration = 'bold',
                                 fgFill=title_bg,wrapText = TRUE,border=c('top','bottom','left','right'),
                                 borderStyle = c('medium','medium','medium','medium'))
  style_title_bot <- createStyle(fontColour=title_font,halign = 'center', valign = 'center', textDecoration = 'bold',
                                 fgFill=title_bg,wrapText = TRUE,border=c('top','bottom','left','right'),
                                 borderStyle = c('medium','double','medium','medium'))
  style_fisrt_column <- createStyle(fontColour=col1_font,halign = 'left', valign = 'center', textDecoration = 'bold',
                                    fgFill=col1_bg,wrapText = TRUE, border=c('right'), borderStyle = c('thin'))
  style_last_column <- createStyle(fontColour='black',halign = 'left', valign = 'center', textDecoration = NULL,
                                   wrapText = TRUE, border=c('left'), borderStyle = c('thin'))
  style_data <- createStyle(fontColour='black',halign = 'center', valign = 'center', textDecoration = NULL,
                            wrapText = FALSE)
  style_last_row <- createStyle(fontColour='black',halign = 'center', valign = 'center', textDecoration = NULL,
                                wrapText = FALSE,border=c('top'), borderStyle = c('thin'))

  ### função

  col.fim <- col.inic + ncol(df) - 1
  cols <- length(col.inic:col.fim)

  titulo <- names(df)
  if (length(tit.sep) > 0){
    top <- str_extract(titulo,paste0("^.*(?=",tit.sep,")"))
    bot <- str_extract(titulo,paste0("(?<=",tit.sep,").*$"))
    merge <- is.na(bot)
  } else {
    merge <- rep(TRUE,length(titulo))
  }
  if (sum(merge) == length(merge)){
    writeData(wb, sheet = sheet,df,startRow = lin.inic,startCol = col.inic, colNames = TRUE,rowNames = FALSE)
    addStyle(wb, sheet=sheet,style_title_bot, rows=lin.inic, cols=col.inic:col.fim,gridExpand = TRUE)

    lin.fim <- lin.inic + nrow(df)
    lin.tit <- lin.inic
  } else {
    top[merge] <- titulo[merge]
    names(df) <- bot

    writeData(wb, sheet = sheet,as.data.frame(t(top)),startRow =lin.inic,startCol = col.inic,colNames = FALSE,rowNames = FALSE)
    writeData(wb, sheet = sheet,df,startRow = lin.inic+1,startCol = col.inic,colNames = TRUE,rowNames = FALSE)

    #merging top 2 rows
    pos.merge <- which(merge == TRUE) + col.inic - 1
    walk(pos.merge,~mergeCells(wb, sheet, cols = ., rows = lin.inic:(lin.inic+1)))
    #merging columns from same variable
    runs <- rle(top)
    if (sum(runs$lengths > 1) > 0){
      len <- runs$lengths[runs$lengths > 1]
      fim <- cumsum(runs$lengths)[runs$lengths > 1] + col.inic - 1
      inic <- fim - len + 1
      label <- runs$values[runs$lengths > 1]
      walk2(inic,fim,~mergeCells(wb, sheet, cols =.x:.y, rows = lin.inic))
    }

    addStyle(wb, sheet=sheet,style_title_top, rows=lin.inic, cols=col.inic:col.fim,gridExpand = TRUE)
    addStyle(wb, sheet=sheet,style_title_bot, rows=lin.inic+1, cols=col.inic:col.fim,gridExpand = TRUE)

    lin.fim <- lin.inic + nrow(df) + 1
    lin.tit <- lin.inic + 1
  }

  setColWidths(wb, sheet, cols = col.inic:col.fim, widths = c(width.first,rep(width.cols,cols-1)))

  addStyle(wb, sheet=sheet,style_fisrt_column, rows=(lin.tit+1):lin.fim, cols=col.inic,gridExpand = TRUE)
  addStyle(wb, sheet=sheet,style_data, rows=(lin.tit+1):lin.fim, cols=(col.inic+1):col.fim,gridExpand = TRUE)
  addStyle(wb, sheet=sheet,style_last_row, rows=lin.fim+1, cols=col.inic:col.fim,gridExpand = TRUE)
  addStyle(wb, sheet=sheet,style_last_column, rows=lin.inic:lin.fim, cols=col.fim+1,gridExpand = TRUE)

}

