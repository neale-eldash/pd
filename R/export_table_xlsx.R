# require(dplyr)
# require(tidyr)
# library(tableHTML)
# require(xlsx)
#
# dir <- "E:\\DADOS\\CONSULTORIA\\PESSOAS JURÍDICAS\\FlyFrog\\Perception - 10-04-2017"
#
# #tutorial
# #https://lyzander.github.io/tableHTML/
#
# ##########################################
# ##########    Outras opções   ############
# ##########    para tabelas    ############
# ##########################################
#
# #http://stackoverflow.com/questions/3438226/exporting-r-tables-to-html
#
# ##########################################
# ############    tableHTML     ############
# ##########################################
#
# #vignette do tableHTML
# #https://lyzander.github.io/tableHTML/
#
# #formatted tables with superscript
# dados <- mtcars
# dados$mpg[1] <- paste0(dados$mpg[1],'<sup>ABCd</sup>')
# tab1 <- tableHTML(dados, widths = c(140, rep(50, 11)), second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3')), theme = 'rshiny-blue',footer = 'Número superscrito é exemplo de teste de significancia')
# tab2 <- tableHTML(dados, widths = c(140, rep(50, 11)), second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3')), theme = 'scientific',footer = 'Número superscrito é exemplo de teste de significancia')
# tab3 <- tableHTML(dados, widths = c(140, rep(50, 11)), second_header = list(c(3, 4, 5), c('col1', 'col2', 'col3')), theme = 'default',footer = 'Número superscrito é exemplo de teste de significancia')
# write_tableHTML(tab1, file = paste0(dir,'\\tab.html'))
# write_tableHTML(tab1, file = paste0(dir,'\\tab1.html'))
# write_tableHTML(tab2, file = paste0(dir,'\\tab2.html'))
# write_tableHTML(tab3, file = paste0(dir,'\\tab3.html'))
#
#
# ##########################################
# ##############    EXCEL     ##############
# ##########################################
#
# require(RDCOMClient)
#
# xl <- COMCreate("Excel.Application")
# xl[["Visible"]] <- TRUE
# xl[["DisplayAlerts"]] <- FALSE
# #wb <- xl[["Workbooks"]]$Add()
# #sheet <- wb$Worksheets(1)
#
# file.xlsx <- "E:\\DADOS\\CONSULTORIA\\PESSOAS JURÍDICAS\\FlyFrog\\Perception - 10-04-2017\\processamento.xlsx"
# file <- "E:\\DADOS\\CONSULTORIA\\PESSOAS JURÍDICAS\\FlyFrog\\Perception - 10-04-2017\\tab.html"
# file1 <- "E:\\DADOS\\CONSULTORIA\\PESSOAS JURÍDICAS\\FlyFrog\\Perception - 10-04-2017\\tab1.html"
# file2 <- "E:\\DADOS\\CONSULTORIA\\PESSOAS JURÍDICAS\\FlyFrog\\Perception - 10-04-2017\\tab2.html"
# file3 <- "E:\\DADOS\\CONSULTORIA\\PESSOAS JURÍDICAS\\FlyFrog\\Perception - 10-04-2017\\tab3.html"
# files <- c(file1,file2,file3)
#
# df.names <- data.frame(sheet="Initial")
#
# wb = xl$Workbooks()$Open(file)
# sheet <- wb$Sheets(1)
# sheet[["Name"]] <- "Initial"
# #new <- wb$Worksheets()$Add()
#
# cont <- 0
# for (i in files){
#
#   cont <- cont + 1
#
#   #inport html table
#   html = xl$Workbooks()$Open(i)
#
#   #nome plan
#   nome <- paste0("tabela-",cont)
#   df.names <- df.names %>% add_row(sheet=nome)
#
#   #rename sheet
#   html_sheet <- html$Sheets(1)
#   html_sheet[["Name"]] <- nome
#
#   #move sheet to xlsx main file
#   html$Sheets(1)$Move(After=sheet)
#
#   #remove html file
#   file.remove(i)
#
# }
#
# #############################################
# #add hyper-links to tabs
#
# #ActiveSheet.Hyperlinks.Add Anchor:=Selection, Address:="", SubAddress:= "'tabela-1'!A1", TextToDisplay:="tab1"
#
# df.names$nome <- paste0('Link para ',df.names$sheet)
# df.names$adress <- paste0("'",df.names$sheet,"'!A1")
#
# #new <- wb$Worksheets()$Add(Before=sheet)
# #new[["Name"]] <- "Índice"
# tab_indice <- tableHTML(df.names[,-3],theme = 'rshiny-blue',rownames = FALSE)
# write_tableHTML(tab_indice, file = paste0(dir,'\\tab_indice.html'))
# html = xl$Workbooks()$Open(paste0(dir,'\\tab_indice.html'))
# html_sheet <- html$Sheets(1)
# html_sheet[["Name"]] <- "Índice"
# html$Sheets(1)$Move(Before=sheet)
# indice <- wb$sheets("Índice")
# indice$Activate()
#
# for (i in 1:dim(df.names)[1]){
#   local <- indice$Cells(i+1,2)
#   indice$Hyperlinks()$Add(Anchor=local,Address="",SubAddress= df.names$adress[i], TextToDisplay=df.names$nome[i])
# }
#
# wb$SaveAs(Filename=file.xlsx,FileFormat=51)
# wb$Close()
# xl$Quit()
