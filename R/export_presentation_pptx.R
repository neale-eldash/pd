# require(neale)
# library(tidyverse)
# require(stringr)
# require(haven)
# require(xlsx)
# library(FactoMineR)
# require(mschart)
# require(officer)
# require(broom)
# options(stringsAsFactors = FALSE)
#
# #dados
# dir <- "E:\\DADOS\\CONSULTORIA\\PESSOAS FÍSICAS\\Paulo Cidade\\Projetos Imagem Institucional + Projeto Selo - 28-11-2017\\Análise Imagem"
# dir.dados <- "E:\\DADOS\\CONSULTORIA\\PESSOAS FÍSICAS\\Paulo Cidade\\Projetos Imagem Institucional + Projeto Selo - 28-11-2017\\Dados Imagem"
# file <- paste0(dir.dados,"\\HAREBR_126488_20171229.sav")
# file.pesos <- paste0(dir.dados,"\\Base Imagem - Ponderado.sav")
#
#
# ########################################
# ###### Funções para o PPT automatizado
# ########################################
#
# ms_graph_corr <- function(df,x,y,label,pts.size=4,title="",xlab="",ylab=""){
#
#   title <- iconv(title,to='ASCII//TRANSLIT')
#   xlab <- iconv(xlab,to='ASCII//TRANSLIT')
#   ylab <- iconv(ylab,to='ASCII//TRANSLIT')
#
#   slide <- ms_scatterchart(data=df_,x=x,y=y, group=label)
#   slide <- chart_settings(slide, vary_colors=FALSE,scatterstyle='marker')
#   slide <- slide %>% chart_data_labels(show_serie_name = TRUE,show_legend_key = FALSE)
#   slide <- slide %>% chart_labels(title = title, xlab = xlab,ylab = ylab)
#   slide <- slide %>% chart_data_fill(values = "#6FA2FF")
#   slide <- slide %>% chart_data_stroke(values = "#6FA2FF")
#   slide <- slide %>% chart_data_size(values = pts.size)
#
#   slide <- slide %>% chart_theme(legend_position = "n",
#                                  legend_text = fp_text(font.size = 8),
#                                  axis_text_x = fp_text(font.size = 8),
#                                  axis_text_y = fp_text(font.size = 8),
#                                  axis_title_x = fp_text(font.size = 8),
#                                  axis_title_y = fp_text(font.size = 8),
#                                  main_title = fp_text(font.size = 8),
#                                  grid_major_line_x = fp_border(color="black", style="solid", width=0),
#                                  grid_major_line_y = fp_border(color="black", style="solid", width=0)
#   )
#
#   #não consegui editar a font size dos data labels
#   #https://stackoverflow.com/questions/48177853/data-label-font-family-and-size-using-mschart-package
#
#   return(slide)
#
# }
#
# add_slide_corr <- function(doc,df,x,y,label,title="",height = 4, width = 8){
#
#   title <- iconv(title,to='ASCII//TRANSLIT')
#
#   chart <- ms_graph_corr(df_,x=x,y=y,label='nome',title="",xlab="",ylab="")
#   doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
#   doc <- ph_with_chart(doc, chart = chart) %>% ph_with_text(type = "title", str = title)
#   #doc <- ph_with_chart_at(doc, chart = chart,top=2,left=0,height = height, width = width) %>% ph_with_text(type = "title", str = title)
#
#   return(doc)
#
# }
#
# add_table <- function(doc,df,title="",height = 4, width = 8){
#
#   title <- iconv(title,to='ASCII//TRANSLIT')
#
#   doc <- doc %>% add_slide(layout = "Title and Content", master = "Office Theme")
#   #doc <- doc %>% ph_with_table(value = df,type = "body",last_row = FALSE, last_column = FALSE, first_row = TRUE) %>% ph_with_text(type = "title", str = title)
#   doc <- doc %>% ph_with_table_at(value = df,top=2,left=2,height = height, width = width,last_row = FALSE, last_column = FALSE, first_row = TRUE) %>% ph_with_text(type = "title", str = title)
#
# }
#
#
# ########################################
# ########################################
# ###### carregando dados e pesos
# ########################################
# ########################################
#
# pesos <- get_spss(file=file.pesos)$sav
# pesos <- pesos %>% select(numericalId,reg:weights)
#
# spss <- get_spss(file=file)
# df <- spss$sav
# vars <- spss$vars
# df.nomes <- vars %>% select(var,name)
#
# df <- df %>% left_join(pesos)
# df$rj_sp <- ifelse(df$int_cidade %in% c('São Paulo','Rio de Janeiro'),1,2)
# df$rj_sp <- factor(df$rj_sp, levels=1:2,labels=c('rj-sp','outros'))
#
# ########################################
# ########################################
# ###### análises
# ########################################
# ########################################
#
# ########################################
# ###### Imagem - P19
# ########################################
#
# # Mapa perceptual (Correspondência). Bateria de imagem Q19.
#
# ### correspondecia
# #http://www.gastonsanchez.com/visually-enforced/how-to/2012/07/19/Correspondence-Analysis/
# #https://rstudio-pubs-static.s3.amazonaws.com/2120_cfbb7161b23e494ea47707f5a12032f1.html
# #http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/
#
# df19 <- df %>% select(numericalId,starts_with('Q19_'),rj_sp,weights)
# df19 <- df19 %>% gather(var,n,-numericalId,-rj_sp,-weights) %>% group_by(var,rj_sp) %>% summarise(
#   n=sum(as.numeric(as.character(n))*weights)
# )
# df19 <- df19 %>% left_join(df.nomes)
# df19 <- df19 %>% separate(name,into=c('inst','frase'),sep = ' - ',remove = TRUE)
# df19$frase <- str_replace(df19$frase,'_.*$','')
# df19$frase <- str_replace(df19$frase,'</br>e ética na maneira como conduz seus relacionamentos','')
# df19$frase <- str_replace(df19$frase,' de </br>interesse geral e das propostas para o país, manifestando claramente suas posições','')
# df19$frase <- str_replace(df19$frase,' de determinadas </br>parcelas da população.*$','')
# df19 <- df19 %>% ungroup() %>% select(-var)
# df19 <- df19 %>% spread(frase,n)
#
# df19.tot <- df19 %>% select(-rj_sp) %>% group_by(inst) %>% summarise_all(funs(sum))
# df19.tot$rj_sp <- 'total'
#
# df19 <- df19 %>% bind_rows(df19.tot)
# df19 <- df19 %>% group_by(rj_sp) %>% nest()
# df19 <- df19 %>% mutate(
#   data=map(data,~column_to_rownames(.,'inst')),
#   ca=map(data,~CA(., graph = FALSE)),
#   coords=map(ca,function(x){
#     rows <- as.data.frame(x$row$coord[,1:2])
#     names(rows) <- c('lin','col')
#     rows <- rows %>% rownames_to_column('nome')
#     cols <- as.data.frame(x$col$coord[,1:2])
#     names(cols) <- c('lin','col')
#     cols <- cols %>% rownames_to_column('nome')
#     ret <- bind_rows(rows,cols)
#     ret$nome <- str_conv(ret$nome,encoding = 'UTF-8')
#     ret$nome <- iconv(ret$nome,from='UTF-8',to='ASCII//TRANSLIT')
#     return(ret)
#   })
# )
# df19$title <- paste0("Análise Correspondecia - Q19 - ",df19$rj_sp)
#
#
# ########################################
# ###### Key Drivers
# ########################################
#
# # Key Drive (Odds) Abrinq: Favorabilidade Abrinq (Q18) X Imagem Abrinq (Q19)
# # Key Drive (Odds) AACD: Favorabilidade AACD (Q18) X Imagem AACD (Q19)
# # Key Drive (Odds) Abrinq: Recomendação Abrinq (Q20) X Imagem Abrinq (Q19)
# # Key Drive (Odds) AACD: Recomendação AACD (Q20) X Imagem AACD (Q19)
# # Key Drive (Odds) Abrinq: Favorabilidade Abrinq (Q18) X Fatos Abrinq (Q23)
# # Key Drive (Odds) Abrinq: Favorabilidade Abrinq (Q18) X Redes sociais (Q37)
#
# aux <- map_df(df %>% select(starts_with('Q19_')),~as.numeric(as.character(.)))
# df <- df %>% select(-starts_with('Q19_')) %>% bind_cols(aux)
#
# #categs.dummy.q18 <- c("Muito favorável","Favorável")
# categs.dummy.q18 <- c("Muito favorável")
#
# key.q18q19.abrinq <- df %>% select(numericalId,Q18_1,matches('Q19_.*[^0-9][1]{1}$'),rj_sp,weights)
# key.q18q19.abrinq <- key.q18q19.abrinq %>% rename(dummy=Q18_1)
# key.q18q19.abrinq$dummy <- ifelse(key.q18q19.abrinq$dummy %in% categs.dummy.q18,1,0)
# names(key.q18q19.abrinq) <- str_replace(names(key.q18q19.abrinq),'\\.[0-9]$','')
# key.q18q19.abrinq$modelo <- 'q18q19_abrinq'
#
# key.q18q19.aacd <- df %>% select(numericalId,Q18_4,matches('Q19_.*[^0-9][4]{1}$'),rj_sp,weights)
# key.q18q19.aacd <- key.q18q19.aacd %>% rename(dummy=Q18_4)
# key.q18q19.aacd$dummy <- ifelse(key.q18q19.aacd$dummy %in% categs.dummy.q18,1,0)
# names(key.q18q19.aacd) <- str_replace(names(key.q18q19.aacd),'\\.[0-9]$','')
# key.q18q19.aacd$modelo <- 'q18q19_aacd'
#
# key.q20q19.abrinq <- df %>% select(numericalId,Q20_1,matches('Q19_.*[^0-9][1]{1}$'),rj_sp,weights)
# key.q20q19.abrinq <- key.q20q19.abrinq %>% rename(dummy=Q20_1)
# key.q20q19.abrinq$dummy <- ifelse(as.numeric(key.q20q19.abrinq$dummy) %in% 1:2,1,0)
# names(key.q20q19.abrinq) <- str_replace(names(key.q20q19.abrinq),'\\.[0-9]$','')
# key.q20q19.abrinq$modelo <- 'q20q19_abrinq'
#
# key.q20q19.aacd <- df %>% select(numericalId,Q20_2,matches('Q19_.*[^0-9][4]{1}$'),rj_sp,weights)
# key.q20q19.aacd <- key.q20q19.aacd %>% rename(dummy=Q20_2)
# key.q20q19.aacd$dummy <- ifelse(as.numeric(key.q20q19.aacd$dummy) %in% 1:2,1,0)
# names(key.q20q19.aacd) <- str_replace(names(key.q20q19.aacd),'\\.[0-9]$','')
# key.q20q19.aacd$modelo <- 'q20q19_aacd'
#
# key <- bind_rows(key.q18q19.abrinq,key.q18q19.aacd,key.q20q19.abrinq,key.q20q19.aacd)
# key.aux <- key
# key.aux$rj_sp <- "total"
# key <- bind_rows(key,key.aux)
# key <- key %>% group_by(modelo,rj_sp) %>% nest()
#
# key.q18q23.abrinq <- df %>% select(numericalId,Q18_1,starts_with('Q23'),rj_sp,weights)
# key.q18q23.abrinq <- key.q18q23.abrinq %>% rename(dummy=Q18_1)
# aux <- map_df(key.q18q23.abrinq %>% select(starts_with('Q23')),~as.numeric(as.character(.)))
# key.q18q23.abrinq <- key.q18q23.abrinq %>% select(-starts_with('Q23')) %>% bind_cols(aux)
# key.q18q23.abrinq$dummy <- ifelse(key.q18q23.abrinq$dummy %in% categs.dummy.q18,1,0)
# key.q18q23.abrinq$modelo <- 'q18q23_abrinq'
# key.aux <- key.q18q23.abrinq
# key.aux$rj_sp <- "total"
# key.q18q23.abrinq <- bind_rows(key.q18q23.abrinq,key.aux)
# key.q18q23.abrinq <- key.q18q23.abrinq %>% group_by(modelo,rj_sp) %>% nest()
#
# key.q18q37.abrinq <- df %>% select(numericalId,Q18_1,starts_with('Q37'),rj_sp,weights)
# key.q18q37.abrinq <- key.q18q37.abrinq %>% rename(dummy=Q18_1)
# aux <- map_df(key.q18q37.abrinq %>% select(starts_with('Q37')),~ifelse(as.numeric(.) <= 2,1,0))
# key.q18q37.abrinq <- key.q18q37.abrinq %>% select(-starts_with('Q37')) %>% bind_cols(aux)
# key.q18q37.abrinq$dummy <- ifelse(key.q18q37.abrinq$dummy %in% categs.dummy.q18,1,0)
# key.q18q37.abrinq$modelo <- 'q18q37_abrinq'
# key.aux <- key.q18q37.abrinq
# key.aux$rj_sp <- "total"
# key.q18q37.abrinq <- bind_rows(key.q18q37.abrinq,key.aux)
# key.q18q37.abrinq <- key.q18q37.abrinq %>% group_by(modelo,rj_sp) %>% nest()
#
# key <- key %>% bind_rows(key.q18q23.abrinq,key.q18q37.abrinq)
# key <- key %>% arrange(modelo,rj_sp)
# key$title <- paste0("Key Drivers - ",key$modelo," - ", key$rj_sp)
#
# form = dummy ~ . - numericalId - weights
# key <- key %>% mutate(
#   model = map(data,~glm(form,family=binomial(link='logit'),data=.,weights=weights)),
#   betas = map(model,~tidy(.))
# )
#
# df.nomes.betas <- df.nomes %>% filter(str_detect(var,'(Q19_[0-9]{1,2}\\.1$|Q23|Q37)'))
# df.nomes.betas$var <- str_replace(df.nomes.betas$var,'(Q19_[0-9]{1,2})(\\.[0-9]+)','\\1')
# df.nomes.betas$name <- str_replace(df.nomes.betas$name,'_.*$','')
# df.nomes.betas$name <- str_replace(df.nomes.betas$name,'- Considerando essa mesma .*$','')
# df.nomes.betas$name <- str_replace(df.nomes.betas$name,'Fundação Abrinq - ','')
# df.nomes.betas$name <- str_replace(df.nomes.betas$name,'</br>e ética na maneira como conduz seus relacionamentos','')
# df.nomes.betas$name <- str_replace(df.nomes.betas$name,' de </br>interesse geral e das propostas para o país, manifestando claramente suas posições','')
# df.nomes.betas$name <- str_replace(df.nomes.betas$name,' de determinadas </br>parcelas da população.*$','')
#
#
# df.betas <- key %>% select(title,betas) %>% unnest()
# df.betas <- df.betas %>% rename(var=term) %>% filter(var != '(Intercept)')
# df.betas$odds <- round(exp(df.betas$estimate),1)
# df.betas <- df.betas %>% arrange(title,desc(odds))
# df.betas <- df.betas %>% left_join(df.nomes.betas)
# df.betas <- df.betas %>% select(title,name,odds) %>% group_by(title) %>% nest()
#
# ########################################
# ########################################
# ###### Gerando o PPT
# ########################################
# ########################################
#
# #opções para o officer
# #https://davidgohel.github.io/officer/articles/powerpoint.html
#
# #inicializando o PPT
# #doc <- read_pptx()
# doc <- read_pptx(paste0(dir,'\\Projeto Imagem.pptx'))
#
# #gravando mapas de correspondencia no PPT
# map2(df19$coords,df19$title,~add_slide_corr(doc,.x,x='lin',y='col',label='nome',title=.y))
#
# #gravando tabelas no PPT
# map2(df.betas$data,df.betas$title,~add_table(doc,.x,title=.y))
#
# #salvando o PPT
# print(doc, target = paste0(dir,"\\Analise Imagem.pptx"))
#
# #arrumar tamanho do titulo do ppt
# #arrumar formatação da tabela
