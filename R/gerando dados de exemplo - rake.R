# require(survey)
# require(haven)
# require(tidyverse)
# require(openxlsx)
#
# dir <- "E:\\DADOS\\CONSULTORIA\\PESSOAS FÍSICAS\\Paulo Cidade\\Atitudes Financeiras - 03-12-2018"
# file.data <- paste0(dir,'\\dados\\HAREBR_138180_cNetPanel Publico Geral 20190301.sav')
#
# dir.anbima <- "E:\\DADOS\\CONSULTORIA\\PESSOAS FÍSICAS\\Paulo Cidade\\Atitudes Financeiras - 03-12-2018\\Dados Anbima 2018"
# file.anbima <- paste0(dir.anbima,"\\anexo_publicacao_1.xlsx")
#
# file.spss <- paste0(dir,"\\Base Atitudes Financeiras - Ponderada.sav")
# file.xlsx <- paste0(dir,"\\check ponderação.xlsx")
#
# ####################
# ### Dados da pesquisa
# ####################
#
# dados <- as_factor(read_spss(file.data),only_labelled = TRUE)
#
# dados <- dados %>% mutate(
#   reg = REGIAO,
#   sexo_cota = case_when(
#     F1 == "Homem" ~ 1,
#     F1 == "Mulher" ~ 2
#   ),
#   sexo_cota = factor(sexo_cota,levels=c(1,2),labels=c("Masculino","Feminino")),
#   idade_cota = case_when(
#     F2 == "25_34" ~ 1,
#     F2 == "35_44" ~ 2,
#     F2 == "45_54" ~ 3,
#     F2 == "55_70" ~ 4
#   ),
#   idade_cota = factor(idade_cota,levels=1:4,labels=c("25 a 34 anos", "35 a 44 anos","45 a 54 anos", "55 a 70 anos")),
#   classe_cota = case_when(
#     F4 == "A" ~ 1,
#     F4 %in% c("B1","B2") ~ 2
#   ),
#   classe_cota = factor(classe_cota,levels=1:2,labels=c("A","B"))
# )
#
# # #check filtro
# # ttt <- rowSums(map_df(select(dados,starts_with('Q1#')),~as.numeric(as.character(.))))
# # xtabs(~ttt,addNA = TRUE)
#
#
# ####################
# ### Dados Anbima
# ####################
#
# df <- read.xlsx(file.anbima,sheet = 2)
#
# invest <- c("Caderneta de poupança",
#             "Fundos de investimentos, como Renda Fixa, Multimercado, Fundo cambial, Fundos de ações, RDB etc.",
#             "Plano de previdência privada/ VGBL",
#             "Títulos privados, como Debêntures, CDB, LCI, LCA, LC, Certificados de Operações estruturadas (COE), etc.",
#             #  "Abrir próprio negócio",
#             "Ações na bolsa de valores (ações de empresas)",
#             #  "Consórcio/ carta de crédito",
#             #  "Moedas digitais (Bitcoin)",
#             "Títulos públicos via tesouro direto (pré-fixados, pós-fixados e de inflação/ Selic/ taxa Selic/ tesouro Selic)",
#             #  "Compra e venda de imóveis",
#             "Título de Capitalização/ Pic Itaú/ Pé Quente Bradesco/ Ourocap"
#             #  "Compra e venda de automóveis",
#             #  "Dólar/ compra de dólar",
#             #  "Em casa/ no colchão",
#             #  "Agronegócio (compra e venda de gados)"
# )
#
# base <- df %>% select(nquest,pesoe,regiao,metrop,uf,RCLASSE2,sexo,idade1,pea,escola,escolac,renda,p3a,starts_with('p9'))
# base$invest <- ifelse(base$p9a %in% invest |
#                         base$p9b %in% invest |
#                         base$p9c %in% invest |
#                         base$p9d %in% invest |
#                         base$p9e %in% invest ,1,0)
#
# base <- base %>% mutate(
#   idade_cota = case_when(
#     idade1 %in% 25:34 ~ 1,
#     idade1 %in% 35:44 ~ 2,
#     idade1 %in% 45:54 ~ 3,
#     idade1 %in% 55:70 ~ 4
#   ),
#   idade_cota = factor(idade_cota,levels=1:4,labels=c('25 a 34 anos','35 a 44 anos','45 a 54 anos','55 a 70 anos')),
#   classe_cota = case_when(
#     RCLASSE2 %in% c("CLASSE A") ~ 1,
#     RCLASSE2 %in% c("Classe B2","Classe B1") ~ 2
#   ),
#   classe_cota = factor(classe_cota,levels=1:2,labels=c('A','B'))
# )
#
# svy <- dados %>% dplyr::rename(regiao=REGIAO)
# svy <- svy %>% dplyr::select(numericalId,regiao,ends_with('_cota'))
# pop <- base %>% dplyr::rename(sexo_cota=sexo)
# pop$regiao <- str_replace(pop$regiao,"Centro Oeste","Centro-Oeste")
# pop <- pop %>% dplyr::filter(RCLASSE2 %in% c("CLASSE A","Classe B2","Classe B1"),idade1 %in% 25:70)
# pop <- pop %>% dplyr::select(nquest,pesoe,regiao,ends_with('_cota'))
#
# devtools::use_data(svy,pop)
