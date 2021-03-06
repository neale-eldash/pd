% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_raking.R
\docType{data}
\name{cps}
\alias{cps}
\title{Base de dados da pesquisa CPS (Current Population Survey) do censo americano.}
\format{
An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 98991 rows and 12 columns.
}
\usage{
cps
}
\description{
Base de dados contendo perfil dos americanos, filtrada para idade maior ou igual a 18 anos.
}
\details{
@format Um dataframe com 104.715 casos e 12 variáveis:
 \describe{
 \item{HRHHID}{id do respondente}
 \item{PWSSWGT}{peso do respondente}
 \item{regiao}{região do país}
 \item{SEX}{sexo do respondente}
 \item{AGE_GRP}{idade do respondente}
 \item{INCOME2}{renda do respondente}
 \item{EDU}{escolaridade do respondente}
 \item{RACE_}{raça do respondente}
 \item{employed}{situação trabalhista do respondente}
 \item{metro}{tamanho cidade do respondente}
 \item{metro2}{tamanho cidade do respondente}
 \item{STATE}{estado do respondente}
 \item{GEODIV9}{divisão9 do respondente}
 }
 @source \url{https://www.census.gov/programs-surveys/cps.html}
}
\keyword{datasets}
