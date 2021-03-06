% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_raking.R
\name{allocate_pts}
\alias{allocate_pts}
\title{Distribuir amostra proporcionalmentem, levando em consideração o arredondamento.}
\usage{
allocate_pts(df, pts, min.pts)
}
\arguments{
\item{df}{\emph{Dataframe} com a população de cada estrato. Nessa base devem haver pelo menos duas
colunas. Uma chamada strata, com os nomes dos estratos, e outra chamada pop, com a contagem populacional
 de cada estrato. Colunas extra serão ignoradas, porém mantidas.}

\item{pts}{\emph{Inteiro} definindo o número de entevistas (ou pontos) a serem distribuídos.}

\item{min.pts}{\emph{Inteiro} definindo o número mínimo de casos por estrato.}
}
\value{
Um \emph{Dataframe} com os dados originais, além do tamanho da amostra e algumas informações
amostrais:
\itemize{
 \item \strong{ordem_show} \emph{(inteiro)}: ordem original dos estratos.
 \item \strong{size.prop} \emph{(real)}: Tamanho relativo de cada estrato.
 \item \strong{pts.prop} \emph{(real)}: Distribuição da amostra proporcional, sem arredondamento.
 \item \strong{pts.arred} \emph{(Inteiro)}: Distribuição da amostra proporcional, com arredondamento.
 \item \strong{pts} \emph{(Inteiro)}: Distribuição da amostra proporcional, com ajustes finais.
}
}
\description{
Essa função distribui a amostra proporcionalmente a população, controlando o arredondamento de forma
que o menor erro de arredondamento possível seja cometido. Ela também permite definir um número mínimo
de pontos por estrato.
}
\examples{

data(svy)
df <- svy \%>\% group_by(regiao) \%>\% summarise(pop=n()) \%>\% rename(strata=regiao)

# Sem definir um minimo de pontos por estrato
allocate_pts(df,pts = 20,min.pts = 0)

#Definindo pelo menos 2 pontos por estrato
allocate_pts(df,pts = 20,min.pts = 2)
}
