% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.R
\name{view_html}
\alias{view_html}
\title{render's HTML code in RStudio's Viewer.}
\usage{
view_html(html_txt)
}
\arguments{
\item{html_txt}{HTML code to be rendered.}
}
\value{
Invisible \emph{NULL}. The functions puts up a Viewer window and returns immediately.
}
\description{
To render's HTML code in RStudio's Viewer, first this function creates a temporary file
and then file is loaded to be displayed in the viewer.
}
\examples{

#generate html
  df <- tibble(
    id = 1:8,
    info = paste0("info jogo",id),
    nome1 = paste0("time 1 - J",id),
    nome2 = paste0("time 2 - J",id),
    prob1 = paste0("prob 1 - J",id),
    prob2 = paste0("prob 2 - J",id)
  )
html_txt <- pd:::tab_copa_america(df$info,df$nome1,df$nome2,df$prob1,df$prob2)

#view it in RStudio's Viewer
view_html(html_txt)

}
