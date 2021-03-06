% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_raking.R
\name{rake_df}
\alias{rake_df}
\title{Rake sample to match population (Population input is a data frame of population).}
\usage{
rake_df(
  df.svy = NA,
  df.pop = NA,
  reg.exp.vars = NA,
  reg.exp.cruz = NA,
  reg.exp.id = NA,
  reg.exp.wgts = NA
)
}
\arguments{
\item{df.svy}{The sample \emph{dataframe}, containing the variables to be used in the analysis (unique id,
targets and cross-variable).}

\item{df.pop}{The population \emph{dataframe}, containing the variables to be used in the analysis (weights,
targets, cross-variable).}

\item{reg.exp.vars}{A \emph{string} with the regular expression identifying the target variables (i.e., those
variables that the sample total should match the population total). These variables should exist in both the
sample and population dataframes.}

\item{reg.exp.cruz}{[Optional] A \emph{string} with the regular expression identifying the variable which the target
variables are crossed by (usually reagion). The target variables will match the population within each label of
the crossing variable. These variables should exist in both the sample and population dataframes.}

\item{reg.exp.id}{A \emph{string} with the regular expression identifying the unique id variable. This variable
needs to exist only in the sample dataframe.}

\item{reg.exp.wgts}{[Optional] A \emph{string} with the regular expression identifying the population weight variable.
This variable needs to exist only in the population dataframe.}
}
\value{
A list with three components:
\itemize{
 \item \strong{weights}\emph{(dataframe)}: the original sample dataframe with the weights.
 \item \strong{check.vars}\emph{(dataframe)}: comparison of all variables and labels used.
 \item \strong{check.wgts}\emph{(dataframe)}: comparison of all weights and population totals.
}
}
\description{
This function rakes the sample to match the population counts. Missing values are excluded from the
raking and get value 1 before normalization. This algorithm has 4 basic steps:
\itemize{
 \item \strong{Check variables}: checks that same variables with same labels are in both dataframes.
 \item \strong{Population targets}: Calculates the population targets from the population dataframe.
 \item \strong{Rake sample}: Uses a adjusted raking algorithm adapted from
 in \code{\link[survey]{rake}}.
 \item \strong{Check weights}: Compares the weights to the population targets to make sure the raking
 worked.
}
}
\examples{

###################
##Example 1
###################

# Survey data
data(svy)
# Population data
data(pop)

## Raking WITHOUT crossing variable:
weights <- rake_df(df.svy=svy,df.pop=pop,reg.exp.vars="_cota$",reg.exp.cruz=NA,reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")

## Raking WITH crossing variable:
weights <- rake_df(df.svy=svy,df.pop=pop,reg.exp.vars="_cota$",reg.exp.cruz="^regiao$",reg.exp.id="^numericalId$",reg.exp.wgts="^pesoe$")

###################
##Example 2
###################

# Survey data
data(svy.vote)

# Population data
data(cps)

#creating regular expression that includes all desired variables
#INCOME2 removido por causa de missings
vars_cotas <- c("AGE_GRP", "EDU", "RACE_","SEX", "employed","metro2")
vars_cotas <- paste0("^",vars_cotas,"$")
vars_cotas <- paste(vars_cotas,collapse = "|")
vars_cotas <- paste0("(",vars_cotas,")")

## Raking WITHOUT crossing variable:
weights <- rake_df(df.svy=svy.vote,df.pop=cps,reg.exp.vars=vars_cotas,reg.exp.id="^RESPID$",reg.exp.wgts="^PWSSWGT$")

## Raking WITH crossing variable:
weights.cross <- rake_df(df.svy=svy.vote,df.pop=cps,reg.exp.vars=vars_cotas,reg.exp.cruz="^GEODIV9$",reg.exp.id="^RESPID$",reg.exp.wgts="^PWSSWGT$")
}
