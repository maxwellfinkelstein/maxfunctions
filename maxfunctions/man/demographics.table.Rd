\name{demographics.table}
\alias{demographics.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{demographics.table
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

This function creates demographics/description tables of variables.

Does not automatically add p-values/show significance.

Returns the demographics table as a dataframe.

}
\usage{
table<-demographics.table(df_to_analyze = subset(DiabetesDataFinal,DiagnosisDR=="DR"),
                           x_var = "Diagnosis",
                           y_vars = names(DiabetesDataFinal)[16:21])

require(openxlsx)
write.xlsx(table,"Dropbox/SERI/Pupillometry Data/Summer 2020 Analyses/Processed Data/DiabetesDataDemographicsTable4.xlsx")



}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df_to_analyze}{
%%     ~~Describe \code{x} here~~
df_to_analyze is the data frame in tidy, wide (normal) format.
}
  \item{x_var}{
%%     ~~Describe \code{x} here~~
x_var is the name of the x variable in quotes, eg 'Diagnosis'
}
  \item{y_vars}{
%%     ~~Describe \code{x} here~~
vector of chr names of columns in df_to_analyze with demographics to analyze
}
  \item{sd}{
%%     ~~Describe \code{x} here~~
Used for numeric y_vars; If 1 displays mean (sd), if 2 displays median (IQR), if 3 (default) then it assesses the data and chooses automatically. Else error.

}


}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
