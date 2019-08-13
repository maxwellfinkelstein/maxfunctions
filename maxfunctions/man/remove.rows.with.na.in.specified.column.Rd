\name{remove.rows.with.na.in.specified.column}
\alias{remove.rows.with.na.in.specified.column}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{remove.rows.with.na.in.specified.column
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Removes rows with NA values in certain columns
}
\usage{
df_to_analyze_without_na<-remove.rows.with.na.in.specified.column(df_to_analyze,
                                                                  columns_with_na=c(x_var,y_var))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df_to_analyze}{
%%     ~~Describe \code{x} here~~
Dataframe from which to remove NAs
}
 \item{columns_with_na}{
%%     ~~Describe \code{x} here~~
A string or vector of strings which are column names of which to remove NAs from
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
