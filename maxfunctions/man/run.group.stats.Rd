\name{run.group.stats}
\alias{run.group.stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{run.group.stats
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

This is the main function to quickly calculate statistics relating categorical x groups.
Function requires 3 inputs: x_var, y_var, and df_to_analyze
Function is for categorical x groups - does not work for ordinal x groups

Tests default to two sided tests for difference, rather than tests for greater/lesser.
Model assessments are not conducted, and detailed modelling should be done manually.

If repeated measures data is in wide format (one column for first measure and separate column for
next measure), select columns and run the command below to put all measures in the same column
df_to_analyze<-gather(Analyzing_the_Preliminary_DR_Data_on_RStudio,'Key','Name of y Variable',c(8,10)) , eg for columns 8 and 10
This is required for the stats function to work; check the resulting df and column classes,
then set x_var='Key' and y_var='Name of y Variable'

}
\usage{
For a single iteration:

run.group.stats(df_to_analyze = Analyzing_the_Preliminary_DR_Data_on_RStudio,x_var = "Diagnosis",y_var = "Max.Blue")


For looping through the 3rd-48th variable in a dataframe:

for (i in 3:48) {
  out<-capture.output(run.group.stats(df_to_analyze = EYC,x_var = "Order",y_var = names(EYC)[i],group_dependence="Eye"))
  cat(out, file="Dropbox/SERI/Pupillometry Data/SUMMER 2019 DATA/DataAnalysis/Stats Output/summary_of_prelim_diabetes_stats.txt", sep="\n", append=TRUE)
}


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
  \item{y_var}{
%%     ~~Describe \code{x} here~~
y_var is the name of the y variable in quotes, eg 'Maximum Constriction to Blue Light'
}
  \item{group_dependence}{
%%     ~~Describe \code{x} here~~
group_dependence is either FALSE (default) or the name of column grouping/relating the observations, which would be the 'Subject' or 'ID' column for repeated measures.

}
  \item{y_order}{
%%     ~~Describe \code{x} here~~
y_order is used for ordinal dependent variables. Defaults to FALSE. Specify as TRUE if the ordinal dependent variable is already an ordered factor, else specify ascending order as a vector using c().
}
  \item{dependence_is_random_effect}{
%%     ~~Describe \code{x} here~~
dependence_is_random_effect is used when specifying group_dependence. Defaults to FALSE, ie for a fixed effect group_dependence. Specify TRUE if group_dependence is a random effect, eg for repeated measures.

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
