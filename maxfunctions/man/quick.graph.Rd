\name{quick.graph}
\alias{quick.graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{quick.graph
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This is the main function to quickly draw a graph comparing categorical x groups.

Function requires 3 inputs: x_var, y_var, and df_to_analyze

Function is for categorical x groups - does not work for ordinal x groups.


}
\usage{
For a single iteration:

run.group.stats(df_to_analyze = Analyzing_the_Preliminary_DR_Data_on_RStudio,x_var = "Diagnosis",y_var = "Max.Blue")


For looping through the 4th-7th variable in a dataframe:

p<-list()
for (i in 4:7){
  y_var<-names(Analyzing_the_Preliminary_DR_Data_on_RStudio)[i]
  p[[i-3]]<-quick.graph(df_to_analyze = Analyzing_the_Preliminary_DR_Data_on_RStudio,
                        x_var = "Diagnosis",y_var = y_var)
}
plots_arranged<-do.call(arrangeGrob,c(p,nrow=2))
grid.arrange(plots_arranged)
ggsave(plot=plots_arranged,filename = "/Users/maxwellfinkelstein/Dropbox/SERI/Pupillometry Data/SUMMER 2019 DATA/DataAnalysis/Plots/PLRFeatsByDR.tiff",dpi = 100,height = 7,width = 14)



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
  \item{bars}{
%%     ~~Describe \code{x} here~~
bars defines error bars for parametric graphs: can be "SD", "SE", "CI". Defaults to "SD".
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
