# Instructions ------------------------------------------------------------
"
Function is for categorical x groups - does not work for ordinal x groups
Function requires 3 inputs: x_var, y_var, and df_to_analyze
If unspecified, both group_dependence and y_order default to FALSE
df_to_analyze is the data frame in tidy, wide (normal) format
x_var is the name of the x variable in quotes, eg 'Diagnosis'
y_var is the name of the y variable in quotes, eg 'Maximum Constriction to Blue Light'
group_dependence is either FALSE or the name of column grouping the observations, which would be
the 'Subject' or 'ID' column for repeated measures
y_order is used for ordinal dependent variables. Defaults to FALSE. Specify as TRUE if the ordinal
dependent variable is already an ordered factor, else specify ascending order as a vector using c().
dependence_is_random_effect is used when specifying group_dependence. Defaults to FALSE, ie for a fixed
effect group_dependence. Specify TRUE if group_dependence is a random effect, eg for repeated measures.

Tests default to two sided tests for difference, rather than tests for greater/lesser.
Model assessments are not conducted, and detailed modelling should be done manually.

If repeated measures data is in wide format (one column for first measure and separate column for
next measure), select columns and run the command below to put all measures in the same column
df_to_analyze<-gather(Analyzing_the_Preliminary_DR_Data_on_RStudio,'Key','Name of y Variable',c(8,10)) eg columns 8 and 10
above is required for the stats function to work; check the resulting df and column classes,
then set x_var='Key' and y_var='Name of y Variable'
"

#Analyzing_the_Preliminary_DR_Data_on_RStudio$`Fake Height`<-as.factor(Analyzing_the_Preliminary_DR_Data_on_RStudio$`Fake Height`)
#run.group.stats(df_to_analyze = Analyzing_the_Preliminary_DR_Data_on_RStudio,x_var = "Diagnosis",y_var = "Max.Blue",group_dependence = "Fake HbA1c Status")

# for (i in 3:48) {
#   out<-capture.output(run.group.stats(df_to_analyze = EYC,x_var = "Order",y_var = names(EYC)[i],group_dependence="Eye"))
#   cat(out, file="summary_of_myopia_stats.txt", sep="\n", append=TRUE)
# }

#run.group.stats(df_to_analyze = Analyzing_the_Preliminary_DR_Data_on_RStudio,x_var = "Diagnosis",y_var = "Max.Blue")

# run.group.stats ---------------------------------------------------------


#Major Stats Function
run.group.stats<-function(df_to_analyze,x_var,y_var,group_dependence=FALSE,y_order=FALSE,dependence_is_random_effect=FALSE){
  #Starts the output with intro; useful for looping
  cat(paste("","","","",sep="\n"))
  print(paste("x variable:",x_var),quote=FALSE)
  print(paste("y variable:",y_var),quote=FALSE)
  cat(paste("","",sep="\n"))

  #Makes the x_variable a character/group - important if groups are eg 1, 2, 3, 4
  x_levels<-levels(df_to_analyze[[x_var]])
  df_to_analyze[[x_var]]<-as.character(df_to_analyze[[x_var]])
  df_to_analyze_without_na<-remove.rows.with.na.in.specified.column(df_to_analyze,c(x_var,y_var))
  number_of_unique_x_groups<-count.types.of.x.groups(df_to_analyze_without_na,x_var)

  is_ordered<-c()
  for (i in 1:length(y_order)) {
    is_ordered[i]<-y_order[i]!=FALSE
  }

  checking_if_already_ordered<-c()
  for (i in 1:length(y_order)) {
    checking_if_already_ordered[i]<-y_order[i]==TRUE
  }

  #Tests if y_var is categorical or numerical and then defines the stats function
  if(any(is_ordered)==TRUE){
    stats_to_run<-define.group.stats.for.numerical.y.variable(df_to_analyze_without_na,x_var,y_var,
                                          group_dependence,number_of_unique_x_groups)
  }else if(class(df_to_analyze_without_na[[y_var]])=="character"|class(df_to_analyze_without_na[[y_var]])=="factor"){
    stats_to_run<-define.group.stats.for.categorical.y.variable(df_to_analyze_without_na,x_var,y_var,
                                          group_dependence,number_of_unique_x_groups)
  }else if(class(df_to_analyze_without_na[[y_var]])=="numeric"){
    stats_to_run<-define.group.stats.for.numerical.y.variable(df_to_analyze_without_na,x_var,y_var,
                                          group_dependence,number_of_unique_x_groups)
  }else{
    stats_to_run<-"There is an error - y var is not categorical or numerical."

  }
  print(stats_to_run, quote=FALSE)
  #Run the stats
  run.the.stats(df_to_analyze_without_na,group_dependence,x_var,y_var,stats_to_run,y_order,is_ordered,checking_if_already_ordered,dependence_is_random_effect,x_levels)

}


# Small Subfunctions ------------------------------------------------------


#Subfunctions
#Removes rows with NA values in certain columns
remove.rows.with.na.in.specified.column <- function(df_to_analyze, columns_with_na) {
  complete_rows_vector <- complete.cases(df_to_analyze[, columns_with_na])
  return(df_to_analyze[complete_rows_vector, ])
}

#Counts number of different types of groups and saves the verdict as a variable
count.types.of.x.groups<-function(df_to_analyze,x_var){
  if(length(unique(df_to_analyze[[x_var]]))==1){
    number_of_unique_x_groups<-"There is one type of group."
  }else if(length(unique(df_to_analyze[[x_var]]))==2){
    number_of_unique_x_groups<-"There are two types of groups."
  }else if(length(unique(df_to_analyze[[x_var]]))>2){
    number_of_unique_x_groups<-"There are more than two types of groups."
  }else{
    print("Error - There are no x groups.")
  }
  return(number_of_unique_x_groups)
}


# Define Group Stats for Categorical y ------------------------------------


#Defines stats using number of x groups and group dependence if y is categorical
define.group.stats.for.categorical.y.variable<-function(df_to_analyze_without_na,x_var,y_var,
                                          group_dependence,number_of_unique_x_groups){
  if(number_of_unique_x_groups=="There is one type of group."){
    stats_to_run<-"Run Stats for one x group and categorical y"

  }else if(number_of_unique_x_groups=="There are two types of groups."){
    if(group_dependence==FALSE){
      stats_to_run<-"Run Stats for two independent x groups and categorical y"

    }else if(group_dependence!=FALSE){
      stats_to_run<-"Run Stats for two dependent x groups and categorical y"

    }else{
      stats_to_run<-"Error - two x groups and categorical y but unknown group dependence"
    }

  }else if(number_of_unique_x_groups=="There are more than two types of groups."){
    if(group_dependence==FALSE){
      stats_to_run<-"Run Stats for more than two independent x groups and categorical y"

    }else if(group_dependence!=FALSE){
      stats_to_run<-"Run Stats for more than two dependent x groups and categorical y"

    }else{
      stats_to_run<-"Error - more than two x groups and categorical y but unknown group dependence"
    }
  }else{
    stats_to_run<-"There is an error - the categorical y stats don't know how many x groups there are"
  }
  return(stats_to_run)
}


# Define group stats for numerical/ordinal y ------------------------------


#Defines stats using number of x groups and group dependence if y is numerical
define.group.stats.for.numerical.y.variable<-function(df_to_analyze_without_na,x_var,y_var,
                                          group_dependence,number_of_unique_x_groups){
  if(number_of_unique_x_groups=="There is one type of group."){
    stats_to_run<-"Run Stats for one x group and numerical/ordinal y"

  }else if(number_of_unique_x_groups=="There are two types of groups."){
    if(group_dependence==FALSE){
      stats_to_run<-"Run Stats for two independent x groups and numerical/ordinal y"

    }else if(group_dependence!=FALSE){
      stats_to_run<-"Run Stats for two dependent x groups and numerical/ordinal y"

    }else{
      stats_to_run<-"Error - two x groups and numerical/ordinal y but unknown group dependence"
    }

  }else if(number_of_unique_x_groups=="There are more than two types of groups."){
    if(group_dependence==FALSE){
      stats_to_run<-"Run Stats for more than two independent x groups and numerical/ordinal y"

    }else if(group_dependence!=FALSE){
      stats_to_run<-"Run Stats for more than two dependent x groups and numerical/ordinal y"

    }else{
      stats_to_run<-"Error - more than two x groups and numerical/ordinal y but unknown group dependence"
    }
  }else{
    stats_to_run<-"There is an error - the numerical/ordinal y stats don't know how many x groups there are"
  }
  return(stats_to_run)
}


# run.the.stats -----------------------------------------------------------


#Runs the actual stats functions using the defined type of stats needed
run.the.stats<-function(df_to_analyze_without_na,group_dependence,x_var,y_var,stats_to_run,y_order,is_ordered,checking_if_already_ordered,dependence_is_random_effect,x_levels){
  if(stats_to_run=="Error - two x groups and categorical y but unknown group dependence"){
    print("Error- Unable to run stats as stats_to_run contains an error.",quote=FALSE)
  }else if(stats_to_run=="Error - more than two x groups and categorical y but unknown group dependence"){
    print("Error- Unable to run stats as stats_to_run contains an error.",quote=FALSE)
  }else if(stats_to_run=="There is an error - the categorical y stats don't know how many x groups there are"){
    print("Error- Unable to run stats as stats_to_run contains an error.",quote=FALSE)
  }else if(stats_to_run=="Error - two x groups and numerical/ordinal y but unknown group dependence"){
    print("Error- Unable to run stats as stats_to_run contains an error.",quote=FALSE)
  }else if(stats_to_run=="Error - more than two x groups and numerical/ordinal y but unknown group dependence"){
    print("Error- Unable to run stats as stats_to_run contains an error.",quote=FALSE)
  }else if(stats_to_run=="There is an error - the numerical/ordinal y stats don't know how many x groups there are"){
    print("Error- Unable to run stats as stats_to_run contains an error.",quote=FALSE)
  }else if(stats_to_run=="There is an error - y var is not categorical or numerical."){
    print("Error- Unable to run stats as stats_to_run contains an error.",quote=FALSE)


# Run group stats: 1 x group, categorical y -------------------------------


  }else if(stats_to_run=="Run Stats for one x group and categorical y"){
    print("Please input expected values/probabilities and run the chi-square goodness-of-fit: http://www.sthda.com/english/wiki/chi-square-goodness-of-fit-test-in-r",quote=FALSE)
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
    #expected_values<-c()
    #expected_values should be a vector of same length as table(subsetted_df_to_analyze)
    #chisq.test(table(subsetted_df_to_analyze,expected_values))


# Run group stats: 2 independent x groups, categorical y ------------------


  }else if(stats_to_run=="Run Stats for two independent x groups and categorical y"){
    #Chi-square test
    #Selects only x_var and y_var columns from the data frame
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
    #Converts data to a frequency table, then runs the chi square test
    freq_table<-table(subsetted_df_to_analyze[[y_var]],subsetted_df_to_analyze[[x_var]],dnn = c(y_var,x_var))
    cat(paste("","",sep="\n"))
    print(freq_table)
    cat(paste("","",sep="\n"))
    chi_square_result<-chisq.test(freq_table)
    if(any(chi_square_result$expected<5)==TRUE){
      #Run Fisher's Exact Test
      attributes(freq_table)$class <- "matrix"
      #Run Fisher's Exact Test. If error, try Monte Carlo simulation to calculate p value.
      #Monte Carlo simulation would fix errors caused by insufficient workspace when using
      #Fisher test on large, non 2x2 tables.
      tryCatch(print(fisher.test(freq_table)),
               error=function(error_message){
                 print("Error generated on normal Fisher Test. Monte Carlo simulation used instead.",quote=F)
                 print(fisher.test(freq_table,simulate.p.value = T))
               })



    }else{
      print(chi_square_result)
    }


# Run group stats: 2 dependent x groups, categorical y ---------------------


  }else if(stats_to_run=="Run Stats for two dependent x groups and categorical y"){
    #McNemar
    #Requires indication of which subjects are paired - script assumes the same
    #subjects are repeated and labelled accordingly.
    #Selects the subjects, the x_var and y_var columns from the data frame
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(group_dependence,x_var,y_var)]


    #Testing for multiple observations within x_var groups using group_dependence
    testing_for_repeated_group_dependence_values<-c()
    for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
      x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
      testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
    }
    if(any(testing_for_repeated_group_dependence_values==FALSE)){
      print("There are repeated observations within individual x_var groups; A model must be fitted.",quote=FALSE)
      run_model=TRUE
    }else if(class(subsetted_df_to_analyze[[group_dependence]])=="numeric"){
      print("group_dependence is a numeric; A model must be fitted",quote=FALSE)
      run_model=TRUE
    }else{
      print("A model is unnecessary; normal tests will be run.",quote=FALSE)
      run_model=FALSE
    }





    if(run_model==TRUE){
      cat(paste("","",sep="\n"))
      #repeated measures logistic regression
      require(lme4)
      subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(group_dependence,x_var,y_var)]
      #Print data table
      freq_table_to_print<-as.data.frame(cbind(subsetted_df_to_analyze[[x_var]],subsetted_df_to_analyze[[y_var]],subsetted_df_to_analyze[[group_dependence]]))
      names(freq_table_to_print)<-c(x_var,y_var,group_dependence)
      print(table(freq_table_to_print))
      cat(paste("","",sep="\n"))
      #as.factor converts categorical data to 0,1,2,3
      #use contrasts(factor) to see what corresponds to 0,1,2,3
      subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
      subsetted_df_to_analyze[[y_var]]<-as.factor(subsetted_df_to_analyze[[y_var]])
      subsetted_df_to_analyze[[group_dependence]]<-as.factor(subsetted_df_to_analyze[[group_dependence]])

      if(class(x_levels)!="NULL"){
        levels(subsetted_df_to_analyze[[x_var]])<-x_levels
      }


      #Builds formula and runs analysis
      formula_x_text<-paste("`",x_var,"`",sep="")
      formula_y_text<-paste("`",y_var,"`",sep="")
      formula_group_text<-paste("`",group_dependence,"`",sep="")
      formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
      formula_text2<-paste(formula_text1,"+","(1|",formula_group_text,")",sep="")
      formula_for_glmer_with_x_effect<-as.formula(formula_text2)
      glmer_with_x_effect<-glmer(data=subsetted_df_to_analyze,formula_for_glmer_with_x_effect,family=binomial, nAGQ = 25,control = glmerControl(optimizer = "bobyqa"))

      formula_text_for_glmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
      formula_for_glmer_without_x_effect<-as.formula(formula_text_for_glmer_without_x_effect)
      glmer_without_x_effect<-glmer(data=subsetted_df_to_analyze,formula_for_glmer_without_x_effect,family=binomial, nAGQ = 25,control = glmerControl(optimizer = "bobyqa"))

      #Creates output
      print(anova(glmer_without_x_effect,glmer_with_x_effect))
      cat(paste("","","","",sep="\n"))
      print(paste("Mixed Effects Logistic Regression comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
      print(contrasts(subsetted_df_to_analyze[[y_var]]))
      cat(paste("","","",sep="\n"))
      print(summary(glmer_with_x_effect))
    }else if(run_model==FALSE){

      require(tidyr)
      wide_df<-spread(subsetted_df_to_analyze,x_var,y_var)
      dependent_freq_table <- table(wide_df[,c((unique(subsetted_df_to_analyze[[x_var]])[1]),(unique(subsetted_df_to_analyze[[x_var]])[2]))] )
      cat(paste("","",sep="\n"))
      print(dependent_freq_table)
      cat(paste("","",sep="\n"))
      print(mcnemar.test(dependent_freq_table))
    }else{
      print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know whether to run a linear model or another test.",quote=FALSE)
    }


# Run group stats: 2+ independent x groups, categorical y -----------------


  }else if(stats_to_run=="Run Stats for more than two independent x groups and categorical y"){
    #Chi-square test
    #Selects only x_var and y_var columns from the data frame
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
    #Converts data to a frequency table, then runs the chi square test
    freq_table<-table(subsetted_df_to_analyze[[y_var]],subsetted_df_to_analyze[[x_var]],dnn = c(y_var,x_var))
    attributes(freq_table)$class <- "matrix"
    cat(paste("","",sep="\n"))
    print(freq_table)
    cat(paste("","",sep="\n"))
    chi_square_result<-chisq.test(freq_table)
    if(any(chi_square_result$expected<5)==TRUE){
      #Run Fisher's Exact Test. If error, try Monte Carlo simulation to calculate p value.
      #Monte Carlo simulation would fix errors caused by insufficient workspace when using
      #Fisher test on large, non 2x2 tables.
      tryCatch(print(fisher.test(freq_table)),
               error=function(error_message){
                 print("Error generated on normal Fisher Test. Monte Carlo simulation used instead.",quote=F)
                 print(fisher.test(freq_table,simulate.p.value = T))
               })



      require(rcompanion)
      print(pairwiseNominalIndependence(freq_table,fisher = TRUE,gtest  = FALSE,
                                  chisq  = FALSE,method = "BH",compare = "column"))
      print("Method of Fisher's Test p value adjustment: Benjamini–Hochberg",quote=FALSE)
    }else{
      print(chi_square_result)
      require(rcompanion)
      print(pairwiseNominalIndependence(freq_table,fisher = FALSE,gtest  = FALSE,
                                  chisq  = TRUE,method = "BH",compare = "column"))
      print("Method of Chi Square p value adjustment: Benjamini–Hochberg",quote=FALSE)
    }


# Run group stats: 2+ dependent x groups, categorical y -------------------


  }else if(stats_to_run=="Run Stats for more than two dependent x groups and categorical y"){
    cat(paste("","",sep="\n"))
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(group_dependence,x_var,y_var)]
    #Testing for repeated group_dependence values to choose the type of frequency table to print
    testing_for_repeated_group_dependence_values<-c()
    for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
      x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
      testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
    }
    if(any(testing_for_repeated_group_dependence_values==FALSE)){
      #Print data table
      freq_table_to_print<-as.data.frame(cbind(subsetted_df_to_analyze[[x_var]],subsetted_df_to_analyze[[y_var]],subsetted_df_to_analyze[[group_dependence]]))
      names(freq_table_to_print)<-c(x_var,y_var,group_dependence)
      print(table(freq_table_to_print))
      cat(paste("","",sep="\n"))
    }else if(all(testing_for_repeated_group_dependence_values==TRUE)){
      print("There are no repeated observations within individual x_var groups.",quote=FALSE)
      require(tidyr)
      wide_df<-spread(subsetted_df_to_analyze,x_var,y_var)

      x_variable_names<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
        x_variable_names[i]<-unique(subsetted_df_to_analyze[[x_var]])[i]
      }
      dependent_freq_table <- table(wide_df[,x_variable_names] )

      cat(paste("","",sep="\n"))
      print(dependent_freq_table)
      cat(paste("","",sep="\n"))
    }else{
      print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know how to print the data table.",quote=FALSE)
    }

    #repeated measures logistic regression
    require(lme4)
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(group_dependence,x_var,y_var)]
    #as.factor converts categorical data to 0,1,2,3
    #use contrasts(factor) to see what corresponds to 0,1,2,3
    subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
    subsetted_df_to_analyze[[y_var]]<-as.factor(subsetted_df_to_analyze[[y_var]])
    subsetted_df_to_analyze[[group_dependence]]<-as.factor(subsetted_df_to_analyze[[group_dependence]])

    if(class(x_levels)!="NULL"){
      levels(subsetted_df_to_analyze[[x_var]])<-x_levels
    }



    #Builds formula and runs analysis
    formula_x_text<-paste("`",x_var,"`",sep="")
    formula_y_text<-paste("`",y_var,"`",sep="")
    formula_group_text<-paste("`",group_dependence,"`",sep="")
    formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
    formula_text2<-paste(formula_text1,"+","(1|",formula_group_text,")",sep="")
    formula_for_glmer_with_x_effect<-as.formula(formula_text2)
    glmer_with_x_effect<-glmer(data=subsetted_df_to_analyze,formula_for_glmer_with_x_effect,family=binomial, nAGQ = 25,control = glmerControl(optimizer = "bobyqa"))

    formula_text_for_glmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
    formula_for_glmer_without_x_effect<-as.formula(formula_text_for_glmer_without_x_effect)
    glmer_without_x_effect<-glmer(data=subsetted_df_to_analyze,formula_for_glmer_without_x_effect,family=binomial, nAGQ = 25,control = glmerControl(optimizer = "bobyqa"))

    #Creates output
    print(anova(glmer_without_x_effect,glmer_with_x_effect))
    cat(paste("","","","",sep="\n"))
    print(paste("Mixed Effects Logistic Regression comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
    print(contrasts(subsetted_df_to_analyze[[y_var]]))
    cat(paste("","","",sep="\n"))
    print(summary(glmer_with_x_effect))
    cat(paste("","","",sep="\n"))


# Run group stats: 1 x group, numerical/ordinal y ---------------------------------


  }else if(stats_to_run=="Run Stats for one x group and numerical/ordinal y"){
    #Detects if y variable is ordered or numerical
    if(any(is_ordered)==TRUE){
        #y variable is ordered

        if(all(checking_if_already_ordered)==TRUE){
          #y_var is already numerically ordered
          print("y variable is already numerically ordered. If untrue, please redefine y_order as a vector with ordering.",quote = FALSE)
          subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
        }else{
          #y_var is not already numerically ordered
          print("y variable is not yet numerically ordered, and ascending order will be assigned using y_order.",quote=FALSE)
          #apply y_order as levels to y_var and convert to numeric
          subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
          subsetted_df_to_analyze[[y_var]]<-factor(subsetted_df_to_analyze[[y_var]],ordered = TRUE)
          levels(subsetted_df_to_analyze[[y_var]])<-y_order
          subsetted_df_to_analyze[[y_var]]<-as.numeric(subsetted_df_to_analyze[[y_var]])
          y_order_to_print<-paste(y_order, collapse=", ")
          print(paste("The y variable is ordinal with levels in ascending order: ",y_order_to_print,".",sep="",collapse=""),quote = FALSE)
        }
        #Run the ordinal tests once the y variable is converted into a numeric
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        print(table(subsetted_df_to_analyze))
        cat(paste("","","",sep="\n"))
        print(summary(subsetted_df_to_analyze))
        if(class(subsetted_df_to_analyze[[y_var]])!="numeric"){
          print("Error: either y_order is unspecified for a non-numerical y variable, or your numerical y variable is not read as a numeric.",quote = FALSE)
        }
        print("Please input expected value and run a one-sample wilcoxon signed rank test: http://www.sthda.com/english/wiki/one-sample-wilcoxon-signed-rank-test-in-r",quote=FALSE)
        #wilcox.test(subsetted_df_to_analyze[[y_var]],mu=EXPECTEDVALUE,exact = FALSE)
    }else{
      #y variable is numerical
      y_normality<-shapiro.test(df_to_analyze_without_na[[y_var]])
      print(y_normality)
      print(paste("The sample size is",length(df_to_analyze_without_na[[y_var]]),"observations."),quote = FALSE)

      #Determines whether to run parametric or non-parametric tests
      if((y_normality$p.value>0.05&&length(df_to_analyze_without_na[[y_var]])>20)|length(df_to_analyze_without_na[[y_var]])>30){
        print("There is normality and a sufficient sample size to run a parametric test.", quote = FALSE)
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        print(summary(subsetted_df_to_analyze))
        print(paste("Standard Deviation:",signif(sd(subsetted_df_to_analyze[[y_var]]),digits = 4)),quote=FALSE)
        print("Please input the expected value and run a one-sample t-test: https://www.r-bloggers.com/one-sample-students-t-test/",quote=FALSE)
        #print(t.test(subsetted_df_to_analyze[[y_var]],mu=EXPECTEDVALUE))
      }else if(y_normality$p.value<=0.05|length(df_to_analyze_without_na[[y_var]])<21){
        print("There is not normality and/or a sufficient sample size to run a parametric test; non-parametric tests will be conducted.", quote=FALSE)
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        print(summary(subsetted_df_to_analyze))
        print(paste("Standard Deviation:",signif(sd(subsetted_df_to_analyze[[y_var]]),digits = 4)),quote=FALSE)
        print("Please input the expected value and run a one-sample wilcoxon signed rank test: http://www.sthda.com/english/wiki/one-sample-wilcoxon-signed-rank-test-in-r",quote=FALSE)
        #print(wilcox.test(subsetted_df_to_analyze[[y_var]], mu = EXPECTEDVALUE, alternative = "two.sided"))
      }else{
        print("Error: The function does not know whether to run a parametric or non-parametric test for one x group and numerical y.",quote = FALSE)
      }
    }


# Run group stats: 2 independent x groups, numerical/ordinal y ------------


  }else if(stats_to_run=="Run Stats for two independent x groups and numerical/ordinal y"){
    #If interval and normal: independent sample t-test. If ordinal/interval: Wilcoxon-Mann Whitney Test
    #Detects if y variable is ordered or numerical
    if(any(is_ordered)==TRUE){
      #y variable is ordered

      if(all(checking_if_already_ordered)==TRUE){
        #y_var is already numerically ordered
        print("y variable is already numerically ordered. If untrue, please redefine y_order as a vector with ordering.",quote = FALSE)
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
      }else{
        #y_var is not already numerically ordered
        print("y variable is not yet numerically ordered, and ascending order will be assigned using y_order.",quote=FALSE)
        #apply y_order as levels to y_var and convert to numeric
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
        subsetted_df_to_analyze[[y_var]]<-factor(subsetted_df_to_analyze[[y_var]],ordered = TRUE)
        levels(subsetted_df_to_analyze[[y_var]])<-y_order
        subsetted_df_to_analyze[[y_var]]<-as.numeric(subsetted_df_to_analyze[[y_var]])
        y_order_to_print<-paste(y_order, collapse=", ")
        print(paste("The y variable is ordinal with levels in ascending order: ",y_order_to_print,".",sep="",collapse=""),quote = FALSE)
      }
      #Run the ordinal tests once the y variable is converted into a numeric
      subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
      print(table(subsetted_df_to_analyze))
      if(class(subsetted_df_to_analyze[[y_var]])!="numeric"){
        print("Error: either y_order is unspecified for a non-numerical y variable, or your numerical y variable is not read as a numeric.",quote = FALSE)
      }
      cat(paste("","","",sep="\n"))
      #Wilcoxon Mann-Whitney U-Test
      formula_x_text<-paste("`",x_var,"`",sep="")
      formula_y_text<-paste("`",y_var,"`",sep="")
      formula_text<-paste(formula_y_text,formula_x_text,sep="~")
      u_test_formula<-as.formula(formula_text)
      print(wilcox.test(data=subsetted_df_to_analyze,u_test_formula,exact=FALSE))
      cat(paste("","",sep="\n"))
      print("Note: a two-sample Wilcoxon rank sum test is equivalent to a two-sample Mann-Whitney U-Test.",quote = FALSE)
    }else{
      #y variable is numerical
      subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
      #Subsetting by group to test for normality
      df_with_first_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[1]),]
      df_with_second_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[2]),]
      #Testing for normality
      looped_normality_output<-with(data=df_to_analyze_without_na,tapply(df_to_analyze_without_na[[y_var]],df_to_analyze_without_na[[x_var]],shapiro.test))
      print(looped_normality_output)
      normality.p.values<-c()
      normality.p.value_names<-c()
      for (i in 1:length(looped_normality_output)) {
        normality.p.values[i]<-looped_normality_output[[i]]$p.value
        normality.p.value_names[i]<-names(looped_normality_output)[i]
      }
      names(normality.p.values)<-normality.p.value_names
      cat(paste("","",sep="\n"))
      normality_p_values_to_print<-paste(signif(normality.p.values,digits = 4), collapse=", ")
      print(paste("p-values from testing x variable normality:",normality_p_values_to_print),quote=FALSE)
      #Printing the sample sizes
      x_group_names<-c()
      x_group_sample_sizes<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
        x_group_names[i]<-unique(subsetted_df_to_analyze[[x_var]])[i]
        x_group_sample_sizes[i]<-length(subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),][[x_var]])
      }
      names(x_group_sample_sizes)<-x_group_names
      cat(paste("","",sep="\n"))
      sample_size_df<-data.frame(x_group_names,x_group_sample_sizes)
      sample_size_names<-c("x Variable Group","Sample Size")
      colnames(sample_size_df)<-sample_size_names
      print(sample_size_df,row.names=F)
      cat(paste("","",sep="\n"))
      #Determines whether to run parametric or non-parametric tests
      parametric_or_not_vector<-c()
      for (i in 1:length(normality.p.values)) {
        sample_size_column<-match(names(normality.p.values)[i],names(x_group_sample_sizes))
        parametric_or_not_vector[i]<-(15<x_group_sample_sizes[sample_size_column]&&normality.p.values[i]>0.05)|x_group_sample_sizes[sample_size_column]>30
      }
      if(all(parametric_or_not_vector==TRUE)){
        print("There is normality and a sufficient sample size to run a parametric test.", quote = FALSE)
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        df_with_first_x_group[[x_var]]<-as.factor(df_with_first_x_group[[x_var]])
        df_with_second_x_group[[x_var]]<-as.factor(df_with_second_x_group[[x_var]])
        #Gives a summary of each group
        cat(paste("","","",sep="\n"))
        print(summary(df_with_first_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_first_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","",sep="\n"))
        print(summary(df_with_second_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_second_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","","",sep="\n"))
        #Checks for homogeneity of variances
        require(car)
        formula_x_text<-paste("`",x_var,"`",sep="")
        formula_y_text<-paste("`",y_var,"`",sep="")
        x_y_formula_text<-paste(formula_y_text,formula_x_text,sep="~")
        formula_y_by_x<-as.formula(x_y_formula_text)
        levene_result<-leveneTest(data=subsetted_df_to_analyze,formula_y_by_x)
        print(levene_result)
        #Defines output based on homogeneity of variance
        if(levene_result$`Pr(>F)`[1]>0.05){
          print("Levene's Test is insignificant: the variances of x groups are assumed to be homogenous.",quote = FALSE)
          #Run 2-sample student's t-test
          cat(paste("","","",sep="\n"))
          print(t.test(data=subsetted_df_to_analyze,formula_y_by_x,var.equal = TRUE))
        }else if(levene_result$`Pr(>F)`[1]<=0.05){
          print("Levene's Test is significant: the variances of x groups are heterogenous.",quote = FALSE)
          #Run 2-sample Welch's t-test
          cat(paste("","","",sep="\n"))
          print(t.test(data=subsetted_df_to_analyze,formula_y_by_x,var.equal = FALSE))
        }else{
          print("There is an error with leveneTest. The p-value could not be extracted.",quote = FALSE)
        }

      }else if(any(parametric_or_not_vector==FALSE)){
        print("There is not normality and/or a sufficient sample size to run a parametric test; non-parametric tests will be conducted.", quote=FALSE)
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        df_with_first_x_group[[x_var]]<-as.factor(df_with_first_x_group[[x_var]])
        df_with_second_x_group[[x_var]]<-as.factor(df_with_second_x_group[[x_var]])
        #Gives a summary of each group
        cat(paste("","","",sep="\n"))
        print(summary(df_with_first_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_first_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","",sep="\n"))
        print(summary(df_with_second_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_second_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","","",sep="\n"))
        #Wilcoxon Mann-Whitney U-Test
        formula_x_text<-paste("`",x_var,"`",sep="")
        formula_y_text<-paste("`",y_var,"`",sep="")
        formula_text<-paste(formula_y_text,formula_x_text,sep="~")
        u_test_formula<-as.formula(formula_text)
        print(wilcox.test(data=subsetted_df_to_analyze,u_test_formula))
        cat(paste("","",sep="\n"))
        print("Note: a two-sample Wilcoxon rank sum test is equivalent to a two-sample Mann-Whitney U-Test.",quote = FALSE)
      }else{
        print("Error: The function does not know whether to run a parametric or non-parametric test for two independent x groups and numerical y.",quote = FALSE)
      }
    }



# Run group stats: 2 dependent x groups, numerical/ordinal y --------------


  }else if(stats_to_run=="Run Stats for two dependent x groups and numerical/ordinal y"){
    #Interval and normal: paired t-test. Ordinal/Interval: Wilcoxon signed ranks test
    #Detects if y variable is ordered or numerical
    if(any(is_ordered)==TRUE){
      #y variable is ordered

      if(all(checking_if_already_ordered)==TRUE){
        #y_var is already numerically ordered
        print("y variable is already numerically ordered. If untrue, please redefine y_order as a vector with ordering.",quote = FALSE)
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]
      }else{
        #y_var is not already numerically ordered
        print("y variable is not yet numerically ordered, and ascending order will be assigned using y_order.",quote=FALSE)
        #apply y_order as levels to y_var and convert to numeric
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]
        subsetted_df_to_analyze[[y_var]]<-factor(subsetted_df_to_analyze[[y_var]],ordered = TRUE)
        levels(subsetted_df_to_analyze[[y_var]])<-y_order
        subsetted_df_to_analyze[[y_var]]<-as.numeric(subsetted_df_to_analyze[[y_var]])
        y_order_to_print<-paste(y_order, collapse=", ")
        print(paste("The y variable is ordinal with levels in ascending order: ",y_order_to_print,".",sep="",collapse=""),quote = FALSE)
      }
      #Run the ordinal tests once the y variable is converted into a numeric
      subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
      summary_data_frame_for_output<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
      print(table(summary_data_frame_for_output))
      if(class(subsetted_df_to_analyze[[y_var]])!="numeric"){
        print("Error: either y_order is unspecified for a non-numerical y variable, or your numerical y variable is not read as a numeric.",quote = FALSE)
      }
      cat(paste("","","",sep="\n"))


      #Testing for multiple observations within x_var groups using group_dependence
      testing_for_repeated_group_dependence_values<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
        x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
        testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
      }
      if(any(testing_for_repeated_group_dependence_values==FALSE)){
        print("There are repeated observations within individual x_var groups; A model must be fitted.",quote=FALSE)
        run_model=TRUE
      }else if(class(subsetted_df_to_analyze[[group_dependence]])=="numeric"){
        print("group_dependence is a numeric; A model must be fitted",quote=FALSE)
        run_model=TRUE
      }else{
        print("A model is unnecessary; normal tests will be run.",quote=FALSE)
        run_model=FALSE
      }




      if(run_model==TRUE){

        #Check if group_dependence is a random effect, if so then do lmer. If not do lm().
        if(class(x_levels)!="NULL"){
          levels(subsetted_df_to_analyze[[x_var]])<-x_levels
        }




        if(dependence_is_random_effect==TRUE){
          #It's a random effect, so do lmer.
          require(lme4)
          require(lmerTest)
          cat(paste("","",sep="\n"))
          print("group_dependence is a random effect, so a linear mixed model will be run.",quote=F)
          cat(paste("","",sep="\n"))

          formula_x_text<-paste("`",x_var,"`",sep="")
          formula_y_text<-paste("`",y_var,"`",sep="")
          formula_group_text<-paste("`",group_dependence,"`",sep="")
          formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
          random_slope_text<-paste("+(1+",formula_x_text,"|",formula_group_text,")",sep="")
          formula_text2<-paste(formula_text1,random_slope_text,sep="")
          formula_for_lmer_with_x_effect<-as.formula(formula_text2)

          lmer_with_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_with_x_effect,REML=F)

          formula_text_for_lmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
          formula_for_lmer_without_x_effect<-as.formula(formula_text_for_lmer_without_x_effect)
          lmer_without_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_without_x_effect,REML=F)

          #Creates output
          print(anova(lmer_without_x_effect,lmer_with_x_effect))
          cat(paste("","","","",sep="\n"))
          print(paste("Linear Mixed Model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
          cat(paste("","","",sep="\n"))
          print(summary(lmer_with_x_effect))


        }else if(dependence_is_random_effect==FALSE){
          #It's a fixed effect, so do lm()
          cat(paste("","",sep="\n"))
          print("group_dependence is a fixed effect, so a multivariable linear model will be run.",quote=F)
          cat(paste("","",sep="\n"))
          formula_x_text<-paste("`",x_var,"`",sep="")
          formula_y_text<-paste("`",y_var,"`",sep="")
          formula_group_dependence_text<-paste("`",group_dependence,"`",sep="")
          formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
          formula_text<-paste(formula_y_by_x,formula_group_dependence_text,sep="*")
          linear_model_formula<-as.formula(formula_text)
          print(summary(aov(data = subsetted_df_to_analyze,linear_model_formula)))
          cat(paste("","","",sep="\n"))
          print(paste("OLS linear model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
          print(summary(lm(data = subsetted_df_to_analyze,linear_model_formula,model = TRUE)))

        }else{
          print("Error: dependence_is_random_effect has an invalid value (not y or n).",quote=F)
        }

      }else if(run_model==FALSE){

        #Remove corresponding (according to group_dependence) observations to those removed earlier due to NAs
        cat(paste("","","",sep="\n"))
        no_of_groups<-length(unique(subsetted_df_to_analyze[[x_var]]))
        for (i in length(unique(subsetted_df_to_analyze[[group_dependence]])):1) {
          if (no_of_groups>length((subsetted_df_to_analyze[which(subsetted_df_to_analyze[[group_dependence]] == unique(subsetted_df_to_analyze[[group_dependence]])[i]),][[y_var]]))){
            val_to_remove<-unique(subsetted_df_to_analyze[[group_dependence]])[i]
            cat("Removing",val_to_remove,"from analysis as it has missing y_var values.","\n")
            subsetted_df_to_analyze<-subsetted_df_to_analyze[subsetted_df_to_analyze[[group_dependence]]!=val_to_remove,]
          }
        }
        cat(paste("","","",sep="\n"))

        #Wilcoxon Signed Ranks Test
        formula_x_text<-paste("`",x_var,"`",sep="")
        formula_y_text<-paste("`",y_var,"`",sep="")
        formula_text<-paste(formula_y_text,formula_x_text,sep="~")
        wilcoxon_signed_ranks_test_formula<-as.formula(formula_text)

        print(wilcox.test(data=subsetted_df_to_analyze,wilcoxon_signed_ranks_test_formula,exact=FALSE,paired = TRUE))
      }else{
        print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know whether to run a linear model or another test.",quote=FALSE)
      }
    }else{
      #y variable is numerical
      subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]
      df_with_first_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[1]),]
      df_with_second_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[2]),]
      #Testing for normality
      looped_normality_output<-with(data=df_to_analyze_without_na,tapply(df_to_analyze_without_na[[y_var]],df_to_analyze_without_na[[x_var]],shapiro.test))
      print(looped_normality_output)
      normality.p.values<-c()
      normality.p.value_names<-c()
      for (i in 1:length(looped_normality_output)) {
        normality.p.values[i]<-looped_normality_output[[i]]$p.value
        normality.p.value_names[i]<-names(looped_normality_output)[i]
      }
      names(normality.p.values)<-normality.p.value_names
      cat(paste("","",sep="\n"))
      normality_p_values_to_print<-paste(signif(normality.p.values,digits = 4), collapse=", ")
      print(paste("p-values from testing x variable normality:",normality_p_values_to_print),quote=FALSE)
      #Printing the sample sizes
      x_group_names<-c()
      x_group_sample_sizes<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
        x_group_names[i]<-unique(subsetted_df_to_analyze[[x_var]])[i]
        x_group_sample_sizes[i]<-length(subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),][[x_var]])
      }
      names(x_group_sample_sizes)<-x_group_names
      cat(paste("","",sep="\n"))
      sample_size_df<-data.frame(x_group_names,x_group_sample_sizes)
      sample_size_names<-c("x Variable Group","Sample Size")
      colnames(sample_size_df)<-sample_size_names
      print(sample_size_df,row.names=F)
      cat(paste("","",sep="\n"))
      #Determines whether to run parametric or non-parametric tests
      parametric_or_not_vector<-c()
      for (i in 1:length(normality.p.values)) {
        sample_size_column<-match(names(normality.p.values)[i],names(x_group_sample_sizes))
        parametric_or_not_vector[i]<-(15<x_group_sample_sizes[sample_size_column]&&normality.p.values[i]>0.05)|x_group_sample_sizes[sample_size_column]>30
      }
      if(all(parametric_or_not_vector==TRUE)){
        print("There is normality and a sufficient sample size to run a parametric test.", quote = FALSE)
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        df_with_first_x_group[[x_var]]<-as.factor(df_with_first_x_group[[x_var]])
        df_with_second_x_group[[x_var]]<-as.factor(df_with_second_x_group[[x_var]])
        #Gives a summary of each group
        cat(paste("","","",sep="\n"))
        print(summary(df_with_first_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_first_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","",sep="\n"))
        print(summary(df_with_second_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_second_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","","",sep="\n"))


        #Testing for multiple observations within x_var groups using group_dependence
        testing_for_repeated_group_dependence_values<-c()
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
          x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
          testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
        }
        if(any(testing_for_repeated_group_dependence_values==FALSE)){
          print("There are repeated observations within individual x_var groups; A model must be fitted.",quote=FALSE)
          run_model=TRUE
        }else if(class(subsetted_df_to_analyze[[group_dependence]])=="numeric"){
          print("group_dependence is a numeric; A model must be fitted",quote=FALSE)
          run_model=TRUE
        }else{
          print("A model is unnecessary; normal tests will be run.",quote=FALSE)
          run_model=FALSE
        }





        if(run_model==TRUE){

          #Check if group_dependence is a random effect, if so then do lmer. If not do lm().
          if(class(x_levels)!="NULL"){
            levels(subsetted_df_to_analyze[[x_var]])<-x_levels
          }



          if(dependence_is_random_effect==TRUE){
            #It's a random effect, so do lmer.
            require(lme4)
            require(lmerTest)
            cat(paste("","",sep="\n"))
            print("group_dependence is a random effect, so a linear mixed model will be run.",quote=F)
            cat(paste("","",sep="\n"))

            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_text<-paste("`",group_dependence,"`",sep="")
            formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
            random_slope_text<-paste("+(1+",formula_x_text,"|",formula_group_text,")",sep="")
            formula_text2<-paste(formula_text1,random_slope_text,sep="")
            formula_for_lmer_with_x_effect<-as.formula(formula_text2)

            lmer_with_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_with_x_effect,REML=F)

            formula_text_for_lmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
            formula_for_lmer_without_x_effect<-as.formula(formula_text_for_lmer_without_x_effect)
            lmer_without_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_without_x_effect,REML=F)

            #Creates output
            print(anova(lmer_without_x_effect,lmer_with_x_effect))
            cat(paste("","","","",sep="\n"))
            print(paste("Linear Mixed Model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            cat(paste("","","",sep="\n"))
            print(summary(lmer_with_x_effect))


          }else if(dependence_is_random_effect==FALSE){
            #It's a fixed effect, so do lm()
            cat(paste("","",sep="\n"))
            print("group_dependence is a fixed effect, so a multivariable linear model will be run.",quote=F)
            cat(paste("","",sep="\n"))
            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_dependence_text<-paste("`",group_dependence,"`",sep="")
            formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
            formula_text<-paste(formula_y_by_x,formula_group_dependence_text,sep="*")
            linear_model_formula<-as.formula(formula_text)
            print(summary(aov(data = subsetted_df_to_analyze,linear_model_formula)))
            cat(paste("","","",sep="\n"))
            print(paste("OLS linear model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            print(summary(lm(data = subsetted_df_to_analyze,linear_model_formula,model = TRUE)))

          }else{
            print("Error: dependence_is_random_effect has an invalid value (not y or n).",quote=F)
          }
        }else if(run_model==FALSE){

          #Remove corresponding (according to group_dependence) observations to those removed earlier due to NAs
          cat(paste("","","",sep="\n"))
          no_of_groups<-length(unique(subsetted_df_to_analyze[[x_var]]))
          for (i in length(unique(subsetted_df_to_analyze[[group_dependence]])):1) {
            if (no_of_groups>length((subsetted_df_to_analyze[which(subsetted_df_to_analyze[[group_dependence]] == unique(subsetted_df_to_analyze[[group_dependence]])[i]),][[y_var]]))){
              val_to_remove<-unique(subsetted_df_to_analyze[[group_dependence]])[i]
              cat("Removing",val_to_remove,"from analysis as it has missing y_var values.","\n")
              subsetted_df_to_analyze<-subsetted_df_to_analyze[subsetted_df_to_analyze[[group_dependence]]!=val_to_remove,]
            }
          }
          cat(paste("","","",sep="\n"))

          #Run the paired t-test
          formula_x_text<-paste("`",x_var,"`",sep="")
          formula_y_text<-paste("`",y_var,"`",sep="")
          x_y_formula_text<-paste(formula_y_text,formula_x_text,sep="~")
          formula_y_by_x<-as.formula(x_y_formula_text)

          print(t.test(data=subsetted_df_to_analyze,formula_y_by_x,paired=TRUE))
        }else{
          print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know whether to run a linear model or another test.",quote=FALSE)
        }
      }else if(any(parametric_or_not_vector==FALSE)){
        print("There is not normality and/or a sufficient sample size to run a parametric test; non-parametric tests will be conducted.", quote=FALSE)
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])


        df_with_first_x_group[[x_var]]<-as.factor(df_with_first_x_group[[x_var]])
        df_with_second_x_group[[x_var]]<-as.factor(df_with_second_x_group[[x_var]])
        #Gives a summary of each group
        cat(paste("","","",sep="\n"))
        print(summary(df_with_first_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_first_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","",sep="\n"))
        print(summary(df_with_second_x_group))
        print(paste("Standard Deviation:",signif(sd(df_with_second_x_group[[y_var]]),digits = 4)),quote=FALSE)
        cat(paste("","","",sep="\n"))





        #Testing for multiple observations within x_var groups using group_dependence
        testing_for_repeated_group_dependence_values<-c()
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
          x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
          testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
        }
        if(any(testing_for_repeated_group_dependence_values==FALSE)){
          print("There are repeated observations within individual x_var groups; A model must be fitted.",quote=FALSE)
          run_model=TRUE
        }else if(class(subsetted_df_to_analyze[[group_dependence]])=="numeric"){
          print("group_dependence is a numeric; A model must be fitted",quote=FALSE)
          run_model=TRUE
        }else{
          print("A model is unnecessary; normal tests will be run.",quote=FALSE)
          run_model=FALSE
        }



        if(run_model==TRUE){
          #Check if group_dependence is a random effect, if so then do lmer. If not do lm().
          if(class(x_levels)!="NULL"){
            levels(subsetted_df_to_analyze[[x_var]])<-x_levels
          }



          if(dependence_is_random_effect==TRUE){
            #It's a random effect, so do lmer.
            require(lme4)
            require(lmerTest)
            cat(paste("","",sep="\n"))
            print("group_dependence is a random effect, so a linear mixed model will be run.",quote=F)
            cat(paste("","",sep="\n"))

            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_text<-paste("`",group_dependence,"`",sep="")
            formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
            random_slope_text<-paste("+(1+",formula_x_text,"|",formula_group_text,")",sep="")
            formula_text2<-paste(formula_text1,random_slope_text,sep="")
            formula_for_lmer_with_x_effect<-as.formula(formula_text2)

            lmer_with_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_with_x_effect,REML=F)

            formula_text_for_lmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
            formula_for_lmer_without_x_effect<-as.formula(formula_text_for_lmer_without_x_effect)
            lmer_without_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_without_x_effect,REML=F)

            #Creates output
            print(anova(lmer_without_x_effect,lmer_with_x_effect))
            cat(paste("","","","",sep="\n"))
            print(paste("Linear Mixed Model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            cat(paste("","","",sep="\n"))
            print(summary(lmer_with_x_effect))


          }else if(dependence_is_random_effect==FALSE){
            #It's a fixed effect, so do lm()
            cat(paste("","",sep="\n"))
            print("group_dependence is a fixed effect, so a multivariable linear model will be run.",quote=F)
            cat(paste("","",sep="\n"))
            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_dependence_text<-paste("`",group_dependence,"`",sep="")
            formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
            formula_text<-paste(formula_y_by_x,formula_group_dependence_text,sep="*")
            linear_model_formula<-as.formula(formula_text)
            print(summary(aov(data = subsetted_df_to_analyze,linear_model_formula)))
            cat(paste("","","",sep="\n"))
            print(paste("OLS linear model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            print(summary(lm(data = subsetted_df_to_analyze,linear_model_formula,model = TRUE)))

          }else{
            print("Error: dependence_is_random_effect has an invalid value (not y or n).",quote=F)
          }


        }else if(run_model==FALSE){

          #Remove corresponding (according to group_dependence) observations to those removed earlier due to NAs
          cat(paste("","","",sep="\n"))
          no_of_groups<-length(unique(subsetted_df_to_analyze[[x_var]]))
          for (i in length(unique(subsetted_df_to_analyze[[group_dependence]])):1) {
            if (no_of_groups>length((subsetted_df_to_analyze[which(subsetted_df_to_analyze[[group_dependence]] == unique(subsetted_df_to_analyze[[group_dependence]])[i]),][[y_var]]))){
              val_to_remove<-unique(subsetted_df_to_analyze[[group_dependence]])[i]
              cat("Removing",val_to_remove,"from analysis as it has missing y_var values.","\n")
              subsetted_df_to_analyze<-subsetted_df_to_analyze[subsetted_df_to_analyze[[group_dependence]]!=val_to_remove,]
            }
          }
          cat(paste("","","",sep="\n"))

          #Wilcoxon Signed Ranks Test
          formula_x_text<-paste("`",x_var,"`",sep="")
          formula_y_text<-paste("`",y_var,"`",sep="")
          formula_text<-paste(formula_y_text,formula_x_text,sep="~")
          wilcoxon_signed_ranks_test_formula<-as.formula(formula_text)

          print(wilcox.test(data=subsetted_df_to_analyze,wilcoxon_signed_ranks_test_formula,paired=TRUE))
        }else{
          print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know whether to run a linear model or another test.",quote=FALSE)
        }
      }else{
        print("Error: The function does not know whether to run a parametric or non-parametric test for two paired x groups and numerical y.",quote = FALSE)
      }
    }


# Run group stats: 2+ independent x groups, numerical/ordinal y -----------


  }else if(stats_to_run=="Run Stats for more than two independent x groups and numerical/ordinal y"){
    #Detects if y variable is ordered or numerical
    if(any(is_ordered)==TRUE){
      #y variable is ordered

      if(all(checking_if_already_ordered)==TRUE){
        #y_var is already numerically ordered
        print("y variable is already numerically ordered. If untrue, please redefine y_order as a vector with ordering.",quote = FALSE)
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
      }else{
        #y_var is not already numerically ordered
        print("y variable is not yet numerically ordered, and ascending order will be assigned using y_order.",quote=FALSE)
        #apply y_order as levels to y_var and convert to numeric
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
        subsetted_df_to_analyze[[y_var]]<-factor(subsetted_df_to_analyze[[y_var]],ordered = TRUE)
        levels(subsetted_df_to_analyze[[y_var]])<-y_order
        subsetted_df_to_analyze[[y_var]]<-as.numeric(subsetted_df_to_analyze[[y_var]])
        y_order_to_print<-paste(y_order, collapse=", ")
        print(paste("The y variable is ordinal with levels in ascending order: ",y_order_to_print,".",sep="",collapse=""),quote = FALSE)
      }
      #Run the ordinal tests once the y variable is converted into a numeric
      subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
      print(table(subsetted_df_to_analyze))
      if(class(subsetted_df_to_analyze[[y_var]])!="numeric"){
        print("Error: either y_order is unspecified for a non-numerical y variable, or your numerical y variable is not read as a numeric.",quote = FALSE)
      }
      cat(paste("","","",sep="\n"))
      #Kruskal Wallis
      formula_x_text<-paste("`",x_var,"`",sep="")
      formula_y_text<-paste("`",y_var,"`",sep="")
      formula_text<-paste(formula_y_text,formula_x_text,sep="~")
      kruskal_test_formula<-as.formula(formula_text)
      kruskal_test_result<-kruskal.test(data=subsetted_df_to_analyze,kruskal_test_formula)
      print(kruskal_test_result)
      cat(paste("","","",sep="\n"))
      #Post-hoc Dunn's Test
      print("Dunn's Post-hoc Test:",quote=FALSE)
      require(dunn.test)
      dunn.test(x=subsetted_df_to_analyze[[y_var]],g=subsetted_df_to_analyze[[x_var]],method="BH",kw=FALSE,list=TRUE,table=FALSE,altp = TRUE)

    }else{
      #y variable is numerical
      subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
      #Testing for normality
      looped_normality_output<-with(data=df_to_analyze_without_na,tapply(df_to_analyze_without_na[[y_var]],df_to_analyze_without_na[[x_var]],shapiro.test))
      print(looped_normality_output)
      normality.p.values<-c()
      normality.p.value_names<-c()
      for (i in 1:length(looped_normality_output)) {
        normality.p.values[i]<-looped_normality_output[[i]]$p.value
        normality.p.value_names[i]<-names(looped_normality_output)[i]
      }
      names(normality.p.values)<-normality.p.value_names
      cat(paste("","",sep="\n"))
      normality_p_values_to_print<-paste(signif(normality.p.values,digits = 4), collapse=", ")
      print(paste("p-values from testing x variable normality:",normality_p_values_to_print),quote=FALSE)
      #Printing the sample sizes
      x_group_names<-c()
      x_group_sample_sizes<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
        x_group_names[i]<-unique(subsetted_df_to_analyze[[x_var]])[i]
        x_group_sample_sizes[i]<-length(subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),][[x_var]])
      }
      names(x_group_sample_sizes)<-x_group_names
      cat(paste("","",sep="\n"))
      sample_size_df<-data.frame(x_group_names,x_group_sample_sizes)
      sample_size_names<-c("x Variable Group","Sample Size")
      colnames(sample_size_df)<-sample_size_names
      print(sample_size_df,row.names=F)
      cat(paste("","",sep="\n"))
      #Determines whether to run parametric or non-parametric tests
      parametric_or_not_vector<-c()
      for (i in 1:length(normality.p.values)) {
        sample_size_column<-match(names(normality.p.values)[i],names(x_group_sample_sizes))
        parametric_or_not_vector[i]<-(15<x_group_sample_sizes[sample_size_column]&&normality.p.values[i]>0.05)|x_group_sample_sizes[sample_size_column]>30
      }
      if(all(parametric_or_not_vector==TRUE)){
        print("There is normality and a sufficient sample size to run a parametric test.", quote = FALSE)
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        #Gives a summary of each group
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
          cat(paste("","",sep="\n"))
          df_with_i_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),]
          print(summary(df_with_i_x_group))
          print(paste("Standard Deviation:",signif(sd(df_with_i_x_group[[y_var]]),digits = 4)),quote=FALSE)
        }
        cat(paste("","","",sep="\n"))
        #Checks for homogeneity of variances
        require(car)
        formula_x_text<-paste("`",x_var,"`",sep="")
        formula_y_text<-paste("`",y_var,"`",sep="")
        x_y_formula_text<-paste(formula_y_text,formula_x_text,sep="~")
        formula_y_by_x<-as.formula(x_y_formula_text)
        levene_result<-leveneTest(data=subsetted_df_to_analyze,formula_y_by_x)
        print(levene_result)
        cat(paste("","","",sep="\n"))
        #Defines output based on homogeneity of variance
        if(levene_result$`Pr(>F)`[1]>0.05){
          print("Levene's Test is insignificant: the variances of x groups are assumed to be homogenous.",quote = FALSE)
          cat(paste("","",sep="\n"))
          #One-way ANOVA with Tukey's Honestly Signficant Differences post-hoc
          parametric_anova_result<-aov(data = subsetted_df_to_analyze,subsetted_df_to_analyze[[y_var]]~subsetted_df_to_analyze[[x_var]])
          print("One-way ANOVA:",quote=FALSE)
          print(summary(parametric_anova_result))
          cat(paste("","",sep="\n"))
          print(TukeyHSD(parametric_anova_result))

        }else if(levene_result$`Pr(>F)`[1]<=0.05){
          print("Levene's Test is significant: the variances of x groups are heterogenous.",quote = FALSE)
          cat(paste("","",sep="\n"))
          #Welch's ANOVA with Games-Howell post-hoc
          print(oneway.test(data=subsetted_df_to_analyze,formula_y_by_x,var.equal = FALSE))
          cat(paste("","",sep="\n"))
          require(userfriendlyscience)
          print("Games-Howell Post-hoc Test:",quote=FALSE)
          print(posthocTGH(y=subsetted_df_to_analyze[[y_var]],x=subsetted_df_to_analyze[[x_var]],method = "games-howell",p.adjust="BH",digits = 4,formatPvalue = FALSE))
          cat(paste("","",sep="\n"))
          print("Method of Games-Howell post-hoc Test p value adjustment: Benjamini–Hochberg",quote=FALSE)
        }else{
          print("There is an error with leveneTest. The p-value could not be extracted.",quote = FALSE)
        }
      }else if(any(parametric_or_not_vector==FALSE)){
        print("There is not normality and/or a sufficient sample size to run a parametric test; non-parametric tests will be conducted.", quote=FALSE)
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        #Gives a summary of each group
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
          cat(paste("","",sep="\n"))
          df_with_i_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),]
          print(summary(df_with_i_x_group))
          print(paste("Standard Deviation:",signif(sd(df_with_i_x_group[[y_var]]),digits = 4)),quote=FALSE)
        }
        cat(paste("","","",sep="\n"))
        #Kruskal Wallis
        formula_x_text<-paste("`",x_var,"`",sep="")
        formula_y_text<-paste("`",y_var,"`",sep="")
        formula_text<-paste(formula_y_text,formula_x_text,sep="~")
        kruskal_test_formula<-as.formula(formula_text)
        kruskal_test_result<-kruskal.test(data=subsetted_df_to_analyze,kruskal_test_formula)
        print(kruskal_test_result)
        cat(paste("","","",sep="\n"))
        #Post-hoc Dunn's Test
        require(dunn.test)
        print("Dunn's Post-hoc Test:",quote=FALSE)
        dunn.test(x=subsetted_df_to_analyze[[y_var]],g=subsetted_df_to_analyze[[x_var]],method="BH",kw=FALSE,list=TRUE,table=FALSE,altp = TRUE)
      }else{
        print("Error: The function does not know whether to run a parametric or non-parametric test for more than two independent x groups and numerical y.",quote = FALSE)
      }
    }


# Run group stats: 2+ dependent x groups, numerical/ordinal y -------------


  }else if(stats_to_run=="Run Stats for more than two dependent x groups and numerical/ordinal y"){
    #Detects if y variable is ordered or numerical
    if(any(is_ordered)==TRUE){
      #y variable is ordered

      if(all(checking_if_already_ordered)==TRUE){
        #y_var is already numerically ordered
        print("y variable is already numerically ordered. If untrue, please redefine y_order as a vector with ordering.",quote = FALSE)
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]
      }else{
        #y_var is not already numerically ordered
        print("y variable is not yet numerically ordered, and ascending order will be assigned using y_order.",quote=FALSE)
        #apply y_order as levels to y_var and convert to numeric
        subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]
        subsetted_df_to_analyze[[y_var]]<-factor(subsetted_df_to_analyze[[y_var]],ordered = TRUE)
        levels(subsetted_df_to_analyze[[y_var]])<-y_order
        subsetted_df_to_analyze[[y_var]]<-as.numeric(subsetted_df_to_analyze[[y_var]])
        y_order_to_print<-paste(y_order, collapse=", ")
        print(paste("The y variable is ordinal with levels in ascending order: ",y_order_to_print,".",sep="",collapse=""),quote = FALSE)
      }
      #Gives Summary of Each Group
      subsetted_df_to_analyze[[x_var]]<-factor(subsetted_df_to_analyze[[x_var]],levels = unique(subsetted_df_to_analyze[[x_var]]))
      summary_df<-subsetted_df_to_analyze[ , !(names(subsetted_df_to_analyze) %in% group_dependence)]
      print(table(summary_df))
      if(class(subsetted_df_to_analyze[[y_var]])!="numeric"){
        print("Error: either y_order is unspecified for a non-numerical y variable, or your numerical y variable is not read as a numeric.",quote = FALSE)
      }
      cat(paste("","","",sep="\n"))



      #Testing for multiple observations within x_var groups using group_dependence
      testing_for_repeated_group_dependence_values<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
        x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
        testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
      }
      if(any(testing_for_repeated_group_dependence_values==FALSE)){
        print("There are repeated observations within individual x_var groups; A model must be fitted.",quote=FALSE)
        run_model=TRUE
      }else if(class(subsetted_df_to_analyze[[group_dependence]])=="numeric"){
        print("group_dependence is a numeric; A model must be fitted",quote=FALSE)
        run_model=TRUE
      }else{
        print("A model is unnecessary; normal tests will be run.",quote=FALSE)
        run_model=FALSE
      }




      if(run_model==TRUE){
        #Check if group_dependence is a random effect, if so then do lmer. If not do lm().
        if(class(x_levels)!="NULL"){
          levels(subsetted_df_to_analyze[[x_var]])<-x_levels
        }



        if(dependence_is_random_effect==TRUE){
          #It's a random effect, so do lmer.
          require(lme4)
          require(lmerTest)
          cat(paste("","",sep="\n"))
          print("group_dependence is a random effect, so a linear mixed model will be run.",quote=F)
          cat(paste("","",sep="\n"))

          formula_x_text<-paste("`",x_var,"`",sep="")
          formula_y_text<-paste("`",y_var,"`",sep="")
          formula_group_text<-paste("`",group_dependence,"`",sep="")
          formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
          random_slope_text<-paste("+(1+",formula_x_text,"|",formula_group_text,")",sep="")
          formula_text2<-paste(formula_text1,random_slope_text,sep="")
          formula_for_lmer_with_x_effect<-as.formula(formula_text2)

          lmer_with_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_with_x_effect,REML=F)

          formula_text_for_lmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
          formula_for_lmer_without_x_effect<-as.formula(formula_text_for_lmer_without_x_effect)
          lmer_without_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_without_x_effect,REML=F)

          #Creates output
          print(anova(lmer_without_x_effect,lmer_with_x_effect))
          cat(paste("","","","",sep="\n"))
          print(paste("Linear Mixed Model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
          cat(paste("","","",sep="\n"))
          print(summary(lmer_with_x_effect))


        }else if(dependence_is_random_effect==FALSE){
          #It's a fixed effect, so do lm()
          cat(paste("","",sep="\n"))
          print("group_dependence is a fixed effect, so a multivariable linear model will be run.",quote=F)
          cat(paste("","",sep="\n"))
          formula_x_text<-paste("`",x_var,"`",sep="")
          formula_y_text<-paste("`",y_var,"`",sep="")
          formula_group_dependence_text<-paste("`",group_dependence,"`",sep="")
          formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
          formula_text<-paste(formula_y_by_x,formula_group_dependence_text,sep="*")
          linear_model_formula<-as.formula(formula_text)
          print(summary(aov(data = subsetted_df_to_analyze,linear_model_formula)))
          cat(paste("","","",sep="\n"))
          print(paste("OLS linear model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
          print(summary(lm(data = subsetted_df_to_analyze,linear_model_formula,model = TRUE)))

        }else{
          print("Error: dependence_is_random_effect has an invalid value (not y or n).",quote=F)
        }


      }else if(run_model==FALSE){

        #Remove corresponding (according to group_dependence) observations to those removed earlier due to NAs
        cat(paste("","","",sep="\n"))
        no_of_groups<-length(unique(subsetted_df_to_analyze[[x_var]]))
        for (i in length(unique(subsetted_df_to_analyze[[group_dependence]])):1) {
          if (no_of_groups>length((subsetted_df_to_analyze[which(subsetted_df_to_analyze[[group_dependence]] == unique(subsetted_df_to_analyze[[group_dependence]])[i]),][[y_var]]))){
            val_to_remove<-unique(subsetted_df_to_analyze[[group_dependence]])[i]
            cat("Removing",val_to_remove,"from analysis as it has missing y_var values.","\n")
            subsetted_df_to_analyze<-subsetted_df_to_analyze[subsetted_df_to_analyze[[group_dependence]]!=val_to_remove,]
          }
        }
        cat(paste("","","",sep="\n"))

        #Friedman Test
        formula_x_text<-paste("`",x_var,"`",sep="")
        formula_y_text<-paste("`",y_var,"`",sep="")
        formula_group_text<-paste("`",group_dependence,"`",sep="")
        formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
        formula_text<-paste(formula_y_by_x,formula_group_text,sep="|")
        friedman_test_formula<-as.formula(formula_text)

        print(friedman.test(friedman_test_formula,data = subsetted_df_to_analyze))
        cat(paste("","","",sep="\n"))
        #Post-hoc tests
        require(PMCMRplus)
        require(PMCMR)
        print(posthoc.friedman.nemenyi.test(friedman_test_formula,data = subsetted_df_to_analyze))
        cat("Note: Nemenyi's post-hoc tests naturally account for the inflation of family-wise/Type I error,\nso p-value adjustment is unnecessary.")
      }else{
        print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know whether to run a linear model or another test.",quote=FALSE)
      }








    }else{
      #y variable is numerical
      subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]
      #Testing for normality
      looped_normality_output<-with(data=df_to_analyze_without_na,tapply(df_to_analyze_without_na[[y_var]],df_to_analyze_without_na[[x_var]],shapiro.test))
      print(looped_normality_output)
      normality.p.values<-c()
      normality.p.value_names<-c()
      for (i in 1:length(looped_normality_output)) {
        normality.p.values[i]<-looped_normality_output[[i]]$p.value
        normality.p.value_names[i]<-names(looped_normality_output)[i]
      }
      names(normality.p.values)<-normality.p.value_names
      cat(paste("","",sep="\n"))
      normality_p_values_to_print<-paste(signif(normality.p.values,digits = 4), collapse=", ")
      print(paste("p-values from testing x variable normality:",normality_p_values_to_print),quote=FALSE)
      #Printing the sample sizes
      x_group_names<-c()
      x_group_sample_sizes<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
        x_group_names[i]<-unique(subsetted_df_to_analyze[[x_var]])[i]
        x_group_sample_sizes[i]<-length(subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),][[x_var]])
      }
      names(x_group_sample_sizes)<-x_group_names
      cat(paste("","",sep="\n"))
      sample_size_df<-data.frame(x_group_names,x_group_sample_sizes)
      sample_size_names<-c("x Variable Group","Sample Size")
      colnames(sample_size_df)<-sample_size_names
      print(sample_size_df,row.names=F)
      cat(paste("","",sep="\n"))
      #Determines whether to run parametric or non-parametric tests
      parametric_or_not_vector<-c()
      for (i in 1:length(normality.p.values)) {
        sample_size_column<-match(names(normality.p.values)[i],names(x_group_sample_sizes))
        parametric_or_not_vector[i]<-(15<x_group_sample_sizes[sample_size_column]&&normality.p.values[i]>0.05)|x_group_sample_sizes[sample_size_column]>30
      }
      if(all(parametric_or_not_vector==TRUE)){
        print("There is normality and a sufficient sample size to run a parametric test.", quote = FALSE)
        subsetted_df_to_analyze[[x_var]]<-as.factor(subsetted_df_to_analyze[[x_var]])
        #Gives a summary of each group
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
          cat(paste("","",sep="\n"))
          df_with_i_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),]
          print(summary(df_with_i_x_group))
          print(paste("Standard Deviation:",signif(sd(df_with_i_x_group[[y_var]]),digits = 4)),quote=FALSE)
        }
        cat(paste("","","",sep="\n"))


        #Testing for multiple observations within x_var groups using group_dependence
        testing_for_repeated_group_dependence_values<-c()
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
          x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
          testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
        }
        if(any(testing_for_repeated_group_dependence_values==FALSE)){
          print("There are repeated observations within individual x_var groups; A model must be fitted.",quote=FALSE)
          run_model=TRUE
        }else if(class(subsetted_df_to_analyze[[group_dependence]])=="numeric"){
          print("group_dependence is a numeric; A model must be fitted",quote=FALSE)
          run_model=TRUE
        }else{
          print("A model is unnecessary; normal tests will be run.",quote=FALSE)
          run_model=FALSE
        }



        if(run_model==TRUE){

          #Check if group_dependence is a random effect, if so then do lmer. If not do lm().
          if(class(x_levels)!="NULL"){
            levels(subsetted_df_to_analyze[[x_var]])<-x_levels
          }



          if(dependence_is_random_effect==TRUE){
            #It's a random effect, so do lmer.
            require(lme4)
            require(lmerTest)
            cat(paste("","",sep="\n"))
            print("group_dependence is a random effect, so a linear mixed model will be run.",quote=F)
            cat(paste("","",sep="\n"))

            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_text<-paste("`",group_dependence,"`",sep="")
            formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
            random_slope_text<-paste("+(1+",formula_x_text,"|",formula_group_text,")",sep="")
            formula_text2<-paste(formula_text1,random_slope_text,sep="")
            formula_for_lmer_with_x_effect<-as.formula(formula_text2)

            lmer_with_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_with_x_effect,REML=F)

            formula_text_for_lmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
            formula_for_lmer_without_x_effect<-as.formula(formula_text_for_lmer_without_x_effect)
            lmer_without_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_without_x_effect,REML=F)

            #Creates output
            print(anova(lmer_without_x_effect,lmer_with_x_effect))
            cat(paste("","","","",sep="\n"))
            print(paste("Linear Mixed Model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            cat(paste("","","",sep="\n"))
            print(summary(lmer_with_x_effect))


          }else if(dependence_is_random_effect==FALSE){
            #It's a fixed effect, so do lm()

            cat(paste("","",sep="\n"))
            print("group_dependence is a fixed effect, so a multivariable linear model will be run.",quote=F)
            cat(paste("","",sep="\n"))
            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_dependence_text<-paste("`",group_dependence,"`",sep="")
            formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
            formula_text<-paste(formula_y_by_x,formula_group_dependence_text,sep="*")
            linear_model_formula<-as.formula(formula_text)
            print(summary(aov(data = subsetted_df_to_analyze,linear_model_formula)))
            cat(paste("","","",sep="\n"))
            print(paste("OLS linear model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            print(summary(lm(data = subsetted_df_to_analyze,linear_model_formula,model = TRUE)))

          }else{
            print("Error: dependence_is_random_effect has an invalid value (not y or n).",quote=F)
          }


        }else if(run_model==FALSE){
          #Interval and normal: One-way repeated measures ANOVA.
          require(car)

          #Remove corresponding (according to group_dependence) observations to those removed earlier due to NAs
          cat(paste("","","",sep="\n"))
          no_of_groups<-length(unique(subsetted_df_to_analyze[[x_var]]))
          for (i in length(unique(subsetted_df_to_analyze[[group_dependence]])):1) {
            if (no_of_groups>length((subsetted_df_to_analyze[which(subsetted_df_to_analyze[[group_dependence]] == unique(subsetted_df_to_analyze[[group_dependence]])[i]),][[y_var]]))){
              val_to_remove<-unique(subsetted_df_to_analyze[[group_dependence]])[i]
              cat("Removing",val_to_remove,"from analysis as it has missing y_var values.","\n")
              subsetted_df_to_analyze<-subsetted_df_to_analyze[subsetted_df_to_analyze[[group_dependence]]!=val_to_remove,]
            }
          }
          cat(paste("","","",sep="\n"))


          subsetted_df_to_analyze[[group_dependence]]<-factor(subsetted_df_to_analyze[[group_dependence]])

          #Preparing the data and model parameters
          require(tidyr)
          wide_df<-spread(subsetted_df_to_analyze,x_var,y_var)
          #This is needed for the Anova on the model design
          x_var_levels<-c()
          for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
            x_var_levels[i]<-i
          }
          x_var_factor<-as.factor(x_var_levels)
          x_var_df<-as.data.frame(x_var_factor)
          #Formatting the data
          model_wide_df<-wide_df[ , !(names(wide_df) %in% group_dependence)]
          model_matrix<-as.matrix(model_wide_df)

          #Running the model
          repeated_measures_linear_model<-lm(model_matrix~1)
          repeated_measures_anova<-Anova(repeated_measures_linear_model,idata = x_var_df,idesign = ~x_var_factor,type="III")
          print(summary(repeated_measures_anova,multivariate=FALSE))
          #Post-hoc pairwise paired t-tests
          cat(paste("","","",sep="\n"))
          pairwise.t.test(x=subsetted_df_to_analyze[[y_var]],g=subsetted_df_to_analyze[[x_var]],p.adjust.method = "BH",paired = TRUE)
        }else{
          print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know whether to run a linear model or another test.",quote=FALSE)
        }









      }else if(any(parametric_or_not_vector==FALSE)){
        print("There is not normality and/or a sufficient sample size to run a parametric test; non-parametric tests will be conducted.", quote=FALSE)
        subsetted_df_to_analyze[[x_var]]<-factor(subsetted_df_to_analyze[[x_var]],levels = unique(subsetted_df_to_analyze[[x_var]]))
        #Gives a summary of each group
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
          cat(paste("","",sep="\n"))
          df_with_i_x_group <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),]
          print(summary(df_with_i_x_group))
          print(paste("Standard Deviation:",signif(sd(df_with_i_x_group[[y_var]]),digits = 4)),quote=FALSE)
        }
        cat(paste("","","",sep="\n"))

        #Testing for multiple observations within x_var groups using group_dependence
        testing_for_repeated_group_dependence_values<-c()
        for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
          x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
          testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
        }
        if(any(testing_for_repeated_group_dependence_values==FALSE)){
          print("There are repeated observations within individual x_var groups; A model must be fitted.",quote=FALSE)
          run_model=TRUE
        }else if(class(subsetted_df_to_analyze[[group_dependence]])=="numeric"){
          print("group_dependence is a numeric; A model must be fitted",quote=FALSE)
          run_model=TRUE
        }else{
          print("A model is unnecessary; normal tests will be run.",quote=FALSE)
          run_model=FALSE
        }


        if(run_model==TRUE){
          #Check if group_dependence is a random effect, if so then do lmer. If not do lm().
          if(class(x_levels)!="NULL"){
            levels(subsetted_df_to_analyze[[x_var]])<-x_levels
          }



          if(dependence_is_random_effect==TRUE){
            #It's a random effect, so do lmer.
            require(lme4)
            require(lmerTest)
            cat(paste("","",sep="\n"))
            print("group_dependence is a random effect, so a linear mixed model will be run.",quote=F)
            cat(paste("","",sep="\n"))

            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_text<-paste("`",group_dependence,"`",sep="")
            formula_text1<-paste(formula_y_text,formula_x_text,sep="~")
            random_slope_text<-paste("+(1+",formula_x_text,"|",formula_group_text,")",sep="")
            formula_text2<-paste(formula_text1,random_slope_text,sep="")
            formula_for_lmer_with_x_effect<-as.formula(formula_text2)

            lmer_with_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_with_x_effect,REML=F)

            formula_text_for_lmer_without_x_effect<-paste(formula_y_text,"~1+(1|",formula_group_text,")",sep="")
            formula_for_lmer_without_x_effect<-as.formula(formula_text_for_lmer_without_x_effect)
            lmer_without_x_effect<-lmer(data=subsetted_df_to_analyze,formula_for_lmer_without_x_effect,REML=F)

            #Creates output
            print(anova(lmer_without_x_effect,lmer_with_x_effect))
            cat(paste("","","","",sep="\n"))
            print(paste("Linear Mixed Model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            cat(paste("","","",sep="\n"))
            print(summary(lmer_with_x_effect))


          }else if(dependence_is_random_effect==FALSE){
            #It's a fixed effect, so do lm()

            cat(paste("","",sep="\n"))
            print("group_dependence is a fixed effect, so a multivariable linear model will be run.",quote=F)
            cat(paste("","",sep="\n"))
            formula_x_text<-paste("`",x_var,"`",sep="")
            formula_y_text<-paste("`",y_var,"`",sep="")
            formula_group_dependence_text<-paste("`",group_dependence,"`",sep="")
            formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
            formula_text<-paste(formula_y_by_x,formula_group_dependence_text,sep="*")
            linear_model_formula<-as.formula(formula_text)
            print(summary(aov(data = subsetted_df_to_analyze,linear_model_formula)))
            cat(paste("","","",sep="\n"))
            print(paste("OLS linear model comparing against reference group:"," '",levels(subsetted_df_to_analyze[[x_var]])[1],"'",sep=""),quote=FALSE)
            print(summary(lm(data = subsetted_df_to_analyze,linear_model_formula,model = TRUE)))

          }else{
            print("Error: dependence_is_random_effect has an invalid value (not y or n).",quote=F)
          }

        }else if(run_model==FALSE){
          print("There are no repeated observations within individual x_var groups. Normal tests will be run.",quote=FALSE)

          #Remove corresponding (according to group_dependence) observations to those removed earlier due to NAs
          cat(paste("","","",sep="\n"))
          no_of_groups<-length(unique(subsetted_df_to_analyze[[x_var]]))
          for (i in length(unique(subsetted_df_to_analyze[[group_dependence]])):1) {
            if (no_of_groups>length((subsetted_df_to_analyze[which(subsetted_df_to_analyze[[group_dependence]] == unique(subsetted_df_to_analyze[[group_dependence]])[i]),][[y_var]]))){
              val_to_remove<-unique(subsetted_df_to_analyze[[group_dependence]])[i]
              cat("Removing",val_to_remove,"from analysis as it has missing y_var values.","\n")
              subsetted_df_to_analyze<-subsetted_df_to_analyze[subsetted_df_to_analyze[[group_dependence]]!=val_to_remove,]
            }
          }
          cat(paste("","","",sep="\n"))

          #Friedman Test
          formula_x_text<-paste("`",x_var,"`",sep="")
          formula_y_text<-paste("`",y_var,"`",sep="")
          formula_group_text<-paste("`",group_dependence,"`",sep="")
          formula_y_by_x<-paste(formula_y_text,formula_x_text,sep="~")
          formula_text<-paste(formula_y_by_x,formula_group_text,sep="|")
          friedman_test_formula<-as.formula(formula_text)

          print(friedman.test(friedman_test_formula,data = subsetted_df_to_analyze))
          cat(paste("","","",sep="\n"))
          #Post-hoc tests
          require(PMCMRplus)
          require(PMCMR)
          print(posthoc.friedman.nemenyi.test(friedman_test_formula,data = subsetted_df_to_analyze))
          cat("Note: Nemenyi's post-hoc tests naturally account for the inflation of family-wise/Type I error,\nso p-value adjustment is unnecessary.")

        }else{
          print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know whether to run a linear model or another test.",quote=FALSE)
        }
      }else{
        print("Error: The function does not know whether to run a parametric or non-parametric test for more than two independent x groups and numerical y.",quote = FALSE)
      }
    }
  }else{
    print("Error - stats_to_run is undefined or unrecognized.")
  }
}
