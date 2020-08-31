
#Script to create demographics/description tables of variables

#TODO: Add it to maxfunctions

# df_to_analyze<-DiabetesDataFinal
# x_var<-"DiagnosisDR"
# y_vars<-names(DiabetesDataFinal)[8:15]

# table<-demographics.table(df_to_analyze = subset(DiabetesDataFinal,DiagnosisDR=="DR"),
#                           x_var = "Diagnosis",
#                           y_vars = names(DiabetesDataFinal)[16:21])
#
# require(openxlsx)
# write.xlsx(table,"Dropbox/SERI/Pupillometry Data/Summer 2020 Analyses/Processed Data/DiabetesDataDemographicsTable4.xlsx")


# library(readxl)
# ComparingGlaucInHighMyopes <- read_excel("~/Dropbox/SERI/Pupillometry Data/SUMMER 2019 DATA/CompiledAnalysisFiles/ComparingGlaucInHighMyopes.xlsx",
#                                          sheet = "WHOClassified")

# df_to_analyze<-ComparingGlaucInHighMyopes
# x_var<-"Myopia"
# y_vars<-c("CataractStatusDetailed")
# y_var<-"CataractStatusDetailed"
# sd=3




demographics.table<-function(df_to_analyze,x_var,y_vars,sd=3){
  "
  Main function to create demographics tables.
  Does not automatically add p-values/show significance.
  Inputs:
  df_to_analyze: the dataframe with the data
  x_var: chr name of the column in df_to_analyze whose groups will be described
  y_vars: vector of chr names of columns in df_to_analyze with demographics to analyze
  sd: for numeric y_vars; If 1 displays mean (sd), if 2 displays median (IQR), if 3
      then it assesses the data and chooses automatically. Else error.

  Returns the demographics table as a dataframe.
  "

  #Start output table with column names according to x_var
  output_table<-data.frame()
  x_groups<-names(table(df_to_analyze[[x_var]]))

  #Add counts to column names and define output_table_col_names
  output_table_col_names<-c("Demographic Characteristics")
  counts<-table(df_to_analyze[[x_var]])

  for (i in 1:length(names(counts))) {
    col_name<-paste(names(counts)[i]," (n=",counts[i],")",sep="")
    output_table_col_names<-c(output_table_col_names,col_name)
  }


  #Looping through each variable in y_vars to summarize
  for (i in 1:length(y_vars)) {
    y_var<-y_vars[i]

    require(maxfunctions)
    df_to_analyze_without_na<-remove.rows.with.na.in.specified.column(df_to_analyze,c(x_var,y_var))

    #Check if y_var is a chr or num, then create the row
    if(class(df_to_analyze_without_na[[y_var]])=="character"|class(df_to_analyze_without_na[[y_var]])=="factor"){
      #y_var is a chr
      #Count number of groups
      number_of_y_groups<-length(unique(df_to_analyze_without_na[[y_var]]))
      print(paste(y_var,"is a categorical/ordinal variable with",number_of_y_groups,"levels."),quote=F)

      #Determine how many rows to print based on number_of_y_groups
      if(number_of_y_groups>2){
        #Create blank first output row with y_var
        #note output_row will actually be multiple rows in this case; otherwise is 1 row
        output_row<-c(y_var,rep("",length(x_groups)))
        names(output_row)<-output_table_col_names

        #Add a row for each level of y_var showing % within each x group
        for (j in 1:number_of_y_groups) {
          #Select y_group values based on descending order of counts
          y_group<-names(table(df_to_analyze_without_na[[y_var]]))[order(table(df_to_analyze_without_na[[y_var]]),decreasing = T)][j]

          #Calculate counts and proportions of y_group for each x_group, then add to row
          row_to_add<-c(paste(y_group,", no. (%)",sep = ""))

          for (k in 1:length(x_groups)) {
            x_group<-x_groups[k]
            x_group_df<-df_to_analyze_without_na[df_to_analyze_without_na[[x_var]]==x_group,]

            #For that x_group, calculate the count and proportion of y_group in y_var

            y_group_count<-sum(x_group_df[[y_var]]==y_group)
            y_group_proportion<-sprintf('%.1f',100*sum(x_group_df[[y_var]]==y_group)/sum(table(x_group_df[[y_var]])))

            y_group_description<-paste(y_group_count," (",y_group_proportion,")",sep = "")
            row_to_add<-c(row_to_add,y_group_description)
          }

          #Add the row to output_row
          names(row_to_add)<-output_table_col_names
          output_row<-rbind(output_row,row_to_add)
        }



      }else if(number_of_y_groups==1|number_of_y_groups==2){
        #Create row of counts and proportions of y_group for each x_group
        if(any(df_to_analyze_without_na[[y_var]]=="1")){
          #If y_var has a "1" (eg in successes/failures), set y_group to 1
          y_group<-"1"
        }else{
          #Else set y_group to the most common group in y_var
          y_group<-names(table(df_to_analyze_without_na[[y_var]]))[order(table(df_to_analyze_without_na[[y_var]]),decreasing = T)][1]
        }

        output_row<-c(paste(y_var,", no. ",y_group," (%)",sep = ""))

        for (k in 1:length(x_groups)) {
          x_group<-x_groups[k]
          x_group_df<-df_to_analyze_without_na[df_to_analyze_without_na[[x_var]]==x_group,]

          #For that x_group, calculate the count and proportion of y_group in y_var
          y_group_count<-sum(x_group_df[[y_var]]==y_group)
          y_group_proportion<-sprintf('%.1f',100*sum(x_group_df[[y_var]]==y_group)/sum(table(x_group_df[[y_var]])))

          x_group_description<-paste(y_group_count," (",y_group_proportion,")",sep = "")
          output_row<-c(output_row,x_group_description)

        }

        #Prepare for rbinding
        names(output_row)<-output_table_col_names
        output_row<-t(data.frame(output_row))



      }else{
        print("There is an error in the counted number_of_y_groups.",quote=F)
      }


    }else if(class(df_to_analyze_without_na[[y_var]])=="numeric"){
      #y_var is a numeric
      #Check if sd is 1, 2, or 3 to determine whether to print mean (SD) or median (IQR)
      if(sd==1){
        parametric<-T
      }else if(sd==2){
        parametric<-F
      }else if(sd==3){
        #Display as mean (SD) or median (IQR) depending on data structure
        #Detect data structure, then reassign sd<-1 or sd<-2 accordingly
        print(paste(y_var,"is a continuous variable and showing mean vs median will be chosen automatically."),quote=F)

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
        for (i in 1:length(unique(df_to_analyze_without_na[[x_var]]))){
          x_group_names[i]<-unique(df_to_analyze_without_na[[x_var]])[i]
          x_group_sample_sizes[i]<-length(df_to_analyze_without_na[which(df_to_analyze_without_na[[x_var]] == unique(df_to_analyze_without_na[[x_var]])[i]),][[x_var]])
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
          #Parametric tests are valid. Set sd<-1 for mean (SD)
          parametric<-T
        }else if(any(parametric_or_not_vector==FALSE)){
          #Nonparametric tests are valid. Set sd<-2 for median (IQR)
          parametric<-F
        }else{
          print(paste("Error: The function does not know whether",y_var,"is parametric."),quote = FALSE)
        }

      }else{
        print("Invalid sd input. Please enter 1, 2, or 3.",quote=F)
      }


      #Calculate statistics and generate row
      if(parametric==T){
        #Display as mean (SD)
        print(paste(y_var,"is a continuous variable and will be represented as mean (SD)."),quote=F)

        output_row<-c(paste(y_var,", mean (SD)",sep = ""))

        for (k in 1:length(x_groups)) {
          x_group<-x_groups[k]
          x_group_df<-df_to_analyze_without_na[df_to_analyze_without_na[[x_var]]==x_group,]

          #For that x_group, calculate the mean and sd of y_var
          y_mean<-sprintf('%.1f',mean(x_group_df[[y_var]]))
          y_sd<-sprintf('%.1f',sd(x_group_df[[y_var]]))

          x_group_description<-paste(y_mean," (",y_sd,")",sep = "")
          output_row<-c(output_row,x_group_description)
        }

        #Prepare for rbinding
        names(output_row)<-output_table_col_names
        output_row<-t(data.frame(output_row))

      }else if(parametric==F){
        #Display as median (IQR)
        print(paste(y_var,"is a continuous variable and will be represented as median (IQR)."),quote=F)

        output_row<-c(paste(y_var,", median (IQR)",sep = ""))

        for (k in 1:length(x_groups)) {
          x_group<-x_groups[k]
          x_group_df<-df_to_analyze_without_na[df_to_analyze_without_na[[x_var]]==x_group,]

          #For that x_group, calculate the mean and sd of y_var
          y_median<-sprintf('%.1f',median(x_group_df[[y_var]]))
          y_IQR<-sprintf('%.1f',IQR(x_group_df[[y_var]]))

          x_group_description<-paste(y_median," (",y_IQR,")",sep = "")
          output_row<-c(output_row,x_group_description)
        }

        #Prepare for rbinding
        names(output_row)<-output_table_col_names
        output_row<-t(data.frame(output_row))

      }else{
        print(quote=F,paste("The function doesn't know whether",y_var,"is parametric or nonparametric."))
      }

    }else{
      print("There is an error - y var is not categorical or numerical.",quote = F)

    }

    #Add output_row to output_table
    output_table<-rbind(output_table,output_row)

  }

  #remove output_table rownames
  rownames(output_table)<-NULL
  return(output_table)


}
