# Instructions ------------------------------------------------------------
#Only works for categorical x, numerical y
#bars defines error bars for parametric graphs: can be "SD", "SE", "CI"

# p<-list()
# for (i in 4:7){
#   y_var<-names(Analyzing_the_Preliminary_DR_Data_on_RStudio)[i]
#   p[[i-3]]<-quick.graph(df_to_analyze = Analyzing_the_Preliminary_DR_Data_on_RStudio,x_var = "Diagnosis",y_var = y_var)
# }
#
# do.call(grid.arrange,p)

#TODO: Make it additionally draw a histogram regardless of data features.




# quick.graph ---------------------------------------------------------


#Major Graph Function
quick.graph<-function(df_to_analyze,x_var,y_var,group_dependence=FALSE,bars="SD"){
  #Starts the output with intro; useful for looping
  require(ggplot2)
  cat(paste("","","","",sep="\n"))
  print(paste("x variable:",x_var),quote=FALSE)
  print(paste("y variable:",y_var),quote=FALSE)
  cat(paste("","",sep="\n"))

  #Makes the x_variable a character/group - important if groups are eg 1, 2, 3, 4
  df_to_analyze[[x_var]]<-as.character(df_to_analyze[[x_var]])
  df_to_analyze_without_na<-remove.rows.with.na.in.specified.column(df_to_analyze,c(x_var,y_var))
  if(is.numeric(df_to_analyze_without_na[[y_var]])==FALSE){
    print("y_var is not a numeric; please reclassify.",quote=FALSE)
  }else{
    graph_to_draw<-define.graph.using.sample.size.and.normality(df_to_analyze_without_na,x_var,y_var,
                                                                group_dependence)
    print(graph_to_draw, quote=FALSE)
    #Draw the graph
    graph_that_is_drawn<-draw.the.graph(df_to_analyze_without_na,group_dependence,x_var,y_var,graph_to_draw,bars)
    return(graph_that_is_drawn)
  }
}


# Removes rows with NA values in certain columns ------------------------------------------------------

remove.rows.with.na.in.specified.column <- function(df_to_analyze, columns_with_na) {
  complete_rows_vector <- complete.cases(df_to_analyze[, columns_with_na])
  return(df_to_analyze[complete_rows_vector, ])
}

# Define graph using sample size and distribution ------------------------------


#Defines stats using number of x groups and group dependence if y is numerical
define.graph.using.sample.size.and.normality<-function(df_to_analyze_without_na,x_var,y_var,
                                                       group_dependence){


  #y variable is numerical
  subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
  #Testing for normality
  looped_normality_output<-with(data=df_to_analyze_without_na,tapply(df_to_analyze_without_na[[y_var]],df_to_analyze_without_na[[x_var]],shapiro.test))



  #TEMP
  print(looped_normality_output)



  normality.p.values<-c()
  normality.p.value_names<-c()
  for (i in 1:length(looped_normality_output)) {
    normality.p.values[i]<-looped_normality_output[[i]]$p.value
    normality.p.value_names[i]<-names(looped_normality_output)[i]
  }
  names(normality.p.values)<-normality.p.value_names



  #TEMP
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




  #TEMP
  cat(paste("","",sep="\n"))
  sample_size_df<-data.frame(x_group_names,x_group_sample_sizes)
  sample_size_names<-c("x Variable Group","Sample Size")
  colnames(sample_size_df)<-sample_size_names
  print(sample_size_df)
  cat(paste("","",sep="\n"))





  #Determines whether to run parametric or non-parametric tests
  parametric_or_not_vector<-c()
  for (i in 1:length(normality.p.values)) {
    sample_size_column<-match(names(normality.p.values)[i],names(x_group_sample_sizes))
    parametric_or_not_vector[i]<-(15<x_group_sample_sizes[sample_size_column]&&normality.p.values[i]>0.05)|x_group_sample_sizes[sample_size_column]>30
  }

  if(all(parametric_or_not_vector==TRUE)){

    if(group_dependence==FALSE){
      graph_to_draw<-"There is normality and a sufficient sample size to draw an independent parametric graph"

    }else if(group_dependence!=FALSE){
      graph_to_draw<-"There is normality and a sufficient sample size to draw a dependent parametric graph"

    }else{
      graph_to_draw<-"Error - parametric graph but unknown group dependence"
    }

  }else if(any(parametric_or_not_vector==FALSE)){


    if(group_dependence==FALSE){
      graph_to_draw<-"There is not normality and/or a sufficient sample size to draw a parametric graph; independent non-parametric graphs will be drawn."

    }else if(group_dependence!=FALSE){
      graph_to_draw<-"There is not normality and/or a sufficient sample size to draw a parametric graph; dependent non-parametric graphs will be drawn."

    }else{
      graph_to_draw<-"Error - non-parametric graph but unknown group dependence"
    }

    #Checks if sample size cals for violin or dotplots
  }else{
    graph_to_draw<-"Error: The function does not know whether to run a parametric or non-parametric test for more than two independent x groups and numerical y."
  }
  return(graph_to_draw)
}
# draw.the.graph -----------------------------------------------------------


#Draws the actual graph using the defined type of graph needed
draw.the.graph<-function(df_to_analyze_without_na,group_dependence,x_var,y_var,graph_to_draw,bars){
  require(ggplot2)
  if(graph_to_draw=="Error - parametric graph but unknown group dependence"){
    print("Error- Unable to run stats as graph_to_draw contains an error.",quote=FALSE)
  }else if(graph_to_draw=="Error - non-parametric graph but unknown group dependence"){
    print("Error- Unable to run stats as graph_to_draw contains an error.",quote=FALSE)
  }else if(graph_to_draw=="Error: The function does not know whether to run a parametric or non-parametric test for more than two independent x groups and numerical y."){
    print("Error- Unable to run stats as graph_to_draw contains an error.",quote=FALSE)

    # Draw Graph: Parametric, independent data ---------------------------------

  }else if(graph_to_draw=="There is normality and a sufficient sample size to draw an independent parametric graph"){
    #For parametric independent data
    #Bar Graphs with SD/SE

    #Prepares data to collect stats for bar graph
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]
    subsetted_df_to_analyze$ExtraRowToAddWithNamesAsNumbersToRemoveLater<-c(1:length(subsetted_df_to_analyze[[x_var]]))
    require(tidyr)
    spreaded_data<-spread(subsetted_df_to_analyze,key = x_var,value = y_var)

    #Builds stats table
    stats_table<-data.frame()

    for (i in 2:length(names(spreaded_data))) {
      column_without_na<-remove.rows.with.na.in.specified.column(spreaded_data,names(spreaded_data)[i])[i]
      matrix_column<-as.matrix(column_without_na)
      initial_stats<-data.frame(mean(matrix_column[,1]),sd(matrix_column[,1]))
      names(initial_stats)<-c("mean","SD")
      initial_stats$SE<-initial_stats$SD/sqrt(length(matrix_column))
      initial_stats$CI<-1.96*initial_stats$SD
      initial_stats$x_group<-names(column_without_na)
      stats_table<-rbind(stats_table,initial_stats)
    }

    #Builds graph labels
    y_label<-paste("Mean",y_var)

    #builds bar graph
    graph_that_is_drawn<-ggplot(data = stats_table,aes(x=x_group,y=mean))+
      geom_col(position = "dodge")+xlab(x_var)+ylab(y_label)+
      geom_errorbar(aes(ymax=stats_table$mean+stats_table[[bars]],ymin=stats_table$mean-stats_table[[bars]]),position=position_dodge(0.9),width=0.3)+
      theme_bw()+theme(plot.title=element_text(size=14,face="bold",hjust = 0.5),
                       axis.title.y = element_text(size = 16),
                       axis.text.x = element_text(size=12),
                       axis.title.x = element_text(size=16))

    return(graph_that_is_drawn)
    # Draw Graph: Parametric, dependent data ------------

  }else if(graph_to_draw=="There is normality and a sufficient sample size to draw a dependent parametric graph"){
    #For parametric dependent data
    #Spaghetti plots of mean and SD/SE for each group

    #Prepares data to collect stats for graph
    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]

    #TESTING FOR REPEATED GROUP_DEPENDENCE VALUES
    testing_for_repeated_group_dependence_values<-c()
    for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
      x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
      testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
    }

    if(any(testing_for_repeated_group_dependence_values==FALSE)){
      print("There are repeated observations within individual x_var groups.",quote=FALSE)

      #Deals with each group_dependence line to draw individually to build a dataframes

      stats_table<-data.frame()

      for (i in 1:length(unique(subsetted_df_to_analyze[[group_dependence]]))) {
        df_with_unique_group_dependence <- subsetted_df_to_analyze[which(subsetted_df_to_analyze[[group_dependence]] == unique(subsetted_df_to_analyze[[group_dependence]])[i]),]

        #Formats data for spreading
        df_with_unique_group_dependence$ExtraRowToAddWithNamesAsNumbersToRemoveLater<-c(1:length(df_with_unique_group_dependence[[x_var]]))
        df_with_unique_group_dependence[[group_dependence]]<-NULL

        #Spreads data
        require(tidyr)
        spreaded_data<-spread(df_with_unique_group_dependence,key = x_var,value = y_var)

        #Builds stats table

        for (j in 2:length(names(spreaded_data))) {
          column_without_na<-remove.rows.with.na.in.specified.column(spreaded_data,names(spreaded_data)[j])[j]
          matrix_column<-as.matrix(column_without_na)
          initial_stats<-data.frame(mean(matrix_column[,1]),sd(matrix_column[,1]))
          names(initial_stats)<-c("mean","SD")
          initial_stats$SE<-initial_stats$SD/sqrt(length(matrix_column))
          initial_stats$CI<-1.96*initial_stats$SD
          initial_stats$x_group<-names(column_without_na)
          initial_stats$dependent_groups<-unique(subsetted_df_to_analyze[[group_dependence]])[i]
          stats_table<-rbind(stats_table,initial_stats)
        }
      }

      #Builds graph labels
      y_label<-paste("Mean",y_var)

      #builds graph
      graph_that_is_drawn<-ggplot(data = stats_table,aes(x=x_group,y=mean,group=dependent_groups))+
        geom_line(data = stats_table,aes(x=x_group,y=mean,group=dependent_groups),alpha=0.7)+
        theme_bw()+xlab(x_var)+ylab(y_label)+labs(shape=group_dependence)+
        geom_point(data = stats_table,aes(x=x_group,y=mean,shape=dependent_groups))+
        geom_errorbar(aes(ymax=stats_table$mean+stats_table[[bars]],ymin=stats_table$mean-stats_table[[bars]]),width=0.3,position = position_dodge(0.03))+
        theme_bw()+theme(plot.title=element_text(size=14,face="bold",hjust = 0.5),
                         axis.title.y = element_text(size = 16),
                         axis.text.x = element_text(size=12),
                         axis.title.x = element_text(size=16))+
        theme(legend.text = element_text(size = 12),legend.title = element_text(size=14))

    }else if(all(testing_for_repeated_group_dependence_values==TRUE)){
      print("There are no repeated observations within individual x_var groups.",quote=FALSE)

      require(tidyr)
      spreaded_data<-spread(subsetted_df_to_analyze,key = x_var,value = y_var)

      #Builds stats table
      stats_table<-data.frame()

      for (i in 2:length(names(spreaded_data))) {
        column_without_na<-remove.rows.with.na.in.specified.column(spreaded_data,names(spreaded_data)[i])[i]
        matrix_column<-as.matrix(column_without_na)
        initial_stats<-data.frame(mean(matrix_column[,1]),sd(matrix_column[,1]))
        names(initial_stats)<-c("mean","SD")
        initial_stats$SE<-initial_stats$SD/sqrt(length(matrix_column))
        initial_stats$CI<-1.96*initial_stats$SD
        initial_stats$x_group<-names(column_without_na)
        initial_stats$dependent_groups<-"Population"
        stats_table<-rbind(stats_table,initial_stats)
      }

      #Builds graph labels
      y_label<-paste("Mean",y_var)

      #builds graph
      graph_that_is_drawn<-ggplot(data = stats_table,aes(x=x_group,y=mean,group=dependent_groups))+
        geom_line(alpha=0.7)+
        theme_bw()+xlab(x_var)+ylab(y_label)+
        geom_point()+
        geom_errorbar(aes(ymax=stats_table$mean+stats_table[[bars]],ymin=stats_table$mean-stats_table[[bars]]),position=position_dodge(0.9),width=0.3)+
        theme_bw()+theme(plot.title=element_text(size=14,face="bold",hjust = 0.5),
                         axis.title.y = element_text(size = 16),
                         axis.text.x = element_text(size=12),
                         axis.title.x = element_text(size=16))

    }else{
      print("There is an error - the function cannot detect, for at least one x_var group, whether group_dependence has any repeated values; The function does not know how many lines it must draw.",quote=FALSE)
    }

    return(graph_that_is_drawn)

    # Draw Graph: Non-parametric, independent data --------------

  }else if(graph_to_draw=="There is not normality and/or a sufficient sample size to draw a parametric graph; independent non-parametric graphs will be drawn."){
    #For non-parametric independent data
    #Boxplot with dotplot

    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var)]

    require(reshape2)
    melted_data_to_plot<-melt(subsetted_df_to_analyze,id.vars = x_var)

    graph_that_is_drawn<-ggplot(melted_data_to_plot,aes(x=melted_data_to_plot[[x_var]],y=value))+
      geom_boxplot(outlier.colour="black",outlier.shape=4,outlier.size=8,fill="gray")+
      geom_dotplot(binaxis="y",stackdir="center",dotsize = .75,fill="black")+
      labs(y=y_var,x=x_var) + theme_bw()+
      theme(plot.title=element_text(size=14,face="bold",hjust = 0.5),
             axis.title.y = element_text(size = 16),
             axis.text.x = element_text(size=12),
             axis.title.x = element_text(size=16))

    return(graph_that_is_drawn)

    # Draw Graph: Non-parametric, dependent data -----------

  }else if(graph_to_draw=="There is not normality and/or a sufficient sample size to draw a parametric graph; dependent non-parametric graphs will be drawn."){
    #For non-parametric dependent data
    #Spaghetti median plots with boxplots / just spaghetti based on sample size: 15

    subsetted_df_to_analyze<-df_to_analyze_without_na[,names(df_to_analyze_without_na)%in%c(x_var,y_var,group_dependence)]

    #TESTING FOR REPEATED GROUP_DEPENDENCE VALUES
    testing_for_repeated_group_dependence_values<-c()
    for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))) {
      x_var_to_test<-subset(subsetted_df_to_analyze,subsetted_df_to_analyze[[x_var]]==unique(subsetted_df_to_analyze[[x_var]])[i])
      testing_for_repeated_group_dependence_values[i]<-length(x_var_to_test[[group_dependence]])==length(unique(x_var_to_test[[group_dependence]]))
    }

    if(any(testing_for_repeated_group_dependence_values==FALSE)){
      print("There are repeated observations within individual x_var groups.",quote=FALSE)

      #builds graph
      graph_that_is_drawn<-ggplot(data=subsetted_df_to_analyze,aes(x=subsetted_df_to_analyze[[x_var]],y=subsetted_df_to_analyze[[y_var]],fill=subsetted_df_to_analyze[[group_dependence]]))+
        geom_boxplot(outlier.colour="black",outlier.shape=4,outlier.size=8)+
        geom_dotplot(binaxis="y",stackdir="center",dotsize = .75,position = position_dodge(width = 0.75))+
        labs(y=y_var,x=x_var,fill=group_dependence) + theme_bw()+
        theme(plot.title=element_text(size=14,face="bold",hjust = 0.5),
               axis.title.y = element_text(size = 16),
               axis.text.x = element_text(size=12),
               axis.title.x = element_text(size=16))

    }else if(all(testing_for_repeated_group_dependence_values==TRUE)){
      print("There are no repeated observations within individual x_var groups.",quote=FALSE)

      #Decides whether to show boxplots or all data
      x_group_names<-c()
      x_group_sample_sizes<-c()
      for (i in 1:length(unique(subsetted_df_to_analyze[[x_var]]))){
        x_group_names[i]<-unique(subsetted_df_to_analyze[[x_var]])[i]
        x_group_sample_sizes[i]<-length(subsetted_df_to_analyze[which(subsetted_df_to_analyze[[x_var]] == unique(subsetted_df_to_analyze[[x_var]])[i]),][[x_var]])
      }
      names(x_group_sample_sizes)<-x_group_names

      if(all(x_group_sample_sizes>14)){
        print("There is sufficient sample size (<14 - arbitrary value) in each group to represent using boxplots.",quote=FALSE)

        require(reshape2)
        melted_data_to_plot<-melt(subsetted_df_to_analyze,id.vars = c(x_var,group_dependence))

        graph_that_is_drawn<-ggplot(melted_data_to_plot,aes(x=melted_data_to_plot[[x_var]],y=value))+
          geom_boxplot(outlier.colour="black",outlier.shape=4,outlier.size=8,fill="gray")+
          geom_dotplot(binaxis="y",stackdir="center",dotsize = .75,fill="black")+
          labs(y=y_var,x=x_var) + theme_bw()+
          theme(plot.title=element_text(size=14,face="bold",hjust = 0.5),
                 axis.title.y = element_text(size = 16),
                 axis.text.x = element_text(size=12),
                 axis.title.x = element_text(size=16))
      }else{
        print("There is not sufficient sample size (>14 - arbitrary value) in each dependent group to represent using boxplots. All data will be shown.",quote=FALSE)

        require(reshape2)
        melted_data_to_plot<-melt(subsetted_df_to_analyze,id.vars = c(x_var,group_dependence))

        #builds graph
        graph_that_is_drawn<-ggplot(data = melted_data_to_plot,aes(x=melted_data_to_plot[[x_var]],y=value,group=melted_data_to_plot[[group_dependence]]))+
          geom_line(alpha=0.7)+
          theme_bw()+xlab(x_var)+ylab(y_var)+
          geom_point()+
          theme_bw()+theme(plot.title=element_text(size=14,face="bold",hjust = 0.5),
                           axis.title.y = element_text(size = 16),
                           axis.text.x = element_text(size=12),
                           axis.title.x = element_text(size=16))
      }

      return(graph_that_is_drawn)
    }
  }
}


