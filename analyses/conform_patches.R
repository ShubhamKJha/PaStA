#!/usr/bin/env Rscript
library(dplyr)
library(ggplot2)
library(gridExtra)


args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  d_dst <- '/tmp/R'
  f_characteristics <- '~/PycharmProjects/PaStA/resources/linux/resources/characteristics.csv'
} else {
  d_dst <- args[1]
  f_characteristics <- args[2]
}

all_lists_label <- 'all_lists_one_mtr_per_sec'
list_and_sec_mtr_label <- 'one_list_mtr_per_sec'
list_and_mtr_label <- 'one_list_and_mtr'
list_or_mtr_label <- 'one_list_or_mtr'
list_label <- 'one_list'
maintainer_integrated_label <- 'integrated by Maintainer'
total_metric_label <- 'total'
total_integrated_label <- 'total integrated'

dir.create(d_dst, showWarnings = FALSE)

load_csv <- function(filename) {
  data <- read.csv(filename, header = TRUE, sep=",")
  data$list_matches_patch <- as.logical(data$list_matches_patch)
  data$all_lists_one_mtr_per_sec<- as.logical(data$all_lists_one_mtr_per_sec)
  data$one_list_and_mtr <- as.logical(data$one_list_and_mtr)
  data$one_list_mtr_per_sec <- as.logical(data$one_list_mtr_per_sec)
  data$one_list_or_mtr <- as.logical(data$one_list_or_mtr)
  data$one_list <- as.logical(data$one_list)
  data$maintainers_integrated <- as.logical(data$maintainers_integrated)
  
  return(data)
}


# generate a data frame to show the metrics as stacked and the integrated patches
# as dodged
boxplot_data_frame <- function(data){
  list_vec <- c()
  metric_vec <- c()
  integrated_vec <- c()
  value_vec <- c()
  
  all_lists <- unique(data$list)
  
  for (l in all_lists){
    acc <- 0
    
    append_data <- function(l, metric, value){
      list_vec <<- c(list_vec, l)
      metric_vec <<- c(metric_vec, metric)
      value_vec <<- c(value_vec, value)
    }
    
    accumulate_data <- function(vector){
      value <- sum(vector, na.rm = TRUE)
      ret <- value - acc
      acc <<- value
      return(ret)
    }
    relevant <- data %>% filter(list == l)
    
    num <- accumulate_data(relevant$all_lists_one_mtr_per_sec)
    append_data(l, all_lists_label, num)
    
    num <- accumulate_data(relevant$one_list_mtr_per_sec)
    append_data(l, list_and_sec_mtr_label, num)
    
    num <- accumulate_data(relevant$one_list_and_mtr)
    append_data(l, list_and_mtr_label, num)
    
    num <- accumulate_data(relevant$one_list_or_mtr)
    append_data(l, list_or_mtr_label, num)
    
    num <- accumulate_data(relevant$one_list)
    append_data(l, list_label, num)
    
    num <- length(relevant$id) - acc
    append_data(l, total_metric_label, num)
    
    acc <- 0
    
    num <- sum(relevant$maintainers_integrated, na.rm = TRUE)
    num <- accumulate_data(relevant$all_lists_one_mtr_per_sec)
    append_data(l, maintainer_integrated_label, num)
    
    num <- length(relevant$id) - acc
    append_data(l, total_integrated_label, num)
  }
  data_frame <- data.frame(list_vec, metric_vec, value_vec)
  
  return(data_frame)
}

all_levels <- c(all_lists_label, list_and_sec_mtr_label, list_and_mtr_label,
                 list_or_mtr_label, list_label, total_metric_label, 
                 maintainer_integrated_label, total_integrated_label)

metric_levels <- c(all_lists_label, list_and_sec_mtr_label,
            list_and_mtr_label, list_or_mtr_label, list_label, total_metric_label)

integrated_levels <- all_levels[is.na(pmatch(all_levels, metric_levels))]

data <- load_csv(f_characteristics)

df <- boxplot_data_frame(data)


#filtering the metric and the integrated values into two data frames
metric_df <- df %>% filter(metric_vec %in% metric_levels)
integrated_df <- df %>% filter(metric_vec %in% integrated_levels)

# reordering the values
metric_df$metric_vec <- factor(metric_df$metric_vec,
                        levels = metric_levels)

integrated_df$metric_vec <- factor(integrated_df$metric_vec,
                                   levels = integrated_levels)


metric_plot <- ggplot(metric_df, aes(fill=metric_vec, y=value_vec, x=list_vec)) + 
  geom_bar(position=position_stack(reverse=TRUE), stat="identity")


integrated_plot <- ggplot(integrated_df, aes(fill=metric_vec, y=value_vec, x=list_vec)) + 
  geom_bar(position=position_stack(reverse=TRUE), stat="identity")

gridExtra::grid.arrange(metric_plot, integrated_plot, ncol=2)
