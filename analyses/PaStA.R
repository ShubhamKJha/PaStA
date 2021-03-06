#!/usr/bin/env Rscript

# PaStA - Patch Stack Analysis
#
# Copyright (c) OTH Regensburg, 2016
#
# Author:
#   Ralf Ramsauer <ralf.ramsauer@othr.de>
#
# This work is licensed under the terms of the GNU GPL, version 2.  See
# the COPYING file in the top-level directory.

library(tikzDevice)
library(ggplot2)
library(reshape2)
library(plyr)

# Global color palette
cols <- c("coral1",
          "cyan3",
          "#E69F00",
          "#56B4E9",
          "#009E73",
          "#0072B2",
          "#D55E00",
          "#CC79A7",
          "#999999",
          "#E69F00",
          "#56B4E9",
          "#009E73",
          "#0072B2",
          "#D55E00",
          "#CC79A7",
          "#999999",
          "#E69F00",
          "#56B4E9",
          "#009E73",
          "#0072B2",
          "#D55E00",
          "#CC79A7")

# Y-m-d Date converter
convertDate <- function(table, columns) {
  table[, columns] <- as.data.frame(lapply(table[, columns, drop = FALSE],
                                           as.Date))
  return(table)
}

# Save as ggplot as tikz and png
savePlot <- function(filename, plot) {
  TEXWIDTH <- 5.87614
  tex_filename <- file.path(output_dir, paste(project_name, "-", filename, ".tex", sep = ""))
  png_filename <- file.path(output_dir, paste(project_name, "-", filename, ".png", sep = ""))

  tikz(tex_filename, standAlone = FALSE, width = 5, height = 5, sanitize = TRUE)
  print(plot)
  dev.off()

  png(png_filename, width = 800, height = 640)
  print(plot)
  dev.off()
}

# Read a CSV file
read_csv <- function(filename) {
  read.csv(filename,
           header=TRUE,
           sep=' ')
}

# The num_commit plot
num_commits <- function() {
  commitcount <- as.data.frame(table(patch_groups$StackVersion))
  colnames(commitcount) <- c("Version", "NumCommits")
  commitcount <- merge(x = commitcount,
                       y =  stack_release_dates,
                       by.x = "Version",
                       by.y = "Version")

  # some sugar
  commitcount <- commitcount[, c(3,1,4,2)]

  mindate <- min(commitcount$ReleaseDate)
  maxdate <- max(commitcount$ReleaseDate)

  vgs <- unique(commitcount$VersionGroup)
  vgs <- sort(vgs)

  xticks <- do.call("c", lapply(vgs, function(x)
    c(min(subset(commitcount, VersionGroup == x)$ReleaseDate)#,
      #max(subset(commitcount, VersionGroup == x)$ReleaseDate)
    )
  ))

  p <- ggplot(commitcount,
              aes(x = ReleaseDate,
                  y = NumCommits,
                  group = VersionGroup,
                  colour = VersionGroup)) +
    geom_line(size = 1.2) +
    ylim(200, max(commitcount$NumCommits)) +
    scale_x_date(date_labels = "%b %Y",
                 limits = c(mindate, maxdate),
                 breaks = xticks) +
    theme_bw(base_size = 15) +
    scale_color_manual(values = cols) +
    xlab("Timeline") +
    ylab("Number of Patches") +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 65,
                                     hjust = 1))
  return(p)
}

# Diffstat analysis
diffstat <- function() {
  my_diffstats = diffstats

  my_diffstats <- merge(x = my_diffstats,
                        y =  stack_release_dates,
                        by.x = "Version",
                        by.y = "Version")

  my_diffstats$Sum <- my_diffstats$Insertions + my_diffstats$Deletions

  mindate <- min(my_diffstats$ReleaseDate)
  maxdate <- max(my_diffstats$ReleaseDate)

  vgs <- unique(my_diffstats$VersionGroup)
  vgs <- sort(vgs)

  xticks <- do.call("c", lapply(vgs, function(x)
    c(min(subset(my_diffstats, VersionGroup == x)$ReleaseDate))
  ))

  p <- ggplot(my_diffstats) +
    geom_line(size = 1.2,
              aes(x = ReleaseDate,
                  y = Sum,
                  group = VersionGroup,
                  colour = VersionGroup)) +
    ylim(min(my_diffstats$Sum),
         max(my_diffstats$Sum)) +
    scale_x_date(date_labels = "%b %Y",
                 limits = c(mindate, maxdate),
                 breaks = xticks) +
    theme_bw(base_size = 15) +
    scale_color_manual(values = cols) +
    xlab("Timeline") +
    ylab("LOC deleted + inserted") +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_text(angle = 65,
                                     hjust = 1))
  return(p)
}

# Upstream analysis
upstream_analysis <- function(binwidth) {
  p <- ggplot(upstream, aes(upstream$DateDiff)) +
    xlab("Days between release and upstream") +
    ylab("Upstream patch density [a.u.]") +
    theme_bw(base_size = 15) +
    theme(axis.line = element_line()) +
    #geom_histogram(binwidth = binwidth)
    geom_density() +
    geom_vline(xintercept = 0,
               colour = "red")

  return(p)
}

# Branch observation
branch_observation <- function() {

  pg <- merge(x = patch_groups[, c("PatchGroup", "StackVersion")],
              y = stack_release_dates[, c("VersionGroup", "Version")],
              by.x = "StackVersion",
              by.y = "Version",
              all.x = TRUE,
              all.y = FALSE)

  pg <- merge(x = pg,
              y = upstream[, c("PatchGroup", "Type")],
              by = "PatchGroup",
              all.x = TRUE,
              all.y = FALSE)

  pg$PatchGroup <- NULL

  pg$Type <- replace(pg$Type, is.na(pg$Type), "invariant")

  branch_observation <- ddply(pg, .(VersionGroup, StackVersion, Type), nrow)

  create_plot <- function(ver_grp) {
    observation <- subset(branch_observation, VersionGroup == ver_grp)
    plot <- ggplot(observation,
                   aes(x = StackVersion, y = V1, fill = Type)) +
      geom_bar(stat = "identity") +
      xlab("Stack Version") +
      theme(legend.position = "right",
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 90,
                                       hjust = 1)) +
      scale_fill_discrete(name = ver_grp)
    return(list(ver_grp, plot))
  }

  results <- lapply(ord_version_grps, create_plot)
  return(results)
}

# Stack future (maybe merge with branch observation)
stack_future <- function(stack_versions) {

  pg <- merge(x = patch_groups[, c("PatchGroup", "StackVersion")],
              y = stack_release_dates[, c("VersionGroup", "Version")],
              by.x = "StackVersion",
              by.y = "Version",
              all.x = TRUE,
              all.y = FALSE)

  pg <- merge(x = pg,
              y = upstream[, c("PatchGroup", "Type")],
              by = "PatchGroup",
              all.x = TRUE,
              all.y = FALSE)

  pg$PatchGroup <- NULL

  pg$Type <- replace(pg$Type, is.na(pg$Type), "invariant")

  branch_observation <- ddply(pg, .(VersionGroup, StackVersion, Type), nrow)

  observation <- subset(branch_observation, StackVersion %in% stack_versions)
  plot <- ggplot(observation,
                 aes(x = StackVersion, y = V1, fill = Type)) +
    geom_bar(stat = "identity") +
    xlab("Stack Version") +
    ylab("Number of commits") +
    theme_bw(base_size = 15) +
    theme(legend.position = "top",
          axis.line = element_line(),
          axis.text.x = element_text(angle = 65,
                                     hjust = 1)) +
    scale_fill_discrete(name = "Types of patches")
  return(plot)
}


### Program start ###
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  project_name <- "SAMPLE"
  output_dir <- "foo"

  release_sort_filename <- '/home/ralf/workspace/PaStA/foo/release-sort'
  mainline_release_dates_filename <- '/home/ralf/workspace/PaStA/foo/mainline-release-dates'
  stack_release_dates_filename <- '/home/ralf/workspace/PaStA/foo/stack-release-dates'
  patch_groups_filename <- '/home/ralf/workspace/PaStA/foo/patches'
  upstream_filename <- '/home/ralf/workspace/PaStA/foo/upstream'
  occurrence_filename <- '/home/ralf/workspace/PaStA/foo/patch-occurrence'
  diffstat_filename <- '/home/ralf/workspace/PaStA/foo/diffstat'
  persistent = FALSE
} else {
  project_name <- args[1]
  output_dir <- args[2]

  release_sort_filename <- args[3]
  mainline_release_dates_filename <- args[4]
  stack_release_dates_filename <- args[5]
  patch_groups_filename <- args[6]
  upstream_filename <- args[7]
  occurrence_filename <- args[8]
  diffstat_filename <- args[9]

  persistent = TRUE
}

# Load all csv files
mainline_release_dates <- read_csv(mainline_release_dates_filename)
stack_release_dates <- read_csv(stack_release_dates_filename)
patch_groups <- read_csv(patch_groups_filename)
upstream <- read_csv(upstream_filename)
occurrence <- read_csv(occurrence_filename)
release_sort <- read_csv(release_sort_filename)
diffstats <- read_csv(diffstat_filename)

# Convert columns containing dates
mainline_release_dates <- convertDate(mainline_release_dates,
                                      c("ReleaseDate"))
stack_release_dates <- convertDate(stack_release_dates,
                                   c("ReleaseDate"))
upstream <- convertDate(upstream, c("UpstreamCommitDate", "FirstStackOccurence"))

# Prepare Tables
upstream$DateDiff <- upstream$UpstreamCommitDate - upstream$FirstStackOccurence
upstream$DateDiff <- as.numeric(upstream$DateDiff, units="days")
upstream$Type <- sapply(upstream$DateDiff, function(x) {
  if (x < -1)
    "backport"
  else
    "forwardport"
})

# Set Version as ord. factors
ord_stack_ver = factor(unique(release_sort$Version),
                       ordered = TRUE,
                       levels = unique(release_sort$Version))

ord_version_grps = factor(unique(release_sort$VersionGroup),
                          ordered = TRUE,
                          levels = unique(release_sort$VersionGroup))

release_sort$VersionGroup <- factor(release_sort$VersionGroup,
                                    ordered = TRUE,
                                    levels = ord_stack_ver)

release_sort$Version <- factor(release_sort$Version,
                               ordered = TRUE,
                               levels = ord_stack_ver)

occurrence$OldestVersion <- factor(occurrence$OldestVersion,
                                   ordered = TRUE,
                                   levels = ord_stack_ver)

occurrence$LatestVersion <- factor(occurrence$LatestVersion,
                                   ordered = TRUE,
                                   levels = ord_stack_ver)

occurrence$FirstReleasedIn <- factor(occurrence$FirstReleasedIn,
                                   ordered = TRUE,
                                   levels = ord_stack_ver)

occurrence$LastReleasedIn <- factor(occurrence$LastReleasedIn,
                                   ordered = TRUE,
                                   levels = ord_stack_ver)

patch_groups$StackVersion <- factor(patch_groups$StackVersion,
                                    ordered = TRUE,
                                    levels = ord_stack_ver)

stack_release_dates$VersionGroup <- factor(stack_release_dates$VersionGroup,
                                           ordered = TRUE,
                                           levels = ord_version_grps)

stack_release_dates$Version <- factor(stack_release_dates$Version,
                                      ordered = TRUE,
                                      levels = ord_stack_ver)

fl_stack_versions <- factor(c(),
                            ordered = TRUE,
                            levels = ord_stack_ver)
f_stack_versions <- factor(c(),
                           ordered = TRUE,
                           levels = ord_stack_ver)
l_stack_versions <- factor(c(),
                           ordered = TRUE,
                           levels = ord_stack_ver)
for (i in ord_version_grps) {
  stacks_of_grp <- subset(stack_release_dates, VersionGroup == i)
  minver <- min(stacks_of_grp$Version)
  maxver <- max(stacks_of_grp$Version)
  fl_stack_versions[length(fl_stack_versions)+1] <- minver[1]
  fl_stack_versions[length(fl_stack_versions)+1] <- maxver[1]

  f_stack_versions[length(f_stack_versions)+1] <- minver[1]
  l_stack_versions[length(l_stack_versions)+1] <- maxver [1]
}

binwidth <- 7

commitcount_plot <- num_commits()
diffstat_plot <- diffstat()
upstream_analysis_plot <- upstream_analysis(binwidth)
branch_observation_plots <- branch_observation()

fl_sf_plot <- stack_future(fl_stack_versions)
f_sf_plot <- stack_future(f_stack_versions)
l_sf_plot <- stack_future(l_stack_versions)

if (persistent) {
  savePlot("commitcount", commitcount_plot)
  savePlot("diffstat", diffstat_plot)
  savePlot("upstream-analysis", upstream_analysis_plot)
  for (i in branch_observation_plots) {
    savePlot(paste("branch-observation-",
                   i[[1]],
                   sep = ""), i[[2]])
  }
  savePlot("fl_sf_plot", fl_sf_plot)
  savePlot("f_sf_plot", f_sf_plot)
  savePlot("l_sf_plot", l_sf_plot)
} else {
  print(commitcount_plot)
  print(diffstat_plot)
  print(upstream_analysis_plot)
  for(i in branch_observation_plots) {
    print(i[[2]])
  }
  print(fl_sf_plot)
  print(f_sf_plot)
  print(l_sf_plot)
}
