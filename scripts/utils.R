# utilities for plotting and cleaning data

options(scipen = 999)

# function to load all packages, installing if needed
# source: https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

if(!require("here")){install.packages("here")}

# custom color theme
if(file.exists(here::here("data/nrel-colors.csv"))){
  
  nrel.cols <- as.list(read.csv(here::here("data/nrel-colors.csv"))$hex)
  names(nrel.cols) <- c(read.csv(here::here("data/nrel-colors.csv"))$name)
  
} else {
  
  nrel.cols <-     list("#0084c9", "#00a5db"  , "#666d70", "#d1cec6"  , "#191919", "#568e14", "#7fba00"   , "#ffc61e", "#d88c02", "#ffffff", "#c6414c")
  names(nrel.cols) <- c("blue"   , "lightblue", "gray"   , "lightgray", "black"  , "green"  , "lightgreen", "yellow" , "orange" , "white"  , "red"    )
}

# custom spectral color ramp
#spec.cols <- c("#1f729b", "#70c1b3", "#b2dbbf", "#f3ffbd", "#ff1654") # original
#spec.cols <- c("#1f729b", "#70c1b3", "#f3ffbd", "#ed9589", "#ff1654") # more yellow mid
spec.cols <- c(nrel.cols$blue, "#70c1b3", "#f3ffbd", "#ed9589", "#ff1654") # nrel blue
#spec.cols <- c("#0b5e82", "#00a581", "#d6e886", "#ed6050", "#bc103b")
#spec.cols <- c("#1376a0", "#57baa4", "#dbe8a2", "#ed8074", "#ff1654")

my.cols <- colorRampPalette(spec.cols)

# cut functions for discrete x axis with pretty labels
pretty.cut <- function(x){
  levels(x) <- gsub("\\,", " to ", gsub("\\(|\\)|\\[|\\]", "", levels(x)))
  levels(x) <- gsub(" to Inf", "+", levels(x))
  return(x)}

pretty.cut.pct <- function(x){
  #x <- factor(x) # preserve order
  levels(x) <- gsub("\\(|\\)|\\[|\\]", "",  levels(x))
  levels(x) <- gsub("\\(|\\)|\\[|\\]", "",  levels(x))
  levels(x) <- paste0(as.numeric(unlist(tstrsplit(levels(x), ",")[1])) * 100, "% to ", 
                      as.numeric(unlist(tstrsplit(levels(x), ",")[2])) * 100, "%")
  levels(x) <- gsub(" to Inf%", "+", levels(x))
  return(x)}

# pretty round the limit for fixed coordinates based on max of pred and actual volume
pretty.round <- function(x, ratio, sigs){signif(max(x, na.rm = T) * ratio, sigs)}

# pretty significant figures (e.g., 21. )
signif.pretty <- function(x, digits = 1){formatC(signif(x, digits = 1), digits = 1, format = "fg", flag = "#")}

# use a function to make sure if all obs for date x ID are NA, returns NA
sum.na <- function(x){if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = T)}
mean.na <- function(x){if (all(is.na(x))) x[NA_integer_] else mean(x, na.rm = T)}
min.na <- function(x){if (all(is.na(x))) x[NA_integer_] else min(x, na.rm = T)}
max.na <- function(x){if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = T)}

# print markdown table in R console (useful for copy paste to GitHub comments)
markdown.table <- function(x){
  header <- paste(colnames(x), collapse = " | ")
  head.sep <- paste0("--- ", paste0(rep("| --- ", length(colnames(x)) - 1), collapse = ""), collapse = "")
  data <- apply(x, 1, function(y) paste(y, collapse = " | "))
  cat(header, head.sep, data, sep = "\n")
}

# function to replace zeros with NAs
# useful for summing after aggregation that returns zeros when no obs instead of NAs
na.replace <- function(v, value = 0) { v[v == 0] <- NA; v } 

# function to pull out linear model (lm) p-value (not coefficient p-values)
# useful for general 2 variable linear model R2 w/ p value
# source: https://stackoverflow.com/a/5587781
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# common CRS
ll.proj.espg <- 4326
albers.proj.espg <- 5070 # equal area albers for continuous us

# file list of everything in the repo
# prefer data.table (if installed)
if(require("data.table")){
  
  all.files.here <- data.table::as.data.table(cbind(data.table::data.table(filename = list.files(here::here(), recursive = T)), 
                                                    file.info(list.files(here::here(), recursive = T))))
} else {
  
  all.files.here <- data.frame(cbind(data.frame(filename = list.files(here::here(), recursive = T)), 
                                     file.info(list.files(here::here(), recursive = T))))
}

# TODO process to check repo files and add large (>100mb) files to .gitignore
#all.files[size / 1e6 > 100] # 1e6 bytes per mb, 200 mb filesize limit for git
#all.files[size / 1e6 > 200] # 1e6 bytes per mb, 200 mb filesize limit for git

# get x y limits from ggplot obj after creation for easy annotating after 
# source: https://stackoverflow.com/a/40304848
get_plot_limits <- function(plot) { # FIXME
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
