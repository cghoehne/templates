# sample cor plots for selected numeric variables in a data set

# custom functions & schemes
if(!require("here")){install.packages("here")} # first get here, easy file paths
if(file.exists(here::here("scripts/utils.R"))){ # if our utility script exists
  source(here::here("scripts/utils.R"))  # import
} else { # but if it doesn't
  if(!require("devtools")){install.packages("devtools")} # source it from github with devtools
  devtools::source_url("https://github.com/cghoehne/templates/raw/main/scripts/utils.R")
}

# load packages, installing any if needed, using the ipak utility function
packages <- c("corrplot", "data.table", "here") # my most common pkgs
ipak(packages) # loads all packages, installing if needed

# ----

# generate some sample data
set.seed(42)
dt <- data.table(actual = 1:1e3)
dt[, actual := actual * rnorm(1, 1, sd = 0.1) ]
dt[, pred := rnorm(.N, actual, sd = actual/4) ]

# alt data
dt <- as.data.table(mtcars)

# set gradient color scale for coloring density of observations
spec.cols <- c("#0084c9", "#70c1b3", "#f3ffbd", "#ed9589", "#ff1654")
my.cols <- colorRampPalette(spec.cols)


###############################################################
# create plots of correlations between variables for all data #
###############################################################

dir.create(here("figures/corrplots/"), showWarnings = F, recursive = T)

# if you want to reorder where the varaibles appear in the plot
#setcolorder(dt, c(colnames(dt)[5:15], colnames(dt)[1:4]))

# get column names
my.cols <- colnames(dt)
my.num.cols <- my.cols[unlist(lapply(dt, class)) %in% c("numeric", "integer")]

# define desired columns to plot correlations
cor.cols <- my.num.cols[!my.num.cols %in% c("exclude.this.col")] # exclude specified numeric columns
names(cor.cols) <- cor.cols # change displayed names on figure if desired
#names(cor.cols) <- c("")

# define correlation type
cor.type <- c("pearson", "kendall", "spearman")[3] # use Spearman for now as data/variance not all normal

# custom color palette
my.cols.r <- colorRampPalette(rev(spec.cols)) # reverse: red to blue spectral
cor.colors <- c(my.cols.r(8)[1:4], "#ffffff", my.cols.r(8)[5:8])
cor.pal <- colorRampPalette(cor.colors)

my.cor.title <- "Sample correlation of numeric variables in the `mtcars` dataset"

#for(i in something){
  
  dt.t <- dt
  #dt.t <- dt[i.n == i,] # subset
  
  no.obs.t <- names(which(unlist(dt.t[, lapply(.SD, function(x) all(x == 0 | is.na(x) | is.infinite(x)))]))) # no finite obs
  cor.cols.t <- colnames(dt.t)[colnames(dt.t) %in% cor.cols & !(colnames(dt.t) %in% no.obs.t)] # keep what available desired columns
  new.cor.names <- names(cor.cols[cor.cols %in% cor.cols.t]) # renamed columns
  dt.t[, (new.cor.names) := lapply(.SD, as.numeric), .SDcols = cor.cols.t] # force numeric for correlation
  
  # plot and save
  cor.t <- cor(dt.t[, ..new.cor.names], use = "pairwise.complete.obs", method = cor.type)
  cor.t[is.na(cor.t)] <- 0 # force NA to zero
  diag(cor.t) <- NA # force diagonal to NA to use NA label
  png(here(paste0("figures/corrplots/", cor.type, ".png")), width = 720, height = 720, res = 90)
  print(corrplot(cor.t, 
                 title = my.cor.title,
                 method = "color",
                 #order = "hclust",
                 type = "lower", 
                 tl.col = nrel.cols$black,
                 outline = alpha("black", 0.4), #"transparent", 
                 na.label = "1.0", 
                 na.label.col = "#0084c9",
                 col = cor.pal(10),
                 mar = c(0, 0, 4, 0),
                 addCoef.col = "white", 
                 number.digits = 2, 
                 number.font = 0.1,
                 number.cex = 1
  ))
  dev.off()
#}