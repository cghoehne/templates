# sample hex plots for predicted vs actual data

# custom functions & schemes
if(!require("here")){install.packages("here")} # first get here, easy file paths
if(file.exists(here::here("scripts/utils.R"))){ # if our utility script exists
  source(here::here("scripts/utils.R"))  # import
} else { # but if it doesn't
  if(!require("devtools")){install.packages("devtools")} # source it from github with devtools
  devtools::source_url("https://github.com/cghoehne/templates/raw/main/scripts/utils.R")
}

# load packages, installing any if needed, using the ipak utility function
packages <- c("ggplot2", "hexbin", "scales", "data.table", "here") # my most common pkgs
ipak(packages) # loads all packages, installing if needed

# ----

# generate some sample data
set.seed(42)
dt <- data.table(actual = 1:1e3)
dt[, actual := actual * rnorm(1, 1, sd = 0.1) ]
dt[, pred := rnorm(.N, actual, sd = actual/4) ]

# alt data
dt <- as.data.table(diamonds)

# set gradient color scale for coloring density of observations
spec.cols <- c("#0084c9", "#70c1b3", "#f3ffbd", "#ed9589", "#ff1654")
my.cols <- colorRampPalette(spec.cols)

#####################
# standard hex plot #
#####################

#min.x <- min()
#min.y

hex.plot <- ggplot(data = dt, aes(y = price, x = carat)) +
  
  # optional  1:1 slope reference line 
  geom_abline(color = "grey") + # FIXME alpha doesn't work if not set to 1
  
  # hex binning with observation density on log scale
  # number of bins are flexible, more bins = smaller hexes 
  stat_binhex(aes(fill = ..count.., alpha = log(..density..) ), bins = 60) + 
  
  # color scaling
  scale_fill_gradientn("Observations",  
                       colours = my.cols(11)[3:11], 
                       trans = "log", 
                       labels = comma_format(accuracy = 1), 
                       breaks = log_breaks(n = 10)) +
  
  # slightly fades lower N hexes to draw attention towards more dense hexes
  scale_alpha(range = c(0.4, 1), guide = "none") + 
  
  # normal countious variable scales
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), labels = comma, limits = c(0, NA)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = comma, limits = c(0, NA)) +
  
  # other optional theme / formatting
  labs(title = "Diamonds Price vs Carat") +
  theme_minimal() +
  theme(title = element_text(color = "black"),
        plot.title = element_text(size = 10),
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "mm"),
        panel.spacing.x = unit(2, "mm"),
        panel.spacing.y = unit(4, "mm"),
        panel.border = element_blank(),
        legend.key.height = unit(14, "mm"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 2, r = 0, b = 1, l = 0)),
        axis.text.x = element_text(angle = 0, size = 9, margin = margin(t = 1, r = 0, b = 1, l = 0, "mm")),
        axis.text.y = element_text(angle = 0, size = 9, margin = margin(t = 0, r = 1, b = 0, l = 1, "mm")),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

hex.plot

#ggsave(here("figures/sample-hex-log-plot.png"), hex.plot, scale = 1, width = 6, height = 5) 

######################
# log scale hex plot #
######################

hex.log.plot <- ggplot(data = dt, aes(y = price, x = carat)) +
  
  # optional  1:1 slope reference line 
  geom_abline(color = "grey") + # FIXME alpha doesn't work if not set to 1
  
  # hex binning with observation density on log scale
  # number of bins are flexible, more bins = smaller hexes 
  stat_binhex(aes(fill = ..count.., alpha = log(..density..) ), bins = 60) + 
  
  # color scaling
  scale_fill_gradientn("Observations",  
                       colours = my.cols(11)[3:11], 
                       trans = "log", 
                       labels = comma_format(accuracy = 1), 
                       breaks = log_breaks(n = 10)) +
  
  # slightly fades lower N hexes to draw attention towards more dense hexes
  scale_alpha(range = c(0.4, 1), guide = "none") + 
  
  # axis log scaling
  # limits are flexible, but log(0) is -Inf so start at non-zero positive number
  scale_x_log10(expand = expansion(mult = c(0, 0.05)), labels = comma, limits = c(NA, NA)) + 
  scale_y_log10(expand = expansion(mult = c(0, 0.05)), labels = comma, limits = c(NA, NA)) +  
  annotation_logticks(sides = "lb", outside = T) +
  coord_cartesian(clip = "off", xlim = c(NA, NA),) +
  
  # other optional theme / formatting
  labs(title = "Diamonds Price vs Carat (log x & y)") +
  theme_minimal() +
  theme(title = element_text(color = "black"),
        plot.title = element_text(size = 12),
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "mm"),
        panel.spacing.x = unit(2, "mm"),
        panel.spacing.y = unit(4, "mm"),
        panel.border = element_blank(),
        legend.key.height = unit(14, "mm"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 2, r = 0, b = 1, l = 0)),
        axis.text.x = element_text(angle = 0, size = 9, margin = margin(t = 3, r = 0, b = 1, l = 0, "mm")),
        axis.text.y = element_text(angle = 0, size = 9, margin = margin(t = 0, r = 3, b = 0, l = 1, "mm")),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

hex.log.plot

#ggsave(here("figures/sample-hex-log-plot.png"), hex.log.plot, scale = 1, width = 6, height = 5) 

#####################################################
# log hex plot with simple linear model R2 and pval #
#####################################################

# function to pull out linear model (lm) p-value (not coefficient p-values)
# useful for general 2 variable linear model R2 w/ p value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# get R2 and p-value from a simple linear model: Y ~ X
lm.dt <- lm(price ~ carat, data = dt)
#lm.dt <- lm(pred ~ actual, data = dt)
r2.dt <- signif(summary(lm.dt)$r.squared, 2)
pval.dt <- lmp(lm.dt) # lmp is custom function to get model p-value, defined in `scripts/x_cgplot.R`
pval.lab.dt <- ifelse(pval.dt < 0.01, "p < 0.01", paste("p =", signif(pval.dt, 2)))

# relative annotate assignment
xy.r2 <- c('x' = 0.9, 'y' = 0.12)
xy.pv <- c('x' = 0.9, 'y' = 0.10)

# FIXME --- 

#lms <- get_plot_limits(hex.log.plot)

#a.xr <- lms$xmin + (xy.r2['x'] * (lms$xmax - lms$xmin))
#a.xp <- a.xr

#a.yr <- lms$ymin + (xy.pv['y'] * (lms$ymax - lms$ymin))
#a.yp <- lms$ymin + (xy.pv['y'] * (lms$ymax - lms$ymin))

# ----------

a.xr <- 3.05
a.xp <- a.xr

a.yr <- 700
a.yp <- 550

hex.log.lm.plot <- hex.log.plot +

  # customize location of R2 and pval by change x/y
  annotate("text", y = a.yr, x = a.xr, label = bquote(R^2 == .(r2.dt)), size = 3) +
  annotate("text", y = a.yp, x = a.xp, label = bquote(italic(.(pval.lab.dt))), size = 3) +
  ggtitle(paste("Diamonds Price vs Carat (log x & y w/ R2 and pval)"))

hex.log.lm.plot

#ggsave(here("figures/sample-hex-log-lm-plot.png"), hex.log.lm.plot, scale = 1, width = 6, height = 5) 

