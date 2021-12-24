## ------------------------------------------------------------------------
# 'Useful function and parameters'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)

# Packages ----------------------------------------------------------------

library("officer")

# Functions ---------------------------------------------------------------

# Standard error:
std <- function(x) sd(x)/sqrt(length(x))

# Function to shift legend:
# Taken from:
# https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
shift_legend <- function(p){
  require("gtable")
  require("cowplot")
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}


# Plot parameters ---------------------------------------------------------

## Colors
COL <- c("black", "darkgreen", "blue", "purple", "orange", "cyan4")

## Names
Names_variables <- c("Morphology",
                     "Ecology & Behavior",
                     "Geography",
                     "People",
                     "Modern & Past Culture",
                     "Others")

# Confidence interval
ci_z <- 1.96

# Breaks (year)
yrs <- c(seq(from=1757,to=2010,by=30),2019)

## ggplot2 custom themes
theme_custom <- function(){
  theme_bw() + theme(
    legend.position = c(0.25, 0.7),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=11),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    plot.caption = element_text(size = 10, color = "gray50"),
    plot.title = element_text(face="bold", size=12)
  ) }

theme_map <- function(){
  theme_bw() + theme(
  axis.text.x  = element_blank(), 
  axis.text.y  = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(), 
  axis.line.x = element_blank(), 
  axis.line.y = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.x = element_blank(),                                          
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(), 
  axis.ticks = element_blank(),
  plot.margin = unit(c(1,1,1,1), 'cm'),
  plot.title = element_text(size = 18, vjust = 1, hjust = 0),
  legend.text = element_text(size = 12),          
  legend.title = element_blank(),                              
  legend.position = c(0.1, 0.2), 
  legend.key = element_blank(),
  legend.background = element_rect(color = "black", 
                                   fill = "white", 
                                   size = 2, linetype = "blank")) }

# Table parameters --------------------------------------------------------

sect_properties <- officer::prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)

