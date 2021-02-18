knitr::opts_chunk$set(
  tidy      = FALSE,
  fig.pos   = "H", 
  out.extra = "", 
  fig.align = "center", 
  out.width = "100%",
  fig.width = 6, 
  fig.height = 4,
  graphics.auto_pdf = TRUE,
  echo      = FALSE,
  include   = FALSE,
  warning   = FALSE,
  message   = FALSE, 
  cache     = FALSE,
  verbose   = TRUE,
  comment   = NA
)

# Our Libraries
VLIBS <- c(
  "raster", "MASS", "tidyverse", "patchwork", "here", 
  "bookdown", "geomorph", "kableExtra", "broom"
  )

# Use Colors in Plots
# https://stackoverflow.com/questions/42458412/plotting-data-by-color-variable-with-ggplot2-in-r#comment72062654_42458412
colorBlindBlack8  <- c("#464343", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

F_LoadLibs <- function(x){
  print(x)
  if(!require(x, character.only = TRUE)) install.packages(x, type = "mac.binary"); library(x, character.only = TRUE);
  return(T)
}
# https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
options(dplyr.summarise.inform=F) 
# https://stackoverflow.com/questions/13286531/how-to-suppress-warnings-when-plotting-with-ggplot
options(warn=-1)
sapply(VLIBS, F_LoadLibs)
rm(VLIBS, F_LoadLibs)

theme_set(theme_classic(base_size=12))
