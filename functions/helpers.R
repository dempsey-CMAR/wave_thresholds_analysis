
# Get the current figure size in pixels:
h_interactive <- function()
  with(knitr::opts_current$get(c("fig.height", "dpi", "fig.retina")),
       fig.height*dpi/fig.retina)


# set bin width for different variables
get_bin_width <- function(var_to_bin) {

  if(str_detect(var_i, "height")) bin_width <- 0.1
  if(str_detect(var_i, "period")) bin_width <- 2
  if(str_detect(var_i, "direction")) bin_width <- 20
  if(str_detect(var_i, "depth")) bin_width <- 1
  if(str_detect(var_i, "speed")) bin_width <- 0.05

  bin_width
}
