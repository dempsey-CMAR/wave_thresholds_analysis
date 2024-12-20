library(here)
library(quarto)
library(rmarkdown)
library(waves)

wv_extract_deployment_info3 <- function(file_path) {
  sub(".*/", "", file_path, perl = TRUE) %>%
    data.frame() %>%
    separate(
      col = ".",
      into = c("depl_date", "depl_id", "station", "process_info"),
      sep = "_") %>%
    mutate(
      depl_date = str_replace_all(depl_date, "\\.", "-"),
      depl_date= as_date(depl_date),
      process_info = str_remove(process_info, ".txt")
    )
}


################ update this ################
path <- here("data-compare/raw")
#############################################

depls <- list.files(path, pattern = ".txt", full.names = TRUE)


# export html file for each county showing the flagged observations
sapply(depls, function(x) {

  quarto_render(
    input = here("1_compile_reprocessed_waves.qmd"),
    output_file = paste0(
      wv_extract_deployment_info3(x)$depl_date, "_",
      wv_extract_deployment_info3(x)$station, "_",
      wv_extract_deployment_info3(x)$depl_id, "_",
      wv_extract_deployment_info3(x)$process_info,
      ".html"
    ),
    execute_params = list(depl_file = x))
})





