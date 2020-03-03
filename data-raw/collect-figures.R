
library(tidyverse)
".*?knitr::include_graphics(\"(.*?)\")"
fig_regex <- regex(
  "```\\{r (figure-[0-9a-z-]+).*?knitr::include_graphics\\(\"(.*?)\"\\)",
  multiline = TRUE, 
  dotall = TRUE
)

files <- tibble(
  rmd_path = list.files(".", "\\.Rmd$"),
  content = map_chr(rmd_path, read_file),
  figs = str_extract_all(content, fig_regex)
) %>% 
  select(-content) %>% 
  unnest(figs) %>% 
  mutate(
    fig_id = str_match(figs, fig_regex)[, 2],
    fig_src = str_match(figs, fig_regex)[, 3],
    fig_url = str_remove(fig_src, "#.*$"),
    ext = tools::file_ext(fig_url),
    fig_path = file.path(
      "figures",
      str_remove(rmd_path, "\\.Rmd$"),
      paste0(fig_id, ".", ext)
    )
  ) %>% 
  select(-figs)

dir.create("figures")
write_csv(files, "figures/figures.csv")

download_lazy <- function(url, dest) {
  if (file.exists(dest)) {
    return(NA)
  }
  
  if (!dir.exists(dirname(dest))) {
    dir.create(dirname(dest), recursive = TRUE)
  }
  
  message(url)
  Sys.sleep(runif(1, min = 0, max = 0.5))
  result <- tryCatch(curl::curl_download(url, dest), error = function(e) {
    message(glue::glue("Error downloading: {e}"))
    FALSE
  })
  
  if (identical(result, FALSE)) {
    FALSE
  } else {
    TRUE
  }
}

files$result <- map2_lgl(files$fig_url, files$fig_path, download_lazy)

replace_url_with_local <- function(rmd_path, fig_src, fig_path) {
  read_file(rmd_path) %>% 
    str_replace(
      fixed(sprintf('knitr::include_graphics("%s")', fig_src)), 
      sprintf('knitr::include_graphics("%s")', fig_path)
    ) %>% 
    write_file(rmd_path)
}

files %>% 
  select(rmd_path, fig_src, fig_path) %>% 
  pwalk(replace_url_with_local)

