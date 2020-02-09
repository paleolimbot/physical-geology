
library(tidyverse)
library(xml2)
library(rvest)

root <- read_xml("data-raw/Physical-Geology-First-University-of-Saskatchewan-Edition-1547781813.xml")

item_nodes <- root %>% xml_find_all("//item")

items <- tibble(
  item = map(seq_along(item_nodes), ~item_nodes[.x]),
  title = map_chr(item, . %>% xml_find_first("./title") %>% xml_text()),
  type = map_chr(item, . %>% xml_find_first("./wp:post_type") %>% xml_text()),
  url = map_chr(item, . %>% xml_find_first("./wp:attachment_url") %>% xml_text()),
  status = map_chr(item, . %>% xml_find_first("./wp:status") %>% xml_text()),
  order = as.numeric(map_chr(item, . %>% xml_find_first("./wp:menu_order") %>% xml_text())),
  content = map_chr(item, . %>% xml_find_first("./content:encoded") %>% xml_text()),
)

attachments <- items %>% 
  filter(type == "attachment", status != "trash")

chapter_titles <- items %>% 
  filter(type == "part", status != "trash") %>% 
  select(-type, -item) %>% 
  extract(title, c("chapter", "chapter_title"), "Chapter\\s+([0-9]+)\\s*\\.\\s*(.*)") %>% 
  filter(!is.na(chapter)) %>% 
  mutate(chapter = as.numeric(chapter)) %>% 
  arrange(chapter)

front_matter <- items %>% 
  filter(type == "front-matter", status != "trash", title != "Download PDF Files") %>% 
  arrange(order)

chapter_content <- items %>% 
  filter(type == "chapter", status != "trash") %>% 
  select(-url, -type, -item) %>% 
  extract(title, c("chapter", "section"), "([0-9]+)\\.([0-9X]+)", remove = FALSE) %>% 
  extract(title, c("chapter2", "section2"), "Chapter\\s+([0-9]+)\\s+(.*)", remove = FALSE) %>% 
  mutate(
    chapter = as.numeric(coalesce(chapter, chapter2)),
    section = coalesce(section, section2) %>% 
      factor(levels = c(1:20, "X", "Summary", "Review Questions"))
  ) %>% 
  arrange(chapter, section) %>% 
  select(-chapter2, -section2)


# html->md filter functions
md_filter_links <- function(x) {
  str_replace_all(
    x,
    "<a\\s+.*?href\\s*=\\s*(['\"])(.*?)\\1.*?>(.*?)</a>", 
    "[\\3](\\2)"
  )
}

md_filter_bold <- function(x) {
  str_replace_all(
    x,
    "<(strong|b)>(.*?)</\\1>", 
    "__\\2__"
  )
}

# in this book <h1> is used as a first heading
# on individual pages. in bookdown, this needs to be
# a ##
md_filter_hn <- function(x) {
  matches <- str_match_all(
    x,
    "<h([0-9])>(.*?)</h\\1>"
  )[[1]]
  headings <- strrep("#", as.numeric(matches[, 2]) + 2)

  for (i in seq_len(nrow(matches))) {
    x <- str_replace(x, fixed(matches[i, 1]), paste0("\n", headings[i], " ", matches[i, 3], "\n\n"))
  }
  
  x
}

md_filter_italics <- function(x) {
  str_replace_all(
    x,
    "<(em|i)>(.*?)</\\1>", 
    "_\\2_"
  )
}

md_filter_textbox <- function(x) {
  str_replace_all(
    x,
    regex("<div\\s+class=\"textbox\">(.*?)</div>", multiline = TRUE, dotall = TRUE), 
    "\n```{block, type='textbox'}\n\\1\n```\n\n"
  )
}

# will bork nested ULs, of which there are hopefully not too many
md_filter_ul <- function(x) {
  ul_items <- str_extract_all(
    x,
    regex("<ul>(.*?)</ul>", multiline = TRUE, dotall = TRUE)
  ) %>% 
    .[[1]] %>% 
    unique()
  
  for (ul_item in ul_items) {
    li_items <- read_html(ul_items) %>% 
      html_nodes("li") %>% 
      as.character() %>% 
      str_replace("<li>(.*?)</li>", "\\1") %>% 
      paste0("- ", ., collapse = "\n") %>% 
      paste0("\n\n", ., "\n\n")
    x <- str_replace_all(x, fixed(ul_item), li_items)
  }
  
  x
}

md_filter_p <- function(x) {
  str_replace_all(
    x,
    regex("<p>(.*?)</p>", multiline = TRUE, dotall = TRUE), 
    "\n\n\\1\n\n"
  )
}

md_filter_all <- function(x) {
  if (length(x) > 1) {
    return(map_chr(x, md_filter_all))
  }
  
  x %>% 
    md_filter_links() %>% 
    md_filter_bold() %>% 
    md_filter_italics() %>% 
    md_filter_hn() %>% 
    md_filter_textbox() %>% 
    md_filter_ul() %>% 
    md_filter_p()
}

# copy front matter so that it can be pasted into index.Rmd
front_matter %>% 
  mutate(
    content_markdown = glue::glue("\n## {title}\n\n{md_filter_all(content)}")
  ) %>% 
  pull(content_markdown) %>% 
  paste(collapse = "\n\n") %>% 
  clipr::write_clip()
  


