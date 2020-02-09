
library(tidyverse)
library(xml2)

root <- read_xml("data-raw/Physical-Geology-First-University-of-Saskatchewan-Edition-1547781813.xml")

item_nodes <- root %>% xml_find_all("//item")

items <- tibble(
  item = map(seq_along(item_nodes), ~item_nodes[.x]),
  title = map_chr(item, . %>% xml_find_first("./title") %>% xml_text()),
  type = map_chr(item, . %>% xml_find_first("./wp:post_type") %>% xml_text()),
  url = map_chr(item, . %>% xml_find_first("./wp:attachment_url") %>% xml_text()),
  status = map_chr(item, . %>% xml_find_first("./wp:status") %>% xml_text()),
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
  print()

chapter_content <- items %>% 
  filter(type == "chapter") %>% 
  select(-url, -type, -item) %>% 
  extract(title, c("chapter", "section"), "([0-9]+)\\.([0-9X]+)", remove = FALSE) %>% 
  extract(title, c("chapter2", "section2"), "Chapter\\s+([0-9]+)\\s+(.*)", remove = FALSE) %>% 
  mutate(
    chapter = as.numeric(coalesce(chapter, chapter2)),
    section = coalesce(section, section2) %>% 
      factor(levels = c(1:20, "X", "Summary", "Review Questions"))
  ) %>% 
  arrange(chapter, section) %>% 
  select(-chapter2, -section2) %>% 
  print()



