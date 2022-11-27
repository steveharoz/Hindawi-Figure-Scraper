library(tidyverse)
library(rvest)
library(glue)

# what TOC pages to scrape
page_numbers = 1:10

# get everything in a string (x) that appears before the pattern
get_first_token = function (x, pattern) {
  retval = c()
  for (xi in x) {
    retval = c(retval, str_split(xi, pattern)[[1]][[1]])
  }
  retval
}

# get everything in a string (x) that appears after the last instance of pattern
get_last_token = function (x, pattern) {
  retval = c()
  for (xi in x) {
    tokens = str_split(xi, pattern) %>% unlist()
    tokens = tokens[nchar(tokens) > 0]
    token = tokens[[length(tokens)]]
    retval = c(retval, token)
  }
  retval
}

# get all the articles linked from a page of the TOC
get_article_urls_from_TOC_page = function(pagenum) {
  toc_url = "https://www.hindawi.com/journals/cin/contents/year/2022/page/{pagenum}/"
  
  # get the page
  html_file = read_file(glue(toc_url))
  page = read_html(html_file)

  # parse out the article URLs
  article_urls = page %>% 
    html_elements(".toc_article > a") %>% 
    html_attr("href")
  
  # don't remove this delay!!!
  Sys.sleep(1)
}


# pull out info from an article
parse_article = function(html_content) {
  html_content = read_html(html_content)
  
  article = tibble(

    title = html_content %>% 
      html_element("h1.articleHeader__title") %>% 
      html_text(),
    
    journal = html_content %>% 
      html_element("#journal-header-title") %>% 
      html_text(),
    
    editor = html_content %>% 
      html_element(".articleHeader__academicEditor span") %>% 
      html_text(),
    
    published_date = html_content %>% 
      html_element(".articleHeader__timeline_item span") %>% 
      html_text() %>% 
      last(),
    
    authors = html_content %>% 
      html_elements(".articleHeader__authors_author") %>% 
      html_text() %>% 
      get_first_token("[\\,\\d]") %>% 
      paste(collapse = ", "),
      
    doi = html_content %>% 
      html_element(".articleHeader__meta_doiLink>a") %>% 
      html_text(),
    
    articleID = get_last_token(doi, "/"),
    
    figure_url = html_content %>% 
      html_elements(".partial-carousel-wrapper") %>% 
      html_element("img") %>% 
      html_attr("src"),
    
    figure_number = html_content %>% 
      html_elements(".partial-carousel-wrapper .floats-partial-footer .caption-text") %>% 
      html_text(),
      
    figure_caption = html_content %>% 
      html_elements(".partial-carousel-wrapper .floats-partial-footer .floats-partial-caption-text") %>% 
      html_text()
  )
  
  article
}


# get articles from multiple TOC pages
article_urls = lapply(page_numbers, get_article_urls_from_TOC_page)
article_urls = unlist(article_urls)

# pull the raw text from each article URL
articles_raw = c()
for (index in seq_along(article_urls)) {
  article_url = article_urls[[index]]
  url = glue("https://www.hindawi.com{article_url}")
  html_file = read_file(url)
  articles_raw = c(articles_raw, html_file)
  
  print(glue("{index} / {length(article_urls)} - {url}"))
  
  # don't remove this delay!!!
  Sys.sleep(2)
}

# parse each article and build a tidy table of figures
figures = lapply(articles_raw, parse_article)
figures = figures %>% bind_rows()

write_csv(figures, "~/hindawi/figures.csv")