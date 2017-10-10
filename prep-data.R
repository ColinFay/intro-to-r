library(tidyverse)
library(stringr)
library(glue)
library(magrittr)

file.create("ga")
write("<!-- Global Site Tag (gtag.js) - Google Analytics -->\n<script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-65307055-3\"></script>\n<script>\n  window.dataLayer = window.dataLayer || [];\n  function gtag(){dataLayer.push(arguments);}\n  gtag('js', new Date());\n\n  gtag('config', 'UA-65307055-3');\n</script>", file="ga",append=TRUE)

# Download epub

download.file("https://cran.r-project.org/doc/manuals/r-release/R-intro.epub", destfile = "intro.epub")
unzip(zipfile = "intro.epub")
file.remove(c("toc.ncx","titlepage.xhtml", "stylesheet.css"))

# Rename file and keep a track of file change
rename_file <- function(name){
  new_file <- gsub("(R-intro)_split_([0-9]*)", "\\2-\\1", name)
  new_file <- gsub("^0", "", new_file)
  file.rename(from = name, to = new_file)
  return(data.frame(orig = name, new = new_file))
}

file_names_change <- purrr::map_df(list.files(pattern = "R-int"), rename_file)

html_converter <- function(file){
  file_name <- gsub("\\.html", "", file)
  system(command = glue("pandoc {file_name}.html -o {file_name}.Rmd"))
}

purrr::walk(list.files(pattern = "R-int"), html_converter)

purrr::walk(list.files(pattern = "\\.html"), file.remove)

clean_html_rmd <- function(file){
  
  a <- readLines( file )
  
  a %<>% str_replace_all("<h1 .*>([A-Za-z0-9])", "# \\1") %>%
    str_replace_all("</h1>", "")%>% 
    str_replace_all("<h2 .*>([A-Za-z0-9])", "# \\1") %>%
    str_replace_all("</h2>", "") %>%
    str_replace_all("# [0-9]+", "# ")
  write(a, file = file)
}

purrr::walk(list.files(pattern = "Rmd"), clean_html_rmd)

clean_auto_ref <- function(file){
  
  a <- readLines( file )
  
  a %<>% str_replace_all("(R-intro)_split_([0-9]*)", "\\2-\\1") %>%
    str_replace_all("^0", "")
  write(a, file = file)

}

purrr::walk(list.files(pattern = "Rmd"), clean_auto_ref)

build_url_ref <- function(file){
  a <- readLines( file )
  url <- tolower(a[1]) %>%
    str_replace_all("# *", "") %>%
    str_replace_all(" ", "-")
  return(glue("{url}.html"))
}

file_names_change$url <- map(list.files(pattern = "ro.Rmd"), build_url_ref)

# Remove some useless files

file.remove(c("01-intro.Rmd","02-literature.Rmd","03-method.Rmd",
              "04-application.Rmd", "05-summary.Rmd","06-references.Rmd"))
file.remove("00-R-intro.Rmd")
file.remove("01-R-intro.Rmd")

file.append("index.Rmd", "02-R-intro.Rmd")
file.append("index.Rmd", "03-R-intro.Rmd")
file.remove(c("00-R-intro.Rmd","01-R-intro.Rmd", "02-R-intro.Rmd", "03-R-intro.Rmd"))

# Manually replace url 

clean_url <- function(file){
  a <- readLines( file )
  
  a %<>% str_replace_all("004-R-intro.html", "introduction-and-preliminaries.html") %>%
    str_replace_all("005-R-intro.html", "simple-manipulations-numbers-and-vectors.html") %>%
    str_replace_all("006-R-intro.html", "objects-their-modes-and-attributes.html") %>%
    str_replace_all("007-R-intro.html", "ordered-and-unordered-factors.html") %>%
    str_replace_all("008-R-intro.html", "arrays-and-matrices.html") %>%
    str_replace_all("009-R-intro.html", "lists-and-data-frames.html") %>%
    str_replace_all("010-R-intro.html", "reading-data-from-files.html") %>%
    str_replace_all("011-R-intro.html", "probability-distributions.html") %>%
    str_replace_all("012-R-intro.html", "grouping-loops-and-conditional-execution.html") %>%
    str_replace_all("013-R-intro.html", "writing-your-own-functions.html") %>%
    str_replace_all("014-R-intro.html", "statistical-models-in-r.html") %>%
    str_replace_all("015-R-intro.html", "graphical-procedures.html") %>%
    str_replace_all("016-R-intro.html", "packages.html") %>%
    str_replace_all("017-R-intro.html", "os-facilities.html") %>%
    str_replace_all("018-R-intro.html", "appendix-a-a-sample-session.html") %>%
    str_replace_all("019-R-intro.html", "appendix-b-invoking-r.html") %>%
    str_replace_all("020-R-intro.html", "appendix-c-the-command-line-editor.html") %>%
    str_replace_all("021-R-intro.html", "appendix-d-function-and-variable-index.html") %>%
    str_replace_all("022-R-intro.html", "appendix-e-concept-index.html") %>%
    str_replace_all("023-R-intro.html", "appendix-f-references.html") 
  write(a, file = file)
}

purrr::walk(list.files(pattern = "Rmd"), clean_url)

# Check for broken links
library(rvest)

all_links_page <- function(url){
  link <- read_html(url) %>%
    html_nodes("a") %>%
    html_attr("href") 
  
    data.frame(base = url, 
      url = link)
}

home <- all_links_page("http://colinfay.me/intro-to-r/")

complete_url <- function(url, canonical){
  if(grepl("^http", url) != 0){
    as.character(url)
  } else {
    paste0(canonical, url)
  }
}

home$url %<>% map(complete_url, canonical = "http://colinfay.me/intro-to-r/")

all_link_website <- map_df(home$url, all_links_page)
all_link_website$url %<>% map(complete_url, canonical = "http://colinfay.me/intro-to-r/")
all_link_website$res <- map(all_link_website$url, ~httr::status_code(httr::GET(.x)))

fourofour <- filter(all_link_website, res != 200)[grepl("colin", fourofour$base), ]

# Do some manual work here

# Build \o/

bookdown::render_book("index.Rmd", "bookdown::gitbook")
