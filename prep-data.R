library(tidyverse)
library(stringr)
library(glue)
library(magrittr)
# Download epub

download.file("https://cran.r-project.org/doc/manuals/r-release/R-intro.epub", destfile = "intro.epub")
unzip(zipfile = "intro.epub")
file.remove(c("toc.ncx","titlepage.xhtml", "stylesheet.css"))
rename_file <- function(name){
  new_file <- gsub("(R-intro)_split_([0-9]*)", "\\2-\\1", name)
  new_file <- gsub("^0", "", new_file)
  file.rename(from = name, to = new_file)
}

purrr::walk(list.files(pattern = "R-int"), rename_file)

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

# Some manual adjustements 

file.remove(c("01-intro.Rmd","02-literature.Rmd","03-method.Rmd",
              "04-application.Rmd", "05-summary.Rmd","06-references.Rmd"))
file.remove("00-R-intro.Rmd")
file.remove("01-R-intro.Rmd")

file.append("index.Rmd", "02-R-intro.Rmd")
file.append("index.Rmd", "03-R-intro.Rmd")
file.remove(c("00-R-intro.Rmd","01-R-intro.Rmd", "02-R-intro.Rmd", "03-R-intro.Rmd"))

# Manually replace url 

