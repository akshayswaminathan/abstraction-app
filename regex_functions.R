library(glue)
#get_string_between <- function(string, pattern1, pattern2) {
  
  # patterns <- list(first = glue("(?<={pattern1})(.*?)(?={pattern2})"),
  #                  last = glue("(?<={pattern1})(.*)(?={pattern2})"))
#  patterns <- list(first = glue("({pattern1})(.*?)({pattern2})"),
#                   last = glue("({pattern1})(.*)({pattern2})"))
  
#  map_chr(patterns[1],
#      ~str_extract_all(string, .x) %>%
#        unlist())
#}

exports$get_n_words_before_after <- get_n_words_before_after <- function(string,
                                     pattern,
                                     before_after = NULL,
                                     n_words) {
  
  word_pattern <- paste0("(?:[^ ]+ ?){0,", n_words, "} ", pattern, " (?: ?[^ ]+){0,", n_words, "}")
  
}

get_words_before_or_after <- function(){}
get_characters_before_or_after <- function(){}

test <- "This is just a simple sentence that sentence that sentence"
pattern <- "(?:[^ ]+ ){0,2}simple(?: [^ ]+){0,2}"

#get_string_between("This", "sentence", test)


pattern <- "(?=findings: a)(.+)(?=m4)"
pattern <- "(?<=This is)(.*)(?=sentence)"
pattern <- "(?<=This is)(.*?)(sentence)"

#str_view(test, pattern, match = NA)

#str_extract_all(test, pattern)


