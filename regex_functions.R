library(glue)
get_string_between <- function(string, pattern1, pattern2) {
  
  # patterns <- list(first = glue("(?<={pattern1})(.*?)(?={pattern2})"),
  #                  last = glue("(?<={pattern1})(.*)(?={pattern2})"))
  patterns <- list(first = glue("({pattern1})(.*?)({pattern2})"),
                   last = glue("({pattern1})(.*)({pattern2})"))
  
  map_chr(patterns,
      ~str_extract_all(string, .x) %>% 
        unlist())
}

get_n_words_before_after <- function(string,
                                     pattern,
                                     before_after = NULL,
                                     n_words) {
  
  word_pattern <- glue("(?:[^ ]+ ){0,{n_words}}{pattern}(?: [^ ]+){0,{n_words}}")
  
}

get_words_before_or_after
get_characters_before_or_after

test <- "This is just a simple sentence that sentence that sentence"
pattern <- "(?:[^ ]+ ){0,2}simple(?: [^ ]+){0,2}"

get_string_between("This", "sentence", test)


pattern <- "(?=findings: a)(.+)(?=m4)"
pattern <- "(?<=This is)(.*)(?=sentence)"
pattern <- "(?<=This is)(.*?)(sentence)"

str_view(test, pattern, match = NA)

str_extract_all(test, pattern)
