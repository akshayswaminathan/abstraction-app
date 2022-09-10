library(glue)

get_string_between <- function(search_matrix, search_string) {
  
  return <- pmap_dfr(search_matrix,
                     function(start_string, end_string) {
                       
                       patterns <- list(first = glue("({start_string})(.*?)({end_string})"),
                                        last = glue("({start_string})(.*)({end_string})"))
                       
                       return <- map_chr(patterns,
                                         ~str_extract_all(search_string, .x) %>% 
                                           unlist())
                       
                       
                       return(return)
                     })
  return(return)
  
}

if (F) {
  
  text = "this is just a simple sentence that sentence. here is another random group of text we care about because we care about it"
  text_2 = "this is just a simple sentence that sentence. here is another random group of text we care about"
  
  search_matrix <- tibble(start_string = c("this is", "another"),
                          end_string = c("sentence", "care about")) %>% 
    mutate(search_id = row_number())
  
  get_string_between(search_matrix, search_string = text_2)
  
}

