library(glue)

get_string_between <- function(search_matrix, search_string, all_incidents = F) {
  
  if (all_incidents) {
  return <- pmap_dfr(search_matrix,
                     function(start_string, end_string) {
                       
                       patterns <- list(first = glue("({start_string})(.*?)({end_string})"),
                                        last = glue("({start_string})(.*)({end_string})"))
                       
                       return <- map_chr(patterns,
                                         ~str_extract_all(search_string, .x) %>% 
                                           unlist())
                       
                       return(return)
                     })
  } else {
    
    return <- str_extract(search_string, glue("({search_matrix[1,1]})(.*?)({search_matrix[1,2]})"))
    
  }
  return(return)
  
}

# if (all_incidents) {
#   
#   return <- pmap_dfr(search_matrix,
#                      function(start_string, end_string) {
#                        
#                        patterns <- list(first = glue("({start_string})(.*?)({end_string})"),
#                                         last = glue("({start_string})(.*)({end_string})"))
#                        
#                        return <- map_chr(patterns,
#                                          ~str_extract_all(search_string, .x) %>% 
#                                            unlist())
#                        
#                        
#                        return(return)})
# } else {
#   
#   return <- pmap_dfr(search_matrix,
#                      function(start_string, end_string) {
#                        
#                        patterns <- list(first = glue("({start_string})(.*?)({end_string})"),
#                                         last = glue("({start_string})(.*)({end_string})"))
#                        
#                        return <- map_chr(patterns,
#                                          ~str_extract_all(search_string, .x) %>% 
#                                            unlist())
#                        
#                        
#                        return(return)})
# }

if (F) {
  
  text = "this is just a simple sentence that sentence. here is another random group of text we care about because we care about it"
  text_2 = "this is just a simple sentence that sentence. this is just another simple sentence with another that sentence. here is another random group of text we care about"
  
  search_matrix <- tibble(start_string = c("this is", "another"),
                          end_string = c("sentence", "care about")) #%>% 
    # mutate(search_id = row_number())
  
  get_string_between(search_matrix, search_string = text_2, all_incidents = T)
  
  str_extract_all(text_2, "this is(.*?)sentence")
  
}

