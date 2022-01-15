
str_detect_any <- function(s, elements){
  # check whether each item in the string vector s
  # has at least one item in the string vector elements
  
  # get a list: one "row" per item in s
  #   each row is a logical vector with same length as elements
  logical_list <- purrr::map(s, stringr::str_detect, elements)
  
  # are any of the items in each "row" of the list true?
  purrr::map_lgl(logical_list, any)
  
  # test with the following code:
  # s <- c("str one", "str two", "str 3", "str 4", "my 8")
  # elements <- c("one", "3", "str", "7")
  # 
  # str_detect_any(s, elements)
}

str_extract_before_first <- function(s, first){
  # str_extract(ulabel, '^[^!]+'),  # everything before first !
  pattern <- paste0("^[^", first, "]+")
  stringr::str_extract(s, pattern)
}

str_extract_after_last <- function(s, last){
  # str_extract(ulabel, '![^!]+$'),  # everything after last !
  pattern <- paste0("^[^", last, "]+$")
  stringr::str_extract(s, pattern)
}


#..regex notes ----
# str_extract(ulabel, '![^!]+$'),  # everything after last !
# str_extract(ulabel, '^[^!]+'),  # everything before first !
#  "^[^,]+"  # everything before first ,
# x <- c("abc, def", "hijklm ,zyz")  str_extract(x, "^[^,]+")