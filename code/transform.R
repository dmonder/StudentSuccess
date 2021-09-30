#' downgrade multibyte strings
fix_strings <- function(df){
  df %>% 
    mutate(across(
      .cols = where(is.character), 
      .fns = ~iconv(.x, "latin1", "ASCII")))
}

#' change dollar amounts from strings to floats
fix_numbers <- function(df){
  
  fix.dollar <- function(x){
    if(!is.character(x)) return(x)
    my_value <- max(x, na.rm = TRUE)
    if(is.na(my_value)) return(x)
    if( !str_detect(my_value, "\\$")) return(x) # the max is to weed out NAs
    x <- str_replace(x,"\\(","-") # negatives are in paren
    x <- str_remove(x, "\\)") # remove right paren if any
    x <- str_remove(x, "\\$") # remove $
    x <- str_replace(x, " M","000000") #000000")
    x <- str_remove_all(x, ",") # remove commas
    return(as.integer(x))
  }
  
  fix.percent <- function(x){
    if(!is.character(x)) return(x)
    my_value <- max(x, na.rm = TRUE)
    if(is.na(my_value)) return(x)
    if( !str_detect(my_value,"%")) return(x)
    x <- str_remove(x, "%") # remove $
    return(as.integer(x))
  }
  
  df <- df %>% 
    map_df(fix.dollar) %>% 
    map_df(fix.percent) %>% 
    return()
}

#' pick appropriate number of digits for display
format_numbers <- function(x){
  
  if(is.character(x)) return(x)
  xout <- x # otherwise, x gets converted to char on row 1
  
  for(row in 1:length(x)){
    xt <- x[row]
    Percent <- (xt>= 0 && xt <= 1)
  
    xout[row] <- case_when(
      xt >= 1e9 ~ paste0( round(xt / 1e9,1) ,"B"), # billions
      xt >= 1e6 ~ paste0( round(xt / 1e6,1) ,"M"), # millions
      xt >= 1e3 ~ paste0( round(xt / 1e3,1) ,"K"), # millions
      xt >= 10  ~ as.character(round(xt)),         # smaller numbers
  #    Percent    ~ paste0(round(xt*1000)/10,"%"),
      is.nan(xt)  ~ "-",
      is.na(xt)   ~ "-",
      TRUE       ~ as.character(round(xt*10)/10)
    )
  }
  return(xout)
}
