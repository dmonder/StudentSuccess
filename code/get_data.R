#' This script contains functions to extract data from the IPEDS files
#' and deliver summary dataframes of interesting information

# get the NACIQI 2018 
get_dashboard <- function(){
  # functions to clean up dashboard
  read_csv("data/Institutional-Performance-by-Accreditor_2021-07-12.csv", guess_max = 1e5) %>% 
    fix_strings() %>% 
    fix_numbers() %>% 
    select(OPEID = 4,6:95) %>% 
    return()
}

#' Get grad rates from IPEDS file
get_grad_rates <- function(program_length){
 
   # friendly descriptions of codes
   grad_codes <- tribble(~GRTYPE,~GradType,
                        8, "Adjusted cohort",
                        9, "150% completers",
                        13, "100% completers",
                        16, "Transfer-out",
                        43, "Still enrolled",
                        44, "Non-completers",
                        29, "Adjusted cohort",
                        30, "150% completers",
                        33, "100% completers",
                        35, "Transfer-out",
                        47, "Still enrolled",
                        48, "Non-completers")
  
   df <- read_csv("data/gr2019.csv") %>% 
        filter(GRTYPE %in% c(8,9,13,16,43,44,29,30,33,35,47,48)) %>% # 2 is cohort size, 3 is grads in 150%
        select(UNITID,
               GRTYPE,
               Total  =     GRTOTLT,
               Men    =    GRTOTLM,
               Women  =    GRTOTLW,
               `American Indian` = GRAIANT,
               Asian = GRASIAT,
               `Black or African American` = GRBKAAT,
               Hispanic = GRHISPT,
               `Pacific Islander` = GRNHPIT,
               White = GRWHITT,
               Multirace = GR2MORT,
               Unknown   = GRUNKNT,
               `Nonresident alien` = GRNRALT)
    
   if (!program_length %in% c(2,4)) stop("program_length must be 2 or 4")
   
   if(program_length == 2){
     df <- df %>% 
       filter(GRTYPE %in% c(29,30,33,35,47,48)) 
   } else { # length = 4
        df <- df %>% 
          filter(GRTYPE %in% c(8,9,13,16,43,44)) 
   }  
   
   # clean up and turn into a long table
   df <- df %>% 
     left_join(grad_codes) %>% 
     select(-GRTYPE) %>% 
     gather(DemoCategory, M, -UNITID, -GradType) %>% 
     group_by(UNITID, DemoCategory) %>% 
     mutate(DemoTotal = max(M)) %>% # assumes cohort is the largest count
     group_by(UNITID) %>% 
     mutate(Cohort = max(M)) %>% 
     ungroup() %>% 
    filter(GradType != "Adjusted cohort") %>% # this is now in its own column
     mutate(DemoFraction = DemoTotal/Cohort, # how many of this category were there out of whole cohort?
            TypeFraction = M / DemoTotal) %>% 
     select(UNITID, Cohort, DemoCategory, DemoTotal, DemoFraction, GradType, GradTypeCount = M, TypeFraction)
   
   return(df)
}

get_retention <- function(){
   
   fnames <- list.files(path = "data", pattern = "ef20.*")
   
   out <- data.frame()
   
   for(fname in fnames){
      year <- as.integer(substr(fname, 3, 6)) - 1
   
      df <- read_csv(paste0("data/",fname)) %>% 
         select(UNITID,
                FT_cohort = RRFTCTA,
                FT_retained = RET_NMF,
                FT_retention = RET_PCF,
                PT_cohort = RRPTCTA,
                PT_retained = RET_NMP,
                PT_retention = RET_PCP) %>% 
         mutate(Year = year)
         
      out <- rbind(out,df)
   }
   
   return(out)
}
   
   