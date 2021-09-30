# tables for output
mean_outcomes_table <- function(my_peers){
  
  # generate stats
  my_stats <- function(x){
    avg <- mean(x, na.rm = TRUE)
    stdev <- sd(x, na.rm = TRUE)
    qs <- quantile(x, c(.25, .5, .75), na.rm = TRUE)
    list(avg, stdev, qs[1], qs[2], qs[3])
  }
  
  comps <- my_peers %>% 
    select(-UNITID,-OPEID) %>% 
    filter(Peer) %>% 
    select_if(is.numeric) %>% 
    gather(Metric, Value) %>% 
    group_by(Metric) %>% 
    summarize(`Peer avg` =mean(Value, na.rm = TRUE),
              `Peer stdev` = sd(Value, na.rm = TRUE),
              `Peer q25` = quantile(Value, .25, na.rm = TRUE),
              `Peer q50` = quantile(Value, .50, na.rm = TRUE),
              `Peer q75` = quantile(Value, .75, na.rm = TRUE) )
    
  us <-   my_peers %>% 
    select(-UNITID,-OPEID) %>% 
    filter(!Peer) %>% 
    select_if(is.numeric) %>% 
    gather(Metric, Us)
  
  comps <- us %>% 
    left_join(comps) %>% 
    mutate(Z = (Us - `Peer avg`)/`Peer stdev`) %>% 
    filter(!is.na(Us)) %>% 
    select(-`Peer stdev`)
  
  return(comps)
}

# graduation rates
# tables for output
grad_outcomes_table <- function(grad_rates, my_UNITID){
  
  comps <- grad_rates %>%
    filter(UNITID != my_UNITID,
           GradType %in% c("100% completers",
                           "150% completers",
                           "Transfer-out")) %>% 
    group_by(DemoCategory, GradType) %>% 
    summarize(`Avg` = sum(GradTypeCount, na.rm = TRUE) / sum(DemoTotal, na.rm = TRUE)) %>% 
    mutate(GradType = paste(GradType, "(Peers)")) %>% 
    spread(GradType, Avg) %>% 
    mutate(`150% + Transfer (Peers)` = `150% completers (Peers)` + `Transfer-out (Peers)`)
  
  us <-    grad_rates %>%
    filter(UNITID == my_UNITID,
           GradType %in% c("100% completers",
                           "150% completers",
                           "Transfer-out")) %>% 
    mutate(GradType = paste(GradType, "(Us)")) %>% 
    select(DemoCategory, GradType, TypeFraction, DemoTotal) %>% 
    spread(GradType, TypeFraction) %>% 
    mutate(`150% + Transfer (Us)` = `150% completers (Us)` + `Transfer-out (Us)`)

  comps <- us %>% 
    left_join(comps) %>% 
    select(Category = DemoCategory,
           N = DemoTotal,
           `100% (Us)` = `100% completers (Us)`,
           `100% (Peers)` = `100% completers (Peers)`,
           `150% (Us)` = `150% completers (Us)`,
           `150% (Peers)` = `150% completers (Peers)`,
           `150% + Trans. (Us)` = `150% + Transfer (Us)`,
           `150% + Trans. (Peers)` = `150% + Transfer (Peers)`) %>% 
    mutate(Category = ifelse(Category == "Total","*Total*",Category)) %>% 
    arrange(Category)
  
  return(comps)
}