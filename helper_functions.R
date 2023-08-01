search_var = function(term){
  if(!exists("v21")){
    v21 = load_variables(2021, "acs5")
  }
  
  v21[grep(term, v21$label, ignore.case =T),]
}

get_var_label = function(var){
  if(!exists("v21")){
    v21 = load_variables(2021, "acs5")
  }
  
  v21 %>% filter(name == var) %>% pull(label)
}

get_var_labels = function(column) {
  sapply(column, get_var_label)
}