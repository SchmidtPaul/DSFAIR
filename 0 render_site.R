#setwd("D:/Coding/DSFAIR")
pacman::p_load(tidyverse, rmarkdown)
`%not_in%` <- Negate(`%in%`)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

### create html files ###
rmarkdown::clean_site(preview = FALSE)  # delete old files
rmarkdown::render_site(encoding="UTF-8") # render all files new; UTF-8 for ä, ö, ü, ß

### create purled R files ###
purl_files <- list.files(pattern = ".Rmd") %>% 
  tibble(Rmd = .) %>% 
  dplyr::filter(Rmd %not_in% c("0contactinfo.Rmd",
                        "0courseprep.Rmd",
                        "DesigningExperiments.Rmd",
                        "Diagnostics.Rmd",
                        "index.Rmd")) %>% 
  mutate(R = paste0("Rpurl/",str_sub(Rmd, 1, -3)))

for(i in 1:nrow(purl_files)){
  
  knitr::purl(input  = purl_files %>% slice(i) %>% pull(Rmd), 
              output = purl_files %>% slice(i) %>% pull(R),
              documentation = 0)
  
}

