library(tidyverse)

#read in and tidy
elementsByFacility <- read_csv("raw_csv/elementByFacility.csv")
elementsByFacility_tidy <- elementsByFacility %>% 
  gather("facility", "prop", -elementID) %>% 
  separate(elementID, c("measure", "element"), sep = "\\.")

#textual data on spreads
elementsByFacility_tidy %>% 
  group_by(measure, element) %>% 
  summarise(min = min(prop), max = max(prop), mean = mean(prop), median = median(prop), sd = sd(prop)) %>% 
  write_csv("measure_by_element_summary.csv")

#list of the measyres
measures<- elementsByFacility_tidy %>% 
  select(measure) %>% 
  distinct() %>% 
  pull()

#iterate over each measure to generate a dot plot of element values, 
#excluding those that are perfect ie = 1
plots_without1 <- map(measures,
                      ~ggplot(elementsByFacility_tidy %>% 
                                filter(measure == .x, prop != 1),
                              aes(x = prop, y = element)) +
                        geom_jitter(alpha = 0.5, height = 0, width = .03) +
                        theme_bw() +
                        labs(x = "Value", y = "Element", title = .x))

#iterate over each measure to generate a dot plot of element values
plots_with1 <- map(measures,
                   ~ggplot(elementsByFacility_tidy %>% 
                             filter(measure == "X19"),
                           aes(x = prop, y = element)) +
                     geom_point(alpha = 0.5) +
                     theme_bw() +
                     labs(x = "Value", y = "Element", title = "X19"))

#ggsave the plots without 1
map2(plots_without1, 
     measures, 
     ~ ggsave(plot = .x, filename = paste0("plots_without1/",.y, ".tiff")))

#ggsave the plots with 1
map2(plots_with1, 
     measures, 
     ~ ggsave(plot = .x, filename = paste0("plots_with1/",.y, ".tiff")))
