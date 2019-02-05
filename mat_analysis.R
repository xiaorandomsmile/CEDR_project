#read in and tidy
elementsByFacility <- read_csv("raw_csv/elementByFacility.csv")
elementsByFacility_tidy <- elementsByFacility %>% 
  gather("facility", "prop", -elementID) %>% 
  separate(elementID, c("measure", "element"), sep = "\\.")

#get count data
counts <- read_csv("mips_pro_data_removeNa.csv") %>% select(-1)
counts <- counts %>% 
  mutate(Denominator = EGP - Exclusion - Exception, prop = Numerator / Denominator)
