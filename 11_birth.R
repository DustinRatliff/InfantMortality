#import data

birth <- read_csv("data/birth/RestrictedOhioLiveBirths_Export.csv")

# birth2 <- birth %>%
#   str_replace_all("99", NA)
#     
#     "99" = "NA",
#     "Unknown" = "NA",
#     "Unknown/Not Reported" = "NA"
#   ))



birth_total <- birth %>%
  filter(`Birth Year` == 2016) %>%
  filter(`Residence County` == "Warren") %>%
  # select(`Birth Year`) %>%
  summarise(Total = n()) %>%
  pull()

preterm <- birth %>%
  filter(`Residence County` == "Warren") %>%
  select(`Birth Year`, Gestation) %>%
  mutate(term = 
           ifelse(Gestation < 23, "Less than 23 Weeks Gestation Birth",
                  ifelse(Gestation >= 23 & Gestation < 32, "Very-Preterm Birth",
                         ifelse(Gestation >=23 & Gestation <37, "Preterm Birth", 
                                "Normal Term Birth")))) %>%

  mutate(term = factor(term, levels = c("Less than 23 Weeks Gestation Birth",
                                        "Very-Preterm Birth",
                                        "Preterm Birth",
                                        "Normal Term Birth")))   %>%
  group_by(`Birth Year`, term) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(pct = freq * 100) %>%
  mutate(preterm = 
           ifelse(term == "Less than 23 Weeks Gestation Birth" |
                  term == "Very-Preterm Birth" |
                  term == "Preterm", 
                  "Preterm",
                  "Normal Term Birth")) %>%
  ungroup() %>%
  group_by(`Birth Year`)

preterm_gen <- birth %>%
  filter(`Residence County` == "Warren") %>%
  select(`Birth Year`, Gestation) %>%
  mutate(term = 
           ifelse(Gestation < 37, 
                  "Preterm Birth",
                  "Normal Term Birth")) %>%
  
  mutate(term = factor(term, levels = c("Preterm Birth",
                                        "Normal Term Birth")))   %>%
  group_by(`Birth Year`, term) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(pct = freq * 100)


prenatal <- birth %>%
  filter(`Residence County` == "Warren") %>%
  mutate(prenatal = 
           ifelse(`Month Prenatal Care Began` == "Unknown", NA,
                  ifelse(`Month Prenatal Care Began` < 4, 
                         "Prenatal Care Before First Trimester",
                         "Prenatal Care After First Trimester or No Care"))) %>%
  group_by(`Birth Year`, prenatal) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count/sum(Count) * 100) %>%
  mutate(Percentage = round(Percentage, digits = 1))
  

