library(data.table)
library(googlesheets)
library(ggplot2)

suicide <- gs_title("china_suicide") %>% 
  gs_read()

suicide2 <- melt(suicide, id.vars = c("year", "sex", "area"), 
                 variable.name = "age", value.name = "rate")
setDT(suicide2)

ggplot(suicide2[age != "total" & sex != "all"]) + 
  geom_point(aes(age, rate, color = area, shape = sex)) + 
  geom_line(aes(age, rate, group = interaction(area, sex), color = area, linetype = sex)) + 
  theme_minimal() + 
  labs(title = "China Suicide Mortality Rate, 2012", 
       subtitle = "The rate is measured by suicides per 100,000 people.", 
       caption = "Data: 2013 China Health Yearbook")

ggplot() +
  geom_col(aes(sex, rate, fill = area), position = "dodge", width = 0.3, 
           data = suicide2[age == "total"]) + 
  theme_minimal()
