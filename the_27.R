library(tidyverse)
library(lubridate)
library(rvest)

#pobieranie danych
link <- "https://en.wikipedia.org/wiki/27_Club"
df00 <- read_html(link) %>%
        html_nodes(xpath= '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
        html_table()

#czyszczenie
df0 <- df00[[1]]
df <- df0 %>% janitor::clean_names()
df <- df[,1:6]
#manipulacja
#urodziny
df <- df %>%
  separate(date_of_birth,
           into = c("month_of_birth","day_of_birth","year_of_birth")) %>%
  mutate(month_of_birth = match(month_of_birth,month.name)) %>% 
  unite(date_of_birth,
          c("month_of_birth","day_of_birth","year_of_birth"),sep = "-")
#śmierć
df <- df %>%
  separate(date_of_death,
           into = c("month_of_death","day_of_death","year_of_death")) %>%
  mutate(month_of_death = match(month_of_death,month.name)) %>% 
  unite(date_of_death,
        c("month_of_death","day_of_death","year_of_death"),sep = "-")
#zamiana na datę
df$date_of_birth <-  df$date_of_birth %>% mdy() %>% as.Date()
df$date_of_death <- df$date_of_death %>% mdy() %>% as.Date()
df$date_of_birth <- as.factor(df$date_of_birth)
df$date_of_death <- as.factor(df$date_of_death)
df1 <- read.csv("./df1.csv")
df1 <- df1[,2:7]

df1$official_cause_of_death <- as.factor(df1$official_cause_of_death)

#ład i porządek, jak u mnie w pokoju
remove(list = base::setdiff(ls(),"df1"))
#dane
df1 %>% glimpse()
df1 %>% introduce() %>% t()
#
do_wykresu <- df1 %>% group_by(official_cause_of_death) %>% summarise(cases = n())
do_wykresu %>% ggplot(aes(official_cause_of_death,cases))+geom_col()

wykres1 <-  ggplot(df1, aes(x=reorder(official_cause_of_death,table(official_cause_of_death)[official_cause_of_death]))) + geom_bar()+
  coord_flip()+xlab("")+ggtitle("Official cause of death")

wykres1+
geom_curve(data = data.frame(x = c(22.8625148784086, 22.8625148784086, 22.8625148784086),
                             y = c(8.94214566548643, 8.94214566548643, 8.94214566548643),
                             xend = c(32.0334454119525, 30.0178562837011, 31.1264303042394),
                             yend = c(10.1319228732662, 7.13944782672745, 8.20288771157972)),
           mapping = aes(x = x, y = y, xend = xend, yend = yend),
           arrow = arrow(30L, unit(0.1, "inches"),
                         "last", "closed"),
           inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = 21.8547203142829, y = 8.64842535385257, label ="major cases"),
            mapping = aes(x = x, y = y, label = label),
            colour = "red", inherit.aes = FALSE)+theme_minimal()

