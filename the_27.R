library(tidyverse)
library(lubridate)
library(rvest)
library(DataEditR)

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
#
major_cases <- c("Drug overdose","Traffic collision","Murdered","Drowned in a swimming pool","Alcohol poisoning","Heart failure")


ggplot(df1, aes(x=reorder(
  official_cause_of_death,table(
    official_cause_of_death)[official_cause_of_death])))+
  geom_bar()+
  coord_flip()+
  xlab("")+
  ggtitle("Official cause of death")+
  geom_curve(data = data.frame(x = c(22.8625148784086,
                                     22.8625148784086,
                                     22.8625148784086),
                               y = c(8.94214566548643,
                                     8.94214566548643,
                                     8.94214566548643),
                               xend = c(32.0334454119525,
                                        30.0178562837011,
                                        31.1264303042394),
                               yend = c(10.1319228732662, 7.13944782672745,
                                        8.20288771157972)),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
           arrow = arrow(30L, unit(0.1, "inches"), "last", "closed"),
           inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = 21.8547203142829,
                              y = 8.64842535385257,
                              label ="major cases"),
            mapping = aes(x = x, y = y,
                          label = label),
            colour = "red",
            inherit.aes = FALSE,size=6)+
  gghighlight::gghighlight(official_cause_of_death %in% major_cases)+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
                        title = element_text(size=20),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  geom_curve(data = data.frame(x = c(16.0412784853583,
                                     2.96702660699448,
                                     10.0599802216788),
                               y = c(4.17084168207853,
                                     4.30162133818894,
                                     4.26622245231903),
                               xend = c(16.0412784853583,
                                        2.96702660699448,
                                        10.0599802216788),
                               yend = c(1.18847417070886,
                                        1.17765774319342,
                                        1.16880812300336)),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0L,
             arrow = arrow(30L, unit(0.1, "inches"),"last", "closed"),
             inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = c(16.0511155455454,
                                    10.0599802216788,
                                    2.99653958888545),
                              y = c(4.89553840955319,
                                    4.98895237246832,
                                    5.04500091226125),
            label = c("Jim Morrison", "Brian Jones", "Amy Winehouse" )),
            mapping = aes(x = x, y = y, label = label),
            size = 5.64, colour = "red", inherit.aes = FALSE)
#koniec wykressu
df1 %>% filter(official_cause_of_death == "Drowned in a swimming pool")
df1 %>% filter(official_cause_of_death == "Alcohol poisoning")
df1 %>% filter(name == "Jim Morrison")
#Kurt Cobain (1967-1994) Kurt Cobain performing as part of Nirvana. ...
#Amy Winehouse (1983-2011) ...
#Jim Morrison (1943-1971) ...
#Jean-Michel Basquiat (1960-1988) ...
#Janis Joplin (1943-1970) ...
#Robert Johnson (1911-1938) ...
#Jimi Hendrix (1942-1970) ...
#Brian Jones (1942-1969)
df1 %>% glimpse()
df1$date_of_birth <- as.Date(df1$date_of_birth)
df1$date_of_death <- as.Date(df1$date_of_death)
df1 %>% glimpse()
#
df2 <- df1 %>%
  arrange(name) %>%
  #moje wyżyny intelektualne
  mutate(number = 1:nrow(df1)) %>%
  select(name,number,date_of_birth,date_of_death) %>% 
  pivot_longer(cols = date_of_birth:date_of_death, names_to = "date")
#
df2$name <-  gsub("[^a-zA-Z]", " ", df2$name)



df3 <-  df2 %>%
  left_join(df2[seq(from = 1, to = 124,by = 2),] %>%
                    mutate(value_predykcja_daty_start_output = value-1800))

df4 <- df3 %>%
  full_join(df2[seq(from = 2, to = 124,by = 2),] %>% 
              mutate(value_predykcja_daty_koniec_output = value+1800))
#
#
ggplot(df2,aes(value,reorder(name,-number), group = number))+
  geom_line()+
  geom_point(col = "grey40")+
  geom_text(aes(x= df4$value_predykcja_daty_start_output, label = value))+
  geom_text(aes(x= df4$value_predykcja_daty_koniec_output, label = value))+
  theme_minimal()+xlab("")+ylab("")+ggtitle("27 Club on the timeline")+
  scale_x_date(date_breaks = "10 year", date_labels = "%Y")+
  theme(title = element_text(size=20))

