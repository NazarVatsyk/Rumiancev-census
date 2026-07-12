#statevo-vikova piramida
ggplot(tidy_village%>%
         filter(sex=="m")%>%
         filter(age<101&!is.na(age)))+
  geom_bar(aes(cut_width(age,5,boundary = 0)),fill ="blue")+
  ylim(c(0,520))+
  coord_flip()+
  labs(x= " ",y=" ") 
ggplot(tidy_village%>%filter(sex=="f")%>%filter(!is.na(age)))+
  geom_bar(aes(cut_width(age,5,boundary = 0)),fill ="pink")+
  ylim(c(0,520))+
  coord_flip()+
  labs(x= " ",y=" ") 
#ditu
#avg_kids_per_household 
mean(tidy_village%>%
      filter(str_detect(ralation, "syn")|str_detect(ralation, "doch"))%>%
       group_by(village, HH, hata)%>%
       count()%>%pull(n), na.rm= TRUE)

#soc_structure
tidy_village%>%
  group_by(social_status)%>%
  count()%>%
  ggplot()+
  geom_col(aes(fct_reorder(social_status, n),n))+
  labs(title = "Соціальна структура сіл навколо Переяслава", x = "Стан", y ="осіб")

#mean_age
tidy_village%>%
  group_by(social_status)%>%
  summarise(mean_age = mean(age,na.rm=TRUE))%>%
  ggplot()+
  geom_col(aes(social_status, mean_age))+
  labs(title="Середній вік стану",x =  "Стан", y =  "Середній вік")


#health
ggplot(tidy_village%>%
        filter(!is.na(age)))+
  geom_point(aes(age,health, colour=health),position = "jitter")+
  geom_violin(aes(age,health), alpha = 0.5)+
  labs(title = "Стан здоров'я за віком людини", x="Вік",y="Порушення")+
  theme(legend.position="none")
tidy_village%>%
  mutate(h = health=="Здоровий")%>%
  filter(!is.na(age))%>%
  group_by(age)%>%
  summarise(ratio = sum(h)/(sum(h)+sum(!h)))%>%
  ggplot()+
  geom_point(aes(age, ratio*100))+
  stat_smooth(aes(age, ratio*100),level = 0.1)+
  ylim(0,101)+
  labs(title = "Відсоток здорових людей за віком", x = "Вік", y = "%")
ggplot(tidy_village%>%
         filter(health!="Здоровий"))+
  geom_bar(aes(health, fill =social_status), position = "dodge")+
  labs(title = "здоров'я за соціальним групами", x="Стан",y="Порушення")

#workers
workers <- tidy_village%>%
  mutate(hosp_ss=if_else(ralation=="hospodar",social_status,NA))%>%
  fill(hosp_ss)%>%
  filter(str_detect(ralation, regex("pids|pods|rab")))
workers%>%
  group_by(hosp_ss)%>%
  count()%>%
  left_join(tidy_village%>%
              filter(ralation=="hospodar")%>%
              group_by(social_status)%>%
              count(), 
            join_by(hosp_ss==social_status))%>%
  mutate(ratio=n.x/n.y)%>%
  ggplot()+
  geom_col(aes(hosp_ss,ratio))+
  labs(title = "Кількість робітників на одного господаря за станом",x="Стан госопдаря",y="Осіб")

workers%>%
  group_by(social_status)%>%
  count()%>%
  left_join(tidy_village%>%
              filter(age>10)%>%
              group_by(social_status)%>%
              count(),
            join_by(social_status))%>%
  ggplot()+
  geom_col(aes(social_status, n.x/n.y*100))+
  labs(title = "Частка найманих працівників від населення старшого за 10 років", x= "Стан", y ="%",
       caption = "10 років - вік наймолодшого працівника")

#hospodari
tidy_village%>%
  filter(!is.na(`plowed land`))%>%
  mutate(zemli = if_else(`plowed land`=="-", 0, parse_double(`plowed land`, locale= locale(decimal_mark = ","))))%>%
  group_by(social_status)%>%
  summarise(mean = mean(zemli, na.rm=TRUE))%>%
  ggplot()+
  geom_col(aes(social_status,mean))+
  labs(title="Гектарів ріллі на одне господарство",x="Стан господаря",y="Гектарів")

tidy_village%>%
  filter(!is.na(social_status)&ralation=="hospodar")%>%
  group_by(social_status)%>%
  count()%>%
  ggplot()+
  geom_col(aes(fct_reorder(social_status,n),n))+
  labs(title = "Кількість господарів за станом", x = "Стан господаря", y ="Кількість")
ggplot()+
  geom_boxplot(aes(social_status,zemli),outliers = FALSE)+
  labs(title="Гектарів ріллі на одне господарство",x="Стан господаря",y="Гектарів")

#relationship between number of workers and the are of land
tidy_village%>%
  mutate(`plowed land` = if_else(`plowed land`=="-", 0, parse_double(`plowed land`, locale= locale(decimal_mark = ","))))%>%
  filter(ralation=="hospodar"|str_detect(ralation, regex("pids|pods|rab")))%>%
  mutate(hospodar_land = if_else(ralation=="hospodar", `plowed land`, NA))%>%
  fill(hospodar_land)%>%
  group_by(village,HH,hospodar_land)%>%
  count()%>%
  ggplot()+
  geom_point(aes(hospodar_land, n))+
  geom_smooth(aes(hospodar_land, n), method = "lm")
#relationship between number of household members (including workers) and the area of land
tidy_village%>%
  mutate(`plowed land` = if_else(`plowed land`=="-", 0, parse_double(`plowed land`, locale= locale(decimal_mark = ","))))%>%
  mutate(hospodar_land = if_else(ralation=="hospodar", `plowed land`, NA))%>%
  fill(hospodar_land)%>%
  group_by(village,HH,hospodar_land)%>%
  count()%>%
  ggplot()+
  geom_point(aes(hospodar_land, n))+
  geom_smooth(aes(hospodar_land, n), method = "lm")


hudoba <- tidy_village%>%
  filter(!is.na(pigs))%>%
  mutate(pigs = if_else(pigs=="-", 0 , pigs),
         oxen = if_else(oxen=="-", 0 , oxen),
         cow = if_else(cow=="-", 0 ,cow),
         horses = if_else(horses=="-", 0 , horses),
         sheep = if_else(sheep=="-", 0 , sheep),
         apiaries = if_else(apiaries=="-"|is.na(apiaries), 0 , apiaries),
         goats = if_else(goats=="-"|is.na(goats), 0 , goats),
         )%>%
  select(social_status, oxen:goats)%>%
  pivot_longer(oxen:goats, names_to = "hudoba", values_to = "quantity")
hudoba%>%
  group_by(social_status, hudoba)%>%
  summarise(mean = mean(quantity, na.rm=TRUE))%>%
  ggplot()+
  geom_col(aes(hudoba, mean, fill=social_status), position = "dodge")+
  labs(title = "Середня кількість голів худоби на одне господарство за станом господаря", x= "вид худоби", y = "голів")

ggplot(hudoba)+
  geom_boxplot(aes(social_status, quantity, fill = hudoba), outliers = FALSE)+
  labs(title = "Кількість голів худоби на одне господарство за станом господаря", x= "Стан", y = "голів")

 tidy_village%>%
  filter(!is.na(grasslands))%>%
  mutate(grasslands = if_else(grasslands=="-", 0, parse_number(grasslands)))%>%
  group_by(social_status)%>%
  summarise(mean = mean(grasslands, na.rm=TRUE))%>%
  ggplot()+
  geom_col(aes(social_status,mean))+
  labs(title = "Гектарів пасовищ на господарство",x= "Стан господаря",y="Гектарів")

tidy_village%>%
  filter(!is.na(forest))%>%
  group_by(social_status,forest)%>%
  count()%>%
  pivot_wider(names_from = forest, values_from = n)%>%
  mutate(ratio = `+`/(`+`+`-`))%>%
  ggplot()+
  geom_col(aes(social_status,ratio))+
  labs(title= "Частка господарств, які мають доступ до лісу",x="Стан господаря",y="%")

