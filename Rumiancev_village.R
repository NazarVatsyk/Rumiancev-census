library("tidyverse")
library(ggiraph)
village_data <- read_csv("C:\\Users\\ceoet\\Downloads\\clean_village - Аркуш1 (5).csv")%>%mutate(age = parse_number((age)))
village_data%>%group_by(ralation)%>%count()%>%print(n=251)
village_data%>%filter(str_detect(ralation, regex("rab|pid|^pod")))%>%count()
village_data%>%filter(is.na(ralation))%>%count
tidy_village <- village_data%>%
  fill(HH,hata,village)%>%
  mutate(ralation = as.factor(ralation), social_status = as.factor(`soc. status`),
         health=as.factor(health),
         social_status = fct_collapse(social_status,
                                      Козаки = c("k", "k,"),
                                      Посполиті = "p",
                                      Шляхта = "shlyakht.",
                                      Клір = "s"))%>%
  mutate(zemli = if_else(`plowed land`=="-", 0, parse_double(`plowed land`, locale= locale(decimal_mark = ","))))%>%

  mutate(health = fct_collapse(health,
                               Здоровий = c("healthy","health"),
                               Зору = c("right eye does not see","can’t work because of old age, blind" ,"healthy, blind on right eye" , "eye disease" ,"blind on right eye"  ,"blind on left eye","blind","blind on one eye" ,"left wall-eye"),
                               Слуху = c("deaf","voiceless  and deaf", "voiceless"  ),
                               `Опорно-рухового апарату` = c("no  right hand" ,"crippled on one hand"    ,  "right hand damaged","right and left hand cripple","no right foot","Lame on left leg"  ,"lame on right hand" ,"lame","ill (sovsem iskalechena)" ,"ill (bolen na nohi I ruki)","decrepit" , "cripple", "crippled on left leg","crippled on both hands","bandy on left leg"  ,"weak in legs", "lame on right leg","lame on one leg","lame on left leg"   ,"ill legs","bandy-legged", "crippled on hands and legs","crippled on legs","has not left leg"),
                               
                               Інші = c("?", "Mangle sick confusion in the head"   ,"dumb",  "without a nose", "unknown"  ,"Ill (gostec)"     ,"unknown where he lives","scabby","poor health","Ill (na lico boleet)", "ill (gostec)" ,"ill"  ,"crippled by illness","disappeared without a trace","epilepsy (paduchaya bolezh`)" ),
                               Старість = c("old age is weak","weak because of old age"  ,"Can’t work because of old age" ,"old","can’t work because of old age", "lame on right leg, weak because of old age")))%>%
  filter(!is.na(social_status)&social_status!="unknown"&social_status!="m"&social_status!="d.")
#statevo-vikova piramida
ggplot(village_data%>%filter(sex=="m")%>%filter(age<101&!is.na(age)))+
  geom_bar(aes(cut_width(age,5,boundary = 0)),fill ="blue")+
  ylim(c(0,520))+
  coord_flip()+
  labs(x= " ",y=" ") 
ggplot(village_data%>%filter(sex=="f")%>%filter(!is.na(age)))+
  geom_bar(aes(cut_width(age,5,boundary = 0)),fill ="pink")+
  ylim(c(0,520))+
  coord_flip()+
  labs(x= " ",y=" ") 
#ditu
kids <- tidy_village%>%
  filter(str_detect(ralation, "syn")|str_detect(ralation, "doch"))%>%
  group_by(village, HH, hata)%>%
  count()
avg_kids_per_household <- mean(kids$n, na.rm= TRUE)

#soc_structure
soc_structure <- tidy_village%>%
  group_by(social_status)%>%
  count()
soc_structure_over60 <- tidy_village%>%
  filter(age>59)%>%
  group_by(social_status)%>%
  count()%>%
  filter(social_status == "Козаки"|social_status=="Посполиті")
ggplot(soc_structure)+
  geom_col(aes(fct_reorder(social_status, n),n))+
  labs(title = "Соціальна структура сіл навколо Переяслава", x = "Стан", y ="осіб")
ggplot(soc_structure_over60)+
  geom_col(aes(fct_reorder(social_status, n),n))+
  labs(title = "Соціальна структура сіл навколо Переяслава", x = "Стан", y ="осіб")


#mean_age
mean_age <- tidy_village%>%
  group_by(social_status)%>%
  summarise(mean_age = mean(age,na.rm=TRUE))%>%
  filter(social_status == "Козаки"|social_status=="Посполиті")
ggplot(mean_age)+
  geom_col(aes(social_status, mean_age))+
  labs(title="Середній вік стану",x =  "Стан", y =  "Середній вік")


#health
ggplot(tidy_village%>%
        filter(!is.na(age)))+
  geom_point(aes(age,health, colour=health),position = "jitter")+
  geom_violin(aes(age,health), alpha = 0.5)+
  labs(title = "Стан здоров'я за віком людини", x="Вік",y="Порушення")+
  theme(legend.position="none")
zdorovi_ratio <- tidy_village%>%
  mutate(h = health=="Здоровий")%>%
  filter(!is.na(age))%>%
  group_by(age)%>%
  summarise(ratio = sum(h)/(sum(h)+sum(!h)))
ggplot(zdorovi_ratio)+
  geom_point(aes(age, ratio*100))+
  stat_smooth(aes(age, ratio*100),level = 0.1)+
  ylim(0,101)+
  labs(title = "Відсоток здорових людей за віком", x = "Вік", y = "%")
ggplot(tidy_village%>%
         filter(!is.na(social_status)&(social_status == "Козаки"|social_status=="Посполиті")&health!="Здоровий"))+
  geom_bar(aes(health, fill =social_status), position = "dodge")+
  labs(title = "Стан здоров'я за соціальним групами", x="Стан",y="Порушення")
#rabotniki
str_detect("Yakim's pidsusidok" , "pidsusidok")
rabotniki <- tidy_village%>%
  mutate(hosp_ss=if_else(ralation=="hospodar",social_status,NA))%>%
  fill(hosp_ss)%>%
  filter(str_detect(ralation, "pidsusidok")|
           str_detect(ralation, "podsusidok")|
           str_detect(ralation, "podsusedka" )|
           str_detect(ralation, "rabotnik" )|
           str_detect(ralation, "rabotnitsa")|
           str_detect(ralation, "podsusedok" )|
         str_detect(ralation,"rabornik"))
rabotniki_per_hosp_ratio <- left_join(rabotniki%>%group_by(hosp_ss)%>%count(),tidy_village%>%filter(ralation=="hospodar")%>%group_by(social_status)%>%count(), join_by(hosp_ss==social_status))%>%
  mutate(ratio=n.x/n.y)
ggplot(rabotniki_per_hosp_ratio)+
  geom_col(aes(hosp_ss,ratio))+
  labs(title = "Кількість робітників на одного господаря за станом",x="Стан госопдаря",y="Осіб")
ggplot(left_join(rabotniki%>%group_by(social_status)%>%count(),
                 tidy_village%>%filter(age>10&(social_status == "Козаки"|social_status=="Посполиті"))%>%group_by(social_status)%>%count(),
                 join_by(social_status)))+
  geom_col(aes(social_status, n.x/n.y*100))+
  labs(title = "Частка найманих працівників від населення старшого за 10 років", x= "Стан", y ="%")

#hospodari
zemli <- tidy_village%>%
  filter(!is.na(`plowed land`))%>%
  filter(social_status!="Клір")%>%
  mutate(zemli = if_else(`plowed land`=="-", 0, parse_double(`plowed land`, locale= locale(decimal_mark = ","))))%>%
  group_by(social_status)%>%
  summarise(mean = mean(zemli, na.rm=TRUE))
ggplot(zemli)+
  geom_col(aes(social_status,mean))+
  labs(title="Гектарів ріллі на одне господарство",x="Стан господаря",y="Гектарів")
ggplot(tidy_village%>%filter(social_status!="Клір"&!is.na(social_status)&ralation=="hospodar")%>%group_by(social_status)%>%count)+
  geom_col(aes(fct_reorder(social_status,n),n))+
  labs(title = "Кількість господарів за станом", x = "Стан господаря", y ="Кількість")
ggplot(tidy_village%>%mutate()%>%filter(!is.na(zemli)&!is.na(social_status)&social_status!="Клір"))+
  geom_boxplot(aes(social_status,zemli),outliers = FALSE)+
  labs(title="Гектарів ріллі на одне господарство",x="Стан господаря",y="Гектарів")

rabotniki_na_hosp_to_zemlya_ratio <- tidy_village%>%
  mutate(`plowed land` = if_else(`plowed land`=="-", 0, parse_double(`plowed land`, locale= locale(decimal_mark = ","))))%>%
  filter(ralation=="hospodar"|
           str_detect(ralation, "pidsusidok")|
           str_detect(ralation, "podsusidok")|
           str_detect(ralation, "podsusedka" )|
           str_detect(ralation, "rabotnik" )|
           str_detect(ralation, "rabotnitsa")|
           str_detect(ralation, "podsusedok" )|
           str_detect(ralation,"rabornik"))%>%
  mutate(hospodar_land = if_else(ralation=="hospodar", `plowed land`, NA))%>%
  fill(hospodar_land)%>%
  group_by(village,HH)
rabotniki_na_hosp_to_zemlya <- left_join(
  rabotniki_na_hosp_to_zemlya_ratio%>%count(),
  rabotniki_na_hosp_to_zemlya_ratio%>%summarise(hospodar_land),
  join_by(village,HH))%>%
  mutate(num = row_number())%>%
  filter(num==1)%>%
  mutate(n = n-1)
ggplot(rabotniki_na_hosp_to_zemlya)+
  geom_point(aes(hospodar_land, n))+
  geom_smooth(aes(hospodar_land, n))
#correlation between plowed land and number of people in household
zemlya_to_family_members <- left_join(
  tidy_village%>%filter(!is.na(`plowed land`)),
  tidy_village%>%group_by(village, HH)%>%count(),
  join_by(village, HH)
)%>%
  mutate(land=if_else(`plowed land`=="-", 0, parse_double(`plowed land`, locale = locale(decimal_mark = ","))))%>%
  select(village,HH,n, land)
ggplot(zemlya_to_family_members)+
  geom_point(aes(land, n))+
  stat_smooth(aes(land, n), method = "lm")+
  coord_cartesian(xlim=c(0,250))+
  labs(title = "Відношення площі ріллі до кількості членів домогосподарства", x ="рілля, га", y = "осіб")
corr <- cor.test(zemlya_to_family_members$n, zemlya_to_family_members$land,method = "pearson")
p <- ggplot(zemlya_to_family_members) +
  geom_point_interactive(
    aes(land, n, colour = village, data_id = village),
    size = 2,
    alpha = 0.2
  ) +
  geom_point_interactive(
    aes(land, n, colour = village, data_id = village),
    size = 5,
    alpha = 0.8
  )

w <- girafe(
  ggobj = p,
  options = list(
    opts_selection(type = "single", only_shiny = FALSE),
    opts_selection(css = "opacity:1;"),
    opts_selection_inv(css = "opacity:0.1;")
  )
)
htmlwidgets::saveWidget(w, "test.html", selfcontained = TRUE)
browseURL("test.html")
#economy
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
  filter(social_status!="Клір")%>%
  pivot_longer(oxen:goats, names_to = "hudoba", values_to = "quantity")
hudoba_per_social_stan <- hudoba%>%
  group_by(social_status, hudoba)%>%
  summarise(mean = mean(quantity, na.rm=TRUE))
ggplot(hudoba_per_social_stan)+
  geom_col(aes(hudoba, mean, fill=social_status), position = "dodge")+
  labs(title = "Середня кількість голів худоби на одне господарство за станом господаря", x= "вид худоби", y = "голів")
ggplot(hudoba%>%filter(hudoba!="apiaries"&hudoba!="goats"))+
  geom_boxplot(aes(social_status, quantity, fill = hudoba), outliers = FALSE)+
  labs(title = "Кількість голів худоби на одне господарство за станом господаря", x= "Стан", y = "голів")
grassland <- tidy_village%>%
  filter(!is.na(grasslands))%>%
  mutate(grasslands = if_else(grasslands=="-", 0, parse_number(grasslands)))%>%
  group_by(social_status)%>%
  summarise(mean = mean(grasslands, na.rm=TRUE))%>%
  filter(social_status!="Клір")
ggplot(grassland)+
  geom_col(aes(social_status,mean))+
  labs(title = "Гектарів пасовищ на господарство",x= "Стан господаря",y="Гектарів")
forest <- tidy_village%>%
  filter(!is.na(forest)&social_status!="Клір")%>%
  group_by(social_status,forest)%>%
  count()%>%
  pivot_wider(names_from = forest, values_from = n)%>%
  mutate(ratio = `+`/(`+`+`-`))
ggplot(forest)+
  geom_col(aes(social_status,ratio))+
  labs(title= "Частка господарств, які мають доступ до лісу",x="Стан господаря",y="%")

tidy_pereyaslav%>%
  mutate(is_local = if_else(!str_detect(root, regex("^Pereyaslav", ignore_case = T)),F,T))%>%
  filter(ralation == "nonkin"&sex%in%c('f', 'm'))%>%
  ggplot()+
  geom_histogram(aes(x = sex), stat = 'count')+
  facet_wrap(~is_local)
 