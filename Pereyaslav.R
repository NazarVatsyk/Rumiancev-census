#Gender-age distribution
ggplot(tidy_pereyaslav%>%filter(sex=="m", !is.na(age)))+
  geom_bar(aes(cut_width(age, 5,boundary = 0)), fill = "blue")+
  coord_flip()+
  labs(title = " ", x = "", y = "")
ggplot(tidy_pereyaslav%>%filter(sex=="f", !is.na(age)))+
  geom_bar(aes(cut_width(age, 5,boundary = 0)), fill = "pink")+
  labs(title = "Статево-вікова піраміда Переяслава", x = "", y = "")

ggplot(tidy_pereyaslav%>%filter(is.na(social_status)))+
  geom_freqpoly(aes(age), stat = "count")+
  geom_vline(aes(xintercept = 24), color = "red")

tidy_pereyaslav%>%
  group_by(social_status)%>%
  summarise(avg_age = mean(age, na.rm = TRUE))%>%
  filter(!is.na(social_status))%>%
  ggplot()+
  geom_col(aes(fct_reorder(social_status, avg_age), avg_age))+
  coord_cartesian(ylim = c(20, 30))+
  labs(title = "Середній вік найбільших соціальних груп", x = "Стан", y = "Середній вік",
       caption = "")



#Health with respect to age
ggplot(tidy_pereyaslav%>%filter(!is.na(health)))+
  geom_point(aes(x = age, y = health, colour = health), position = "jitter")+
  geom_violin(aes(x = age, y = health), alpha = 0.5)+
  labs(title = "Стан здоров'я за віком людини", x = "Вік", y = "Стан(порушення)" )+
  theme(legend.position="none") 
ggplot(tidy_pereyaslav%>%filter(health!="Здорові"&!is.na(social_status)&social_status!="unknown"))+
  geom_bar(aes(health, fill = social_status), position = "dodge")+
  labs(title = "Хвороби за соціальними групами", x ="Порушення", y = "Кількість осіб")

#Percentage of health people with respect to their age
tidy_pereyaslav%>%
  mutate(Здорові = (health=="Здорові"))%>%
  group_by(age)%>%
  count(Здорові)%>%
  pivot_wider(names_from = Здорові, values_from = n)%>%
  replace_na(list(`TRUE` = 0, `FALSE`=0))%>%
  summarise(ratio = 100*sum(`TRUE`)/(sum(`TRUE`)+sum(`FALSE`)))%>%
  ggplot()+
  geom_point(aes(age, ratio))+
  stat_smooth(aes(age, ratio),level = 0.1)+
  ylim(0,101)+
  labs(title = "Відсоток здорових людей за віком", x = "Вік", y = "%")

#crafts and wealth
tidy_pereyaslav%>%
  select(capital, job, social_status, job_experience)%>%
  filter(!is.na(job)&!is.na(capital))%>%
  ggplot()+
  geom_boxplot(aes(fct_reorder(job, capital, .fun = median, na.rm = TRUE), capital), outliers = FALSE)+
  labs(x = "remeslo")

tidy_pereyaslav%>%
  select(capital, job, social_status, job_experience)%>%
  filter(!is.na(job)&!is.na(capital))%>%
  group_by(job)%>%
  summarise(mean_inc = mean(capital), median_inc = median(capital))

tidy_pereyaslav%>%
  filter(status == "hospodar")%>%
  group_by(social_status)%>%
  filter(!is.na(social_status))%>%
  summarise(mean_inc = mean(capital, na.rm = T))%>%
  ggplot()+
  geom_col(aes(fct_reorder(social_status, mean_inc), mean_inc))+
  labs(title = "Середній дохід за соціальними станами", x= "Стан",y="Дохід")

#number of household-owners for each social status 
tidy_pereyaslav%>%
  filter(status=="hospodar")%>%
  group_by(social_status)%>%
  count()%>%
  ggplot()+
  geom_col(aes(fct_reorder(social_status,n),n))+
  labs(x = "soc_status")


#Workers age distribution with respect to origin and social status
tidy_pereyaslav %>%
  filter(str_detect(status, "rab"))%>%
  mutate(local = root!="Pereyaslav"&root!="Pereyaslav podvarok Zadolhomostianskij"&root!="Pereyaslava"&!is.na(root))%>%
  ggplot() +
  geom_boxplot(aes(fct_reorder(social_status, age, .fun = median, na.rm = TRUE),age,fill = local))+
  labs(x= "status", y = "age")+
  theme_minimal()

tidy_pereyaslav %>%
  filter(str_detect(status, "rabot")) %>%
  summarize(min(age, na.rm = TRUE))

#Workers' income
rabotniki <- tidy_pereyaslav%>%
  filter(str_detect(status, "rab")|status =="hospodar")%>%
  mutate(hosp_ss = if_else(status == "hospodar", social_status, NA),
         hosp_inc = if_else(status == "hospodar", capital, NA),
         hosp_job = if_else(status == "hospodar", job, NA))%>%
  fill(hosp_job)%>%
  fill(hosp_ss)%>%
  fill(hosp_inc)%>%
  filter(status!="hospodar")%>%
  mutate(money = if_else(str_detect(`wage(for_naymyt\`)`, regex("[0-9]")), parse_double(`wage(for_naymyt\`)`, locale = locale(decimal_mark = ",")),NA))%>%
  mutate(wage = as.factor(`wage(for_naymyt\`)`))%>%
  mutate(`Спосіб оплати` = fct_collapse(wage,
                                        `За гроші` = c("Z zarabotku priadivoho","Z zarabotka priazhej khoziajskoj","na 0,5 roku za 40 kop","0,15","0,4", "0,6","0,8", "1","1 v god, odezha i kharchi khozyajskie","1,1","1,2","1,3", "1,4", "1,5", "1,8", "2", "2 v god, odezha khozyajskaya", "2,2", "2,4", "2,5", "3", "3, na odeji hoziayskoy", "3,25", "3,4","3,5", "4", "5", "6","60 kop", "60 kopeek, v odeyanii khozyajskom","7","8", "9"),                     
                                        `За харчування та одяг` = c("Bez zaplaty za propitanie" ,"за пропитание в одежу","Za propitanie i snabzhenie", "Za propitanie I snabzhenie",  "Za propitanie I odezhu" ,"Za propitanie i odezhu", "Za propitanie bez zaplaty" ,"Za propitanie (bol'she pitaetsia z milostynnoho podajanija)","Za propitanie"  ,"Po svojstvu v odeyanii khozyajskom" ,"Na vsem soderzhanii svoem" ,"Na poslushanii", "Na propitanii hoziayskom" ,"Bez zaplaty na vsem svoem soderzhanii" ,"Bez zaplaty na vsem soderzhanii hoziajskom", "Bez zaplaty","Bez zaplaty na vsem soderzhanii hoziajskom"),
                                        `Через борг` = "Za dolh 19 rublej po prohovoru hrodskoho suda"  ,
                                        `За навчання` = c("Za vospitanie i obuchenie remeslu", "Vo izuchenii remesla" ,"bez zaplaty za obuchenie remeslu",  "Za obechenie remesla","Za obuchenie ramaslu", "Za obuchenie remesla", "Za obuchenie remesla, plat`e khozyajskoe", "Za obuchenie remeslu", "Za obuzhenie remeslu shevskomu"),
                                        інше = c("Z diskrecij")))%>%
  select(dvor,age, social_status, sex, job_experience, money, `wage`,`Спосіб оплати`, hosp_ss, hosp_inc, hosp_job)%>%
  mutate(job_experience= if_else(str_detect(job_experience, regex("[0-9]")),parse_number(job_experience), NA))%>%
  filter(is.numeric(job_experience))
cor.test(rabotniki$money, rabotniki$job_experience, method = "pearson")


ggplot(rabotniki)+
  geom_boxplot(aes(x= hosp_ss, y=age))

ggplot(rabotniki)+
  geom_boxplot(aes(cut_width(hosp_inc,width = 2, boundary=1), age))

ggplot(rabotniki)+
  geom_point(aes(job_experience,money))
oplata_hosp_ss <- rabotniki%>%
  filter(!is.na(`Спосіб оплати`))%>%
  group_by(hosp_ss, `Спосіб оплати`)%>%
  count(`Спосіб оплати`)
oplata_rab_ss <- rabotniki%>%
  filter(social_status=="Підсусідки"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Посполиті")%>%
  filter(!is.na(`Спосіб оплати`))%>%
  group_by(social_status, `Спосіб оплати`)%>%
  count(`Спосіб оплати`)
oplata_ratio <- left_join(oplata_hosp_ss,oplata_hosp_ss%>%group_by(hosp_ss)%>%summarise(y = sum(n)),join_by(hosp_ss))%>%
  mutate(ratio = n/y)%>%
  select(hosp_ss, `Спосіб оплати`, ratio)
oplata_rabu_ratio <- left_join(oplata_rab_ss,oplata_rab_ss%>%group_by(social_status)%>%summarise(y = sum(n)),join_by(social_status))%>%
  mutate(ratio = n/y)%>%
  select(social_status, `Спосіб оплати`, ratio)
ggplot(oplata_ratio)+
  geom_col(aes(hosp_ss, ratio*100, fill= `Спосіб оплати`), position = "dodge")+
  labs(title = "Структура оплати роботи за станом господаря",x="Стан господаря",y="%")
ggplot(oplata_rabu_ratio)+
  geom_col(aes(social_status, ratio*100, fill= `Спосіб оплати`), position = "dodge")+
  labs(title = "Структура оплати роботи за станом праціника",x="Стан",y="%")
hospodar_hroshi_platut <- rabotniki%>%
  filter(`Спосіб оплати`=="За гроші")
write.csv2(hospodar_hroshi_platut,"C:\\Users\\ceoet\\Downloads\\Протокол район - Аркуш1.csv")
hospodar_hroshi_platut_mean_wage <- read_csv("C:\\Users\\ceoet\\Downloads\\uhjjis - Протокол район - Аркуш1.csv")%>%
  mutate(wage = parse_double(wage, locale = locale(decimal_mark = ",")))%>%
  group_by(hosp_ss)%>%
  summarise(mean_wage = mean(wage))%>%
  filter(hosp_ss=="Підсусідки"|hosp_ss=="Шляхта"|hosp_ss=="Міщани"|hosp_ss=="Козаки"|hosp_ss=="Посполиті")
ggplot(hospodar_hroshi_platut_mean_wage)+
  geom_col(aes(fct_reorder(hosp_ss, mean_wage), mean_wage))+
  labs(title="Середня зарплата у господаря за станом",x="Стан господаря",y="Середня зарплата")
ggplot(oplata_ratio)+
  geom_col(aes(social_status, ratio*100, fill= `Спосіб оплати`), position = "dodge")+
  labs(title = "Структура оплати роботи за станом господаря",x="Стан господаря",y="%")
zarplata_za_stanom <- read_csv("C:\\Users\\ceoet\\Downloads\\uhjjis - Протокол район - Аркуш1.csv")%>%
  mutate(wage = parse_double(wage, locale = locale(decimal_mark = ",")))%>%
  group_by(social_status)%>%
  summarise(mean_wage = mean(wage))%>%
  filter(social_status=="Підсусідки"|social_status=="Шляхта"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Посполиті")

ggplot(zarplata_za_stanom)+
  geom_col(aes(fct_reorder(social_status, mean_wage), mean_wage))+
  labs(title="Середня зарплата за станом",x="Стан",y="Середня зарплата")
  

number_of_hospodars <- tidy_pereyaslav%>%
  filter(status == "hospodar")%>%
  filter(social_status=="Підсусідки"|social_status=="Шляхта"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Посполиті")%>%
  count(social_status)%>%
  mutate(nhosp = n)

rabotniki_number <- rabotniki%>%
  filter(hosp_ss=="Підсусідки"|hosp_ss=="Шляхта"|hosp_ss=="Міщани"|hosp_ss=="Козаки"|hosp_ss=="Посполиті")%>%
  group_by(hosp_ss)%>%
  count()%>%
  mutate(nrab = n)
hosp_per_rabot <- left_join(number_of_hospodars, rabotniki_number, join_by(social_status==hosp_ss))%>%
  mutate(ratio = nrab/nhosp)

ggplot(hosp_per_rabot%>%filter(social_status!="Шляхта"))+
  geom_col(aes(fct_reorder(social_status,ratio), ratio))+
  labs(title = "Кількість робітників на господаря", x = "Стан господаря", y = "Кількість робітників")


ggplot(rabotniki%>%filter(social_status=="Підсусідки"|social_status=="Шляхта"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Посполиті"))+
  geom_bar(aes(social_status))+
  labs(title = "Сумарна кількість робітників на ")

pruizd <- tidy_pereyaslav%>%
  filter(root!="Pereyaslav"&root!="Pereyaslav podvarok Zadolhomostianskij"&root!="Pereyaslava"&!is.na(root))%>%
  filter(!is.na(social_status)&social_status!="unknown")
ggplot(pruizd)+
  geom_boxplot(aes(social_status, age))
ggplot(pruizd%>%group_by(social_status)%>%count)+
  geom_col(aes(fct_reorder(social_status, n),n))

ratio_of_aliens <- left_join(pruizd%>%count(social_status),social_structure, join_by(social_status))%>%
  mutate(ratio=n.x/n.y)%>%
  select(social_status, ratio)
ggplot(ratio_of_aliens%>%filter(social_status=="Підсусідки"|social_status=="Шляхта"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Посполиті"))+
  geom_col(aes(fct_reorder(social_status, ratio), ratio*100))+
  labs(title="Частка людей, які не походять з Переяслава",x ="Стан",y="%")
ggplot(rabotniki)+
  geom_bar(aes(hosp_ss))


pospoluti_rabochi <- tidy_pereyaslav%>%
  filter(social_status=="Посполиті",!is.na(status))%>%
  mutate(rabotnik = (status=="rabotnik"|status=="rabotnitsa"|status=="rabotnica"))%>%
  mutate(rabotnik = if_else(rabotnik==TRUE, "Так","Ні"))
ggplot(pospoluti_rabochi%>%group_by(rabotnik)%>%count())+
  geom_col(aes(rabotnik, n, fill=rabotnik))+
  theme(legend.position="none")+
  labs(title ="Чи працюють в  наймі посполиті", x = " ", y = "Кількість")
pruizd <- pruizd%>%
  mutate(marital = fct_collapse(marital, married = c("married", "maried"), unmarried = c("unmarried"), widowed = c("vdov", "vdova")))
pruizd%>%
  group_by(social_status, marital)%>%
  count()%>%
  left_join(pruizd%>%group_by(social_status)%>%count(), join_by(social_status))%>%
  group_by(social_status, marital)%>%
  summarize(ratio = n.x/n.y)%>%
  ggplot()+
  geom_col(aes(social_status, ratio, fill = marital), position = "dodge")

tidy_pereyaslav%>%
  filter(age>quantile((tidy_pereyaslav%>%filter(marital =="married"))$age, probs = 0.01, na.rm = TRUE))%>%
  mutate(is_local = root=="Pereyaslav"|root=="Pereyaslav podvarok Zadolhomostianskij"|root=="Pereyaslava"|is.na(root))%>%
  group_by(social_status, is_local, marital)%>%
  count()%>%
  left_join(tidy_pereyaslav%>%
              filter(age>quantile((tidy_pereyaslav%>%filter(marital =="married"))$age, probs = 0.01, na.rm = TRUE))%>%
              mutate(is_local = root=="Pereyaslav"|root=="Pereyaslav podvarok Zadolhomostianskij"|root=="Pereyaslava"|is.na(root))%>%
              group_by(social_status,is_local)%>%
              count(),
            join_by(social_status, is_local))%>%
  group_by(social_status, is_local, marital)%>%
  summarize(ratio = n.x/n.y)%>%
  filter(social_status=="Посполиті"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Підсусідки")%>%
  ggplot()+
  geom_col(aes(marital, ratio, fill = is_local), position = "dodge")+
  facet_wrap(vars(social_status))

tidy_pereyaslav%>%
  filter(age>quantile((tidy_pereyaslav%>%filter(marital =="married"))$age, probs = 0.01, na.rm = TRUE))%>%
  mutate(is_working = str_detect(status, 'rabot') & !is.na(status),
    is_local = root=="Pereyaslav"|root=="Pereyaslav podvarok Zadolhomostianskij"|root=="Pereyaslava"|is.na(root))%>%
  group_by(social_status, marital)%>%
  filter(is_local==FALSE)%>%
  ggplot()+
  geom_boxplot(aes(social_status, age, colour = is_working))



