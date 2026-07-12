#Gender-age distribution
ggplot(tidy_pereyaslav%>%filter(sex=="m", !is.na(age)))+
  geom_bar(aes(cut_width(age, 5,boundary = 0)), fill = "blue")+
  coord_flip()+
  labs(title = " ", x = "", y = "")
ggplot(tidy_pereyaslav%>%filter(sex=="f", !is.na(age)))+
  coord_flip()+
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

#the age of the youngest worker
tidy_pereyaslav %>%
  filter(str_detect(status, "rabot")) %>%
  summarize(min(age, na.rm = TRUE))

#Workers' income
workers <- tidy_pereyaslav%>%
  filter(str_detect(status, "rab")|status =="hospodar")%>%
  mutate(hosp_ss = if_else(status == "hospodar", social_status, NA),
         hosp_inc = if_else(status == "hospodar", capital, NA),
         hosp_job = if_else(status == "hospodar", job, NA))%>%
  fill(hosp_job)%>%
  fill(hosp_ss)%>%
  fill(hosp_inc)%>%
  filter(status!="hospodar")%>%
  mutate(money = if_else(str_detect(`wage(for_naymyt\`)`, regex("[0-9]")), parse_double(`wage(for_naymyt\`)`, locale = locale(decimal_mark = ",")),NA))%>%
  select(dvor,age, social_status, sex, job_experience, money, `wage`, hosp_ss, hosp_inc, hosp_job)

ggplot(workers)+
  geom_boxplot(aes(x= hosp_ss, y=age))

workers%>%
  mutate(hosp_inc=cut_width(hosp_inc, width =5, boundary = 1))%>%
  group_by(hosp_inc)%>%
  count()%>%
  left_join(tidy_pereyaslav%>%
              filter(status == "hospodar"&!is.na(capital))%>%
              mutate(hosp_inc = cut_width(capital, width =5, boundary = 1))%>%
              group_by(hosp_inc)%>%
              count(),
            join_by(hosp_inc))%>%
  summarize(workers_per_owner = n.x/n.y)%>%
  ggplot()+
  geom_col(aes(hosp_inc, workers_per_owner))

ggplot(workers)+
  geom_point(aes(job_experience,money))

#how landlords of different social ststuses pay
workers%>%
  filter(!is.na(wage))%>%
  group_by(hosp_ss, `wage`)%>%
  count(`wage`)%>%
  left_join(workers%>%
              filter(!is.na(wage))%>%
              group_by(hosp_ss, `wage`)%>%
              count(`wage`)%>%
              group_by(hosp_ss)%>%
              summarize(y = sum(n))
              ,join_by(hosp_ss))%>%
  mutate(ratio = n/y)%>%
  ggplot()+
  geom_col(aes(hosp_ss, ratio*100, fill= wage), position = "dodge")+
  labs(title = "Структура оплати роботи за станом господаря",x="Стан господаря",y="%")+
  theme_minimal()

#how workers get paid  
workers%>%
  filter(social_status=="Підсусідки"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Посполиті")%>%
  filter(!is.na(`wage`))%>%
  group_by(social_status, wage)%>%
  count(wage)%>%
  left_join(workers%>%
              filter(social_status=="Підсусідки"|social_status=="Міщани"|social_status=="Козаки"|social_status=="Посполиті")%>%
              filter(!is.na(`wage`))%>%
              group_by(social_status, wage)%>%
              count(wage)%>%
              group_by(social_status)%>%
              summarise(y = sum(n)),
            join_by(social_status))%>%
  mutate(ratio = n/y)%>%
  ggplot()+
  geom_col(aes(social_status, ratio*100, fill= `wage`), position = "dodge")+
  labs(title = "Структура оплати роботи за станом працівника",x="Стан",y="%")+
  theme_minimal()


#migrants
migrants <- tidy_pereyaslav%>%
  filter(root!="Pereyaslav"&root!="Pereyaslav podvarok Zadolhomostianskij"&root!="Pereyaslava"&!is.na(root))%>%
  filter(!is.na(social_status)&social_status!="unknown")
ggplot(migrants)+
  geom_boxplot(aes(social_status, age))
ggplot(migrants%>%group_by(social_status)%>%count)+
  geom_col(aes(fct_reorder(social_status, n),n))

migrants%>%
  count(social_status)%>%
  left_join(tidy_pereyaslav%>%
              filter(!is.na(root))%>%
              group_by(social_status)%>%
              count(), 
            join_by(social_status))%>%
  mutate(ratio=n.x/n.y)%>%
  select(social_status, ratio)%>%
  ggplot()+
  geom_col(aes(fct_reorder(social_status, ratio), ratio*100))+
  labs(title="Частка людей, які не походять з Переяслава",x ="Стан",y="%",
       caption = "серед тих, у кого вказано походження")


tidy_pereyaslav%>%
  filter(social_status=="Посполиті")%>%
  mutate(worker = (status=="rabotnik"|status=="rabotnitsa"|status=="rabotnica"))%>%
  mutate(is_worker = if_else(worker==TRUE, "Так","Ні"))%>%
  group_by(is_worker)%>%count()%>%
  ggplot()+
  geom_col(aes(is_worker, n))+
  theme(legend.position="none")+
  labs(title ="Чи працюють в наймі посполиті", x = " ", y = "Кількість")

migrants <- migrants%>%
  mutate(marital = fct_collapse(marital, married = c("married", "maried"), unmarried = c("unmarried"), widowed = c("vdov", "vdova")))
migrants%>%
  group_by(social_status, marital)%>%
  count()%>%
  left_join(migrants%>%group_by(social_status)%>%count(), join_by(social_status))%>%
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



