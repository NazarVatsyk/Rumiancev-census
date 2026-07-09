library(tidyverse)
starodub <- read_csv("C:\\Users\\ceoet\\Downloads\\Rumiancev data_Starodub - Лист1.csv")
tidy_starodub <- starodub%>%
  select(`HH`:`Час учнівства`)%>%
    mutate(status_category = fct_collapse(as.factor(`soc. status`),
                                          kozaky = c(
                                            "k",
                                            "kozacha zhena",
                                            "kozachij podsusedok",
                                            "kozachij syn",
                                            "kozachyj pidsusidok",
                                            "kozachyj podsusedok",
                                            "kozachyj syn",
                                            "kozachyj syn-pid",
                                            "kozackaja zhena",
                                            "zhena kozachaja",
                                            "zhena karabinerskaja", 
                                            "sotennoho osaula syn",
                                            "sotennoho pysaria syn",
                                            "sotennoho pysaria vdova",
                                            "znachkovoho tovarysha doch'",
                                            "znachkovoho tovarysha syn",
                                            "polkovoj ciriul'nik, byvshyj kozak"
                                          ),
                                          
                                          pospoliti = c(
                                            "p",
                                            "muzhychaja zhena",
                                            "muzhychij syn",
                                            "muzhychka",
                                            "muzhychoho zvanija",
                                            "muzhychyj syn",
                                            "muzhyckaja zhena",
                                            "muzhyckij syn",
                                            "muzhyckoj syn",
                                            "muzhycyj syn",
                                            "muzhechaja dochka",
                                            "syn muzhyckij",
                                            "zvanija muzhyckoho",
                                            "poddancheskij syn",
                                            "poddanicheskij syn",
                                            "poddanyj",
                                            "doch' poddanicheskaja",
                                            "vladenija hrafa Kirila Hrihorievicha Razumovskoho",
                                            "vladenija umersheho podskarbija heneral'noho Vasylia Hudovicha"
                                          ),
                                          mishchany = c(
                                            "m",
                                            "m, baba",
                                            "syn muzykanta, m",
                                            "kupec",
                                            "kupec'",
                                            "kupeckij syn",
                                            "kupiec (hrek)",
                                            "hrek",  "honcharova zhena"
                                          ),
                                         pidsusidky = c(
                                            "pid",
                                            "k-pid",
                                            "m-pid",
                                            "p-pid",
                                            "poddanyj-pid",
                                            "pid zvanija raznochinskoho",
                                            "kozachyj pidsusidok",   # дублюється — можна залишити тут або в козаках
                                            "kozachij podsusedok",
                                            "dochka pidsusidka",
                                            "zhivet v sosediakh",
                                            "pid-diakovskij syn"
                                          ),
                                          inshi = c(
                                            "krepostnaja",
                                            "krepostnaja velikorossijanka",
                                            "kripachka",
                                            "kripak",
                                            "byvshyj pushkar",
                                            "diakonskoj syn",
                                            "doch' diaka",
                                            "doch' kanceliarista",
                                            "doch' shliakhetskaja",
                                            "doch' soldatskaja",
                                            "dochka harmasha",
                                            "dovbysh artilerii polkovoj",
                                            "dyjakons'kyj syn",
                                            "husarskaja zhena",
                                            "inozemka",
                                            "kanceliarist",
                                            "kanceliarist polkovoj",
                                            "karabinerka",
                                            "karabinerskaja zhena",
                                            "m (ranishe buv pushkarskoho, do obmezhennia kil'kosti pushkariv)",
                                            "otstavnoj husar",
                                            "otstavnoj husar, naciji i very hrecheskoj",
                                            "otstavnoj husarskoho polku kapral venherskoj nacji",
                                            "otstavnoj polkovoj ciriul'nik",
                                            "pol'skoj nacyji",
                                            "polkovoj artilerii pushkar",
                                            "popadia",
                                            "popovich",
                                            "popovskaja doch'",
                                            "popovskij syn",
                                            "popovskoho zvanija",
                                            "porody evrejskoj",
                                            "prirody prusskoj",
                                            "pushkar",
                                            "pushkar artilerii polkovoj",
                                            "pushkarka",
                                            "pushkarskaja doch'",
                                            "Pushkarskaja zhena",
                                            "pushkarskij syn",
                                            "pushkarskoho zvanija",
                                            "zvanija pushkarskoho",
                                            "raskol'nica",
                                            "raskol'nickaja doch'",
                                            "raskol'nik",
                                            "velikorossijanin, raskol'nik",
                                            "sirota",
                                            "sirota rodstva nepomniashchaja",
                                            "soldat",
                                            "streleckoj syn",
                                            "sviashchennichyj syn",
                                            "syn diachka",
                                            "syn diaka",
                                            "syn kanceliarysta",
                                            "syn ponomarskij",
                                            "syn popovskij",
                                            "syn sviashchenika",
                                            "syn sviashchennika",
                                            "vdova kanceliarista polkovoho",
                                            "vdova pushkaria",
                                            "vdova vojskovoho kanceliarista",
                                            "Venherskoho husarskoho polku praporshchica",
                                            "venherskoho husarskoho polku vdova",
                                            "zvanija raznochinskoho",
                                            "zvanija shliakhetskoho",
                                            "doch' shliakhetskaja",
                                            "sotennoho pysaria vdova",
                                            "syn muzhyckij"
                                          )
    ),
    age = parse_double(age, locale = locale(decimal_mark = ',')),
    marital = fct_collapse(`marital status`, married =c("married", "zhenka"), unmarried = c("syrota","unmarried"), widiwed = c("vdov", "vdova")))

ggplot(tidy_starodub%>%filter(sex=="m", !is.na(age)))+
  geom_bar(aes(cut_width(age, 5,boundary = 0)), fill = "blue")+
  coord_flip()+
  labs(title = " ", x = "", y = "")
ggplot(tidy_starodub%>%filter(sex=="f", !is.na(age)))+
  geom_bar(aes(cut_width(age, 5,boundary = 0)), fill = "pink")+
  coord_flip()+
  labs(title = "Статево-вікова піраміда Стародуба", x = "", y = "")

ggplot(tidy_starodub%>%filter(is.na(status_category)))+
  geom_freqpoly(aes(age))

#total status
tidy_starodub%>%
  group_by(status_category)%>%
  count()%>%
  mutate(quantity = n)%>%
  ggplot()+
    geom_col(aes(fct_reorder(status_category,quantity), quantity))
  
#average age for each status
tidy_starodub%>%
  group_by(status_category)%>%
  summarise(avg_age = mean(age, na.rm=TRUE))%>%
  ggplot()+
  geom_col(aes(fct_reorder(status_category, avg_age), avg_age))+
  ylim(c(0,30))
tidy_starodub%>%
  ggplot()+
  geom_boxplot(aes(fct_reorder(status_category, age, .fun = median),age))

#total hospodari
tidy_starodub%>%
  filter(str_detect(ralation, 'hosp'))%>%
  ggplot()+
  geom_bar(aes(status_category))

tidy_starodub <- tidy_starodub%>%
  mutate(status_category = if_else(is.na(status_category), 'missing', status_category))%>%
  mutate(hosp_status=if_else(str_detect(ralation, 'hosp'), status_category, 'missing'))%>%
  fill(hosp_status)%>%
  mutate(status_category = if_else(status_category=='missing' & str_detect(ralation, 'doch') & str_detect(ralation, "syn"), 
                                    hosp_status, status_category))%>%
  mutate(status_category = if_else(is.na(status_category), 'missing', status_category))
  
ggplot(tidy_starodub)+
    geom_bar(aes(status_category))

#RABOTNIKI
tidy_starodub%>%
  select(status_category,age,sex,ralation)%>%
  filter(str_detect(ralation,'rabot'))%>%
  ggplot()+
  geom_bar(aes(x=status_category))
#  ggplot()+
#  geom_boxplot(aes(x=status_category, y= age))

#aliens
tidy_starodub%>%
  filter(!is.na(`Походження`))%>%
  mutate(is_local = if_else(str_detect(`Походження`,regex('^starodub$', ignore_case = TRUE)),TRUE, FALSE))%>%
  filter(is_local==FALSE)%>%
  ggplot()+
  geom_boxplot(aes(status_category, age))
tidy_starodub%>%
  filter(!is.na(`Походження`))%>%
  mutate(is_local = if_else(str_detect(`Походження`,regex('^starodub$', ignore_case = TRUE)),TRUE, FALSE))%>%
  group_by(status_category)%>%
  count(is_local==TRUE)%>%
  filter(`is_local == TRUE`==FALSE)%>%
  left_join((tidy_starodub%>%group_by(status_category)%>%count()), join_by(status_category))%>%
  group_by(status_category)%>%
  summarize(ratio_of_aliens = n.x/n.y*100)%>%
  ggplot()+
  geom_col(aes(fct_reorder(status_category, ratio_of_aliens), ratio_of_aliens))
tidy_starodub%>%  
  filter(!is.na(`Походження`))%>%
  mutate(is_local = if_else(str_detect(`Походження`,regex('^starodub$', ignore_case = TRUE)),TRUE, FALSE))%>%
  filter(is_local==FALSE & !is.na(`Платня / наймит`))%>%
  group_by(status_category)%>%
  count()%>%
  left_join((tidy_starodub%>%
               group_by(status_category)%>%
               filter(!str_detect(`Походження`, regex('^starodub$', ignore_case = TRUE)))%>%
               count()), 
            join_by(status_category))%>%
  group_by(status_category)%>%
  summarize(ratio_of_workers = n.x/n.y*100)


tidy_starodub%>%
  filter(age>quantile((tidy_starodub%>%filter(marital =="married"))$age, probs = 0.01, na.rm = TRUE))%>%
  mutate(is_local = if_else(str_detect(`Походження`,regex('^starodub$', ignore_case = TRUE))|is.na(`Походження`),TRUE, FALSE))%>%
  group_by(status_category, is_local, marital)%>%
  count()%>%
  left_join(tidy_starodub%>%
              filter(age>quantile((tidy_starodub%>%filter(marital =="married"))$age, probs = 0.01, na.rm = TRUE))%>%
              mutate(is_local = if_else(str_detect(`Походження`,regex('^starodub$', ignore_case = TRUE))|is.na(`Походження`),TRUE, FALSE))%>%
              group_by(status_category,is_local)%>%
              count(),
            join_by(status_category, is_local))%>%
  group_by(status_category, is_local, marital)%>%
  summarize(ratio = n.x/n.y)%>%
  filter(status_category=="pospoliti"|status_category=="mishchany"|status_category=="kozaky"|status_category=="pidsusidky")%>%
  ggplot()+
  geom_col(aes(marital, ratio, fill = is_local), position = "dodge")+
  facet_wrap(vars(status_category))

tidy_starodub%>%
  filter(!is.na(`Платня / наймит`))%>%
  mutate(income = parse_double(`Платня / наймит`, locale = locale(decimal_mark = ',')))%>%
  ggplot()+
  geom_bar(aes(status_category))
  geom_boxplot(aes(status_category, income))
  
tidy_starodub%>%
  filter(ralation == 'nonkin')%>%
  mutate(is_local = if_else(str_detect(`Походження`,regex('^starodub$', ignore_case = TRUE))|is.na(`Походження`),TRUE, FALSE))%>%
  ggplot()+
  geom_histogram(aes(x = sex), stat = "count")+
  facet_wrap(~is_local)

tidy_starodub%>%
  mutate(capital = `Капітал / госп.`, job = `Спосіб заробітку`)%>%
  filter(!is.na(capital)&!is.na(job))%>%
  select(capital, job)%>%
  View()
tidy_starodub%>%
  mutate(wage = `Платня / наймит`,experience = `Час наймитування`)%>%
  filter(!is.na(experience))%>%
  select(wage, experience)
