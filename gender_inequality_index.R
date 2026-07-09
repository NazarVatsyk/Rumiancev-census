library(tidyverse)

# male_domination village

tidy_village%>%
  select(HH, hata, ralation, sex, age)%>%
  View()
  group_by(ralation)%>%
  count()%>%
  View()
  
      #CLUSTER1
tidy_village%>% 
  filter(ralation=='hospodar')%>% #знайти тільки господарів
  group_by(sex)%>%
  count(sex == 'f')%>% # порахувати яка кількість з них жінки
  mutate(total = tidy_village%>%filter(ralation=="hospodar")%>%count())%>%  #порахуємо яка сумарна кількість господарів
  filter(sex == 'f')%>% 
  summarise(Female_heads = n/total) #виведемо коефіцієнт
Female_headsV <- 0.0815 

tidy_village%>%
  filter(sex == 'f'&(age>=15)&(age<=19))%>%#Вибереио ТІЛЬКИ жінок віком від 15 до 19
  count(ralation == 'zhena'|ralation == "nevistka") #порахуємо яка кілкість з них одружені  
young_bridesV <- 39/(148+39)
#!! Дисклеймер: якщо рахувати не за колонкою ralation а за колонкою marital status, то виявиться що 3 жінки мають статус дружини ,але на записані як одружені

tidy_village%>%
  mutate(husband_age = lag(age))%>%#Створимо колонку в яку запишемо вік людини з попереднього рядочка у базі
  filter(ralation == 'zhena'| ralation == 'nevistka')%>%# виберемо з бази тільки дружин
  count(age>=husband_age)#порахуємо яка кількість з них старші або ровесники (ми не можемо судити про місяць їхнього народження)
older_wivesV <- 128/810
# тут є два уточнення: 1) використати вік людини з попереднього рядочку дозволяє нам специфіка побудови бази, для інших це можливо не працюватиме
# 2)якщо рахувати жінок які СТРОГО старші, то їхня кількість зменшується до 44
tidy_village%>%
  filter(sex == 'f'& age>=20 &age<=34)%>%#виберемо тільки жінок віком від 20 до 34
  count(ralation == "nonkin")#рахуємо яка кількість з них не рідні
female_nonkinV <- 5/508
     

#CLUSTER 2
tidy_village%>%
  mutate(hospodar_age = if_else(ralation=="hospodar", age, NA))%>%
  fill(hospodar_age)%>%# перші два рядки написані для того щоб у кожному домгосподарстві створити колонку з віком господаря
  # ми можемо так робити через специфіку побудови бази: кожне господарство починається з господаря, тобто нам достатньо спершу зробити колонку
  # в якій ми для господарів продублюємо власний вік, а для інших поставимо NA і далі заповнимо цей стовпець донизу
  filter(sex == "m"& age>=65& hospodar_age<age& ralation!="nonkin"& ralation!="test_teshcha")%>%
  #тепер ми рахуємо кількість чоловіків старших за 65 років, які є членами родини молодшого господаря 
  count() # 3
tidy_village%>%filter(age>=65& sex == "m"& ralation!="nonkin"& ralation!="test_teshcha")%>%count() #рахуємо сумарну кількість чоловіків старших за 65 і рідних до свого господаря
younger_household_headV <- 3/(52)
#!!! в джерелі говориться про різницю в поколіннях, тут варто переглядати безпосередньо результат коду перед підрахунком

tidy_village%>%
  filter(age>19&age<=29&sex =='m'&`marital status`=="married")%>%#знайдемо одружених чоловіків віком від 19 до 29 
  count(ralation == 'hospodar')#порахуємо яка кількість серед них господарів
neolocalV <- 15/(116+15+19)

tidy_village%>%
  group_by(village,HH)%>%#погрупуємо усіх за їхньою "адресою"
  summarise(has_lateral = any(ralation%in% c('zyat', 'nevistka','lateral', 'in_law', 'brat', 'sestra', 'test_teshcha')))%>%
  # подивимося в яких домогосподарствах взагалі є брати, сестри, тещі, дядьк, тощо
  filter(has_lateral == TRUE)%>%#виберемо тільки ті, в яких є
  left_join(tidy_village, join_by(village, HH))%>%#повертаємося до нашої бази, вибравши жителів цікавих нам домогосподарств
  ungroup()%>%# тепер розгрупуємо їх
  filter((age>=65& ralation!='nonkin'))%>%#виберемо тільки господарів та їх далеких родичів родичів, які старші за 65
  count()#порахуємо їх кількість
tidy_village%>%filter(age>=65&ralation!='nonkin')%>%count()#порахуємо кількість рідних до господаря і господарів, які старші за 65
lateralV <- 64/105

tidy_village%>%
  filter(ralation=='syn'&`marital status`=='married')%>%#виберемо тільки одружених синів
  group_by(village,HH)%>%#погрупуємо їх "по адресах"
  count()%>%#порахуємо кількість одружених синів на домогосподарство
  filter(n>=2)%>%#виберемо тільки ті
  left_join(tidy_village, join_by(village,HH))%>%#повернемося до нашої таблиці, обравши тільки членів цікавих нам домогосподарств
  filter(age>=65&(ralation=='hospodar'| ralation== 'zhena'))%>%# з них оберемо тільки мам та батьків 
  ungroup(village,HH)%>%#розгрупуємо їх, щоб порахувати їхю кількість
  count()
tidy_village%>%filter(age>=65&(ralation=="hospodar"| ralation== 'zhena'))%>%count()#порахуємо сумарну кількість господарів та їхніх дружин старших за 65
joint_familyV <- 9/65


#CLUSTER 3
tidy_village%>%
  group_by(village,HH)%>%#погрупуємо по адресах
  filter((ralation=='doch'&`marital status`=='married')|ralation %in% c("test_teshcha", 'zyat'))%>%
  #виберемо тільки ті домогосподарства в яких або є одружена донька, або є зять, або живе тесть чи теща
  count()%>%#знайдемо ці домогосподарства
  left_join(tidy_village, join_by(village,HH))%>%#повернемося до бази, обравши цікаві нам домогосподарства
  filter(age>=65&ralation%in%c('hospodar', 'zhena', 'test_teshcha'))%>%#оберемо тільки старших за 65 батьків жінки
  ungroup(village,HH)%>%
  count()#порахуємо їхню кількість
tidy_village%>%filter(age>=65&ralation%in%c('hospodar', 'zhena', 'test_teshcha'))%>%count()#порахуємо загальну кількість батьків
married_daughterV <- 10/73


#CLUSTER 4
tidy_village%>%
  filter(ralation == "doch"|ralation =="syn")%>%#оберемо тільки синів або доньок 
  group_by(village, HH, hata)%>%#погрупуємо людей за їхніми адресами
  filter(min(age)>=10&min(age)<=14)%>%#оберемо тільки ті домогосподарства в яких наймолодшій дитині від 10 до 14
  arrange(village,HH,hata,age)%>%#посортуємо їх за віком в порядк у спадання
  slice(1)%>%#оберемо наймолодшого
  ungroup()%>%
  count(sex =='m')#порахуємо кількість чоловіків 
boy_as_last_childV <- 13/(13+16)

tidy_village%>%
  filter(age>=0&age<=4)%>%#виберемо дітей віком від 0 до 4
  count(sex=='f')#порахуємо жінок
sex_ratioV <- 302/290 *100

#male_domination pereyaslav
tidy_pereyaslav%>%
  group_by(status)%>%
  count()%>%
  print(n=100)

#CLUSTER 1
tidy_pereyaslav%>%
  filter(ralation=='hospodar')%>%
  group_by(sex)%>%
  count(sex == 'f')%>%
  mutate(total = tidy_pereyaslav%>%filter(ralation=="hospodar")%>%count())%>%
  filter(sex == 'f')%>%
  summarise(Female_heads = n/total)
Female_headsP <- 0.179

tidy_pereyaslav%>%
  filter(sex == 'f'&(age>=15)&(age<=19))%>%
  count(ralation == 'zhena'|ralation == "nevistka")
young_bridesP <- 2/(2+99)

tidy_pereyaslav%>%
  mutate(husband_age = lag(age))%>%
  filter(ralation == 'zhena')%>%
  count(age>=husband_age)
older_wivesP <- 27/(227+27+44)

tidy_pereyaslav%>%
  filter(sex == 'f'& age>=20 &age<=34)%>%
  count(ralation == "nonkin")
female_nonkinP <- 54/(188+54+1)

#CLUSTER 2

#tidy_pereyaslav%>%
#  mutate(hospodar_age = if_else(ralation=="hospodar", age, NA))%>%
#  fill(hospodar_age)%>%
#  filter(sex == "m", age>=65, hospodar_age<age)%>%
#  count(ralation=='hospodar')
#tidy_pereyaslav%>%filter(age>=65, sex == "m")%>%count()
#younger_household_headP <- 4/(23)


tidy_pereyaslav%>%
  mutate(hospodar_age = if_else(ralation=="hospodar", age, NA))%>%
  fill(hospodar_age)%>%
  filter(hospodar_age<age& age>=65)%>%
  filter(sex == "m"& age>=65& hospodar_age<age& ralation!="nonkin"& ralation!="test_teshcha")%>%
  count()
tidy_pereyaslav%>%filter(age>=65& sex == "m"& ralation!="nonkin")%>%count() 
younger_household_headP <- 0/(19)

tidy_pereyaslav%>%filter(age>=65&ralation!='nonkin')%>%count()
tidy_pereyaslav%>%
  group_by(dvor)%>%
  summarise(has_lateral = any(ralation%in% c('zyat', 'nevistka','lateral', 'in_law', 'brat', 'sestra', 'test_teshcha')))%>%
  filter(has_lateral == TRUE)%>%
  left_join(tidy_pereyaslav, join_by(dvor))%>%
  ungroup()%>%
  filter((age>=65& ralation!='nonkin'))%>%
  count()
lateralP <- 12/44

#tidy_pereyaslav%>%
#  filter(ralation=="hospodar"&age<=29)%>%
#  left_join(tidy_pereyaslav%>%
#              filter((ralation=="hospodar"&sex=='m'&age>29&marital=="married")|!(ralation %in% c('hospodar','zhena','syn','doch')))%>%
#              mutate(temp = TRUE)%>%
#              select(dvor, hata,temp),
#            join_by(dvor, hata))%>%
#  filter(is.na(temp))%>%
#  count()
#left_join(tidy_pereyaslav, join_by(dvor, hata))%>%
#  select(dvor, hata, ralation.y,age.y)%>%View()
tidy_pereyaslav%>%
  filter(sex == 'm'&age>=20&age<=29&marital=="married")%>%
  count(ralation=='hospodar')
neolocalP <-25/41

tidy_pereyaslav%>%
  filter(ralation=='syn'&`marital`=='married')%>%
  group_by(dvor)%>%
  count()%>%
  filter(n>=2)%>%
  left_join(tidy_pereyaslav, join_by(dvor))%>%
  filter(age>=65)%>%
  ungroup(dvor)%>%
  count()
tidy_pereyaslav%>%filter(age>=65&(ralation=="hospodar"| ralation== 'zhena'))%>%count()
joint_familyP <- 1/32


#CLUSTER 3
tidy_pereyaslav%>%
  filter((ralation=='doch'&`marital`=='married')|ralation ==  "test_teshcha")%>%
  group_by(dvor)%>%
  count()%>%
  left_join(tidy_pereyaslav, join_by(dvor))%>%
  filter(age>=65)%>%
  ungroup(dvor)%>%
  count()
tidy_pereyaslav%>%filter(age>=65&ralation%in%c('hospodar', 'zhena', 'test_teshcha'))%>%count()
married_daughterP<- 4/35


#CLUSTER 4
tidy_pereyaslav%>%
  group_by(dvor, hata)%>%
  filter(ralation == "doch"|ralation =="syn")%>%
  filter(min(age)>=10&min(age)<=14)%>%
  arrange(dvor,hata,age)%>%
  slice(1)%>%
  ungroup()%>%
  count(sex =='m')
boy_as_last_childP <- 19/(25+19)

tidy_pereyaslav%>%
  filter(age<=4)%>%
  count(sex=='m')
sex_ratioP <- 77/82*100


#male_domination starodub
tidy_starodub%>%
  group_by(ralation)%>%
  count()%>%
  print(n=114)

#CLUSTER 1
tidy_starodub%>%
  filter(ralation=='hospodar')%>%
  group_by(sex)%>%
  count(sex == 'f')%>%
  mutate(total = tidy_starodub%>%filter(ralation=="hospodar")%>%count())%>%
  filter(sex == 'f')%>%
  summarise(Female_heads = n/total)
Female_headsS <- 0.169

tidy_starodub%>%
  filter(sex == 'f'&(age>=15)&(age<=19))%>%
  count(ralation == 'zhena'|ralation == "nevistka")
young_bridesS <- 26/(26+243)

tidy_starodub%>%
  mutate(husband_age = lag(age))%>%
  filter(ralation == 'zhena')%>%
  count(age>=husband_age)
older_wivesS <- 119/(629+119+2)

tidy_starodub%>%
  filter(sex == 'f'& age>=20 &age<=34)%>%
  count(ralation == "nonkin")
female_nonkinS<- 83/(468+83+2)

#CLUSTER 2
tidy_starodub%>%
  mutate(hospodar_age = if_else(ralation=="hospodar", age, NA))%>%
  fill(hospodar_age)%>%
  filter(sex == "m", age>=65, hospodar_age<age)%>%
  count(ralation!='hospodar'&ralation!="nokin")
tidy_starodub%>%filter(age>=65, sex == "m")%>%count()
younger_household_headS <- 18/(66)
tidy_starodub%>%
  mutate(hospodar_age = if_else(ralation=="hospodar", age, NA))%>%
  fill(hospodar_age)%>%
  filter(hospodar_age<age& age>=65)%>%
  filter(sex == "m"& age>=65& hospodar_age<age& ralation!="nonkin"& ralation!="test_teshcha")%>%
  count()
tidy_starodub%>%filter(age>=65& sex == "m"& ralation!="nonkin")%>%count() 
younger_household_headS <- 3/(55)


tidy_starodub%>%filter(age>=65&ralation!='nonkin')%>%count()
tidy_starodub%>%
  group_by(HH)%>%
  summarise(has_lateral = any(ralation%in% c('zyat', 'nevistka','lateral', 'in_law', 'brat', 'sestra', 'test_teshcha')))%>%
  filter(has_lateral == TRUE)%>%
  left_join(tidy_starodub, join_by(HH))%>%
  ungroup()%>%
  filter((age>=65& ralation!='nonkin'))%>%
  count()
lateralS <- 52/121

#tidy_starodub%>%
#  filter(ralation=="hospodar"&age<=29)%>%
#  left_join(tidy_starodub%>%
#              filter((ralation=="hospodar"&sex=='m'&age>29&marital=="married")|!(ralation %in% c('hospodar','zhena','syn','doch')))%>%
#              mutate(temp = TRUE)%>%
#              select(HH, Hata,temp),
#            join_by(HH, Hata))%>%
#  filter(is.na(temp))%>%
#  count()
#  left_join(tidy_starodub, join_by(HH, Hata))%>%
#  select(HH, Hata, ralation.y,age.y)%>%View()
#tidy_starodub%>%filter(sex == 'm'&age>=20&age<=29&marital=="married")%>%count()
#neolocalS <-17/123
tidy_starodub%>%
  filter(sex == 'm'&age>=20&age<=29&marital=="married")%>%
  count(ralation=='hospodar')
neolocalS <-66/(66+57)
tidy_starodub%>%
  filter(ralation=='syn'&`marital`=='married')%>%
  group_by(HH)%>%
  count()%>%
  filter(n>=2)%>%
  left_join(tidy_starodub, join_by(HH))%>%
  filter(age>=65)%>%
  ungroup(HH)%>%
  count()
tidy_starodub%>%count(age>=65&(ralation=="hospodar"| ralation== 'zhena'))
joint_familyS <- 1/74


#CLUSTER 3
tidy_starodub%>%
  filter((ralation=='doch'&`marital`=='married')|ralation ==  "test_teshcha")%>%
  group_by(HH)%>%
  count()%>%
  left_join(tidy_starodub, join_by(HH))%>%
  filter(age>=65)%>%
  ungroup(HH)%>%
  count()
tidy_starodub%>%count(age>=65&(ralation=="hospodar"| ralation== 'zhena'|ralation == 'test_teshcha'))
married_daughterS<- 23/87


#CLUSTER 4
tidy_starodub%>%
  group_by(HH, Hata)%>%
  filter(ralation == "doch"|ralation =="syn")%>%
  filter(min(age)>=10&min(age)<=14)%>%
  arrange(HH,Hata,age)%>%
  slice(1)%>%
  ungroup()%>%
  count(sex =='m')
boy_as_last_childP <- 28/(28+34)

tidy_starodub%>%
  filter(age<=4)%>%
  count(sex=='m')
sex_ratioS <- 186/211*100




### PATRIARCHY INDEX (Europe)
VILAGE
(floor(10-10*Female_headsV/0.24)+
    floor(10*young_bridesV/0.66)+
    floor(10-10*older_wivesV/0.37)+
    floor(10-10*female_nonkinV/0.41))/4+
  (floor(10-10*neolocalV/1)+floor(10*lateralV/0.71)+floor(10*joint_familyV/0.34))/3+
  floor(10-10*married_daughterV/0.80)+(floor(10*boy_as_last_childV/0.81)+floor(10*sex_ratioV/137.33))/2

PEREYASLAV
(floor(10-10*Female_headsP/0.24)+
    floor(10*young_bridesP/0.66)+
    floor(10-10*older_wivesP/0.37)+
    floor(10-10*female_nonkinP/0.41))/4+
  (floor(10-10*neolocalP/1)+floor(10*lateralP/0.71)+floor(10*joint_familyP/0.34))/3+
  floor(10-10*married_daughterP/0.80)+(floor(10*boy_as_last_childP/0.81)+floor(10*sex_ratioP/137.33))/2

STARODUB
(floor(10-10*Female_headsS/0.24)+
    floor(10*young_bridesS/0.66)+
    floor(10-10*older_wivesS/0.37)+
    floor(10-10*female_nonkinS/0.41))/4+
  (floor(10-10*neolocalS/1)+floor(10*lateralS/0.71)+floor(10*joint_familyS/0.34))/3+
  floor(10-10*married_daughterS/0.80)+(floor(10*boy_as_last_childS/0.81)+floor(10*sex_ratioS/137.33))/2

## Patriarchy index in relation to each other
VILAGE



  (round(10-10*Female_headsV/0.24)+
    round(10*young_bridesV/0.66)+
    round(10-10*older_wivesV/0.37)+
    round(10-10*female_nonkinV/0.41))/4+
  (round(10-10*neolocalV/1)+
     round(10*lateralV/0.71)+
     round(10*joint_familyV/0.34)+
     round(10-10*younger_household_headV/0.68))/4+
  round(10-10*married_daughterV/0.80)+
  (round(10*boy_as_last_childV/0.81)+
     round(10*sex_ratioV/137.33))/2


(10-round(10*Female_headsP/0.24)+
    round(10*young_bridesP/0.66)+
    10-round(10*older_wivesP/0.37)+
    10-round(10*female_nonkinP/0.41))/4+
  (10-round(10*neolocalP/1)+
     round(10*lateralP/0.71)+
     round(10*joint_familyP/0.34)+
     10-round(10*younger_household_headP/0.68))/4+
  10-round(10*married_daughterP/0.80)+
  (round(10*boy_as_last_childP/0.81)+
     round(10*sex_ratioP/137.33))/2

(round(10-10*Female_headsS/0.24)+
    round(10*young_bridesS/0.66)+
    round(10-10*older_wivesS/0.37)+
    round(10-10*female_nonkinS/0.41))/4+
  (round(10-10*neolocalS/1)+
     round(10*lateralS/0.71)+
     round(10*joint_familyS/0.34)+
     round(10-10*younger_household_headS/0.68))/4+
  round(10-10*married_daughterS/0.80)+
  (round(10*boy_as_last_childS/0.81)+
     round(10*sex_ratioS/137.33))/2







(floor(10-10*Female_headsV/Female_headsP)+
    floor(10*young_bridesV/young_bridesV)+
    floor(10-10*older_wivesV/older_wivesS)+
    floor(10-10*female_nonkinV/female_nonkinP))/4+
  (floor(10-10*neolocalV/neolocalP)+
     floor(10*lateralV/lateralV)+
     floor(10*joint_familyV/joint_familyV))/3+
  floor(10-10*married_daughterV/married_daughterS)+
  (floor(10*boy_as_last_childV/boy_as_last_childV)+
     floor(10*sex_ratioV/sex_ratioV))/2


(floor(10-10*Female_headsS/Female_headsP)+
    floor(10*young_bridesS/young_bridesV)+
    floor(10-10*older_wivesS/older_wivesS)+
    floor(10-10*female_nonkinS/female_nonkinP))/4+
  (floor(10-10*neolocalS/neolocalP)+
     floor(10*lateralS/lateralV)+
     floor(10*joint_familyS/joint_familyV))/3+
  floor(10-10*married_daughterS/married_daughterS)+
  (floor(10*boy_as_last_childS/boy_as_last_childV)+
     floor(10*sex_ratioS/sex_ratioV))/2

(floor(10-10*Female_headsP/Female_headsP)+
    floor(10*young_bridesP/young_bridesV)+
    floor(10-10*older_wivesP/older_wivesS)+
    floor(10-10*female_nonkinP/female_nonkinP))/4+
  (floor(10-10*neolocalP/neolocalP)+
     floor(10*lateralP/lateralV)+
     floor(10*joint_familyP/joint_familyV))/3+
  floor(10-10*married_daughterP/married_daughterS)+
  (floor(10*boy_as_last_childP/boy_as_last_childV)+
     floor(10*sex_ratioP/sex_ratioV))/2
#cleaning pereyaslav data
tidy_pereyaslav <- tidy_pereyaslav %>%
  mutate(status = as_factor(str_trim(status))) %>%
  mutate(ralation = fct_collapse(status,
                                       hospodar = c("hospodar", "rabotnik / hospodar"),
                                       
                                       syn = c("syn", "syn / brat", "syn / vnuk",
                                               "syn yiyi nezakonnonarodzhenyj"),
                                       
                                       doch = c("doch'", "doch' / sestra", "doch' / vnuchka"),
                                       
                                       zhena = c("zhena", "zena", "zhena syna", "muzh"),
                                       
                                       mat = c("mat'", "mater hospodara", "svekrukha"),
                                       
                                       brat = c("brat", "brat / rabotnik"),
                                       
                                       sestra = c("sestra", "sestra jej"),
                                       
                                       vnuk = c("onuk", "onuka", "onuka / doch'", "vnuka"),
                                       
                                       pasynok = c(),
                                       
                                       lateral = c("plemiannica", "plemiannik", "plemyannitsa",
                                                   "titka zheny",
                                                   "svojstvenny`e ikh deti umershego burmistra Mikhajla Timofeeva"),
                                       
                                       test_teshcha = c("teshcha", "tescha"),
                                       
                                       in_law = c("ziat'", "shurin", "shvager", "shvaher",
                                                  "svoyachenitsa", "svoyak", "svojashnica",
                                                  "svojstvenna", "svojstvennaja", "svojstvennyj"),
                                       
                                       nonkin = c("rabotnik", "rabotnica", "rabotnitsa", "rabotnsk",
                                                  "podsusedka", "podsusedok", "pidsusidok / non kin",
                                                  "podsusedka / non kin", "podsusedok / non kin",
                                                  "non kin", "non kin (podsusedok)",
                                                  "hospodar / podsusedok", "sedelec",
                                                  "nishchaja", "nishchij",
                                                  "shkol'nik", "poslushnik",
                                                  "sluzhytel'", "sluzhytel'ka",
                                                  "sirota", "staritca")
  ))

# cleaning village data
tidy_village <- tidy_village%>%
  fill(village)
tidy_village <- tidy_village %>%
  mutate(ralation = str_trim(ralation) %>% str_remove("^\\d+\\s+"))
tidy_village <- tidy_village %>%
  mutate(ralation = as_factor(str_trim(ralation)))%>%
  mutate(ralation = fct_collapse(ralation,
                                       hospodar = c("hospodar"),
                                       
                                       syn = c("syn", "son", "1 syn", "2 syn", "3 syn", "4 syn", "5 syn", "6 syn",
                                               "Agafiya's syn", "Aleksey's syn", "Anna's syn", "Darya's syn",
                                               "Dmytriyev syn", "Evdokiya's syn", "Ivan's syn", "Kalenik Yarmak's syn",
                                               "Matvey's syn", "Opanas's syn", "Tatyana's syn", "Vasiliy's syn",
                                               "Yakov's syn", "syn Fedora ot pervoi zheni", "syn Maksyma", "syn Marka",
                                               "syn Matrona", "syn Parfena", "syn ot pervoy zhena", "pervoy zheny syn",
                                               "vtoroy zheny syn", "umershego shvagera syn",
                                               "Fedor Butenko's umershego dyadi syn"),
                                       
                                       doch = c("doch", "doch'", "doch`", "docn", "dich`", "2 doch'", "3 doch'",
                                                "4 doch'", "5 doch'", "6 doch'", "7 doch'", "8 doch'", "9 doch'",
                                                "10 doch'", "Anna's doch'", "Feska's doch'", "Mariya's doch'",
                                                "their doch'", "her doch'", "Dmytriyeva doch", "vtoroy zheny doch'",
                                                "umershego shvagera doch'"),
                                       
                                       zhena = c("zhena", "zhenz", "jena Stepana", "Mikhaylo's zhena",
                                                 "zhena umershego brata", "bratanicha zhena",
                                                 "Stepan's zhena, pidsusidok", "umershego syna Matveya zhena"),
                                       
                                       mat = c("mat'", "mat`", "matka", "maty", "macheha", "machuha",
                                               "Efim's, Miron's and Dmitriy's mat'", "Fedor's mat'", "Gritsko's mat'",
                                               "Mikhaylo's mat'", "Ohrim's mat'", "Paraskeviya's mat'", "Stepan's mat'",
                                               "Ulas's and Savva's mat'", "Vasiliy's mat'",
                                               "Vasiliy's and Paraskeviya's vtoraya mat'", "Yakov's mat'",
                                               "mat' Babyntsov", "mat' Demyanenka"),
                                       
                                       otets = c("Iosif's otets", "Stepan's otets"),
                                       
                                       brat = c("brat", "brat Artema", "brat Denysa", "brat Efima", "brat Fedora",
                                                "brat Gerasyma", "brat Grigoriya", "brat Ivana", "brat Marka",
                                                "brat Ostapa", "brat Parfena", "brat Romana", "brat Rudenka",
                                                "brat Trofyma", "brat Yakova", "brat Zenoviya", "brat rodnoy ih ottsa",
                                                "brat zheny", "Aleksey's brat", "Aleksey`s brat", "Andrey's brat",
                                                "Fedor's brat", "Ivan's brat", "Ivan's and Semen's brat",
                                                "Kalenik Yarmak's brat", "Kindrat's brat", "Lukyan's brat",
                                                "Matvey's brat", "Mikhaylo's brat", "Opanas's brat", "Petro's brat",
                                                "Prohor's brat", "Radion's brat", "Savva's brat", "Semen`s brat",
                                                "Stepan's brat", "umershego muzha brat"),
                                       
                                       sestra = c("sestra", "sestra Ivana", "sestra zheny", "sestr zheny", "sister",
                                                  "Fedor Butenko's sestre", "Anna's sestra", "Fedor's sestra",
                                                  "Ivan's sestra", "Mariya`s sestra", "Mikhaylo's sestra",
                                                  "Nestor's sestra", "Nikita's sestra", "Radion's and Nikita's sestra",
                                                  "Pavel Lihovid's sestra", "Semen`s sestra"),
                                       
                                       vnuk = c("vnuchka", "vnuka", "Feodosiya's vnuk"),
                                       
                                       pasynok = c("pasynok", "pasynok vtoroi zheni", "pasynok zheny",
                                                   "padcheritsa", "Nestor's pasynok"),
                                       
                                       lateral = c("dyadya", "dyadya Demyanenka", "Demyda dyadina",
                                                   "Tihon's and Yakov's dyadya", "Semen's dyadya",
                                                   "Stepa's, Vasiliy's and Petro's dyadena vdova",
                                                   "Iosif's t'otka", "Kirilo's t'otka",
                                                   "plemyannik", "plemyannitsa", "plemyannik, syrota",
                                                   "Ivan Shedrik's plemyannik", "Ivan's and Semen'splemyannik",
                                                   "Fedor's plemyannik", "Salyvonyha's plemyannik",
                                                   "sestrinets", "Andrey's sestrinets", "Fedor's sestrinets",
                                                   "dvoyurodnyy brat", "dvoyuridny brat", "dvoyuridnuy brat",
                                                   "Andrey's dvoyurodnyy brat",
                                                   "Efim's, Miron's and Dmitriy's dvoyurodnyy brat",
                                                   "Kirilo's dvoyurodnyy brat", "Nestor's dvoyurodnyy brat",
                                                   "Parfen's dvoyurodnyy brat", "Tihon's and Yakov's dvoyurodnyy brat",
                                                   "bratanich", "bratanych", "bratanych Borovikov",
                                                   "bratanych Fastovtsa", "bratanych Maksyma", "bratanych Omelyana",
                                                   "Grigoriy's bratanich", "bratova", "bratovaya", "bratovaya Maksyma",
                                                   "Iosif's rodimets", "umershego muzha rodimka"),
                                       
                                       test_teshcha = c("teshcha", "Stepan's teshcha", "test'", "test`",
                                                        "test', pidsusidok"),
                                       nevistka = c("nevestka", "Ivan's nevestka", "Mikhaylo's nevestka"),
                                       zyat = c("zyat'", "zyat", "zyat`", "Grigoriy Kubrak's zyat'",
                                                "Gritsko's zyat'", "Nikita Sviridenko's zyat'",
                                                "Semen's and Mikhaylo's zyat'", "Yosyp`s zyat`", "zyat' Kyryla"),
                                       
                                       in_law = c("shurin", "schvager", "shvager", "shvaher", "zhvaher",
                                                  "svoyak", "svoyak Yakova", "Grigoriy's svoyak", "svoyachka",
                                                  "svoystvenna", "svoystvennaya Ivana", "Yosyp`s svoystvenny",
                                                  "Filip's svest'", "rodstvennitsa"),
                                       
                                       nonkin = c("rabotnik", "rabotnitsa", "rabornik", "pabotnitsa",
                                                  "rabotnik Yakova", "Dmitriy's rabotnik", "Manoylo's rabotnik",
                                                  "Opanas's rabotnik", "Pavel's rabotnik", "Radion's rabotnik",
                                                  "pidsusidok", "pidsusidok Fastovtsa", "pidsusidok Filona",
                                                  "pidsusidok Katerini", "pidsusidok Yakova", "podsusedka",
                                                  "podsusedok", "podsusidok", "Ivan Barabash's pidsusidok",
                                                  "Ivan's padsusidok", "Fedor's pidsusidok",
                                                  "Manoylo Barabash's pidsusidok", "Mikhaylo's pidsusidok",
                                                  "Yakim Barabash's pidsusidok", "Yakim's pidsusidok",
                                                  "syrota", "uchenik", "dlya obucheniya shevskoho mastersnva",
                                                  "podmaster'e", "v toy zhe izbe", "hodovanka", "psalomshchyk"),
                                       
                                       other = c("Vassa`s", "zhe", "sned'", "?")
  ))

tidy_starodub <- tidy_starodub %>%
  mutate(ralation = as_factor(str_trim(ralation))) %>%
  mutate(ralation = fct_collapse(ralation,
                                       hospodar = c("hospodar", "mat' / hospodar", "zhena / hospodar",
                                                    "starosta"),
                                       
                                       syn = c("syn", "syn ot pervoj zheny", "syn s pervoj zhenoj prizhytyj",
                                               "syn z pervym muzhem prizhytyj",
                                               "vid poperedn'oho cholovika (pospolytoho) syn",
                                               "onuk / syn"),
                                       
                                       doch = c("doch'", "onuka / doch'",
                                                "vid poperedn'oho cholovika (pospolytoho) doch'",
                                                "z pervym muzhem prizhytaja doch'"),
                                       
                                       zhena = c("zhena", "zhena / mat' Maksima", "cholovik", "muzh",
                                                 "muzh jej vtorobrachnyj", "molodyk"),
                                       
                                       mat = c("mat'", "baba", "babka", "svekrov'", "svekrukha", "otchim"),
                                       
                                       otets = c("otec", "bat'ko"),
                                       
                                       brat = c("brat", "brat dvojurodnyj"),
                                       
                                       sestra = c("sestra", "sestra dvojurodnaja", "sestra muzha",
                                                  "sestra z'atia"),
                                       
                                       vnuk = c("onuk", "onuka", "jej onuka", "vnuk"),
                                       
                                       pasynok = c("pasynok", "pasynok jej", "padcherica",
                                                   "vziataja na vospitanie", "vziataja vmesto docheri",
                                                   "vziat na propitanie", "vziata na propitanie",
                                                   "vziataja vmesto docheri"),
                                       
                                       lateral = c("bratova", "titka", "tetka zheny",
                                                   "bat'ko teshchi", "diad'ko",
                                                   "plemiannica", "plemiannik", "pleminnycia", "pleminnyk",
                                                   "rodstvenica", "rodstvennica", "rodstvennik", "dvojurodnaja sestra"),
                                       
                                       test_teshcha = c("teshcha", "test'"),
                                       
                                       in_law = c("z'at'", "ziat'", "nevestka", "nevistka",
                                                  "shurin", "shvaher", "shvahr",
                                                  "svoystvennaia", "svojstvenna", "svojstvenna hospodaria dvoru",
                                                  "svojstvenny", "svojstvennyj", "svojstennyj",
                                                  "svojachka", "svojak"),
                                       
                                       nonkin = c("rabotnik", "rabotnica", "rabotnitsa", "rabotnia", "rabota",
                                                  "rabotnik (kucher)", "rabotnik (povar)", "rabotnik (vinokur)",
                                                  "rabotnica (kormilica)", "rabotnica kormilia",
                                                  "rabotnica baba dlia obuchenija detej hramote",
                                                  "podsusedok", "dvorovyj", "sosed",
                                                  "non kin", "v sosediakh / non kin",
                                                  "nishchaja", "nishchyj",
                                                  "nishchaja zhyvet khrista radi",
                                                  "s propitaniia zhyvet", "s propitanija zhyvet",
                                                  "na propitanii", "bez najmu",
                                                  "sirota", "sluzhanka", "sluzhytel'", "sluzhytel'ka",
                                                  "shkol'nik", "shkolnik", "uchenik", "uchenik shurina",
                                                  "podmasterije", "diachok", "diak", "ponomar", "psalomshchik",
                                                  "piddiachyj", "poddiachyj", "starec","bez najmu")
  ))%>%
  fill(HH)%>%fill(Hata)
