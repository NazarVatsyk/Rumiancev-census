#Used chatgpt to clean data
tidy_pereyaslav <- X_Rumiancev_census_Pereyaslav%>%
    fill(dvor)%>%
    fill(hata)%>%
    mutate(age = parse_number(age))%>%
    mutate(root = as.factor(root), status = as.factor(status), social_status = as.factor(social_status), health = as.factor(health), job = as.factor(job))%>%
    mutate(social_status = fct_collapse(social_status, 
                                        Підсусідки = c("doch' podsusedka", "k-pid", "m-pid","pid ; z podsusedkov kozachikh","podsusedork, nacji tureckoj", "syn diachka - podsusedok", "syn ponomarskij - pidsusidok", "z kozachjikh podsusedkov", "z podsusedkov","p-pid","pid", "podsusedcheskogo", "S podsusedkov sviashchenika Peschanskogo", "Z podsusedkov sviashchennika"),
                                        Козаки = c("k"),
                                        Посполиті = c("p"),
                                        `Міщани` = c("m"),
                                        Кріпосні = c("krepostnaia","krepostnaja", "krepostnoj", "krest'ianskij syn"),
                                        Інше = c("soldatka","Doch' Soldatskaia" ,"Doch' soldatskaia", "otstavnoy husar", "soldat", "syn kompanejskij", "z perekhozhykh plotnikov", "doch plotnitckaya","doch' shliakhetskaja", "syn shliakhetskij", "znachkovoho tovarysha doch'", "znachkovyj tovarysh", "moskovskoho", "doch' hreckaja", "z evreev","hrek","syn kupieckij","doch' kupeckaja","doch' kupecheskaja", "kupiec", "syn kupeckij", "kupec","p / palamar", "doch' ponomaria", "syn palamarskoj", "doch' diakovskaia", "doch' popovskaja", "k /  diachok", "syn popovskij", "syn sviashchenika", "syn vikarija", "vdova sviashchenika","d", "?", "syn nezakonnyj", "ne vidomo")
    ))%>%
    mutate(health = fct_collapse(health,
                                 Здорові = "health",
                                 Зору = c( "Na pravyj hlaz slepa", "Slepa", "na pravoe oko slepa I na levuju nohu kriva", "na levyj hlaz slep", "kriv na oba glaza", "Na glaz slep", "na hlaza slep","Na levyj hlaz slep", "Na levoe oko slepa", "na levyj hlaz slepa", "na levyj hlaz slepa" , "na oba ochi clepa","Na oba ochi slep","Na oba ochi slepa","na oba ochi slep","na oba ochi slepa","na pravyj hlaz slepa","Na pravyj hlaz slep","pravym okom po prichine bel'ma ne vidit","zdorov, na hlaz oslep"),
                                 Слуху = c("glukhyj","nimyj"),
                                 `Опорно-рухового апарату` = c("Na zdorov'e grudej razbit","Oderzhima lomovoy bolezniiu", "horbat","oderzhym lomotnoju bolezniju","Na levu nogu kriv",  "na levu nohu kriva","Na nogi kriv",  "na pravu nohu kriv", "na pravuju nohu kriv", "Ruki I nogi oderzhymy lomotnoj bolezniiu", "leva noha oderzhyma hostcevoju bolezniju", "Na levu nogu kriva","na obe nohi kriva" ,"Na pravuiu nogu kriv" ,  "na pravuju nohu kriva", "Nohi oderzhymy lomotnoju bolezniju"),
                                 Інші = c("?","paralich I po starosti slaba" ,"Paralich","oderzyma paralichnoj bolezniju","oderzhyma paralezhoju bolezniju","oderzhym paralichnoju bolezniju",
                                          "Odarzhyma chakhotnoj bolezniyu",
                                          "oderjym chernoyu bolezniyu",
                                          "Nosa net",
                                          "Oderzhyma slaboumiem","v ume pomeshatel'na","V ume pomeshatelen","V ume pomeshatel'na",
                                          "vechno oderzhym hostcevoju bolezniju","oderzhym vnutrenneju bolezneju","Vnutrenneiu bolezniu oderzhyma","vnutrenneju bolezniju oderzhym","Vnutrenneyu boleznyu oderjym","vnutrenniaja bolezn'", "oderzhyma vnutrennej bolezniiu", "vnutrenneju bolezneju oderzhyma","vnutrenneju bolezniu oderzhym","Vnutrenneyu boleznyu oderjyma" ,"vnutrenniyu bolezniyu oderzhyma"),
                                 Старість = c("po starosti slab","po starosti slaba","za starostyu slaba","Po starosri slaba" ,"Po starosti slab","Po starosti slaba","Zdorova, po starosti slaba"),
                                 
    ))%>%
    mutate(job = fct_collapse(job,
                              `Роздрібна торгівля` = c("Prodazha vina","Torg raznym kramarskim tovarom","Torguet raznym kramarskim melkim drobjazkom","Torhuet raznym kramarskim","Torhuet raznym kramarskom tovarom","Torhuet raznym kramnym drobiazkom","Torhuet raznym kramskom tovarom","Toth raznym kramarskim tovarom","Tprhuet kramnym tovarom","Prodazh belogo hleba I shynkovanie vina","Prodazh beloho khleba","Prodazh ryby","Prodazh soli i ryby", "Prodazha beloho khleba","Prodazha miasa","Prodazha na bazare orekhov I drugikh fruktov","Prodazha na bazare vialoj ryby","Prodazha ryby","Prodazha skla I soli na bazare","Prodazha soli","Prodazha soli i ryby","Prodazha v rozne shkla","Prodaet veshchi na bazare","Torg raznym kramarskim tovarom", "Torguet raznym kramarskim melkim drobjazkom", "Torhuet raznym kramarskim","Torhuet raznym kramarskom tovarom","Torhuet raznym kramnym drobiazkom","Torhuet raznym kramskom tovarom", "Toth raznym kramarskim tovarom",  "Torhuet raznym kramnym drobiazkom", "Prodazha khleba", "Prodazha ryby, remeslo kushnirskoe", "Torhuet raznym kramarskim drobiazkom","Torguet kramnym drobyazkom","Prodazha olii"),
                              Винокуріння = c("Shinkovanie vina","Shynkovanie brahi","Shynkovanie vina I remeslo koval'skoe","Shynkovanie vina, medu I piva", "Shynkovanie vina, promysel reznickij","Shynkovanie vina, Remeslo koval'skoe", "Shynkovanie vina, remeslo kushnirskoe","Shynkovanie voloskoho vina","Pitaetsia z vyvaru medu I piva" ,"Shynkovanie vina I dehtiu" ,  "Prodazha brahi na bazare", "Shynkovanie dehtu","Shynkovaniye vina", "Shynkovanie vina i remeslo kraveckoe","Shynkovanie vina, prodazha soli i ryby", "Shynkovaniye vina","Prodazha brahi v dome","Shynkovanie"  ,"shynkovanie vina", "Shynkovanie vina", "Shynkovanie vina + Remeslo kraveckoe" ),
                              Кушнірство = c( "Remeslo Kushnirske"  , "Remeslp kushnirskoe", "Remeslo Kushnirskoe"  ,"Remeslo kushnirskoe","Remeslo Kushnirskoe, prodazha d'iohtia"),
                              Ковальство = "Remeslo Koval'skoe",
                              Теслярство = c("Remeslo tesliarske","Remeslo teselne"), 
                              Чоботарство = c( "Remeslo Shevskoe","Remeslo shapochnicheskoe", "Remeslo shapochnickoe", "Remeslo shevskoe",  "Remeslo shevske" , "Remeslo shevskoye" ),
                              Столярство =  "Remeslo stoliarskoe" ,
                              Ткацтво =   c("Remeslo tkackoe, shynkovanie dehtu","Remeslo tkackoe"),
                              Гутництво = "Remeslo skliarskoe",
                              Мистецтво = c( "Remeslo Ikonopisnoe","Iskustvo fershal'skoe (fel'dsher)" ,"Ihra muzyki","Uprazhniaetsia iskustvom ikonopisnym" ),
                              Шаповальсво = c( "Remeslo shapoval'skoe","Делают ковдры"),
                              Різьбярство =  c("Promysel reznickij", "Promysel rezniczkij"  ), 
                              Лимарство = c("Remeslo lymarskoe", "Remeslo lymarskoe" ,"Remeslo rymarskoe"),
                              Кравецтво = "Remeslo Kraveckoe",
                              `Фінанасові послуги` =  "Menianie den'hami" ,
                              `Робота в церкві` =  c("Ponomar'" ,"Pddiachij cerkvi Voskresenskoj", "palamar","Poslushnik monastyria" ,"dyyakon", "Diachok v shkole", "Diachek Preobrazhenskoj cerkvi", "Diachek cerkvi Pokrovskoj","Rabotnik monastyrskiy" ,"V monastyre Mikhaylovskom knigi perepletayet", "pisar Mikhaylovskogo monastyria"),
                              Канцелярія = c("Rajcia mahistratu","Poddyachij kanczelyarii"),
                              Заробітки = c( "S zapabotkov serpom"  ,"S zarabotkov"  , "S zarabotkov serpom" ,"Pitaetsia z zazhynok serpom","Pitaetsia z zarabotkov","na propitanii bez zaplaty","Maliar (маляр), с заработков"  ,"Nachoditsia na usluzhenii kozaka Pereyaslavskoho Thokhyma Karpenka","Detskaja kormilica","Z zarabotkov","Z zarabotkov khleba","Z zarobotkov","Z zazhynok serpom"),
                              Орендарство = "6 niv zdae vojtoveckim obyvateliam za desiatunu",
                              Благодійність = c( "Z milostyni dobrookhotnykh datelej","Z milosti ot dobrookhotnyh datelej","S milostyni" , "Remeslo zolotarskoe","Pitaetsia z dobrookhotnykh datelej" ,"Pitaetsia z miloslivoho podajanija"),
                              Бондарство = c( "Remeslo bondarskoe"),
                              Золотарство="Remeslo zolotarskoe"))%>%
    mutate(marital = fct_collapse(marital, married = c("married", "maried"), unmarried = c("unmaried","unmarried"), widowed = c("vdov", "vdova")))%>%
    mutate(status = as_factor(str_trim(status))) %>%
    mutate(relation = fct_collapse(status,
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
  ))%>%
  mutate(capital = parse_number(capital), job = as.factor(job))%>%
  mutate(`Спосіб оплати` = fct_collapse(wage,
                                        `За гроші` = c("Z zarabotku priadivoho","Z zarabotka priazhej khoziajskoj","na 0,5 roku za 40 kop","0,15","0,4", "0,6","0,8", "1","1 v god, odezha i kharchi khozyajskie","1,1","1,2","1,3", "1,4", "1,5", "1,8", "2", "2 v god, odezha khozyajskaya", "2,2", "2,4", "2,5", "3", "3, na odeji hoziayskoy", "3,25", "3,4","3,5", "4", "5", "6","60 kop", "60 kopeek, v odeyanii khozyajskom","7","8", "9"),                     
                                        `За харчування та одяг` = c("Bez zaplaty za propitanie" ,"за пропитание в одежу","Za propitanie i snabzhenie", "Za propitanie I snabzhenie",  "Za propitanie I odezhu" ,"Za propitanie i odezhu", "Za propitanie bez zaplaty" ,"Za propitanie (bol'she pitaetsia z milostynnoho podajanija)","Za propitanie"  ,"Po svojstvu v odeyanii khozyajskom" ,"Na vsem soderzhanii svoem" ,"Na poslushanii", "Na propitanii hoziayskom" ,"Bez zaplaty na vsem svoem soderzhanii" ,"Bez zaplaty na vsem soderzhanii hoziajskom", "Bez zaplaty","Bez zaplaty na vsem soderzhanii hoziajskom"),
                                        `Через борг` = "Za dolh 19 rublej po prohovoru hrodskoho suda"  ,
                                        `За навчання` = c("Za vospitanie i obuchenie remeslu", "Vo izuchenii remesla" ,"bez zaplaty za obuchenie remeslu",  "Za obechenie remesla","Za obuchenie ramaslu", "Za obuchenie remesla", "Za obuchenie remesla, plat`e khozyajskoe", "Za obuchenie remeslu", "Za obuzhenie remeslu shevskomu"),
                                        інше = c("Z diskrecij")))
#Given that there is no socia status of kids we assign their parents' one to them
tidy_pereyaslav <- tidy_pereyaslav%>%
  mutate(hospodar_ss = if_else(status=="hospodar", social_status, NA))%>%
  fill(hospodar_ss)%>%
  mutate(social_status = if_else(is.na(social_status) & 
                                   str_detect(status,regex('doch|brat|syn|sest', ignore_case = TRUE)), 
                                 hospodar_ss, 
                                 social_status))
  