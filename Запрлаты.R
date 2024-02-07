#Загрузка библиотек
lapply(c('tidyverse','rstudioapi','openxlsx','gridExtra', 'grid', "readxl", "scales", "showtext"), require, character.only = TRUE)

#Подготовка данных
Data <- read_excel("C:/Users/ex633/Downloads/Анонимный зарплатный опрос чата ричерчеров - волна 2 (январь 2024).xlsx") # сюда пишите местонахождение эксельки на вашем компьютере
Data[Data=="NA"] <- NA
Data$Sphere <- factor(Data$Sphere, levels = c("Академические исследования, преподавание",
                                              "Клиентский опыт (CX)",
                                              "Маркетинговые исследования",
                                              "Продуктовые исследования (UX)",
                                              "Затрудняюсь ответить" ))

Data$Type <- factor(Data$Type, levels = c("Исследовательское агентство",
                                          "IT-компания",
                                          "Услуги",
                                          "Производство",
                                          "Фриланс",
                                          "Консалтинг",
                                          "Затрудняюсь ответить"))

Data$Status <- factor(Data$Status, levels = c("Руководитель компании",
                                              "Руководитель направления, директор",
                                              "Руководитель отдела/команды",
                                              "Ведущий сотрудник",
                                              "Сотрудник",
                                              "Стажер, практикант",
                                              "Фрилансер",
                                              "Затрудняюсь ответить"))

Data$Level <- factor(Data$Level, levels = c("Рок-звезда",
                                            "Lead",
                                            "Senior",
                                            "Middle",
                                            "Junior",
                                            "Затрудняюсь ответить"))

Data$Mode <- factor(Data$Mode, levels = c("Количественник",
                                          "Качественник",
                                          "И то, и другое"))

#Шрифты и цвета
font_add_google(db_cache = F, "Wix Madefor Display", family = "wix")
showtext_auto()
Color <- RColorBrewer::brewer.pal(7, 'Greens')[4]

#РАСЧЕТЫ ПО ВСЕМ

#Зарплаты - боксплот
Allsalary <- ggplot(na.omit(Data), aes (x=Salary, y=Sphere)) + geom_boxplot(width=0.3)+ coord_flip() + geom_point(shape=15, size=4, aes(col=Level)) + 
  scale_color_brewer(palette = 'Greens', direction=-1) + theme_minimal()+ scale_x_continuous(labels = scales::number) +
  theme(text = element_text(size=30, family="wix", colour = 'gray40'))+ 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank())+ scale_y_discrete(labels = 
                                                                                                                           c("Академические \n исследования, \n преподавание",
                                                                                                                             "Клиентский опыт (CX)",
                                                                                                                             "Маркетинговые \n исследования",
                                                                                                                             "Продуктовые\n исследования (UX)",
                                                                                                                             "Затрудняюсь ответить" ))

png("~/ЗП/Зарплаты_все.png", width = 2500, height = 1280)
Allsalary
dev.off()






#Зарплаты - боксплот Академия

table(Data[which(Data$Sphere == "Академические исследования, преподавание"),]$Status)

my_colors <- rev(RColorBrewer::brewer.pal(9,"Blues")[3:9])
Allsalary <- ggplot(na.omit(Data[which(Data$Sphere == "Академические исследования, преподавание"),]),
                    aes (x=Salary, y=Status)) + geom_boxplot(width=0.3)+ coord_flip() + geom_point(shape=15, size=4, aes(col=Level)) + 
 theme_minimal()+ scale_x_continuous(labels = scales::number) +
  theme(text = element_text(size=30, family="wix", colour = 'gray40'))+ 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank()) +
  scale_y_discrete(labels = c("Руководитель \n отдела/команды",
                            "Ведущий сотрудник",
                            "Сотрудник",
                            "Стажер, практикант"))+ 
  scale_colour_manual(values = my_colors)

png("~/ЗП/Зарплаты_академия.png", width = 2500, height = 1280) #папку "ЗП" сделайте заранее, потому что может быть ошибка, если ее не сделать ручками
Allsalary
dev.off()


#Зарплаты - боксплот UX

table(Data[which(Data$Sphere == "Продуктовые исследования (UX)"),]$Status)

my_colors <- rev(RColorBrewer::brewer.pal(9,"Purples")[3:9])
Allsalary <- ggplot(na.omit(Data[which(Data$Sphere == 'Продуктовые исследования (UX)'),]),
                    aes (x=Salary, y=Status)) + geom_boxplot(width=0.3)+ coord_flip() + geom_point(shape=15, size=4, aes(col=Level)) + 
  theme_minimal()+ scale_x_continuous(labels = scales::number) +
  theme(text = element_text(size=25, family="wix", colour = 'gray40'))+ 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank()) +
 
  scale_colour_manual(values = my_colors) +

scale_y_discrete(labels = c("Руководитель \n направления, директор",
                            "Руководитель \n отдела/команды",
                            "Ведущий сотрудник",
                            "Сотрудник",
                            "Стажер, практикант",
                            "Затрудняюсь ответить" ))

png("~/ЗП/Зарплаты_юикс.png", width = 2800, height = 1280)
Allsalary
dev.off()


#Зарплаты - боксплот маркетинг

table(Data[which(Data$Sphere == "Маркетинговые исследования"),]$Status)

my_colors <- rev(RColorBrewer::brewer.pal(9,"Reds")[3:9])
Allsalary <- ggplot(na.omit(Data[which(Data$Sphere == "Маркетинговые исследования"),]),
                    aes (x=Salary, y=Status)) + geom_boxplot(width=0.3)+ coord_flip() + geom_point(shape=15, size=4, aes(col=Level)) + 
  theme_minimal()+ scale_x_continuous(labels = scales::number) +
  theme(text = element_text(size=25, family="wix", colour = 'gray40'))+ 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank()) +
  scale_y_discrete(labels = c("Руководитель \n компании",
                              "Руководитель \n направления, директор",
                              "Руководитель \n отдела/команды",
                              "Ведущий сотрудник",
                              "Сотрудник",
                              "Стажер, практикант",
                              "Фрилансер",
                              "Затрудняюсь ответить" ))+ 
  scale_colour_manual(values = my_colors)

png("~/ЗП/Зарплаты_маркетинг.png", width = 2800, height = 1280)
Allsalary
dev.off()

#Зарплаты - боксплот CX

table(Data[which(Data$Sphere == "Клиентский опыт (CX)"),]$Status)

my_colors <- rev(RColorBrewer::brewer.pal(9,"Oranges")[3:9])
Allsalary <- ggplot(na.omit(Data[which(Data$Sphere == "Клиентский опыт (CX)"),]),
                    aes (x=Salary, y=Status)) + geom_boxplot(width=0.3)+ coord_flip() + geom_point(shape=15, size=4, aes(col=Level)) + 
  theme_minimal()+ scale_x_continuous(labels = scales::number) +
  theme(text = element_text(size=25, family="wix", colour = 'gray40'))+ 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank()) +
  scale_y_discrete(labels = c("Руководитель \n направления, директор",
                              "Руководитель \n отдела/команды",
                              "Ведущий сотрудник",
                              "Сотрудник",
                              "Стажер, практикант",
                              "Фрилансер",
                              "Затрудняюсь ответить" ))+ 
  scale_colour_manual(values = my_colors)

png("~/ЗП/Зарплаты_cx.png", width = 2800, height = 1280)
Allsalary
dev.off()


#Сфера - барчарт

Sphere <- Data %>% group_by(Sphere) %>% count(Sphere) %>% na.omit() %>%
  ggplot(aes(x=reorder(Sphere, desc(Sphere)), y=n)) + geom_bar(stat="identity", fill = Color) + coord_flip() + theme_void()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank(), axis.line.y = element_blank())+
  geom_text(aes(label = n), hjust = -1, size = 10, color="gray40")+
  theme (axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y = element_text(size=15, hjust = 1),
         plot.title = element_text(size=20),
         text =element_text(family = "wix"))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 2))) 

png("~/ЗП/Сфера_все.png", width = 680, height = 580)
par(mar = c(1000, 1000, 1000, 1000))
Sphere
dev.off()

#Тип  - барчарт

Type <- Data %>% group_by(Type) %>% count(Type) %>% na.omit() %>%
  ggplot(aes(x=reorder(Type, desc(Type)), y=n)) + geom_bar(stat="identity", fill = Color) + coord_flip() + theme_void()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank(), axis.line.y = element_blank())+
  geom_text(aes(label = n), hjust = -1, size = 10, color="gray40")+
  theme (axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y = element_text(size=15, hjust = 1),
         plot.title = element_text(size=20),
         text =element_text(family = "wix"))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 2)))

png("~/ЗП/Тип_все.png", width = 680, height = 580)
par(mar = c(1000, 1000, 1000, 1000))
Type
dev.off()

#Статус - барчарт

Status <- Data %>% group_by(Status) %>% count(Status) %>% na.omit() %>%
  ggplot(aes(x=reorder(Status, desc(Status)), y=n)) + geom_bar(stat="identity", fill = Color) + coord_flip() + theme_void()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank(), axis.line.y = element_blank())+
  geom_text(aes(label = n), hjust = -1, size = 10, color="gray40")+
  theme (axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size=15, hjust = 1),
    plot.title = element_text(size=20),
    text =element_text(family = "wix"))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 2)))

png("~/ЗП/Статус_все.png", width = 680, height = 580)
par(mar = c(1000, 1000, 1000, 1000))
Status
dev.off()

#Уровень - барчарт

Level <- Data %>% group_by(Level) %>% count(Level) %>% na.omit() %>%
  ggplot(aes(x=reorder(Level, desc(Level)), y=n)) + geom_bar(stat="identity", fill = Color) + coord_flip() +
  theme_void()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank(), axis.line.y = element_blank())+
  geom_text(aes(label = n), hjust = -1, size = 10, color="gray40")+
  theme (axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y = element_text(size=15, hjust = 1),
         plot.title = element_text(size=20),
         text =element_text(family = "wix"))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 2)))

png("~/ЗП/Уровень_все.png", width = 680, height = 580)
Level
dev.off()

#Вид исследователя

DonutData <- Data %>% group_by(Mode) %>% count(Mode)  
DonutData$fraction = DonutData$n / sum(DonutData$n)
DonutData$ymax = cumsum(DonutData$fraction)
DonutData$ymin = c(0, head(DonutData$ymax, n=-1))
DonutData$labelPosition <- (DonutData$ymax + DonutData$ymin) / 2

Mode <- ggplot(DonutData, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Mode)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) + theme_void() +
  geom_label( x=3.5, aes(y=labelPosition, label=paste(n)), size=30, show.legend=FALSE, color='gray40',  label.size = NA) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank(), axis.line.y = element_blank())+
    theme (axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         plot.title = element_text(size=20),
         text =element_text(family = "wix"),
         legend.position = c(0.5,0.5),
         legend.text = element_text(size=35, color='gray40', hjust = 0),
         legend.key.size = unit(400, 'char'),
         legend.key=element_rect(colour='white', size = 5)) +
  scale_fill_brewer(palette = 'Greens', direction=-1)
png("~/ЗП/Качкол_все.png", width = 1680, height = 1680, res=1)
Mode
dev.off()


#Года опыта - барчарт

Experience <- Data %>% mutate(Experience = cut(as.numeric(Experience), seq(0, 30, 5))) %>% mutate(Experience = factor(Experience, labels = 
        c("От 0 до 5 лет", "От 6 до 10 лет", "От 11 до 15 лет", "От 16 до 20 лет", "От 21 до 25 лет", "От 26 до 30 лет")))%>% group_by(Experience) %>% count(Experience) %>% na.omit() %>%
  ggplot(aes(x=reorder(Experience, desc(Experience)), y=n)) + geom_bar(stat="identity", fill = Color) + coord_flip() +
  theme_void()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), legend.title = element_blank(), axis.line.y = element_blank())+
  geom_text(aes(label = n), hjust = -1, size = 10, color="gray40")+
  theme (axis.line=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y = element_text(size=15, hjust = 1),
         plot.title = element_text(size=20),
         text =element_text(family = "wix"))+
  scale_y_continuous(expand = expansion(mult = c(0.1, 2)))

png("~/ЗП/Опыт_все.png", width = 680, height = 580)
Experience
dev.off()

