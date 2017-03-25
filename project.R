s=read.csv("C:\\Users\\Roni Artzi\\Desktop\\R\\2011_INC.csv",sep=";")
x=subset(s,birthy <= 1986 & childalo != 1 & oldageal != 1 & incsal > 2000 & incavr > 0  & schooly !=99 & schoolyg != 9 & schoolyg != 0,  select=c(age,schooly, schoolyg, incsal, incavr,sex, district,religion,immigry))
#table of zone
unique(s$district)
code=c(24, 32, 31, 21, 61, 44, 70, 51, 41, 53, 23, 43, 62, 52, 42,11)
code=code[order(code)]
x_cor=c(31.769450,32.964715,32.672664,32.934189,32.793995,32.434917,32.313691,32.084027,31.903926,31.892158,32.083672,32.068091,32.015928,31.668134,31.251903,31.983468)
y_cor=c(35.212784,35.496659,35.219418,35.081513,34.987422,34.919143,34.940245,34.887271,35.204080,34.810040,34.789865,34.826218,34.788162,34.577185,34.794835,35.302694)
zone=data.frame(district=code, name=c("Jerusalem","Zefat","Yizrael","Akko","Haija",
                                      "Hadera","Sharon","Petach-Tikva","Ramala","Rehovot",
                                      "Tel-Aviv","Ramat-Gan","Holun","Ashkelon","Beer-Sheva",
                                      "Judea and Sammaria"), x_cordinate=x_cor, y_cordinate=y_cor)
#table of diplima





#join
data=merge(x = x, y = zone, by = "district", all = TRUE)
names(data)
#main_data - without the cordinate!!
main_data=data[,c("schoolyg","schooly","incsal","incavr","district","name","sex","religion","immigry","age")]

hist(log(main_data$incsal))

#NIRMOL
edu=scale(main_data$schooly)
edug=scale(main_data$schoolyg)
money=scale(main_data$incsal)
city=scale(main_data$district)

#chang the value in the data base (to binary)
main_data$sex[main_data$sex==1]=0
main_data$sex[main_data$sex==2]=1
main_data$religion[main_data$religion==1]=0
main_data$religion[main_data$religion==2]=1
main_data$immigry[is.na(main_data$immigry)]=0
main_data$immigry[main_data$immigry!= 0]=1

#normalization 
qqnorm(log(main_data$incsal),
       main="Normal Q-Q Plot of log.income salary",
       xlab="Theoretical Quantiles of log (incsal)",
       ylab="Sample Quantile of log (incsal)")
qqline(log(main_data$incsal),col="red")
hist(log(main_data$incsal),main="Histogram of log.income sala",
     xlab="log (incsal)")
