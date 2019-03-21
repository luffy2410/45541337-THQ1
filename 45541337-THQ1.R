Github username: luffy2410
Name of repository: 45541337-THQ1
File name: 45541337-THQ1.R

#Question1
e=2.7183
Coupon=C
Face value=F
The number of coupon payments=n
#coupon C is paid every 6 months 
tn=n/2
tj=seq(0.5,tn,by=0.5)
y=c(y(0.5),y(1),y(1.5),...,y(tn))
P=sum(C*e^-(y*tj))+F*e^-(y(tn)*tn)

#Question3
(a)
dataset=read.csv(file.choose())

(b)
dataset=na.omit(dataset)
dataset

(c)
plot(dataset$time,dataset$gdp,main = "Singapore GDP growth",xlab = "Time",ylab = "GDP (%)")

(d)
#period1
mg1=mean(subset(dataset,period==1)$gdp)
sd1=sd(subset(dataset,period==1)$gdp)
#period2
mg2=mean(subset(dataset,period==2)$gdp)
sd2=sd(subset(dataset,period==2)$gdp)
#period3
mg3=mean(subset(dataset,period==3)$gdp)
sd3=sd(subset(dataset,period==3)$gdp)
#create data table
statistics=c("mean gdp","sd gdp")
period1=c(mg1,sd1)
period2=c(mg2,sd2)
period3=c(mg3,sd3)
stat.table=data.frame(statistics,period1,period2,period3)
stat.table

(e)
pairs(dataset[,-(1:2)])

(f)
S=lm(dataset$exp~dataset$gdp)
summary(S)
This linear regression established that Export growth rate could statistically  predict GDP, F(1, 108) = 43.66, p = 1.524e-09 and Export growth rate accounted for 28.133% of the explained variability in GDP. 
The regression equation predicted GDP = -0.9336+1.5092*(Export growth rate).

(g)
M=lm(dataset$exp+dataset$epg+dataset$hpr+dataset$gdpus+dataset$oil+dataset$crd~dataset$gdp)
summary(M)
The model above shows that 6 variables:exp, epg, hpr, oil, gdpus, crd could  predict GDP, F(1, 108) = 1.141, p = 0.2879,
and and 6 variables accounted for 0.13% of the explained variability in GDP.

(h)
#calculate quantile
q=quantile(dataset$gdp,0.05)
#create vecto state
state=rep("crisis",nrow(dataset))
state[dataset$gdp>q]="normal"
state=as.factor(state)
dataset=data.frame(dataset,state)
#fit model
G=glm(dataset$bci[1:72]~dataset$state[1:72])
#compute confusion matrix
predict(G)
table(dataset$state[1:72],predict(G))

