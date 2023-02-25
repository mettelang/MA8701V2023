bonf=function(alpha,m) return(alpha/m)

sidak=function(alpha,m) return(1-(1-alpha)^(1/m))

alpha=0.05
m=1:100
x=bonf(0.05,1:100)
y=sidak(0.05,1:100)
plot(log(x),log(y),type="b")

plot(1:100,y)
plot(1:100,x)

plot(x-y,0.5*(x+y))
insx-y
summary(x-y)


set.seed(8701)
sample(1:5,5,replace=FALSE)


https://csse.szu.edu.cn/staff/zhuzx/Datasets.html
https://github.com/kivancguckiran/microarray-data

https://aacrjournals.org/cancerres/article/62/17/4963/509160/Translation-of-Microarray-Data-into-Clinically

sykkeldata fra eksamen i TMA4268?
  
  
  Besparing: 3020 timer i 2022 til 2740 timer i 2023. Vi betaler ca 250 pr time for studass (det er inkludert sosial kostander, de for betalt 180) og for de som tar LAOS dekker vi i tillegg 20 timer oppå dette. 
Besparingen er ikke veldig stor: (3020-2740)*250=70k
