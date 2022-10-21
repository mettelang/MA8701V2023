library(ggplot2)

qs=c("The supervision was good",
"Good knowledge of the topic of my thesis",
"Good knowledge of admin aspects",
"Valuable follow-up theory",
"Valuable help programming",
"Answered questions quickly",
"Good feedback before submission")
qvals=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree","Not relevant")

qtime="How much time did you spend with your supervisor(s)?"
timespent=c("Less than one hour every month",
            "Around one hour every month",
            "Around one hour every second week",
            "Around one hour every week",
            "More than one hour every week")

mtfyma=rbind(c(1,0,1,2,11,0),
       c(0,1,1,3,10,0),
       c(1,4,2,5,3,0),
       c(1,0,1,3,9,0),
       c(0,2,4,5,1,2),
       c(2,1,0,0,11,0),
       c(1,1,2,5,6,0))
msmnfma=rbind(c(0,0,0,3,6,0),
              c(0,1,1,2,6,0),
              c(0,2,3,4,1,0),
              c(0,2,2,1,5,0),
              c(1,2,1,0,0,5),
              c(0,0,2,2,4,1),
              c(0,1,2,3,3,0))
mlreal=rbind(c(0,0,0,1,1,0),
             c(0,0,0,0,2,0),
             c(0,0,0,2,0,0),
             c(0,0,0,1,0,1),
             c(0,0,0,0,0,2),
             c(0,0,1,0,1,1),
             c(0,0,0,1,1,0))

tmsmnfma=c(0,2,0,6,1)
tmtfyma=c(1,0,3,8,2)
tmlreal=c(1,0,1,0,0)

tall=tmsmnfma+tmtfyma+tmlreal
tall
qall=msmnfma+mlreal+mtfyma
qall

timespent
shorttimes=c("<1hr/month","1hr/month","1hr/2weeks","1hr/week",">1hr/week")
tframe=as.data.frame(cbind(shorttimes,tall,100*tall/sum(tall)))
colnames(tframe)=c("Supervision","Count","Percentage")
tframe$Count=as.numeric(tframe$Count)
tframe$Percentage=as.numeric(tframe$Percentage)
tframe$Supervision=factor(tframe$Supervision,levels=shorttimes)

p=ggplot(data=tframe,aes(x=Supervision,y=Percentage))
p=p+geom_bar(stat="identity",fill="#0073C2FF")
#p=p+ylab("prosent")
p=p+theme(axis.text.x = element_text(size = 15))
                                     #angle = 0, vjust = 0, hjust=0,cex=2))
p

df=data.frame(qall)
colnames(df)=qvals
rownames(df)=qs

out<-df %>%
  rownames_to_column() %>%            # set row names as a variable
  gather(rowname2,value,-rowname) %>% # reshape
  rowwise() 
colnames(out)=c("Question","Answer","Count")

library(splitstackshape)
out2<- expandRows(out,"Count")

out$Question=factor(out$Question,levels=rev(qs))
out2$Question=factor(out2$Question,levels=rev(qs))
out$Answer=factor(out$Answer,levels=rev(qvals)
out2$Answer=factor(out2$Answer,levels=rev(qvals)


p = ggplot(out, aes(x=Question, y=Count,fill=Answer) ) + 
  geom_bar(position="fill",stat="identity")+
  theme_bw()+coord_flip()+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),strip.text = element_text(size=10, face="bold"),
        axis.text.x = element_text(hjust = 1, face="bold", size=12, color="black"),
        legend.text = element_text(color = "black", size = 16,face="bold"),
        axis.text.y = element_text( face="bold", size=12, color="black"))+
    ggtitle("My supervisor(s)")
p
  theme(, 
        axis.title.x = element_blank(),
        
        axis.title.y = element_blank(),
        
        legend.position = "none",
        legend.title = element_blank(),
        
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

#ggtitle("")

