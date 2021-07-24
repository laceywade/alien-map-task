library(ggplot2)
library(lmerTest)
library(dplyr)
library(binom)


### Import data file Wade_Roberts_Data.csv as "conv"
conv<-read.csv(file.choose(), header=T)
conv$Before.After<-factor(conv$Before.After, levels=c("before", "after"))


######################################################################################################
                                      ##    GRAPHS   ##
######################################################################################################
                  
#### Summarise [f] usage by condition
sum.by.condition <- conv %>%
  dplyr::group_by(Player,  Condition) %>%
  dplyr::summarise(N = n(),
                   f.pct = mean(f),
                   f.sd = sd(f),
                   f.se = f.sd/sqrt(N),
                   f.ci = f.se*1.96)
### Visualize [f] usage for explicit-expectation participants
ggplot(sum.by.condition[sum.by.condition$Player=="playerA",], 
       aes(Condition, f.pct*100, color=Condition))+geom_point()+
  ggtitle("Explicit-expectation participants")+
  geom_errorbar(aes(ymin=f.pct*100-f.ci*100, ymax=f.pct*100+f.ci*100))+
  theme_bw()+ylab("% [ f ] usage")+
  scale_color_brewer(palette="Dark2")+
  ylim(0, 100)+
  theme(legend.position="none")


### Summarise [p] usage by phase order
sum.by.phase <- conv %>%
  dplyr::group_by(Player,  Phase) %>%
  dplyr::summarise(N = n(),
                   p.pct = mean(p),
                   p.sd = sd(p),
                   p.se = p.sd/sqrt(N),
                   p.ci = p.se*1.96)
### Visualize [p] usage for no-expectation participants
sum.by.phase$Phase<-as.factor(sum.by.phase$Phase)
ggplot(sum.by.phase[sum.by.phase$Player=="playerB",], 
       aes(Phase, p.pct*100, color=Phase))+geom_point()+
  geom_errorbar(aes(ymin=p.pct*100-p.ci*100, ymax=p.pct*100+p.ci*100))+
  theme_bw()+ylab("% [ p ] usage")+
  scale_color_manual(values=c("darkseagreen3", "darkslategray4", "dodgerblue4"))+
  ggtitle("No-expectation participants")+ylim(0,100)+
  theme(legend.position="none")



#### Summarise variant usage by condition order
sum.by.first <- conv %>%
  dplyr::group_by(Player, Condition, Before.After, First) %>%
  dplyr::summarise(N = n(),
                   f.pct = mean(f),
                   f.sd = sd(f),
                   f.se = f.sd/sqrt(N),
                   f.ci = f.se*1.96,
                   v.pct = mean(v),
                   v.sd = sd(v),
                   v.se = v.sd/sqrt(N),
                   v.ci = v.se*1.96)
sum.by.first$Order[sum.by.first$First=="matched" & 
                     sum.by.first$Condition=="matched"]<-"   Matched First"
sum.by.first$Order[sum.by.first$First=="unmatched" & 
                     sum.by.first$Condition=="unmatched"]<-" Unmatched First"
sum.by.first$Order[sum.by.first$First=="unmatched" & 
                     sum.by.first$Condition=="matched"]<-"Matched After"
sum.by.first$Order[sum.by.first$First=="matched" & 
                     sum.by.first$Condition=="unmatched"]<-"  Unmatched After"
### Visualize
ggplot(sum.by.first[sum.by.first$Player=="playerA" & 
                      sum.by.first$Condition!="samespecies",], 
       aes(Before.After, f.pct*100, color=Condition))+
  geom_point(color="royalblue4")+
  geom_point(aes(y=v.pct*100), color="forestgreen")+
  facet_wrap(Order~Condition)+
  geom_errorbar(aes(ymin=f.pct*100-f.ci*100, ymax=f.pct*100+f.ci*100), color="royalblue4")+
  geom_errorbar(aes(ymin=v.pct*100-v.ci*100, ymax=v.pct*100+v.ci*100), color="forestgreen")+
  geom_text(color="royalblue4", label="F", nudge_y=8)+
  geom_text(aes( y=v.pct*100), color="forestgreen", label="V", nudge_y=5)+
  theme_bw()+ylab("% variant usage")+
  xlab("Status of exposure to partner's speech")+
  ggtitle("Explicit-expectation participants")+
  theme(legend.position = "none")+
  ylim(-1, 50)


### Summarise all variant usage by condition
sum.by.condition <- conv %>%
  dplyr::group_by(Player,  Condition, Before.After) %>%
  dplyr::summarise(N = n(),
                   f.pct = mean(f)*100,
                   p.pct = mean(p)*100,
                   v.pct = mean(v)*100,
                   f.sd = sd(f)*100,
                   f.se = f.sd/sqrt(N),
                   f.ci = f.se*1.96,
                   p.sd = sd(p)*100,
                   p.se = p.sd/sqrt(N),
                   p.ci = p.se*1.96,
                   v.sd = sd(v)*100,
                   v.se = v.sd/sqrt(N),
                   v.ci = v.se*1.96)
### Visualize explicit-expectation participants' variant usage by condition & pre/post observation
ggplot(sum.by.condition[sum.by.condition$Player=="playerA",], 
       aes(Before.After, f.pct))+
  geom_point(color="royalblue4")+
  geom_point(aes( y=v.pct), color="forestgreen")+
  geom_point(aes( y=p.pct), color="orangered3")+
  geom_text(color="royalblue4", label="F", nudge_y=8)+
  geom_text(aes( y=v.pct), color="forestgreen", label="V", nudge_y=6)+
  geom_text(aes( y=p.pct), color="orangered3", label="P", nudge_y=8)+
  facet_wrap(~Condition)+
  ggtitle("Explicit-expectation participants")+
  geom_errorbar(aes(ymin=f.pct-f.ci, ymax=f.pct+f.ci), color="royalblue4")+
  geom_errorbar(aes(ymin=v.pct-v.ci, ymax=v.pct+v.ci), color="forestgreen")+
  geom_errorbar(aes(ymin=p.pct-p.ci, ymax=p.pct+p.ci), color="orangered3")+
  theme_bw()+ylab("% variant usage")+
  scale_color_brewer(palette="Dark2")+ylim(-1, 105)+
  theme(legend.position="none")+
  xlab("Status of exposure to partner's speech")


#### Summarise by Individuals
sum.by.first.all <- conv %>%
  dplyr::group_by(Group, Player, Condition, Before.After) %>%
  dplyr::summarise(N = n(),
                   f.pct = mean(f),
                   v.pct = mean(v),
                   sd = sd(f),
                   se = sd/sqrt(N),
                   ci = se*1.96)
sum.by.first.all2 <- sum.by.first.all %>%
  dplyr::group_by(f.pct, v.pct, Player, Condition, Before.After) %>%
  dplyr::summarise(N = n())
### Visualize individual differences
ggplot(sum.by.first.all2[sum.by.first.all2$Player=="playerA" ,], 
       aes(Before.After, f.pct*100, color=Condition))+
  geom_point(aes(size=N), alpha=.5)+
  facet_wrap(~Condition)+
geom_line(data=sum.by.first.all[sum.by.first.all$Player=="playerA"
                                  ,], 
            aes(x=Before.After, y=f.pct*100, group=Group), alpha=.5)+
  theme_bw()+ylab("% [ f ] usage")+
  scale_color_brewer(palette="Dark2", guide=FALSE)+
  xlab("Status of exposure to partner's speech")+
  ylim(0, 100)+
  ggtitle("Explicit-expectation participants")



######################################################################################################
                                          ##    MODELS  ##
######################################################################################################

f.mod<-glmer(f~relevel(Condition, "matched")*relevel(Before.After, "before")+
               Phase*First+
               (1|Group),data=conv[conv$Player=="playerA",], 
             family=binomial)
ss <- getME(fit1,c("theta","fixef"))
f.mod.update <- update(f.mod,start=ss,control=glmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)))

p.mod<-glmer(p~Phase+(1|Group), data=conv[conv$Player=="playerB",], family=binomial)

