rm(list = ls())
cat("\f") # clear console
dev.off()

############################# Regression Model and Visualization of RCTOA Pilot Data (8 subjects total) ##############################

# upload data
Data1 = read.csv("C:\\Users\\blindse3\\Dropbox\\Research Projects\\Biomech Imaging Projects\\RCT OA\\Pilot_RCT_OA\\Preliminary Data\\RCTOA Pilot Data.csv")
attach(Data1)
# set the following variables as factors:
# Session 1 = baseline, 2 = week 4 posttest, 3 = week 8 posttest, 4 = week 9 posttest
Data1$Session <- factor(Data1$Session,levels=c(1,2,3,4)) 
# add levels to group assignment (control, or trunk lean) 
Data1$Group <- factor(Data1$Group, levels=c('Control','Trunk Lean'))
# add levels to trial factor, there were 5 trials per each condition
Data1$Trial <- factor(Data1$Trial, levels=c(1,2,3,4,5))

# load libraries
library(nlme)
library(psych)
library(lme4)
library(lmerTest)

# linear mixed model fit using maximum likelihood approach
# random intercept and slope by participant with REML (less biased)
mod3 = lmer(PKAM1~Session*Group+(1+Session|ID),data=Data1,REML=TRUE)
summary(mod3)


##################### observation line plots ######################
Data1$predict=predict(mod3)
library(gplots)
library(ggplot2)
library(ggpubr)


                                                                                                                                                                                                                                            
plot1=ggplot(data=Data1[which(Data1$ID==2),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="gray") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                             panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="gray")+ labs(y="")+ylim(-0.6,0.4) + theme(axis.title.x=element_blank(),axis.text.x=element_blank())
plot2=ggplot(data=Data1[which(Data1$ID==4),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="gray") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                             panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="gray")+ labs(y="")+ylim(-0.6,0.4) + theme(axis.title.x=element_blank(),axis.text.x=element_blank())
plot3=ggplot(data=Data1[which(Data1$ID==5),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="gray") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                             panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="gray")+ labs(y="")+ylim(-0.6,0.4) + theme(axis.title.x=element_blank(),axis.text.x=element_blank())
plot4=ggplot(data=Data1[which(Data1$ID==7),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="gray") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                             panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="gray")+ labs(y="")+ylim(-0.6,0.4) + theme(axis.title.x=element_blank(),axis.text.x=element_blank())
plot5=ggplot(data=Data1[which(Data1$ID==8),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="gray") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                             panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="gray")+ labs(y="")+ylim(-0.6,0.4)
plot6=ggplot(data=Data1[which(Data1$ID==1),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="black") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                              panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="black") + labs(y="")+ylim(-0.6,0.4) + theme(axis.title.x=element_blank(),axis.text.x=element_blank())

plot7=ggplot(data=Data1[which(Data1$ID==9),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="black") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                              panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="black")+ labs(y="")+ylim(-0.6,0.4) + theme(axis.title.x=element_blank(),axis.text.x=element_blank())
plot8=ggplot(data=Data1[which(Data1$ID==11),], 
             aes(x=Session, y=PKAM1, group=ID)) + geom_point(alpha = 1,color="black") + theme(axis.text  = element_text(size = 10),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                              panel.background = element_blank(),axis.line.y=element_blank())+geom_line(aes(y=predict), size=0.8,color="black")+ labs(y="")+ylim(-0.6,0.4)

theme_set(theme_pubr())
figure <- ggarrange(ggarrange(plot1,plot2,plot3,plot4,plot5,labels = c("A"), ncol=1,nrow=5),ggarrange(plot6,plot7,plot8, labels = c("B"),ncol=1,nrow=3),
                    ncol = 2, nrow = 1)

figure <- annotate_figure(figure, left = text_grob("First Peak Knee Abduction Moment (Nm/kgm)", rot = 90))
                
figure



####################### confidence interval plots ##############################

s1c=contest(mod3,c(0,1,0,0,0,0,0,0),confint=TRUE,joint=FALSE) #control average change from baseline
s1t=contest(mod3,c(0,1,0,0,0,1,0,0),confint=TRUE,joint=FALSE) #trunk lean average change from baseline

s2c=contest(mod3,c(0,0,1,0,0,0,0,0),confint=TRUE,joint=FALSE) #control average change from baseline
s2t=contest(mod3,c(0,0,1,0,0,0,1,0),confint=TRUE,joint=FALSE) #trunk lean average change from baseline

s3c=contest(mod3,c(0,0,0,1,0,0,0,0),confint=TRUE,joint=FALSE) #control average change from baseline
s3t=contest(mod3,c(0,0,0,1,0,0,0,1),confint=TRUE,joint=FALSE) #trunk lean average change from baseline


contrast=data.frame(group=c("Control","Control","Control","Intervention","Intervention","Intervention"),
                    Session=c(1,2,3,1,2,3),mean=c(s1c$Estimate,s2c$Estimate,s3c$Estimate,s1t$Estimate,s2t$Estimate,s3t$Estimate),
                    conf.low=c(s1c$lower,s2c$lower,s3c$lower,s1t$lower,s2t$lower,s3t$lower),conf.high=c(s1c$upper,s2c$upper,s3c$upper,s1t$upper,s2t$upper,s3t$upper))

# plot estimated change in mean KAM from baseline across sessions
plot=ggplot(contrast, aes(x=factor(Session), y=mean, color=group)) +
  geom_point(aes(shape=group),position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)
  ) +scale_shape_manual(values=c(3, 16),name="Group",labels=c("Control", "Intervention"))+
  labs(y="Estimated Change in Mean KAM from Baseline",x="Posttest")+theme(legend.position="right")+scale_color_manual(values = c("gray", "black"),name="Group",labels=c("Control", "Intervention"))+
  geom_hline(yintercept=0)+ylim(-0.2,0.4)

plot



