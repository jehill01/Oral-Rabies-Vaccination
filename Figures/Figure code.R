###Figures

springgrid<-read.csv("springgrid.csv")
newracc<-read.csv("newracc.csv")
newgrid<-read.csv("newgrid.csv")
springraccoon<-read.csv("springraccoon.csv")

vars <- c("#009E73", "#0072B2", "#D55E00", "#CC79A7")
vars2<-c("#009E73", "#D55E00")

######Fig 2

springraccoon$Area[springraccoon$Area== 0.5] <- 0.16
raccspringfit<-(glmer(Biomarker~as.factor(Area)+as.factor(Area):Habitat+Habitat+scale(Raccoon)+Sex+(1|Year),
                      family=binomial(link="logit"), na.action=na.pass, data=springraccoon))
plot.df<-emmip(raccspringfit, ~as.factor(Area)|Habitat, type="response", plotit=FALSE)
ggplot(plot.df, aes(x=as.factor(Area), y=yvar, color=Habitat))+
  geom_point(size=3)+geom_errorbar(aes(ymin=yvar-SE, ymax=yvar+SE), data=plot.df, width=0.4)+facet_grid(~Habitat)+
  theme(strip.background=element_rect(color="black", fill="darkslategray3"), panel.border = element_rect(colour='black', fill='NA'), 
        panel.background = element_rect(fill='gray97'), panel.grid = element_line(colour = NA), panel.spacing = unit(1, "lines"),
        legend.key = element_rect(fill='white'), legend.position="none", legend.text=element_text(size=14),
        axis.text=element_text(size=14), axis.title.x = element_text(size=14, vjust=-0.5),axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14, vjust=3.9), strip.text = element_text(size=14, face="bold"))+
  theme(plot.margin=unit(c(0.3,0.3,0.5,0.5), "cm"), axis.ticks.length.x =unit(.25, "cm"))+
  labs(y = expression(atop("Estimated probabilty of","testing positive for biomarker")))+
  labs(x = expression(paste("Baited area ", "(km"^2 ,")")))+scale_color_manual(values=vars)

#####Fig3 
prop.regfit<-glmer(formula=cbind(Pos, Sum-Pos)~as.factor(Area)*Habitat+scale(Raccoon)+(1|Year)+(1|Grid), 
                   weights=Sum, data=springgrid, na.action=na.pass, family=binomial())
plot.df2<-emmip(prop.regfit, ~Area|Habitat, type="response", plotit=FALSE)
ggplot(plot.df2, aes(x=as.factor(Area), y=yvar, color=Habitat))+
  geom_point(size=3)+geom_errorbar(aes(ymin=yvar-SE, ymax=yvar+SE), data=plot.df2, width=0.4)+facet_grid(~Habitat)+
  theme(strip.background=element_rect(color="black", fill="darkslategray3"), panel.border = element_rect(colour='black', fill='NA'), 
        panel.background = element_rect(fill='gray97'), panel.grid = element_line(colour = NA), panel.spacing = unit(1, "lines"),
        legend.key = element_rect(fill='white'), legend.position="none", legend.text=element_text(size=14),
        axis.text=element_text(size=14), axis.title.x = element_text(size=14, vjust=-0.5),axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14, vjust=3.9), strip.text = element_text(size=14, face="bold"))+
  theme(plot.margin=unit(c(0.3,0.3,0.5,0.5), "cm"), axis.ticks.length.x =unit(.25, "cm"))+
  labs(y = expression(atop("Estimated proportion of raccoons", "positive for biomarker")))+
  labs(x = expression(paste("Baited area ", "(km"^2 ,")")))+scale_color_manual(values=vars)

##Fig 4
raccfit <-(glm(Biomarker~Season*Density+scale(Opossum, center=TRUE)+Residency, 
               family=binomial(link="logit"), na.action=na.pass, data=newracc))

plot.df3<-emmip(raccfit, ~as.factor(Density)|Season, type="response", plotit=FALSE)
ggplot(plot.df3, aes(x=Season, y=yvar, color=as.factor(Density)))+
  geom_point(size=3, position = position_dodge(width = 0.2))+geom_errorbar(aes(ymin=yvar-SE, ymax=yvar+SE),
                                                                           position = position_dodge(width = 0.2),data=plot.df3, width=0.15)+
  theme(strip.background=element_rect(color="black", fill="darkslategray3"), panel.border = element_rect(colour='black', fill='NA'), 
        panel.background = element_rect(fill='gray97'), panel.grid = element_line(colour = NA), panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=12), axis.title.x = element_blank(),axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=12, vjust=3.9), strip.text = element_text(size=14, face="bold"),
        legend.key = element_rect(fill='white'), legend.position="bottom", legend.text = element_text(size=12))+
  theme(plot.margin=unit(c(0.3,1,0.5,0.5), "cm"), axis.ticks.length.x =unit(.25, "cm"))+
  guides(color=guide_legend(title=expression(paste("Bait density"," (baits per km"^2 ,")"))))+
  labs(y = expression(atop("Probability of testing positive", "for biomarker")))+scale_color_manual(values = vars2)

#Fig 5
newgrid$Density<-as.factor(newgrid$Density)
mixed<-glmer(formula=cbind(Pos, Sum-Pos)~scale(Opossum)+as.factor(Density)*Season*Habitat+
               Raccoon+(1|Grid), 
             weights=Sum, data=newgrid, na.action=na.pass, family=binomial())
plot.df5<-emmip(mixed, ~Season|Habitat|Density, type="response", plotit=FALSE)
ggplot(plot.df5, aes(x=Density, y=yvar, color=Habitat))+
  geom_point(size=3)+geom_errorbar(aes(ymin=yvar-SE, ymax=yvar+SE), data=plot.df5, width=0.4)+facet_grid(Season~Habitat)+
  theme(strip.background=element_rect(color="black", fill="darkslategray3"), panel.border = element_rect(colour='black', fill='NA'), 
        panel.background = element_rect(fill='gray97'), panel.grid = element_line(colour = NA), panel.spacing = unit(1, "lines"),
        legend.key = element_rect(fill='white'), legend.position="none", legend.text=element_text(size=14),
        axis.text=element_text(size=14), axis.text.x = element_text(size=14), axis.title.x = element_text(size=14, vjust=1),
        axis.title.y = element_text(size=14, vjust=3.9), strip.text = element_text(size=14, face="bold"))+
  theme(plot.margin=unit(c(0.3,0.3,0.5,0.5), "cm"), axis.ticks.length.x =unit(.25, "cm"))+
  labs(y = expression(atop("Estimated proportion of raccoons","positive for biomarker")))+
  labs(x = expression(paste("Bait density"," (baits per km"^2 ,")")))+scale_color_manual(values=vars)