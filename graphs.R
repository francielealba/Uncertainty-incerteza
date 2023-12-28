install.packages("ggthemes")
install.packages("patchwork")

require(ggplot2)
require(ggthemes)
require(dplyr)
library(patchwork)


##volume

graphs <- read.table("C:/Users/frann/OneDrive/Tese/Doutorado/Processamento Cunia - tese/Tese/graficos.txt", header = T)

all <- graphs[graphs$clas=="all",]
clas <- graphs[c(-1,-5,-9),]

sp <- data.frame(spline(all$r, all$under, n = 15*nrow(all)))

windowsFonts(A = windowsFont("Times New Roman"))  


allv <- ggplot(all,aes(x=r, y= under))+
  geom_line(data=sp, aes(x=x,y=y))+
  geom_point()+
  labs(x="R?", y="underestimated uncertainty %", title = "Volume")+
  scale_y_continuous(breaks = seq(0,80,10))+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"), 
        panel.border = element_rect(color="black",
                                    fill = NA))
##volume
clas098 <-clas[clas$r==0.98,]
clas080 <-clas [clas$r==0.80,]
clas050 <-clas [clas$r==0.50,]

clas$rc <-as.character(clas$r)
clas$rc

d <-ggplot(clas,aes(x=clas, y= under, group=rc, color=rc))+
  geom_line()+
  geom_point()+
  labs(x="Variability classes", y="underestimated uncertainty %", title = "Volume")+
  scale_y_continuous(breaks = seq(0,80,10))+
  scale_color_manual(name = "R?", 
                     values = c("#DE6065FF","steelblue","darkgreen"),
                     labels = c("0.50", "0.80", "0.98"))+
   theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"), 
        panel.border = element_rect(color="black",
                                    fill = NA),
        legend.background = element_rect(fill = "white"))

clasbio$rc <-as.character(clasbio$r)

d+e
##biomass
e <-ggplot(clasbio,aes(x=clas, y= under, group=rc, color=rc))+
  geom_line()+
  geom_point()+
  labs(x="Variability classes", y="underestimated uncertainty %", title="Biomass")+
  scale_y_continuous(breaks = seq(0,80,10))+
  scale_color_manual(name = "R?", 
                     values = c("#DE6065FF","steelblue","darkgreen"),
                     labels = c("0.50", "0.80", "0.98"))+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"), 
        panel.border = element_rect(color="black",
                                    fill = NA))
#Barras
##volume

modelo <-clas$under
inv <- 100-modelo
r1v<-clas$r
runi<-as.character(union_all(clas$r,r1v))
cv<-clas$clas
c1v <-clas$clas
cuni <-union_all(cv,c1v)
incer<-union_all(modelo,inv)
fac <- (c("Model","Model","Model","Model","Model","Model","Model","Model","Model","Design","Design","Design","Design","Design","Design","Design","Design","Design"))
duni <-data.frame(runi,cuni,fac,incer)

r098 <- duni[duni$runi=="0.98",]

l <- ggplot(r098, aes(x=cuni, y=incer, fill = fac)) + 
  geom_col()+
  labs(x="Variability Classes",
       y="Uncertainty %",
       subtitle="Volume",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

r080 <- duni[duni$runi=="0.8",]

M <- ggplot(r080, aes(x=cuni, y=incer, fill = fac)) + 
  geom_col()+
  labs(x="Variability Classes",
       y="Uncertainty %",
       subtitle="R? 0.80",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

r050 <- duni[duni$runi=="0.5",]

N <- ggplot(r050, aes(x=cuni, y=incer, fill = fac)) + 
  geom_col()+
  labs(x="Variability Classes",
       y="Uncertainty %",
       subtitle="R? 0.50",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA)) 

grafv <-l+M+N
grafv +plot_annotation(title = 'Volume',
                theme = theme(plot.title = element_text(size = 16)))& 
                theme(text = element_text(family ="A"))&
                theme(plot.title = element_text(hjust=0.5))
  
##biomassa
modelobio <-clasbio$under
invbio <- 100-modelobio
r1<-clasbio$r
runibio <-as.character(union_all(clasbio$r,r1))
c<-clasbio$clas
c1 <-clasbio$clas
cunibio <-union_all(c,c1)
incerbio<-union_all(modelobio,invbio)
facbio <- (c("Model","Model","Model","Model","Model","Model","Model","Model","Model","Design","Design","Design","Design","Design","Design","Design","Design","Design"))
dunibio <-data.frame(runibio,cunibio,facbio,incerbio)

r098bio <- dunibio[dunibio$runibio=="0.95",]

i <- ggplot(r098bio, aes(x=cunibio, y=incerbio, fill = facbio)) + 
  geom_col()+
  labs(x="Variability Classes",
       y="Uncertainty %",
       subtitle="Biomass",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        #legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

l+i

r080bio <- dunibio[dunibio$runibio=="0.8",]

j <- ggplot(r080bio, aes(x=cunibio, y=incerbio, fill = facbio)) + 
  geom_col()+
  labs(x="Variability Classes",
       y="Uncertainty %",
       subtitle="R? 0.80",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

r050bio <- dunibio[dunibio$runibio=="0.5",]

k <- ggplot(r050bio, aes(x=cunibio, y=incerbio, fill = facbio)) + 
  geom_col()+
  labs(x="Variability Classes",
       y="Uncertainty %",
       subtitle="R? 0.50",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA)) 

graf <-i+j+k

graf +plot_annotation(title = 'Biomass',
                       theme = theme(plot.title = element_text(size = 16)))& 
  theme(text = element_text(family ="A"))&
  theme(plot.title = element_text(hjust=0.5))

#intervalo de confian?a##

##volume
(q <- qt(0.975, 18-1))

exgrafico <- read.table("C:/Doutorado/Processamento Cunia - tese/Tese/iceq.txt", header = T)

qI <-qt(0.975, 33-1)
qII <-qt(0.975, 43-1)
qIII <-qt(0.975, 18-1)

clas$q <- c(qI,qII,qIII,qI,qII,qIII,qI,qII,qIII)
clas$ICAA <-sqrt(clas$first)*clas$q
clas$media <-as.numeric(clas$mean)
clas098 <-clas[clas$r==0.98,]
clas080 <-clas [clas$r==0.80,]
clas050 <-clas [clas$r==0.50,]


a <-ggplot(clas098) + 
  geom_errorbar(aes(x = clas, ymin=media-ic, ymax=media+ic, color = "Icc"), width=.1) +
  geom_errorbar(aes(x = clas, ymin=media-ICAA, ymax=media+ICAA, color = "Ica"), width=.1) +
  geom_point(aes(x=clas,y=media))+
  labs(x="Variability Classes",
       y="Volume (m?/ha)",
       subtitle="R? 0.98") +
  theme_classic() +
  scale_color_manual(name = "Legenda", values = c("Icc" = "black", "Ica" = "red")) +
  theme(legend.position="none")+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13))
        

b <-ggplot(clas080) + 
  geom_errorbar(aes(x = clas, ymin=media-ic, ymax=media+ic, color = "Icc"), width=.1) +
  geom_errorbar(aes(x = clas, ymin=media-ICAA, ymax=media+ICAA, color = "Ica"), width=.1) +
  geom_point(aes(x=clas,y=media))+
  labs(x="Variability Classes",
       y="Volume (m?/ha)",
       subtitle="R? 0.80") +
  theme_classic() +
  scale_color_manual(name = "Legenda", values = c("Icc" = "black", "Ica" = "red")) +
  theme(legend.position="none")+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13))


c <-ggplot(clas050) + 
  geom_errorbar(aes(x = clas, ymin=media-ic, ymax=media+ic, color = "Icc"), width=.1) +
  geom_errorbar(aes(x = clas, ymin=media-ICAA, ymax=media+ICAA, color = "Ica"), width=.1) +
  geom_point(aes(x=clas,y=media))+
  labs(x="Variability Classes",
       y="Volume (m?/ha)",
       subtitle="R? 0.50") +
  theme_classic() +
  scale_color_manual(name = "", values = c("Ic" = "black", "Ica" = "red")) +
  theme(legend.position = "right")+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=10),
        axis.title = element_text(size = 12))
a+b+c



###biomass

graphsbio <- read.table("C:/Users/frann/OneDrive/Tese/Doutorado/Processamento Cunia - tese/Tese/resimulbiom.txt", header = T)

allbio <- graphsbio[graphsbio$clas=="all",]
clasbio <- graphsbio[c(-1,-5,-9),]


spbio <- data.frame(spline(allbio$r, allbio$under, n = 15*nrow(allbio)))

windowsFonts(A = windowsFont("Times New Roman"))  


allb <-ggplot(allbio,aes(x=r, y= under))+
  geom_line(data=spbio, aes(x=x,y=y))+
  geom_point()+
  labs(x="R?", y="Underestimated uncertainty %", title="Biomass")+
  scale_y_continuous(breaks = seq(0,80,10))+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"), 
        panel.border = element_rect(color="black",
                                    fill = NA))


ggplot(clas,aes(x=clas, y= under, group=r))+
  geom_line()+
  geom_point()+
  labs(x="Variability classes", y="Model uncertainty %")+
  scale_y_continuous(breaks = seq(0,80,10),labels = scales::percent)+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"), 
        panel.border = element_rect(color="black",
                                    fill = NA))

modelo <-clas$under
inv <- 100-modelo
r<-clas$r
r1<-clas$r
runi <-union_all(r,r1)
c<-clas$clas
c1 <-clas$clas
cuni <-union_all(c,c1)
incer<-union_all(modelo,inv)
fac <- (c("m","m","m","m","m","m","m","m","m","i","i","i","i","i","i","i","i","i"))
duni <-data.frame(runi,cuni,fac,incer)

r098 <- duni[duni$runi=="0.98",]

ggplot(r098, aes(x=cuni, y=incer)) + 
  geom_col(aes(fill = fac))

+
  scale_fill_viridis_d() 

+
  scale_y_continuous(labels = scales::percent)



##intervalo de confian?a##
(q <- qt(0.975, 18-1))

exgrafico <- read.table("C:/Doutorado/Processamento Cunia - tese/Tese/iceq.txt", header = T)

qI <-qt(0.975, 33-1)
qII <-qt(0.975, 43-1)
qIII <-qt(0.975, 18-1)

clasbio$q <- c(qI,qII,qIII,qI,qII,qIII,qI,qII,qIII)
clasbio$ICAA <-sqrt(clasbio$first)*clasbio$q
clasbio$media <-as.numeric(clasbio$media)
clas098bio <-clasbio[clasbio$r==0.95,]
clas080bio <-clasbio[clasbio$r==0.80,]
clas050bio <-clasbio [clasbio$r==0.50,]


Fe <-ggplot(clas098bio) + 
  geom_errorbar(aes(x = clas, ymin=media-ic, ymax=media+ic, color = "Icc"), width=.1) +
  geom_errorbar(aes(x = clas, ymin=media-ICAA, ymax=media+ICAA, color = "Ica"), width=.1) +
  geom_point(aes(x=clas,y=media))+
  labs(x="Variability Classes",
       y="Biomass (kg/ha)",
       subtitle="R 0.95") +
  theme_classic() +
  scale_color_manual(name = "Legenda", values = c("Icc" = "black", "Ica" = "red")) +
  theme(legend.position="none")+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13))


g <-ggplot(clas050bio) + 
  geom_errorbar(aes(x = clas, ymin=media-ic, ymax=media+ic, color = "Icc"), width=.1) +
  geom_errorbar(aes(x = clas, ymin=media-ICAA, ymax=media+ICAA, color = "Ica"), width=.1) +
  geom_point(aes(x=clas,y=media))+
  labs(x="Variability Classes",
       y="Biomass (Kg/ha)",
       subtitle="R? 0.80") +
  theme_classic() +
  scale_color_manual(name = "Legenda", values = c("Icc" = "black", "Ica" = "red")) +
  theme(legend.position="none")+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13))


h <-ggplot(clas080bio) + 
  geom_errorbar(aes(x = clas, ymin=media-ic, ymax=media+ic, color = "Icc"), width=.1) +
  geom_errorbar(aes(x = clas, ymin=media-ICAA, ymax=media+ICAA, color = "Ica"), width=.1) +
  geom_point(aes(x=clas,y=media))+
  labs(x="Variability Classes",
       y="Biomass (kg/ha)",
       subtitle="R? 0.50") +
  theme_classic() +
  scale_color_manual(name = "", values = c("Ic" = "black", "Ica" = "red")) +
  theme(legend.position = "right")+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=10),
        axis.title = element_text(size = 12))

Fe+g+h

#Resultados

##percentage underestimate variance in relation R?

allv+allb

##Icbiomass
Fe+g+h

##icvolume
a+b+c

##volume e biomassa all r? 0.98

AllAll <- read.table("C:/Users/frann/OneDrive/Tese/Doutorado/Processamento Cunia - tese/Tese/allall.txt", header = T)

allallall <- ggplot(AllAll, aes(x=Var, y=under, fill = phase)) + 
  geom_col()+
  labs(x="",
       y="Uncertainty %",
       subtitle="",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA)) 

####contribui??o de cada derivada para a incerteza#####

bioderi <- read.table("C:/Users/frann/OneDrive/Tese/Doutorado/Processamento Cunia - tese/Tese/bioderi.txt", header = T)
derivol <- read.table("C:/Users/frann/OneDrive/Tese/Doutorado/Processamento Cunia - tese/Tese/derivol.txt", header = T)



deribio <-ggplot(bioderi,aes(x=Deri, y= pri, group=sig, color=sig))+
  geom_line()+
  geom_point()+
  labs(x="Variability classes", y="uncertainty kg/ha", title = "Biomass")+
  #scale_y_continuous(breaks = seq(0,80,10))+
  #scale_color_manual(name = "R?", 
                    # values = c("#DE6065FF","steelblue","darkgreen"),
                     #labels = c("0.50", "0.80", "0.98"))+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"), 
        panel.border = element_rect(color="black",
                                    fill = NA),
        legend.background = element_rect(fill = "white"))


derivolg <-ggplot(derivol,aes(x=Deri, y= uncer, group=sig, color=sig))+
  geom_line()+
  geom_point()+
  labs(x="", y="uncertainty m/ha", title = "Volume")+
  #scale_y_continuous(breaks = seq(0,80,10))+
  #scale_color_manual(name = "R?", 
  # values = c("#DE6065FF","steelblue","darkgreen"),
  #labels = c("0.50", "0.80", "0.98"))+
  theme(text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"), 
        panel.border = element_rect(color="black",
                                    fill = NA),
        legend.background = element_rect(fill = "white"))


prim <- derivol[derivol$Deri=="prim",]
sec <- derivol[derivol$Deri=="sec",]
derivol<- rbind(prim,sec)
derivolg4 <- derivol[derivol$Group=="G4",]

l <- ggplot(derivol, aes(x=sig, y=uncer, fill = forcats::fct_rev(Deri))) + 
  geom_col()+
  labs(x="",
       y="Uncertainty (m≥/ha)",
       subtitle="Volume",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        #legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))


prim <- bioderi[bioderi$Deri=="prim",]
sec <- bioderi[bioderi$Deri=="sec",]
deribio<- rbind(prim,sec)
deribio$prit <-sqrt(deribio$pri)

lm <- ggplot(deribio, aes(x=sig, y=prit, fill = forcats::fct_rev(Deri))) + 
  geom_col()+
  labs(x="",
       y="Uncertainty (kg/ha)",
       subtitle="Biomass",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        #legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

data("movielens")
View(movielens)

 l+lm

log(1024, base = exp(4))

class(movielens$genres)

nlevels(movielens$genres)


##########Chapter 4####################
chapterfour <- read.table("C:/Users/frann/OneDrive/Tese/Doutorado/Processamento Cunia - tese/Tese/chapterfour.txt", header = T)


ordem <- c("cen55", "cen54", "cen43", "cen42", "cen41", "cen3", "cen2","cen1", "model", "amotra")

chapterfour$cenarios <- factor(chapterfour$cenarios,
                               levels = ordem)

h <-ggplot(chapterfour %>% 
             mutate(cenarios = factor(ncenarios, levels= rev(ncenarios))), 
           aes(mean, cenarios)) + 
  geom_errorbarh(aes(xmin = mean-icnorm, xmax=mean+icnorm,  color= "IC Total"), size=1, height = 0.5) +
  geom_errorbarh(aes(xmin = mean-icinvme, xmax=mean+icinvme, color = "IC Design"), size=1.8, height=0.5) +
  geom_errorbarh(aes(xmin = mean-icmvme, xmax=mean+icmvme, color = "IC Design+model"), size=1, height= 0.9) +
  geom_point(aes(x=mean,y=cenarios), size = 2.5, color = "#f50f9d")+
  labs(x=expression("Volume (m"^3*".ha"^-1*")"),
       y="Scenarios",
       title = "Volume") +
  theme_classic() +
  scale_color_manual(name = "", values = c("#7fcdbb","#110478", "#a19d9d")) +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 14),
        legend.position = "none")
h

ICCH4 <-h+f

ggsave(filename = 'ICCH4.png', plot = ICCH4, width = 28, height = 14,
       units = 'cm', dpi = 800)

##########Biomassa################

biochfour <- read.table("C:/Users/frann/OneDrive/Tese/Doutorado/Processamento Cunia - tese/Tese/biochapterfour.txt", header = T)

biochfour$cenarios <- factor(chapterfour$cenarios,
                               levels = ordem)

f <-ggplot(biochfour %>% 
             mutate(cenarios = factor(ncenarios, levels= rev(ncenarios))), 
           aes(mean, cenarios)) + 
  geom_errorbarh(aes(xmin = mean-icnorm, xmax=mean+icnorm,  color= "IC Total"), size=1, height = 0.5) +
  geom_errorbarh(aes(xmin = mean-icinvme, xmax=mean+icinvme, color = "IC Design"), size=1.8, height=0.5) +
  geom_errorbarh(aes(xmin = mean-icmvme, xmax=mean+icmvme, color = "IC Design+model"), size=1, height= 0.9) +
  geom_point(aes(x=mean,y=cenarios), size = 2.5, color = "#f50f9d")+
  labs(x=expression("Biomass (Kg.ha"^-1*")"),
       y="Scenarios",
       title = "Biomass") +
  theme_classic() +
  scale_color_manual(name = "", values = c("#7fcdbb","#110478", "#a19d9d")) +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(family = "A"), 
        axis.text = element_text(size=12),
        axis.title = element_text(size = 14))



##############################

ncenarios <- c("Design", "Model", "Scenario 1", "Scenario 2", "Scenario 3",
              "Scenario 41", "Scenario 42", "Scenario 43", "Scenario 54", "Scenario 55")

i <-ggplot(chapterfour %>% 
             mutate(cenarios = factor(ncenarios, levels= rev(ncenarios))), 
           aes(mean, cenarios)) + 
    geom_errorbarh(aes(xmin = mean-icnorm, xmax=mean+icnorm,  color= "IC Total"), size=1, height = 0.5) +
    geom_errorbarh(aes(xmin = mean-icinvme, xmax=mean+icinvme, color = "IC Design"), size=2, height=0.3) +
    geom_errorbarh(aes(xmin = mean-icmvme, xmax=mean+icmvme, color = "IC Design+model"), size=1.4, height= 0.8) +
    geom_point(aes(x=mean,y=cenarios), size = 2, color = "#f50f9d")+
    labs(x=expression("Volume (m"^3*".ha"^-1*")"),
         y="Scenarios") +
    theme_classic() +
    scale_color_manual(name = "",values = c("#7fcdbb","#110478", "#a19d9d")) +
    theme(axis.text = element_text(size=12),
          text = element_text(family = "A"),
          axis.title = element_text(size = 14),
          legend.position = "bottom") +
geom_label(
  data = data.frame(
    mean = 345,
    scenarios = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 41",
                  "Scenario 42 ", "Scenario 43", "Scenario 54", "Scenario 55", "Model"),
   dif = chapterfour$dif[1:10],
    y = c(9,8, 7, 6, 5, 4, 3, 2, 1)
  ) %>% mutate(label= paste0('\nR$ ', round(dif,1))),
  aes(x = mean+c(-10, -10, -10, 0, -10, -10, -10, -10, -8), 
      y = y, label = label), hjust = 0, 
  vjust = 0.0, nudge_y = 0.01,
  size = 3,
  label.size = 0) +
  scale_x_continuous(limits = c(290, 350))

i


j <-ggplot(chapterfour %>% 
             mutate(cenarios = factor(ncenarios, levels= rev(ncenarios))), 
           aes(mean, cenarios)) + 
  geom_errorbarh(aes(xmin = mean-icnorm, xmax=mean+icnorm,  color= "IC Total"), size=1, height = 0.5) +
  geom_errorbarh(aes(xmin = mean-icinvme, xmax=mean+icinvme, color = "IC Design"), size=1.8, height=0.5) +
  geom_errorbarh(aes(xmin = mean-icmvme, xmax=mean+icmvme, color = "IC Design+model"), size=1, height= 0.9) +
  geom_point(aes(x=mean,y=cenarios), size = 2.5, color = "#f50f9d")+
  labs(x=expression("Volume (m"^3*".ha"^-1*")"),
       y="Scenarios") +
  theme_classic() +
  scale_color_manual(name = "", values = c("#7fcdbb","#110478", "#a19d9d")) +
  theme(axis.text = element_text(size=12),
        text = element_text(family = "A"),
        axis.title = element_text(size = 14),
        legend.position = "bottom") +
  geom_label(
    data = data.frame(
      mean = 345,
      scenarios = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 41",
                    "Scenario 42 ", "Scenario 43", "Scenario 54", "Scenario 55", "Model", "Design"),
      icmais = chapterfour$icmais[1:10],
      y = c(10,9,8, 7, 6, 5, 4, 3, 2, 1)
    ) %>% mutate(label= paste0( '\nR$ ', round(icmais))),
    aes(x = mean+c(-10,-10,-10, -10, 0, -10, -10, -10, -10, -8), 
        y = y, label = label), hjust = 0, 
    vjust = 0.2, nudge_y = 0.01,
    size = 3,
    label.size = 0) +
  scale_x_continuous(limits = c(280, 370))+
  geom_label(
    data = data.frame(
      mean = 295,
      scenarios = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 41",
                    "Scenario 42 ", "Scenario 43", "Scenario 54", "Scenario 55", "Model", "Design"),
      icmenos = chapterfour$icmenos[1:10],
      y = c(10,9,8, 7, 6, 5, 4, 3, 2, 1)
    ) %>% mutate(label= paste0( '\nR$ ', round(icmenos))),
    aes(x = mean-c(1,1,1,2,11, 2, 2, 2, 2,3), 
        y = y, label = label), hjust = 0, 
    vjust = 0.2, nudge_y = 0.01,
    size = 3,
    label.size = 0) +
  geom_label(
    data = data.frame(
      mean = 318.399,
      scenarios = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 41",
                    "Scenario 42 ", "Scenario 43", "Scenario 54", "Scenario 55", "Model", "Design"),
      media = chapterfour$meanst.1[1:10],
      y = c(10)
    ) %>% mutate(label= paste0( '\nR$ ', round(media))),
    aes(x = mean[1], 
        y = y[1], label = label[1]), hjust = 0.5, 
    vjust = -0.2, nudge_y = 0.01,
    size = 3,
    label.size = 0)+
  geom_label(
    data = data.frame(
      mean = 355,
      scenarios = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 41",
                    "Scenario 42 ", "Scenario 43", "Scenario 54", "Scenario 55"),
      dif = chapterfour$dif[3:10],
      y = c(8, 7, 6, 5, 4, 3, 2, 1)
    ) %>% mutate(label= paste0(scenarios, '\nR$ ', round(dif,2))),
    aes(#x = mean+c(-10, -10, -10, 0, -10, -10, -10, -10, -8), 
        y = y, label = label), hjust = 0, 
    vjust = 0.5, nudge_y = 0.01,
    size = 3,
    label.size = 0)

j+i


getwd()
ggsave(filename = 'figcustos.png', plot = j, width = 22, height = 14,
       units = 'cm', dpi = 800)

?paste0


#######Gr√°ficos ch3###########

library("readr")
write_delim(derivolg4, "derivolg4.txt", delim = "\t")

write_delim(derivolg4, "deriG4VOL.txt", delim = "\t")


derivol<- rbind(prim,sec)
derivolg4 <- derivol[derivol$Group=="G4",]

modeltypes <- c("Spurr", "Schumacher-Hall", "Spurr", "Schumacher-Hall")
perc <- c(93, 84,7, 16)

derivolg4 <-data.frame(derivolg4, modeltypes, perc)

pit <- ggplot(derivolg4, aes(x=modeltypes, y=uncer, fill = forcats::fct_rev(Deri))) + 
  geom_col()+
  labs(x="",
       y="Uncertainty (m¬≥/ha)¬≤",
       #subtitle="Volume",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        #legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

pitporc <- ggplot(derivolg4, aes(x=modeltypes, y=perc, fill = forcats::fct_rev(Deri))) + 
  geom_col()+
  labs(x="",
       y="Uncertainty (%)",
       #subtitle="Volume",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        #legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

deriv <- pit+pitporc
fig <- deriv + plot_annotation(
  title = 'Volume') & 
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave(filename = 'figderivol.png', plot = fig, width = 18, height = 9,
       units = 'cm', dpi = 800)




First <- bioderi[bioderi$Deri=="prim",]
Sec <- bioderi[bioderi$Deri=="sec",]
deribio <- rbind(First,Sec)
derivolg4 <- deribio[deribio$Group=="G4",]
derivolg4$prit <-sqrt(derivolg4$pri)

modeltypes <- c("Spurr", "Schumacher-Hall", "Spurr", "Schumacher-Hall")
perc <- c(83.19, 59.21, 16.81, 40.79)

deribiog4 <-data.frame(derivolg4, modeltypes, perc)


bioha <- ggplot(deribiog4, aes(x=modeltypes, y=prit, fill = forcats::fct_rev(Deri))) + 
  geom_col()+
  labs(x="",
       y="Uncertainty (kg/ha)≤",
       subtitle="Biomass",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        #legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))

biohaperc <- ggplot(deribiog4, aes(x=modeltypes, y=perc, fill = forcats::fct_rev(Deri))) + 
  geom_col()+
  labs(x="",
       y="Uncertainty %",
       subtitle="Biomass",
       fill="") +
  scale_fill_brewer(palette="Accent") +
  theme(text = element_text(family = "A"),
        #legend.position="none",
        axis.text = element_text(size=12),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color="black",
                                    fill = NA))



bioderi <- bioha+biohaperc

figbiomass <- bioderi + plot_annotation(
  title = 'Biomass') & 
  theme(plot.title = element_text(hjust = 0.5)) 



sqrt(19868980)
