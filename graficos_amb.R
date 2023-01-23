# Proyecto de Semestre 
# Economía Ambiental
# Gráficos

# Librerías
library(tidyverse) # ggplot2 y otros
library(lmtest)
library(car)

# Cargar base
df<-read.csv('df.csv')
df$year<-as.character(df$year) # para que año sea un string
df$Kyoto<-ifelse(df$kyoto==1,"Participa", "No Participa")

# Boxplots

# Boxplot de Emisiones PC por año y kyoto

co2_year<-ggplot(df,aes(x=year,y=co2_pc, fill=Kyoto))+
          geom_boxplot(outlier.size = 1, width=0.5, outlier.shape = 1)+
          scale_y_continuous(limits=quantile(df$co2_pc, c(0.1,0.95),na.rm=T))+
          xlab('Año')+ylab('Emisiones de CO2 per cápita (tm)')+theme_bw()+
          theme(legend.position = 'bottom',
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank())+
          labs(fill='Acuerdo de Kyoto')
co2_year

# Boxplot de Emisiones PC por año y región

co2_reg<-ggplot(df,aes(x=region,y=co2_pc, fill=year))+
  geom_boxplot(outlier.size = 1, width=0.5, outlier.shape = 1)+
  scale_y_continuous(limits=quantile(df$co2_pc, c(0.1,0.95),na.rm=T))+
  xlab('Región (BM)')+ylab('Emisiones de CO2PC (tm)')+theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x =element_text(angle=15, hjust=1,vjust=1, size=7))+
  labs(fill='Año')
co2_reg

# Boxplot de IMPORTACIONES DE COMBUSTIBLE

co2_reg<-ggplot(df,aes(x=region,y=impfuel, fill=year))+
  geom_boxplot(outlier.size = 1, width=0.5, outlier.shape = 1)+
  scale_y_continuous(limits=quantile(df$co2_pc, c(0.1,1),na.rm=T))+
  xlab('Región (BM)')+ylab('Importaciones de Combustible')+theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x =element_text(angle=15, hjust=1,vjust=1, size=7))+
  labs(fill='Año')
co2_reg


# Histogramas

# Histograma de PIB per capita
gdp_hist<-ggplot(df, aes(x=gdp_pc))+
          geom_histogram(binwidth = 2000, fill='#3c89d0', colour='black')+
          xlab('PIB per Capita (2017 PPP)')+ylab('Frecuencia')+
            theme_bw()
gdp_hist

# Histograma del PIB en miles
df$gdpK<-df$gdp_pc/1000

lgdp_hist<-ggplot(df, aes(x=gdpK))+
           geom_histogram(fill='#3c89d0', colour='black')+
           xlab('PIBPC (Miles de dólares 2017 PPP)')+ylab('Frecuencia')+
           theme_bw()+
           scale_x_continuous(breaks=c(0,2,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
lgdp_hist


# Scatter plot co2_pc vs log(gdp_pc)

co2_gdp<-ggplot(df,aes(x=log(gdp_pc),y=co2_pc))
co2_gdp+geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Ln GDP per Capita (2017 PPP Dollars)')+
  ylab('Emisiones de CO2 per cápita (tm)')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
# Regresión:
md_co2gdp<-lm(co2_pc~log(gdp_pc), data=df)
summary(md_co2gdp)
coeftest(md_co2gdp,vcov=hccm)


# Scatter plot co2_pc vs natr
co2_natr<-ggplot(df,aes(x=agr_emp,y=co2_pc))
co2_natr+geom_point()+stat_smooth(method=lm, se=F)+theme_bw()+
  xlab('Empleo en Agricultura (% de la Fuerza Laboral)')+
  ylab('Emisiones de CO2 per cápita (tm)')+guides(colour=guide_legend(title=NULL))+
  theme(legend.position='bottom', 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Regresión:
md_co2natr<-lm(co2_pc~agr_emp, data=df)
summary(md_co2natr)
coeftest(md_co2natr,vcov=hccm)

# Graficar efectos parciales cuadráticos
partial_graph<-read.xlsx('partial graph.xlsx')
graph_pe<-ggplot(partial_graph,aes(x=log(gdp_pc),y=fit,colour=factor(year)))+
          geom_line(size=0.8)+theme_bw()+
          theme(legend.position='bottom')+
          labs(colour='Año', x='Ln PIB PC (2017 PPP)', y='Emisiones CO2 Estimadas')
graph_pe




