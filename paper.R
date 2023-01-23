# Proyecto de Semestre 
# Economía Ambiental
# Script Reducido para modelos a utilizar en paper


# Preliminares ------------------------------------------------------------


# Cargar librerías
library(sandwich) # HTK para stargazer
library(lmtest) # Para coeftest
library(tidyverse) # Varios incluyendo ggplot
library(broom) # Para limpiar con tidy
library(car) # Para pruebas de Wald (htk robust)
library(stargazer) # Para exportar a latex y text
library(openxlsx) # Para exportar a excel

# Cargar la base de datos
df<-read.csv('df.csv')
df$year<-as.character(df$year) # Lee como texto para hacer dummy de año
df_03<-subset(df, df$year=='2003') # Base solo para 2003
df_07<-subset(df, df$year=='2007') # Base solo para 2007
df$lgdp<-log(df$gdp_pc)
df$lcer<-log(df$cer_yield)

# Modelos -----------------------------------------------------------------

# Modelo Base con POLS: estimador de diferencias en diferencias

mdl_base<-lm(co2_pc~year*kyoto,data=df) # Modelo simple sin heterocedasticidad
summary(mdl_base)

mdlh_base<-coeftest(mdl_base,vcov=hccm(mdl_base,type='hc1')) # Modelo con HTC, Stata
print(mdlh_base)

linearHypothesis(mdl_base,c('year2007','kyoto','year2007:kyoto'),vcov=hccm)

# Modelo estima cual es el cambio en las emisiones per capita debido a la participación
# en el acuerdo de kyoto, y el año
# El estimador diff in diff es la interacción entre año y kyoto
# Muestra si existió una diferencia en emisiones entre esos años en países que 
# Hubo participación en el acuerdo
# La diferencia en el tiempo de las diferencias de emisiones entre los países 
# que participaron en Kyoto y los que no
# No estaría completamente bien inmediatamente concluir esto. Modelos más complejos

# Modelo 2: Con estimación de la curva de kuznets

# Agregamos el logaritmo del PIBPC cuadrático, para un cambio proporcional creciente

mdl_kuz<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T),data=df) 
summary(mdl_kuz)

mdlh_kuz<-coeftest(mdl_kuz,vcov=hccm(mdl_kuz,type='hc1'))
print(mdlh_kuz)

# Es significativa el término cuadrático, lo que implica un efecto parcial
# No lineal del PIBPC en las emisiones de co2 per capita
# Sin embargo, no es igual a la curva de Kuznets. un primer beta negativo, 
# y el siguiente positivo, implica que a mayor pib mayor emisiones pc, 
# kuznets estima relación de forma diferente (alreves)


# Modelo sin formas funcionales complejas, solamente cuadrático y diff in diff
mdl_fn<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
           +ind_work+pop_g+energy_gdp+natrent+forrent+fossil_use
           +impfuel+log(cer_yield)+agr_emp,data=df)
summary(mdl_fn)

mdlh_fn<-coeftest(mdl_fn,vcov=hccm(mdl_fn,type='hc1'))
print(mdlh_fn)

# Modelo con interacciones varias, se vuelve importante year y algunas interacciones
# de kyoto

mdl_inter<-lm(co2_pc~year*(kyoto+energy_gdp)+kyoto*(oilrent+serv_emp+ind_work)
             +poly(log(gdp_pc),2,raw=T)
             +fossil_use*(pop_g+forrent)+impfuel
             +log(cer_yield)+energy_gdp*ind_work
             ,data=df)
summary(mdl_inter)

mdlh_inter<-coeftest(mdl_inter,vcov=hccm(mdl_inter,type='hc1'))
print(mdlh_inter)


# Stargazer --------------------------------------------------------

# Errores para stargazer de todos los modelos 

cov1<-vcovHC(mdl_base,type='HC1')
robust1<-sqrt(diag(cov1))

cov2<-vcovHC(mdl_kuz, type='HC1')
robust2<-sqrt(diag(cov2))

cov3<-vcovHC(mdl_fn, type='HC1')
robust3<-sqrt(diag(cov3))

cov4<-vcovHC(mdl_inter, type='HC1')
robust4<-sqrt(diag(cov4))

# Nombres de variables
xlab<-c('Intercepto',
        'Dummy de Año 2007',
        'Participación en Acuerdo de Kyoto',
        'Ln GDP per capita (2017 PPP)',
        'Ln GDPPC Cuadrado',
        'Empleo en Industria (% del Empleo Total)',
        'Tasa de Crecimiento de la Población',
        'Uso de Energía PIB',
        'Rentas de Recursos Naturales',
        'Rentas Petroleras',
        'Empleo en Servicios',
        'Rentas Forestales',
        'Uso de Combustibles Fósiles',
        'Importaciones de Combustible',
        'Ln Rendimiento de Cultivos Cereales',
        'Empleo en Agricultura',
        'Estimador de diferencias en diferencias',
        'Interación Año y Uso de Energía por PIB',
        'Interacción Kyoto y Rentas Petroleras',
        'Interacción Kyoto y Empleo en Servicios',
        'Interacción Kyoto y Empleo en Industria',
        'Interacción Uso de Comb. Fósiles y Tasa de crecimiento pob.',
        'Interacción Uso de Comb. Fósiles y Rentas Forestales',
        'Interacción Uso de Energía por PIB y Empleo en Industria')

# Tabla stargazer para los 4 modelos
stargazer(mdl_base,mdl_kuz,mdl_fn,mdl_inter, type='text',
          dep.var.labels = 'Emisiones de CO2 per capita',
          covariate.labels=xlab,
          se=list(robust1,robust2,robust3,robust4),
          omit.stat = c('rsq','f','ser'),
          dep.var.caption = "", 
          intercept.bottom = F, 
          intercept.top = T)

# Tabla stargazer descriptiva

# Primero una base reducida de las variables continuas

df_red<-select(df,co2_pc, gdp_pc,lgdp,ind_work,pop_g,energy_gdp,natrent,forrent,
               oilrent,serv_emp,fossil_use,impfuel,cer_yield,lcer,agr_emp)

# Nombres de variables


xlab_des<-c('Emisiones de CO2 per capita (kt)',
        'PIB per cápita (2017 PPP)',
        'Ln GDP per capita (2017 PPP)',
        'Empleo en Industria (% del Empleo Total)',
        'Tasa de Crecimiento de la Población (Delta% (t-t-1))',
        'Uso de energía (kg) por cada 1K dólares de PIBPC ',
        'Rentas de Recursos Naturales (% PIB)',
        'Rentas Forestales (% PIB)',
        'Rentas Petroleras (% PIB)',
        'Empleo en Servicios (% del Empleo Total)',
        'Uso de Combustibles Fósiles (% del uso de energía)',
        'Importaciones de Combustible (% de importaciones mercantiles)',
        'Rendimiento de Cultivos Cereales (Kg por hta.)',
        'Ln Rendimiento de Cultivos Cereales',
        'Empleo en Agricultura (% del PIB)')


stargazer(df_red,
          title='Estadística Descriptiva de las variables continuas', 
          #type='text',
          covariate.labels=xlab_des,
          summary.stat = c('n','mean','min','p25','median','p75','max'))

stargazer(df_red,
          title='Estadística Descriptiva de las variables continuas', 
          #type='text',
          covariate.labels=xlab_des,
          summary.stat = c('n','mean','min','p25','median','p75','max'))






# CSV -------------------------------------------------------------------

# Exportar modelos con heterocedasticidad a un Excel

write.table(tidy(mdlh_base),'models.csv', col.names=T,sep=',', row.names = F)
write.table(tidy(mdlh_kuz),'models.csv', col.names=T,sep=',',append=T, row.names = F)
write.table(tidy(mdlh_fn),'models.csv', col.names=T,sep=',',append=T, row.names = F)
write.table(tidy(mdlh_inter),'models.csv', col.names=T,sep=',',append=T, row.names = F)



