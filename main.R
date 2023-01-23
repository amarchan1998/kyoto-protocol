# Proyecto de Semestre 
# Economía Ambiental
# Script Principal de Modelos Empíricos

# Cargar librerías
library(sandwich) # HTK para stargazer
library(lmtest) # Para coeftest
library(car) # Para pruebas de Wald (htk robust)
library(openxlsx) # Para exportar a excel

# Cargar base

df<-read.csv('df.csv')

# Leer al año como texto para que funcione como factor

df$year<-as.character(df$year)

# Modelo Base: Emisiones PC con dummy de año, participación

mdl_base<-lm(co2_pc~year*kyoto, data=df)
summary(mdl_base)

mdlh_base<-coeftest(mdl_base,vcov=hccm(mdl_base,type='hc1'))
print(mdlh_base)

# Modelo No. 1: Considerando PIB per cápita y población

mdl_ec<-lm(co2_pc~year*kyoto+log(gdp_pc)+log(pop), data=df)
summary(mdl_ec)

mdlh_ec<-coeftest(mdl_ec,vcov=hccm(mdl_ec,type='hc1'))
print(mdlh_ec)

# Población no es significativo

# Modelo 2: Considerando crecimiento del PIB respecto a los últimos 5 años

mdl_ec1<-lm(co2_pc~year*kyoto+log(gdp_pc)+gdp_g5, data=df)
summary(mdl_ec1)

mdlh_ec1<-coeftest(mdl_ec1,vcov=hccm(mdl_ec1,type='hc1'))
print(mdlh_ec1)

# Modelo 3: Considerando desempleo

mdl_ec2<-lm(co2_pc~year*kyoto+log(gdp_pc)+gdp_g5+unem, data=df)
summary(mdl_ec1)

mdlh_ec2<-coeftest(mdl_ec2,vcov=hccm(mdl_ec2,type='hc1'))
print(mdlh_ec2)

# Modelo 4: Considerando relación cuadrática del PIB (curva de kuznets)

mdl_kz1<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)+gdp_g5+unem, data=df)
summary(mdl_kz1)

mdlh_kz1<-coeftest(mdl_kz1,vcov=hccm(mdl_kz1,type='hc1'))
print(mdlh_kz1)

# Pierde significancia el desempleo

# Modelo 5: Considerando exportaciones de combustible

mdl_expfuel<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
            +gdp_g5+expfuel, data=df)
summary(mdl_expfuel)

mdlh_expfuel<-coeftest(mdl_expfuel,vcov=hccm(mdl_expfuel,type='hc1'))
print(mdlh_expfuel)


# Modelo 6: Considerando % de Trabajo en Industria

mdl_ind1<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
               +gdp_g5+expfuel+ind_work, data=df)
summary(mdl_ind1)

mdlh_ind1<-coeftest(mdl_ind1,vcov=hccm(mdl_ind1,type='hc1'))
print(mdlh_ind1)

# Se pierde la tendencia de crecimiento

# Modelo 7: Considerando exportaciones minerales

mdl_exp1<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
             +expfuel+ind_work+minexp, data=df)
summary(mdl_exp1)

mdlh_exp1<-coeftest(mdl_exp1,vcov=hccm(mdl_exp1,type='hc1'))
print(mdlh_exp1)

# Modelo 8: Surface, densidad poblacionaL y crecimiento poblacional

# Sin el logaritmo

mdl_ppl1<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
             +expfuel+ind_work+minexp+denspop+surface+pop_g, data=df)
summary(mdl_ppl1)

mdlh_ppl1<-coeftest(mdl_ppl1,vcov=hccm(mdl_ppl1,type='hc1'))
print(mdlh_ppl1)

# Con el logaritmo

mdl_ppl2<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
            +expfuel+ind_work+minexp+log(denspop)+log(surface)+pop_g, data=df)
summary(mdl_ppl2)

mdlh_ppl2<-coeftest(mdl_ppl2,vcov=hccm(mdl_ppl2,type='hc1'))
print(mdlh_ppl2)

# Modelo 9: Considerando el uso de energía por PIB y la electricidad renovable

mdl_energy1<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
             +expfuel+ind_work+pop_g+energy_gdp+elect_rnw, data=df)
summary(mdl_energy1)

mdlh_energy1<-coeftest(mdl_energy1,vcov=hccm(mdl_energy1,type='hc1'))
print(mdlh_energy1)

# Murió minexp, se queda expfuel

# Modelo 10 con todas las rentas de recursos naturales

mdl_rents1<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
                +expfuel+ind_work+pop_g+energy_gdp
                +elect_rnw+minrent+natrent+gasrent+forrent+coalrent, data=df)
summary(mdl_rents1)

mdlh_rents1<-coeftest(mdl_rents1,vcov=hccm(mdl_rents1,type='hc1'))
print(mdlh_rents1)

# Modelo 10.1 eliminando todo lo que está demas

mdl_energy2<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
                +ind_work+pop_g+energy_gdp
                +elect_rnw+natrent+gasrent+forrent+coalrent, data=df)
summary(mdl_energy2)

mdlh_energy2<-coeftest(mdl_energy2,vcov=hccm(mdl_energy2,type='hc1'))
print(mdlh_energy2)

# Con uso de combustibles fósiles

mdl_ff<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
                +ind_work+pop_g+energy_gdp
                +natrent+forrent+coalrent+
                  fossil_use, data=df)
summary(mdl_ff)

mdlh_ff<-coeftest(mdl_ff,vcov=hccm(mdl_ff,type='hc1'))
print(mdlh_ff)

# Con empleo en agricultura (no fue importante en servicios)

mdl_ag<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
           +ind_work+pop_g+energy_gdp
           +natrent+forrent+coalrent+
             fossil_use+agr_emp, data=df)
summary(mdl_ag)

mdlh_ag<-coeftest(mdl_ag,vcov=hccm(mdl_ag,type='hc1'))
print(mdlh_ag)


# Modelo Final

mdl_fn<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
           +ind_work+pop_g+energy_gdp+natrent+forrent+fossil_use
           +impfuel+log(cer_yield)+agr_emp,data=df)
summary(mdl_fn)

mdlh_fn<-coeftest(mdl_fn,vcov=hccm(mdl_fn,type='hc1'))
print(mdlh_fn)

# Modelo con interacciones

# Trabajo en industria con uso de energía por unidad de GDP

mdl_int1<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
           +pop_g+natrent+forrent+fossil_use
           +impfuel+log(cer_yield)+agr_emp+
           +energy_gdp*ind_work,data=df)
summary(mdl_int1)

mdlh_int1<-coeftest(mdl_int1,vcov=hccm(mdl_int1,type='hc1'))
print(mdlh_int1)

# Fossil Use con popg

mdl_int2<-lm(co2_pc~year*kyoto+poly(log(gdp_pc),2,raw=T)
             +natrent+forrent+fossil_use*pop_g
             +impfuel+log(cer_yield)+agr_emp+
               +energy_gdp*ind_work,data=df)
summary(mdl_int2)

mdlh_int2<-coeftest(mdl_int2,vcov=hccm(mdl_int2,type='hc1'))
print(mdlh_int2)

# Con otras interacciones
mdl_int3<-lm(co2_pc~kyoto*(year+oilrent+serv_emp+ind_work+elect_rnw)
             +poly(log(gdp_pc),2,raw=T)
             +fossil_use*(pop_g+forrent)+impfuel
             +log(cer_yield)+agr_emp+energy_gdp*ind_work
             ,data=df)
summary(mdl_int3)

mdlh_int3<-coeftest(mdl_int3,vcov=hccm(mdl_int3,type='hc1'))
print(mdlh_int3)

# Con otras interacciones 2
mdl_int4<-lm(co2_pc~kyoto*(year+oilrent+serv_emp+ind_work)
             +poly(log(gdp_pc),2,raw=T)
             +fossil_use*(pop_g+forrent)+impfuel
             +log(cer_yield)+energy_gdp*ind_work+year*energy_gdp
             ,data=df)
summary(mdl_int4)

mdlh_int4<-coeftest(mdl_int4,vcov=hccm(mdl_int4,type='hc1'))
print(mdlh_int4)




