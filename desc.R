# Proyecto de Semestre 
# Economía Ambiental
# Estadística descriptiva del dataset

# Cargar librerías
library(psych) # Estadística descriptiva
library(tidyverse) # Tidyverse
library(openxlsx) # Exportar a Excel

# Cargar bases
df<-read.csv('df.csv') # Normal 
dfnum<-read.csv('dfnum.csv') # Para matrices de cor

# Separar bases por año
df_03<-subset(df, df$year=='2003')
df_07<-subset(df, df$year=='2007')

# Matriz de correlación
dfnum$kyoto<-df$kyoto
a<-cor(dfnum,use='complete.obs')
write.xlsx(a,'cor.xlsx', rowNames=T)

# Estadística descriptiva de países por categoría

# Por año
describeBy(df$co2_pc,group=df$year)

# Por año y kyoto
describeBy(df_03$co2_pc,group=df_03$kyoto)
describeBy(df_07$co2_pc,group=df_07$kyoto)


