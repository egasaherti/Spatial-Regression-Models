Data yang digunakan

KecamatanYX1X2X3GEDEBAGE17583.746117UJUNGBERUNG1833011.8113685CINAMBO15266.752310BANDUNG KULON8324422.434857ANDIR11556026.619906BABAKAN CIPARAY4425020.083975BOJONGLOA KALER2722840.3915580SUKAJADI2313025.4710860CIDADAP97289.646511COBLONG42515418.142692CICENDO87132614.6920066BANDUNG WETAN75349.213158SUMUR BANDUNG2773210.582546BATUNUNGGAL2965424.294154CIBEUNYING KIDUL709820.8214968REGOL213619.33994BANDUNG KIDUL578349.948088ASTANAANYAR357624.145204KIARACONDONG6008421.819810BUAHBATU1053612.149009MANDALAJATI155149.527151CIBIRU263611.149962BOJONGLOA KIDUL5441013.998992CIBEUNYING KALER1155815.999057PANYILEUKAN190347.724569ANTAPANI1573819.913550LENGKONG1811612.31679SUKASARI5631013.22275RANCASARI80610.332831ARCAMANIK2324611.687209

Syntak yang digunakan

#Packages
library(maptools)
library(sp)
library(locfit)
library(McSpatial)
require(McSpatial)
library(rgeos)
library(rgdal)
library(lmtest)
library(McSpatial)
library(classInt)
library(RColorBrewer)
library(raster)
library(tmap)
library(car)
library(spatialreg)
library(spdep)
library(gplots)
require(spdep)
library(rgdal)
library(shapefiles)
library(lmtest)
library(spatialreg)
library(gtools)
library(rgdal)
library(spdep)
library(raster)
library(splm)
library(plm)
library(lmtest)
library(tseries)
library(maptools)
library(rgeos)
library(rgdal)
library(classInt)
library(RColorBrewer)
library(car))
library(gplots)

#Input Data
data=read.csv(file.choose(),header=T,sep=",")
head(data)
summary(data)

#Plot
colnames(data)=c("Wilayah","Y","X1","X2","X3")
head(data)

#Ols
attach(data)
ols=lm(Y~X1+X2, data=data)
summary(ols)


#Mapping
bdg.map=readOGR(dsn="C:/Users/HP/Downloads/data_ega/kotabdg.shp",layer="kotabdg")
plot(bdg.map,main="PETA KOTA BANDUNG BERDASARKAN KECAMATAN")
text(bdg.map,bdg.map$NAMA,cex=0.5)
no=c(1:30)
kabkot=bdg.map$NAMA
Coordinate=coordinates(bdg.map)

plot(bdg.map,main="PETA KOTA BANDUNG",col="white",axes=TRUE)
text(Coordinate-0.001,label=no,cex=0.8)
#points(Coordinate+0.001,lwd=2,cex=0.5)
#Masukin data dulu
bdg.map@data=cbind(data,bdg.map=data)
textp=list("sp.text", coordinates(bdg.map), as.character(bdg.map@data[,1]),col="black", cex=0.75,font=0.6)
b=spplot(bdg.map[,2],sp.layout=list(textp),colorkey=list(space="right"),main="PNEUMONIA BALITA KOTA BANDUNG 2018")
b

#Matriks Pembobot Spasial
ID<-c(1:30)
ID<-bdg.map$ID
WrA <- poly2nb(bdg.map, row.names=ID, queen=TRUE)
WmA <- nb2mat(WrA, style='B', zero.policy = TRUE)
WA<-as.matrix(WmA)
WA<-as(WA, "dgTMatrix")
W<-nb2listw(WrA)
W

#Ols
attach(data)
ols=lm(Y~X1+X2, data=data)
summary(ols)

##moran test
lm.morantest(ols,W)
lm.LMtests(ols,W,test=c("LMlag"))

#LM
Bdg.ols<-lm(Y~X1+X2+X3, data)
moran.lm<-lm.morantest(Bdg.ols, W, alternative="two.sided")
print(moran.lm)
LM<-lm.LMtests(Bdg.ols, W, test="all")
print(LM)

##model Sar dan Sem dan AIC masing masing model

sar.Bdg1<-lagsemlm(Y~X1+X2, data, W)
summary(sar.Bdg1)

sar2sls.Bdg1<-stsls(Y~X1+X2, data, W)
summary(sar2sls.Bdg1)

s_err=errorsarlm(formula=log(Y)~X1+X2,data,list=W)
summary(s_err)
s_lag=lagsarlm(formula=log(Y)~X1+X2+X3,data,list=W)
summary(s_lag)






