Data yang digunakan

Kecamatan

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





