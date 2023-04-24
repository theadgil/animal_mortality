################################################################
################################################################
#####               R-Script: Curve fits figure (SI)       #####
#####      Dorothea Gilbert & Matz A. Haugen (2023):       #####
#####                      03.04.2023				       #####
################################################################
################################################################


# the figure can be created with base R functions


# change path as needed

# setwd("~/Figures/")


# the script relies on the lists "mort.Gompertz" and "mort.Logistic" with nls fit outputs
#  created with the script "mortality_modelling.R"

################################################################
#### Write to pdf
################################################################



pdf(file = "~/Gompertz-Sigmoidal.pdf", 
    title="Curve fits",
    #par(mar=c(4,4,0.5,0.5)),
    #paper = "a4",
    width=8.27, height=11.69*0.75)


# Multipanel plot

par(mfcol = c(7, 3))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# x-coordinates for modelled fit line
xvec <- seq(0, 15, length=100)


# first column: all-cause mortality

plot(norm$weeks,norm$total_GER, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$total_GER2, ylim=c(0,1.4),pch=16)

lines(xvec, predict(mort.Gompertz$total_GER2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Gompertz$total_GER, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(mort.Logistic$total_GER2, list(weeks=xvec)),col='blue')
lines(xvec, predict(mort.Logistic$total_GER, list(weeks=xvec)),col='blue', lty="dashed")

mtext("all-causes (GER)", side = 3, line = -1, adj = 0.05, cex = 0.6)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
box(col = "grey40")

plot(norm$weeks,norm$total_LUX, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$total_LUX2, ylim=c(0,1.4),pch=16)

lines(xvec, predict(mort.Gompertz$total_LUX2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$total_LUX2, list(weeks=xvec)),col='blue')

mtext("all-causes (LUX)", side = 3, line = -1, adj = 0.05, cex = 0.6)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
box(col = "grey40")


plot(norm$weeks,norm$total_AUS, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$total_AUS2, ylim=c(0,1.4),pch=16)

lines(xvec, predict(mort.Gompertz$total_AUS2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$total_AUS2, list(weeks=xvec)),col='blue')

mtext("all-causes (AUS)", side = 3, line = -1, adj = 0.05, cex = 0.6)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
box(col = "grey40")

plot(norm$weeks,norm$total_ECU, axes=F,ylim=c(0,1.4),type = "n" )
mtext("all-causes (THA)", side = 3, line = -1, adj = 0.05, cex = 0.6)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
box(col = "grey40")

plot(norm$weeks,norm$total_ECU, axes=F,ylim=c(0,1.4), pch=16)

lines(xvec, predict(mort.Gompertz$total_ECU, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$total_ECU, list(weeks=xvec)),col='blue')

mtext("all-causes (ECU)", side = 3, line = -1, adj = 0.05, cex = 0.6)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
box(col = "grey40")

plot(norm$weeks,norm$Euromomo, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$Euromomo3, ylim=c(0,1.4),pch=16)

lines(xvec, predict(mort.Gompertz$Euromomo3, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Gompertz$Euromomo, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(mort.Logistic$Euromomo3, list(weeks=xvec)),col='blue')
lines(xvec, predict(mort.Logistic$Euromomo, list(weeks=xvec)),col='blue', lty="dashed")

mtext("all-causes (EuroMOMO)", side = 3, line = -1, adj = 0.05, cex = 0.6)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
box(col = "grey40")


plot(norm$weeks,norm$total_USA, axes=F,ylim=c(0,1.4),pch=16)

lines(xvec, predict(mort.Gompertz$total_USA, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$total_USA, list(weeks=xvec)),col='blue')

mtext("all-causes (USA)", side = 3, line = -1, adj = 0.05, cex = 0.6)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
box(col = "grey40")
axis(1, col = "grey40", col.axis = "grey20", at = seq(0,16, 2),labels=seq(10,26,2))


# second column: Covid-mortality

plot(norm$weeks,norm$Covid_GER, axes=F,ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$Covid_GER, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$Covid_GER, list(weeks=xvec)),col='blue')
mtext("Covid (GER)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(norm$weeks,norm$Covid_LUX, axes=F,ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$Covid_LUX, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$Covid_LUX, list(weeks=xvec)),col='blue')
mtext("Covid (LUX)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")


plot(norm$weeks,norm$Covid_AUS, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$Covid_AUS2, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$Covid_AUS2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Gompertz$Covid_AUS, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(mort.Logistic$Covid_AUS2, list(weeks=xvec)),col='blue')
lines(xvec, predict(mort.Logistic$Covid_AUS, list(weeks=xvec)),col='blue', lty="dashed")

mtext("Covid (AUS)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(norm$weeks,norm$Covid_THA, axes=F,ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$Covid_THA, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$Covid_THA, list(weeks=xvec)),col='blue')
mtext("Covid (THA)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(norm$weeks,norm$Covid_ECU, axes=F,ylim=c(0,1.4))
lines(norm$weeks,norm$Covid_ECU, ylim=c(0,1.4),lty="dotted")
mtext("Covid (ECU)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(norm$weeks,norm$Covid_ECDC, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$Covid_ECDC2, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$Covid_ECDC2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Gompertz$Covid_ECDC, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(mort.Logistic$Covid_ECDC2, list(weeks=xvec)),col='blue')
lines(xvec, predict(mort.Logistic$Covid_ECDC, list(weeks=xvec)),col='blue', lty="dashed")

points(norm$weeks,norm$Covid_ECDC22, col='grey50')
mtext("Covid (EU55)", side = 3, line = -1, adj = 0.05, cex = 0.6)
mtext("Covid (EU22)", side = 3, line = -2, adj = 0.05, cex = 0.6,col='grey50')
box(col = "grey40")

plot(norm$weeks,norm$Covid_USA, axes=F,ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$Covid_USA, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$Covid_USA, list(weeks=xvec)),col='blue')
mtext("Covid (USA)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")
axis(1, col = "grey40", col.axis = "grey20", at = seq(0,16, 2),labels=seq(10,26,2))


# third column: animals

plot(norm$weeks,norm$birds_GER, axes=F,ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$birds_GER, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$birds_GER, list(weeks=xvec)),col='blue')
mtext("birds (GER)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(norm$weeks,norm$birds_LUX, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$birds_LUX2, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$birds_LUX2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Logistic$birds_LUX, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(mort.Logistic$birds_LUX2, list(weeks=xvec)),col='blue')
lines(xvec, predict(mort.Logistic$birds_LUX, list(weeks=xvec)),col='blue', lty="dashed")

mtext("birds (LUX)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(birds$weeks,birds$birds, axes=F,ylim=c(0,1.4))
points(birds$weeks,birds$birds2, ylim=c(0,1.4), pch=16)
lines(xvec, predict(mort.Gompertz$birds2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Gompertz$birds, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(mort.Logistic$birds2, list(weeks=xvec)),col='blue')
lines(xvec, predict(mort.Logistic$birds, list(weeks=xvec)),col='blue', lty="dashed")

mtext("birds (GER & LUX)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(norm$weeks,norm$horses_cases, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$horses_cases2, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$horses_deaths2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Gompertz$horses_deaths, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(Sigm_horses_cases2, list(weeks=xvec)),col='blue')
lines(xvec, predict(Sigm_horses_cases, list(weeks=xvec)),col='blue', lty="dashed")

mtext("AHS cases (THA)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")

plot(norm$weeks,norm$horses_deaths, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$horses_deaths2, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$horses_deaths2, list(weeks=xvec)),col='red')
lines(xvec, predict(mort.Gompertz$horses_deaths, list(weeks=xvec)),col='red', lty="dashed")

lines(xvec, predict(mort.Logistic$horses_deaths2, list(weeks=xvec)),col='blue')
lines(xvec, predict(mort.Logistic$horses_deaths, list(weeks=xvec)),col='blue', lty="dashed")

mtext("horse deaths (THA)", side = 3, line = -1, adj = 0.05, cex = 0.6)
box(col = "grey40")
axis(1, col = "grey40", col.axis = "grey20", at = seq(0,16, 2),labels=seq(10,26,2))

plot(norm$weeks,norm$total_ECU, axes=F,ylim=c(0,1.4),type = "n")
#box(col = "grey40")

plot(norm$weeks,norm$total_ECU, axes=F,ylim=c(0,1.4),type = "n")
#box(col = "grey40")
#axis(1, col = "grey40", col.axis = "grey20", at = seq(0,16, 2),labels=seq(10,26,2))
legend(x = "bottomright",          # Position
       legend = c("Gompertz", "Gompertz (wo/ outliers)","Logistic","Logistic (wo/ outliers)"),  # Legend texts
       bty = "n",
       cex = 1.5, # Change legend size
       trace=TRUE,
       col = c("red","red","blue","blue"),
       lty = c(1,2,1,2)) 

mtext("week of year 2020", side = 1, outer = TRUE, cex = 1, line = 2.2,col = "grey20")
mtext("normalized number of deaths (min = 0%, max = 100%)", side = 2, outer = TRUE, cex = 1, line = 2.2,col = "grey20")
#mtext("Gompertz curve fits", side=3, outer=TRUE, line=1.2)



dev.off()
