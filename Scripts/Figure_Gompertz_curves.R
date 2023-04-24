################################################################
################################################################
#####               R-Script: Gompertz figure (MAIN)       #####
#####      Dorothea Gilbert & Matz A. Haugen (2023):       #####
#####                      03.04.2023				               #####
################################################################
################################################################


# the figure can be created with base R functions


# change path as needed

# setwd("~/Figures/")


# the script relies on the lists "mort.Gompertz" with nls fit outputs
# created with the script "mortality_modelling.R"

################################################################
#### Write to pdf
################################################################

# change path as needed

pdf(file = "~/OutFiles/Gompertz-Figure2.pdf", 
    title="Gompertz",
    #par(mar=c(4,4,0.5,0.5)),
    #paper = "a4",
    height=8.27, width=11.69)


# Multipanel plot
par(mfcol = c(3, 3))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 6, 4, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# vector for fit line x-values
xvec <- seq(0, 15, length=100)


# start plotting
plot(norm$weeks,norm$total_GER, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$total_GER2, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$total_GER2, list(weeks=xvec)))
points(norm$weeks,norm$Covid_GER, col=4,pch=16)
lines(xvec, predict(mort.Gompertz$Covid_GER, list(weeks=xvec)),col=4)
points(norm$weeks,norm$birds_GER, col=2,pch=4)
lines(xvec, predict(mort.Gompertz$birds_GER, list(weeks=xvec)),col=2)
points(norm$weeks,norm$birds_LUX2, col=2,pch=3)
lines(xvec, predict(mort.Gompertz$birds_LUX2, list(weeks=xvec)),col=2, lty=2)
mtext("Germany", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",         
       legend = c("all-cause (GER)",
                  "Covid (GER)",
                  "birds (GER)","birds (LUX)"),  
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col=c(1,4,2,2),
       lty = c(1,1,1,2),
       pch=c(16,16,4,3)) 
box(col = "grey40")
#
##plot(norm$weeks,norm$total_LUX, axes=F,ylim=c(0,1.4))
##lines(norm$weeks,norm$total_LUX,lty=2,col=1)
#
##points(norm$weeks,norm$Covid_LUX, col=4,pch=18)
##lines(xvec, predict(mort.Gompertz$Covid_LUX, list(weeks=xvec)),col=4,lty=1)
##points(norm$weeks,norm$birds_LUX, col=2,pch=4)
##points(norm$weeks,norm$birds_LUX2, pch=18,col=2)
##lines(xvec, predict(mort.Gompertz$birds_LUX2, list(weeks=xvec)),col=2,lty=1)
##axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
##mtext("Luxembourg", side = 3, line = -1.3, adj = 0.05, cex = 0.8)
##legend(x = "topright",         
##       legend = c("all-cause (LUX)",
##                  "Covid (LUX)",
##                  "birds (LUX)"),  
##       bty = "n",
##       cex = 0.7, # Change legend size
##       trace=TRUE,
##       col=c(1,4,2),
##       lty = c(1,1,1),
##       pch=c(16,16,4))  
##box(col = "grey40")

plot(norm$weeks,norm$Covid_THA, axes=F,ylim=c(0,1.4),col=4,pch=16)
lines(xvec, predict(mort.Gompertz$Covid_THA, list(weeks=xvec)),col=4)
points(norm$weeks,norm$horses_deaths, col=2)
points(norm$weeks,norm$horses_deaths2, pch=16,col=2)
lines(xvec, predict(mort.Gompertz$horses_deaths2, list(weeks=xvec)),col=2)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
mtext("Thailand", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",         
       legend = c("Covid (THA)","horses (THA)"),
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col=c(4,2),
       lty = c(1,1),
       pch=c(16,16))
box(col = "grey40")

plot(norm$weeks,norm$total_ECU, axes=F,ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$total_ECU, list(weeks=xvec)))
points(norm$weeks,norm$Covid_ECU, col=4)
lines(norm$weeks,norm$Covid_ECU, col=4,lty=2)
axis(2, las=1, col = "grey40", col.axis = "grey20", at = seq(0,1.4, 0.4), labels = seq(0,140, 40))
axis(1, col = "grey40", col.axis = "grey20", at = seq(0,16, 2),labels=seq(10,26,2))
box(col = "grey40")
mtext("Ecuador", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",         
       legend = c("all-cause (ECU)","Covid (ECU)"),
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col=c(1,4),
       lty = c(1,2),
       pch=c(16,16))

plot(norm$weeks,norm$Euromomo, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$Euromomo3, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$Euromomo3,list(weeks=xvec)))
points(norm$weeks,norm$Covid_ECDC, col=4,pch=16)
lines(xvec, predict(mort.Gompertz$Covid_ECDC2,list(weeks=xvec)),col=4)
box()
mtext("Europe", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",         
       legend = c("all-cause (EU)","Covid (EU)"),
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col=c(1,4),
       lty = c(1,1),
       pch=c(16,16))

plot(norm$weeks,norm$total_USA, axes=F,ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$total_USA, list(weeks=xvec)))
points(norm$weeks,norm$Covid_USA, col=4,pch=16)
lines(xvec, predict(mort.Gompertz$Covid_USA, list(weeks=xvec)),col=4)
box(col = "grey40")
mtext("United States", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",         
       legend = c("all-cause (USA)","Covid (USA)"),
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col=c(1,4),
       lty = c(1,1),
       pch=c(16,16))

plot(norm$weeks,norm$total_AUS, axes=F,ylim=c(0,1.4))
points(norm$weeks,norm$total_AUS3, ylim=c(0,1.4),pch=16)
lines(xvec, predict(mort.Gompertz$total_AUS3, list(weeks=xvec)))
points(norm$weeks,norm$Covid_AUS, col=4,pch=16)
lines(xvec, predict(mort.Gompertz$Covid_AUS2,list(weeks=xvec)),col=4)
axis(1, col = "grey40", col.axis = "grey20", at = seq(0,16, 2),labels=seq(10,26,2))
box(col = "grey40")
mtext("Australia", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",         
       legend = c("all-cause (AUS)","Covid (AUS)"),
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col=c(1,4),
       lty = c(1,1),
       pch=c(1,1))


# Summary panel

plot(norm$weeks,norm$Euromomo, axes=F,ylim=c(0,1.4), col=0)
# Covid
lines(xvec, predict(mort.Gompertz$Covid_ECDC2, list(weeks=xvec)),col="grey80",lty=1)
lines(xvec, predict(mort.Gompertz$Covid_USA, list(weeks=xvec)), col="grey80",lty=2)
lines(xvec, predict(mort.Gompertz$Covid_AUS2, list(weeks=xvec)),col="grey80",lty=3)
lines(norm$weeks,norm$Covid_ECU, col="grey80",lty=4)
lines(xvec, predict(mort.Gompertz$Covid_THA, list(weeks=xvec)),col="grey80",lty=5)

# Animals
lines(xvec, predict(mort.Gompertz$horses_deaths2, list(weeks=xvec)),col="grey80",lty=6)
lines(xvec, predict(mort.Gompertz$birds_LUX2, list(weeks=xvec)),col="grey80",lty=7)
lines(xvec, predict(mort.Gompertz$birds_GER, list(weeks=xvec)),col="grey80",lty=8)

# all-cause
lines(xvec, predict(mort.Gompertz$Euromomo3, list(weeks=xvec)),lwd=2)
lines(xvec, predict(mort.Gompertz$total_USA, list(weeks=xvec)), col=1,lty=2,lwd=2)
lines(xvec, predict(mort.Gompertz$total_AUS3, list(weeks=xvec)),col=1,lty=3,lwd=2)
lines(xvec, predict(mort.Gompertz$total_ECU, list(weeks=xvec)),col=1,lty=4)
#
box(col = "grey40")
#
mtext("all-causes", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",          # Position
       legend = c("EU", "USA","AUS","ECU"),  # Legend texts
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col = c(1,1,1,1),
       lty = c(1, 2,3,4),
       lwd = c(1,1,1,1))           


plot(norm$weeks,norm$Covid_ECDC, axes=F,ylim=c(0,1.4),col=0)
# all-cause
lines(xvec, predict(mort.Gompertz$Euromomo3, list(weeks=xvec)),col="grey80",lty=1)
lines(xvec, predict(mort.Gompertz$total_USA, list(weeks=xvec)), col="grey80",lty=2)
lines(xvec, predict(mort.Gompertz$total_AUS3, list(weeks=xvec)),col="grey80",lty=3)
lines(xvec, predict(mort.Gompertz$total_ECU, list(weeks=xvec)),col="grey80",lty=3)

# Animals
lines(xvec, predict(mort.Gompertz$horses_deaths2, list(weeks=xvec)),col="grey80",lty=6)
lines(xvec, predict(mort.Gompertz$birds_LUX2, list(weeks=xvec)),col="grey80",lty=7)
lines(xvec, predict(mort.Gompertz$birds_GER, list(weeks=xvec)),col="grey80",lty=8)

# Covid
lines(xvec, predict(mort.Gompertz$Covid_ECDC2, list(weeks=xvec)),col=4,lwd=2)
lines(xvec, predict(mort.Gompertz$Covid_USA, list(weeks=xvec)), col=4, lty=2,lwd=2)
lines(xvec, predict(mort.Gompertz$Covid_AUS2, list(weeks=xvec)),col=4,lty=3,lwd=2)
lines(norm$weeks,norm$Covid_ECU, col=4,lty=4,lwd=3)
lines(xvec, predict(mort.Gompertz$Covid_THA, list(weeks=xvec)),col=4,lty=5,lwd=2)
#
box(col = "grey40")
mtext("Covid", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",          # Position
       legend = c("EU", "USA","AUS","ECU","THA"),  # Legend texts
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col = c(4,4,4,4,4),
       lty = c(1,2,3,4,5),
       lwd = c(1,1,1,1,1))          


plot(norm$weeks,norm$horses_deaths, axes=F,ylim=c(0,1.4),col=0)
# all-cause
lines(xvec, predict(mort.Gompertz$Euromomo3, list(weeks=xvec)),col="grey80",lty=1)
lines(xvec, predict(mort.Gompertz$total_USA, list(weeks=xvec)), col="grey80",lty=2)
lines(xvec, predict(mort.Gompertz$total_AUS3, list(weeks=xvec)),col="grey80",lty=3)
lines(xvec, predict(mort.Gompertz$total_ECU, list(weeks=xvec)),col="grey80",lty=3)

# Covid
lines(xvec, predict(mort.Gompertz$Covid_ECDC2, list(weeks=xvec)),col="grey80",lty=1)
lines(xvec, predict(mort.Gompertz$Covid_USA, list(weeks=xvec)), col="grey80",lty=2)
lines(xvec, predict(mort.Gompertz$Covid_AUS2, list(weeks=xvec)),col="grey80",lty=3)
lines(norm$weeks,norm$Covid_ECU, col="grey80",lty=4)
lines(xvec, predict(mort.Gompertz$Covid_THA, list(weeks=xvec)),col="grey80",lty=5)

# Animals
lines(xvec, predict(mort.Gompertz$horses_deaths2, list(weeks=xvec)),col=2,lwd=2)
lines(xvec, predict(mort.Gompertz$birds_LUX2, list(weeks=xvec)),col=2,lty=2,lwd=2)
lines(xvec, predict(mort.Gompertz$birds_GER, list(weeks=xvec)),col=2,lty=3,lwd=2)
#
axis(1, col = "grey40", col.axis = "grey20", at = seq(0,16, 2),
     labels=seq(10,26,2))
box(col = "grey40")
#
mtext("animals", side = 3, line = -1.3, adj = 0.05, cex = 1)
legend(x = "topright",          # Position
       legend = c("horses (THA)", "birds (LUX)","birds (GER)"),  # Legend texts
       bty = "n",
       cex = 0.7, # Change legend size
       trace=TRUE,
       col=c(2,2,2),
       lty = c(1,2,3),
       lwd = c(1,1,1)) 

mtext("week of year 2020", side = 1, outer = TRUE, cex = 1, line = 2.2,col = "grey20")
mtext("normalized number of deaths (%)", side = 2, outer = TRUE, cex = 1, line = 4.5,col = "grey20")
mtext("(min = 0 %, max = 100 %)", side = 2, outer = TRUE, cex = 1, line = 3.0,col = "grey20")
mtext("Gompertz curve fits", side=3, outer=TRUE, line=1.2)

dev.off()
