## This R file creates 'semicircle.pdf'

pdf(file="semicircle.pdf",height=9,width=9)
factor <- 0.8
plot(NA,xlim=c(0,1.2),ylim=c(0,0.6),axes=FALSE,xlab='',ylab='',asp=1)
points(0,0,pch=16)
text(0,0,expression(a^infinity),pos=1)
text(1,0,expression(ba^infinity),pos=1)
text(factor,0,expression(aba^infinity),pos=1)
f <- function(n){
    text(factor^n,0,substitute(a^n * b * a^infinity,list(n=n)),pos=1)
}


for(i in 2:5){f(i)}
f(9)
circ <- function(n, center, radius, dopoints=FALSE, ...){
    theta <- seq(from=0,to= pi,len=100)
    points(center[1] + radius*cos(theta),
           center[2] + radius*sin(theta),
           type="l",...)

    if(dopoints){
        theta <- pi-pi*factor^(0:17)
        points(center[1] + radius*cos(theta),
               center[2] + radius*sin(theta),
               type="p",pch=16)
    }
}

for(n in 0:9){circ(n,  center=c(factor^n/2,0),radius=factor^n/2,dopoints=TRUE, lwd=0.5)}

manycirc <- function(x2,y,...){
    for(n in 0:9){
        circ(n, center=c(x2[1] + (x2[2]-x2[1])*factor^n/2, y), radius=factor^n*(x2[2]-x2[1])/2,lwd=0.5,...)}
}

manycirc(c(1,1+0.2),0,col='red')
manycirc(c(0.905,0.905+0.2*factor),0.295,col='red')
manycirc(c(0.71,0.71+0.2*factor^2),0.45,col='red')
manycirc(c(0.52,0.52+0.2*factor^3),0.5,col='red')

manycirc(c(1.12,1.12+0.2*factor^6),0.1,col='red')
manycirc(c(1.17,1.17+0.2*factor^9),0.11,col='red')

manycirc(c(factor,factor+0.2*factor),0,col='blue')
manycirc(c(0.72,0.72+0.2*factor^2),0.235,col='blue')
manycirc(c(0.57,0.57+0.2*factor^3),0.362,col='blue')

manycirc(c(factor^2,factor^2+0.2*factor^2),0,col='green')
manycirc(c(0.58,0.58+0.2*factor^2),0.19,col='green')

    
text(0.93,0.30,expression(ababa^infinity),pos=1)
text(0.76,0.45,expression(a^2*b*a*b*a^infinity),pos=1)
text(0.58,0.50,expression(a^3*b*a*b*a^infinity),pos=1)
text(0.76,0.24,expression(a^2*b*a^2*b*a^infinity),pos=1)
text(0.63,0.36,expression(a^3*b*a^2*b*a^infinity),pos=1)
text(0.64,0.19,expression(a^2*b*a^3*b*a^infinity),pos=1)

text(0.64,0.19,expression(a^2*b*a^3*b*a^infinity),pos=1)

text(1.2,0,expression(b*a*b*a^infinity),pos=1)
points(1.2,0,pch=16,col='red')


points(1,0.373,pch=16,col='red')

text(1.1,0,expression(b * a^7 * b * a^infinity),pos=1)
points(1.05,0.087,pch=16,col='red')

text(1.07,0.25,expression(a*b*a*b*a*b*a^infinity),pos=3)
points(1.065,0.295,pch=16,col='red')

text(1.07,0.1,expression(b*a^9*b*a*b*a^infinity),pos=3,col='blue')

text(1.07,0.37,expression(a*b*a*b*a^3*b*a*b*a^infinity),pos=3)

text(0.92,0.36,expression(a*b*a*b*a^5*b*a*b*a^infinity),pos=3,col='red',cex=0.6)
points(0.938,0.36,pch=16,col='red')

points(cbind(factor^(0:100),0),pch=16)
dev.off()
