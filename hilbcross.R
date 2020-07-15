pdf(file="hilbcross.pdf")
par(pty="s")
par(xpd=TRUE)
plot(NA,xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE,xlab='',ylab='')

theta <- seq(from=0,to=2*pi,len=100)


ang <- function(theta, ...){ segments(cos(theta),sin(theta),-cos(theta),-sin(theta),...)}
tex <- function(theta, labels,...){ text(cos(theta),sin(theta), labels=labels, ...)}

ang(0)
tex(0,expression(10^infinity),pos=4)
tex(-pi,expression(0*1^infinity),pos=2)

ang(pi/2)
tex(pi/2,expression(1^infinity),pos=3)
tex(-pi/2,expression(0^infinity),pos=1)

ang(theta=pi/6)
tex(theta=pi/6,expression(101^infinity),pos=4)
tex(theta=pi+pi/6,expression(0*1*0^infinity),pos=2)

ang(theta=pi/3)
tex(theta=pi/3,expression(110^infinity),pos=4)
tex(theta=pi+pi/3,expression(0*0*1^infinity),pos=2)

ang(pi/18)
tex(theta=pi/18,expression(1001^infinity),pos=4)
tex(theta=pi+pi/18,expression(0*1*1*0^infinity),pos=2)

ang(pi/2-pi/18)
tex(theta=pi/2-pi/18,expression(1110^infinity),pos=3)
tex(theta=3*pi/2-pi/18,expression(0*0*0*1^infinity),pos=1)

ang(pi/2-2*pi/18)
tex(theta=pi/2-2*pi/18,expression(1*1*0*1^infinity),pos=3)
tex(theta=3*pi/2-2*pi/18,expression(0*0*1*0^infinity),pos=1)

ang(2*pi/18)
tex(theta=2*pi/18,expression(1010^infinity),pos=4)
tex(theta=pi+2*pi/18,expression(0*101^infinity),pos=2)

ang(pi/54)
ang(pi/27)

ang(pi/9 + pi/54)
ang(pi/9 + pi/27)

ang(pi/2-pi/54)
ang(pi/2-pi/27)

ang(pi/2-(pi/9 + pi/54))
ang(pi/2-(pi/9 + pi/27))

text(0,0,expression(0.5^infinity),adj=c(-0.5,1.3))

  
dev.off()

