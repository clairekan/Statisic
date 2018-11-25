####11.32 example####

plot(c(0,3),type='n',xlim=c(6,9),xlab='Mean hour of sleep',ylab='',axes=F,
     main='Comparsion of the two confidence interval')

lines(c(7.1,8.22),c(2,2))
text(6.9,2,labels='Stat10')
lines(c(7.1,7.1),c(1.95,2.05))
lines(c(7.66,7.66),c(1.95,2.05))
lines(c(8.22,8.22),c(1.95,2.05))

lines(c(6.53,7.09),c(1,1))
text(6.3,1,labels='Stat13')
lines(c(6.53,6.53),c(0.95,1.05))
lines(c(6.81,6.81),c(0.95,1.05))
lines(c(7.09,7.09),c(0.95,1.05))

axis(1)

