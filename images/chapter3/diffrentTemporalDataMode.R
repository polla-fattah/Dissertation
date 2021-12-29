m = matrix(c( 1.1,2.1,1.1,1.1
             ,2.2,2.2,1.2,1.2 
             ,2.3,1.3,2.3,1.3 
             ,5.1,6.1,5.1,6.1 
             ,6.2,6.2,6.2,5.2 
             ,5.3,5.3,6.3,6.3), 
           ncol = 4, byrow = T)
par(mfrow=c(1,1))
plot(m[1, ], ylim = c(0,7), 
     xlab = "Time Points", ylab = "Item Values",
     xaxp=c(1, 4, 3), pch=1)
#axis(1, xaxp=c(1, 4, 4), las=1)
lines(m[1, ])
for(i in 2:6){
  points(m[i, ], col=i, pch=i)
  lines(m[i, ], col=i)
}

par(mfrow=c(1,4))

for(i in 1:4){
  plot(m[, i], ylim = c(0,7),  col=1:6, pch=1:6,
       xlab = paste("Time Point", i), ylab = "Item Values")
}
