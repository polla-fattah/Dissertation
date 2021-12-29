xx = c( cos(pi/2), cos(pi/4), cos(pi/8), cos(pi/25), cos(0)) * 20
yy = c( sin(pi/2), sin(pi/4), sin(pi/8), sin(pi/25), sin(0)) * 20
plot(xx, yy, pch='.', xlab = "Others' Contribution", ylab = "Own Contribution")

x = c( cos(pi/4), cos(pi/8), cos(pi/25)) * 20
y = c( sin(pi/4), sin(pi/8), sin(pi/25)) * 20
arrows(0,0, x, y, angle = 10, length = 0.15, col = c(3, 4, 2))

X = c( cos(pi/2), cos(0)) * 20
Y = c( sin(pi/2), sin(0)) * 20
arrows(0,0, X, Y, angle = 10, length = 0.2, lwd=2)

text(0.35 * 20,0.6 * 20,"Strong Contributor", col=3, srt=40)

text(0.5 * 20,0.35 * 20,"Normal Contributor", col=4, srt=25)

text(0.65 * 20,0.18 * 20,"Weak Contributor", col=2, srt=10)

text(0.75 * 20,0.05 * 20,"Free Rider", col=1)