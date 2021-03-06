colors <- rainbow(9)
library(rgl)
spheres3d(-2.0,-2.0,-2.0, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,-4.75)), col=c("red","red"))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,-4.75,0.0)), col=c("red","red"))
segments3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(-4.75,0.0,0.0)), col=c("red","red"))

spheres3d(-2.0,-2.0,2.0, radius=0.1, color="red")
segments3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,-4.75,0.0)), col=c("red","red"))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(-3.75,0.0,0.0), c(-4.75,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,3.75), c(0.0,0.0,4.75)), col=c("red","red"))

spheres3d(-2.0,2.0,-2.0, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,-4.75)), col=c("red","red"))
segments3d(rbind(c(0.0,0.0,-3.75), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,4.75,0.0)), col=c("red","red"))
segments3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(-4.75,0.0,0.0)), col=c("red","red"))

spheres3d(-2.0,2.0,2.0, radius=0.1, color="red")
segments3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,4.75,0.0)), col=c("red","red"))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(-3.75,0.0,0.0), c(-4.75,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,3.75), c(0.0,0.0,4.75)), col=c("red","red"))

spheres3d(2.0,-2.0,-2.0, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,-4.75)), col=c("red","red"))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(4.75,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,-4.75,0.0)), col=c("red","red"))

spheres3d(2.0,-2.0,2.0, radius=0.1, color="red")
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(4.75,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,-4.75,0.0)), col=c("red","red"))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,3.75), c(0.0,0.0,4.75)), col=c("red","red"))

spheres3d(2.0,2.0,-2.0, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,-4.75)), col=c("red","red"))
segments3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(4.75,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,3.75,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,4.75,0.0)), col=c("red","red"))

spheres3d(2.0,2.0,2.0, radius=0.1, color="red")
segments3d(rbind(c(3.75,0.0,0.0), c(4.75,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,4.75,0.0)), col=c("red","red"))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,3.75), c(0.0,0.0,4.75)), col=c("red","red"))

spheres3d(-0.5,-0.5,-0.5, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,0.0)))

spheres3d(-0.5,-0.5,0.5, radius=0.1, color="red")
segments3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,0.0), c(0.0,0.0,3.75)))

spheres3d(-0.5,0.5,-0.5, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,0.0)))

spheres3d(-0.5,0.5,0.5, radius=0.1, color="red")
segments3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,0.0), c(0.0,0.0,3.75)))

spheres3d(0.5,-0.5,-0.5, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,0.0)))

spheres3d(0.5,-0.5,0.5, radius=0.1, color="red")
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,0.0), c(0.0,0.0,3.75)))

spheres3d(0.5,0.5,-0.5, radius=0.1, color="red")
segments3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-3.75), c(0.0,3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,3.75,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,0.0)))

spheres3d(0.5,0.5,0.5, radius=0.1, color="red")
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,3.75,0.0)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(3.75,0.0,0.0), c(0.0,3.75,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,0.0)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,0.0), c(0.0,0.0,3.75)))
segments3d(rbind(c(0.0,0.0,0.0), c(0.0,0.0,3.75)))

triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0), c(-3.75,0.0,0.0)), color=colors[1], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(-3.75,0.0,0.0), c(0.0,0.0,0.0)), color=colors[1], alpha=0.75)
triangles3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0), c(0.0,0.0,0.0)), color=colors[1], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0), c(0.0,0.0,0.0)), color=colors[1], alpha=0.75)

triangles3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0), c(0.0,0.0,3.75)), color=colors[2], alpha=0.75)
triangles3d(rbind(c(0.0,-3.75,0.0), c(-3.75,0.0,0.0), c(0.0,0.0,0.0)), color=colors[2], alpha=0.75)
triangles3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,3.75), c(0.0,0.0,0.0)), color=colors[2], alpha=0.75)
triangles3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,3.75), c(0.0,0.0,0.0)), color=colors[2], alpha=0.75)

triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,3.75,0.0), c(-3.75,0.0,0.0)), color=colors[3], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0), c(-3.75,0.0,0.0)), color=colors[3], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,0.0), c(0.0,3.75,0.0), c(-3.75,0.0,0.0)), color=colors[3], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0), c(0.0,3.75,0.0)), color=colors[3], alpha=0.75)

triangles3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0), c(0.0,0.0,3.75)), color=colors[4], alpha=0.75)
triangles3d(rbind(c(0.0,3.75,0.0), c(-3.75,0.0,0.0), c(0.0,0.0,0.0)), color=colors[4], alpha=0.75)
triangles3d(rbind(c(-3.75,0.0,0.0), c(0.0,0.0,0.0), c(0.0,0.0,3.75)), color=colors[4], alpha=0.75)
triangles3d(rbind(c(0.0,3.75,0.0), c(0.0,0.0,0.0), c(0.0,0.0,3.75)), color=colors[4], alpha=0.75)

triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0), c(3.75,0.0,0.0)), color=colors[5], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0), c(0.0,0.0,0.0)), color=colors[5], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,-3.75,0.0), c(0.0,0.0,0.0)), color=colors[5], alpha=0.75)
triangles3d(rbind(c(0.0,-3.75,0.0), c(3.75,0.0,0.0), c(0.0,0.0,0.0)), color=colors[5], alpha=0.75)

triangles3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0), c(0.0,0.0,3.75)), color=colors[6], alpha=0.75)
triangles3d(rbind(c(3.75,0.0,0.0), c(0.0,-3.75,0.0), c(0.0,0.0,0.0)), color=colors[6], alpha=0.75)
triangles3d(rbind(c(0.0,-3.75,0.0), c(0.0,0.0,0.0), c(0.0,0.0,3.75)), color=colors[6], alpha=0.75)
triangles3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0), c(0.0,0.0,3.75)), color=colors[6], alpha=0.75)

triangles3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0), c(0.0,3.75,0.0)), color=colors[7], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(3.75,0.0,0.0), c(0.0,0.0,0.0)), color=colors[7], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,-3.75), c(0.0,0.0,0.0), c(0.0,3.75,0.0)), color=colors[7], alpha=0.75)
triangles3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0), c(0.0,3.75,0.0)), color=colors[7], alpha=0.75)

triangles3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0), c(0.0,3.75,0.0)), color=colors[8], alpha=0.75)
triangles3d(rbind(c(3.75,0.0,0.0), c(0.0,0.0,0.0), c(0.0,0.0,3.75)), color=colors[8], alpha=0.75)
triangles3d(rbind(c(0.0,0.0,0.0), c(0.0,3.75,0.0), c(0.0,0.0,3.75)), color=colors[8], alpha=0.75)
triangles3d(rbind(c(3.75,0.0,0.0), c(0.0,3.75,0.0), c(0.0,0.0,3.75)), color=colors[8], alpha=0.75)

