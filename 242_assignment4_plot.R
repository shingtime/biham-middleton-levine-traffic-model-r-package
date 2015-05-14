
### Appendix B
########## source code to reproduce the plot in the report.


#### figure1: 4*4 grid
dev.off()
par(mfrow=c(1,1))
g = createBMLGrid(3,3,c(2,2))
plot(g,main="inital 4*4 grid")
par(mfrow=c(2,4),mar=c(1,1,1,1))

#### figure 2: moving 4 steps
dev.off()
par(mfrow=c(2,4),mar=c(1,1,1,1))
plot(rumBMLGrid(1,g),main="step 1:runBML")
plot(rumBMLGrid(2,g),main="step 2:runBML")
plot(rumBMLGrid(3,g),main="step 3:runBML")
plot(rumBMLGrid(4,g),main="step 4:runBML")


plot(CrunBMLGrid(g,1),main="step 1:CrunBML")
plot(CrunBMLGrid(g,2),main="step 2:CrunBML")
plot(CrunBMLGrid(g,3),main="step 3:CrunBML")
plot(CrunBMLGrid(g,4),main="step 4:CrunBML")

####figure 3: 128*128 density=0.5 timestep=100
dev.off()
par(mfrow=c(1,2),mar=c(1,1,1,1))
g = createBMLGrid(128,128,c(128*128*0.5/2,128*128*0.5/2))
plot(rumBMLGrid(100,g),main="runBML:128*128")
plot(CrunBMLGrid(g,100),main="CrunBML:128*128")

##### table r profiling 

Rprof("/tmp/r_BML.prof")
g = createBMLGrid(150,150,c(3000,3000))
ss = rumBMLGrid(100,g)
Rprof(NULL)
head(summaryRprof("/tmp/r_BML.prof")$by.self, 5)

Rprof("/tmp/C_BML.prof")
g = createBMLGrid(150,150,c(3000,3000))
ss = CrunBMLGrid(g,100)
Rprof(NULL)
head(summaryRprof("/tmp/C_BML.prof")$by.self, 10)
system.time(rumBMLGrid(100,g))
system.time(CrunBMLGrid(g,100))


###### figure 4: 



######## performance with different grid size


### grid size with user's time
#### suppose the density is 0.2 ,0.35 , 0.7
#### run 100 hundered times

N = 2^(2:10)
timings =sapply(N,
         function(n) {
           print(n)
           g = createBMLGrid(n,n,c(n^2*0.2/2,n^2*0.2/2))
           system.time(rumBMLGrid(100,g))
         })

timings_C =
  sapply(N,
         function(n) {
           print(n)
           g = createBMLGrid(n,n,c(n^2*0.2/2,n^2*0.2/2))
           system.time(CrunBMLGrid(g,100))
         })

dev.off()
plot(N, timings[1,], type = "p",
     xlab = "Grid size" , ylim = c(0,20) , 
     ylab = "User's Time (seconds)",pch=4,col=24,main="Grid Size vs. User's time")
lines(N,timings[1,],lty=2)
points(N,timings_C[1,],pch=18,col=58)
lines(N,timings_C[1,],lty=20)

legend("topleft",title="Different Code",c("R code"," C code"),
       pch=c(4,18),col=c(24,58),cex=0.75)



timings_0.35 = sapply(N,
                      function(n) {
                        print(n)
                        g = createBMLGrid(n,n,c(n^2*0.35/2,n^2*0.35/2))
                        system.time(rumBMLGrid(100,g))
                      })

timings_C_0.35 =
  sapply(N,
         function(n) {
           print(n)
           g = createBMLGrid(n,n,c(n^2*0.35/2,n^2*0.35/2))
           system.time(CrunBMLGrid(g,100))
         })


plot(N, timings_0.35[1,], type = "p",
     xlab = "Grid size",
     ylab = "User's Time (seconds)",pch=4,col=24,main="Grid Size vs. User's time")
lines(N,timings_0.35[1,],lty=2)
points(N,timings_C_0.35[1,],pch=18,col=58)
lines(N,timings_C_0.35[1,],lty=20)
legend("topleft",title="Different Code",c("R code","C code"),
       pch=c(4,18),col=c(24,58),cex=0.75)



timings_0.7 = sapply(N,
                      function(n) {
                        print(n)
                        g = createBMLGrid(n,n,c(n^2*0.7/2,n^2*0.7/2))
                        system.time(rumBMLGrid(100,g))
                      })

timings_C_0.7 =
  sapply(N,
         function(n) {
           print(n)
           g = createBMLGrid(n,n,c(n^2*0.7/2,n^2*0.7/2))
           system.time(CrunBMLGrid(g,100))
         })

plot(N, timings_0.7[1,], type = "p",
     xlab = "Grid size",
     ylab = "User's Time (seconds)",pch=4,col=24,main="Grid Size vs. User's time")
lines(N,timings_0.7[1,],lty=2)
points(N,timings_C_0.7[1,],pch=18,col=58)
lines(N,timings_C_0.7[1,],lty=20)
legend("topleft",title="Different Code",c("R code","C code"),
       pch=c(4,18),col=c(24,58),cex=0.75)

ratio = timings_0.7[1,]/timings_C_0.7[1,]


plot(N,ratio,xlab = "Grid Density",ylab = "ratio of user time(C/R)",,type="p",pch=15,
     main = "Ratio of user's time")
lines(N,ratio,lty=2)

######## different density 

density = seq(from=0.1,to=0.7,length.out = 13)

timing_density_128 =
  sapply(density,
         function(n) {
           print(n)
           g = createBMLGrid(128,128,c(red = ceiling(128^2*n/2),blue = ceiling(128^2*n/2)))
           system.time(rumBMLGrid(100,g))
         })


timings_density_C_128 =
  sapply(density,
         function(n) {
           print(n)
           g = createBMLGrid(128,128,c(red = ceiling(128^2*n/2),blue = ceiling(128^2*n/2)))
           system.time(CrunBMLGrid(g,100))
         })


plot(density, timing_density_128[1,], type = "p",
     xlab = "Different Densities", 
     ylab = "User's Time (seconds)",pch=4,col=24,main="Density vs. User's time",ylim = c(0,0.5))
lines(density,timing_density_128[1,],lty=2)
points(density,timings_density_C_128[1,],pch=18,col=58)
lines(density,timings_density_C_128[1,],lty=20)

legend("topleft",title="Different Code",c("R code"," C code"),
       pch=c(4,18),col=c(24,58),cex=0.75)

##### 64*64 grid size

timing_density_64 =
  sapply(density,
         function(n) {
           print(n)
           g = createBMLGrid(64,64,c(red = ceiling(64^2*n/2),blue = ceiling(64^2*n/2)))
           system.time(rumBMLGrid(100,g))
         })

timings_density_C_64 =
  sapply(density,
         function(n) {
           print(n)
           g = createBMLGrid(64,64,c(red = ceiling(64^2*n/2),blue = ceiling(64^2*n/2)))
           system.time(CrunBMLGrid(g,100))
         })


plot(density, timing_density_64[1,], type = "p",
     xlab = "Different Densities", 
     ylab = "User's Time (seconds)",pch=4,col=24,main="Density vs. User's time",ylim = c(0,0.3))
lines(density,timing_density_64[1,],lty=2)
points(density,timings_density_C_64[1,],pch=18,col=58)
lines(density,timings_density_C_64[1,],lty=20)
legend("topleft",title="Different Code",c("R code"," C code"),
       pch=c(4,18),col=c(24,58),cex=0.75)

##### 256*256 grid size

timing_density_256 =
  sapply(density,
         function(n) {
           print(n)
           g = createBMLGrid(256,256,c(red = ceiling(256^2*n/2),blue = ceiling(256^2*n/2)))
           system.time(rumBMLGrid(100,g))
         })

timings_density_C_256 =
  sapply(density,
         function(n) {
           print(n)
           g = createBMLGrid(256,256,c(red = ceiling(256^2*n/2),blue = ceiling(256^2*n/2)))
           system.time(CrunBMLGrid(g,100))
         })

plot(density, timing_density_256[1,], type = "p",
     xlab = "Different Densities", 
     ylab = "User's Time (seconds)",pch=4,col=24,main="Density vs. User's time",ylim = c(0,2.5))
lines(density,timing_density_256[1,],lty=2)
points(density,timings_density_C_256[1,],pch=18,col=58)
lines(density,timings_density_C_256[1,],lty=20)
legend("topleft",title="Different Code",c("R code"," C code"),
       pch=c(4,18),col=c(24,58),cex=0.75)


############## different iteration times

iteration = seq(1000,10000,1000)

timings_iteration =
  sapply(iteration,
         function(n) {
           print(n)
           g = createBMLGrid(128,128,c(red = ceiling(128^2*0.35/2),blue = ceiling(128^2*0.35/2)))
           system.time(rumBMLGrid(n,g))
         })


timings_iteration_C =
  sapply(iteration,
         function(n) {
           print(n)
           g = createBMLGrid(128,128,c(red = ceiling(128^2*0.35/2),blue = ceiling(128^2*0.35/2)))
           system.time(CrunBMLGrid(g,n))
         })

plot(iteration, timings_iteration[1,], type = "p",
     xlab = "Different iterations", 
     ylab = "User's Time (seconds)",pch=4,col=24,main="Iterations vs. User's time (R)")
lines(iteration,timings_iteration[1,],lty=2)

plot(iteration, timings_iteration_C[1,], type = "p",
     xlab = "Different iterations", 
     ylab = "User's Time (seconds)",pch=4,col=24,main="Iterations vs. User's time (C)")
lines(iteration,timings_iteration_C[1,],lty=2)
