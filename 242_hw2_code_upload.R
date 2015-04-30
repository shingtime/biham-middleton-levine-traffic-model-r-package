## Appendix 1: R source code to generate the package


################# STA 242 HW2 final version ##################
#set.seed(1001)
#### use some packages #######
library(grid)
library(ggplot2)

########### define global variables to represent colors ###########
white = 0
red = 1
blue = -1

###########  function to create initial grid  ############

createBMLGrid =function(r,c, ncars){  
  stopifnot(!is.na(ncars["red"]))
  stopifnot(!is.na(ncars["blue"]))
  ## check if the cars' numbers are reasonable
  if (sum(ncars)/(r*c) > 1) return ("There are too many cars.")
  else{
    ## generate the empty grid
    grid = matrix(white, r, c)
    pos = sample(1:length(grid), sum(ncars))
    grid[pos] = sample(rep(c(blue, red), ceiling(ncars)))[seq(along = pos)]
  }
  ## s3 method to define grid's class
  class(grid) = c("BMLGrid", "matrix")
  grid
}

########## function to store the cars' coordinates ##########
## In this algorithms, all the cars will move simultaneouly
.car_coordinate = function(matrix){
  ## check cars' location
  index = which(matrix!=white)
  ## row number with cars
  row_indice = row(matrix)[index]
  ## column number with cars
  col_indice = col(matrix)[index]
  ## each car's position
  position = t(rbind(row_indice, col_indice))
  ## cars' coordinates on the initial grid
  color = matrix[position]
  data.frame(row_indice=row_indice,col_indice=col_indice,colors = color)
}

########## function to move cars ###################
### the arguments color specifies what color cars we want to move
.move_cars = function (matrix,color){
  ## call car_coodinate function to extract cars coordinates
  car_info = .car_coordinate(matrix) 
  ## those color's index we want
  indice = which(car_info$colors==color)
  ## create a matrix to store specific color's cars' location
  current_location = as.matrix(cbind(car_info$row_indice[indice],car_info$col_indice[indice]))
  colnames(current_location) = c("row_indice","col_indice")
  ## duplicate the current location matrix and use it to compute the next location
  possible_next_location = current_location  
  if(color==red){  
    ## add one to all red cars' column indices-- move to the right
    possible_next_location[,"col_indice"] = possible_next_location[,"col_indice"] + 1
    ## check if the red cars move to the right border of the grid, if yes, change row indices to one
    possible_next_location[,"col_indice"][possible_next_location[,"col_indice"] > ncol(matrix)] = 1
  }
  ## blue cars:move upwards
  else{
    ## minus one to all blue cars' row indices-- move upwards
    possible_next_location[,"row_indice"] = possible_next_location[,"row_indice"] - 1
    ## check if the blue cars move to the upper border of the grid, if yes, change row indices to the
    ## number of columns of the initial grid.
    possible_next_location[,"row_indice"][possible_next_location[,"row_indice"] < 1] = nrow(matrix)
  }
  ## call change_color function to move the cars
  .change_color(current_location,possible_next_location,color,matrix)
}  

.change_color = function (current_location,possible_next_location,color,matrix){
  ## use all the new indices for the cars to subset the original grid
  new_indice = matrix[possible_next_location]
  ## check if the new positions are blank. If yes, change those blanks to the corresponding colors
  matrix[possible_next_location[which(new_indice == white),,drop=FALSE ]] = color
  ## change those cars' current locations' colors to blank
  matrix[current_location[which(new_indice == white),,drop=FALSE ]] = white
  matrix
}

########## function to get the final grid
## need to export all global functions and global variables
rumBMLGrid= function(numSteps,grid){
  # Register cluster
  for(i in 1:numSteps){
    grid = .move_cars(grid,blue)
    grid = .move_cars(grid,red)    
  }
  grid
}


######## plot function: S3 method ###########
plot.BMLGrid = function(initial_grid,...){
  image_grid = matrix(match(initial_grid, c(white, red, blue)),
                      nrow(initial_grid), ncol(initial_grid))
  ## the row number should be opposite
  image_grid = image_grid[c(nrow(image_grid):1),]
  ## get the correct order
  image(t(image_grid), col = c("white", "red", "blue"),axes=FALSE,...)
  box()
  
}

###### summary function : S3 method ###########
#### this function exports the given color's grid's some summary statistics.
#### 1. how many cars for this color
#### 1. how many cars can move 
#### 2. how many cars are blocked
#### 3. average velocity

summary.BMLGrid = function(grid,...){
  car_info_blue = .car_coordinate(grid)
  output_blue = .blue_summary(grid,car_info_blue)
  ### red cars
  grid_red = .move_cars(grid,blue)
  car_info_red = .car_coordinate(grid_red)
  output_red = .red_summary(grid_red,car_info_red)
  output = c(output_blue,output_red)
  names(output) = c("total-blue","move-blue","block-blue","velocity-blue","total-red","move-red","block-red","velocity-red")
  options(digits=4)
  output
}
######### function to summary blue car
.blue_summary = function(grid,car_info){
  indice = which(car_info$colors == blue)
  num_car = length(indice)
  current_row = car_info$row_indice[indice]
  current_col = car_info$col_indice[indice]
  next_col = current_col
  next_row = current_row - 1
  next_row[ next_row < 1 ]  = nrow(grid)
  new_possible_position = cbind(next_row,next_col)
  move = sum(grid[new_possible_position] == white)
  block = num_car - move 
  velocity = move/num_car
  output = c(num_car,move,block,velocity)
  output
}
######## function to summary red car
.red_summary = function(grid,car_info){
  indice = which(car_info$colors == red)
  num_car = length(indice)
  current_row = car_info$row_indice[indice]
  current_col = car_info$col_indice[indice]
  next_row = current_row
  next_col = current_col + 1
  next_col[ next_col > ncol(grid) ]  = 1
  new_possible_position = cbind(next_row,next_col)
  move = sum(grid[new_possible_position] == white)
  block = num_car - move 
  velocity = move/num_car
  output = c(num_car,move,block,velocity)
  output
}
########## function to plot a given series of densities' grids plot

.reproduce_plot = function(r,c, density, numSteps){
  car_num = sapply(density, function(x) round(r*c*x/2))
  initial = lapply(array(car_num),function(x)createBMLGrid(r, c, c(red = car_num[[x]], blue = car_num[[x]])))
  final = lapply(1:length(density), function(x) rumBMLGrid(numSteps,initial[[x]]))
  par(mfrow = c(2, 3), mar = c(1,1,1,1), pty = "s")
  sapply(1:length(density), function(x) plot(final[[x]], main = paste("density = ", density[x])))
}


######## this function is to get the average velocity over different density ##########
velocity_density = function(r,c,density,numSteps){
  car_num = sapply(density, function(x) round(r*c*x/2))
  initial = lapply(1:length(density),function(x)createBMLGrid(r, c, 
                                                              c(red = car_num[[x]], blue = car_num[[x]])))
  final = lapply(1:length(density),function(x){
    rumBMLGrid(numSteps,initial[[x]])
  })
  veo_summary = lapply(1:length(density),function(x){
    summary(final[[x]])
  })
  ## list to store red cars and blue cars' average velocity across different density
  veo_summary = do.call(rbind.data.frame,veo_summary)
  velocity = cbind((veo_summary[,2]+veo_summary[,6])/(veo_summary[,1]+veo_summary[,5]),density)
  colnames(velocity) = c("velocity","density")
  as.data.frame(velocity)
}


######### SLOWER VERSION OF move_cars & rumBMLGrid ##############################

.move_cars1 = function(matrix){
  car_info = car_coordinate(matrix)
  red_indice = which(car_info$colors==red)
  blue_indice = which(car_info$colors==blue)
  ## red cars: move to the right
  ## check if we can move the red cars
  for (i in red_indice){
    current_row = car_info$row_indice[i]
    current_col = car_info$col_indice[i] 
    next_row = current_row
    if(current_col==ncol(matrix)) next_col = 1
    else next_col = current_col +1
    ## change the position 
    if(matrix[next_row,next_col] ==white){
      matrix[next_row,next_col]=red
      matrix[current_row,current_col]=white
    }
  }
  ## blue cars:move upwards
  for (j in blue_indice){
    current_row = car_info$row_indice[j]
    current_col = car_info$col_indice[j]
    next_col = current_col
    if(current_row==1) next_row = nrow(matrix)
    else next_row = current_row - 1
    ## change the position
    if(matrix[next_row,next_col]==white){
      matrix[next_row,next_col]=blue
      matrix[current_row,current_col]=white     
    }
  }
  matrix
}

.rumBMLGrid1= function(initial_grid,numSteps){
  for (i in 1:numSteps){
    update_grid = move_cars1(initial_grid)
    update_grid = move_cars1(update_grid)
    initial_grid = update_grid
  }
  initial_grid
}


############### Appendix2 : R codes to generate plots in the report


##########   figure 1   ##########
### density from 0.2 to 0.7 & 0.31,0.32,0.39,0.4
g = createBMLGrid(256,256,c(256*256*0.2/2,256*256*0.2/2)) 
x = rumBMLGrid(5000,x)
plot(x)

#######

########   figure 2   #############

par(mfrow = c(2,3),mar=c(1,1,1,1),pty = "s")

g = createBMLGrid(144,289,c(144*289*0.38/2,144*289*0.38/2)) 
g1 = rumBMLGrid(5000,g)
x1 = rumBMLGrid(500,g1)
x2 = rumBMLGrid(1000,g1)
x3 = rumBMLGrid(1500,g1)
x4 = rumBMLGrid(2000,g1)
x5 = rumBMLGrid(2500,g1)
x6 = rumBMLGrid(3000,g1)

plot(x1,main="iteration:5500")

##########

##########  figure 3 #########
par(mfrow = c(2,3),mar=c(1,1,1,1),pty = "s")
#### density from 0.3 to 0.5
#### red:blue = 7:3
g = createBMLGrid(256,256,c(256*256*0.3*7/10,256*256*0.3*3/10)) 
x  = rumBMLGrid(5000,g)
plot(x)
### blue:red = 7:3
g = createBMLGrid(256,256,c(256*256*0.3*3/10,256*256*0.3*7/10))
x = rumBMLGrid(5000,g)
plot(x)

###########

#########   figure 4 & 5 ##########
density = seq(from=0.2,to=0.7,length.out=50)
### iteration time: 5000 or 1000
velocity_64 = velocity_density(64,64,density,1000)
velocity_128 = velocity_density(128,128,density,1000)
velocity_256 = velocity_density(256,256,density,1000)

plot(velocity_64[,2],velocity_64[,1],col=652,pch=20,title("Different grid size vs.density"),
     xlab="Density",ylab="Average velocity(move cars / total cars)")
lines(velocity_64[,2],velocity_64[,1],lty=2)
points(velocity_128[,2],velocity_128[,1],pch=18,col=58)
lines(velocity_128[,2],velocity_128[,1],lty=2)
points(velocity_256[,2],velocity_256[,1],pch=17,col=27)
lines(velocity_256[,2],velocity_256[,1],lty=2)
legend("topright",title="different grid size",c("64","128","256"),
       pch=c(20,18,17),col=c(652,58,27),cex=0.75)


###### rprofile #####

##### faster version ######
Rprof("/tmp/quick_BML.prof")
g = createBMLGrid(150,150,c(3000,3000))
x = rumBMLGrid(g,100)
Rprof(NULL)
head(summaryRprof("/tmp/quick_BML.prof")$by.self, 5)
time_quick = system.time(rumBMLGrid(g,100))

##### slow version #######
Rprof("/tmp/slow_BML.prof")
g = createBMLGrid(150,150,c(3000,3000))
x = rumBMLGrid1(g,100)
Rprof(NULL)
head(summaryRprof("/tmp/slow_BML.prof")$by.self, 5)
time_slow = system.time(rumBMLGrid1(g,100))

######## performance with different grid size


### grid size with user's time
#### suppose the density is 0.2 ,0.35 0.7 
#### run 100 hundered times
N = 2^(3:10)
timings =
  sapply(N,
         function(n) {
           print(n)
           g = createBMLGrid(n,n,c(n^2*0.2/2,n^2*0.2/2))
           system.time(rumBMLGrid(100,g))
         })

timings_0.35 = sapply(N,
                      function(n) {
                        print(n)
                        g = createBMLGrid(n,n,c(n^2*0.35/2,n^2*0.35/2))
                        system.time(rumBMLGrid(100,g))
                      })

timings_0.7 = sapply(N,
                     function(n) {
                       print(n)
                       g = createBMLGrid(n,n,c(n^2*0.7/2,n^2*0.7/2))
                       system.time(rumBMLGrid(100,g))
                     })


plot(N, timings[1,], type = "p",
     xlab = "Grid size",
     ylab = "User's Time (seconds)",pch=4,col=24,main="Grid Size vs. User's time")
lines(N,timings[1,],lty=2)
points(N,timings_0.35[1,],pch=18,col=58)
lines(N,timings_0.35[1,],lty=20)
points(N,timings_0.7[1,],pch=23,col=99)
lines(N,timings_0.7[1,],lty=14)
legend("topleft",title="Density",c("0.2","0.35","0.7"),
       pch=c(4,18,23),col=c(24,58,99),cex=0.75)

######## density with users time

density = seq(from=0.2,to=0.7,length.out=10)
timing_density_64 =
  sapply(density,
         function(n) {
           g = createBMLGrid(64,64,c(ceiling(64^2*n/2),ceiling(64^2*n/2)))
           system.time(rumBMLGrid(500,g))
         })

timing_density_128 =
  sapply(density,
         function(n) {
           g = createBMLGrid(128,128,c(ceiling(128^2*n/2),ceiling(128^2*n/2)))
           system.time(rumBMLGrid(500,g))
         })

timing_density_256 =
  sapply(density,
         function(n) {
           g = createBMLGrid(256,256,c(ceiling(256^2*n/2),ceiling(256^2*n/2)))
           system.time(rumBMLGrid(500,g))
         })

ggplot() + geom_line(data = time, aes(x = density, y = time[,1]))


plot(density, timing_density_256[1,],
     xlab = "density",
     ylab = "User's Time (seconds)",pch=4,col=24,main="density vs. User's time")
lines(density,timing_density_256[1,],lty=2)


