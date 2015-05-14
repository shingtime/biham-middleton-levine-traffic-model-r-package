################# STA 242 HW2 final version ##################
### Appendix C: R source code

#####Note: In the source file, all the 0 mean white color on the grid. All the 1 
#### represent red cars. All the 2 represent blue cars. These three constant are using
### to represent color.

###########  function to create initial grid  ############
createBMLGrid =function(r,c, ncars){  
  ## check if the cars' numbers are reasonable
  if (sum(ncars)/(r*c) > 1) return ("There are too many cars.")
  else{
    ## generate the empty grid
    grid = matrix(0, r, c)
    pos = sample(1:length(grid), sum(ncars))
    ## -1:blue;  1: red
    grid[pos] = sample(rep(c(2, 1), ceiling(ncars)))[seq(along = pos)]
  }
  ## s3 method to define grid's class
  class(grid) = c("BMLGrid", "matrix")
  grid
}

########## function to store the cars' coordinates ##########
## In this algorithms, all the cars will move simultaneouly
car_coordinate = function(matrix){
  ## check cars' location
  index = which(matrix!=0)
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
  car_info = car_coordinate(matrix) 
  ## those color's index we want
  indice = which(car_info$colors==color)
  ## create a matrix to store specific color's cars' location
  current_location = as.matrix(cbind(car_info$row_indice[indice],car_info$col_indice[indice]))
  colnames(current_location) = c("row_indice","col_indice")
  ## duplicate the current location matrix and use it to compute the next location
  possible_next_location = current_location  
  if(color==1){  
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
  matrix[possible_next_location[which(new_indice == 0),,drop=FALSE ]] = color
  ## change those cars' current locations' colors to blank
  matrix[current_location[which(new_indice == 0),,drop=FALSE ]] = 0
  matrix
}

########## function to get the final grid
## need to export all global functions and global variables
rumBMLGrid= function(numSteps,grid){
  # Register cluster
  for(i in 1:numSteps){
    ##move blue car
    grid = .move_cars(grid,2)
    ## move red car
    grid = .move_cars(grid,1)    
  }
  grid
}

######### C vrsion to move cars and get the final grid ##############

CrunBMLGrid = function(grid,numsteps){
  location = car_coordinate(grid)
  ### red cars indices
  red = as.matrix(location[which(location$colors==1),1:2])
  ### blue cars indices
  blue = as.matrix(location[which(location$colors==2),1:2])
  ### matrix to store cars that can move
  move = matrix(rep(0L,2*numsteps),ncol=2)
  result = .C("crunBMLGrid",grid,dim(grid),red,blue,nrow(red),
              nrow(blue),as.integer(numsteps),move)
  ### return the final grid
  result[[1]]
  
}
######## plot function: S3 method ###########
plot.BMLGrid = function(x,...){
  image_grid = matrix(match(x, c(0, 1, 2)),
                      nrow(x), ncol(x))
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

summary.BMLGrid = function(object,...){
  car_info_blue = car_coordinate(object)
  output_blue = .blue_summary(object,car_info_blue)
  ### red cars
  grid_red = .move_cars(object,2)
  car_info_red = car_coordinate(grid_red)
  output_red = .red_summary(grid_red,car_info_red)
  output = c(output_blue,output_red)
  names(output) = c("total-blue","move-blue","block-blue","velocity-blue","total-red","move-red","block-red","velocity-red")
  options(digits=4)
  output
}
######### function to summary blue car
.blue_summary = function(grid,car_info){
  indice = which(car_info$colors == 2)
  num_car = length(indice)
  current_row = car_info$row_indice[indice]
  current_col = car_info$col_indice[indice]
  next_col = current_col
  next_row = current_row - 1
  next_row[ next_row < 1 ]  = nrow(grid)
  new_possible_position = cbind(next_row,next_col)
  move = sum(grid[new_possible_position] == 0)
  block = num_car - move 
  velocity = move/num_car
  output = c(num_car,move,block,velocity)
  output
}
######## function to summary red car
.red_summary = function(grid,car_info){
  indice = which(car_info$colors == 1)
  num_car = length(indice)
  current_row = car_info$row_indice[indice]
  current_col = car_info$col_indice[indice]
  next_row = current_row
  next_col = current_col + 1
  next_col[ next_col > ncol(grid) ]  = 1
  new_possible_position = cbind(next_row,next_col)
  move = sum(grid[new_possible_position] == 0)
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
  red_indice = which(car_info$colors==1)
  blue_indice = which(car_info$colors==2)
  ## red cars: move to the right
  ## check if we can move the red cars
  for (i in red_indice){
    current_row = car_info$row_indice[i]
    current_col = car_info$col_indice[i] 
    next_row = current_row
    if(current_col==ncol(matrix)) next_col = 1
    else next_col = current_col +1
    ## change the position 
    if(matrix[next_row,next_col] ==0){
      matrix[next_row,next_col]=1
      matrix[current_row,current_col]=0
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
    if(matrix[next_row,next_col]==0){
      matrix[next_row,next_col]=2
      matrix[current_row,current_col]=0    
    }
  }
  matrix
}

.rumBMLGrid1= function(initial_grid,numSteps){
  for (i in 1:numSteps){
    update_grid = .move_cars1(initial_grid)
    update_grid = .move_cars1(update_grid)
    initial_grid = update_grid
  }
  initial_grid
}