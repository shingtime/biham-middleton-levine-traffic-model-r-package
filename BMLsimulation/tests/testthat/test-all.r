test_that("Package works!", {
  expect_that({
    g = createBMLGrid(3, 3, c(red = 3, blue = 2));
    g = matrix(c(0, 1, 1, 0, -1, 0, 1, -1, 0), nrow = 3, byrow = T);
    g = rumBMLGrid(1, g);
    g;
  }, equals(matrix(c(1, 1, 0, 0, -1, 0, 1, -1, 0), nrow = 3, byrow = T)))
  
  expect_that({
    g = createBMLGrid(3, 3, c(red = 3, blue = 2));
    g = matrix(c(0, 1, 1, 0, -1, 0, 1, -1, 0), nrow = 3, byrow = T);
    g = rumBMLGrid(5, g);
    g;
  }, equals(matrix(c(1, -1, 1, 0, -1, 0, 0, 0, 1), nrow = 3, byrow = T)))
  
  
  
})
  
  