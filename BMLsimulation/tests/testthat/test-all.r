test_that("Package works!", {
  
  expect_that({
    g = createBMLGrid(3, 3, c(red = 3, blue = 2));
    g = matrix(c(0, 1, 1, 0, 2, 0, 1, 2, 0), nrow = 3, byrow = T);
    g = CrunBMLGrid(g, 5);
    g;
  }, equals(matrix(c(1, 2, 1, 0, 2, 0, 0, 0, 1), nrow = 3, byrow = T)))
  
  
  
})
  
  