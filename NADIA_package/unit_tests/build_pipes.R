library(testthat)
library(EMMA)

all_pipes <- grep(ls("package:EMMA"), pattern = "PipeOp", value = TRUE)

test_result <- lapply(all_pipes, function(p){
  
    eval_pipe <- eval(parse(text = p))
    pipe_imp <- eval_pipe$new()
    
    test_that("build_pipe", {
      expect_is(pipe_imp, "PipeOp")
    })
  
})

