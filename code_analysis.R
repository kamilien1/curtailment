# helper functions


# test a bit of how long a code takes to run
howLong <- function(expr) {
    
    start_time <- proc.time()
    expr
    end_time <- proc.time()
    print(end_time - start_time)
    
}

