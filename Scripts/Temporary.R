registerDoParallel(cores = 2)

pmap(list(inputs = unlist(model_inputs, recursive = FALSE)[1],
          niter = list(rep(50000, 1)),
          nburnin = list(rep(500, 1)),
          nchains = list(rep(2, 1)),
          scenario = list(rep(1, 1)),
          mat_num = list(rep("mat1", each = 1)),
          location = list(rep("./Data files/2x2/Baseline/baseline_result_", 1))), 
     run_model)

plan(multisession, workers = 4)

future_pmap(list(inputs = unlist(model_inputs, recursive = FALSE)[1:3],
                 niter = as.list(rep(50000, 3)),
                 nburnin = as.list(rep(500, 3)),
                 scenario = as.list(1:3),
                 mat_num = as.list(rep("mat1", 3)),
                 location = as.list(rep("./Data files/2x2/Baseline/baseline_result_", 3))), 
            run_model, .options = furrr_options(seed = TRUE,
                                                packages = c("nimble",
                                                             "nimbleEcology")))

library(parallel)

test_list <- list(inputs = unlist(model_inputs, recursive = FALSE)[1:3],
                  niter = as.list(rep(50000, 3)),
                  nburnin = as.list(rep(500, 3)),
                  scenario = as.list(1:3),
                  mat_num = as.list(rep("mat1", 3)),
                  location = as.list(rep("./Data files/2x2/Baseline/baseline_result_", 3)))

this_cluster <- makeCluster(4)

test <- parLapply(cl = this_cluster, X = test_list,
          fun = run_model)


load("./Data files/2x2/Baseline/baseline_result_1mat1.RData")

MCMCsummary(model_result)
