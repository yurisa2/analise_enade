
################################################################################
# FILENAME :        functions.R
#
# DESCRIPTION :
#       Function repository for system
#
# PUBLIC FUNCTIONS :
#       To-Do
#
#
# NOTES :
#
#
#
#
#
# AUTHOR :    @yurisa2        START DATE :    01 Oct 18
#
# CHANGES :
#
#
#
#
################################################################################

library("FuzzyR")
library("caret")

# Functions

# Simple normalization, 0..1 - input is vector
normalize <- function(x) {
  return ((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}

# Expand grid based on reps
new.expand.grid <- function(input, reps) {
  new_grid <- expand.grid(replicate(reps, input, simplify = FALSE))
  return(new_grid)
}


# Analyze and create the fuzzy rules, using weights from previous functions
create_fuzzy_rules <- function(dataset,features) {

    total_col <- new.expand.grid(c(1,2,3,4,5),as.integer(length(features)))

    res_row <- NULL
    for(j in 1:nrow(total_col)) {
      res_row[j] = sum(total_col[j,])
    }


    res_row <- normalize(res_row)

    res_row[res_row >= 0.8] = 5
    res_row[res_row > 0.6 & res_row < 0.8 ] = 4
    res_row[res_row > 0.4 & res_row < 0.6 ] = 3
    res_row[res_row >= 0.2 & res_row < 0.4 ] = 2
    res_row[res_row < 0.2 ] = 1

    total_col <- cbind(total_col,res_row)
    total_col <- cbind(total_col,1,1)

    m <- as.matrix(total_col)

  return(m)
}
#
# ma_test <-matrix(0L, nrow = 5, ncol = 9)  # @nrussell
# ma_test[,2] <- c(1,2,3,4,5)
create_fuzzy_inputs <- function(fuzzy_model,dataset,features) {

  i_mf <- 1
  for(i in 1:length(features)) {
  fuzzy_model <- addvar(fuzzy_model,
                        "input",
                        colnames(dataset)[features[i]],
                        c(0,1) )

  fuzzy_model <- addmf(fuzzy_model,
                      "input",
                      i_mf,
                      paste0("DT",colnames(dataset)[features[i]]),
                      "trimf",
                      c(0, 0, 0.25))

  fuzzy_model <- addmf(fuzzy_model,
                      "input",
                      i_mf,
                      paste0("DP",colnames(dataset)[features[i]]),
                      "trimf",
                      c(0, 0.25, 0.5))

  fuzzy_model <- addmf(fuzzy_model,
                      "input",
                      i_mf,
                      paste0("NSR",colnames(dataset)[features[i]]),
                      "trimf",
                      c(0.25, 0.5, 0.75))

  fuzzy_model <- addmf(fuzzy_model,
                      "input",
                      i_mf,
                      paste0("CPA",colnames(dataset)[features[i]]),
                      "trimf",
                      c(0.5, 0.75, 1))

  fuzzy_model <- addmf(fuzzy_model,
                      "input",
                      i_mf,
                      paste0("CPL",colnames(dataset)[features[i]]),
                      "trimf",
                      c(0.75, 1, 1))
  i_mf <- i_mf + 1 # Increases the index for the MFs
    }
  return(fuzzy_model)
}
# Create Model and OutPut
# Add outputs to modem (pre-defined scale), 3 MFs
create_fuzzy_outputs <- function(fuzzy_model, plots = F){
  fuzzy_model <- addvar(fuzzy_model,"output","Output MODEL", c(0,100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Pouco","trimf", c(0, 25, 25))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Moderado","trimf", c(0, 25, 50))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Razoavel","trimf", c(25, 50, 75))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Elevado","trimf", c(50, 75, 100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Muito","trimf", c(75, 100, 100))
  return(fuzzy_model)
}


######## RETURNS 2col, [FIS0,FIS1] ########
# Creates two models, one for 0 and one for 1 and evaluate them.
# the RETURN is a DF with Eval0 and Eval1 (ranks for the probability of 0 or 1)
fuz_sis <- function(dataset,features,inputs){

  model <- newfis("model")
  model <- create_fuzzy_inputs(model,dataset,features)
  model <- create_fuzzy_outputs(model)
  rules <- create_fuzzy_rules(dataset,features)
  model <- addrule(model,rules)
  # showfis(model) # DEBUG

  print(rules) # DEBUG

  Ev <- evalfis(inputs,model)
  #
  return(Ev)
}
############################################
