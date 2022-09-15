
# 1) Install and load packages --------------------------------------------
# load NBDa via devtools - using R version 4.1.2
#install.packages("devtools")
library(devtools)
#install_github("whoppitt/NBDA")
library(NBDA)

# first load the packages above, 
# and then run the NBDAdatafunc_mod.R script at this point
source("CODE/NBDAdatafunc_mod_OADA_withspatial.R")
# then continue with the code below

# 2) Generate input data ------------------------------------------------------

#source( 'CODE/sleep_analysis/NBDAinputdataprepfunc.R' )
source( 'CODE/NBDAinputdataprepfunc_withspatial.R' )

# prepare the input data for the NBDA. If you are curious about what the arguments do, they are explained at the top of the source code 'CODE/sleep_analysis/NBDAinputdataprepfunc.R'
# Sonja: change both the NULL's for the 'dom' arguments to 'aggregated' if you want to include the dominance network and node attributes

# prepare_NBDA_input( contagion_type = 'waking', night_start = '21:00:00', night_end = '05:00:00', cosit_net_to_use_network = 'aggregated', cosit_net_to_use_node_attr = NULL, dom_net_to_use_network = 'aggregated', dom_net_to_use_node_attr = NULL, dup_times_allowable = F, randomize_ties = T, night_rerun_number = 1, nights_to_use = 1:23, input_seed = 234 )

prepare_NBDA_input_spatial( contagion_type = 'waking', night_start = '20:00:00', night_end = '05:00:00', cosit_net_to_use_network = 'aggregated', cosit_net_to_use_node_attr = NULL, dom_net_to_use_network = NULL, dom_net_to_use_node_attr = NULL, dup_times_allowable = F, randomize_ties = T, night_rerun_number = 1, nights_to_use = 1:20, min_tags_per_diffusion = 3, min_events_per_diffusion = 3, min_diffusion_duration = 3, input_seed = 234 )



# 3) Create NBDA Data Object ----------------------------------------------

type = "OADA"
ILVsinclude <- TRUE

# This loop prepares the nbdaData Objects for each 
for(n in 1:length(diffuse_net_list)){
  # Assign a label to each night (e.g "night_1")
  night <- paste("night_", n, sep="")
  # Extract the order of acquisition (integers correspond to the position of the individuals in the social cosit_network)
  
  order <- as.vector(order_of_acq[[n]])
  # Extract the time of acquisition 
  # (added some time 'jitter' to individuals waking up in the same minute to prevent errors - e.g. 3.1, 3.2, 3.3 minutes instead of all on 3) 
  time <- as.vector(time_of_acq[[n]])
  # Extract the duration (how many extra minutes was an individual awake for after waking up)
  duration <- durations[[n]]
  # Extract the presence matrix - individuals whose collar stopped working are considered as 'absent' during events and get an entry of 0 at those events
  pres_mat <- presence_matrices[[n]]
  # Times rounded to full minutes (needed to extract true ties below)
  time.rounded <- floor(time)
  # extract who was awake at the start of the diffusion
  demons <- as.vector(early_demonstrators_list[[n]])
  # Extract how long these 'demonstrators' were awake for
  demons.duration <- as.vector(as.numeric(early_demo_durations_list[[n]]))
  
  # Pepare individual-level variables (ILVs)
  ILVs.df <- node_attribute_list[[n]]
  
  # extract which ILVs to include
  which.ILVs <- as.vector(colnames(ILVs.df)[2:length(colnames(ILVs.df))])
  
  # make a new object to store the names of the ILVs
  ILVs <- as.vector(NULL)
  
  # and make a count for each ILV
  count <- 0
  
  if("sex" %in% which.ILVs){
    # sex - males are assigned 0.5, females -0.5
    sex <- ILVs.df$sex
    sex[sex=="male"] <- 0.5
    sex[sex=="female"] <- -0.5
    sex <- as.matrix(as.numeric(sex)) # needs to be a matrix with one row per individual
    # assign to global environmnet
    assign(paste("sex", night, sep="_"), sex, envir = .GlobalEnv)
    # add the name to the ILVs vector
    count <- count+1
    ILVs[count] <- "sex"
  }
  
  if("age_class_vet" %in% which.ILVs){
    #age - the first age class is 'adult' (0.5) or 'other' (-0.5)
    adult <- ILVs.df$age_class_vet
    adult[adult!="adult"] <- -0.5
    adult[adult=="adult"] <- 0.5
    adult <- as.matrix(as.numeric(adult))
    
    # age - the second age class is 'juvenile' (0.5) or 'other' (-0.5')
    juvenile <- ILVs.df$age_class_vet
    juvenile[juvenile!="juvenile"] <- -0.5
    juvenile[juvenile=="juvenile"] <- 0.5
    juvenile <- as.matrix(as.numeric(juvenile))
    
    assign(paste("adult", night, sep="_"), adult, envir = .GlobalEnv)
    assign(paste("juvenile", night, sep="_"), juvenile, envir = .GlobalEnv)
    
    # add adult to ILVs
    count <- count+1
    ILVs[count] <- "adult"  
    
    # add juvenile to ILVs
    count <- count+1
    ILVs[count] <- "juvenile"
  }
  
  if("eigen_cent" %in% which.ILVs){
    # eigenvector centrality - we standardize it by subtracting the mean (for better model convergence)
    eigenvect <- ILVs.df$eigen_cent
    eigenvect <- eigenvect-mean(eigenvect)
    eigenvect <- as.matrix(as.numeric(eigenvect)) # needs to be a matrix with one row per individual
    assign(paste("eigenvect", night, sep="_"), eigenvect, envir = .GlobalEnv)
    count <- count+1
    ILVs[count] <- "eigenvect"
    
  }
  
  if("dominance" %in% which.ILVs){
    dominance <- ILVs.df$dominance
    dominance <- dominance-mean(dominance)
    dominance <- as.matrix(as.numeric(dominance)) # needs to be a matrix with one row per individual
    assign(paste("dominance", night, sep="_"), dominance, envir = .GlobalEnv)
    count <- count+1
    ILVs[count] <- "dominance"
  }
  
  # The ILVs then need to be saved into a character vector
  ILVs_glob <- ILVs
  ILVs <- paste(ILVs, night, sep="_")
  assign(paste("ILVs", night, sep="_"), ILVs, envir = .GlobalEnv)
  assign("ILVs_glob", ILVs_glob, envir = .GlobalEnv)
  
  # here, we prepare the ties
  # if OADA:
  if(type=="OADA"){
    #true Ties - for individuals who woke up in the same minute, we don't know the order in which they work up
    # we can 'tie' these events together - NBDA then considers all possible combinations
    dup_inds <- which( duplicated( time.rounded ) | duplicated( time.rounded, fromLast =  T )) 
    dup_time.rounded <- time.rounded[ dup_inds ]
    dup_inds_list <- split( dup_inds, cumsum( c( 1, diff( dup_time.rounded ) ) != 0 ) )
    names( dup_inds_list ) <- NULL
    # these ties are saved in a list e.g. list(c(2,3), c(9, 10)) for tied events 2 and 3, and 9 and 10 
    
    if(length(dup_inds)==0){
      trueTies <- list(NULL)
    } else {trueTies <- dup_inds_list}
  } else if(type=="TADA"){
    if(isTRUE(ties_input)){
      # add ties for tada
      ties <- as.vector(0)
      for(i in 2:length(time.rounded)){
        if(time.rounded[i]==time.rounded[i-1]){
          ties[i] <- 1
        } else {
          ties[i] <- 0
        }
      }
    } else {
      ties <- NULL
    }
  }
  
  
  # As we have repeated measures of the same individuals, we incorporate random effects
  random.ID <- matrix(as.numeric(ILVs.df$tag, ncol=1))
  assign(paste("tags", night, sep="_"), random.ID, envir = .GlobalEnv)
  
  RE <- paste(c("tags"), night, sep="_")
  assign(paste("RE", night, sep="_"), RE, envir = .GlobalEnv)
  
  # Here we create the nbdaData object for each night
  
  if(type=="OADA"){
    object          <- nbdaData(label = night,
                                # labelled by night
                                assMatrix = diffuse_net_list[[n]],
                                #assMatrix = net,
                                # networks as matrix array (one slot per network)
                                #idname = rownames(diffuse_net_list[[n]]),
                                asoc_ilv = get(paste("ILVs", night, sep="_")),
                                #asoc_ilv = "ILVabsent",
                                # influence of ILVs on asocial learning - character vector with ILVs (e.g. ILVs_night_1)
                                int_ilv = get(paste("ILVs", night, sep="_")),
                                #int_ilv = "ILVabsent",
                                # influence of ILVs on social learning (independently estimated from influence on asocial learning)
                                multi_ilv = "ILVabsent",
                                # we are not running multiplicative models as they assume that ILVs influence asocial and sociall learning to the same extent
                                orderAcq = order,
                                # order acquisition vector
                                timeAcq = time,                                 
                                # time of acquisition vector (normally not needed for OADA - this only works with the adapted function)
                                durationAcq = duration,
                                # duration of the event (extra minutes awake) - this is part of the adapted function, individuals go back to sleep after the duration is over
                                #endTime = num_mins_in_diffusion + 1,
                                # calculate end time of diffusion (in minutes) +1
                                type = type,
                                # Specifying whether to run OADA or TADA - this only works with the adapted version
                                asocialTreatment = "constant",
                                # ILVs are treated as 'constant' over time (as opposed to time-varying)                             
                                #  ties = ties,
                                # Defining which events are tied together (those that woke up in the same minute- TADA only)
                                presenceMatrix = pres_mat,
                                # Presence matrix defining which  events individuals were available for (collar functioning)
                                demons = demons,
                                # a vector specifying who was awake (1) or asleep (0) as the diffusion starts
                                demons.duration = demons.duration,
                                # a vector specifying how many extra minutes demonstrators were awake for
                             #   random_effects = get(paste("RE", night, sep="_")),
                                  trueTies = trueTies
                                
    )
    # We need to adjust the nbda data objects (ONLY FOR OADA)
    # we need to remove the 'times' from the objects for the oadaFit function to recognize it as a OADA rather than a TADA object
    object@timeAcq <- NA 
    object@endTime <- as.numeric(NA)
    
  } else if(type=="TADA"){
    object          <- nbdaData(label = night,
                                # labelled by night
                                assMatrix = diffuse_net_list[[n]],
                                #assMatrix = net,
                                # networks as matrix array (one slot per network)
                                idname = rownames(diffuse_net_list[[n]]),
                                asoc_ilv = get(paste("ILVs", night, sep="_")),
                                #asoc_ilv = "ILVabsent",
                                # influence of ILVs on asocial learning - character vector with ILVs (e.g. ILVs_night_1)
                                int_ilv = get(paste("ILVs", night, sep="_")),
                                #int_ilv = "ILVabsent",
                                # influence of ILVs on social learning (independently estimated from influence on asocial learning)
                                multi_ilv = "ILVabsent",
                                # we are not running multiplicative models as they assume that ILVs influence asocial and sociall learning to the same extent
                                orderAcq = order,
                                # order acquisition vector
                                timeAcq = time,                                 
                                # time of acquisition vector (normally not needed for OADA - this only works with the adapted function)
                                durationAcq = duration,
                                # duration of the event (extra minutes awake) - this is part of the adapted function, individuals go back to sleep after the duration is over
                                #endTime = num_mins_in_diffusion + 1,
                                # calculate end time of diffusion (in minutes) +1
                                type = type,
                                # Specifying whether to run OADA or TADA - this only works with the adapted version
                                asocialTreatment = "constant",
                                # ILVs are treated as 'constant' over time (as opposed to time-varying)                             
                                ties = ties,
                                # Defining which events are tied together (those that woke up in the same minute)
                                presenceMatrix = pres_mat,
                                # Presence matrix defining which  events individuals were available for (collar functioning)
                                demons = demons,
                                # a vector specifying who was awake (1) or asleep (0) as the diffusion starts
                                demons.duration = demons.duration
                                # a vector specifying how many extra minutes demonstrators were awake for
                                #    random_effects = get(paste("RE", night, sep="_")),
                                
                                
    )
  }
  
  # save the nbda data object into the global environment
  assign(paste("nbdaData", night, sep="_"), object)
  
}

# Note: warning on 'Found more than one class "nbdaData"....'
# can be safely ignored


# oadatry.soc <- oadaFit(list(nbdaData_night_1, 
#                             nbdaData_night_2,
#                             nbdaData_night_3,
#                             nbdaData_night_4,
#                             nbdaData_night_5,
#                             nbdaData_night_6,
#                             #  nbdaData_night_7,
#                             nbdaData_night_8,
#                             nbdaData_night_9,
#                             nbdaData_night_10
# ), type="social")
# 
# 
# oadatry.asoc <- oadaFit(list(nbdaData_night_1, 
#                              nbdaData_night_2,
#                              nbdaData_night_3,
#                              nbdaData_night_4,
#                              nbdaData_night_5,
#                              nbdaData_night_6,
#                              #  nbdaData_night_7,
#                              nbdaData_night_8,
#                              nbdaData_night_9,
#                              nbdaData_night_10
# ), type="asocial")
# oadatry.soc@aicc
# oadatry.asoc@aicc
# 
# nbdaData_night_1@orderAcq
# nbdaData_night_2@orderAcq
# nbdaData_night_3@orderAcq#
# nbdaData_night_4@orderAcq
# nbdaData_night_5@orderAcq
# nbdaData_night_6@orderAcq
# nbdaData_night_7@orderAcq
# nbdaData_night_8@orderAcq#
# nbdaData_night_9@orderAcq
# nbdaData_night_10@orderAcq
# 
# nbdaData_night_1@assMatrix
# nbdaData_night_2@assMatrix
# nbdaData_night_3@assMatrix#
# nbdaData_night_4@assMatrix
# nbdaData_night_5@assMatrix
# nbdaData_night_6@assMatrix
# nbdaData_night_7@assMatrix
# nbdaData_night_8@assMatrix#
# nbdaData_night_9@assMatrix
# nbdaData_night_10@assMatrix
# 
# nbdaData_night_1@trueTies
# nbdaData_night_2@trueTies
# nbdaData_night_3@trueTies#
# nbdaData_night_4@trueTies
# nbdaData_night_5@trueTies
# nbdaData_night_6@trueTies
# nbdaData_night_7@trueTies
# nbdaData_night_8@trueTies#
# nbdaData_night_9@trueTies
# nbdaData_night_10@trueTies
# 
# nbdaData_night_1@timeAcq
# nbdaData_night_2@timeAcq
# nbdaData_night_3@timeAcq#
# nbdaData_night_4@timeAcq
# nbdaData_night_5@timeAcq
# nbdaData_night_6@timeAcq
# nbdaData_night_7@timeAcq
# nbdaData_night_8@timeAcq#
# nbdaData_night_9@timeAcq
# nbdaData_night_10@timeAcq
# 
# nbdaData_night_4@random_effects
# 
# 
# nbdaData_night_2@assMatrix
# 


# we have to create a list with the names of all nbda data objects
# we add a function that automatically removes those objects that have fewer than 3 events (can happen if events get 'tied' together)
nbdaData.list <- list(NULL)
count <- 1
for(i in 1:length(diffuse_net_list)){
  name <- paste("nbdaData_night_", i, sep="")
# get number of actual events (that are not tied)
    if(length(get(name)@orderAcq)-(lengths(get(name)@trueTies)-1)>=3){
      nbdaData.list[[count]] <- name
      count <- count+1      
    }
}


# 4) Create constraints vector matrix -------------------------------------
# this function create the constraints vector matrix
source("CODE/sleep_analysis/NBDAconstraintsVectMatrixFun.R")

# run the function using a nbda data object (it is set up to use the first in the nbdaDat.list)
# it specifies which models to run (i.e. which parameters to estimate in which model)
# see output explained below
constraintsVectMatrix <-
  create.constraints.Vect.Matrix(
    NBDA_data_object = get(nbdaData.list[[1]]),
    num_networks = dim(nbdaData_night_1@assMatrix)[3],
    num_ILVs = length(ILVs_glob)  )

# We assign column names for easier interpretation
# first the ILVs
colnames(constraintsVectMatrix)[(length(colnames(constraintsVectMatrix))-length(ILVs_glob)*2+1):length(colnames(constraintsVectMatrix))] <- c(paste("asoc", ILVs_glob, sep="_"), paste("soc", ILVs_glob, sep="_"))
# then the network(s)
colnames(constraintsVectMatrix)[1:dim(nbdaData_night_1@assMatrix)[3]] <- paste("network", 1:dim(nbdaData_night_1@assMatrix)[3], sep="_")

# Let's look at the matrix:
head(constraintsVectMatrix)
# Each row corresponds to one model, columns to parameters
# Entries of >0 indicate that the parameter is estimated in that particular model
# Entries of0 mean that the parameter is constrained to 0 in that particular model
# The first column refers to the social learning parameter s - entries of 1 mean that it is a social model, 0 an asocial model
# asoc_sex = sex influencing the asocial learning rate
# asoc_adult = age influencing the asocial learning rate (as adults versus other age classes)
# asoc_juvenile = age influencing the asocial learning rate (as juveniles versus other age classes)
# asoc_eigenvect = eigenvector centrality influencing the asocial learning rate
# soc_sex = sex influencing the social learning rate
# soc_adult = age influencing the social learning rate (as adults versus other age classes)
# soc_juvenile = age influencing the social learning rate (as juveniles versus other age classes)
# soc_eigenvect = eigenvector centrality influencing the social learning rate
# We are estimating all parameters independently (they all have different integers assigned)
# We could in theory constrain them to be the same by setting the same integers, but we are not interested in this here

# Let's look at a specific example - model 250
constraintsVectMatrix[250,]
# network       asoc_sex     asoc_adult  asoc_juvenile asoc_eigenvect        soc_sex      soc_adult   soc_juvenile  soc_eigenvect 
# 1              0              0              0              0              0              2              0              0

class(constraintsVectMatrix)

# This model is a social model (network=1) and has age (as adults or other) influencing the social learning rate, all other parameters are set to 0
# How many models are we fitting in total?
dim(constraintsVectMatrix)
# 272


# 5) Run OADA -------------------------------------------------------------
# Now we can run OADA with multiple diffusions (one for each night)
# Detect how many cores are available and set below (cores)
detectCores()
cores <- 6

# run AIC function
OADA_output <- oadaAICtable(nbdadata = 
                            lapply(nbdaData.list, get),
                            constraintsVectMatrix = constraintsVectMatrix, 
                            writeProgressFile = F,
                            cores = cores)


# dir.create( paste0( getwd(), '/RESULTS' ) )
# dir.create( paste0( getwd(), '/RESULTS/NBDA' ) )
#saveRDS( OADA_output, 'RESULTS/NBDA/OADA_trueties.rds' )

OADA_output <- readRDS( 'RESULTS/NBDA/OADA_trueties.rds' )


# 6) Look at summary output: ----------------------------------------------
## IMPORTANT: for looking at model summary and for extracting effect sizes, set name of OADA object to 'OADA_output'

# First the AIC table - best performing models are at the top
print(OADA_output@printTable)
# we can see that most models did not converge (too many parameters for the data)

##Create a new object with a printTable that excludes unfitted model
OADA_output_new<-OADA_output
OADA_output_new@printTable<-OADA_output@printTable[!is.nan(OADA_output@printTable$aicc)&!is.na(OADA_output@printTable$aicc),]

OADA_output_new@aicc<-OADA_output@aicc[!is.nan(OADA_output@aicc)&!is.na(OADA_output@aicc)]
OADA_output_new@MLEs<-OADA_output@MLEs[!is.nan(OADA_output@aicc)&!is.na(OADA_output@aicc),]
OADA_output_new@MLEilv<-OADA_output@MLEilv[!is.nan(OADA_output@aicc)&!is.na(OADA_output@aicc),]
OADA_output_new@MLEint<-OADA_output@MLEint[!is.nan(OADA_output@aicc)&!is.na(OADA_output@aicc),]


OADA_output_new@printTable<-OADA_output_new@printTable[order(OADA_output_new@printTable$aicc),]
OADA_output_new@printTable$deltaAICc<-OADA_output_new@printTable$aicc-OADA_output_new@printTable$aicc[1]

# calculate support for fitted models
OADA_output_new@printTable$RelSupport<- exp(-0.5*OADA_output_new@printTable$deltaAICc)
OADA_output_new@printTable$AkaikeWeight<-OADA_output_new@printTable$RelSupport/sum(OADA_output_new@printTable$RelSupport)


OADA_output_new@deltaAIC<-OADA_output_new@aicc-min(OADA_output_new@aicc)

OADA_output_new@RelSupport<- exp(-0.5*OADA_output_new@deltaAIC)
OADA_output_new@AkaikeWeight<-OADA_output_new@RelSupport/sum(OADA_output_new@RelSupport)

dim(OADA_output@printTable)[1]-dim(OADA_output_new@printTable)[1]
dim(OADA_output@printTable)[1]

##194 models could not be fitted out of 200

# we re-assign the OADA output to the 'old' object name
OADA_output <- OADA_output_new

# Extract network support
networksSupport(OADA_output)
# the first column shows the combination of social networks (0:0 for asocial models; 1:0 for co-sitting network only; 0:2 for spatial network only, etc)

# here we see the support (again in summed Akaike weights) for the different ILVs influencing asocial and social learning rates
# we take those with support >0.5 as important (more support than against)
variableSupport(OADA_output)

# extracting effect sizes: model averaged estimates
# we take the median as a more stabilized inference, as the means can sometimes be skewed due to highly asymmetrical profile likelihoods
mle <- modelAverageEstimates(OADA_output , averageType = "median")


# 7) Extract effect sizes -------------------------------------------------

# we need to create a constrained NBDA data object for each diffusion based on the best performing model that includes social transmission
# we extract this from the AIC table from the OADA object (top model by AICc that is not asocial)
OADA_output@printTable

best.model <- as.numeric(rownames(OADA_output@printTable[OADA_output@printTable$type!="asocial",])[1])

# we can see which model that is by looking at the constraints vector matrix

constraintsVectMatrix[best.model,]


# 7.1 Create constrained NBDA data objects --------------------------------

# we loop through the nbda data list and create a constrained object for each
nbdaData.list.constrained <- list(NULL)
for(i in 1:length(nbdaData.list)){
  best.model.data <- constrainedNBDAdata(nbdadata = get(nbdaData.list[[i]]), constraintsVect = constraintsVectMatrix[best.model,])
  assign(paste("best.model.data_night_", i, sep=""), best.model.data , envir = .GlobalEnv)
  
  name <- paste("best.model.data_night_", i, sep="")
  nbdaData.list.constrained[[i]] <- name
  
}


# 7.2. Run OADA (best model only) -----------------------------------------

# we then run OADA on that best model using the constrained NBDA data objects
model.best.social <-
  oadaFit(
    nbdadata = lapply(nbdaData.list.constrained, get),
    type= "social"
  )

# we can have a first look at the output parameters
best.model.output.table <- cbind.data.frame(model.best.social@varNames, model.best.social@outputPar, model.best.social@se)


#dir.create( paste0( getwd( ), "/DATA/sleep_analysis_data/NBDA_data" ) )

#save.image( "DATA/sleep_analysis_data/NBDA_data/best_model_checkpoint.RData" )

#load( "DATA/sleep_analysis_data/NBDA_data/best_model_checkpoint.RData" )

# 7.3. Extract effect size for s ------------------------------------------

# first we extract the likelihood of an event occurring through social learning
prop.solve.social.byevent <-
  oadaPropSolveByST.byevent(
    nbdadata =  lapply(nbdaData.list.constrained, get),
    model = model.best.social
  )
#let's look at the output: each row is an event (eventID) and p(Network) gives a likelihood of that particular event having occurred through social learning
prop.solve.social.byevent 


# ## test with linear model and random effect
# # plot the data to model
# hist( prop.solve.social.byevent$`P(Network 1)`, breaks = 300 )
# hist( prop.solve.social.byevent$`P(Network 2)`[ prop.solve.social.byevent$`P(Network 2)` > 0], breaks = 500 )
# 
# 
# 
# mean( prop.solve.social.byevent$`P(Network 2)`  == 0 )

# next we extracts the overall percentage of events that occurred socially
prop.solve.social <-
  oadaPropSolveByST(
    nbdadata = lapply(nbdaData.list.constrained, get),
    model = model.best.social
  )
prop.solve.social 

# 7.5. Extracting confidence intervals for s ------------------------------

# standard errors for parameters resulting from NBDA are often misleading due to their highly asymmetrical nature of their profile likelihood
# as in most cases, we have more information on the lower bound than the upper bound (particularly for s)

# profile likelihood curves for each parameter can be plotted using the plotProfLik function and setting the 'which' argument
# which refers to the position of the parameter in the best model
# it can be checked in the model object: 
model.best.social@varNames

# for s:
plotProfLikTrueTies(which=1, model= model.best.social,range=c(0,50), resolution=10)

# the best model has no ILVs - otherwise we would continue here with setting which=2, which=3 etc (for the number of parameters in that model)

# we extract the confidence intervals
# the lower includes 0
CIs <- profLikCITrueTies(which=1, model= model.best.social, lower=NA, upper=c(30,40), interval=c(30,40), conf = 0.95)

CIs


# we now want these values in % as well
# To get the estimates for the lower bound 
# we have to compute the corresponding values of the other parameters for that model if s is constrained to the value of the lower bound 
# we do this by running an asocial model, but specifying an offset that takes the value of the lower bound

# specify the number of ILVs (in original analysis)
numILVs <- 3


constraintsVectMatrix[best.model, ]

nbdaData.list.constrained.lower.bound <- list(NULL)
for(i in 1:length(nbdaData.list)){
  best.model.data.lower <-
    constrainedNBDAdata(nbdadata = get(nbdaData.list[[i]]),
                        constraintsVect = constraintsVectMatrix[best.model, ],
                        offsetVect = as.vector(c( CIs[1], 0, rep(0, numILVs*2))))
  assign(paste("best.model.data.lower.bound_night_", i, sep=""), best.model.data.lower , envir = .GlobalEnv)
  name <- paste("best.model.data.lower.bound_night_", i, sep="")
  nbdaData.list.constrained.lower.bound[[i]] <- name
  
}

#Now, when we fit an "asocial" model it constrains the value of s=0
# but then the value of s at the lower bound is added to s as an offset
best.model.lower.bound <-
  oadaFit(
    nbdadata = lapply(nbdaData.list.constrained.lower.bound, get),
    type = "social"
  )

# We plug the resulting model into the prop solve function to get %
prop.solve.social.lower.bound <-
  oadaPropSolveByST(
    model = best.model.lower.bound,
    nbdadata = lapply(nbdaData.list.constrained.lower.bound, get)
  )
prop.solve.social.lower.bound

# we now repeat the same procedure for the upper bound

nbdaData.list.constrained.upper.bound <- list(NULL)
for(i in 1:length(nbdaData.list)){
  best.model.data.upper <-
    constrainedNBDAdata(nbdadata = get(nbdaData.list[[i]]),
                        constraintsVect = c( 0,1,2,3,0,4,0,0 ),
                        offsetVect = as.vector(c( CIs[2], 0, rep(0, numILVs*2))))
  assign(paste("best.model.data.upper.bound_night_", i, sep=""), best.model.data.upper , envir = .GlobalEnv)
  name <- paste("best.model.data.upper.bound_night_", i, sep="")
  nbdaData.list.constrained.upper.bound[[i]] <- name
  
}

#Now, when we fit an "asocial" model it constrains the value of s=0
# but then the value of s at the upper bound is added to s as an offset
best.model.upper.bound <-
  oadaFit(
    nbdadata = lapply(nbdaData.list.constrained.upper.bound, get),
    type = "social"
  )

# We plug the resulting model into the prop solve function to get %
prop.solve.social.upper.bound <-
  oadaPropSolveByST(
    model = best.model.upper.bound,
    nbdadata = lapply(nbdaData.list.constrained.upper.bound, get)
  )
prop.solve.social.upper.bound




#### repeat for other network ####



nbdaData.list.constrained.lower.bound <- list(NULL)
for(i in 1:length(nbdaData.list)){
  best.model.data.lower <-
    constrainedNBDAdata(nbdadata = get(nbdaData.list[[i]]),
                        constraintsVect = c( 1,0,2,3,0,4,0,0 ),
                        offsetVect = as.vector(c( 0, CIs2[1], rep(0, numILVs*2))))
  assign(paste("best.model.data.lower.bound_night_", i, sep=""), best.model.data.lower , envir = .GlobalEnv)
  name <- paste("best.model.data.lower.bound_night_", i, sep="")
  nbdaData.list.constrained.lower.bound[[i]] <- name
  
}

#Now, when we fit an "asocial" model it constrains the value of s=0
# but then the value of s at the lower bound is added to s as an offset
best.model.lower.bound <-
  tadaFit(
    nbdadata = lapply(nbdaData.list.constrained.lower.bound, get),
    type = "social"
  )

# We plug the resulting model into the prop solve function to get %
prop.solve.social.lower.bound <-
  oadaPropSolveByST(
    model = best.model.lower.bound,
    nbdadata = lapply(nbdaData.list.constrained.lower.bound, get)
  )
prop.solve.social.lower.bound

# we now repeat the same procedure for the upper bound

nbdaData.list.constrained.upper.bound <- list(NULL)
for(i in 1:length(nbdaData.list)){
  best.model.data.upper <-
    constrainedNBDAdata(nbdadata = get(nbdaData.list[[i]]),
                        constraintsVect = c( 1,0,2,3,0,4,0,0 ),
                        offsetVect = as.vector(c( 0, CIs2[2], rep(0, numILVs*2))))
  assign(paste("best.model.data.upper.bound_night_", i, sep=""), best.model.data.upper , envir = .GlobalEnv)
  name <- paste("best.model.data.upper.bound_night_", i, sep="")
  nbdaData.list.constrained.upper.bound[[i]] <- name
  
}

#Now, when we fit an "asocial" model it constrains the value of s=0
# but then the value of s at the upper bound is added to s as an offset
best.model.upper.bound <-
  tadaFit(
    nbdadata = lapply(nbdaData.list.constrained.upper.bound, get),
    type = "social"
  )

# We plug the resulting model into the prop solve function to get %
prop.solve.social.upper.bound <-
  oadaPropSolveByST(
    model = best.model.upper.bound,
    nbdadata = lapply(nbdaData.list.constrained.upper.bound, get)
  )
prop.solve.social.upper.bound




# 7.4. Extract confidence intervals for ILVs ------------------------------

# we repeat the same procedure as for s with one difference - we take the parameter estimate from the model averaged estimates


# 7.4.1 Sex on asocial rate -----------------------------------------------
mle
sex.asoc.estimate <- mle[3]
sex.asoc.se <- subset(best.model.output.table$'model.best.social@se', best.model.output.table$`model.best.social@varNames`=="3 Asocial: sex_night_1")


CIs.sex.asoc <- c(sex.asoc.estimate-(1.96*sex.asoc.se), sex.asoc.estimate+(1.96*sex.asoc.se))
# back-transform, as on log-scale
round( exp(c(sex.asoc.estimate, CIs.sex.asoc)), 2 )

# the first output corresponds to the parameter estimate, the seccond to the lower bound and the third to the upper bound

# 7.4.1 Age (adult or other) on asocial rate -----------------------------------------------

adult.asoc.estimate <- mle[4]
adult.asoc.se <- subset(best.model.output.table$'model.best.social@se', best.model.output.table$`model.best.social@varNames`=="4 Asocial: adult_night_1")


CIs.adult.asoc <- c(adult.asoc.estimate-(1.96*adult.asoc.se), adult.asoc.estimate+(1.96*adult.asoc.se))
# back-transform, as on log-scale
round( exp(c(adult.asoc.estimate, CIs.adult.asoc)), 2 )


# 7.4.1 Sex on social rate -----------------------------------------------

adult.asoc.estimate <- mle[6]
adult.asoc.se <- subset(best.model.output.table$'model.best.social@se', best.model.output.table$`model.best.social@varNames`=="5 Social: sex_night_1")


CIs.adult.asoc <- c(adult.asoc.estimate-(1.96*adult.asoc.se), adult.asoc.estimate+(1.96*adult.asoc.se))
# back-transform, as on log-scale
round( exp(c(adult.asoc.estimate, CIs.adult.asoc)), 2 )


# 7.4.1 Age (adult or other) on social rate -----------------------------------------------

adult.asoc.estimate <- mle[7]
adult.asoc.se <- subset(best.model.output.table$'model.best.social@se', best.model.output.table$`model.best.social@varNames`=="6 Social: adult_night_1")


CIs.adult.asoc <- c(adult.asoc.estimate-(1.96*adult.asoc.se), adult.asoc.estimate+(1.96*adult.asoc.se))
# back-transform, as on log-scale
round( exp(abs( c(adult.asoc.estimate, CIs.adult.asoc))), 2 )


# 7.4.1 Juvenile (adult or other) on social rate -----------------------------------------------

adult.asoc.estimate <- mle[8]
adult.asoc.se <- subset(best.model.output.table$'model.best.social@se', best.model.output.table$`model.best.social@varNames`=="7 Social: juvenile_night_1")


CIs.adult.asoc <- c(adult.asoc.estimate-(1.96*adult.asoc.se), adult.asoc.estimate+(1.96*adult.asoc.se))
# back-transform, as on log-scale
round( exp(c(adult.asoc.estimate, CIs.adult.asoc)), 2 )





## merge age, sex, eigenvector centrality, and tag data in to the data to model

library( stringr )
library( plyr )
library( lme4 )
library( LaplacesDemon )


# load in the daytime co-sitting network data (this data came from Roi)
load( 'DATA/social_data/cosit_2019.RData' )
# cosit_all - overall rate of co-sitting (26x26)
# cosit_daily - day by day rate of co-sitting (26x26x30)
# tgs - tag IDs (26)
# days - dates (30)

dimnames( cosit_all ) <- list( tgs, tgs )
dimnames( cosit_daily ) <- list( tgs, tgs, as.character( as.Date(  days ) ) )

# load in the displacement data


displace_dat <- read.csv( 'DATA/social_data/Displace.csv', header = F )

displace_dat <- as.matrix( displace_dat )

rownames( displace_dat ) <- tgs
colnames( displace_dat ) <- tgs

diag( displace_dat ) <- 0



# turn the network into a graph object in igraph
g <- graph_from_adjacency_matrix( cosit_all, mode = 'undirected', weighted = T )

# calculate the eigenvector centralities and save them in a dataframe
eigen_cent_vec <- eigen_centrality( g, weights = E(g)$weight )$vector

centrality_dat <- data.frame( tag = names( eigen_cent_vec ), eigen_cent = eigen_cent_vec )

identical( centrality_dat$tag, rownames( displace_dat ) )

centrality_dat$out_dom <- rowSums( displace_dat, na.rm = T )
centrality_dat$in_dom <- colSums( displace_dat, na.rm = T )

centrality_dat$net_dom <- rowSums( displace_dat, na.rm = T ) - colSums( displace_dat, na.rm = T )


rowSums( displace_dat, na.rm = T ) - colSums( displace_dat, na.rm = T )

rowSums( displace_dat - t( displace_dat ) )

summary( centrality_dat$eigen_cent ~ centrality_dat$net_dom )

# create dataframe replicate of prop.solve.social.byevent with tag name and diffusion number as their own columns

re_mod_dat_temp <- prop.solve.social.byevent

re_mod_dat_temp$logit_p_network1 <- logit( prop.solve.social.byevent$`P(Network 1)` + 0.01 )
hist( re_mod_dat_temp$logit_p_network1 )

re_mod_dat_temp$logit_p_network2 <- logit( prop.solve.social.byevent$`P(Network 2)` + 0.01 )
hist( re_mod_dat_temp$logit_p_network2 )


re_mod_dat_temp$tag <- gsub( 'X', '', str_split_fixed( row.names( re_mod_dat_temp ), '\\.', 2 )[ , 1 ] )
re_mod_dat_temp$tag <- substr( re_mod_dat_temp$tag, 1 , 4 )
#re_mod_dat_temp$diffus_num <- as.numeric( str_split_fixed( re_mod_dat_temp$eventID, '_', 3 )[ , 2 ] )

prop.solve.social.byevent[ which( !re_mod_dat_temp$tag %in% node_attribute_list[[ 1  ]]$tag ), ]

# # add the diffusion number information to the node attribute dataframes and add them to a new list
# new_node_attribute_list <- vector( 'list', length = length( node_attribute_list ) )
# for( li in 1:length( node_attribute_list ) ){
#   
#   temp_node_df <- node_attribute_list[[ li ]]
#   
#   temp_node_df$diffus_num <- li 
#   
#   new_node_attribute_list[[ li ]] <- temp_node_df 
# }

# combine the list elements into a one large dataframe with the node attributes for each diffusion, which we will merge into re_mod_dat_temp
#total_node_att_df <- ldply( new_node_attribute_list )

# merge the dataframes
re_mod_dat <- merge( x = re_mod_dat_temp, y = centrality_dat, by = c( 'tag' ), all.x = T, all.y = F, sort = F  )

# write.csv( re_mod_dat, 'DATA/sleep_analysis_data.csv', row.names = F )

mod_1_no_re <- lm( re_mod_dat$logit_p_network1 ~  re_mod_dat$out_dom + re_mod_dat$eigen_cent )
summary( mod_1_no_re )
# 
# mod_1_with_re <- lmerTest::lmer( re_mod_dat$logit_p_network1 ~ re_mod_dat$out_dom + re_mod_dat$eigen_cent + ( 1| re_mod_dat$tag ) )
# summary( mod_1_with_re )

mod_1_no_re <- lm( re_mod_dat$logit_p_network1 ~  re_mod_dat$in_dom + re_mod_dat$eigen_cent )
summary( mod_1_no_re )

mod_1_no_re <- lm( re_mod_dat$logit_p_network1 ~  re_mod_dat$in_dom + re_mod_dat$out_dom + re_mod_dat$eigen_cent )
summary( mod_1_no_re )

mod_1_no_re <- lm( re_mod_dat$logit_p_network1 ~  re_mod_dat$net_dom + re_mod_dat$eigen_cent )
summary( mod_1_no_re )

summary( re_mod_dat$net_dom )

mod_1_no_re <- lm( re_mod_dat$logit_p_network1 ~ re_mod_dat$eigen_cent )
summary( mod_1_no_re )

mod_1_with_re <- lmerTest::lmer( re_mod_dat$logit_p_network1 ~ re_mod_dat$eigen_cent + ( 1| re_mod_dat$tag ) )
summary( mod_1_with_re )


mod_1_with_re <- glm( re_mod_dat$`P(Network 2)` ~ re_mod_dat$net_dom, family = 'binomial' )
summary( mod_1_with_re )


hist( resid( mod_1_with_re ) )
qqnorm( resid( mod_1_with_re ) )

####

### how many waking events happen socially?

na_sum <- function( x ) if( sum( !is.na( x ) ) == 0 ) return( NA ) else return( sum( x, na.rm = T ) )

nights <- unique( state_change_df$night )

nights <- nights[ - c( 1, 30 ) ]

running_vec <- c()

for( night in nights ){
  
  night_dat <- state_change_df[ state_change_df$night == night, ]
  
  for( col in 1:26 ){
    
    
    print( sum( night_dat[ , col ] == 1, na.rm = T ) )
    
    
    running_vec <- c( running_vec, na_sum( night_dat[ , col ] == 1 ) )
    
  }
}

mean( running_vec, na.rm = T)*0.136

