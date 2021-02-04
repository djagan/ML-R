# Author    : Dr. D.Jagan
# Desc,     : Find-S Algorithm
#*********************************************************
#*************** Data initialization start
assertion_aatr_flag = "YES"
string_ANY = "?"
#*********************************************************
#*************** DATA SET Scenario 1
#*********************************************************
# number_of_vectors = 7
# hypothesis <- c('??','??','??','??','??')
# number_of_attributes = length(hypothesis)
# w1 <- c('GREEN','HARD','NO','WRINKLED','YES')
# w2 <- c('GREEN','HARD','YES','SMOOTH','NO')
# w3 <- c('BROWN','SOFT','NO','WRINKLED','NO')
# w4 <- c('ORANGE','HARD','NO','WRINKLED','YES')
# w5 <- c('GREEN','SOFT','YES','SMOOTH','YES')
# w6 <- c('GREEN','HARD','YES','WRINKLED','YES')
# w7 <- c('ORANGE','HARD','NO','WRINKLED','YES')
# data_mat <- array(c(w1,w2,w3,w4,w5,w6,w7),dim = c(number_of_attributes,number_of_vectors,1))
#*********************************************************
#*
#*************** DATA SET Scenario 2
#*********************************************************
number_of_vectors = 4
hypothesis <- c('??','??','??','??','??','??','??' )
number_of_attributes = length(hypothesis)
w1 <- c('sunny','warm','normal','strong','warm ','same','YES')
w2 <- c('sunny','warm','high','strong','warm ','same' ,'YES')
w3 <- c('rainy','cold','high','strong','warm ','change','NO')
w4 <- c('sunny','warm','high','strong','cool','change','YES')
data_mat <- array(c(w1,w2,w3,w4),dim = c(number_of_attributes,number_of_vectors,1))
#*********************************************************
#*********************************************************
#*#*********************************************************
#*
#*************** DATA SET Scenario 3
#*********************************************************
# number_of_vectors = 14
# hypothesis <- c('??','??','??','??')
# number_of_attributes = length(hypothesis)
# w1 <- c('HIGH','NO','FAIR','NO')
# w2 <- c('HIGH','NO','EXCLLENT','NO')
# w3 <- c('HIGH','NO','FAIR','YES')
# w4 <- c('MEDIUM','NO','FAIR','YES')
# w5 <- c('LOW','YES','FAIR','YES')
# w6 <- c('LOW','YES','EXCLLENT','NO')
# w7 <- c('LOW','YES','EXCLLENT','YES')
# w8 <- c('MEDIUM','NO','FAIR','NO')
# w9 <- c('LOW','YES','FAIR','YES')
# w10 <- c('MEDIUM','YES','FAIR','YES')
# w11 <- c('MEDIUM','YES','EXCLLENT','YES')
# w12 <- c('MEDIUM','NO','EXCLLENT','YES')
# w13 <- c('HIGH','YES','FAIR','YES')
# w14 <- c('MEDIUM','NO','EXCLLENT','NO')
# data_mat <- array(c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14),dim = c(number_of_attributes,number_of_vectors,1))
#*********************************************************
#*********************************************************
assertion_atr_index = number_of_attributes #The last attribute in an input vector
# Data initialization complete
#*
#*
#*
#*********************************************************
#*********************************************************
#*********************************************************
#*************** Find-S Algorithm processing start
#*********************************************************
is_hyp_initialized = 0 
# iterating through all the vectors
for (vector_counter in 1:number_of_vectors) {
  # print(vector_counter)
    current_vector = data_mat[,vector_counter,1]

    # check if the current vector is positive
    if (current_vector[assertion_atr_index] == assertion_aatr_flag)  
      {
              # print(c( "Vector",vector_counter,", is a positive one. So considering it"))
              # initialize the hypothesis vector with first specific example
              if( is_hyp_initialized == 0  ){
                  hypothesis = current_vector
                  is_hyp_initialized = 1
              }
              # check one by one attribute of current vector
              for(attr_counter in 1:number_of_attributes){
                
                    # if the hypothesis attribute value is not marked as ANY and
                    #                             and not same as current vlue 
                    #     also ignore the assertion attribute 
                    if(hypothesis[attr_counter]!= string_ANY && 
                       hypothesis[attr_counter]!= current_vector[attr_counter] && 
                       assertion_atr_index != attr_counter )  {
                      hypothesis[attr_counter] = string_ANY
                    }
                next
              }
      }
    else {
        # print(c( "Vector",vector_counter,", is a Negative one. So ignore this "))
      }
  next
}
#*
#*********************************************************
#*************** Find-S Algorithm processing complete
#*********************************************************
#*********************************************************
#*************** Find-S Algorithm result/output 
print(hypothesis)



# print(dim(data_mat))
# print(data_mat)
