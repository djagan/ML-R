# Author    : Dr. D.Jagan
# Desc,     : CE - Candidate Elimination Algorithm
#
#*********************************************************
#*************** Data initialization start
string_ANY = "?"
#*********************************************************
#*************** DATA SET 1
#*******************DATA SET 1 start**********************

number_of_vectors = 4
number_of_attributes = 7
specific_hyp <- c('??','??','??','??','??','??','??' )
assertion_atr_flag = "YES"
w1 <- c('sunny','warm','normal','strong','warm ','same','YES')
w2 <- c('sunny','warm','high','strong','warm ','same' ,'YES')
w3 <- c('rainy','cold','high','strong','warm ','change','NO')
w4 <- c('sunny','warm','high','strong','cool','change','YES')
number_of_attributes = length(specific_hyp)
data_mat <- array(c(w1,w2,w3,w4),dim = c(number_of_attributes,number_of_vectors,1))
tmp_v <- c('?','?','?','?','?','?','?')
general_hyp <- array( c(tmp_v,tmp_v,tmp_v,tmp_v,tmp_v,tmp_v,tmp_v), dim = c(number_of_attributes,number_of_attributes,1))

#*******************DATA SET 1 complete*******************
#*********************************************************

#*********************************************************
#*************** DATA SET 2
#*******************DATA SET 2 start**********************
# 
# number_of_vectors = 5
# specific_hyp <- c('??','??','??','??','??','??' )
# number_of_attributes = length(specific_hyp)
# assertion_atr_flag = "Positive"
# w1 <- c('Japan','Honda','Blue','1980','Economy','Positive')
# w2 <- c('Japan','Toyota','Green','1970','Sports','Negative')
# w3 <- c('Japan','Toyota','Blue','1990','Economy','Positive')
# w4 <- c('USA','Chrysler','Red','1980','Economy','Negative')
# w5 <- c('Japan','Honda','White','1980','Economy','Positive')
# data_mat <- array(c(w1,w2,w3,w4,w5),dim = c(number_of_attributes,number_of_vectors,1))
# tmp_v <- c('?','?','?','?','?','?')
# general_hyp <- array( c(tmp_v,tmp_v,tmp_v,tmp_v,tmp_v,tmp_v), dim = c(number_of_attributes,number_of_attributes,1))

#*******************DATA SET 2 complete*******************
#*********************************************************
#*
#*********************************************************
#*************** DATA SET 3
#*********************************************************
#*******************DATA SET 3 start**********************
# # 
# number_of_vectors = 7
# specific_hyp <- c('??','??','??','??','??')
# number_of_attributes = length(specific_hyp)
# assertion_atr_flag = "YES"
# w1 <- c('GREEN','HARD','NO','WRINKLED','YES')
# w2 <- c('GREEN','HARD','YES','SMOOTH','NO')
# w3 <- c('BROWN','SOFT','NO','WRINKLED','NO')
# w4 <- c('ORANGE','HARD','NO','WRINKLED','YES')
# w5 <- c('GREEN','SOFT','YES','SMOOTH','YES')
# w6 <- c('GREEN','HARD','YES','WRINKLED','YES')
# w7 <- c('ORANGE','HARD','NO','WRINKLED','YES')
# number_of_attributes = length(specific_hyp)
# data_mat <- array(c(w1,w2,w3,w4,w5,w6,w7),dim = c(number_of_attributes,number_of_vectors,1))
# tmp_v <- c('?','?','?','?','?')
# general_hyp <- array( c(tmp_v,tmp_v,tmp_v,tmp_v,tmp_v), dim = c(number_of_attributes,number_of_attributes,1))
# 
#*******************DATA SET 3 complete*******************
#*********************************************************

#*********************************************************
#*************** DATA SET 4
#*********************************************************
#*******************DATA SET 4 start**********************
# 
# number_of_vectors = 14
# specific_hyp <- c('??','??','??','??')
# number_of_attributes = length(specific_hyp)
# assertion_atr_flag = "YES"
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
# number_of_attributes = length(specific_hyp)
# data_mat <- array(c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14),dim = c(number_of_attributes,number_of_vectors,1))
# tmp_v <- c('?','?','?','?')
# general_hyp <- array( c(tmp_v,tmp_v,tmp_v,tmp_v), dim = c(number_of_attributes,number_of_attributes,1))
# 
#*******************DATA SET 4 complete*******************
#********************************************************
#*********************************************************
assertion_atr_index = number_of_attributes #The last attribute in an input vector
#*********************************************************
#*************** Data initialization Completed
#*********************************************************

#*********************************************************
#***** Candidate Elimination Algorithm processing start***
#*********************************************************

is_hyp_initialized = 0 
# iterating through all the vectors
for (vector_counter in 1:number_of_vectors) {
  
    current_vector = data_mat[,vector_counter,1]
  
    # check if the current vector is positive
    if (current_vector[assertion_atr_index] == assertion_atr_flag)  
      {
              # print(c( "Vector",vector_counter,", is a positive one. So considering it for specific hypothesis"))
              # initialize the hypothesis vector if not initialized 
              if( is_hyp_initialized == 0  ){
                  specific_hyp = current_vector
                  is_hyp_initialized = 1
              }
              # check one by one attribute of current vector
              for(attr_counter in 1:number_of_attributes){
                
                    # if the hypothesis attribute value is not marked as ANY and
                    #                             and not same as current vlue 
                    #     also ignore the assertion attribute 
                    if(specific_hyp[attr_counter]!= string_ANY && 
                       specific_hyp[attr_counter]!= current_vector[attr_counter] && 
                       assertion_atr_index != attr_counter )  {
                       specific_hyp[attr_counter] = string_ANY
                    }
                next
              }
      }
    else 
      { 
        # print(c( "Vector",vector_counter,", is a nagative one. So considering it for generic hypothesis"))
        for(attr_counter in 1:number_of_attributes){
            if(specific_hyp[attr_counter]!= string_ANY && 
               specific_hyp[attr_counter]!= current_vector[attr_counter] && 
              assertion_atr_index != attr_counter ) 
            {
               tmp_value = specific_hyp[attr_counter]
               # print (tmp_value)
               # print (general_hyp)
               general_hyp[attr_counter,attr_counter,1] = specific_hyp[attr_counter]
            }
          next
        }
      }
  next
}

# now verify the general hypothesis and arrive final general hypothesis and specific hypothesis
final_general_hypo = NULL
for (i in 1: number_of_attributes)
  {
    current_v = general_hyp[,i,1]
    # print(current_v)
    if(current_v[i] != string_ANY && specific_hyp[i] != string_ANY)
      {
      rbind(final_general_hypo, current_v)
      final_general_hypo <- rbind(final_general_hypo,c(current_v))
    }
}

#*
#***********************************************************
#********** Candidate Elimination Algorithm Processing End**
#***********************************************************
#***********************************************************
#********** Candidate Elimination Algorithm Output**********
#*
#*
print ("***********************************************************")
print ("**********INPUT for Candidate Elimination Algorithm********")
print(data_mat)
print ("***********************************************************")
print ("**********SPECIFIC HYPOTHESIS IS***************************")
print ("***********************************************************")
print(specific_hyp)
print ("***********************************************************")
print ("**********GENERAL HYPOTHESIS IS****************************")
print ("***********************************************************")
print(final_general_hypo)
print ("***********************************************************")
print ("***********************************************************")

#********** Candidate Elimination Algorithm Completed**********