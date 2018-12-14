A <- c(200, -350, 450, 556, 442)
B <- c(220, 445, 221, 987, -745)
result_sum <- A + B # Additions of two vectors
print(result_sum)
result_sub <- A - B # Subtraction of two vectors
print(result_sub)
A == B              #comparing two vectors
result_sum[c(1,2)]  #first two variables from vector result_sum
num = length(result_sum)
for (i in 1:num)
  if(result_sum[i] > 0)
    print(result_sum[i])   #values in result_sum greater than 0
mean(result_sub) #mean values in result_sub