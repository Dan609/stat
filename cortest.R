df  <- mtcars

cor.test(x = df$mpg, y = df$hp)
fit  <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)+
  facet_grid(. ~ am)


###########################################

df  <- mtcars
df_numeric  <- df[, c(1,3:7)]

pairs(df_numeric)

cor(df_numeric)

fit  <- corr.test(df_numeric)
fit$r
fit$p
fit$adjust

df[sapply(df, is.numeric)]

####


corr.calc <- function(x){
  x <- cor.test(~ x[,1] + x[,2], x)
  return(c(x$estimate, x$p.value))
}

corr.calc( mtcars[, c(1,5)] )
corr.calc( iris[,1:2] )


### step6
#рассчитывает коэффициенты коррел€ции ѕирсона 
#между всеми парами количественных переменных и 
#возвращает наибольшее по модулю значение коэффициента 
#коррел€ции.
step6 <-  read.table("step6.csv", 
                     header=TRUE, sep=',' )

# P.S. sorry for writing code
library(psych)

filtered.cor <- function(x){
  y <- cor(x[sapply(x, is.numeric)])
  diag(y) <- 0
  return(y[which.max(abs(y))]) 
}

filtered.cor(iris)
#[1] 0.9628654  

iris$Petal.Length <- -iris$Petal.Length 
# сделаем отрицательной максимальную по модулю коррел€цию
filtered.cor(iris)
# [1] - 0.9628654

filtered.cor(step6)
# [1] 0.235997

#######################
############data
vec1 <- c(24.985, 8.425, 14.992, 18.096, 16.664, 2.695, 10.919, 12.912, 0.926, -2.941, 27.019, 31.122, 10.999, 37.391, 0.069, 32.565, 18.737, 12.03, 15.988, 29.278, 20.641, 17.138, 27.051, 36.6, 23.38, 12.726, 28.429, 31.066, 41.038, 25.11, -4.407, 20.313, 16.531, 25.782, 24.68, 18.422, 34.917, 22.477, 16.982, 18.531, 20.138, 30.896, 32.664, 34.821, 11.421, 6.543, 39.009, 24.499, 13.345, 5.28)

vec2 <- c(4.247, 3.272, 7.384, -3.743, 10.315, 19.066, -9.901, 6.418, 7.287, 2.714, 5.895, 23.421, 12.151, 15.379, 13.808, 4.635, 11.795, 9.409, -0.799, 22.509, 16.575, 6.88, 24.828, 21.983, 13.111, 0.928, 12.409, 4.864, 6.04, 24.878, -5.797, -1.974, 4.576, 8.737, 2.773, 18.012, 16.747, 6.928, 4.748, 18.557, 8.633, 22.755, 5.759, 26.877, 13.31, 5.642, 14.142, 10.015, 15.29, 19.842)

df <- c(vec1, vec2)
#################
test_data <- as.data.frame(list(col1 = c(0.4, -0.35, 0.82, -0.23, -1.04, 1.21, 0.38, -0.72, 0.26, 1.95, -0.21, 1.96, 1.54, -0.36, -0.78, 0.04, 0.58, -0.49, 0.53, -1.3, -1.06, -0.47, 0.13, 0.95, -1.24, -1.27, 1.55, -0.99, 1.19, -1.49), col2 = c(0.58, -0.28, -0.05, -1.58, -2.13, -0.79, 0.05, 0.82, -0.55, -0.64, 0.35, -2.47, 1.31, 0.97, 1.37, 0.01, -0.34, 1.39, 1.53, -0.51, -1.85, 0.58, 0.21, -0.64, -0.52, 0.16, -0.56, 0.81, -1.14, 1.58)))
#################

smart_cor <- function(x) {
  vv1 <<- shapiro.test(xV1)vv2 <- shapiro.test(xV2)
  if ((vv1p.value<0.05)|(vv1p.value<0.05)) 
  {
     test <- cor.test(~V1+V2,x,method=c("spearman"))
     }
  else {
    test <- cor.test(x=xV1,y=xV2,method=c("pearson"))
  }
  return(test$p.value)
 }

# в if нужно отдельно дл€ каждого a b 
# прописать условие < 0.05

smart_cor <- function(x){
  a <- shapiro.test(x[,1])$p.value 
  b <- shapiro.test(x[,2])$p.value
  if(a|b < 0.05 ){
    no <- cor.test(~ x[ ,1] + x[ ,2], x, 
                   method = "spearman") 
  }
  else{
    no <- cor.test(~ x[ ,1] + x[ ,2], x, 
                   method = "pearson") 
  }
  return(no$estimate)
}


####


smart_cor <- function(x){
  a <- shapiro.test(x[,1])$p.value 
  b <- shapiro.test(x[,2])$p.value
  if (a < 0.05 | b < 0.05) {
    return(c(cor.test(x[,1], x[,2],
                      method="spearman" ))$estimate)
  } else {
    return(c(cor.test(x[,1], x[,2],
                      method="pearson" ))$estimate)
  }
}


###

smart_cor <- function(x){
  if (shapiro.test(x[,1])$p.value < 0.05 | 
      shapiro.test(x[,2])$p.value < 0.05) {
    return(c(cor.test(x[,1], x[,2],
                      method="spearman" ))$estimate)
  } else {
    return(c(cor.test(x[,1], x[,2],
                      method="pearson" ))$estimate)
  }
}

