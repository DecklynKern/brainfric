byte a
byte b
byte c

a <- 0
b <- 1

while ?b
  c <- a + b
  a <- b 
  b <- c
  write b
end