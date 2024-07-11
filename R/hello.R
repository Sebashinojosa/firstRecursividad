
#El factorial de un numero con mulitiplicacion
f_recursiva <- function(N){
  if(N==0){
    return(1)
  }else{
    return(N*f_recursiva(N-1))
  }
}

cat("Ingrese un número: \n")
N <- readLines(n=1)
N <- as.numeric(N)

f2 <- f_recursiva(N)
cat("\nCon recursividad ", N, "es: ", f2)


#La multiplicacion de 2 numeros
recursi_suma <- function(a,b){
  if(a == 0){
    return(0)
  }else{
    return(b + recursi_suma(a-1,b))
  }
}
cat("Ingrese un numero: \n")
a <- readLines(n=1)
a <- as.numeric(a)

cat("Ingrese otro numero: \n")
b <- readLines(n=1)
b <- as.numeric(b)

cr <- recursi_suma(a,b)
cat("\nLa multiplicacion con recursividad es: ", cr)

#Potencia de un numero con sumas
sumar <- function(a,b){
  if(b==0){
    return(0)
  }else{
    return(a+sumar(a,b-1))
  }
}

potencia <- function(base,expo){
  if(expo == 0){
    return(1)
  }else if(expo == 1){
    return(base)
  }else{
    return(sumar(base,potencia(base,expo-1)))
  }
}

base <- as.numeric(readline((prompt= "Ingrese la base: ")))
expo <- as.numeric(readline((prompt= "Ingrese la exponente: ")))

result <- potencia(base, expo)
cat("El resultado de ", base, " elevado a la ", expo, "es: ", result)

#El facorial de un numero con sumas
sumar <- function(a, b) {
  if (b == 0) {
    return(0)
  } else {
    return(a + sumar(a, b - 1))
  }
}

factorial_sumas <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(sumar(factorial_sumas(n - 1), n))
  }
}

cat("Ingrese un numero: \n")
a <- readLines(n=1)
a <- as.numeric(a)

f <- factorial_sumas(a)
cat("El factorial de: ",a, "es: ",f)
