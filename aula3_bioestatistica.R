library(tidyverse)

# Revisao estrutura de dados ---------------------------------------------------

## Variáveis
quantidade <- 1
nome <- "Tarcisio"

## Vetores
idade <- c(20, 30, 40, 50)
sexo <- c("M", "F", "M", "F")

## Dataframes
df <- data.frame(idade, sexo)
head(df)



# Importando dados --------------------------------------------------------

dados <- read_tsv("PNS_2019.tsv")

head(dados)
head(dados, n = 10)

dados

str(dados)
glimpse(dados)
dim(dados)
names(dados)

summary(dados)

View(dados)

# Classes de dados -------------------------------------------------------------

# Checar classe dos dados
class(dados$uf)
class(dados$idade)
class(TRUE)
is.numeric(dados$peso)
is.character(dados$altura)

# Converter entre classes
palavra <- "1.5"
class(palavra)
numero <- as.numeric(palavra)
class(numero)

numero <- 1000
class(numero)
palavra <- as.character(numero)
class(palavra)

# Fatores
estado <- dados$estado_saude
niveis <- c("Muito ruim", "Ruim", "Regular","Bom", "Muito bom")
estado <- factor(estado, levels = niveis)
class(estado)
levels(estado)
estado
sort(estado)

# Operadores lógicos e relacionais ----------------------------------------

## Relacionais
x <- 1
y <- 1
z <- 2

x == y  # TRUE
x == z  # FALSE
x != z  # TRUE
z > x   # TRUE
x > z   # FALSE
x < z   # TRUE
x > y   # FALSE
x >= y  # TRUE
y <= x  # TRUE

vetor <- c(x, y, z)
2 %in% vetor     # TRUE
3 %in% vetor     # FALSE
(z-y) %in% vetor # TRUE 


# Lógicos

## AND (&)
#  TRUE  & TRUE  == TRUE
#  FALSE & FALSE == FALSE
#  TRUE  & FALSE == FALSE

## OR (|)
#  TRUE  | TRUE  == TRUE
#  TRUE  | FALSE == TRUE
#  FALSE | FALSE == FALSE

## NOT (!)
#  ! TRUE  == FALSE
#  ! FALSE == TRUE


(x == y) & (z>y)     # TRUE
(x == y) & (z == y)  # FALSE

(x == y) | (z == y)  # TRUE
(x != y) | (z == y)  # FALSE

! (x != y)           # TRUE
! (x == y)           # TRUE

# Filtragem ---------------------------------------------------------------

dados$idade > 50

dados |> filter(idade > 50)

dados |> filter(idade > 50 & sexo == "Mulher")
dados |> filter(idade > 50 & sexo == "Mulher" & cancer == "Sim")

dados |> filter(estado_saude == "Ruim" | estado_saude == "Muito ruim")

dados |> filter( (estado_saude == "Ruim" | estado_saude == "Muito ruim") & (sexo == "Mulher" & cancer == "Sim") )

dados |> filter(uf != "Rondônia") |> View()


# Valores ausentes (NA) ---------------------------------------------------

ausente <- NA
is.na(ausente) # TRUE
NA + 1         # NA

dados |> filter(is.na(colesterol_alto))

dados |> filter(! is.na(colesterol_alto))

dados |> 
  filter(! is.na(colesterol_alto) & is.na(altura))

colSums(is.na(dados))

dados |> glimpse()
dados |> drop_na() |> glimpse()

sub_dados <- dados |> drop_na()

# Selecionando colunas ----------------------------------------------------

dados |> select(uf)
dados |> select(uf, sexo, altura, cancer)

dados |> select(-colesterol_alto)
dados |> select(-situacao_domicilio, -estado_saude, -peso)

# Criando e transformando colunas -----------------------------------------

dados |>
  mutate(altura_metros = altura/100)

dados |>
  mutate(altura_metros = altura/100) |>
  mutate(imc = peso / altura_metros^2)

dados |>
  mutate(altura_metros = altura/100) |>
  mutate(imc = peso / altura_metros^2) |>
  mutate(imc_round = round(imc))


dados |>
  mutate(categoria_idade = ifelse(idade < 50, "jovem", "velho"))

dados |>
  mutate(altura_metros = altura/100) |>
  mutate(imc = peso / altura_metros^2) |>
  mutate(classe_imc = case_when(
    imc < 18.5 ~ "Baixo peso",
    imc < 25   ~ "Peso normal",
    imc < 30   ~ "Sobrepeso",
    imc >= 30  ~ "Obesidade"
  ))

# Ordenando os dados ------------------------------------------------------

dados |> arrange(peso)
dados |> arrange(desc(peso))
dados |> arrange(peso, altura)
dados |> arrange(peso, desc(altura))

dados |> arrange(sexo)
dados |> arrange(estado_saude)

niveis <- c("Muito ruim", "Ruim", "Regular","Bom", "Muito bom")

dados |> 
  mutate(estado_saude = factor(estado_saude, levels = niveis)) |>
  arrange(estado_saude)


# Resumindo conjuntos de dados --------------------------------------------

dados |>
  group_by(sexo) |>
  summarise(frequencia = n())

sub_dados |>
  group_by(sexo) |>
  summarise(media = mean(altura))

sub_dados |>
  group_by(sexo) |>
  summarise(media = mean(altura), desvio_padrao = sd(altura))


sub_dados |>
  group_by(sexo) |>
  summarise(media = mean(altura), 
            desvio_padrao = sd(altura),
            mediana = median(altura),
            minimo = min(altura),
            maximo = max(altura))

sub_dados |>
  group_by(sexo, uf) |>
  summarise(media = mean(peso)) |>
  arrange(uf)


# Extras ------------------------------------------------------------------

input_1 <- 4
input_2 <- 2
sum(input_1, input_2) 
# output: 6
output <- sum(input_1, input_2) 
sum(output, 20)
# output: 26


x <- dados |> filter(sexo == "Homem") |> head(n=5)
y <- dados |> filter(sexo == "Mulher") |> head(n=5)
bind_rows(x, y) |> group_by(sexo) |> summarise(frequencia = n())
