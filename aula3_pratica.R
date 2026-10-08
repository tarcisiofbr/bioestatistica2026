# Exercicios:
#  1) Em qual estado mora o paciente mais jovem e com maior peso da tabela?
#  2) Quantos anos tem a mulher com câncer mais velha da tabela ?
#  3) Qual o sexo do paciente mais velho que mora no sudeste ?
#  4) Crie uma tabela composta apenas por pessoas jovens (menos de 50 anos), com cancer e que não possua valores faltantes para peso e altura.
#  5) Dentre os pacientes jovens da questao 3, o cancer é mais presente em homens ou mulheres?
#  6) Qual o estado de saúde mais frequente nos homens da questao 3 ?
#  7) Qual é o imc (peso / altura²) médio e desvio padrão entre homens e mulheres com câncer ?
#  8) Crie uma pergunta sobre os dados e tente responder com o que você aprendeu.


# 1)
dados |> arrange(idade, desc(peso))

# 2)
dados |> filter(sexo == "Mulher" & cancer == "Sim") |> arrange(desc(idade))

# 3)
dados |>
  filter(uf == "Espírito Santo" | uf == "Rio de Janeiro" | uf == "Minas Gerais" | uf == "São Paulo") |>
  arrange(desc(idade))

# 4)
sub_dados <- dados |>
  filter(idade < 50 & cancer == "Sim" & ! is.na(peso) & ! is.na(altura))

# 5)
sub_dados |>
  group_by(sexo) |>
  summarise(n = n())

# 6)
sub_dados |>
  filter(sexo == "Homem") |>
  group_by(estado_saude) |>
  summarise(n = n()) |>
  arrange(desc(n))

# 7)
dados |>
  drop_na() |>
  filter(cancer == "Sim") |>
  mutate(altura_metros = altura / 100) |>
  mutate(imc = peso / altura_metros^2) |>
  group_by(sexo) |>
  summarise(media = mean(imc))
