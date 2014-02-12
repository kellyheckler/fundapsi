# Rotina em R desenvolvida para a análise quantitativa dos dados 
# dos questionários desenvolvidos para a disciplina de Fundamentos
# Psicológicos da Educação.
# A proposta básica desta rotina é:
# 
# 0) Organizar os dados em um 'data frame' (banco de dados)
# 1) Conduzir uma Análise de Variância (ANOVA) com as seguintes características:
#   1.1) Variável Independente (x): Curso de Graduação;
#   1.2) Variável Dependente (y): Nota Geral do Questionário;
# 2) Plotar os resultados em um conjunto de gráficos descritivos.
#   2.1) Gráficos de caixa dos dados brutos;
#   2.2) Gráfico indicando o quanto cada nível do fator implica num afastamento da média geral;
#
# A hipótese que orienta essa análise é simples (e óbvia, aparentemente):
#   "O curso de graduação afeta o conhecimento que um aluno da UFFS tem do conceito Física escolhido pelo grupo"
# A hipótese nula (H0) que iremos utilizar para modelar nossos dados, portanto, afirma o seguinte:
#   "O curso de gradução não tem um efeito significativo sobre o conhecimento que um aluno da UFFS tem do conceito de Física escolhido pelo grupo"
# A hipótese nula implica que a média de cada grupo (alunos respondentes de cada curso) é igual,
# ou melhor, que sua diferença não é estatisticamente significativa.
# Verificaremos qual é a probabilidade dos dados obtidos dentro do modelo da hipótese nula.
# Caso a probabilidade se mostre significativa (alpha=0.05), descreveremos como cada nível
# do nosso fator (cada curso de graduação) afeta a média de desempenho dos alunos no conceito escolhido.

# Passo 'zero': montar nosso data frame.
# Primeiro, definimos os 'níveis' de nosso 'fator';
# ou seja, definimos os valores possíveis de nossa variável independente (que é categorial):

 cursos <- c('Administração', 'Agronomia', 'Ciências Biológicas',
             'Engenharia Ambiental', 'Física', 'Letras', 'Química')

# Dados para teste.
# locais <- c('OregonT', 'OregonN', 'Alaska', 'Russia', 'Finland')

# Em seguida, definimos os valores para cada respondente do questionário.
# Usar o seguinte código:
# 1 - Administração
# 2 - Agronomia
# 3 - Ciências Biológicas
# 4 - Engenharia Ambiental
# 5 - Física
# 6 - Letras
# 7 - Química
# É possivel automatizar essa codificação. Quem quiser saber como, entra em contato!

xDummy <- c(1, 2, 3, 4, 5, 6, 7) # Preencher com os dados de todos os respondentes, em ordem!

# Dados para teste.
x <- xDummy <- c(3, 1, 2, 4, 5, 3, 1, 5, 5, 5, 7, 3, 4, 2)

#x <- factor(xDummy, labels=cursos)

# Dados para teste
#x <- factor(xDummy, labels=cursos)

# Depois, definimos os valores da variável dependente.
# A escala pode variar e deve ser definida pelo grupo.

#y <- c(0.0, 6.5, 10.0, 5.0, 3.3, 4.7) # Preencher com a nota calculada para cada respondente, em ordem.

# Dados para teste
y <- c(1, 7, 0, 1, 5, 7, 6, 5, 7, 5, 1, 7, 7, 7)

# Agora juntamos nossos dados num data frame para facilitar as análises.

dados  <- data.frame(x, y)

# Vamos começar fazendo uma análise exploratoria dos dados.
# Primeiro, gráficos de caixa para comprar, rapidamente, os diferentes grupos.
# Usando uma linha horizontal com a média geral da variável independente.
# Verifiquem se os gráficos em caixa estao longes ou distantes da linha da média;
# verifiquem também se a distribuição tem 'outliers' ou se apresenta uma cauda mais longa que a outra

mediaY <- mean(dados$y)
boxplot(y ~ x, main='Gráfico de caixa dividido por níveis do fator')
abline(h=mediaY)

# Função para efetuar a ANOVA da variável dependente y em função dos diferentes níveis do fator x.
anova <- aov(y ~ x, data=dados)

#Imprime informações sumárias da ANOVA
summary(anova)
#Imprime a média geral e a média de cada nível do fator.
model.tables(anova ,'means')
#Imprime os efeitos de cada nível do fator.
model.tables(anova, 'effects')
#Comentário qualquer