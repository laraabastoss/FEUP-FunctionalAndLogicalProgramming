# Programação Funcional e Lógica - Projeto 2

## Membros e contribuições
<b> Grupo T01_G01 </b>

```
Lara Santos Bastos up202108740 ( Contribuição : 50% )
Lia Margarida Mota Sobral up202108741  ( Contribuição : 50% )
```

## Introdução

O Projeto 2 de Programação Funcional e Lógica focou-se na implementação de um interpretador para uma linguagem de máquina específica, bem como na criação de um compilador para traduzir programas nessa linguagem. Este relatório aborda as estruturas de dados escolhidas, a implementação do interpretador, o processo de compilação e parse de programas, e conclui com reflexões sobre os desafios enfrentados.

## Representação dos Dados e Tipos

De forma a facilitar o restante desenvolvimento do projeto, o primeiro passo deste concentrou-se na consideração da estrutura mais adequada para os Dados e Tipos utilizados.

Definimos assim as seguintes Estruturas de Dados:

- Instruções - representa todas as instruções possíveis.

```hs
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
```
- Bool Elements - representa elementos boleanos. Decidimos usar TT e FF, correspondentes a True e False tal como referido no enunciado.

```hs
data BoolElement = TT | FF
    deriving (Eq,Show)
```

- StackElement - inteiros ou BoolElements. Representa os elementos que a stack pode ter.

```hs
data StackElement =  Integer Integer |  BoolElement BoolElement
  deriving (Show)
```

- Aexp - representa espressões aritméticas. Suporta as operações adição, multiplicação, subtração e ainda VarA, uma string a representar o nome de uma variável, e Num a representar inteiros.

```hs
data Aexp = Add' Aexp Aexp | Mult' Aexp Aexp | Sub' Aexp Aexp | VarA String | Num Integer
  deriving (Show)
```

- Bexp - representa espressões boleanas. Suporta as operações de igualdade de expressões bolenaos e aritméticas, menor our igual, negação e o and lógico. Pode ainda te um valor boleano ( True ou False) ou uma variável a representar um boleano.

```hs
data Bexp = EqA Aexp Aexp | EqB Bexp Bexp |Le' Aexp Aexp | Not' Bexp | And' Bexp Bexp | BoolValue Bool | VarB String
  deriving (Show)
```

- Stm - representa statements. Estas podem ser <i>assignment</i> de variávies, sequências de instruçoes, <i>if statements</i> e <i>while statements</i>.

```hs
data Stm = AssignA String Aexp
         | AssignB String Bexp
         | Seq Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Aexp Aexp
         | Bexp Bexp
  deriving (Show)
```

Definimos de seguida, os seguintes novos tipos:

- Code -  lista de instruções (Inst).

```hs
type Code = [Inst]
```

- Stack - lista de "StackElements".

```hs
type Stack = [StackElement]
```

- <i>State</i> -  lista de pares de <i>strings</i> e <i>StackElements</i>.

```hs
type State = [(String, StackElement)]
```

- Program - lista de statements (Stm).

```hs
type Program = [Stm]
```

Por fim, definimos funções para inicar a <i>Stack</i> e <i>State</i> vazios:

```hs
createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []
```

## Parte 1 

O propósito inicial da primeira fase do projeto consistia no desenvolvimento de um interpretador da linguagem da máquina descrita no enunciado. Para tal,era pedido que fosse implementado uma lista de instruções, uma pilha (do tipo Stack), e uma <i>storage</i> (do tipo <i>State</i>). O interpretador deveria percorrer a lista de instruções fornecidas resultando na devolução desta lista vazia e na preservação dos valores resultantes na <i>storage</i>.

Após definir os tipos e dados necessários, começamos por implementamos as funções "stack2Str" e "state2Str" que traduzem a <i>Stack</i> e o <i>State</i> em Strings respetivamentes. 

```hs
-- Converts a stack to its string representation
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (Integer x : xs) = show x ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ stack2Str xs
stack2Str (BoolElement x : xs) = showBoolElement x ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ stack2Str xs

-- Converts a state to its string representation
state2Str :: State -> String
state2Str [] = ""
state2Str ((var, Integer val):xs) = var ++ "=" ++ show val ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ state2Str xs
state2Str ((var, BoolElement val):xs) = var ++ "=" ++ showBoolElement val ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ state2Str xs
```


De seguida, desenvolvemos a função: 

```hs
-- Runs the stack machine with a given initial state
run :: (Code, Stack, State) -> (Code, Stack, State)
```

Esta processa cada instrução da lista de instruções de cada vez, e através de matching de padrões, reconhece que instrução realizar de forma a obter os valores esperados.
- Tru e Fals empilham os valores booleanos True ou False, respectivamente, na <i>Stack</i>.
- Add, Mult e Sub realizam a operação correspondente com os dois valores do topo da stack, empilhando o resultado.
- Equ e Le empilham o resultado da comparação dos últimos dois valores da <i>Stack</i> na forma de valores booleanos na <i>Stack</i>.
- And e Neg realizam as operações correspondentes com os valores booleanos do topo da <i>Stack</i>, empilhando o resultado.
- Push empilha o valor inteiro fornecido na <i>Stack</i>.
- Fetch vai buscar o valor associado à variável no topo de <i>State</i> e coloca-o no topo <i>Stack</i>, caso exista.
- Store atualiza o valor da variável ao valor do topo da <i>Stack</i> caso este já exista. Se a variável nao existir ainda, insere um novo túpulo (String, StackElement), mantendo a ordem alfabética das variáveis no <i>State</i>.
- Branch executa o código correspondente com base no valor booleano no topo da <i>Stack</i>.
- Noop avança para a próxima instrução.
- Loop cria um loop, executando repetidamente code1 enquanto a condição em code2 for verdadeira.
  
Em caso de falta de correspondência de padrões, a função retorna o erro ”Run-time error”, tal como pedido.

Para testar a primeira fase do projeto, utilizamos a função "testAssembler" fornecida no template:

```hs
-- Tests the stack machine with a given code
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
```

#### Exemplo de Teste

Recebendo o input [Fals,Store "var",Fetch "var"] :

1 - A primeira instrução da lista, Fals, é consumida, resultando na colocação do valor booleano False na pilha.

Pilha: [False]

Stack: [False]

State: Vazio

2 - A segunda instrução, Store "var", é então executada, armazenando o valor presente no topo da pilha (que é False) na variável denominada "var".

Pilha: Vazia

Stack: [False]

State: [("var", False)]

3 - A terceira e ultima instrução, Fetch "var", busca o valor associado à variável "var" no estado e o coloca na pilha. Neste caso, o valor False é novamente empilhado.

Pilha: [False]

Stack: [False, False]

State: [("var", False)]

## Parte 2

Por sua vez, a segunda parte do projeto destinava-se a receber uma string com código nesta linguagem, e passá-lo para uma lista de instruções, de forma a poder usar esta na função run, desenvolvida anteriormente.
Esta parte é subdividida em duas funções principais: <b>compile</b> e <b>parse</b>.

### Compile

A função <b>compile</b>, têm como objetivo receber uma lista de "Stm" e transformá-lo em instruções. Desta forma, vai processando cada elemento da lista, reconhece que statement está a processar ( Assignments , Sequências de Instruçoes , If Statements e While Statements). No caso da statement conter uma expressão aritmética ou boleana, estas vão ser compiladas através das funções <b>compA</b> e <b>compB</b> respetivamente.


- Um número é compilado como uma instrução Push para inserir esse número na pilha.
- Uma variável é compilada como uma instrução Fetch para obter o valor associado a essa variável na memória.
- As operações aritméticas são compiladas chamando recursivamente compA para as subexpressões e adicionando instruções Add, Mult e Sub correspondentes.
- Um valor booleano é compilado como Tru ou Fals, respetivamente, para representar True ou False na pilha.
- Uma variável booleana é compilada como Fetch para obter o valor associado a essa variável na memória.
- As operações de comparação são compiladas chamando recursivamente compA ou compB para as subexpressões e adicionando instruções Equ e Le.
- As operações lógicas são compiladas chamando recursivamente compB para as subexpressões e adicionando instruções And e Neg.
- Compila as expressões associadas às atribuições e adiciona instruções Store para armazenar os resultados na memória.
- Compila cada statement na sequência chamando recursivamente a função compile.
- Compila a expressão booleana, gera instruções Branch e compila os blocos "then" e "else".
- Compila a expressão booleana, gera instruções Loop e compila o bloco "do".

### Parse

A função <b>parse</b> recebe uma string e transforma-a num programa (lista de "Stm"), para ser utilizada pelo compilador. 
Esta String, quando recebida na função, passa pelos seguintes passos:

1 - A função <b>lexer</b> divide esta string em tokens, que podem ser nomes de variávies, "(" ,")",";" ou operadores. 
  - Exemplo: lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]

2 - A lista de token é passada de seguida para a função <b>parseProgram</b>. Esta identifica as "Stm" existentes no código:
  - Ao encontrar um "if" o código é passado para a função <b>parseIfStm</b>. Esta encontra o próximo "then" pertencente ao if encontrado, verificando que entre estes há o mesmo número de parênteses abertos e fechados, e processa a expressão entre estas na função <b>parseBexp</b>. De seguida faz o mesmo entre o "then" e o  "else" pertencente à if statement e processa a expressão na função <b>parseSeq</b>. Por fim faz o mesmo entre o "else" e o ";" correspondente.
  - Ao encontrar um "while" o código é passado para a função <b>parseWhileStm</b>. Esta encontra o próximo "do" pertencente ao while encontrado, verificando que entre estes há o mesmo número de parênteses abertos e fechados, e processa a expressão entre estas na função <b>parseBexp</b>. De seguida faz o mesmo entre o "do" e o  ";" correspondente à while statement e processa a sequência de instruções na função <b>parseSeq</b>.
  - Ao encontrar o nome variável o código é passado para a função <b>parseAssignStm</b>. Esta processa a expressão que segue o ":=" na função <b>parseAexp</b>.
  - Ao encontrar um "(" o código é passado para a função <b>parseSeq</b>. Esta começa por retirar os parênteses da sequência e processa cada statement existente na sequência (if, whiles, assignment ou outra sequência) chamando as função referidas previamente.

   As funções <b>parseAexp</b> e <b>parseBexp</b> processam expressões artiméticas e boleanas respetivamente. 
   De forma a ter em conta a precedência de operadores, começamos por processar as operações com menor prioridade. Caso tenham parênteses a delimitar a expressão, uma vez que ficam com prioridades, são processadas no fim, apendas quando todas as expressões sem parênteses tiverem sido processadas. 
   
#### Exemplo: 
  - (1+2)*3 -> (Mult 3 (1+2)) -> (Mult 3 (Add 1 2))
  - 1+2 * 3 -> (Add 1 (2 * 3)) -> (Add 1 (Mult 2 3))

Para auxiliar a implementar a lógica explicada anteriormente e lidar com parênteses criamos as funções:

- <b>parenthesesBalanced</b> - verifica se há o mesmo número de parêntese abertos e fechados numa lista de tokens.
- <b>removeOuterParentheses </b> - remove os parênteses a envolver um elemento
- <b>splitAtBalanced </b> - divide uma lista de tokens na primeira ocorrência de um target com parênteses equilibrados

Para testar a segunda fase do projeto, utilizamos a função "testParser" fornecida no template:

```hs
-- Tests the parser with a given program code
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)
```

#### Exemplo de Teste

- Recebe o input "x := 5; x := x - 1";
- Após ser passar pelo <b>parse</b>: [(AssignA x 5),(AssignA x (Sub x 1))]
- Após ser passar pelo <b>compiler</b>: [ Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"]
- Após passar pelo <b>run</b>: ("","x=4")

## Conclusão

Após a conclusão do projeto, acreditamos que o seu desenvolvimento foi concluido com sucesso. Implementamos todas a funções pedidas e verificamos todos os casos de teste fornecidos no template. Adicionalmente, realizamos testes extra de forma a cobrir todos os casos possíveis e garantir a robustez do nosso programa. A implementação do parse revelou-se particularmente desafiante, no entanto acreditamos que não só ultrapassamos as nossas dificuldades com sucesso como consolidamos todos os conhecimentos de Haskell lecionados na Unidade Curricular.







