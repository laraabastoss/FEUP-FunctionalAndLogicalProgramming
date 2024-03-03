# SPLUT!

PFL_TP1_T01_Splut_4

### Membros e contribuições
```
Lara Santos Bastos up202108740 ( Contribuição : 50% )
Lia Margarida Mota Sobral up202108741  ( Contribuição : 50% )
```

## Instalação e Execução
De forma a executar o jogo, é necessário fazer download dos ficheiros, consultar o ficheiro *main.pl* através do terminal e por fim chamar o predicado **play/0**.

```pl
?- consult('path-to-file').
?- play.
```

## Descrição do Jogo


Splut! é um jogo de tabuleiro para 2 a 4 jogadores. Cada jogador possui e controla três peças: um feiticeiro, um anão e um troll. O objetivo do jogo é eliminar o feiticeiro adversário. Existem ainda 4 rochas para cada jogador, inicialmente posicionadas nas extremidades do tabuleiro.
  
As peças de cada jogador são colocadas nas três células imediatamente após cada extremidade do tabuleiro.

Na primeira ronda, o primeiro jogador pode realizar uma jogada, na segunda ronda o seguinte pode realizar duas e, daí em diante, são feitas três jogadas por ronda. 

Numa jogada, um jogador pode mover uma das suas três peças uma célula para cima, baixo, esquerda ou direita. Cada peça, pode movimentar as pedras de maneira diferente:
* Um feiticeiro pode escolher levitar uma pedra, ou seja, esta vai mimicar o movimento que ele faz.
* Um anão pode empurrar uma pedra uma casa para a frente.
* Um troll pode puxar uma pedra de forma a que esta fique na sua posição anterior.
* Um troll pode ir até à posição de uma pedra e empurrá-la numa direção à escolha. Esta vai mudando de lugar nessa direção continuamente até atingir uma borda do tabuleiro, um troll ou outra pedra. As pedras podem passar por cima de um anão mas ao atingirem um feiticeiro este é morto.
  
Um jogador ganha o jogo quando o feiticeiro da equipa oposta for morto.

Consultamos as regras do jogo nas seguintes fontes:

https://bodogemu.com/pt/games/splut 

https://www.iggamecenter.com/en/rules/splut

## Lógica do Jogo

### Representação Interna do Jogo

A lista <i>GameState</i> representa o estado atual do jogo , e é composta pelos seguintes elementos:
* <b> Board - </b> Uma lista de listas que representa o estado atual do tabuleiro.
* <b> BoardSize - </b> Tamanho do tabuleiro escolhido pelo jogador.
* <b> Player - </b> O jogador que etá a jogar na ronda atual.
* <b> RoundNumber - </b> Ronda atual
* <b> State - </b> Representa se o jogador 1 ganhou, o jogador 2 ganhou ou nenhum ganhou ainda.

Esta lista vai ser passada como argumento de grande parte dos predicados, uma vez que representa o tabuleiro no momento atual. O tamanho do tabuleiro apesar de não ser alterado durante o jogo, vai ser essencial para inúmeras validações, tal como garantir que apenas jogadas dentro deste são permitidas. A RoundNumber vai determinar quantas jogadas deve a ronda atual ter e o Player indica qual o jogador a jogar. O Estado, por fim, vai ser importante para verificar o término do jogo.

Cada elemento do tabuleiro representa uma peça nas coordenadas (Row, Column) e cada peças é representada por um átomo. As peças pertencentes a um jogador têm o número deste no seu nome (peçaX) e as rochas estão numeradas de 1 a 4.

Os átomos criados foram estes:
* **feiticeiro1**, **feiticeiro2** - feiticeiro do jogador 1 e 2 respetivamente
* **dwarf1**, **dwarf2** - anão do jogador 1 e 2 respetivamente
* **stonetroll1**, **stonetroll2** - troll do jogador 1 e 2 respetivamente
* **rock1**, **rock2**, **rock3**, **rock4** - rochas
* **empty** - células vazias
* **null** - células não existentes

Cada um átomos pode ser traduzido numa string, que vai ser disposta no terminal aquando da impressão do tabuleiro.


#### Estado inicial do jogo:

![Screenshot 2023-11-06 at 03 41 54](https://github.com/laraabastoss/feup-pfl/assets/92671491/04aacee6-8714-4159-8cfb-ef52220c641f)

#### Estado intermédio do jogo:

![Image 06-11-2023 at 03 46](https://github.com/laraabastoss/feup-pfl/assets/92671491/b99c7a94-5cdb-4610-8567-6b39f4be3b68)


#### Estado final do jogo:

![Image 06-11-2023 at 03 42](https://github.com/laraabastoss/feup-pfl/assets/92671491/3b0f638f-24bf-4127-90a0-de96e0dca25f)



### Visualização do estado do jogo

Antes de iniciar o jogo, os jogadores têm a oportunidade de escolher as configurações que preferem para a partida, podendo escolher :
* **Modo de jogo**: - Humano/Humano, Humano/Computador, Computador/Computador.
* **Nível de jogo** -Díficil, Fácil (para cada um dos computadores a jogar).
* **Jogador a iniciar o jogo** - Computador 1, Computador 2, Humano (caso seja escolhido o modo de jogo Humano/Computador ou Computador/Computador).
* **Tamanho do tabuleiro** - qualquer número ímpar entre 7 e 21.


![Screenshot 2023-11-06 at 03 33 58](https://github.com/laraabastoss/feup-pfl/assets/92671491/0b367072-e8e3-4710-82c8-7c85aa0f9523)



A validação dos inputs é feita através do predicado *validate_input/2* que verifica se o input dado pelo utilizador pertence a uma lista e, em caso negativo, volta a pedir um input válido. Ao longo do jogo, este predicado é chamado sempre que é pedido input ao utilizador

```pl
validate_input(List,Value):-
    read_number(Value),
    member(Value,List).
validate_input(List,Value):-
    repeat,
    write('Please inser a valid number:\n'),
    read_number(Value),
    member(Value,List),!.
```

O predicado **initial_state/4** cria assim o tabuleiro inicial, tendo em conta o tamanho escolhido pelo utilizador. Este é gerado dinamicamente, ou seja, a lista é criada após ser recebido o input do utilizador. Optamos por permitir apenas 2 jogadores na nossa implementação do jogo.

```pl
initial_state(0, _, _, []).
initial_state(Height, Width, RowNumber, [Row | Rest]):-
    Height > 0,
    create_row(RowNumber, Width, Row),
    NewHeight is Height - 1,
    NewRowNumber is RowNumber + 1,
    initial_state(NewHeight, Width, NewRowNumber, Rest).


create_row(RowNumber, Width, Row) :-
    length(Row, Width),
    fill_row(RowNumber, Row, Width).
```


Após todas as configurações serem recebidas, já pode ser criado o *GameState* inicial. De seguida, o tabuleiro é imprimido no terminal  através do predicado **display_game/4**.

```pl
display_game(_,Line,Size,0):-
    Line >= Size.
display_game(Board, Line, Size,State):-
    display_line(Board,Line,0,Size),
    Newline is Line + 1 ,
    display_game(Board, Newline, Size,State).`
```

Este chama recursivamente o predicado **display_line/4** que imprime todos os átomos da respetiva linha.

```pl
display_line(_,_,Col,Size):-
    Col >= Size,
    nl, nl.
display_line(Board,Line,Col,Size):-
    get_symbol(Board,Line,Col,Symbol),
    display(Symbol,Display),
    write(Display),
    Newcol is Col + 1,
    display_line(Board, Line, Newcol, Size).
```

Após cada jogada feita, o estado do jogo é alterada e é chamado de novo o predicado que imprime o tabuleiro.
O predicado **get_symbol/4** vai buscar o átomo presente na lista Board na posição (Row,Col) e imprime no tabuleiro a string associada ao respetivo átomo. Cada átomo é traduzido nesta string através do predicado display/2.

```pl
display(null, '    ').
display(empty,' ## ').
display(rock1,' R1 ').
display(rock2,' R2 ').
display(rock3,' R3 ').
display(rock4,' R4 ').
display(stonetroll1,' T1 ').
display(stonetroll2,' T2 ').
display(dwarf1,' D1 ').
display(dwarf2,' D2 ').
display(feiticeiro1,' S1 ').
display(feiticeiro2,' S2 ').

```

### Validação e execução das jogadas

O jogo funciona através do predicado **game/5**. Este começa por verificar a ronda atual, uma vez que o número de jogadas por jogador varia consoante esta. De seguida verifica que tipo de jogador está a jogar neste momento (humano, Bot random, Bot greedy). Este ciclo termina quando for detetado o final do jogo, ou seja, quando um dos feiticeiros morrer.
De forma a escolher que jogada executar, o jogador pode escolher os seguintes parâmetros:
* <b>Peça a mover</b> - sorcerer, dwarf, stonetroll.
* <b>Direção da peça</b> - cima, baixo, esquerda e direita, atirar uma pedra.
* <b>Rocha a mover</b> - rock1, rock2, rock3, rock4.
* <b>Direção na qual vai ser lançada a pedra</b>

 Qualquer jogador pode escolher mover uma das suas três peças em qualquer direção na qual esteja uma célula vazia. O troll vai poder também escolher atirar uma pedra se se encontrar adjacente a esta.
 Relativamente à escolha da rocha a mover,o feiticeiro pode escolher levitar qualquer rocha, enquanto o troll apenas vai poder escolher arrastar uma pedra caso esteja adjacente a esta e a ir na direção oposta. O dwarf vai conseguir empurrar uma pedra andando na direção desta, deste que haja uma casa vazia à frente, pelo que não é necessário escolher que pedra mover.
Qualquer jogada na qual se tenha escolhido atirar um pedra, é ainda possivel escolher em que direção atirar.

Todas estas jogadas são validadas através do predicado **possible_moves/4**,**possible_rocks/6** e **possible_directions/5** que verifica que jogadas a peça escolhida pode relizar. De seguida, vai sendo pedido ao jogador para inserir que ações pretende realizar. Mais uma vez, este input é validado através de **validate_input/2** Assim, basta verificar se o input inserido pelo utilizador pertence à lista das opções possíveis e, em caso negativo, incitá-lo a inserir um novo input.


![Image 06-11-2023 at 03 36](https://github.com/laraabastoss/feup-pfl/assets/92671491/b362da6b-5a4e-496f-b659-bb7fdf1f270a)


Uma jogada assume assim o formato Piece-Move-Rock-Direction.
Após estarem selecionados estes 4 parâmetros, basta chamar o predicado **move/3** de forma a alterar o tabuleiro de acordo com a jogada feita. Este predicado vai começar por alterar a posição da peça e, de seguida, a posição da rocha. É assim criando o novo estado do jogo: *NewGameState*. 


### Lista de Jogada Válidas

A lista de jogadas válidas para escolher a movimentação dos Bot, é obtida através do predicado **valid_moves/5**. Este utiliza **findall/3**, que por sua vez permuta por todas as jogadas possíveis e coloca todas as válidas numa lista.

```pl
valid_moves(Board, BoardSize, Player, ListofFinalMoves, 2):-
    findall(Piece-Move-Rock, validate_moves(Piece, Move, Rock, Player, Board, BoardSize), ListofMoves),
    findall(Piece-Move-Rock-Direction,(
                                        member(Piece-Move-Rock,ListofMoves),
                                       get_piece(Player, Piece, BoardPiece),
                                       find_piece_board(BoardPiece, Board, 0, PositionPiece, BoardSize),
                                       get_piece(_, Rock, BoardRock),
                                       find_piece_board(BoardRock, Board, 0, PositionRock, BoardSize),
                                       move_rock(Board, Piece, PositionPiece, BoardRock, BoardSize, Move, PositionRock, _),
                                       change_board(Board, PositionPiece, BoardSize, Move, BoardPiece, NewBoard, Direction,(NewRow,NewCol)),
                                       possible_directions(NewBoard, (NewRow, NewCol), BoardSize, ListofDirections,Move),
                                       member(Direction,ListofDirections)),
                                       ListofFinalMoves).
```


### Final do jogo

Uma vez que apenas é possível ganhar o jogo se um troll atirar um pedra ao feiticeiro da equipa oposta, só avaliamos o estado do jogo após haver o lançamento de uma pedra. Se a pedra conseguir voar até atingir um feiticeiro, esta vai substituir o seu lugar e sabemos assim que o jogo terminou. O predicado **game_over/8** vai ser responsável por esta verificação.

### Avaliação do tabuleiro

Para implementarmos um Bot que avalie o estado do jogo de forma a escolher a melhor jogada, criamos o predicado **value/2**. Assim o Bot greedy calcula todas as jogadas possíveis e avalia o tabuleiro que estas vão gerar. A avaliação do tabuleiro é feita para três casos:

* **Caso 1**: O feiticeiro do jogador oposto foi morto

Value: 100000 

* **Caso 2**: O feiticeiro do jogador atual foi morto

Value: -100000 

* **Caso 3**: Nenhum feiticeiro foi morto
    * **A** - Número de pedras não alinhadas com o feiticeiro do jogador atual.
    * **B** - Número de pedras alinhadas com o feiticeiro do jogador oposto.
    * **C** - Distancia do troll do jogador atual relativamente a uma pedra alinhada com o feiticeiro do jogador oposto.

Value = 10 * A + B * ( 50 +  1/C * 100 )

No primeiro caso, garantimos que caso uma jogada provoque a vitória do jogador atual, deve sempre ser escolhida em detrimento das outras. No segundo caso, por sua vez, é verificado o oposto, ou seja, se houver uma jogada que provoque a vitória do jogador oposto, nunca deve ser escolhida

No Caso 3, o caso mais geral, são tidos em conta vários fatores. Em primeiro lugar, consideramos essencial valorizar jogadas nas quais houver pedras alinhadas (mesma linha ou coluna) com o feiticeiro da equipa oposta. Atribuímos assim o valor de 50 a cada uma destas pedras. Para cada uma, é  calculada a distância até ao feiticeiro do jogador atual. Adicionamos ao valor final o inverso desta distância, por cada pedra alinhada (B), de forma a incitar o troll a aproximar-se destas pedras e multiplicamos por 100 por questões de proporção.
Por fim, para prevenir ser morto pelo adversário, adicionamos 10 por cada pedra não alinhada com o feiticeiro do jogador atual.


### Jogada do computador

Foram implementados dois Bots, um random e um greedy, que correspondem respetivamente ao nível fácil e difícil do jogo. Ambos utilizam o predicado **choose_move/5** para escolher a jogada a realizar.

O Bot random, calcula todas as jogadas possíveis e escolhe uma aleatóriamente com o auxílio do predicado **random_member/2**:

```pl
choose_move(Board, BoardSize, Player, 1, Piece-Move-Rock):-
    valid_moves(Board, BoardSize, Player, ListofMoves, 1),
    random_member(Piece-Move-Rock, ListofMoves).
```

O Bot greedy, por sua vez, adota uma estratégia mais complexa. Começa por calcular todas as jogadas possíveis, no entanto utiliza o predicado referido anteriormente (**value/2**) para atribuir um valor ao tabuleiro resultante de cada jogada. De forma a maximizar as suas chances de ganhar, opta pela jogada que gerou um maior valor.

```pl
choose_move(Board, BoardSize, Player, 2, Piece-Move-Rock-Direction):-
    valid_moves(Board, BoardSize, Player, ListofMoves, 2),
    findall(Value-Piece-Move-Rock-Direction, ( member(Piece-Move-Rock-Direction, ListofMoves), 
                            move([Board, BoardSize, Player, _, 0],[NewBoard, _, _, _, _],Piece-Move-Rock-Direction), 
                            value([NewBoard, BoardSize, Player, _, _],Value)),
                             Pairs),
    sort(Pairs, SortedPairs),
    write(SortedPairs),
    last(SortedPairs, Max-_-_-_-_),
    findall(Piece-Move-Rock-Direction, member(Max-Piece-Move-Rock-Direction, SortedPairs), MaxMoves),
    random_member(Piece-Move-Rock-Direction, MaxMoves).
    
```

De forma a lidar com empates de pontos, caso exista mais de uma jogada com o mesmo valor, é escolhida aleatoriamente a jogada a realizar dentro das jogadas com valor máximo.

Uma vez que o  jogo exige 3 jogadas por ronda, cada jogada é constítuida por 4 valores e são precisas verificar inúmeras restrições, concluímos que implementar o algoritmo *MinMax* ir-se-ia tornar bastante ineficiente, pelo que optamos tomar uma decisão apenas tendo em conta a jogada atual.
  
## Conclusão
Após o desenvolvimento do projeto, acreditamos termos desenvolvido o jogo *Splut!* com sucesso. Implementamos tudo o referido nos requisitos do projeto, nomeadamente 3 modos de jogo, um tabueleiro de tamanho variável e a existência de dois níveis de dificuldade para os Bots. Inputs e jogadas incorretas são sempre validadas de forma a não impossibilitarem a continuação do jogo.
O projeto revelou-se desafiante por vários fatores, nomedamente a adaptação a uma linguagem e paradigma de programação novos,a complexidade das regas do jogo e, por fim, o desenvolvimento de um algoritmo capaz de avalia qual a melhor jogada para o Bot escolher. No entanto, aceditamos que todo o nosso esforço foi recompensado, fizemos um bom trabalho e conseguimos consolidar todos os conhecimentos sobre Prolog lecionados na UC. 
Consideramos, no entanto, que se tivessemos mais tempo, gostaríamos de melhorar a organização e qualidade do código.


### Bibliografia
https://bodogemu.com/pt/games/splut 
https://www.iggamecenter.com/en/rules/splut

