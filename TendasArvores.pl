:- use_module(library(clpfd)).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- ["puzzlesAcampar.pl"].

% Primeiro Predicado
vizinhanca((L, C), Vizinhanca):-
    /* 
        Dada uma coordenada (L, C), o predicado retorna a vizinhanca,
        que consiste nas coordenadas dos vizinhos imediatos em cima,
        no lado esquerdo, em baixo e no lado direito. (Notar que nao
        verifica se as coordenadas estao fora do tabuleiro)
    */

    LAnterior is L-1,
    LPosterior is L+1,
    CAnterior is C-1,
    CDepois is C+1,
    Vizinhanca = [(LAnterior, C), (L, CAnterior), (L, CDepois), (LPosterior, C)].


% Segundo Predicado
vizinhancaAlargada((L, C), VizinhancaAlargada):-
    /*
        Dada uma coordenada (L, C), o predicado retorna a vizinhanca
        alargada, ou seja, a vizinhanca (ja definida anteriormente) com
        a adicao das diagonais a (L,C).
    */

    % Obtem a vizinhanca imediata
    vizinhanca((L, C), Vizinhanca),
    
    % Obtem as coordenadas dos vizinhos diagonais inferiores
    nth1(2,Vizinhanca, CoordBaixo),
    vizinhanca(CoordBaixo, VizinhancaBaixo),
    nth1(1,VizinhancaBaixo, DiagonalBaixoEsquerdo),
    nth1(4,VizinhancaBaixo, DiagonalBaixoDireito),

    % Obtem as coordenadas dos vizinhos diagonais superiores
    nth1(3,Vizinhanca,CoordCima),
    vizinhanca(CoordCima,VizinhancaCima),
    nth1(1,VizinhancaCima, DiagonalCimaEsquerdo),
    nth1(4,VizinhancaCima, DiagonalCimaDireito),

    % Unifica todas as coordenadas numa lista
    Diagonais = [DiagonalBaixoEsquerdo, DiagonalBaixoDireito, DiagonalCimaEsquerdo, DiagonalCimaDireito],
    append(Vizinhanca, Diagonais, VizinhancaUnificada),
    sort(VizinhancaUnificada, VizinhancaAlargada).


% Terceiro Predicado
todasCelulas(Tabuleiro, TodasCelulas):-
    /*
        Dado um tabuleiro, este predicado retorna uma lista, TodasCelulas, contendo
        todas as coordenadas possiveis do tabuleiro no formato (L, C).
    */

    tamanho_tabuleiro(Tabuleiro, N),
    % Encontra todas as coordenadas possiveis do tabuleiro
    findall((L, C), (between(1, N, L), between(1, N, C)), TodasCelulas).

% Predicado Complementar utilizado com frequencia ao longo do programa.
tamanho_tabuleiro(Tabuleiro, N):-
    /* 
        Dado um tabuleiro, este predicado retorna o seu tamanho, N.
    */
    
    nth1(1, Tabuleiro, Linha),
    length(Linha, N).


% Quarto Predicado
todasCelulas(Tabuleiro, TodasCelulas, Objecto):-
    /*
        Dado um tabuleiro, este predicado retorna uma lista, TodasCelulas, contendo
        as coordenadas de celulas que sao variaveis (nao instanciadas) se Objecto for uma variavel,
        ou as coordenadas de celulas que contem o valor Objecto se Objecto for instanciado.
        O tabuleiro eh representado por uma matriz e as coordenadas sao no formato (L, C).
    */

    tamanho_tabuleiro(Tabuleiro, N),
    (
        var(Objecto), !,
        findall((L, C), (
            between(1, N, L),
            between(1, N, C),
            nth1(L, Tabuleiro, LinhaElem),
            nth1(C, LinhaElem, Elemento),
            var(Elemento)), TodasCelulas)
        % Encontra coordenadas de celulas que contem variaveis
    ;
        findall((L, C), (
            between(1, N, L),
            between(1, N, C),
            nth1(L, Tabuleiro, LinhaElem),
            nth1(C, LinhaElem, Elemento),
            Elemento == Objecto), TodasCelulas)
        % Encontra coordenadas de celulas que contem o valor Objecto
    ).


% Quinto predicado
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) :-
    /*
        Dado um tabuleiro, este predicado calcula a contagem de ocorrencias do Objecto
        nas linhas e colunas do tabuleiro e retorna as contagens em ContagemLinhas e
        ContagemColunas, respetivamente. A contagem eh feita por meio do predicado auxiliar contagem.
    */
    
    % Calcula a contagem nas linhas.
    contagem(Tabuleiro, Objecto, ContagemLinhas),

    % Transpoe o tabuleiro para calcular a contagem nas colunas.
    transpose(Tabuleiro, TabuleiroTrans),
    contagem(TabuleiroTrans, Objecto, ContagemColunas).

% Predicado Complementar a calculaObjectosTabuleiro.
contagem([], _, []).
contagem([P|R], Objecto, [Ocorrencias | RestoContagem]) :-
    (
        (var(Objecto),
        exclude(atom, P, NovoP),
        length(NovoP, Ocorrencias))
        % Conta o numero de variaveis na lista
    ;
        (exclude(var, P, NovoP),
        include(==(Objecto), NovoP, PModificado),
        length(PModificado, Ocorrencias))
        % Conta o numero de ocorrencias de Objecto na lista
    ), !,
    contagem(R, Objecto, RestoContagem).


% Sexto predicado
celulaVazia(Tabuleiro, (L, C)):-
    /*
        Dado um tabuleiro, este predicado verifica se a celula na posicao (L, C) esta vazia,
        ou seja, contem r ou uma variavel.
    */

    nth1(L, Tabuleiro, LinhaElem),
    nth1(C, LinhaElem, Elemento),
    % A celula esta vazia se contiver r ou for uma variavel
    (Elemento == r; var(Elemento)),!.

celulaVazia(Tabuleiro, (L,C)):-
    tamanho_tabuleiro(Tabuleiro, N),
    % Verifica se as coordenadas estao fora dos limites do tabuleiro
    (
        L=<0;
        L>N;
        C=<0;
        C>N
    ), !.


% Setimo predicado
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)):-
    /*
        Dado um tabuleiro, um objeto (TendaOuRelva) e as coordenadas (L, C), este predicado
        insere o objeto na celula especificada se ela estiver vazia (variavel).
    */

    nth1(L, Tabuleiro, LinhaElem),
    nth1(C, LinhaElem, Elemento),
    var(Elemento),!,
    % Insere o objeto na celula
    Elemento = TendaOuRelva.

% Regra para caso a celula nao esteja vazia (nao seja uma variavel)
insereObjectoCelula(_, _, _):-!.


% Oitavo predicado
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    /*
        Dado um tabuleiro, um objeto (TendaOuRelva) e as coordenadas (L, C1) e (L, C2),
        este predicado insere o objeto nas celulas entre as posicoes C1 e C2 (inclusive)
        na linha especificada.
    */

    % Inicia o processo de insercao chamando a regra com um acumulador inicial C1
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), C1).

% Regra para encerrar o processo quando o acumulador ultrapassar C2
insereObjectoEntrePosicoes(_, _, _, (_, C2), Acumulador):-
    Acumulador>C2,!.

% Regra principal para inserir o objeto entre as posicoes C1 e C2 na linha L do tabuleiro
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), Acumulador):-
    Acumulador=<C2,!,
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, Acumulador)),
    Acumulador1 is Acumulador+1,
    insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2), Acumulador1).


% Nono predicado
relva(Puzzle):-
    /*
        Dado um Puzzle na forma (Tabuleiro, Linhas, Colunas), este predicado
        preenche todas as linhas/colunas cujo numero de tendas ja atingiu o
        maximo de tendas possivel.
    */

    Puzzle = (Tabuleiro, Linhas, Colunas),
    tamanho_tabuleiro(Tabuleiro, N),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    verificaLinha(Tabuleiro, Linhas, ContagemLinhas, N, 1),
    transpose(Tabuleiro, TabuleiroTrans),
    verificaLinha(TabuleiroTrans, Colunas, ContagemColunas, N, 1).

verificaLinha(_, [], [], _, _):-!.
% Regra para verificar restricoes nas linhas
verificaLinha(Tabuleiro, [P|R], [H|T], N, AcumuladorLinha):-
    % Verifica se a contagem de objetos t na linha eh a mesma que a especificada.
    P =:= H,!,
    insereObjectoEntrePosicoes(Tabuleiro, r, (AcumuladorLinha, 1), (AcumuladorLinha, N)),
    AcumuladorLinha1 is AcumuladorLinha+1,
    verificaLinha(Tabuleiro, R, T, N, AcumuladorLinha1).

% Regra para lidar com o caso em que a contagem nao eh a mesma
verificaLinha(Tabuleiro, [_|R], [_|T], N, AcumuladorLinha):-
    AcumuladorLinha1 is AcumuladorLinha+1,
    verificaLinha(Tabuleiro, R, T, N, AcumuladorLinha1).


% Decimo predicado
inacessiveis(Tabuleiro):-
    /*
        Dado um Tabuleiro, este predicado identifica e marca como relva as celulas
        inacessiveis, ou seja, aquelas em que, independentemente das jogadas, nao
        terao outro tipo de objectos.
    */

    todasCelulas(Tabuleiro, TodasCelulasArvore, a),
    % Mapeia a vizinhanca de todas as celulas contendo arvores
    maplist(vizinhanca, TodasCelulasArvore, TodasVizinhancas),
    % Aplanar a lista de vizinhancas
    flatten(TodasVizinhancas, TodasVizinhancasFiltrado),
    todasCelulas(Tabuleiro, TodasCelulasVariaveis, _),
    % Encontra a intersecao entre as celulas de vizinhanca e as celulas variaveis
    intersection(TodasVizinhancasFiltrado, TodasCelulasVariaveis, Comuns),
    % Encontra as celulas variaveis que nao estao na vizinhanca das celulas de arvores
    subtract(TodasCelulasVariaveis, Comuns, PossivelRelva),
    maplist(insereObjectoCelula(Tabuleiro, r), PossivelRelva).


% Decimo primeiro predicado
aproveita(Puzzle):-
    /*
        Dado um Puzzle na forma (Tabuleiro, Linhas, Colunas), este predicado verifica
        se eh possivel aproveitar o espaco restante no tabuleiro para colocar tendas
        de acordo com as restricoes especificadas nas Linhas e Colunas.
    */

    Puzzle = (Tabuleiro, Linhas, Colunas),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhasTendas, ContagemColunasTendas, t),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhasVariaveis, ContagemColunasVariaveis, _),
    % Subtrai a contagem de tendas especificada nas Linhas
    maplist(subtrair, Linhas, ContagemLinhasTendas, TendasFaltaLinha),
    tamanho_tabuleiro(Tabuleiro, N),
    % Aplica as tendas nas linhas
    aplica(Tabuleiro, ContagemLinhasVariaveis, TendasFaltaLinha, 1, N),
    transpose(Tabuleiro, TabuleiroTrans),
    maplist(subtrair, Colunas, ContagemColunasTendas, TendasFaltaColuna),
    % Aplica as tendas nas colunas
    aplica(TabuleiroTrans, ContagemColunasVariaveis, TendasFaltaColuna, 1, N).

aplica(_,[],[],_,_):-!.

aplica(Tabuleiro, [P|R], [H|T], AcumuladorLinha, N):-
    % Verifica se a contagem de tendas na linha ou coluna eh a mesma que a especificada
    P == H,!,
    insereObjectoEntrePosicoes(Tabuleiro, t, (AcumuladorLinha, 1), (AcumuladorLinha, N)),
    AcumuladorLinha1 is AcumuladorLinha+1,
    aplica(Tabuleiro, R, T, AcumuladorLinha1, N).

% Regra para lidar com o caso em que a contagem nao eh a mesma
aplica(Tabuleiro, [_|R], [_|T], AcumuladorLinha, N):-
    AcumuladorLinha1 is AcumuladorLinha+1,
    aplica(Tabuleiro, R, T, AcumuladorLinha1, N).

% Regra para subtrair dois elementos
subtrair(Elemento1, Elemento2, Resultado) :-
    Resultado is Elemento1 - Elemento2.


% Decimo segundo sredicado
limpaVizinhancas(Puzzle):-
    /*
        Dado um Puzzle, este predicado limpa as vizinhancas
        das tendas marcando as celulas como relva.
    */

    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, TodasCelulasTendas, t),
    % Mapeia a vizinhanca alargada de todas as celulas contendo tendas
    maplist(vizinhancaAlargada, TodasCelulasTendas, TodasVizinhancasTendas),
    flatten(TodasVizinhancasTendas, FiltradoVizinhancas),
    % Insere r nas celulas das vizinhancas das tendas
    maplist(insereObjectoCelula(Tabuleiro, r), FiltradoVizinhancas).


% Decimo terceiro predicado
unicaHipotese(Puzzle):-
    /*
        Dado um Puzzle na forma (Tabuleiro, _, _), este predicado verifica
        se existe uma unica hipotese possivel para a posicao de uma tenda t
        nas vizinhancas das arvores
    */
    
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, TodasCelulasArvore, a),
    maplist(vizinhanca, TodasCelulasArvore, TodasVizinhancasArvore),
    include(verificaVizinhanca(Tabuleiro), TodasVizinhancasArvore, TodasVizinhancasFiltradas),
    flatten(TodasVizinhancasFiltradas, TodasVizinhancasInter),
    maplist(insereObjectoCelula(Tabuleiro, t), TodasVizinhancasInter).

verificaVizinhanca(Tabuleiro, P):-
    % Mapeia os elementos nas posicoes da vizinhanca
    maplist(elemento(Tabuleiro), P, PElementos),
    % Filtra os elementos variaveis
    include(var, PElementos, VoltaP),
    include(==(t), PElementos, VoltaPTenda),
    % Verifica se ha exatamente uma variavel na vizinhanca
    length(VoltaP, 1),
    length(VoltaPTenda, 0), !.

% Regra para obter um elemento nas coordenadas especificadas
elemento(Tabuleiro, (L,C), Elemento):-
    nth1(L, Tabuleiro, LinhaElem),
    nth1(C, LinhaElem, Elemento).

elemento(Tabuleiro, (L,C), Elemento):-
    tamanho_tabuleiro(Tabuleiro, N),
    (
        L=<0;
        L>N;
        C=<0;
        C>N
    ),!,
    (Elemento = r).


% Decimo quarto predicado
valida(LArv, LTen) :-
    /*
        Dadas duas listas LArv (representando as arvores) e LTen (representando as tendas),
        este predicado verifica se as tendas estao colocadas corretamente de acordo com as
        restricoes das arvores.
    */

    % Verifica se o comprimento das listas eh o mesmo
    length(LArv, X),
    length(LTen, X),

    % Chama a regra auxiliar para validar as restricoes
    valida_aux(LArv, LTen),
    % Chama a regra auxiliar n2 para validar existe duas tendas apenas com uma arvore e sem mais nenhuma.
    validaProximidade(LArv, LTen).
    
valida_aux([], _):-!.

valida_aux([A|RA], LTenFiltrado):-
    vizinhanca(A, VizinhancaArvore),

    % Encontra todas as tendas que estao na vizinhanca da arvore
    findall(Tenda,(
    	member(Tenda, LTenFiltrado),
    	member(Tenda, VizinhancaArvore)
    ), (TendasVizinhanca)),
    length(TendasVizinhanca, N),

    % Verifica se ha pelo menos uma tenda na vizinhanca
    N >= 1,
    valida_aux(RA, LTenFiltrado), !.

% Regra auxiliar n2 para validar a proximidade
validaProximidade(LArv, LTen):-
    maplist(vizinhanca, LArv, VizinhancaArvores),
    % Encontra todas as intersecoes entre a vizinhanca e as tendas
    findall(Intersecao, (
        member(Vizinhanca, VizinhancaArvores),
        intersection(Vizinhanca, LTen, Intersecao),
        length(Intersecao, N),
        N >= 2
    ), ListaIntersecao),
    length(ListaIntersecao, L),
    % Se existirem, verifica se em cada intersecao ha uma arvore sem mais nenhuma
    L \== 0, !,

    findall(Coordenada, (
        member(Vizinhanca, ListaIntersecao),
        member(Coordenada, Vizinhanca),
        vizinhanca(Coordenada, VizinhancaTenda),
        intersection(VizinhancaTenda, LArv, ArvoresVizinhanca),
        length(ArvoresVizinhanca, N),
        N \== 1
    ), Instancias),
    
    % Verifica se ha instancias validas
    length(Instancias, N),
    N \== 0.


% Decimo quinto predicado
resolve(Puzzle) :-
    /*
        Dado um Puzzle na forma (Tabuleiro, Linhas, Colunas), este predicado resolve o puzzle
        utilizando diversas estrategias como identificacao de celulas inacessiveis, colocacao
        de tendas, verificacao de unica hipotese e limpeza de vizinhancas.
    */

    Puzzle = (Tabuleiro, _, _),
    % Marca as celulas inacessiveis
    inacessiveis(Tabuleiro),
    % Chama o predicado principal para resolver o Puzzle
    acabarTudo(Puzzle).

acabarTudo(Puzzle):-
    Puzzle = (Tabuleiro, Linhas, Colunas),
    % Cria uma copia do tabuleiro atual
    copy_term(Tabuleiro, Copia),
    % Faz uma jogada (aplica as regras)
    fazerjogada(Puzzle),
    
    % Verifica se houve alteracao no tabuleiro
    (
        (\+compara_tabuleiro(Tabuleiro, Copia),
        acabarTudo(Puzzle), !)
        % Se houve, chama recursivamente para continuar resolvendo
    ;
        (todasCelulas(Tabuleiro, TodasCelulasArvore, a),
        todasCelulas(Tabuleiro, TodasCelulasTenda, t),                                                              
        valida(TodasCelulasArvore, TodasCelulasTenda),
        calculaObjectosTabuleiro(Tabuleiro, Linhas, Colunas, t))
        % Senao, verifica se o tabuleiro eh valido e completo
    ;
        (jogadaAleatoria(Puzzle), !)
        % Se nao for valido, faz uma jogada aleatoria
    ).

fazerjogada(Puzzle):-
    relva(Puzzle),
    aproveita(Puzzle),
    unicaHipotese(Puzzle),
    limpaVizinhancas(Puzzle).

compara_tabuleiro([], []):- !.
compara_tabuleiro([H1|T1], [H2|T2]) :-
    maplist(compara_elementos, H1, H2),
    compara_tabuleiro(T1, T2).

compara_elementos(X, Y):-
    nonvar(X),
    nonvar(Y),
    X==Y,!.
compara_elementos(X, Y):- 
    var(X),
    var(Y), !.

% Estruturacao da jogada aleatoria
jogadaAleatoria(Puzzle):-
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, TodasCelulasArvore, a),
    maplist(vizinhanca, TodasCelulasArvore, TodasVizinhancasArvore),
    flatten(TodasVizinhancasArvore, TodasVizinhancasFiltradas),
    todasCelulas(Tabuleiro, TodasCelulasVariaveis, _),
    % Encontra a intersecao entre as celulas de vizinhanca e as celulas variaveis
    intersection(TodasCelulasVariaveis, TodasVizinhancasFiltradas, PossiveisJogadas),
    member(Celula, PossiveisJogadas),
    insereObjectoCelula(Tabuleiro, t, Celula),
    acabarTudo(Puzzle).