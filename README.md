# Projeto LP - Tendas e Árvores

Este é um programa em Prolog para resolver puzzles "Tendas e Árvores". O objetivo é organizar um "festival de verão", posicionando tendas em torno de árvores, seguindo algumas regras específicas.

## Puzzle "Tendas e Árvores"

É dado um tabuleiro (matriz NxN) com árvores em algumas posições. O número de tendas em cada linha/coluna é especificado. O objetivo é posicionar tendas ao redor de cada árvore, respeitando as regras.

![Exemplo de um puzzle inicial](https://external-content.duckduckgo.com/iu/?u=http%3A%2F%2Fthebadgersett.us%2Fwizardingtimes%2Fimages%2Ftents.jpg&f=1&nofb=1&ipt=5e627bcc59cad1b5be523a8322237a954cbca7697892e1efeb7d9886fc076bc9&ipo=images)

## Estruturas de dados

O puzzle é representado por um triplo contendo:
- Um tabuleiro (matriz).
- Uma lista com o número de tendas por linha.
- Uma lista com o número de tendas por coluna.

Exemplo:
```prolog
puzzle(6-13, (Tabuleiro, Linhas, Colunas)).
