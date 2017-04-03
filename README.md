# string-parser
This is a string parser written in functional progaramming language(SCHEME/DrRacket).

The parser will classify the strings according to the following rules:



number → {digit}[^+]

identifier → letter{letter | digit}[^∗]

variable → identifier | identifier[expression]

term → number | variable | (expression)

expression → term{+ expression}[^∗]

assignment → variable = expression

