# string-parser
This is a string parser written in functional progaramming language(SCHEME/DrRacket).

The parser will classify the strings according to the following rules:


~~~
number → {digit}[^+]

identifier → letter{letter | digit}[^∗]

variable → identifier | identifier[expression]

term → number | variable | (expression)

expression → term{+ expression}[^∗]

assignment → variable = expression

[^+]: what it means in language and automaton

[^*]: what it means in language and automaton

~~~

the spaces leading and preceding spaces won't matter in string
and the spaces in between the string will matter only when parsing as a number or an identifier

