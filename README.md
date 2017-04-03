# string-parser
This is a string parser written in functional progaramming language(SCHEME/DrRacket).

The parser will classify the strings according to the following rules:


**
number → {digit}<sup>*</sup>

**
identifier → letter{letter | digit}<sup>*</sup>

**
variable → identifier | identifier[expression]

**
term → number | variable | (expression)

**
expression → term{+ expression}<sup>*</sup>

**
assignment → variable = expression

<sup>*<sup>: what it means in language and automaton


the spaces leading and preceding spaces won't matter in string
and the spaces in between the string will matter only when parsing as a number or an identifier

the following are some inputs and then there outputs

~~~
Inputs

(identifier-p "a12345")

(variable-p "x[a12345+1]")

(term-p "(a+(b+c))")

(expression-p "(a+(b+c))+d[e]")

(assignment-p "a[a[l]] = a[3+i] + b[2] + xyz")
~~~

~~~
Outputs

(#(struct:ident "a12345") . "")

(#(struct:gnode ARRAY (#(struct:ident "x") #(struct:gnode PLUS (#(struct:ident "a12345") #(struct:num 1))))) . "")

(#(struct:gnode PLUS (#(struct:ident "a") #(struct:gnode PLUS (#(struct:ident "b") #(struct:ident "c"))))) . "")

(#(struct:gnode
   PLUS
   (#(struct:gnode PLUS (#(struct:ident "a") #(struct:gnode PLUS (#(struct:ident "b") #(struct:ident "c")))))
    #(struct:gnode ARRAY (#(struct:ident "d") #(struct:ident "e")))))
 .
 "")
 
 (#(struct:gnode
   ASSIGN
   (#(struct:gnode ARRAY (#(struct:ident "a") #(struct:gnode ARRAY (#(struct:ident "a") #(struct:ident "l")))))
    #(struct:gnode
      PLUS
      (#(struct:gnode ARRAY (#(struct:ident "a") #(struct:gnode PLUS (#(struct:num 3) #(struct:ident "i")))))
       #(struct:gnode PLUS (#(struct:gnode ARRAY (#(struct:ident "b") #(struct:num 2))) #(struct:ident "xyz")))))))
 .
 "")
 ~~~
 
 








