#set page(paper: "a4")
#set text(font: "New Computer Modern")

#show sym.eq.def: math.scripts

#show link: set text(fill: blue)
#show link: underline
#show math.equation: set text(13pt)

== Justificacio\u{0301}n
Se pide demostrar que toda expresión tiene un literal más que su cantidad de operadores. Es decir, en lenguaje formal:

$ forall e :: "Expr". "cantLit" e = S ("cantOp" e) $

\

a) Se define el predicado unario $P$ para la demostración por inducción estructural sobre la estructura $e :: "Expr".$

$ P(e) :eq.def "cantLit" e eq S thick ("cantOp" e) $

b) El esquema formal de inducción estructural se define a continuación:

\

Para probar que 
$ forall e :: "Expr." P("e") $
Solo basta mostrar que se cumple:

1. El predicado $P$ aplicado a los constructores _no recursivos_ de $"Expr"$ :

\
$forall x :: "Float." P("Const" x) \ 
  forall a :: "Float." forall b :: "Float." P("Rango a b")$

2. El predicado $P$ vale para los constructores recursivos asumiendo que valen los parametros de tipo Expr. (en el caso de nuestro ejercicio solo hacemos el de la suma).
$
forall "e1" :: "Expr." forall "e2" :: "Expr." \
underbrace((P("e1") and P("e2")), "H.I.") arrow.double.long.r underbrace((P("Suma" "e1" "e2")), "T.I.")
$
Con este razonamiento se demuestra. 
$ forall e :: "Expr". P(e) $

#pagebreak()
== Demostracio\u{0301}n
#show math.equation: set text(10pt)

=== Casos Base
$forall x :: "Float." 
P("Const" x) := "cantLit" ("Const" x) &= S ("cantOp" ("Const" x))$

\

$ "cantLit" ("Const" x) &= S ("cantOp" ("Const" x)) \
  {"L1"} #h(1cm) "S Z" &= "S Z" #h(1cm) {"O1"} $

#align(right)[
  #scale(130%)[$square.stroked$]
]
\

$forall a :: "Float." forall b :: "Float." 
P("Rango a b") := "cantLit" ("Rango a b") &= S ("cantOp" ("Rango a b"))$

\

$ "cantLit" ("Rango a b") &= S ("cantOp" ("Rango a b")) \
  {"L2"} #h(1cm) "S Z" &= "S Z" #h(1cm) {"O2"} $

#align(right)[
  #scale(130%)[$square.stroked$]
]

=== Caso Inductivo
$forall "e1" :: "Expr." forall "e2" :: "Expr."$ \
\
$"H.I" := "cantLit" "e1" = S ("cantOp e1") and "cantLit" "e2" = S ("cantOp e2")$ \
$"TI" := "cantLit" ("Suma" "e1" "e2") = S ("cantOp" ("Suma" "e1" "e2"))$

\

$ "cantLit" ("Suma" "e1" "e2") &= S ("cantOp" ("Suma" "e1" "e2")) \
  {"L3"}      #h(2cm) "suma" ("cantLit" "e1") ("cantLit" "e2") &= S (S ("suma" ("cantOp" "e1") ("cantOp" "e2")))   #h(1cm) {"O3"} \
  {"H.I"}     #h(1cm) "suma" (S ("cantOp e1")) (S ("cantOp e2")) &= S (S ("suma" ("cantOp" "e2") ("cantOp" "e1"))) #h(1cm) {"CONMUT"} \
  {"S2"}      #h(1cm) S ("suma" ("cantOp" "e1") (S ("cantOp" "e2"))) &= S (S ("suma" ("cantOp" "e2") ("cantOp" "e1"))) \
  {"CONMUT"}  #h(1cm) S ("suma" (S ("cantOp" "e2")) ("cantOp" "e1")) &= S (S ("suma" ("cantOp" "e2") ("cantOp" "e1"))) \
  {"S2"}      #h(1cm) S (S ("suma" ("cantOp" "e2") ("cantOp" "e1"))) &= S (S ("suma" ("cantOp" "e2") ("cantOp" "e1"))) \
$
#align(right)[
  #scale(130%)[$square.stroked$]
]


Luego juntando las demostraciones auxiliares de los casos base y el caso inductivo, podemos concluir

#show math.equation: set text(13pt)
$ forall e :: "Expr". "cantLit" e = S ("cantOp" e) $

#align(right)[
  #scale(130%)[QED $qed$]
]
