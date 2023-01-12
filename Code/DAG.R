#install.packages("DiagrammeR")
library("DiagrammeR")

#https://rich-iannone.github.io/DiagrammeR/mermaid.html
#for mermaid

mermaid(diagram="
 graph LR
 A(Air Pollution)-->B
 
",
)


#https://donlelek.github.io/2015-03-31-dags-with-r/
#for grViz
#https://graphviz.org/doc/info/attrs.html
#shapes and colors of nodes and edges

grViz("
digraph causal {


node [shape=plaintext
      style=filled
      fillcolor=lightgrey] #color = to change the color of the frame
A [label='Air Pollution'] 
B 
C
D 
E


edge [color = black
      ]
rankdir = LR

      
A->B
A->C

B->D

D->E
C->E

C->D

}")
