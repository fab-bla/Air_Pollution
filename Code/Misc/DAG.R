#install.packages("DiagrammeR")
library("DiagrammeR")

#https://rich-iannone.github.io/DiagrammeR/mermaid.html
#for mermaid

mermaid(diagram="
 graph LR
 A(Air Pollution)-->B(Health)
 
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
B [label='Health'] 
C [label='acute diseases'] 
D [label='chronic diseases'] 
E [label='urban employee'] 
F [label='urban unemployed'] 
G [label='rural inhabitants'] 
H [label='Health Care Expenditures'] 


edge [color = black
      ]
rankdir = LR

      
A->B
B->C
B->D
C->E
D->E
D->F
D->G
E->H
F->H
G->H


}")
