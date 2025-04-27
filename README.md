### decrepency
z3:
```
sudo apt-get install z3 libz3-dev
```

### problem construction
for arbitrary graph

$$
G := \{V_i \in \mathcal{V}, E_{ij} \in \mathcal{E} \}
$$

and its corresponding set of colors

$$
C := \{C_c \in \mathcal{C}\}
$$

we assign arbitrary node $V_i$ and its corresponding color $C_c$ a Bool typed variable

$$
b_{i, c} := Bool(V_i, C_c)
$$

then the graph coloring problem can be translated into a `SAT` problem with $N * M$ Bool typed variables(where $N$ is the number of nodes in graph $G$, $M$ is the number of colors in set $C$)


according to `ref: https://airccj.org/CSCP/vol3/csit3213.pdf`， graph coloring problem has three types of clause(or constraint): 
#### Type 1
two nodes connected with one edge can not have the same color

#### Type 2
every node must have at least one color

#### Type 3
every node can not have more than one color

### to run the demo
```
cabal run
```