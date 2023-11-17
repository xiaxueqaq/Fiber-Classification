# Fiber-Classification
This project provides an algorithm to classify geometric fibers by their cardinality. Roughly speaking, if $f_1(u,x)=\cdots=f_s(u,x)=0, h(u,x)\ne 0$ is a polynomial system in parameters $u$ and unknowns $x$, the goal is to find the conditions on $u$ such that the system has exactly $d$ distinct (complex) solutions in $x$, where $d\in [0,+\infty]$ ($+\infty$ means there are infinitely many solutions).

# Dependency
- Mathematica 12
- Singular
- Singular Interface to Mathematica (available at [xiaxueqaq/Mathematica-Interface-of-Singular](https://github.com/xiaxueqaq/Mathematica-Interface-of-Singular))

# Install
1. Make sure that Singular is installed. In Windows 10, it should be installed with WSL (Windows Subsytem for Linux).
2. Install Singular Interface to Mathematica, you may follow the instruction in the above link. It is pretty easy, just copy the source code to somewhere Mathematica can automatically find.
3. Download `FiberClassification.wl`.
4. You can open it and run all codes so you can use the functions in it.

# Usage
The package contains two public functions
- `MorphismImage[f,y,x]`
  $f$ is a set of polynomials (generators for an ideal), $y$ is the set of parameters and $x$ is the set of variables. This function returns a list of locally closed sets $L_i$ such that there is a solution in $x$ if and only if the parameters $y$ take value in some $L_i$. In other words, this is an effective version of Chevalley's Theorem on constructible sets.
- `FiberClassify[f,g,h,y,x]`
  $f$ is a set of polynomials in $y$ and $x$, $g$ is a set of polynomials in $y$, $h$ is a polynomial in $y$ and $x$, $y$ is the set of parameter and $x$ is the set of variables. This function classifies the geometric fibers of $D(h)\cap V(f)\to \mathrm{Spec} \mathbb{Q}[y]$ in $V(g)$. In other words, it computes locally closed sets in $V(g)$ whose union is $V(g)$ and the number of distinct complex solutions to $f_1(y,x)=\cdots=f_s(y,x)=0, h(y,x)\ne 0$ is invariant in each set.

# Example
`MorphismImage[{a - x, b - x y}, {a, b}, {x, y}]` computes the image of morphism $\mathbb{A}^2\to \mathbb{A}^2$ defined by $(x,y)\mapsto (x,xy)$. The result is `{{{}, {a}}, {{b, a}, {1}}}`, i.e. $((V(0)-V(a))\cup (V(a,b)-V(1)))=D(a)\cup V(a,b)$.

`FiberClassify[{a x^3 + p x + q}, {}, 1, {a, p, q}, {x}]` classifies the roots of the parametric equation $a x^3+p x+q=0$. The result is `{{{}, {a (4 p^3 + 27 a q^2)}, 3}, {{4 p^3 + 27 a q^2}, {a q, a p}, 2}, {{q, p}, {a}, 1}, {{1}, {a}, 0}, {{a}, {p}, 1}, {{1}, {p}, 0}, {{p, a}, {q, p, a}, 0}, {{q, p, a}, {1}, \[Infinity]}, {{1}, {1}, 0}}`, i.e.
- 3 roots when $a (4 p^3 + 27 a q^2)\ne 0$,
- 2 roots when $4 p^3 + 27 a q^2=0$ but $a q\ne 0$ or $a p\ne 0$,
- 1 root when $p=q=0$ but $a\ne 0$,
- 1 root when $a=0$ but $p\ne 0$,
- 0 root when $a=p=0$ but $q\ne 0$,
- $+\infty$ roots when $a=p=q=0$.

`FiberClassify[{(x y - 1) x, (x  y - 1) y, (x y - 1) z}, {}, 1, {x}, {y, z}]`. The result is `{{{}, {x^4}, \[Infinity]}, {{x}, {1}, 1}, {{1}, {1}, 0}, {{1}, {1}, 0}}`. i.e.
- Infinitely many solutions in $y,z$ when $x^4\ne 0$,
- 1 solution in $y,z$ when $x=0$.
