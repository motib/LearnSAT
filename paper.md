---
title: 'LearnSAT: A SAT Solver for Education'
tags:
- SAT solving
- computer science education
- Prolog
authors:
- name: Mordechai (Moti) Ben-Ari
  orcid: 0000-0002-2091-5107
  affiliation: "1"
affiliations:
- name: Weizmann Institute of Science
  index: 1
- name: Disney Inc.
  index: 2
date: 25 February 2018
bibliography: learnsat.bib
---

# Summary

Many problems can be solved by encoding them as formulas in propositional logic which are checked for satisfiability by a program called a SAT solver. If found, a satisfying assignment provides the answer to the problem. Donald Knuth [@knuth-sat] calls SAT solvers a *killer app* diverse practical problems can be solved by a single, highly-optimized, program. Despite its importance SAT solvers are described only in research papers. LearnSAT is designed for teaching and learning SAT solving by providing a concise, well-documented, implementation, fine-grained tracing (both textual and graphical) of the algorithms and a detailed tutorial.

LearnSAT implements the core algorithms of modern SAT solvers [@SAT]: DPLL with conflict-driven clause learning (CDCL) and non-chronological backtracking (NCB). CDCL is implemented by backwards resolution from a conflict clause to a unique implication point (UIP) [@mlm]. It is also possible to compute dominators in the implication graph. In addition, two heuristics for lookahead are implemented.

The user of LearnSAT can choose any subset of 24 display options in order to tailor the output to a specific learning context. The display options include elementary steps like decision assignments, unit propagations and identifying conflict clauses, as well as the advanced steps of CDCL: the resolution steps used to obtain a learned clause and the search for UIPs.

LearnSAT can generate two types of graphs that are rendered using the `dot` tool: trees showing the search through the assignments and implication graphs that display the process for learning clauses from conflicts.

LearnSAT is implemented in Prolog and was developed using `SWI-Prolog` available for Windows, MacOS and Linux.

Three documents are included in the project:
- `learnsat-overview` An overview of LearnSAT. 
- `learnsat-ug` User's guide and software documentation.
- `learnsat-tutorial` Tutorial on SAT solving using LearnSAT.

About 20 examples are included of encodings of problems that can be solved by LearnSAT.
