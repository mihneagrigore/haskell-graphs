# Graph Algorithms in Haskell

A Haskell implementation of graph traversal algorithms using a functional programming approach. This project focuses on deriving efficient graph algorithms through equational reasoning while leveraging immutable data structures and recursion.

## Features

- Depth-First Search (DFS)
- Stack-based DFS implementation
- Functional graph traversal
- Immutable graph representation
- Efficient neighbor exploration using sets
- Recursive algorithm derivation through equational reasoning

---

## Overview

The project implements graph traversal algorithms over a generic graph representation while emphasizing functional programming techniques and correctness.

The stack-based Depth-First Search implementation is derived from a recursive specification using equational reasoning. Instead of repeatedly concatenating recursive results, the algorithm maintains an explicit list of nodes to visit, significantly improving traversal efficiency while preserving the same traversal order.

Graphs are represented using immutable data structures, and neighbor exploration relies on sets to provide generic support for ordered node types.

---

## Implemented Algorithms

- Recursive Depth-First Search (DFS)
- Efficient stack-based DFS
- Neighbor exploration
- Graph traversal utilities

---

## Design Principles

The implementation focuses on:

- Pure functional programming
- Immutable data structures
- Recursive algorithm design
- Equational reasoning
- Generic graph representations

---

## Technologies

- Haskell
- Functional Programming
- Graph Algorithms
- Recursion
- Immutable Data Structures
- Data.Set
