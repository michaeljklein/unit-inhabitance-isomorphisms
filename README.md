# unit-inhabitance-isomorpmisms

## Intro

The goal with Flock is to bootstrap it, though Haskell, into compiling to LLVM-IR, with near-complete representation.

The core of Flock doesn't need much more than a regular-tree-language parser/printer, some core functions (the push/pull cycle),
and some integer calculations and/or solvers (depending on how dependent your typing is, haha).

Since compiling Flock to LLVM-IR, in Flock, includes a modicum of dependent typing (see the paper),
implementing a [self-compiling compiler](https://en.wikipedia.org/wiki/Bootstrapping_(compilers)) would provide a
finite, dependently typed implementation of the above.

I think it's nice that natural numbers are built in, and I've found some evidence to suggest that Flock's structure prevents them from
blowing up in complexity..

## Idea

The idea is that:
  smaller sub-languages (with smaller maximum inhabitance types, step numbers)
  can be used to bootstrap solving type constraint systems efficiently enough to be practical.

Flock is designed in such a way that the harder-to-solve stuff can be added in gradually.

For example, if the inhabitance-numbers for a type are all set to 1 (and maybe some empty arguments),
  type checking is reduced to checking applications:

We get the language:

```haskell
     data Expr = One | Expr -> Expr
   `iso`
     data Expr = Maybe (Expr, Expr)
   `iso`
     Fix (Maybe . Join (,))
```

[`src/Lib`](`src/Lib`) implements these isomorphisms as QuickCheck properties.

