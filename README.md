# ocaml-depgraph

Generate a dot graph from your cmi files.

# How To Use:

2 ways to provide cmi's to this utility.

Easiest one is to provide a directory that will be recursively walked for ml
and mli sources

```
$ depgraph src > depend.dot
```

Second one is to provide a newline separated list of files through STDIN.

For example:

```
$ find src -iname "*.ml" -or -iname "*.mli" | depgraph > depend.dot
```

At the end you may view your graph with something like:
```
$ xdot depend.dot
```