# Graphaskell

Graph explorer with a **Pregel (BSP)** engine in Haskell: algorithms use `StateT`/`Writer`, concurrent execution uses `async` + STM.

## Requirements

- GHC >= 9.0
- Cabal >= 3.0

## Installation

```bash
cabal update
cabal build
```

## Usage

The graph file defines topology only (`NODES`, `EDGES`, optionally `WEIGHTED`). Source, target, and algorithm are specified via CLI flags.

### BFS (minimum-hop path)

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -s 0 -t 4 -a BFS
```

### Bellman-Ford (minimum weighted path)

```bash
cabal run graphaskell -- -g examples/grafo-dirigido.txt -s 0 -t 3 -a BELLMANFORD
```

### PageRank

```bash
cabal run graphaskell -- -g examples/grafo-pagerank.txt -s 0 -a PAGERANK
```

### Connected components

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -s 0 -a CC
```

### Label propagation

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -s 0 -a LP
```

## Options

| Flag | Description |
|------|-------------|
| `-g`, `--graph` | Path to the graph file |
| `-s`, `--source` | Source node |
| `-t`, `--target` | Target node (required for BFS and Bellman-Ford) |
| `-a`, `--algorithm` | `BFS`, `BELLMANFORD`, `PAGERANK`, `CC`, `LP` |
| `--threads` | Number of threads (default: RTS capabilities) |
| `-v`, `--verbose` | Detailed per-superstep traces |
| `--sequential` | Sequential engine (no async/STM, useful for debugging) |

## Tests

```bash
cabal test
```

## Graph file format

```
NODES 5
EDGES
0 1
0 2
1 3
2 3
3 4
```

For weighted graphs, add the `WEIGHTED` directive and a weight per edge:

```
NODES 4
WEIGHTED
EDGES
0 1 4
0 2 1
```
