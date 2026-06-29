# Graphaskell

Graph explorer with a **Pregel (BSP)** engine in Haskell. Each algorithm is an `AlgorithmSpec` (pure init, bootstrap, vertex update, and result extraction). Supersteps run with parallel per-vertex compute (`async` worker pool) and STM message queues between steps.

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

Sample graphs live in `examples/`. Expected results and commands for each algorithm: [docs/ejemplos.md](docs/ejemplos.md).

### BFS (minimum-hop path)

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -s 0 -t 4 -a BFS
```

### Bellman-Ford (minimum weighted path)

```bash
cabal run graphaskell -- -g examples/grafo-weighted.txt -s 0 -t 3 -a BELLMANFORD
```

### PageRank

```bash
cabal run graphaskell -- -g examples/grafo-pagerank.txt -a PAGERANK
```

### Connected components

Returns every component in the graph (grouped by label). `--source` is optional and does not affect the result.

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -a CC
```

### Label propagation

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -a LP
```

## Options

| Flag | Description |
|------|-------------|
| `-g`, `--graph` | Path to the graph file |
| `-s`, `--source` | Source node (required for BFS and Bellman-Ford; optional otherwise) |
| `-t`, `--target` | Target node (required for BFS and Bellman-Ford) |
| `-a`, `--algorithm` | `BFS`, `BELLMANFORD`, `PAGERANK`, `CC`, `LP` |
| `--threads` | Worker cap per superstep (flush + vertex compute; default: RTS capabilities) |
| `-v`, `--verbose` | Traza detallada por superstep (vértices activos, mensajes y actualizaciones) |

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
