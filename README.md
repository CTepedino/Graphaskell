# Graphaskell

Graph explorer with a Pregel engine in Haskell. 

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

Sample graphs live in `examples/`.

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

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -a CC
```

### Label propagation

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -a LP
```

## Options

| Flag | Description                                                               |
|------|---------------------------------------------------------------------------|
| `-g`, `--graph` | Path to the graph file                                                    |
| `-s`, `--source` | Source node (required for BFS and Bellman-Ford; optional otherwise)       |
| `-t`, `--target` | Target node (required for BFS and Bellman-Ford)                           |
| `-a`, `--algorithm` | `BFS`, `BELLMANFORD`, `PAGERANK`, `CC`, `LP`                              |
| `--threads` | Worker cap per superstep (default: RTS capabilities) |
| `-v`, `--verbose` | Detailed logs per superstep   |

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
