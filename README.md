# Graphaskell

Explorador de grafos con motor estilo **Pregel (BSP)** en Haskell: algoritmos con `StateT`/`Writer`, ejecución concurrente con `async` + STM.

## Requisitos

- GHC >= 9.0
- Cabal >= 3.0

## Instalación

```bash
cabal update
cabal build
```

## Uso

El archivo de grafo define solo la topología (`NODES`, `EDGES`, opcionalmente `WEIGHTED`). Origen, destino y algoritmo se indican por línea de comandos.

### BFS (camino mínimo en saltos)

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -s 0 -t 4 -a BFS
```

### Bellman-Ford (camino mínimo ponderado)

```bash
cabal run graphaskell -- -g examples/grafo-dirigido.txt -s 0 -t 3 -a BELLMANFORD
```

### PageRank

```bash
cabal run graphaskell -- -g examples/grafo-pagerank.txt -s 0 -a PAGERANK
```

### Componentes conexas

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -s 0 -a CC
```

### Label propagation

```bash
cabal run graphaskell -- -g examples/grafo-simple.txt -s 0 -a LP
```

## Opciones

| Flag | Descripción |
|------|-------------|
| `-g`, `--graph` | Ruta al archivo de grafo |
| `-s`, `--source` | Nodo origen |
| `-t`, `--target` | Nodo destino (requerido para BFS y Bellman-Ford) |
| `-a`, `--algorithm` | `BFS`, `BELLMANFORD`, `PAGERANK`, `CC`, `LP` |
| `--threads` | Cantidad de threads (default: capacidades RTS) |
| `-v`, `--verbose` | Trazas detalladas por superstep |
| `--sequential` | Motor secuencial (sin async/STM, útil para depurar) |

## Tests

```bash
cabal test
```

## Formato del archivo de grafo

```
NODES 5
EDGES
0 1
0 2
1 3
2 3
3 4
```

Para grafos ponderados, agregar la directiva `WEIGHTED` y un peso por arista:

```
NODES 4
WEIGHTED
EDGES
0 1 4
0 2 1
```
