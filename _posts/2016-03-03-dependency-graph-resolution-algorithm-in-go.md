---
layout: post
title: Dependency graph resolution algorithm in Go
tags: golang programming dependency graph resolution algorithm
---
I've been continuing to improve [Gru](https://github.com/dnaeon/gru)
over the past days and one thing I thought would be nice to have is a
way to express commands in Gru in a declarative way. That way I would
not have to bother about the underlying details about a minion and
have an easy way to express things, e.g. install packages, manage
services, deploy configurations, etc.

### TL;DR

In this post we see how to resolve dependency graphs in Go by
implementing a simple, but efficient algorithm.

You can find  the code used in this post at the
[Go Dependency Graph Resolution Algorithm](https://github.com/dnaeon/go-dependency-graph-algorithm)
repository.

### End of TL;DR

Possible solution would be to use *resources* as a way to abstract
things, and an example configuration sent to minions may look
like this, expressed in [HCL](https://github.com/hashicorp/hcl).

```hcl
resource "package" {
  name = "tmux"
  state = "present"
}
```

Having implemented the base resource interfaces and such I thought
about having dependencies between the resources. This is how an
example configuration using resource dependencies would look like.

```hcl
resource "package" {
  name = "tmux"
  state = "present"
}

resource "file" {
  name = "/home/dnaeon/.tmux.conf"
  state = "present"
  owner = "dnaeon"
  group = "dnaeon"
  want = [
    "package[tmux]",
  ]
}
```

Looking at the above example configuration we can clearly see the
relation between our resources.

However one obvious issue arises once we start introducing dependencies
between our resources - and that is - how do we properly resolve them?

Most of the time resolving dependencies such as the one shown above
means dealing with the
[Graph theory](https://en.wikipedia.org/wiki/Graph_theory) and
implementing an algorithm in order to resolve the dependency graph.

[![Graph Theory](https://upload.wikimedia.org/wikipedia/commons/thumb/5/5b/6n-graf.svg/2000px-6n-graf.svg.png)](https://en.wikipedia.org/wiki/Graph_theory)

There are some good graph packages for Go such as the
[gonum/graph](https://github.com/gonum/graph) package, but for my
specific case I wanted something as simple as possible without
having to pull lots of library dependencies, which would happen if I
have to use an external solution.

It was yesterday when I came across the
[An example dependency resolution algorithm in Python](https://breakingcode.wordpress.com/2013/03/11/an-example-dependency-resolution-algorithm-in-python/),
post which implements a simple, but efficient algorithm for resolving
dependency graphs. Even better, the algorithm uses an iterative
approach rather than a recursive one.

I liked the implementation - it is simple, easy to understand and
efficient. Thought I should translate it to Go.

What the algorithm below does is instead of traversing the graph
recursively, is instead iteratively finding nodes with *no* dependencies.

Every node with *no* dependencies is then removed from the graph. If at
any point in time there are still nodes in the graph, but we cannot
find nodes with *no* dependencies - then we have a
circular dependency in our graph.

Having said the above, lets implement now the algorithm.

First, lets create the type for our graph nodes.

```go
// Node represents a single node in the graph with it's dependencies
type Node struct {
	// Name of the node
	name string

	// Dependencies of the node
	deps []string
}

// NewNode creates a new node
func NewNode(name string, deps ...string) *Node {
	n := &Node{
		name: name,
		deps: deps,
	}

	return n
}
```

A graph represents a collection of nodes, so lets create a
type for the graph as well.

```go
type Graph []*Node
```

We will also implement a function which will display the
dependency graph for us.

```go
// Displays the dependency graph
func displayGraph(graph Graph) {
	for _, node := range graph {
		for _, dep := range node.deps {
			fmt.Printf("%s -> %s\n", node.name, dep)
		}
	}
}
```

Now lets implement the iterative algorithm which will resolve the
graph.

```go
// Resolves the dependency graph
func resolveGraph(graph Graph) (Graph, error) {
	// A map containing the node names and the actual node object
	nodeNames := make(map[string]*Node)

	// A map containing the nodes and their dependencies
	nodeDependencies := make(map[string]mapset.Set)

	// Populate the maps
	for _, node := range graph {
		nodeNames[node.name] = node

		dependencySet := mapset.NewSet()
		for _, dep := range node.deps {
			dependencySet.Add(dep)
		}
		nodeDependencies[node.name] = dependencySet
	}

	// Iteratively find and remove nodes from the graph which have no dependencies.
	// If at some point there are still nodes in the graph and we cannot find
	// nodes without dependencies, that means we have a circular dependency
	var resolved Graph
	for len(nodeDependencies) != 0 {
		// Get all nodes from the graph which have no dependencies
		readySet := mapset.NewSet()
		for name, deps := range nodeDependencies {
			if deps.Cardinality() == 0 {
				readySet.Add(name)
			}
		}

		// If there aren't any ready nodes, then we have a cicular dependency
		if readySet.Cardinality() == 0 {
			var g Graph
			for name := range nodeDependencies {
				g = append(g, nodeNames[name])
			}
			displayGraph(g)

			return nil, errors.New("Circular dependency found")
		}

		// Remove the ready nodes and add them to the resolved graph
		for name := range readySet.Iter() {
			delete(nodeDependencies, name.(string))
			resolved = append(resolved, nodeNames[name.(string)])
		}

		// Also make sure to remove the ready nodes from the
		// remaining node dependencies as well
		for name, deps := range nodeDependencies {
			diff := deps.Difference(readySet)
			nodeDependencies[name] = diff
		}
	}

	return resolved, nil
}
```

The above code uses a `mapset.Set` type which can be found in the
[golang-set](https://github.com/deckarep/golang-set) package.

And finally lets create some dependency graphs and resolve them.

```go
func main() {
	//
	// A working dependency graph
	//
	nodeA := NewNode("A")
	nodeB := NewNode("B")
	nodeC := NewNode("C", "A")
	nodeD := NewNode("D", "B")
	nodeE := NewNode("E", "C", "D")
	nodeF := NewNode("F", "A", "B")
	nodeG := NewNode("G", "E", "F")
	nodeH := NewNode("H", "G")
	nodeI := NewNode("I", "A")
	nodeJ := NewNode("J", "B")
	nodeK := NewNode("K")

	var workingGraph Graph
	workingGraph = append(workingGraph, nodeA, nodeB, nodeC, nodeD, nodeE, nodeF, nodeG, nodeH, nodeI, nodeJ, nodeK)

	fmt.Printf(">>> A working dependency graph\n")
	displayGraph(workingGraph)

	resolved, err := resolveGraph(workingGraph)
	if err != nil {
		fmt.Printf("Failed to resolve dependency graph: %s\n", err)
	} else {
		fmt.Println("The dependency graph resolved successfully")
	}

	for _, node := range resolved {
		fmt.Println(node.name)
	}

	//
	// A broken dependency graph with circular dependency
	//
	nodeA = NewNode("A", "I")

	var brokenGraph Graph
	brokenGraph = append(brokenGraph, nodeA, nodeB, nodeC, nodeD, nodeE, nodeF, nodeG, nodeH, nodeI, nodeJ, nodeK)

	fmt.Printf(">>> A broken dependency graph with circular dependency\n")
	displayGraph(brokenGraph)

	resolved, err = resolveGraph(brokenGraph)
	if err != nil {
		fmt.Printf("Failed to resolve dependency graph: %s\n", err)
	} else {
		fmt.Println("The dependency graph resolved successfully")
	}
}
```

When we run our code it produces the following results.

```text
>>> A working dependency graph
C -> A
D -> B
E -> C
E -> D
F -> A
F -> B
G -> E
G -> F
H -> G
I -> A
J -> B
The dependency graph resolved successfully
K
A
B
F
I
J
C
D
E
G
H
>>> A broken dependency graph with circular dependency
A -> I
C -> A
D -> B
E -> C
E -> D
F -> A
F -> B
G -> E
G -> F
H -> G
I -> A
J -> B
Failed to resolve dependency graph: Circular dependency found
```

You can find the code used in this post at the
[Go Dependency Graph Resolution Algorithm](https://github.com/dnaeon/go-dependency-graph-algorithm)
repository.
