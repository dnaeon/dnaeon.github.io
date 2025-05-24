---
layout: post
title: makefile-graph with Apache Echarts support
tags: graph gnu make dot graphviz echarts
---

In a [previous post](https://dnaeon.github.io/makefile-graph/) I've talked about
[makefile-graph](https://github.com/dnaeon/makefile) - a tool which lets you
visualize your Makefile targets so you can better understand how targets relate
to others.

The `makefile-graph` tool initially supported two output formats - `dot` and
`tsort`. The `tsort` format prints the topological order after performing a
[topological sort](https://en.wikipedia.org/wiki/Topological_sorting) of the
graph, and the `dot` format outputs a representation of the graph in the [Dot
language](https://graphviz.org/doc/info/lang.html).

Starting with version `v0.1.4` of `makefile-graph` we can now render a graph of
the Makefile targets using [Apache
Echarts](https://echarts.apache.org/en/index.html), which provides a better
interactive experience when inspecting your Makefile targets.

In order to install the latest version of `makefile-graph` execute the following
command.

``` shell
go install github.com/dnaeon/makefile-graph/cmd/makefile-graph@latest
```

In order to render a graph using Apache Echarts we need to specify the `--format
echarts` option, e.g.

``` shell
makefile-graph \
    --makefile examples/Makefile \
    --direction LR \
    --format echarts
```

The command above will generate an HTML document, which you can save and open.

![_config.yml]({{ site.baseurl }}/images/makefile-graph-echarts-white.gif)

If you want to use a different theme (e.g. `dark`, `vintage`, etc.) you can
specify the Echarts theme using the `--theme` option. The font size and style
can be controlled via the `--font-size` and `--font-style` options
respectively. For example this command will render the graph using the `dark`
theme of Echarts.

``` shell
makefile-graph \
    --makefile examples/Makefile \
    --direction LR \
    --theme dark \
    --format echarts
```

![_config.yml]({{ site.baseurl }}/images/makefile-graph-echarts-dark.gif)

For additional information and examples, please refer to the
[dnaeon/makefile-graph](https://github.com/dnaeon/makefile-graph) repo.
