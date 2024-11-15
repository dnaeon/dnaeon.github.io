---
layout: post
title: Tracking Kubernetes resource origin with kustomize
tags: kustomize dot kubernetes graph graphviz
---
[kustomize](https://kustomize.io/) is a configuration management tool, which
allows you to manage Kubernetes resources in a template-free way.

With `kustomize` you can define a base set of resources describing the desired
state of an application, and then customize the configuration for a specific
environment by leveraging
[overlays](https://kubectl.docs.kubernetes.io/guides/introduction/kustomize/#2-create-variants-using-overlays).

You can also use `kustomize` to [generate secrets and
configmaps](https://kubectl.docs.kubernetes.io/guides/config_management/secrets_configmaps/).

The template-free approach of `kustomize` is something which I like, because it
keeps the whole configuration more explicit, and does not introduce complexity
via some magic template expansion. Another nice thing about `kustomize` is that
it is already integrated into `kubectl`.

In order to get familiar with `kustomize` and the concepts it uses, please make
sure to check the [Introduction to
Kustomize](https://kubectl.docs.kubernetes.io/guides/introduction/kustomize/).

In this post we are going to see how we can track the origin of Kubernetes
resources produced by `kustomize`, and then visualize them.

Let's create a sample kustomization based on the [kustomize
helloWorld](https://github.com/kubernetes-sigs/kustomize/tree/master/examples/helloWorld)
example.

``` yaml
---
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
metadata:
  name: hello-world

resources:
  - https://github.com/kubernetes-sigs/kustomize//examples/helloWorld/?timeout=120
```

Now, let's build it.

``` shell
kustomize build
```

The output from the command above is this.

``` yaml
apiVersion: v1
data:
  altGreeting: Good Morning!
  enableRisky: "false"
kind: ConfigMap
metadata:
  labels:
    app: hello
  name: the-map
---
apiVersion: v1
kind: Service
metadata:
  labels:
    app: hello
  name: the-service
spec:
  ports:
  - port: 8666
    protocol: TCP
    targetPort: 8080
  selector:
    app: hello
    deployment: hello
  type: LoadBalancer
---
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: hello
  name: the-deployment
spec:
  replicas: 3
  selector:
    matchLabels:
      app: hello
      deployment: hello
  template:
    metadata:
      labels:
        app: hello
        deployment: hello
    spec:
      containers:
      - command:
        - /hello
        - --port=8080
        - --enableRiskyFeature=$(ENABLE_RISKY)
        env:
        - name: ALT_GREETING
          valueFrom:
            configMapKeyRef:
              key: altGreeting
              name: the-map
        - name: ENABLE_RISKY
          valueFrom:
            configMapKeyRef:
              key: enableRisky
              name: the-map
        image: monopole/hello:1
        name: the-container
        ports:
        - containerPort: 8080
```

This fairly simple example shows how to create a new `kustomization.yaml`, which
is based on a remote repo. The result of our kustomization is just three
resources - a `Deployment`, a `Service` and a `ConfigMap`.

Where things may become complicated is when your `kustomization.yaml` builds on
top of multiple bases, each of which provides a lot of resources.

In these situations understanding where a given resource comes from becomes a
challenge. Fortunately, in kustomize we can enable
[originAnnotations](https://github.com/kubernetes-sigs/kustomize/blob/master/api/types/kustomization.go#L17-L30),
which helps in this regard.

When we enable `originAnnotations` in our `kustomization.yaml` kustomize will
add annotations to each resource it produces, which describes where the
resources originate from.

Let's try it out.

``` yaml
---
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
metadata:
  name: hello-world

buildMetadata:
  - originAnnotations

resources:
  - https://github.com/kubernetes-sigs/kustomize//examples/helloWorld/?timeout=120
```

This time when we `kustomize build` we get this output instead. Notice the newly
added `config.kubernetes.io/origin` annotation by `kustomize`.

``` yaml
apiVersion: v1
data:
  altGreeting: Good Morning!
  enableRisky: "false"
kind: ConfigMap
metadata:
  annotations:
    config.kubernetes.io/origin: |
      path: examples/helloWorld/configMap.yaml
      repo: https://github.com/kubernetes-sigs/kustomize
  labels:
    app: hello
  name: the-map
---
apiVersion: v1
kind: Service
metadata:
  annotations:
    config.kubernetes.io/origin: |
      path: examples/helloWorld/service.yaml
      repo: https://github.com/kubernetes-sigs/kustomize
  labels:
    app: hello
  name: the-service
spec:
  ports:
  - port: 8666
    protocol: TCP
    targetPort: 8080
  selector:
    app: hello
    deployment: hello
  type: LoadBalancer
---
apiVersion: apps/v1
kind: Deployment
metadata:
  annotations:
    config.kubernetes.io/origin: |
      path: examples/helloWorld/deployment.yaml
      repo: https://github.com/kubernetes-sigs/kustomize
  labels:
    app: hello
  name: the-deployment
spec:
  replicas: 3
  selector:
    matchLabels:
      app: hello
      deployment: hello
  template:
    metadata:
      labels:
        app: hello
        deployment: hello
    spec:
      containers:
      - command:
        - /hello
        - --port=8080
        - --enableRiskyFeature=$(ENABLE_RISKY)
        env:
        - name: ALT_GREETING
          valueFrom:
            configMapKeyRef:
              key: altGreeting
              name: the-map
        - name: ENABLE_RISKY
          valueFrom:
            configMapKeyRef:
              key: enableRisky
              name: the-map
        image: monopole/hello:1
        name: the-container
        ports:
        - containerPort: 8080
```

Now, this is much better as we can clearly see where a given resource came from.
However, when having lots of resources it's still a challenge to visually parse
these annotations, but we can do better than that.

Some time ago I've worked on creating a tool which parses the origin annotations
produced by `kustomize` and renders a graph of the resources and their origins.

You can find the code at the
[dnaeon/kustomize-dot](https://github.com/dnaeon/kustomize-dot) repo.

![_config.yml]({{ site.baseurl }}/images/kustomize-dot-1.svg)

[dnaeon/kustomize-dot](https://github.com/dnaeon/kustomize-dot) can operate in
two modes - as a standalone CLI application, or as a [KRM Function
plugin](https://kubectl.docs.kubernetes.io/guides/extending_kustomize/containerized_krm_functions/).

In order to generate a graph of the Kubernetes resources and their origin when
building a kustomization target we need to enable the `originAnnotations` build
option in our `kustomization.yaml` file, just like we did for the `helloWorld`
example.

The tool is flexible and supports filtering of resources, highlighting of
resources or whole namespaces, setting graph layout direction, etc. This is
useful when we want to get a more focused view of the resulting graph.

This here is the representation of all the resources for [kube-prometheus
operator](https://github.com/prometheus-operator/kube-prometheus).

![_config.yml]({{ site.baseurl }}/images/kustomize-dot-kube-prometheus-full.svg)

The resulting graph is big, and could be confusing as well. In order to focus on
specific resources (or namespaces) we can ask `kustomize-dot` to filter out the
things we do (or don't) need.

This command here for example will keep only resources from the `default` and
`kube-system` namespaces.

``` shell
kustomize-dot generate -f kube-prometheus.yaml \
    --keep-namespace default \
    --keep-namespace kube-system
```

The `kube-prometheus.yaml` file can be found in
[pkg/fixtures/kube-prometheus.yaml](https://github.com/dnaeon/kustomize-dot/blob/v1/pkg/fixtures/kube-prometheus.yaml).

This is what the result looks like.

![_config.yml]({{ site.baseurl }}/images/kustomize-dot-kube-prometheus-1.svg)

We can also apply some highlighting, if needed.

``` shell
kustomize-dot generate -f kube-prometheus.yaml \
    --keep-namespace default \
    --keep-namespace kube-system \
    --highlight-namespace default=pink \
    --highlight-namespace kube-system=yellow
```

![_config.yml]({{ site.baseurl }}/images/kustomize-dot-kube-prometheus-2.svg)

And here's another example, which drops all `ConfigMap` resources, and keeps
everything else.

``` shell
kustomize-dot generate -f kube-prometheus.yaml \
    --keep-namespace monitoring \
    --drop-kind ConfigMap \
    --highlight-kind service=yellow \
    --highlight-kind servicemonitor=orange \
    --highlight-kind serviceaccount=lightgray \
    --highlight-kind deployment=magenta \
    --highlight-kind prometheusrule=lightgreen \
    --highlight-kind networkpolicy=cyan
```

The result looks like this.

![_config.yml]({{ site.baseurl }}/images/kustomize-dot-kube-prometheus-3.svg)

For additional information and examples, please refer to the
[dnaeon/kustomize-dot](https://github.com/dnaeon/kustomize-dot) repo.
