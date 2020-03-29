---
layout: post
title: Install and configure Kubernetes on Arch Linux
tags: kubernetes k8s containers cni cri docker orchestration arch linux
---
In this post we will see how to install and configure a
[Kubernetes](https://kubernetes.io) cluster on [Arch
Linux](https://www.archlinux.org) nodes.

You can find plenty of documentation at the [official Kubernetes
site](https://kubernetes.io/docs/home/), but at the same time it can
feel a bit overwhelming, especially if you are a newcomer, so in this
post I'll try to summarize the steps needed to get your cluster and up
running on Arch Linux.

The lab setup we are going to create as part of this post will be a
highly available Kubernetes cluster. We will have three nodes for the
control-plane, which will also be un-tainted, so we can schedule pods
on them. Each node in the cluster will be running with 2 CPUs and 4 GB
of memory. If needed, we can expand the cluster afterwards by addig
more worker nodes to it.

For more details about Highly Available Kubernetes cluster, please
make sure to check this page
[here](https://kubernetes.io/docs/setup/production-environment/tools/kubeadm/high-availability/).

## Requirements

First, make sure that you have Arch Linux installed. If you haven't
done that before head first to the Arch Linux [installation
guide](https://wiki.archlinux.org/index.php/Installation_guide).

Each node in the Kubernetes cluster will have 2 CPUs and 4 GB of
memory and for bootstrapping the cluster we will use the `kubeadm`
tool. First, make sure your Arch Linux nodes have the basic set of
configuration applied, e.g. [configure
hostnames](https://wiki.archlinux.org/index.php/Network_configuration#Set_the_hostname),
set [static IP
addresseses](https://wiki.archlinux.org/index.php/Network_configuration#Static_IP_address),
configure DNS servers, NTP servers, etc.

We will need `devtools` and `base-devel` packages in order to build the
`kubelet`, `kubeadm` and `kubectl` command-line tools. If you are using
a tool to install packages from [AUR](https://wiki.archlinux.org/index.php/Arch_User_Repository)
you can use that instead to install the `kubelet`, `kubeadm` and `kubectl` tools.
In this post we will build these packages and use `pacman(8)` to install them,
in order to keep things simple and straight-forward.

``` shell
sudo pacman -S devtools base-devel
```

While bootstrapping the Kubernetes cluster one of the preflight checks that
Kubernetes performs is to see whether you have an active swap. If it finds an
active swap partition it will fail and stop there. In order to pass the
preflight checks you need to disable swap on each node in the Kubernetes cluster.

``` shell
sudo swapoff -a
```

Make sure to also update `/etc/fstab`, so that swap is not mounted during boot.

If you really have no choice and must use swap, though not recommended,
you can add the following flag to your `/etc/default/kubernetes` file.

``` text
--fail-swap-on=false
```

## Container runtime

We will install and configure [Docker](https://www.docker.com) as the
[Container Runtime for
Kubernetes](https://kubernetes.io/docs/setup/production-environment/container-runtimes/).
If you have special requirements for your Docker images and containers, e.g.
you want them to reside on an LVM volume now is the time to create that volume
and have it mounted under `/var/lib/docker`.

Install Docker:

``` shell
sudo pacman -S docker
```

If you want any users to be able to interface with Docker you can
add them to the `docker` group as well, e.g.

``` shell
sudo usermod -a -G docker <username>
```

Configure Docker to use `overlay2` storage driver and `systemd` as the
`cgroup driver`. Edit `/etc/docker/daemon.json` file.

``` json
{
  "exec-opts": ["native.cgroupdriver=systemd"],
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "100m"
  },
  "storage-driver": "overlay2"
}
```

Create a [systemd
drop-in](https://wiki.archlinux.org/index.php/systemd#Drop-in_files)
unit and set `TasksMax=infinity` option.

``` shell
sudo systemctl edit docker
```

Add the following content.

``` ini
[Service]
TasksMax=infinity
```

Enable and start the Docker service.

``` shell
sudo systemctl daemon-reload
sudo systemctl enable docker
sudo systemctl start docker
```

Verify that Docker has been installed and working correctly.

``` shell
sudo docker info
```

## Installing Kubernetes packages

Now we can install the `kubectl`, `kubeadm` and `kubelet` packages.
We will use the following script to fetch the packages from AUR
and install them.

``` shell
#!/usr/bin/env sh

OLD_PWD="${OLDPWD}"
PACKAGE_DIR="${HOME}/kube-packages/$( date +%Y-%m-%d )"

mkdir -p "${PACKAGE_DIR}"

for p in kubectl-bin kubelet-bin kubeadm-bin; do
    cd "${PACKAGE_DIR}"

    echo "> Fetching ${p} ..."
    curl -O https://aur.archlinux.org/cgit/aur.git/snapshot/${p}.tar.gz
    tar zxvf ${p}.tar.gz

    cd "${PACKAGE_DIR}/${p}"
    echo "> Building ${p} ..."
    makepkg

    echo "> Installing ${p} ..."
    sudo pacman --noconfirm -U *.tar.xz
done

cd "${OLD_PWD}"
echo "> Done"
```

Save above script somewhere (e.g. `~/install-kube-packages.sh`) and execute it.

``` shell
chmod +x install-kube-packages.sh
./install-kube-packages.sh
```

Verify that `kubectl`, `kubelet` and `kubeadm` have been installed
properly.  It is important to check that `kubectl`, `kubelet` and
`kubeadm` are all at the same version.

``` shell
$ kubelet --version
Kubernetes v1.17.4
```

This is the `kubeadm` version we have.

``` shell
$ kubeadm version
kubeadm version: &version.Info{Major:"1", Minor:"17", GitVersion:"v1.17.4", GitCommit:"8d8aa39598534325ad77120c120a22b3a990b5ea", GitTreeState:"clean", BuildDate:"2020-03-12T21:01:11Z", GoVersion:"go1.13.8", Compiler:"gc", Platform:"linux/amd64"}
```

And this is the `kubectl` version we have.

``` shell
$ kubectl version
Client Version: version.Info{Major:"1", Minor:"17", GitVersion:"v1.17.4", GitCommit:"8d8aa39598534325ad77120c120a22b3a990b5ea", GitTreeState:"clean", BuildDate:"2020-03-12T21:03:42Z", GoVersion:"go1.13.8", Compiler:"gc", Platform:"linux/amd64"}
The connection to the server localhost:8080 was refused - did you specify the right host or port?
```

It is okay for now to see connection refused as the last line of the
output from `kubectl version` shows, since we don't have the services
up and running yet. What is important to note here is that all tools
are at the same version as the output shows above.

Finally, we will install a few additional packages required by `kubeadm`,
which are used as part of the bootstrap process.

``` shell
sudo pacman -S ethtool ebtables socat
```

## Load Balancer setup

Since we are creating a highly available control-plane for the
Kubernetes cluster we would need to load-balance requests to the
control-plane nodes in the cluster. For that purpose we will use
an [HAProxy](http://www.haproxy.org) load-balancer.

You can read more about setting up [HAProxy on Arch
Linux](https://wiki.archlinux.org/index.php/HAproxy) and the official
site of HAProxy as well.

``` shell
sudo pacman -S haproxy
```

Open `/etc/haproxy/haproxy.cfg` in your editor of choice.

``` text
#---------------------------------------------------------------------
# Example configuration.  See the full configuration manual online.
#
#   http://www.haproxy.org/download/1.7/doc/configuration.txt
#
#---------------------------------------------------------------------

global
    maxconn     20000
    log         127.0.0.1 local0
    user        haproxy
    chroot      /usr/share/haproxy
    pidfile     /run/haproxy.pid
    daemon

frontend k8s
    bind            :6443
    mode            tcp
    option          tcplog
    log             global
    option          dontlognull
    timeout client  30s
    default_backend k8s-control-plane

backend k8s-control-plane
    mode            tcp
    balance         roundrobin
    timeout         connect 30s
    timeout         server  30s
    server          k8s-node01 192.168.88.230:6443 check
    server          k8s-node02 192.168.88.231:6443 check
    server          k8s-node03 192.168.88.232:6443 check
```

Make sure to specify the correct settings for your Kubernetes nodes in
the above config file. Finally enable and start the HAProxy service.

``` shell
sudo systemctl enable haproxy
sudo systemctl start haproxy
```

If you check the logs of the service you will see warnings that the
backend nodes of the Kubernetes control plane are not available,
and that is okay for now, since we have yet to bootstrap them.

## Bootstrapping the Kubernetes cluster

In order to bootstrap the cluster we will use the `kubeadm` tool.  We
will be using [Calico](https://www.projectcalico.org) as the Container
Network Interface (CNI), which by default uses the
`192.168.0.0/16` CIDR when setting up the cluster using `kubeadm`.

If you prefer another CNI instead, make sure to check [this
section](https://kubernetes.io/docs/setup/production-environment/tools/kubeadm/create-cluster-kubeadm/#pod-network)
of the Kubernetes documentation and specify the correct CIDR for your
pod network.

Make sure that `kubelet` service is enabled.

``` shell
sudo systemctl enable kubelet
```

Time to bootstrap the Kubernetes cluster.

``` shell
sudo kubeadm init --pod-network-cidr=192.168.0.0/16 --control-plane-endpoint <k8s>:6443 --upload-certs
```

Make sure to replace `<k8s>` in above command with the IP address or
DNS name of your load-balancer you have configured in the previous
step. Once the cluster has been initialized you will see a similar
output.

``` text
Your Kubernetes control-plane has initialized successfully!

To start using your cluster, you need to run the following as a regular user:

  mkdir -p $HOME/.kube
  sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config
  sudo chown $(id -u):$(id -g) $HOME/.kube/config

You should now deploy a pod network to the cluster.
Run "kubectl apply -f [podnetwork].yaml" with one of the options listed at:
  https://kubernetes.io/docs/concepts/cluster-administration/addons/

You can now join any number of the control-plane node running the following command on each as root:

  kubeadm join <k8s>:6443 --token ccgnsn.1kwq7gnvm1xbiwvd \
    --discovery-token-ca-cert-hash sha256:4da745ded3fb48f8894c247c3cdb123e4230c2f5d01bf37c1d1e3a4838c95cf5 \
    --control-plane --certificate-key 57ed53d8968473217fcb6254caba1b5ecc63794f9ae6887a2c8032c8a6a5cb03

Please note that the certificate-key gives access to cluster sensitive data, keep it secret!
As a safeguard, uploaded-certs will be deleted in two hours; If necessary, you can use
"kubeadm init phase upload-certs --upload-certs" to reload certs afterward.

Then you can join any number of worker nodes by running the following on each as root:

kubeadm join <k8s>:6443 --token ccgnsn.1kwq7gnvm1xbiwvd \
    --discovery-token-ca-cert-hash sha256:4da745ded3fb48f8894c247c3cdb123e4230c2f5d01bf37c1d1e3a4838c95cf5
```

Execute the commands as printed in the output above.

``` shell
mkdir -p $HOME/.kube
sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config
sudo chown $(id -u):$(id -g) $HOME/.kube/config
```

Now we can deploy a pod network to the cluster.

``` shell
kubectl apply -f https://docs.projectcalico.org/v3.11/manifests/calico.yaml
```

Once completed you should see output similar to the one below.

``` text
configmap/calico-config created
customresourcedefinition.apiextensions.k8s.io/felixconfigurations.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/ipamblocks.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/blockaffinities.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/ipamhandles.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/ipamconfigs.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/bgppeers.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/bgpconfigurations.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/ippools.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/hostendpoints.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/clusterinformations.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/globalnetworkpolicies.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/globalnetworksets.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/networkpolicies.crd.projectcalico.org created
customresourcedefinition.apiextensions.k8s.io/networksets.crd.projectcalico.org created
clusterrole.rbac.authorization.k8s.io/calico-kube-controllers created
clusterrolebinding.rbac.authorization.k8s.io/calico-kube-controllers created
clusterrole.rbac.authorization.k8s.io/calico-node created
clusterrolebinding.rbac.authorization.k8s.io/calico-node created
daemonset.apps/calico-node created
serviceaccount/calico-node created
deployment.apps/calico-kube-controllers created
serviceaccount/calico-kube-controllers created
```

Remove the taints on the node, so that we can schedule pods on it. For
any newly added control-plane node for which you want to be able to
schedule pods on it, just execute the command there as well. If you've
got plenty of worker nodes and want to leave the control-plane nodes
alone, then skip the following command.

``` shell
kubectl taint nodes --all node-role.kubernetes.io/master-
```

If you check the status of your Kubernetes cluster you should see a
single master node.

``` shell
$ kubectl get nodes -o wide
NAME         STATUS   ROLES    AGE   VERSION   INTERNAL-IP      EXTERNAL-IP   OS-IMAGE     KERNEL-VERSION   CONTAINER-RUNTIME
k8s-node01   Ready    master   16m   v1.17.4   192.168.88.230   <none>        Arch Linux   5.5.10-arch1-1   docker://19.3.8
```

And these are the pods we have so far in our cluster.

``` shell
$ kubectl get pods --all-namespaces=true
NAMESPACE     NAME                                       READY   STATUS    RESTARTS   AGE
kube-system   calico-kube-controllers-5b644bc49c-dph5v   1/1     Running   0          15m
kube-system   calico-node-h2qdx                          1/1     Running   0          15m
kube-system   coredns-6955765f44-2zvgw                   1/1     Running   0          17m
kube-system   coredns-6955765f44-5p6ck                   1/1     Running   0          17m
kube-system   etcd-k8s-node01                            1/1     Running   0          17m
kube-system   kube-apiserver-k8s-node01                  1/1     Running   0          17m
kube-system   kube-controller-manager-k8s-node01         1/1     Running   0          17m
kube-system   kube-proxy-kmm5k                           1/1     Running   0          17m
kube-system   kube-scheduler-k8s-node01                  1/1     Running   0          17m
```

In the next section of this document we will see how to add more nodes
to our Kubernetes cluster.

## Adding more nodes to the cluster

Once the Kubernetes cluster has been initialized you will be provided
with details about adding more control-plane and worker nodes.

So, using the provided output from `kubeadm init` command we can use this
command in order to add a new control-plane node.

``` shell
sudo kubeadm join <k8s>:6443 --token ccgnsn.1kwq7gnvm1xbiwvd \
        --discovery-token-ca-cert-hash sha256:4da745ded3fb48f8894c247c3cdb123e4230c2f5d01bf37c1d1e3a4838c95cf5 \
        --control-plane --certificate-key 57ed53d8968473217fcb6254caba1b5ecc63794f9ae6887a2c8032c8a6a5cb03
```

If you want to be able to schedule pods on the control-plane nodes you need
to un-taint them, e.g.

``` shell
kubectl taint nodes --all node-role.kubernetes.io/master-
```

In order to add worker nodes to the cluster we need to use this
command instead (again from the output of `kubeadm init` command in
previous step).

``` shell
sudo kubeadm join <k8s>:6443 --token ccgnsn.1kwq7gnvm1xbiwvd \
        --discovery-token-ca-cert-hash sha256:4da745ded3fb48f8894c247c3cdb123e4230c2f5d01bf37c1d1e3a4838c95cf5
```

## Accessing your Kubernetes cluster outside the control-plane nodes

In order to access your Kubernetes cluster from outside the
control-plane nodes, you can copy the admin config file to your local
system, e.g.

``` shell
scp k8s-node01.example.org:/etc/kubernetes/admin.conf ~/.kube/config-lab-admin.yml
```

Use the config to interface with your Kubernetes cluster, e.g.

``` shell
kubectl --kubeconfig ~/.kube/config-lab-admin.yml get nodes -o wide
```

## Viewing and managing tokens

In order to get the tokens you can use for joining new nodes in the
cluster, you can use the `kubeadm token list` command, e.g.

``` shell
$ kubeadm token list
TOKEN                     TTL         EXPIRES                     USAGES                   DESCRIPTION                                                EXTRA GROUPS
ccgnsn.1kwq7gnvm1xbiwvd   22h         2020-03-11T13:59:48+02:00   authentication,signing   The default bootstrap token generated by 'kubeadm init'.   system:bootstrappers:kubeadm:default-node-token
```

If you don’t have the value of `--discovery-token-ca-cert-hash` flag
when joining a node, you can retrieve it by executing the following
command on the control-plane node.

``` shell
openssl x509 -pubkey -in /etc/kubernetes/pki/ca.crt | openssl rsa -pubin -outform der 2>/dev/null | \
   openssl dgst -sha256 -hex | sed 's/^.* //'
```

When the token expires you can create a new one by using the following
command, which will also print the join command you can use.

``` shell
kubeadm token create --print-join-command
```

Make sure that you also check the [kubeadm
token](https://kubernetes.io/docs/reference/setup-tools/kubeadm/kubeadm-token/)
documentation for other examples and information on tokens.

## Install the Kubernetes Dashboard

You can optionally install the
[Dashboard](https://kubernetes.io/docs/tasks/access-application-cluster/web-ui-dashboard/),
which is a web UI for your Kubernetes cluster.

For more information about the Kubernetes Dashboard, please refer to these
documents.

* https://kubernetes.io/docs/tasks/access-application-cluster/web-ui-dashboard/
* https://github.com/kubernetes/dashboard/blob/master/docs/user/access-control/creating-sample-user.md

What follows below is a summary from above documents, which you can
follow to get the dashboard up and running.

Deploy the dashboard by applying the following manifest.

``` shell
kubectl apply -f https://raw.githubusercontent.com/kubernetes/dashboard/v2.0.0-beta8/aio/deploy/recommended.yaml
```

Create the manifest for the `admin-user` Service Account. This is what
the `dashboard-adminuser.yml` manifest looks like.

``` shell
---
apiVersion: v1
kind: ServiceAccount
metadata:
  name: admin-user
  namespace: kubernetes-dashboard
```

Create the manifest for Cluster Role Binding. This what the
`dashboard-cluster-rolebinding.yaml` manifest looks like.

``` shell
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: admin-user
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: cluster-admin
subjects:
  - kind: ServiceAccount
    name: admin-user
    namespace: kubernetes-dashboard
```

Apply the manifests.

``` shell
kubectl apply -f dashboard-adminuser.yml
kubectl apply -f dashboard-cluster-rolebinding.yml
```

Get the token we can use to authenticate to the dashboard service.
First, list the secrets in the `kubernetes-dashboard` namespace
using the following command.

``` shell
$ kubectl -n kubernetes-dashboard get secret
NAME                               TYPE                                  DATA   AGE
admin-user-token-zw6sr             kubernetes.io/service-account-token   3      10m
default-token-6g4tt                kubernetes.io/service-account-token   3      14m
kubernetes-dashboard-certs         Opaque                                0      14m
kubernetes-dashboard-csrf          Opaque                                1      14m
kubernetes-dashboard-key-holder    Opaque                                2      14m
kubernetes-dashboard-token-nbxl7   kubernetes.io/service-account-token   3      14m
```

Display the details about the token.

``` shell
$ kubectl -n kubernetes-dashboard describe secret admin-user-token-zw6sr
Name:         admin-user-token-zw6sr
Namespace:    kubernetes-dashboard
Labels:       <none>
Annotations:  kubernetes.io/service-account.name: admin-user
              kubernetes.io/service-account.uid: d8ec3893-7f64-47a2-978a-c06d0d876b7e

Type:  kubernetes.io/service-account-token

Data
====
token:      eyJhbGciOiJSUzI1NiIsImtpZCI6IjF0eTFoM3dkaVJTcUxNQXM1Z21DWllDY0EzeE54ajVFejA0LWdVVmVKYzQifQ.eyJpc3MiOiJrdWJlcm5ldGVzL3NlcnZpY2VhY2NvdW50Iiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9uYW1lc3BhY2UiOiJrdWJlcm5ldGVzLWRhc2hib2FyZCIsImt1YmVybmV0ZXMuaW8vc2VydmljZWFjY291bnQvc2VjcmV0Lm5hbWUiOiJhZG1pbi11c2VyLXRva2VuLXp3NnNyIiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9zZXJ2aWNlLWFjY291bnQubmFtZSI6ImFkbWluLXVzZXIiLCJrdWJlcm5ldGVzLmlvL3NlcnZpY2VhY2NvdW50L3NlcnZpY2UtYWNjb3VudC51aWQiOiJkOGVjMzg5My03ZjY0LTQ3YTItOTc4YS1jMDZkMGQ4NzZiN2UiLCJzdWIiOiJzeXN0ZW06c2VydmljZWFjY291bnQ6a3ViZXJuZXRlcy1kYXNoYm9hcmQ6YWRtaW4tdXNlciJ9.SaWjCqjkPqvB-bPWcGO0Q3DbG8YAYYvCoNjr7E48vcLoY5IS4cp_mQAu3Nf1g1rzbxfK66chgHnzB5IqhAIvK0JlDL-6hn1Yuy5IGAa4qL0-OHbkU4IXQQSfjsNJcSh7MnY9Z8j2-p2ejvm9us83u1y_ZUWkFhe28ej0eNFrLJ_dtaEP-4jua5SrGi3E3_MRGRIdGmAJqY5ZyLmlIfHB9qEefToq8EepZTWmcF7MitcC6PZLyd1AgblYuDCCFc7a3rqiZMlzOqtzMH1hPVDMJkuxZmSmG_T_QT5KqEDWhC3LBR_LktK9oR_akS2UHeAyklA0RoJK-GPBCHyQh4T0Ug
ca.crt:     1025 bytes
namespace:  20 bytes
```

You need the token that is displayed as part of the data section.
This is the [JSON Web Token](https://jwt.io), which we will use
in order to authenticate ourselves to the Dashboard service.

And finally we will use the following command, which would allow us to
access the dashboard service. We need to use the `kubectl proxy`
command, because the dashboard service is not exposed outside the
Kubernetes cluster (it is configured by default as a `ClusterIP`
service).

From your local system.

``` shell
$ kubectl proxy
Starting to serve on 127.0.0.1:8001
```

Open up a browser to the following URL and authenticate using the JWT token
you've retrieved previously.

- http://localhost:8001/api/v1/namespaces/kubernetes-dashboard/services/https:kubernetes-dashboard:/proxy/#/login

## Next steps

Once you've got your Kubernetes cluster up and running you can
continue with the rest of the [Kubernetes
documentation](https://kubernetes.io/docs/home/) and play around.

A good start would probably be
[this tutorial](https://kubernetes.io/docs/tutorials/hello-minikube/),
which walks you through the basics.
