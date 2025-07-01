---
layout: post
title: Setting up a Kubernetes cluster with Vault and OIDC trust
tags: k8s kubernetes vault oidc minikube
---
In this post we will see how to create and configure a development Kubernetes
cluster with Vault running in it and setup [OpenID
Connect](https://en.wikipedia.org/wiki/OpenID) trust between Vault and
Kubernetes workloads.

## Create the Kubernetes cluster

First, create a new Kubernetes cluster using [minikube](https://minikube.sigs.k8s.io/docs/).

``` shell
env KUBECONFIG=$(pwd)/kubeconfig minikube start --profile vault-k8s-dev
```

If you are using [direnv](https://direnv.net/), you can also create a local
`.envrc` file to point to our cluster and kubeconfig when you enter the
directory where your development cluster `kubeconfig` is stored.

``` shell
export KUBECONFIG=$(pwd)/kubeconfig
minikube profile vault-k8s-dev
```

Enable and reload the config.

``` shell
direnv allow
direnv reload
```

Once the cluster is up and running, verify that it runs with enabled [Service
account issuer
discovery](https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/#service-account-issuer-discovery).


``` shell
kubectl get --raw '/.well-known/openid-configuration' | jq
```

The command above will print output similar to the one below. The
`/.well-known/openid-configuration` endpoint provides the
[OpenID Connect Provider Metadata](https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderMetadata). It includes information about the issuer, where to find the public keys for
verifying JWT tokens, the supported signing algorithms, etc.

``` json
{
  "issuer": "https://kubernetes.default.svc.cluster.local",
  "jwks_uri": "https://10.0.2.15:8443/openid/v1/jwks",
  "response_types_supported": [
    "id_token"
  ],
  "subject_types_supported": [
    "public"
  ],
  "id_token_signing_alg_values_supported": [
    "RS256"
  ]
}
```

Alternatively, simply check the options, which are used to start the
`kube-apiserver` static pod, e.g.

``` shell
kubectl --namespace kube-system describe pod kube-apiserver-vault-k8s-dev | grep service-account
```

Above command should print the following output.

``` shell
--service-account-issuer=https://kubernetes.default.svc.cluster.local
--service-account-key-file=/var/lib/minikube/certs/sa.pub
--service-account-signing-key-file=/var/lib/minikube/certs/sa.key
```

We will also enable anonymous access to the API endpoints related to OpenID
Connect, since these API endpoints don't serve any sensitive data and should be
public.

In order to do that we will bind the `system:service-account-issuer-discovery`
cluster role to the `system:anonymous` user. This is what the cluster role
represents.

``` shell
> kubectl describe clusterrole system:service-account-issuer-discovery
Name:         system:service-account-issuer-discovery
Labels:       kubernetes.io/bootstrapping=rbac-defaults
Annotations:  rbac.authorization.kubernetes.io/autoupdate: true
PolicyRule:
  Resources  Non-Resource URLs                     Resource Names  Verbs
  ---------  -----------------                     --------------  -----
             [/.well-known/openid-configuration/]  []              [get]
             [/.well-known/openid-configuration]   []              [get]
             [/openid/v1/jwks/]                    []              [get]
             [/openid/v1/jwks]                     []              [get]
```

Create the cluster role binding.

``` shell
kubectl create clusterrolebinding system:anonymous-service-account-issuer-discovery \
        --clusterrole system:service-account-issuer-discovery \
        --user=system:anonymous
```

Start a test pod in the cluster and verify that you can access the OpenID
Discovery endpoint, e.g.

``` shell
curl -X GET https://kubernetes.default.svc.cluster.local/.well-known/openid-configuration
```

## Deploy Vault in the Kubernetes Cluster

Next, we will create the Kubernetes resources, which describe our Vault
configuration, service and statefulset. There are many different ways to get
Vault installed in your Kubernetes, but in order to keep things simple and
straight forward we will use these simple resources.

This is the `ConfigMap` providing the Vault config.

``` yaml
apiVersion: v1
data:
  config.json: |
    {
      "storage": {
        "file": {
          "path": "/vault/file"
        }
      },
      "listener": [
        {
          "tcp": {
            "address": "0.0.0.0:8200",
            "tls_disable": true
          }
        }
      ],
      "default_lease_ttl": "168h",
      "max_lease_ttl": "720h",
      "ui": true
    }
kind: ConfigMap
metadata:
  creationTimestamp: null
  name: vault-config
```

The `Service` resource.

``` yaml
---
apiVersion: v1
kind: Service
metadata:
  name: vault
  labels:
    app: vault
spec:
  ports:
  - port: 8200
  selector:
    app: vault
  type: ClusterIP
```

And the `StatefulSet` resource.

``` yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: vault
spec:
  serviceName: "vault"
  replicas: 1
  selector:
    matchLabels:
      app: vault
  template:
    metadata:
      labels:
        app: vault
    spec:
      containers:
      - name: vault
        image: hashicorp/vault:1.20
        args: ['server']
        securityContext:
          capabilities:
            add: ["IPC_LOCK"]
        livenessProbe:
          failureThreshold: 3
          httpGet:
            path: /ui/sys/health
            port: 8200
        readinessProbe:
          failureThreshold: 3
          httpGet:
            path: /ui/sys/health
            port: 8200
        ports:
        - containerPort: 8200
        volumeMounts:
        - name: vault-data
          mountPath: /vault/file
        - name: vault-config
          mountPath: /vault/config

      volumes:
      - name: vault-config
        configMap:
          name: vault-config
  volumeClaimTemplates:
  - metadata:
      name: vault-data
    spec:
      accessModes: [ "ReadWriteOnce" ]
      resources:
        requests:
          storage: 5Gi
```

Apply these resources and confirm that Vault is up and running.

``` shell
> kubectl get pods,services
  NAME          READY   STATUS    RESTARTS   AGE
  pod/vault-0   1/1     Running   0          24m

  NAME                 TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)    AGE
  service/kubernetes   ClusterIP   10.96.0.1      <none>        443/TCP    64m
  service/vault        ClusterIP   10.108.56.80   <none>        8200/TCP   48m
```

## Initialize and unseal Vault

Once Vault is up and running we need to initialize it. In order to make things
simpler when interfacing with Vault we will port-forward it's port to our local
system.

``` shell
export VAULT_ADDR=http://localhost:8200/
kubectl port-forward service/vault 8200:8200
```

And now, we can initialize it.

``` shell
vault operator init
```

Above command will print the unseal keys, e.g.

``` shell
Unseal Key 1: 2K/...
Unseal Key 2: xEKFbr...
Unseal Key 3: VFiwfbI43...
Unseal Key 4: 8Je/INW6lQOI...
Unseal Key 5: /1AFwxmvgw7UnFa...

Initial Root Token: hvs...

Vault initialized with 5 key shares and a key threshold of 3. Please securely
distribute the key shares printed above. When the Vault is re-sealed,
restarted, or stopped, you must supply at least 3 of these keys to unseal it
before it can start servicing requests.

Vault does not store the generated root key. Without at least 3 keys to
reconstruct the root key, Vault will remain permanently sealed!

It is possible to generate new unseal keys, provided you have a quorum of
existing unseal keys shares. See "vault operator rekey" for more information.
```

Now, we can unseal the Vault. Repeat the command below at least 3 times in order
to unseal it.

``` shell
vault operator unseal
```

We will also mount the KV v2 secrets engine, which we'll later use.

``` shell
vault secrets enable --version=2 --path kvv2 kv
```

List the enabled secret engines and we should see our `kvv2` mount.

``` shell
> vault secrets list
Path          Type         Accessor              Description
----          ----         --------              -----------
cubbyhole/    cubbyhole    cubbyhole_f86afe98    per-token private secret storage
identity/     identity     identity_fa281c9c     identity store
kvv2/         kv           kv_7e911817           n/a
sys/          system       system_f6068170       system endpoints used for control, policy and debugging
```

## Enable JWT/OIDC authentication method in Vault

Now, we can continue with enabling the `jwt` authentication method.

``` shell
vault auth enable jwt
```

List the enabled authentication methods.

``` shell
> vault auth list
Path      Type     Accessor               Description                Version
----      ----     --------               -----------                -------
jwt/      jwt      auth_jwt_cbaae747      n/a                        n/a
token/    token    auth_token_468286d4    token based credentials    n/a
```

Configure the authentication method for `jwt` by using the
`/auth/<auth-method>/config` endpoint.

You can check the additional parameters, which can be set in the [JWT/OIDC auth
method (API)](https://developer.hashicorp.com/vault/api-docs/auth/jwt)
documentation.

``` shell
vault write auth/jwt/config \
      oidc_discovery_ca_pem="<Kubernetes-API-Server-CA>" \
      oidc_client_id="" \
      oidc_client_secret="" \
      default_role="k8s-dev"
```

> The Kubernetes API Server CA bundle can also be obtained from the
> running `vault-0` pod and the location where the service account token is
> mounted, which is `/var/run/secrets/kubernetes.io/serviceaccount/ca.crt`. Since
> we are running our development Kubernetes cluster in `minikube` the CA bundle
> can also be found in `~/.minikube/ca.crt` on your local system.

If your Kubernetes cluster uses a certificate signed an official Certificate
Authority such as DigiCert, Let's Encrypt, etc. then you don't need to specify
the `oidc_discovery_ca_pem` parameter, but since this is a development cluster
it comes with self-signed certificate and it's own CA, which is why we need to
specify it here.

The previous command can also be executed against the running pod container like
this.

``` shell
kubectl exec vault-0 -- vault write auth/jwt/config \
        oidc_discovery_ca_pem=@/var/run/secrets/kubernetes.io/serviceaccount/ca.crt \
        oidc_client_id="" \
        oidc_client_secret="" \
        default_role="k8s-dev"
```

Verify that the authentication method has been correctly configured.

``` shell
> vault read auth/jwt/config
Key                                     Value
---                                     -----
bound_issuer                            n/a
default_role                            k8s-dev
jwks_ca_pem                             n/a
jwks_pairs                              []
jwks_url                                n/a
jwt_supported_algs                      []
jwt_validation_pubkeys                  []
namespace_in_state                      true
oidc_client_id                          n/a
oidc_discovery_ca_pem                   -----BEGIN CERTIFICATE-----...
oidc_discovery_url                      https://kubernetes.default.svc.cluster.local
oidc_response_mode                      n/a
oidc_response_types                     []
provider_config                         map[]
unsupported_critical_cert_extensions    []
```

Next thing we need to do is to create the `k8s-dev` role, which we've specified
in the previous step. This role will be associated with the `k8s-dev-writer`
policy, allowing Kubernetes workloads read/write access to Vault for a given
path.

``` shell
vault write auth/jwt/role/k8s-dev \
      bound_subject="system:serviceaccount:default:vault-writer" \
      bound_audiences="vault-dev" \
      user_claim="sub" \
      policies=k8s-dev-writer \
      role_type=jwt \
      ttl=1h
```

The `bound_subject` parameter from the command above specifies a Kubernetes
service account named `vault-writer` from the `default` namespace. Kubernetes
workloads, which will access Vault should be running with this service account,
and also the service account token should be issued with the `vault-dev`
audience.

Finally, we need to create the `k8s-dev-writer` policy, which will give our
Kubernetes workloads read/write access to Vault. This is what our simple policy
looks like.

``` hcl
path "kvv2/metadata/k8s-dev/*" {
    capabilities = ["list"]
}

path "kvv2/data/k8s-dev/*" {
    capabilities = ["create", "update", "patch", "read", "delete"]
}
```

We can now apply this policy.

``` shell
vault policy write k8s-dev-writer /path/to/k8s-dev-writer-policy.hcl
```

Time to test things out. Let's create the `vault-writer` Kubernetes service
account.

``` shell
kubectl create sa vault-writer
```

We can now create a test token for our `vault-writer` service account and test
accessing Vault using this token.

``` shell
kubectl create token vault-writer --audience=vault-dev
```

Save the token somewhere, because we will need it a bit later. This is what the
payload of our JWT token looks like when decoded.

``` json
{
  "aud": [
    "vault-dev"
  ],
  "exp": 1751300275,
  "iat": 1751296675,
  "iss": "https://kubernetes.default.svc.cluster.local",
  "jti": "066083b5-1219-4c91-8338-728365776f22",
  "kubernetes.io": {
    "namespace": "default",
    "serviceaccount": {
      "name": "vault-writer",
      "uid": "bc5cffb5-65e6-4921-835c-a0a8cdaa69db"
    }
  },
  "nbf": 1751296675,
  "sub": "system:serviceaccount:default:vault-writer"
}
```

In order to access Vault using our Kubernetes service account token, first we
need to get a Vault token. This step is similar to what [OAuth 2.0 Token
Exchange](https://datatracker.ietf.org/doc/html/rfc8693) does. We use our
Kubernetes service account token in order to get a Vault token. And this is how
we do it.

``` json
vault write auth/jwt/login role=k8s-dev jwt="<k8s-sa-token>"
```

On successful authentication we should see a similar output.

``` json
Key                  Value
---                  -----
token                hvs...
token_accessor       ...
token_duration       1h
token_renewable      true
token_policies       ["default" "k8s-dev-writer"]
identity_policies    []
policies             ["default" "k8s-dev-writer"]
token_meta_role      k8s-dev
```

And now we can use the `token` from the output above in order to access Vault
and write a sample secret.

``` json
export VAULT_TOKEN="<vault-token-goes-here>"
vault kv put kvv2/k8s-dev/foo bar=baz
```


Read back the secret we've just created.

``` json
> vault kv get kvv2/k8s-dev/foo
==== Secret Path ====
kvv2/data/k8s-dev/foo

======= Metadata =======
Key                Value
---                -----
created_time       2025-06-30T15:42:46.718550078Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1

=== Data ===
Key    Value
---    -----
bar    baz
```

Since our policy allows read/write access to the `kvv2/data/k8s-dev/*` path only
we should not be able to read or write anything else, so let's try that out.

``` json
> vault kv put kvv2/some-secret bar=baz
Error writing data to kvv2/data/some-secret: Error making API request.

URL: PUT http://localhost:8200/v1/kvv2/data/some-secret
Code: 403. Errors:

* 1 error occurred:
        * permission denied
```

Looks good. Finally, we can create a test pod which uses our `vault-writer`
service account and access Vault from it. The following example Kubernetes
manifest can be used to start a test pod using the `vault-writer` service
account.

This example uses [Service Account Token
projection](https://kubernetes.io/docs/concepts/storage/projected-volumes/#serviceaccounttoken)
in order to inject a token into the running pod, using an `audience`, which
Vault expects our clients to have as part of their JWT claims.

``` yaml
apiVersion: v1
kind: Pod
metadata:
  creationTimestamp: null
  labels:
    run: vault-client
  name: vault-client
spec:
  containers:
  - command:
    - sleep
    - infinity
    env:
      - name: VAULT_ADDR
        value: http://vault:8200/
    image: hashicorp/vault:1.20
    name: vault-client
    volumeMounts:
      - name: token-vol
        mountPath: /service-account
        readOnly: true
  volumes:
    - name: token-vol
      projected:
        sources:
          - serviceAccountToken:
              audience: vault-dev
              expirationSeconds: 3600
              path: token
  dnsPolicy: ClusterFirst
  restartPolicy: Always
  serviceAccountName: vault-writer
```

Create the test pod and log into it.

``` shell
kubectl apply -f /path/to/vault-client.yaml
kubectl exec -it vault-client -- /bin/sh
```

Once you get a shell in the running pod, simply exchange the Kubernetes service
account token for Vault token, similar to the way it was done in the previous
examples.

``` shell
vault write auth/jwt/login role=k8s-dev jwt=@/service-account/token
```

# References

Make sure to check the following documents for additional information on the
topic.

- [Vault: Developer quick start guide](https://developer.hashicorp.com/vault/docs/get-started/developer-qs)
- [Vault: Use JWT/OIDC authentication](https://developer.hashicorp.com/vault/docs/auth/jwt)
- [Vault: JWT/OIDC auth method (API)](https://developer.hashicorp.com/vault/api-docs/auth/jwt)
- [Vault: OIDC provider list](https://developer.hashicorp.com/vault/docs/auth/jwt/oidc-providers)
- [Vault: Secure workflows with OIDC authentication](https://developer.hashicorp.com/vault/tutorials/auth-methods/oidc-auth)
- [Vault: Introduction to policies](https://developer.hashicorp.com/vault/tutorials/get-started/introduction-policies)
- [Vault: Use Kubernetes for OIDC authentication](https://developer.hashicorp.com/vault/docs/auth/jwt/oidc-providers/kubernetes)
