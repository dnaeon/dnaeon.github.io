---
layout: post
title: Using JWT Authentication Method with the Go SDK for Vault
tags: k8s kubernetes vault oidc jwt openbao
---
In the [Setting up a Kubernetes cluster with Vault and OIDC
trust](https://dnaeon.github.io/kubernetes-vault-oidc/) post we've seen how to
configure identity federation between a Kubernetes cluster and Vault.

Once you have the [JWT Authentication
Method](https://developer.hashicorp.com/vault/docs/auth/jwt) configured properly
in Vault, we can get access to Vault by first authenticating against the auth
method's `/login` endpoint. This is how you would do it when using the Vault
CLI.

``` shell
vault write auth/jwt/login role=my-role-name jwt=@/path/to/my/token
```

On successful authentication Vault will give us back a token, which we can use
in subsequent calls to Vault, e.g. for retrieving secrets, adding new secrets,
etc.

And what about the SDK? How do authenticate against our JWT/OIDC Auth Method
when using the Go SDK?

The Vault SDK for Go can be found in the
[github.com/hashicorp/vault/api](https://github.com/hashicorp/vault/tree/main/api)
package. There's also a [quick start example for
Go](https://github.com/hashicorp/vault-examples/blob/main/examples/_quick-start/go/example.go)
in the
[github.com/hashicorp/vault-examples](https://github.com/hashicorp/vault-examples/tree/main)
repo. And in the same repo there's also an [example of using JWT Auth Method
from a Kubernetes
cluster](https://github.com/hashicorp/vault-examples/blob/main/examples/auth-methods/kubernetes/go/example.go).

But what about a more generic JWT/OIDC Auth Method support in the Go SDK? You
know, one that doesn't actually rely on [Kubernetes Service Account
Tokens](https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account/#service-account-issuer-discovery).

Well, it turns out that a more generic [JWT Auth Method does not exist in the
SDK yet](https://github.com/hashicorp/vault/issues/28912).

Technically speaking the
[auth.KubernetesAuth](https://github.com/hashicorp/vault/blob/0ee8d99d9c7d5ce0e1a4638e26c05a716032b3c5/api/auth/kubernetes/kubernetes.go#L14-L18)
implementation of the
[vault.AuthMethod](https://github.com/hashicorp/vault/blob/0ee8d99d9c7d5ce0e1a4638e26c05a716032b3c5/api/auth.go#L16-L18)
interface does that already, and you could use it outside of Kubernetes by
tweaking the various options such as `WithServiceAccountToken`,
`WithServiceAccountTokenPath`, etc., but at the same time this implementation
contains too much Kubernetes details such as service accounts, default token
path in a Kubernetes pod, etc.

Besides, the name of the Auth Method implementation might confuse people when
reading your code making them think that Kubernetes is a requirement, while
actually it is not.

So, what can we do? Well, we can implement our own
[vault.AuthMethod](https://github.com/hashicorp/vault/blob/0ee8d99d9c7d5ce0e1a4638e26c05a716032b3c5/api/auth.go#L16-L18)
based on the existing `auth.KubernetesAuth` implementation. And this is what we
are going to do in this post.

Will start off by creating a new Go module for our JWT Auth Method
implementation.

``` shell
mkdir vault-jwt-auth-method
cd vault-jwt-auth-method
go mod init vault-jwt-auth-method
go get -v github.com/hashicorp/vault/api
```

Our JWT Auth Method implementation will reside in the `auth/jwt` package,
similar to the way how the existing Auth Method implementations reside in the
upstream Vault SDK repo.

``` shell
mkdir -p pkg/auth/jwt
```

Open up `pkg/auth/jwt/jwt.go` in your favorite `$EDITOR`. And this is what the
JWT Auth Method implementation looks like.

``` go
package jwt

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"

	vault "github.com/hashicorp/vault/api"
)

// DefaultMountPath specifies the default mount path for the JWT
// Authentication Method.
const DefaultMountPath = "jwt"

// ErrNoToken is an error, which is returned when [JWTAuth] is configured
// with an empty token.
var ErrNoToken = errors.New("no token specified")

// ErrInvalidMountPath is an error, which is returned when configuring [JWTAuth]
// to use an invalid mount path for a Vault Authentication Method.
var ErrInvalidMountPath = errors.New("invalid auth method mount path specified")

// JWTAuth implements support for the [JWT Authentication Method].
//
// [JWT Authentication Method]: https://developer.hashicorp.com/vault/docs/auth/jwt
type JWTAuth struct {
	// roleName specifies the name of the role to use.
	roleName string

	// mountPath specifies the mount path for the JWT Authentication Method.
	mountPath string

	// token specifies the JWT token which will be used for authenticating
	// against the Vault Authentication Method endpoint.
	token string
}

var _ vault.AuthMethod = &JWTAuth{}

// Option is a function which configures [JWTAuth].
type Option func(a *JWTAuth) error

// New creates a new [JWTAuth] and configures it with the given options.
//
// The default mount path for the JWT Authentication Method is
// [DefaultMountPath]. In order to configure a different mount path for the
// Authentication Method you can use the [WithMountPath] option.
//
// The JWT token which will be used for authentication against the Vault
// Authentication Method login endpoint may be specified either as a string,
// from path, or via an environment variable. In order to configure the token
// for authentication use the [WithToken], [WithTokenFromPath] or
// [WithTokenFromEnv] options.
func New(roleName string, opts ...Option) (*JWTAuth, error) {
	jwtAuth := &JWTAuth{
		roleName:  roleName,
		mountPath: DefaultMountPath,
	}

	for _, opt := range opts {
		if err := opt(jwtAuth); err != nil {
			return nil, err
		}
	}

	if jwtAuth.token == "" {
		return nil, ErrNoToken
	}

	if jwtAuth.mountPath == "" {
		return nil, ErrInvalidMountPath
	}

	return jwtAuth, nil
}

// Login implements the [vault.AuthMethod] interface.
func (a *JWTAuth) Login(ctx context.Context, client *vault.Client) (*vault.Secret, error) {
	path := fmt.Sprintf("auth/%s/login", a.mountPath)
	data := map[string]any{
		"jwt":  a.token,
		"role": a.roleName,
	}

	return client.Logical().WriteWithContext(ctx, path, data)
}

// WithToken is an [Option], which configures [JWTAuth] to use the given token
// when authenticating against the Vault JWT Authentication Method.
func WithToken(token string) Option {
	opt := func(a *JWTAuth) error {
		a.token = token

		return nil
	}

	return opt
}

// WithTokenFromPath is an [Option], which configures [JWTAuth] to read the
// token from the given path.
func WithTokenFromPath(path string) Option {
	opt := func(a *JWTAuth) error {
		token, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		a.token = string(token)

		return nil
	}

	return opt
}

// WithTokenFromEnv is an [Option], which configures [JWTAuth] to read the token
// from the given environment variable.
func WithTokenFromEnv(env string) Option {
	opt := func(a *JWTAuth) error {
		value := os.Getenv(env)
		a.token = value

		return nil
	}

	return opt
}

// WithMountPath is an [Option], which configures [JWTAuth] to use the given
// mount path for the Vault Authentication Method.
func WithMountPath(mountPath string) Option {
	opt := func(a *JWTAuth) error {
		// Remove any trailing slashes from the given mount path
		a.mountPath = strings.TrimRight(mountPath, "/")

		return nil
	}

	return opt
}
```

And here is a simple example of how we can use our JWT Auth Method in Go.

``` go
package main

import (
	"context"
	"errors"
	"fmt"
	"os"

	vault "github.com/hashicorp/vault/api"

	jwtauth "vault-jwt-auth-method/pkg/auth/jwt"
)

func printErr(err error) {
	fmt.Println(err)
	os.Exit(1)
}

func main() {
	ctx := context.Background()

	// Create a JWT Authentication Method using the token from the following
	// role and token stored in the path
	authMethod, err := jwtauth.New(
		"my-role-name",
		jwtauth.WithTokenFromPath("/path/to/my/token"),
	)
	if err != nil {
		printErr(err)
	}

	// Create Vault client and authenticate using our JWT Authentication
	// Method implementation
	config := vault.DefaultConfig()
	client, err := vault.NewClient(config)
	if err != nil {
		printErr(err)
	}

	// Login using our Auth Method
	authSecret, err := client.Auth().Login(ctx, authMethod)
	if err != nil {
		printErr(err)
	}
	if authSecret == nil {
		printErr(errors.New("no auth info returned after login"))
	}

	// Read a sample KV v2 secret
	kvv2 := client.KVv2("kvv2")
	secret, err := kvv2.Get(ctx, "my/test/secret")
	if err != nil {
		printErr(err)
	}

	fmt.Println(secret.Data)
}
```

Additional improvement to the example code above would be to renew the auth
secret using a
[vault.LifetimeWatcher](https://pkg.go.dev/github.com/hashicorp/vault/api#LifetimeWatcher),
so that our API client is kept authenticated, especially if we are working on a
service, which is supposed to be long-running.

Btw, in case you have missed this one, just want to remind the readers that
Hashicorp Vault got forked a couple of years ago as a response to Hashicorp
relicensing their OSS projects.

The new OSS friendly fork is now known as [OpenBao](https://openbao.org/). The
code from this post has been adapted and [submitted to OpenBao as a
feature](https://github.com/openbao/openbao/pull/1526).
