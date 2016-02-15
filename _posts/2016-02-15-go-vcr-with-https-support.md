---
layout: post
title: Go VCR with HTTPS support
tags: golang programming testing http https vcr go-vcr
---
[go-vcr](https://github.com/dnaeon/go-vcr) is a Go package which
allows you to record and replay your HTTP interactions, in order to
provide fast, deterministic and accurate testing of your code.

Thanks to [@davars](https://github.com/davars) go-vcr has recently
received HTTPS support as well, which makes it possible to use
go-vcr against an HTTPS endpoint and record your interactions.

Here is a simple test of using `go-vcr` for recording and replaying
HTTPS interactions:

```go
package vcr_test

import (
	"io/ioutil"
	"net/http"
	"strings"
	"testing"

	"github.com/dnaeon/go-vcr/recorder"
)

func TestHTTPS(t *testing.T) {
	// Start our recorder
	r, err := recorder.New("fixtures/iana-reserved-domains")
	if err != nil {
		t.Fatal(err)
	}
	defer r.Stop() // Make sure recorder is stopped once done with it

	// Create an HTTP client and inject our transport
	client := &http.Client{
		Transport: r.Transport, // Inject our transport!
	}

	url := "https://www.iana.org/domains/reserved"
	resp, err := client.Get(url)
	if err != nil {
		t.Fatalf("Failed to get url %s: %s", url, err)
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("Failed to read response body: %s", err)
	}

	wantHeading := "<h1>IANA-managed Reserved Domains</h1>"
	bodyContent := string(body)

	if !strings.Contains(bodyContent, wantHeading) {
		t.Errorf("Heading %s not found in response", wantHeading)
	}
}
```

You can find this and also other examples in the
[go-vcr](https://github.com/dnaeon/go-vcr) repository at Github.
