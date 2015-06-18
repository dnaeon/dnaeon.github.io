---
layout: post
title: Disable SSL verification in Python
tags: python programming
---
With Python 2.7.9 release the default HTTPS behaviour has changed,
which is now to always verify the remote HTTPS certificate to which
you are initiating a connection.

In the
[Python 2.7.9 release notes ](https://www.python.org/downloads/release/python-279/)
you can read more about the changes that made it in this release of
Python, and [PEP 476](https://www.python.org/dev/peps/pep-0476/)
provides the technical details and rationale about this change.

In order to disable HTTPS certificate validation by default in
Python versions 2.7.9 or above you could use this snippet of code.

```python
import ssl

try:
    _create_unverified_https_context = ssl._create_unverified_context
except AttributeError:
    # Legacy Python that doesn't verify HTTPS certificates by default
    pass
else:
    # Handle target environment that doesn't support HTTPS verification
    ssl._create_default_https_context = _create_unverified_https_context
```
