---
layout: post
title: Packaging the VMware vSAN SDK for Python
tags: programming python vmware vsan sdk
---
The vSAN Management SDK for Python is distributed as a Zip archive
available at [code.vmware.com](code.vmware.com). The latest version as
of writing this document is 6.7.0 and can be found
[here](https://code.vmware.com/web/sdk/6.7.0/vsan-python).

Unfortunately the vSAN SDK for Python is currently not available at
[PyPI](https://pypi.org), so you cannot install it easily using `pip`
for example. In this document we will see how to package the VMware
vSAN Management SDK for Python and have it uploaded to a package index
in Artifactory.

## Pre-requisites

First, make sure that you download the latest vSAN SDK for Python from
the following link.

* https://code.vmware.com/web/sdk/6.7.0/vsan-python

Once you download the archive, simply unzip it.

## Packaging

The official vSAN SDK for Python archive contains the following files
and directories in version 6.7.0 of the SDK.

``` shell
$ ls -l vsan-sdk-python
total 8
-rw-rw-r--@    1 mnikolov  staff    2051 Feb 28 19:08 README
drwxr-xr-x@    3 mnikolov  staff      96 Feb 28 19:08 bindings
drwxr-xr-x@ 3519 mnikolov  staff  112608 Feb 28 19:08 docs
drwxr-xr-x@    5 mnikolov  staff     160 Feb 28 19:08 samplecode
```

In order to convert this to a proper Python package we will do:

* Move the SDK modules from `bindings` to the `vmware.vsan` package
* The `samplecode` directory contains sample scripts that you can
  run. We will include these as part the installed scripts by the
  package.
* The `docs` directory contains the vSAN API reference, which we will
  include in our source distribution tarball.

The `bindings` and `samplecode` directories contain the following
files. The `docs` directory contains the API reference documentation,
which contains many files, and that is why it was not included in the
output below.

``` shell
$ tree bindings samplecode
bindings
└── vsanmgmtObjects.py
samplecode
├── vsanapisamples.py
├── vsanapiutils.py
└── vsaniscsisamples.py

0 directories, 4 files
```

Our Python modules will reside in the `src` directory, so let's create
the directory structure now for our packages and modules. Also, make
sure to create `__init__.py` files in each package directory, so that
our package is properly recognized as a Python package.

``` shell
$ mkdir -p src/vmware/vsan
$ touch src/vmware/__init__.py
$ touch src/vmware/vsan/__init__.py
```

Rename the `README` file as `README.md` and optionally provide any
details about the repository location of your custom vSAN SDK for
Python package.

``` shell
$ mv README README.md
```

We will move the `vsanmgmtObjects.py` and `vsanapiutils.py` modules to
the `vmware.vsan` package. The `vsanapisamples.py` and
`vsaniscsisamples.py` scripts will be installed as scripts from our
Python package.

``` shell
$ mv bindings/vsanmgmtObjects.py src/vmware/vsan
$ mv samplecode/vsanapiutils.py src/vmware/vsan
$ mv samplecode/vsanapisamples.py src/vsanapisamples
$ mv samplecode/vsaniscsisamples.py src/vsaniscsisamples
```

The included sample scripts import the `vsanmgmtObjects` and
`vsanapiutils` modules, but since we have moved these to the
`vmware.vsan` package, make sure to update the following scripts so
that they now import `vmware.vsan.vsanmgmtObjects` and
`vmware.vsan.vsanapiutils` modules respectively.

* src/vsanapisamples
* src/vsaniscsisamples

Let's also add version of our Python package. Update the
`src/vmware/vsan/__init__.py` file and add the corresponding
`__version__` to it.

``` shell
$ cat src/vmware/vsan/__init__.py
__version__ = '6.7.0'
```

Now, we can create our `setup.py` file, which will drive the packaging
and installation process for our package. Since the vSAN SDK for
Python depends on `pyvmomi` we will add it to the list of dependencies
below. This is how our `setup.py` file looks like.

``` python
import re
import ast

from setuptools import setup, find_packages

_version_re = re.compile(r'__version__\s+=\s+(.*)')

with open('src/vmware/vsan/__init__.py', 'rb') as f:
    version = str(ast.literal_eval(_version_re.search(
        f.read().decode('utf-8')).group(1))
    )

setup(
    name='pyvsan',
    version=version,
    description='VMware vSAN SDK for Python',
    long_description=open('README.md').read(),
    author='VMware, Inc.',
    author_email='unknown@vmware.com',
    maintainer='John Doe',
    maintainer_email='john.doe@example.org',
    license='BSD',
    url='https://code.vmware.com/apis/398/vsan',
    download_url='https://code.vmware.com/apis/398/vsan',
    package_dir={'': 'src'},
    packages=find_packages('src'),
    project_urls={
        'Documentation': 'https://code.vmware.com/apis/398/vsan',
        'Download': 'https://code.vmware.com/web/sdk/6.7.0/vsan-python',
    },
    scripts=[
        'src/vsanapisamples',
        'src/vsaniscsisamples',
    ],
    install_requires=[
        'pyvmomi >= 6.7.0',
    ]
)
```

Since we also want our source distribution tarball to ship with the
official API reference documentation we will create a `MANIFEST.in`
template.

``` shell
$ cat MANIFEST.in
include README.*
include docs/*
```

Finally, remove the now empty `bindings` and `samplecode` directories.

``` shell
$ rmdir bindings
$ rmdir samplecode
```

## Installation

In order to install the package simply execute the following command.

``` shell
$ python setup.py install
```

Above command should take care of properly installing any dependencies
(e.g. `pyvmomi`) and afterwards install our vSAN SDK modules.  We can
confirm that everything has been installed correctly, by checking the
output below.

``` shell
$ pip freeze
certifi==2018.8.13
chardet==3.0.4
idna==2.7
pyvmomi==6.7.0
pyvsan==6.7.0
requests==2.19.1
six==1.11.0
urllib3==1.23
```

## API

The vSAN API Python modules will be installed as:

* `vmware.vsan.vsanmgmtObjects`
* `vmware.vsan.vsanapiutils`

## Sample Scripts

The `pyvsan` package will also install the following sample scripts.

* `vsanapisamples`
* `vsaniscsisamples`

After installing `pyvsan` you can run the included sample scripts.

``` shell
$ vsanapisamples
usage: vsanapisamples [-h] -s HOST [-o PORT] -u USER [-p PASSWORD]
                      [--cluster CLUSTER]
vsanapisamples: error: the following arguments are required: -s/--host, -u/--user
```

And the usage info for `vsaniscsisamples`:

``` shell
$ vsaniscsisamples
usage: vsaniscsisamples [-h] -s HOST [-o PORT] -u USER [-p PASSWORD]
                        [--cluster CLUSTER]
vsaniscsisamples: error: the following arguments are required: -s/--host, -u/--user
```

## Documentation

This `pyvsan` package ships with the official vSAN API Reference.

The following documentation is also worth checking out.

* [vSAN SDKs Programming Guide](https://vdc-download.vmware.com/vmwb-repository/dcr-public/c9b7807d-bc88-442d-93a8-9c75fe638c25/c364bea6-cbc0-454f-a522-b78ba2745f8f/vsan-sdk6.6.pdf)
* [vSAN Management API 6.7](https://code.vmware.com/apis/398/vsan)

## Uploading to a package index

In this section we will see how to upload the vSAN SDK package we've
created in the previous section to a package index, which in this case
would be the internal Artifactory repository.

For more information about PyPI repository support, please refer to the link below.

* https://www.jfrog.com/confluence/display/RTF/PyPI+Repositories

We will be uploading the package to the internal PyPI repository,
called `foo-pypi-local`. First, make sure that you define the
Artifactory repository in your `~/.pypirc`. This is how my `~/.pypirc`
file looks like.

``` ini
[distutils]
index-servers =
    pypi
    pypi-foo

[pypi]
repository: https://pypi.python.org/pypi
username: username
password: p4ssw0rd

[pypi-foo]
repository: http://build-artifactory.example.org/artifactory/api/pypi/foo-pypi-local/
username: foo-deployer
password: p4ssw0rd
```

Register your package. Make sure to specify the Artifactory repository
you've defined above.

``` shell
$ python setup.py register --repository pypi-foo
```

If above command is successful you can now upload your source
distribution by executing the following command.

``` shell
$ python setup.py sdist upload --repository pypi-foo
```

Once you upload the package to Artifactory you should be able to
install it by executing the following command.

``` shell
$ pip install \
        --extra-index https://build-artifactory.example.org/artifactory/api/pypi/foo-pypi-local/simple/ \
        pyvsan
```
