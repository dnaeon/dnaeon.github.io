---
layout: post
title: Apache Proxy Server on FreeBSD
tags: freebsd
---
In this article we will see how you can configure Apache to as a
reverse proxy server.

You might want to use Apache proxy server for many different
reasons. One of it is to is to provide web access to internal private
web server from the outside.

Suppose you have a private network that is serving your intranet
pages.You may even have more than one web server in your private
network, that is not visible from the Internet. You may not want to
directly expose these systems to the big bad Internet, and this is
where an Apache proxy server comes in hand.

Instead of exposing your web servers directly to the Internet you
would have one publicly accessible IP address, where your proxy server
is accepting connections on.

The proxy server itself will be redirecting all those web requests
from the external clients to the internal web servers. You may have as
many as you want Apache vhosts configured on the proxy server
forwarding all those external requests to the internal servers.

## Requirements
* root access or sudo rights
* You have already installed Apache. If you still do not have Apache
installed, please [have a look at this handbook](/node/30), which
explains how you can install Apache web server.

## Installation

In order to get Apache proxy server running, please [refer to the
following document](/node/30), which explains how you can get Apache
installed and configured for you.

Following the above documentation you can safely skip all those steps
where you need to install and configure PHP support for Apache.

Please also note, that before you start building Apache, make sure you
select the following modules, so we can have proxy capabilities within
Apache.

```text
[X] PROXY                   Enable mod_proxy
[X] PROXY_CONNECT           Enable mod_proxy_connect
[X] PATCH_PROXY_CONNECT     Patch proxy_connect SSL support
[X] PROXY_FTP               Enable mod_proxy_ftp
[X] PROXY_HTTP              Enable mod_proxy_http
```

Note, that **PROXY_FTP** is needed only if intend on proxying FTP
connection, otherwise you may uncheck this module.

Now go ahead and [install Apache according to this
handbook](/node/30), where you skip all those steps for enabling PHP
support in your Apache server. Go through the configuration as well,
and once you are ready, come back here so we can configure Apache as a
proxy server.

After you've installed and configured Apache we need to install one
more module from the [FreeBSD Ports
Collection](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports-using.html)
- *www/mod_proxy_html*. This module will let us replace all those
hyperlinks received by the internal web server to something that the
external clients can use.

Suppose that your internal web server is named
**intranet.my-private-network.org**, and your your external publically
accessible domain,
where your proxy server will be running on is named **example.org**.

Now when the proxy server at example.org receives HTTP/HTTPS requests
it will forward them to the internal web server at
**intranet.my-private-network.org**,
but if somewhere in the content we have links to other documents
located on **intranet.my-private-network.org**,
external clients will not be able to follow these links, since
intranet.my-private-network.org is not visible from the outside.

So what we need to do is to make in some way all those for example
**http://intranet.my-private-network.org/index.php** links to
something like **http://example.org/index.php**. And this is where we
are going to use the *www/mod_proxy_html* Apache module. So now, go
ahead and install it:

```bash
$ cd /usr/ports/www/mod_proxy_html && sudo make install
```

Now that we have everything installed, we can continue with the
configuration in the next step.

## Configuration of 

Assuming you have followed the [handbook for installing and
configuring Apache as web server](/node/30), we will do some changes
to our configuration files, so we make Apache work as a proxy server.

Make sure you have these lines in your *Dynamic Shared Object (DSO)
Support* section of */usr/local/etc/apache22/httpd.conf* file

```text
LoadModule proxy_module              libexec/apache22/mod_proxy.so
LoadModule proxy_connect_module      libexec/apache22/mod_proxy_connect.so
LoadModule proxy_ftp_module          libexec/apache22/mod_proxy_ftp.so
LoadModule proxy_http_module         libexec/apache22/mod_proxy_http.so
LoadModule proxy_html_module         libexec/apache22/mod_proxy_html.so
```

Please, note that you need the *proxy_ftp_module* entry only if you
have chosen to have *proxy_ftp* installed in Apache.

We also need to configure the *mod_proxy_html* Apache module, so let's
do this. Either create a new file in
*/usr/local/etc/apache22/Includes* directory called
*mod_proxy_html.conf* or add these lines directly at the end of your
*/usr/local/etc/apache22/httpd.conf* file:

```apacheconf
# Configure the Apache mod_proxy_html module
ProxyHTMLLinks  a               href
ProxyHTMLLinks  area            href
ProxyHTMLLinks  link            href
ProxyHTMLLinks  img             src longdesc usemap
ProxyHTMLLinks  object          classid codebase data usemap
ProxyHTMLLinks  q               cite
ProxyHTMLLinks  blockquote      cite
ProxyHTMLLinks  ins             cite
ProxyHTMLLinks  del             cite
ProxyHTMLLinks  form            action
ProxyHTMLLinks  input           src usemap
ProxyHTMLLinks  head            profile
ProxyHTMLLinks  base            href
ProxyHTMLLinks  script          src for

ProxyHTMLEvents onclick ondblclick onmousedown onmouseup \
                onmouseover onmousemove onmouseout onkeypress \
                onkeydown onkeyup onfocus onblur onload \
                onunload onsubmit onreset onselect onchange
```

Now, let's configure a default HTTP vhost that all proxy requests will
be received by. Open *httpd-vhosts.conf* file and edit it.

This default vhost will pass all requests from the externally
accessible domain *example.org* to the internal web server at
*intranet.my-private-network.org*.

Here's mine *httpd-vhosts.conf* file, feel free to suit it for your
needs:

```apacheconf
#
# Virtual Hosts
#
# If you want to maintain multiple domains/hostnames on your
# machine you can setup VirtualHost containers for them. Most configurations
# use only name-based virtual hosts so the server doesn't need to worry about
# IP addresses. This is indicated by the asterisks in the directives below.
#
# Please see the documentation at
# <URL:http://httpd.apache.org/docs/2.2/vhosts/>
# for further details before you try to setup virtual hosts.
#
# You may use the command line option '-S' to verify your virtual host
# configuration.

#
# Use name-based virtual hosting.
#

NameVirtualHost *:80

#
# VirtualHost example:
# Almost any Apache directive may go into a VirtualHost container.
# The first VirtualHost section is used for all requests that do not
# match a ServerName or ServerAlias in any <VirtualHost> block.
#

<VirtualHost *:80>
        ServerAdmin admin@example.org
        ServerName example.org
        ServerAlias www.example.org

        RewriteEngine On

        RewriteCond %{HTTP_HOST}   !^www\.example\.org [NC]
        RewriteCond %{HTTP_HOST}   !^$
        RewriteRule ^/(.*)         http://www.example.org/$1 [L,R]

        ProxyRequests Off
        ProxyHTMLEnable On

        ProxyPass / http://intranet.my-private-network.org/
        ProxyPassReverse / http://intranet.my-private-network.org/
        ProxyHTMLURLMap http://intranet.my-private-network.org/ http://www.example.org/
        
        ErrorLog "/var/log/apache/httpd-example.org-error.log"
        CustomLog "/var/log/apache/httpd-example.org-access.log" common
</VirtualHost>
```

Now, let's configure the default HTTPS vhost. All SSL requests will
have a default vhost. Feel free to suit the settings for your needs.

Most of the settings are already there in the default configuration,
so just add the needed lines to your configuration (comments were
removed to make the file smaller).

Here's mine **httpd-ssl.conf** file:

```apacheconf
Listen 443

AddType application/x-x509-ca-cert .crt
AddType application/x-pkcs7-crl    .crl

SSLPassPhraseDialog  builtin

SSLSessionCache        "shmcb:/var/run/ssl_scache(512000)"
SSLSessionCacheTimeout  300

SSLMutex  "file:/var/run/ssl_mutex"

##
## SSL Virtual Host Context
##

<VirtualHost *:443>

#   General setup for the virtual host
ServerAdmin admin@example.org
ServerName  example.org:443
ServerAlias www.example.org

RewriteEngine On

RewriteCond %{HTTP_HOST}   !^www\.example\.org [NC]
RewriteCond %{HTTP_HOST}   !^$
RewriteRule ^/(.*)         https://www.example.org/$1 [L,R]

SSLProxyEngine On

ProxyRequests Off
ProxyHTMLEnable On

ProxyPass / https://intranet.my-private-network.org/
ProxyPassReverse / https://intranet.my-private-network.org/
ProxyHTMLURLMap https://intranet.my-private-network.org/ https://www.example.org/

ErrorLog "/var/log/apache/httpd-example.org-ssl-error.log"
TransferLog "/var/log/apache/httpd-example.org-ssl-access.log"

SSLEngine on

SSLCipherSuite ALL:!ADH:!EXPORT56:RC4+RSA:+HIGH:+MEDIUM:+LOW:+SSLv2:+EXP:+eNULL

#   Server Certificate
SSLCertificateFile "/etc/ssl/apache/server.crt"

#   Server Private Key
SSLCertificateKeyFile "/etc/ssl/apache/server.key"

<FilesMatch "\.(cgi|shtml|phtml|php)$">
    SSLOptions +StdEnvVars
</FilesMatch>
<Directory "/usr/local/www/apache22/cgi-bin">
    SSLOptions +StdEnvVars
</Directory>

BrowserMatch ".*MSIE.*" \
         nokeepalive ssl-unclean-shutdown \
         downgrade-1.0 force-response-1.0

#   Per-Server Logging:
#   The home of a custom SSL log file. Use this when you want a
#   compact non-error SSL logfile on a virtual host basis.
CustomLog "/var/log/httpd-ssl_request.log" \
          "%t %h %{SSL_PROTOCOL}x %{SSL_CIPHER}x \"%r\" %b"

</VirtualHost>
```

## Starting Apache

Before starting Apache for the first time, it is a good thing to test
the configuration for errors.

To do so, just execute the command below

```bash
$ sudo apachectl configtest
```

If the output says **Syntax OK**, then we can start our Apache proxy
server, otherwise please double check the configuration for errors.

So, let's start Apache now:

```bash
$ sudo apachectl start
```

In order to start Apache during boot-time add the following line to
your */etc/rc.conf* file:

```bash
apache22_enable="YES"
```

In the next chapter of this handbook you will find examples on how to
add more vhosts to your Apache Proxy server.

## Adding more Apache vhosts

Now let's have a look at some examples, where we add more vhosts to
our Apache Proxy configuration.

Suppose that you have a mail server in your internal network named
**mail.my-private-network.org**, and you want to allow web access to
the mail server using some web interface from
**webmail.example.org**. In this example the mail server should be
accessed only via HTTPS.

We will also add one more vhost to our configuration called
**lists.example.org**, which we will use to access the mailing lists
which are also located on internal web server at
**lists.my-private-network.org**.

Below is a sample configuration covering these two examples. Feel free
to suit the configuration for your needs. So, open *httpd-vhosts.conf*
and update the file as follows:

```apacheconf
#
# Virtual Hosts
#
# If you want to maintain multiple domains/hostnames on your
# machine you can setup VirtualHost containers for them. Most configurations
# use only name-based virtual hosts so the server doesn't need to worry about
# IP addresses. This is indicated by the asterisks in the directives below.
#
# Please see the documentation at
# <URL:http://httpd.apache.org/docs/2.2/vhosts/>
# for further details before you try to setup virtual hosts.
#
# You may use the command line option '-S' to verify your virtual host
# configuration.

#
# Use name-based virtual hosting.
#

NameVirtualHost *:80
NameVirtualHost *:443

#
# VirtualHost example:
# Almost any Apache directive may go into a VirtualHost container.
# The first VirtualHost section is used for all requests that do not
# match a ServerName or ServerAlias in any <VirtualHost> block.
#

# Default vhost
<VirtualHost *:80>
        ServerAdmin admin@example.org
        ServerName example.org
        ServerAlias www.example.org

        RewriteEngine On

        RewriteCond %{HTTP_HOST}   !^www\.example\.org [NC]
        RewriteCond %{HTTP_HOST}   !^$
        RewriteRule ^/(.*)         http://www.example.org/$1 [L,R]

        ProxyRequests Off
        ProxyHTMLEnable On

        ProxyPass / http://intranet.my-private-network.org/
        ProxyPassReverse / http://intranet.my-private-network.org/
        ProxyHTMLURLMap http://intranet.my-private-network.org/ http://www.example.org/
        
        ErrorLog "/var/log/apache/httpd-example.org-error.log"
        CustomLog "/var/log/apache/httpd-example.org-access.log" common
</VirtualHost>

# lists.example.org vhost
<VirtualHost *:80>
        ServerAdmin admin@example.org
        ServerName lists.example.org
        ServerAlias lists.example.org

        ProxyRequests Off
        ProxyHTMLEnable On

        ProxyPass / http://lists.my-private-network.org/
        ProxyPassReverse / http://lists.my-private-network.org/
        ProxyHTMLURLMap http://lists.my-private-network.org/ http://lists.example.org/

        ErrorLog "/var/log/apache/httpd-lists.example.org-error.log"
        CustomLog "/var/log/apache/httpd-lists.example.org-access.log" common
</VirtualHost>

# webmail.example.org vhost
<VirtualHost *:80>
        ServerName webmail.example.org:80
        Redirect permanent / https://webmail.example.org/
</VirtualHost>

<VirtualHost *:443>
        ServerAdmin admin@example.org
        ServerName webmail.example.org:443
        ServerAlias webmail.example.org

        SSLEngine On
        SSLProxyEngine On

        ProxyRequests Off
        ProxyHTMLEnable On

        ProxyPass / https://mail.my-private-network.org/
        ProxyPassReverse / https://mail.my-private-network.org/
        ProxyHTMLURLMap https://mail.my-private-network.org/ https://webmail.example.org/

        SSLCertificateFile "/etc/ssl/apache/server.crt"
        SSLCertificateKeyFile "/etc/ssl/apache/server.key"

        ErrorLog "/var/log/apache/httpd-webmail.example.org-error.log"
        CustomLog "/var/log/apache/httpd-webmail.example.org-access.log" common
</VirtualHost>
```

Now tell Apache to re-read it's configuration, so the changes are
applied.

```bash
$ sudo apachectl graceful
```

And that was it!
