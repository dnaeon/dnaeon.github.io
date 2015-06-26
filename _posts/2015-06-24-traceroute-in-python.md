---
layout: post
title: A simple traceroute(8) implementation in Python
tags: python programming
---
`traceroute(8)` is one of these tools that sysadmins often use
when a networking issue arises and needs troubleshooting.

The `traceroute(8)` tool is used to trace the route packets take
in a network to a destination host. The tool is used to diagnose
possible problems in the network and also measure the latency
between systems.

The way that `traceroute(8)` works is by sending a sequence of UDP
[ICMP Echo Request](https://en.wikipedia.org/wiki/ICMP_Echo_Request)
packets to a destination host with a specified Time-To-Live (TTL)
value.

Each time a packet reaches a gateway, the gateway checks
if the TTL of a packet is greater than one, reduces the TTL of the
packet by one and transmits the packet to its next hop. If the TTL
of a packet is one, then the gateway discards the packet and sends
back a [Time exceeded](https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Time_exceeded)
response to the source client informing it that the destination host
could not be reached as the packet TTL has exceeded.

These `Time exceeded` responses are then being used by the source
client to determine the intermediate gateways sitting between the
source and destination system by manipulating the TTL of packets on
each iteration until we finally reach our destination host or the
max number of iterations (hops limit) has been reached.

You can read more about `traceroute(8)` at the
[Traceroute page on Wikipedia](https://en.wikipedia.org/wiki/Traceroute).

Below you can find a Python module that creates a very basic
implementation of `traceroute(8)`.

Please also note, that the receiver socket used in the class below is a
raw socket (`socket.SOCK_RAW`), so you will need root privileges when
binding it.

You can also find the code below in the
[pytraceroute](https://github.com/dnaeon/pytraceroute) repository on
Github.

```python
"""
A very simple Python traceroute(8) implementation

"""

import socket
import random

__all__ = ['Tracer']


class Tracer(object):
    def __init__(self, dst, hops=30):
        """
        Initializes a new tracer object

        Args:
            dst  (str): Destination host to probe
            hops (int): Max number of hops to probe

        """
        self.dst = dst
        self.hops = hops
        self.ttl = 1

        # Pick up a random port in the range 33434-33534
        self.port = random.choice(range(33434, 33535))

    def run(self):
        """
        Run the tracer

        Raises:
            IOError

        """
        try:
            dst_ip = socket.gethostbyname(self.dst)
        except socket.error as e:
            raise IOError('Unable to resolve {}: {}', self.dst, e)

        text = 'traceroute to {} ({}), {} hops max'.format(
            self.dst,
            dst_ip,
            self.hops
        )

        print(text)

        while True:
            receiver = self.create_receiver() 
            sender = self.create_sender()
            sender.sendto(b'', (self.dst, self.port))

            addr = None
            try:
                data, addr = receiver.recvfrom(1024)
            except socket.error:
                raise IOError('Socket error: {}'.format(e))
            finally:
                receiver.close()                
                sender.close()

            if addr:
                print('{:<4} {}'.format(self.ttl, addr[0]))
            else:
                print('{:<4} *'.format(self.ttl))

            self.ttl += 1

            if addr[0] == dst_ip or self.ttl > self.hops:
                break

    def create_receiver(self):
        """
        Creates a receiver socket

        Returns:
            A socket instance

        Raises:
            IOError

        """
        s = socket.socket(
            family=socket.AF_INET,
            type=socket.SOCK_RAW,
            proto=socket.IPPROTO_ICMP
        )

        try:
            s.bind(('', self.port))
        except socket.error as e:
            raise IOError('Unable to bind receiver socket: {}'.format(e))

        return s

    def create_sender(self):
        """
        Creates a sender socket

        Returns:
            A socket instance

        """
        s = socket.socket(
            family=socket.AF_INET,
            type=socket.SOCK_DGRAM,
            proto=socket.IPPROTO_UDP
        )

        s.setsockopt(socket.SOL_IP, socket.IP_TTL, self.ttl)

        return s
```
