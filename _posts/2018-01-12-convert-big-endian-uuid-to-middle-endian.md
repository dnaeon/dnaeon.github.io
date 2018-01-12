---
layout: post
title: Converting UUID from Big Endian to Middle Endian
tags: golang go uuid big endian middle endian
---
One of the recent projects that I'm working on is
an internal [CMDB](https://en.wikipedia.org/wiki/Configuration_management_database)
system.

The CMDB system represents a mix of different technologies and 
products such as [Go](https://golang.org/), [gRPC](https://grpc.io),
Message Queues ([NATS](https://github.com/nats-io/gnatsd)), 
[RDBMS Systems](https://en.wikipedia.org/wiki/Relational_database_management_system),
[Telemetry](https://en.wikipedia.org/wiki/Telemetry#Software) and others.

The part that I was working on recently is related to the collection of
and establishing relations between Cisco UCS assets and VMware ESXi hosts.
The collector that was developed includes support for collecting information
from a Cisco UCS Manager API endpoint and retrieves information about
Chassis, Blades and Fabric Interconnects.

A CMDB system by itself is only useful when you define the relations
between the CIs that you have.

Having the Cisco UCS Blades collected the next key milestone of the
project was to create a relation of the Cisco UCS assets and the
virtual infrastructure running VMware ESXi on these blades.

Once you establish these relations a CMDB becomes really useful,
because the data that you have along with the established relations
can be used as part of Asset Management and Impact Analys as well.
Being able to tell on which UCS Blade a particular ESXi host runs is
really useful.

In order to properly link the UCS Blades with ESXi hosts I needed
a property that can uniquely identify a blade and ESXi host and also
that property should be something that I can already collect from both
the UCS Blade and the ESXi host.

A good candidate which fulfills that requirement is the
[UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier)
property.

The vSphere Managed Object 
[HostSystem](https://pubs.vmware.com/vsphere-6-5/index.jsp#com.vmware.wssdk.apiref.doc/vim.HostSystem.html) 
is used to represent an instance of a VMware ESXi host in the vSphere API.
Within that managed object we can find the UUID of a host by retrieving the
`hardware.systemInfo.uuid` property.

For UCS Blades we can retrieve the UUID from the UCS Manager by requesting
information about the blade we are interested in.

Having all the data collected it was about time to actually create the
relations between the UCS Blades and the ESXi hosts and things so far
appeared to work just fine, except that once the relations were created
only a small portion of the blades were actually linked with the ESXi hosts.

The big question now was - why were not the rest of the CIs linked properly?

Looking at the UUIDs of a single blade and an ESXi host, for which I knew
were supposed to be linked the issue was noticed immediately - the
UUIDs were not the same. They looked similar, but obviously not the same.

It turned out that the first three groups of the UUID of the blade were
somewhat reversed to what the ESXi host reported as a UUID.

[RFC 4122](https://tools.ietf.org/html/rfc4122) states that UUID is a
128-bit object, where each field is encoded with the Most Significant Byte first.

This however is a recommendation, not a strict requirement that implementations
should follow. As a result you can have a UUID encoded either in Big Endian,
Little Endian byte order, or have a mix of both at the same time.

The [SMBIOS specification](https://www.dmtf.org/standards/smbios)
starting from version 2.6 and later provides a clear definition on how the 
UUID should be encoded.

Here's an excert from that specification:

> Although RFC 4122 recommends network byte order for all fields, the PC
> industry (including the ACPI, UEFI, and Microsoft specifications) has
> consistently used little-endian byte encoding for the first three fields:
> time_low, time_mid, time_hi_and_version. The same encoding, also known as
> wire format, should also be used for the SMBIOS representation of the UUID.
>
> The UUID {00112233-4455-6677-8899-AABBCCDDEEFF} would thus be represented as
>
> 33 22 11 00 55 44 77 66 88 99 AA BB CC DD EE FF.
>
> If the value is all FFh, the ID is not currently present in the system, but can be set.
> If the value is all 00h, the ID is not present in the system.

And this answers our question why the majority of the systems were not properly
linked - the UCS Blades reported the UUID using Big Endian byte order, while
the vCenter server reported the UUIDs of our ESXi hosts using Middle Endian.

The [VMware ESXi 5.0 Update 2 Release Notes](https://www.vmware.com/support/vsphere5/doc/vsp_esxi50_u2_rel_notes.htmlv)
mentions the following about UUID.

> SMBIOS UUID reported by ESXi 5.0 hosts might be different from the actual SMBIOS UUID
>
> If the SMBIOS version of the ESXi 5.0 system is of version 2.6 or later, the
> SMBIOS UUID reported by the ESXi 5.0 host might be different from the actual SMBIOS UUID.
> The byte order of the first 3 fields of the UUID is not correct.
>
> This issue is resolved in this release.

In other words ESXi hosts running 5.0 update 2 and later now perform a check
whether the SMBIOS version is at version 2.6 or later and depending on that they
will represent the UUID either in Big Endian or Middle Endian.

As mentioned earlier during the initial creation of relations between UCS Blades 
and ESXi hosts only a small portion of CIs were able to be linked. These CIs
consisted of old UCS Blade models, with SMBIOS version prior to 2.6, which
now explains why only these were properly linked.

Knowing all that now, what remained to implement was a way to convert the
Big Endian UUIDs to Middle Endian ones, which would allow us to link the
rest of our CIs.

After successfully converting the UUIDs of newer model blades from
Big Endian to Middle Endian we were able to create the correct
relations between the Cisco UCS Blades and the ESXi CIs.

The [Go](https://golang.org/) code that you will find in the rest of this 
post shows how to convert a UUID from Big Endian to Middle Endian, 
which is what we've used in our environment as well to solve this particular issue.

Currently Go (1.10 as of writing) does not provide a builtin type for UUID, so we will
define such. Make sure to check
[RFC 4122](https://tools.ietf.org/html/rfc4122)
and the [UUID Wiki page](https://en.wikipedia.org/wiki/Universally_unique_identifier)
for more details about the different fields below.

```go
// UuidSize is the size in bytes of a UUID object
const UuidSize = 16

// Uuid represents a UUID object as defined by RFC 4122
type Uuid struct {
	TimeLow          uint32
	TimeMid          uint16
	TimeHiAndVersion uint16
	ClockSeqHiAndRes uint8
	ClockSeqLow      uint8
	Node             [6]byte
}
```

Next we define a function that can parse a UUID from a given
byte slice. The code belows uses the
[binary package](https://golang.org/pkg/encoding/binary/), which
provides an implementation of the `ByteOrder` interface for
Big Endian and Little Endian.

```go
// FromBytes reads a UUID from a given byte slice.
func FromBytes(buf []byte) (Uuid, error) {
	var u Uuid
	reader := bytes.NewReader(buf)

	err := binary.Read(reader, binary.BigEndian, &u)

	return u, err
}
```

Now, lets define methods on our UUID types, which can encode
into Big Endian, Little Endian and Middle Endian as well.

This is how our methods look like.

```go
// BigEndianBytes returns the UUID encoded in Big Endian.
func (u Uuid) BigEndianBytes() ([]byte, error) {
	buf := bytes.NewBuffer(make([]byte, 0, UuidSize))

	err := binary.Write(buf, binary.BigEndian, u)
	if err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}

// LittleEndianBytes returns the UUID encoded in Little Endian.
func (u Uuid) LittleEndianBytes() ([]byte, error) {
	buf := bytes.NewBuffer(make([]byte, 0, UuidSize))

	err := binary.Write(buf, binary.LittleEndian, u)
	if err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}

// MiddleEndianBytes returns the UUID encoded in Middle Endian.
func (u Uuid) MiddleEndianBytes() ([]byte, error) {
	buf := bytes.NewBuffer(make([]byte, 0, UuidSize))

	if err := binary.Write(buf, binary.LittleEndian, u.TimeLow); err != nil {
		return nil, err
	}

	if err := binary.Write(buf, binary.LittleEndian, u.TimeMid); err != nil {
		return nil, err
	}

	if err := binary.Write(buf, binary.LittleEndian, u.TimeHiAndVersion); err != nil {
		return nil, err
	}

	if err := binary.Write(buf, binary.BigEndian, u.ClockSeqHiAndRes); err != nil {
		return nil, err
	}

	if err := binary.Write(buf, binary.BigEndian, u.ClockSeqLow); err != nil {
		return nil, err
	}

	if err := binary.Write(buf, binary.BigEndian, u.Node); err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}
```

We will also implement the `fmt.Stringer` interface for our type as well, in order to get
a nice string representation of the UUID.

```go
// String implements the fmt.Stringer interface
func (u Uuid) String() string {
	return fmt.Sprintf("%x-%x-%x-%x%x-%x", u.TimeLow, u.TimeMid, u.TimeHiAndVersion, u.ClockSeqHiAndRes, u.ClockSeqLow, u.Node)
}
```

The method we define below converts a UUID to Middle Endian.

```go
// ToMiddleEndian encodes the UUID into a middle-endian UUID.
//
// A middle-endian encoded UUID represents a UUID where the first
// three groups are Little Endian encoded, while the rest of the
// groups are Big Endian encoded.
func (u Uuid) ToMiddleEndian() (Uuid, error) {
	buf, err := u.MiddleEndianBytes()
	if err != nil {
		return Uuid{}, err
	}

	return FromBytes(buf)
}
```

Wrapping things up, we can now create a sample program that parses a
UUID in Big Endian byte order and converts it to Middle Endian.

The code used so far in this post can be found in the
[go-uuid-endianness repo](https://github.com/dnaeon/go-uuid-endianness).

```go
package main

import (
	"log"

	"github.com/dnaeon/go-uuid-endianness/uuid"
)

func main() {
	data := []byte{0x69, 0xd7, 0x92, 0xd4, 0x3b, 0x2c, 0x11, 0xe4, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x8f}
	u, err := uuid.FromBytes(data)
	if err != nil {
		log.Fatalf("Cannot parse UUID: %s\n", err)
	}

	middleEndian, err := u.EncodeMiddleEndian()
	if err != nil {
		log.Fatalf("Cannot convert UUID to Middle Endian: %s\n", err)
	}

	log.Printf("Big Endian:    %s\n", u)
	log.Printf("Middle Endian: %s\n", middleEndian)
}
```

Running above program we get the following results.

```text
2018/01/12 10:09:31 Big Endian:    69d792d4-3b2c-11e4-00-00000000008f
2018/01/12 10:09:31 Middle Endian: d492d769-2c3b-e411-00-00000000008f
```

Voila!

You can find the code used in this post in the Github repository
[here](https://github.com/dnaeon/go-uuid-endianness).
