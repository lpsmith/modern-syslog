This library aims to be a high-performance log data originator
(client) for modern syslog standards.

There is certainly performance left on the table,  but it should still
be considerably faster than say, hslogger.   (It may be particularly
worth investigating BufferBuilder for this library, as it's certainly
a better fit than bytestring-builder.)

Right now it consists of a formatting library for the syntax specified
by [RFC5424](https://tools.ietf.org/html/rfc5424).   The intention is
that any value of type `RFC5424.SyslogMsg` should be guaranteed to be
compliant to the letter of RFC5424.   The rationale behind this is to
help guard against log injection attacks.   Of course, if you wish to
extend the abstraction,  and possibly even create some syntax that is
not fully compliant with RFC5424,  then the `RFC5424.Internal` module
allows for this.

In the rush to create an initial proof of concept,  I did pencil in
some hasty design desicions I'm not entirely happy with.   In
particular,  I am unhappy with the how to compliance with the RFC5424
header values and structured  identifier names is currently
guaranteed.

The main thing left to add to the library is support for various
syslog transport protocols.   Eventually I'd like to support any
standardized or commonly used transport protocol that uses a proper
message framing mechanism and allows arbitrary payloads.
The first will probably be rsyslog's AF_UNIX Datagram transport;  and
hopefully someday syslog over SSL
([RFC5425](https://tools.ietf.org/html/rfc5425)) as well as plain TCP
will also be supported.
