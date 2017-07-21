---
title: Linux Network Tools - Netcat
tags: linux, networking
---

## Netcat (nc)

Sometimes it is useful to listen to a port a monitor what requests/data are 
being sent to it. 

### TCP

Open a client to port 7000 on localhost. Type something and hit enter to send 
a message. If the client is unable to make a connection then it will close.

```bash
$ nc localhost 7000
Hello from the client.
```


Add the `-l` optiont to listen to port 7000 on localhost.

```bash
$ nc -l 7000
```

### UDP

Use the `-u` option to send to UDP packets.

```bash
$ netcat -u localhost 7000
```

Use the UDP and listen options together to create a simple UDP server.

```bash
$ netcat -lu 7000
```

### Port scan with Netcat

The `-z` option performs a scan. The `-v` option give a verbose output.

```bash
netcat -zv localhost 80

Connection to 127.0.0.1 80 port [tcp/*] succeeded!
```

We can also use a range of ports. Another useful option is `-n` which tells 
`nc` to not resolve the IP address using DNS. You have to use the IP address. 
This improves performance.

```bash
$ netcat -zvn 127.0.0.1 1-1000
```

`nc` sends messages to standard error. In order to pipe the results to another 
program, we need to redirect standard error to standard output via `2>&1`. We 
can then filter the results with `grep`.

```bash
$ netcat -zvn 127.0.0.1 1-1000 2>&1 | grep succeeded

Connection to 127.0.0.1 80 port [tcp/*] succeeded!
```

### Using files with Netcat

Put received messages into a file.

```bash
$ nc -l 7000 > output.txt
```

Send file from client to server.

```bash
$ nc localhost 7000 < message.txt
```

You can even serve html files with netcat and access it with your browser.

```bash
$ nc -l 7000 < index.html
```

### Timeout

`-w` keep connection open for x seconds.

```bash
nc -w 10 localhost 7000
```

`-k` flag forces it to stay open forever, even after the client logs off.

```bash
nc -lk 7000
```

### IPV4 and IPV6

`-4` IPV4 address

```bash
$ nc -4 -l 7000
```

`-6` IPV6 address

```bash
$ nc -6 -l 7000
```

### Proxying

`mkfifo` make first in, first out named pipe. Pipes are generally unidirectional.
This allows the proxy to send and receive data from the target server. We can 
access `www.mchaver.com:80` via `localhost:7000`.

```bash
$ mkfifo bidirectional
$ nc -l 7000 0<bidirectional | nc www.mchaver.com 80 1>bidirectional
```

### Alternatives

#### Python

```
python -m SimpleHTTPServer 7000
```
