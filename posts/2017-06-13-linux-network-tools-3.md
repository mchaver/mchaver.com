---
title: Linux Network Tools - netstat
tags: linux, networking
---

netstat is a command line tool that can list out all network (socket) 
connections on a system: TCP, UDP and Unix socket connections.routing tables, 
interface statistics, masquerade connections and multicast memberships.

## List all connections

```bash
netstat -a
```

## List TCP connections

```bash
netstat -at
```

## List UDP connections

```bash
netstat -au
```

### List listening connections

```bash
netstat -l
```

## Disable reverse DNS lookup

netstat will try to find the hostname of each IP address by doing a reverse DNS
lookup. This is slow.

```bash
netstat -n
```

## List Process Name/ID

It is useful to know what process is using particular port.

```bash
netstat -p
```

## Display Routing Table

Routing tables are used to compute the path of a packet. It is stores routes to 
network destinations and contains the topology of the network. Routing protocols 
and static routes help calculate the routing table.

```bash
netstat -nr
```

## Print Active Connections

```bash
netstat -atnp | grep ESTA
```


## Print network interfaces

```bash
network -i
```

Slightly more readable

```bash
network -ie
```

## Print Statistics

```bash
netstat -s
```


## Continuous output

```bash
netstat -c
```
