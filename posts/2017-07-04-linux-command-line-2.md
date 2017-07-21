---
title: Linux Command Line - Disk Usage and Disk Filesystem
tags: linux
---

`du` displays disk usage information and `df` displays disk filesystem 
information. These commands also work on Mac OS X.

## Disk Usage

`du <path>` displays size of each subdirectory in the path and the total sum of 
the size of all directories in the path in bytes. `du` is equivalent to `du ./`.

`du -h` displays the data in human readable format. It round sizes up to their 
largest unit and the unit abbreviation: bytes `B`, kilobytes `K`, megabytes `M`, 
gigabytes `G`, etc.

`du -sh` provides the total size of the path in human readable format. It may be 
slow if there are many subdirectories.

#### Data sizes

- 1 bit `b`
- 1 byte `B` = 8 bits
- 1 kilobyte `KB` (1024) = 1024 bytes
- 1 megabyte `MB` (1024^2) = 1024 kilobytes
- 1 gigabyte `GB` (1024^3) = 1024 gigabytes
- 1 terabyte `TB` (1024^4) = 1024 gigbaytes

## Disk Filesystem

`df` displays device name, block size, disk space, used disk space, available 
disk space and mount points.

`df -h` displays the same as above but in human readable format.

#### block

A sequence of bytes or bits of a particular length, the block size. Putting data 
into blocks is blocking and extracting data from a block is deblocking. It helps
reduce overhead and speed up data streams. 

#### mount

`mount` command instructs an operating system that a file system is ready for 
use. It associates a point in the file system (the mount point) to another file 
system. File systems, files, directories and devices (USB, CD-ROMs, DVDs, etc.) 
can be mounted.

`umount` dissociates a mount point from its filesystem.

#### device name

A device file is an interface for a device driver that appears in the file 
system as if it were an ordinary file. In Unix systems most devices appear as 
files in a virtual file system.

#### inode

A Unix data structure that describe objects in a file system like files and 
directories. It stores disk block location, owner and permissions data and time 
of last change, access and modification.

POSIX standards requires the following:

- size of file in bytes
- device id of the device the file belongs to 
- file owner's user id
- file's group id
- file mode
- system and user flags
- inode last modified, file content last modified and time last accessed
- count of hard links pointing to the inode
- pointers to the disk blocks that store the file's contents
