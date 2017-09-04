---
title: "Docker: Taming the Beast - Part V: Docker Networks"
description: "In part V, we will examine the \"last\" docker concept that is crucial to creating powerful docker stacks: the Docker Networks. They are a very powerful tool, and is often misunderstood and not used to their full potential."
toc: yes
tags: docker,container,continuous-integration,continuous-delivery
---

## Introduction

Hello again and welcome to Part V!  
Part V... this begins to look like a real series of articles, I'm happy. And I'm happy that you're still with me.  
If you missed it, [here](/articles/2017-01-28-Docker-Taming-the-Beat-Part-V.html) is the Part IV, and in case you're new here, you can start at the beginning [here](/articles/2016-05-26-Docker-Taming-the-Beast-Part-1.html).

Last time, we saw how we could make part of our data persistent and bypass the unionFS mechanism. This was especially important if we did not want to lose data, and this allowed us to have truly ephemeral containers, _i.e._ containers that we can shutdown / destroy and recreate at any moment.  
This is really important, and I have had lots of examples in the last couple of weeks. On the #docker IRC chan, there has been quite a number of people that came and asked variants of "is this possible to bind ports on a container without destroying and recreating it?". This is not possible, the only solution is to effectively destroy the container and recreate it with the correct ports binding, and that should not be a problem!  
Since containers should be ephemeral, all important data should be stored in Volumes: destroying containers should _never_ be a problem.

Anyway, that was topic for Part IV, we're in Part V now. So what's this about?  
It's about the other most misunderstood Docker feature: Docker Networks. I don't blame people because this feature is fairly new (when compared to the rest of Docker), but this has still been quite a while, so with the help of this article, I hope it will all be clear.

To make it extra short, the Docker Network is a feature that enables you to make your containers communicate together!

Excited?! Then read on!

## The Basics
In this article, we will need to test network connectivity because we'll be talking about networks. I have hesitated a long time between using standard Linux tools to test connectivity and making a small real example. To be honest I had even begun writing the article with the example, but I felt that it was too much information in a single article, and besides... Docker Networks is a very misunderstood feature and I would not want people to focus more on the example than on the real topic of the article.

So we will use `netcat` to test connectivity, which is the normal, standard Linux way. If you already know how to use `netcat` easily, this will allow you to focus entirely on the Docker networking aspect. If you do not, fear not, this is _very_ easy to understand, I _will_ explain everything, as usual and you'll get to have one more tool in your toolbox!

And since I can already hear you complaining about the fact that we don't make a real example, don't worry: I've decided that Part VI will be a full article, entirely dedicated to making a slightly complex stack, real example.

Happy? :)

Let's do this, I'm excited!

### Why Do We Need Docker Networks Anyway?
If you've followed this series---or heard a tiny little bit about Docker---you have understood by now, that Docker is about _isolation_. It's about running processes on the same Linux host, but each of these processes, "invisible" to its neighbors.  
This works actually so well, that we run in a problem pretty soon when we're trying to build something slightly more complicated than the examples we've seen up to here.

Let's see this problem in action, right now.

As I said just above, we will use `netcat` to test connectivity. I will now make a very small introduction to `netcat` so that people who don't know about it can follow. If you need a small refresher, read on, otherwise, you can skip it.

#### (Very) Small Introduction to netcat
`netcat` is a standard Linux tool that is used to test TCP and UDP connections. If you know `telnet`, it's a sort of `telnet` on steroids, at least for the usage we will make.

Just to be clear, when I say "test connectivity", I'm not talking about those speed-test you make to know how fast you can download and upload, no no, I'm talking about ports being opened and hosts being reachable.

So the easiest and quickiest way to do this is to have a (very) small server listening on the desired port on the desired host, and a (very) small client that tries to connect to the given host on the given port. That's it. Really.  
That's all you need if you want to test connectivity. And `netcat` is _perfect_ for this.

Just before we begin, know that the command `nc` is an alias for `netcat`. So if you read articles on Internet that has commands beginning with `nc`, it _is_ `netcat`. Use whichever you want.

So how do we create a very simple server with netcat? It's easy, you need to tell `netcat` that is should `listen` (and not `connect`), so this is done with the `-l` option.  
Then you need to specify a port on which it should listen, this is done with the... `-p` option. See? Simple.

Mind that as a non-root user, you cannot create a server on ports `0-1024`, so make sure you pick a port that is above.

For instance, let's make `netcat` listen on port `1089`:

```Bash
$> netcat -l -p 1089
```

"What happened? My screen seems stuck.". It's a good sign, it means `netcat` succeeded in creating a server, and it is now listening on the port you specified.

Now, open another terminal, we'll need to make a client. As a client, `netcat` needs to know two things: the host and the port to which it should connect to. So:

```Bash
$> netcat localhost 1089
```

Again, nothing seems to happen: no feedback. But it there was an error, it would have displayed it. So now your client is connected. Try writing something in the client's console, like `"Hello"`, and press `<Enter>`.  
You should see the string written on the server's console, we now have confirmation that the two can communicate.

Alternatively, if you _want_ feedback, use the `-v` option (for "verbose"):

```Bash
$> netcat -v localhost 1089
localhost [127.0.0.1] 1089 (ff-annunc) open
```

This way you have some feedback that the connection was successfully established.

Told you it was easy, right?!

This is what we will do all along this article, to test connectivity when we need it, so make sure you understand what's going on.

#### So... What Was The Problem Again?

Yes, yes I'm coming to it!

So now that we have seen the basics of `netcat`, you will be able to understand when something works or doesn't.  
So, containers means isolation, as we've said before. But sometimes, it means _too much_ isolation.

Let's suppose for a moment that we are building a sort of webserver and we need a server (`nginx`) and a database `(postgresql)`. As we've understood Docker's basics, we will separate the two instances in two containers.

Let's emulate this by creating two containers, we will name them accordingly, but we won't really setup `nginx` or `postgresql`. Instead we'll use `netcat` to create a server or connect to the corresponding port. This should be enough to highlight the problem.

Just before we begin, we have a small thing to do. Since we will be using `netcat` a lot in this article, we need an image with `netcat` installed; otherwise it will be a pain to re-install it everytime.  
Up until now, we have only worked with the `ubuntu` and `nginx` images. So as not to change too many variables at the same time---which is often crucial to learn correctly---we will keep using `ubuntu` as a base image.

So let's make a very small Dockerfile that creates a `test-net` image:

```Dockerfile
FROM       ubuntu:16.04
MAINTAINER nschoe<nschoe@protonmail.com>

RUN        apt update && apt install -y --no-install-recommends netcat inetutils-ping
```

Now build it with `docker build -t net-test .` and it's done. **Obviously** starting from such a big image as `ubuntu:16.04` (which is in the `600MB`) _just_ to have `netcat` is highly inefficient. But the goal here is to learn about networks.

Okay, back to our test.

Let's begin with `postgresql`. By default, `postgresql`'s port is `5432`, so let's create a container and inside it, a `netcat` server listening on this port.

To emulate the container running postgres, first create a container from `net-test`:

```Bash
$> docker run --rm -it --name fake-postgres net-test bash
root@6995c50eaff3:/# netcat -l 5432
```

Okay, so our fake postgres container is running. Let's now create a fake `nginx` container, which will connect to the fake postgres:

```Bash
$> docker run --rm -it --name fake-nginx net-test bash
root@9e09afc2d19a:/#
```

Now how to connect this fake-nginx to the fake-postgres? We saw earlier that the syntax was `netcat <host> <port>`. We know the port, it's `5432`. But what about the host?

First idea: `localhost`, after all, why not? Containers are on the same host, let's try:

```Bash
root@9e09afc2d19a:/# netcat localhost 5432
localhost [127.0.0.1] 5432 (?) : Connection refused
```

Okay. That doesn't work.  
And after all this is to be expected: `localhost` inside different containers resolve each to their container, otherwise we could not really say they were isolated.  
Careful readers might have a suggestion: you may have seen the `PS1`: `root@9e09afc2d19a`.  
Usually, the part after the `@` is the hostname. And sure enough, if you try to run `hostname` in the terminal, you get just that:

```Bash
root@9e09afc2d19a:/# hostname
9e09afc2d19a
```

So we might try this.  
But the two containers have a different hostname, so which one to try? Let's try both, to be sure:

```Bash
root@9e09afc2d19a:/# netcat 9e09afc2d19a 5432
9e09afc2d19a [172.17.0.3] 5432 (?) : Connection refused

root@9e09afc2d19a:/# netcat 6995c50eaff3 54321
6995c50eaff3: forward host lookup failed: Host name lookup failure : Resource temporarily unavailable
```

So the first had no chance of succeeding: in the fake-nginx container, I tried to connect to host 'fake-nginx', which is localhost, and we've just seen that it failed.
The second attempt however, made much more sense; we tried to connect to the host of the fake-postgresql container, which _should_ work.

It takes time, but then eventually it fails. And _this_ precisely, is our problem: containers are _isolated_, even on the network stack.

So this is what starts this whole article: how do I make these two (or more) containers communicate?

Now I hope I convinced your that we do have a problem, and now let's see if I can help you understand how to fix it!

### Two Separate Use-Cases

So we have experienced the container's network isolation, and we have understood that this might be a problem when we actually need communication. The rest of this article will be about how to break that isolation in a controlled way.

As often with Docker, there is more than one way to do this, and as it is always the case, they correspond to different use-cases.

Here, when it comes to network isolation, we will talk about two use cases. This might be a little early, but I'd like you to take a minute (you know, we have talked about this... do try to think about this for a minute, see if you can find ideas) to find the two use cases.  
Here is advice: it deals with whom containers have to talk to.

...

I hope you tried, let's see if you got that right!

The first use case is simple: this is the one we've just demonstrated; it is when two (or more) containers need to talk to each other. It might be that you have a webserver (like `nginx`) that needs to talk with a database (`postgresql`), maybe we can even imagine a caching stage (like `redis`). All these containers need to talk to each other, and so it is a first use case.

The second use-case if when **the host** needs to talk with a container. You might have forgotten about this, but if Docker can be used to ship applications in a controlled environment for a client, or be used to deploy a great number of application stacks, it _can_ also be used to containerize one instance of a program for use on your host.

Since your host (_i.e._ the computer running docker) is not a container, there are different mechanisms to make it communicate with the containers. We will see both cases.

## First Use-Case: Communication With the Host; Binding Ports

We will start with how we can make the host communicate with containers, as it is generally the first use-case people have.

Suppose you are trying to containerize a web server, like `apache` or `nginx` for your host (for instance, to serve your local, self-hosted website(s)). This is a classic situation. You might be interested in trying a new version of your webserver but are not ready to upgrade the version on your host, or you might be trying to switch from one to another, or your simply want to keep it isolated from the rest of your environment.

Anyway, this is what we are trying to do. The same kind of problem that we saw in the very first paragraph will arise: we need a way to access our container, as it is it, who is running the webserver and has the data in it.

The situation is really simple: when we create a container for `apache` or `nginx`, and we're done configuring it, this server inside will listen to ports `80` and `443`. But Docker is good at isolation, and thus he created a nice, little isolated network environment for the container. Meaning that the container has its own new (virtual) network interface, and through it it communicates (or not) with the rest of the world.  
Remember: the goal of Docker toward your containers, is to make them believe they are running alone in the system, and control everything it can about it, including its Network I/O.

As a reminder, and if you want to see it with your own eyes, first stop &  destroy every container in your host. When you have no other (check with `docker ps -a`), let's look at the network interfaces your host has:

```Bash
$> ip -oneline link show

1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN mode DEFAULT group default \    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
2: wlp4s0: <NO-CARRIER,BROADCAST,MULTICAST,UP> mtu 1500 qdisc mq state DOWN mode DORMANT group default qlen 1000\    link/ether 34:02:86:2b:79:9c brd ff:ff:ff:ff:ff:ff
3: docker0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP mode DEFAULT group default \    link/ether 02:42:70:6d:78:ff brd ff:ff:ff:ff:ff:ff
```

Here we can see 3 networks interfaces: the local loop `lo`, my WiFi connection `wlp4s0` and the `docker0` virtual network interface that docker created when your installed it.  
If your computer has other network interfaces, you'll see them here.

Now let's run a container with `docker run --name test -itd nginx bash`, and run this command again:

```Bash
$> ip -oneline link show

1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN mode DEFAULT group default \    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
2: wlp4s0: <NO-CARRIER,BROADCAST,MULTICAST,UP> mtu 1500 qdisc mq state DOWN mode DORMANT group default qlen 1000\    link/ether 34:02:86:2b:79:9c brd ff:ff:ff:ff:ff:ff
3: docker0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP mode DEFAULT group default \    link/ether 02:42:70:6d:78:ff brd ff:ff:ff:ff:ff:ff
11: veth9170aac@if10: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue master docker0 state UP mode DEFAULT group default \    link/ether 42:a7:d9:db:ca:17 brd ff:ff:ff:ff:ff:ff link-netnsid 0
```

Here you can see a new (virtual) network interface named `veth9170aac@if10`. the `"veth"` prefix stands for `"virtual ethernet"`, and it reprensents the (ethernet) network interface of the container we have just created.

The goal of all of this, is that _for the container_, it has its own network interface. This means that you are free to use any port inside the container, as they are separated from the host's network interface. By defalt then, "all ports are free".

### Okay, So How do I Make My Host and My Container Talk to Each Other?

You bind ports. That's how.  
What that _means_, is that you bind (as in "connect") a port on one of the host's network interface, to a port on the container's network interface.

Concretely that means, that everything going through one of these ports if going out on the other. You can link the host's port 444 to the container's 443 ports. This way, when you're accessing the host's 444 ports, your requests are actually forwarded to the container's 443 port, and vice-versa.  
If the host's interface on which the port is bound has external (Internet) connectivity, this is how you can make people outside access your container's services.


[INSERT yED DRAWING HERE TO DESCRIBE HOW WE JUST BROKE THE ISOLATION]

As often with Docker, there are several syntaxes, let's review them here.

### Explicit Mapping

When binding ports with explicit mapping, you define exactly which ports on the container is bound to which ports on the container. This is obviously done at run time (_i.e._ `docker run`). For that, you use the `-p` (this is lowercase `p`). The syntax is: `docker run -p <hostPort>:<containerPort>`.  
For instance, to bind the container's 443 port to the host's 444 port, you would do `docker run -p 444:443 <image>`.

Also some people find it counter-intuitive (I don't), make sure to remember that the first is the **host's** port and the second is the container's.  
Of course, nothing prevents you from using the same port, so you can run `docker run -p 443:443`. This is used when you want to containerize a service on the host.

This goes without saying, but once you bind a port on the host, you cannot re-use that port. This is not docker, this is classic networking, right? You cannot have two applications listening on the same port, this is why I said that explicit port mapping is used when you want to containerize a service on the host: usuall you have **one** nginx instance running on your computer and it has responsibility of the port 443.  
Well, here this is the same: you have **one** container running nginx and you've bound the ports 443 of both. If you want another nginx instance, your need to bind this second instance's 443 port to another port on the host.

### Random Mapping

This is the opposite of explicit port mapping. In this scenario, you want to bind the container's port to _a port_ on the host. This is used when you want to have several instances of the same application, and you want to bind the ports. But you don't want to / cannot specify the ports every time.

The syntax is: `-p <containerPort>`. For instance: `-p 443`. This is basically saying "I want the container's 443 port bound to the host, use a random port on the host".

This is cool because you don't have to remember and specify the ports on the host on which you want to bind, but the drawback is obviously that you need a way for the application that needs to contact your container to discover the port it needs to connect to.

A use case for this can be for debugging. For instance, if you have a setup that includes spawming database containers, you can bind the database server's access port randomly on the host. Most of the time, you would not need it, so it doesn't really matter, but when comes the days when you need to debug, find the port (we'll see how at the end of this section) and connect to it.

Note that when binding to random ports, the random ports are chosen in the 32,7xx range.

### Specific Interface

By default, when you bind ports (whether specifically or randomly), it is bound on interface `0.0.0.0`, which means "all interfaces". What this means is that if you bound your nginx's `443` ports to the host's `443` ports (for instance), when this `443` ports will be routed to your container's, whatever the interface used: `eth0`, `eth1`, `wlan0`, `localhost`, etc.

It is common for servers to have several network interfaces (meaning several Ethernet connections for instance, several network cards) with different IPs. If you don't specify otherwise, Docker will bind ports on all of these interfaces.

This can be useful (for instance in the case of binding commonly-used ports, like `80`, `443`, etc.). But it might also be very dangerous: if we take the previous example, where I said that if you had several database containers, you could bind the server port (`5432` for PostgreSQL for instance) randomly so that you could debug when needed, then it is dangerous. Doing this would essentially open a port on the outside that is routed to your container's db.  
Normally you have set up well-protected user and passwords to access the database, but still, this is dangerous.

Let's see a quick example of this.

First, find one public IP of your host with `ip addr show`, under the `inet` section (for ipv4 address) or `inet6` (for ipv6 address).  
This is your "public ip" (I'm using quotes here, because if you're doing this on your own computer, at home or at work, you're most likely behind a router NAT-ing your connection, so you'll have a local IP on the network ; only if you're doing this on a remote server will you have a real "public" IP).  
But it doesn't matter, it works exactly the same, just consider it to be your IP to be public, the one unknown users on the Internet use to connect to you host).

```Bash
$> ip -o addr show
1: lo    inet 127.0.0.1/8 scope host lo\       valid_lft forever preferred_lft forever
1: lo    inet6 ::1/128 scope host \       valid_lft forever preferred_lft forever
2: wlp4s0    inet 192.168.43.28/24 brd 192.168.43.255 scope global dynamic wlp4s0\       valid_lft 3332sec preferred_lft 3332sec
2: wlp4s0    inet6 fe80::3602:86ff:fe2b:799c/64 scope link \       valid_lft forever preferred_lft forever
3: docker0    inet 172.17.0.1/16 scope global docker0\       valid_lft forever preferred_lft forever
3: docker0    inet6 fe80::42:1fff:fe82:d2ea/64 scope link \       valid_lft forever preferred_lft forever
```

As you see on the 3rd line (first entry prefixed `2:`, I'm using my `wlp4s0` interface here, and my "public ip" is: `192.168.43.28`).

Then create a container from image `postgres`, bind the port `5432` (in PostgreSQL, this port is the ports used to connect to the database server, so this is where you can take control of the database if you manage to connect).  
You are most likely to already have port `5432` listened to on your host, so if you can afford stopping the postgres on your host for a couple of minutes, do it: `sudo systemctl stop postgres`, otherwise, in the next command, change the port on the host on which you bind the container's `5432` port.

```Bash
$> docker run --name port-binding-test --rm -it -p 5432:5432 postgres
```

Let it start, and then, it's time to inspect the ports.

First, on `localhost`:

```Bash
$> nmap localhost
Starting Nmap 7.40 ( https://nmap.org ) at 2017-03-08 09:09 CET
Nmap scan report for localhost (127.0.0.1)
Host is up (0.000058s latency).
Not shown: 999 closed ports
PORT     STATE SERVICE
5432/tcp open  postgresql

Nmap done: 1 IP address (1 host up) scanned in 0.04 seconds
```

So `nmap` (which is a program used to scan opened ports on an IP) reports that the ports `5432` is opened (here you'll have your other ports, if you chose to bind randomly or if you manually specified another port). Let's check if we can connect to it:

```Bash
$> netcat -v localhost 5432                                                                 ~
localhost [127.0.0.1] 5432 (postgresql) open
```

And yes, this works.

Now let's run this `nmap` again, but on the host's public interface (this is what a user would get if he ran `nmap` on your host's IP address):

```Bash
$> nmap 192.168.43.28

Starting Nmap 7.40 ( https://nmap.org ) at 2017-03-20 08:35 CET
Nmap scan report for 192.168.43.28
Host is up (0.000071s latency).
Not shown: 999 closed ports
PORT     STATE SERVICE
5432/tcp open  postgresql

Nmap done: 1 IP address (1 host up) scanned in 0.12 seconds
```

Depending on the services running on your host, you might have other ports opened, so more lines like this. But the important part for our example is the line `5432/tcp open postgresql`.

So we have confirmed that, when using the `-p` option to bind a port with the format: `-p <host-port>:<container-port>`, this port was bound for every interface.  
And this is not always what we want (think again about opening a connection to out postgresql database to debug ; or suppose your host has a network interface on which there is no external connectivity but is used for LAN connections with other hosts, etc.).

The docker developers have again thought about this and provided us with a way to specify the interface when we want to bind ports, the syntax is: `-p <ip-of-interface>:<host-port>:<container-port>`.

We'll try it again with our postgres example, first, shut the container from the previous example down (and remove it if you did not use the `--rm` flag).

This time, we will try to bind the `5432` port only to the localhost interface, which corresponds to the use-case we described earlier: leave a port opened so that if we need, we can have direct access to the database, but we want this to be possible **only** from the host itself (for someone logged in via SSH) and not for the whole world to see!

```Bash
$> docker run --name port-binding-test --rm -it -p 127.0.0.1:5432:5432 postgres
```

Notice how we prefixed our ports numbers with `127.0.0.1` which is the localhost's IP address. Let it start for a couple of seconds, and now, let's run our `nmap` commands once again.

First, on `localhost`, we expect the port to be opened:

```Bash
$> nmap localhost
Starting Nmap 7.40 ( https://nmap.org ) at 2017-03-20 08:43 CET
Nmap scan report for localhost (127.0.0.1)
Host is up (0.000066s latency).
Not shown: 999 closed ports
PORT     STATE SERVICE
5432/tcp open  postgresql

Nmap done: 1 IP address (1 host up) scanned in 0.05 seconds
```

The output has not changed: the port is opened (this is obviously what we expected, since we bound the port to the localhost interface).

Now, let's run `nmap` on the public IP, and _this time_ we expect the port **not** to be opened:

```Bash
$> nmap 192.168.43.28
Starting Nmap 7.40 ( https://nmap.org ) at 2017-03-20 08:44 CET
Nmap scan report for 192.168.43.28
Host is up (0.000069s latency).
All 1000 scanned ports on 192.168.43.28 are closed

Nmap done: 1 IP address (1 host up) scanned in 0.32 seconds
```

Ah ah! Now the port `5432` is closed! Which is _exactly_ what we wanted. (Note: as before, depending on the services installed and running on your host, there might very well be other ports opened. Please understand that here we don't seek `"All 1000 scanned ports on 192.168.43.28 are closed"`, we simply seek that the line `5432/tcp open postgresql` be not here!)

So this is a _very_ handy tool, because now you can bind port on the desired interfaces and have a very fine granularity expose your services.  
Note that if your host has more than 2 interfaces (let's say 5) and you want to bind the port on 3 of them but not the other two, you need to use the `-p` option three times: `-p <ip-1>:5432:5432 -p <ip-2>:5432:5432 -p <ip-3>:5555:5432`. Here I have bound the postgresql port `5432` to the same port number `5432` of the first two interfaces and on the port `5555` of the third interface.

Imagine the possibilities: you can bind some ports of your containers to the `tun0` interface of your host and this gives access to the ports only through the VPN tunnel, or you can bind ports to the internal Ethernet interface to allow fast communication intra-datacenter.

Handy!

### Range of Ports

This is all very nice and all, but there remains one tricky part. Suppose you are using an application that spawns ports randomly in a specified range and you want to bind them to the host, how do you do?  
With what we've seen so far, it is possible but _very_ cumbersome: we would have to bind all ports manually. What if there is a hundred?

A common example of this situation is if you are dockerizing a WebRTC gateway for instance: it chooses a random UDP port for each client.  
More generally this will happen every time you have a server that spawns a worker on a random port for each new connection.

What we are looking for now is for a way to bind a _range of ports_ from the container to the host.

And lucky for us, there is a way, the syntax is as such: `-p hm-hM:cm-cM` where:

- the `-` sign denotes a range
- `hm` is the lower port of the range for the host
- `hM` is the upper port of the range for the host
- `cm` is the lower port of the range, for the container
- `cM` is the upper port of the range, for the container

For instance, if you want to bind all ports between `2000` and `2100` in the container to ports between `5000` and `5100` on the host, you will write:

```Bash
$> docker run --name port-binding-test --rm -it -p 5000-5100:2000-2100 postgres
```

If you want to check the ports bound for a given container, you have two possibilities: either `docker ps` and there's a column `PORTS`, or you can use the dedicated command `docker port <container-name>`

In our case:

```Bash
$> docker port port-binding-test

2000/tcp -> 0.0.0.0:5000
2016/tcp -> 0.0.0.0:5016
2047/tcp -> 0.0.0.0:5047
2062/tcp -> 0.0.0.0:5062
2063/tcp -> 0.0.0.0:5063
2079/tcp -> 0.0.0.0:5079
2085/tcp -> 0.0.0.0:5085
2007/tcp -> 0.0.0.0:5007
2033/tcp -> 0.0.0.0:5033
2072/tcp -> 0.0.0.0:5072
2073/tcp -> 0.0.0.0:5073
...
```
The only caveat though is that the two ranges must be equal: you can't have something like `-p 5000-6000:2000-2100` which would means "bind ports 2000 to 2100 to whatever ports between 5000 and 6000". I hope this will be fixed, but for now, you must make sure your ranges are of equal length.

#### Conclusion on Binding Ports

That's it for use-case #1!  
You know everything you need to know to establish communication between your host and your containers, network-wise. Just to sum up what we have just seen: we have understood that docker, being so good as isolation actually isolated the network stacks of our containers.

Meaning as far as the containers are concerned, they have their own network interface (usually `eth0`) which is bridged to one of the host's network interface (more on this later) to provide external connectivity ("access to Internet").

But docker hides this from the host, which means that, by default, they have no way to communicate: the host simply cannot talk to the container and vice-versa (the container doesn't even "know" that there _is_ a host!).

But sometimes you want to contact your containerized processes from the host: so you need to bind a port. This is literally just a way of "piping" network packets on one of the host's port to one of the container's.

This binding of port is useful for examples such as:

- when you want to dockerize a host's service, for instance if you want your apache or nginx webserver dockerized (meaning you don't want an apache or nginx running "vanilla" on your host anymore: you want to isolate it).  
In such a case, you need to bind the ports to their host's equivalent (so `80:80`, `443:443`, etc.). This is because when you want to containerize the service, you expect the service to work "as before": applications used to contact your host computer on its 80/443 port, it should not change anything for them. So you simply, brainlessly pass the traffic incoming on your host's 80 or 443 ports to your container's 80 and 443 ports.  
Obviously this is true for _any_ ports of _any_ containerized service: you can containerize your database instance (postgresql, MySQL, etc).  
You can customize your `redis` cache instance, etc.  
Note that you can containerize any service you want: it doesn't have to do with the network! But if it does happen to have something to do with networks, then make sure to bind ports.
- when you want to be able to contact a dockerized application, for debugging, logging, maintenance, etc. This usually happens with applications for which you have several instances. In this case, you will _usually_ use random port mapping (or if you have only a few of them, you can always specify the ports yourself, but past 4-5 instances it becomes cumbersome).  
Usually with such a setup, you tend to use random port mapping but on a specified (often private, non-externally reachable) network interface (maybe a local network interface on your host, or a VPN tunnel?)

We have just concluded big part and it was full of important information. Just so you know, I estimate that a solid 30% of the questions I answer on #docker are about Docker Networks, and a lot of them pertain to this topic of making a container communicate with the host. You _absolutely_ need to understand this.  
I hope I made this crystal-clear, and _please_, if _anything_ is still unclear, [shoot me an email](mailto:nschoe@protonmail.com). I will answer, **really**. It's paramount that you understand this.

I suggest you take a break now, because you need to digest everything you've just read. And as usual, I _mean it_: in the next section, we will begin use-case #2, and when it comes to Docker Networks, people _always_, and I mean _always_ get it wrong. They always mix the two use-cases, so don't read the two cases one right after the other, because in your brain, you will join them and they will become this big mess of "Docker Network-ish". It's important that you understand there are two topics in Docker Networks.

![](/images/coffee_break.png "Coffee time!")

## Second Use-Case: Make Containers Communicate Together

Hi back!

I hope you _did_ take a break, and that you are ready for new concepts now, because if you thought this one was going to be easy, well... you'd be right, because I am going to make my best to explain in a easy-to-understand manner.  
But seriously, this is going to be a difficult and complex topic (but so interesting!), so pay attention and don't skip steps.

I may have already said it earlier, but Docker Networks is, I think, the #1 feature of Docker. This is _the_ most fantastic thing Docker brought to us!

From the first section, we have seen how to break the isolation between a host and a container. This is very interesting and important: we have seen that we can open ports between containers and (some of) the host's network interfaces.

The _second_ use case, equally (if not more!) important (and cool!) is to make two containers (or more!) communicate together, _without the host's intervention_.  
I insist on this last part: too often, way too often I see people use the first method we saw (opening ports between containers and the host) to try and make two containers communicate together. Bah! After you read this I don't want you to even _think_ about this.  
This is wrong at so many levels.

No here, we will see a method, no... **the** method to make containers communicate together.

If you're ready, let's proceed!

### The Old-Fashioned Way: `--link`

**This section is important**: half the time I spend answering questions about how to make container X communicate with container Y, I have to deal with `--link`. So I'd like to address this problem now, once and for all.

If you are not totally new to this and have heard of (or used) `--link`, read this, because you need to forget about `--link`, and I suggest you start now :)

If you are new to either Docker or container's networking, then read this section too, so you know what _not_ to do.

So, how to begin... Docker is an amazing piece of software that made containers sound easy to use. But the truth is that containers are a complex mechanism and it's tough to get it right. So they had _a lot_ to deal with, and I do mean a lot. There is a reason why to this day, there are still about 2 thousand opened issues on their github: it's complex.
On top of that, let's remind us that Docker is still young, so if you've been around a bit, you must have seen a lot of changes. Containers communication if one of them (he he, you thought I was lost in my thoughts, didn't you?!).

All of this to say that, at some point, if you wanted to connect two containers, you would do it with `--link`. `--link` is (I should say _was_) an option to `docker run`, and as its name implies, it allows to "link" / "connect" two containers together, so they can break the isolation and talk to each other.  
For a long time, this was the only method and this worked.

But this was not very _practical_: you launched container #1, and then when launching container #2, you needed to use `--link` to container #1. So there was a sort of "precedence": it was not entirely symmetric.  
Besides, it was a bit more complex to add a third, and fourth and a fifth container to the party. Something else worth noting is that when using `--link`, **all** environment variables were shared, and by "shared" I mean that they were accessible from the other container, as if they had been defined there.  
Considering the number or people I see sharing SQL user / passwords through environment variables, I say it's a _bit_ dangerous.

Anyway, what's important to note, is that:
1. `--link` is a thing of the past: I will show you an alternative; and once I have, there is no reason to ever use `--link` anymore.
2. `--link` was a way to "link" / "connect" two containers, really. But it was inappropriate to build more complex stacks of more that 2 containers, which can happen quite quickly.

`--link` was understandable in the beginning, when docker was still young and you needed an quick way to  connect containers #1 and #2.  
But when you think just a tiny bit more about this, it's quite easy to think of a slightly more complex setup: suppose we have a container running a database, a container running a webserver, a container running a custom DNS server, a container running a cron job to periodically backup the database, a container running a level 2 database.  
Do you see how to connect containers there? Maybe yes, maybe no, but what I see in such a case, it's not that we need to connect containers individually, but rather create a sort of LAN (Local Area Network) in which all these containers should reside.

And this is _exactly_ what the new Docker feature does, so let's forget about `--link` now, and learn something new!

### The New, Powerful And Appropriate Way: Docker Networks!

From the last paragraph that you read, you should have an intuition of what _Docker Networks_ are, and this is cool, because that's exactly what I intended. If you don't, don't worry: I will explain.

Simply put, a Docker Network is comparable to a LAN or custom WiFi network. You create a Docker Network and `connect` containers to the network. The magic is that containers inside a same network can see each other, and thus _talk_ to each other.

The most important aspect (the one I really want to insist on), is that Docker Networks are separate things from containers. On one hand you have your containers, on the second hand you have your Docker Networks. You choose to connect containers to Docker Networks or not, and it's not definitive: once your container has joined / connected to a Docker Network, you can just as easily `disconnect` it.  
In the beginning, I found that it helps a lot to think about Docker Networks as WiFi networks (for the analogy, of course). It helps understand, for instance, that contrary to `--link`, there is no "precedence": container #1 is connected (or not) to the Docker Network, and this is also true for container #2.  
But the order in which you connect them has absolutely no meaning: the same way that you don't case if your smartphone is connected to your home's WiFi network before or after your laptop. Really, this is it.

Another important difference with `--link` is that since containers and Docker Networks are two completely separate entities, you don;t have that leaked environment variable anymore: again, think of WiFi networks. Just because you connected your laptop to your WiFi network doesn't mean every other laptop on the network can access all your data, and much less your environment variables.

And just to make sure, I'll repeat it: connecting containers to a Docker Network breaks the isolation on _the networking stack_. It _just_ means that your containers will be able to see and talk to each other. But let's make it clear that no data is shared, no isolation is broken on the filesystem stack etc. You still have totally independent containers.  
And this is the magic of Docker Networks: hey break the isolation _just enough_, exactly like we need it to.

Let's talk about Docker Networks more in depth!

#### Basic Commands

Commands that deal with Docker Networks are `docker network` commands. It obviously accepts subcommands that we will see now.  
The very first thing we can do with Docker Networks is to create one! This is easy enough, it's done with `docker network create`. This command takes some options and the name of the network. So let's do it:

```bash
$> docker network create MyFirstNetwork
b2c78b7b2ec68ef1fe702be331aa9cd3f3e1733039b8a064c702652439e1b20b
```

Like everything in Docker, a Docker Network has a unique identifier, which is this weird returned hash. We don't really need it, as we will mostly deal with names. But, as with every Docker command, know that in place of names, you can use this ID if ever you need to (by the way, **your** hash will obviously be different than mine!).

So the fact that docker returned a hash means the creation was okay. How can we check this?

By listing the networks, of course. This is done with `docker network ls`, so let's do it now and see our network:

```bash
$> docker network ls
NETWORK ID          NAME                DRIVER              SCOPE
b2c78b7b2ec6        MyFirstNetwork      bridge              local
40670738e633        bridge              bridge              local
2683c3824f8f        host                host                local
a84e731c4387        none                null                local
```

Wait, what?!  
The output above is what you should see if you haven't played with Docker before (or at least with Docker Networks).

So even though we have only created one Docker Network, we can see that we have four already, what's going on? I'll be explaining in a couple of paragraphs, bear with me for now. _Right now_, we're interested in seeing that `MyFirstNetwork` has been created.

Now that we have a Docker Network on hands, we might want to try and look a bit at it, because so far we have only just seen its name, which is already something, but quite limited. There is the `docker network inspect` command, that can, as its name implies, _inspect_ and give some more thorough information about a network.

```bash
$> docker network inspect MyFirstNetwork
[
    {
        "Name": "MyFirstNetwork",
        "Id": "b2c78b7b2ec68ef1fe702be331aa9cd3f3e1733039b8a064c702652439e1b20b",
        "Created": "2017-04-24T08:51:55.089418851+02:00",
        "Scope": "local",
        "Driver": "bridge",
        "EnableIPv6": false,
        "IPAM": {
            "Driver": "default",
            "Options": {},
            "Config": [
                {
                    "Subnet": "172.18.0.0/16",
                    "Gateway": "172.18.0.1"
                }
            ]
        },
        "Internal": false,
        "Attachable": false,
        "Containers": {},
        "Options": {},
        "Labels": {}
    }
]
```

There are quite a lot of information here, and we will come back to it a bit later in this article, when I have introduced the important notions.  
You can already recognized the `Name` of the network as you defined it on the `docker network create` command, the `Id`, which is the hash docker returned when you created the network. If ever that matters to you, you also have the date at which the network was created.

On simple thing we can talk about right now, is that, as you can see, by default, IPv6 is not enabled in docker networks. This means that ---once we learned how to do it--- when we have containers using this network, they can only talk with IPv4. To enable IPv6 in a network, you need to add the `--ipv6` option when running `docker network create`. Like this: `docker network create --ipv6 <network-name>`.

The next logical thing we can do is remove the network, this is done with `docker network rm`. So let's try it:

```bash
$> docker network rm MyFirstNetwork
MyFirstNetwork
```

When a removal is successful, docker prints back the name of the network you've just removed. You can confirm that the network doesn't exist anymore with `docker network ls`.

Okay, so we've got the basic commands, now it's time to let the fun begins!

#### The Principle Behind Docker Networks

Up until now, when we have created containers, we were in a situation similar to this:

![**Fig 1**: containers exist separately, no communication between them](/images/01-separate-containers.svg "Separate, independent containers")

Now, we want to sort of "group" containers together, to allow them to communicate and see each other. We are going to use Docker Networks to do this. For the vocabulary part, know that we say `connect` a container to/in a Docker Network (and obviously the converse is `disconnect`).  
I know you are probably eager to learn more about Docker Networks and start playing _right away_, but there's a reason I'm going slow: as I said in the very first Part of this series, this is typically an area where people tend to read fast (because that's new an exciting), think they understand, skip on sentences and they find themselves with a vague notion of understanding but without the gotchas and nuances.

So here, make sure you don't mix `connect` which is when a container connects to a Docker Network (so between a container and a Docker Network) and `link` which is the old-fashioned way, and which happened _between two containers_.

When we `connect` a container to a Docker Network it becomes part of this Docker Network's network stack and the containers gains a new (virtual) network interface. As far as the container is concerned, everything happened as if you had installed a new network card and connected this new network card's Ethernet connector.

One container connected to a Docker Network in itself is not very interesting: there's not much added behavior. The fun begins when you connect _a second_ container to the Docker Network. And that is also where you begin to see the advantage of Docker Networks over the old-fashioned `--link`: it's _much more_ flexible: you have a Docker Network on a side, you have several containers on the other side, and you can dynamically connect and disconnect some (or all) of the containers to the Docker Network. And all of this, without affecting the others containers!

And what is even more powerful is that you can connect one container to several Docker Networks! The container will simply see a new (virtual) network interface. Exactly as the processes running on your computer would see a new network interface if you (physically) connected a new network card to your computer.

#### Practise Time!
That's a lot to digest, I know, so we're going to illustrate all of this.

Let's start from scratch: no containers, no Docker Networks, nothing. Blank canvas.

Now we create one Docker Network, called `CustomNetwork`:

```bash
$> docker network create CustomNetwork
9aedc6a27184d02d488c79f85eb185e6e2bf841627b03449f42d110c2f7eb4f3
```

Let's make sure it is created:

```bash
$> docker network ls

NETWORK ID          NAME                DRIVER              SCOPE
9aedc6a27184        CustomNetwork       bridge              local
9dbd76505eed        bridge              bridge              local
2683c3824f8f        host                host                local
a84e731c4387        none                null                local
```

Don't worry if you have others (in particular the `MyFirstNetwork` we created earlier), it won't be a problem. All that matters is that we have our `CustomNetwork` network.

Now, we are going to create three containers that we will simply call `one`, `two` and `three` to keep things simple.

```bash
$> docker run --rm -itd --name one nginx sh
2584a55374de2b52050ee553d27f2905369ca47b87fcff6681b22e664ba2635c

$> docker run --rm -itd --name two nginx sh
461e4f83431a043be8089e46c0db6854efcbc6e7a7e278392ad21b8590ac7c32

$> docker run --rm -itd --name three nginx sh
2af69abd1dea705d51246b7c11f27877abec6cf35386bc0aa51491c38cb33009
```

(Make sure to keep the 3 terminals open, because we used option `--rm` to remove the containers once they exit).

Let's check (for sanity's sake) that we do have our containers:

```bash
$> docker ps --format "{{.Names}}"
three
two
one
```

Okay, perfect.

So basically, we are in this situation:

![**Fig 2**: Our "CustomNetwork" Docker Network and the three containers "one", "two" and "three"](/images/02-net-cont-separated.svg "The newly-created Docker Networks and the three containers")

Remember what I told you about the story of container gaining a new (virtual) network interface? Well we're going to check this.

Let's enter container `one` and check its network interfaces, as a reminder, you enter the container with `docker exec -it <container-name> bash` (or replace with `sh` is `bash` is not present).

Once inside, we will list network interfaces with `ip link show` (of the caveman can use `ifconfig` if they want ^^):

```bash
$> ip link show
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN mode DEFAULT group default
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
6: eth0@if7: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP mode DEFAULT group default
    link/ether 02:42:ac:11:00:02 brd ff:ff:ff:ff:ff:ff
```

I will leave you verify that the same command run in containers `two` and `three` will yield similar results. So what we see here is that we have two network interfaces: `lo` which corresponds to the local loop (localhost) and `eth0`, which... you should consider as the "default network interface" for now.  
I will explain the truth about it a bit later in the article, for the moment, just admit that the container has a default network interface.

Okay, so you can exit this terminal (again: make sure to keep the first three terminals opened, not to terminate the containers).

Right now, we have one Docker Networks (`CustomNetwork`) and three containers, totally unrelated. It's time to unleash the power of Docker Networks!  
We are now going to `connect` container `one` to the `Custom Network`.

This is done with the `docker network connect` subcommand. This subcommand takes the name of the network and the name of the container to connect.  
A small warning on this command, some people find the order of the arguments weird (I don't), but it's the network name **first**, and _then_ the container's name.

```bash
$> docker network connect CustomNetwork one
```

Tadaaaaaa!  

...


...

Okay there was neither smoke nor flashy lights, but we have just done something interesting. Granted, docker doesn't provide you with a lot of input, but `docker network` commands are part of this family of "shy" commands: when they don't tell you anything, it means it went all right. They start talking where some bad things happened.

So we still need some confirmation that what we've just done was actually useful.

We can start by inspecting the network and confirm that the container is now connected to it:

```bash
$> docker network inspect CustomNetwork
[
    {
        "Name": "CustomNetwork",
        "Id": "9aedc6a27184d02d488c79f85eb185e6e2bf841627b03449f42d110c2f7eb4f3",
        "Created": "2017-04-24T19:00:25.165477676+02:00",
        "Scope": "local",
        "Driver": "bridge",
        "EnableIPv6": false,
        "IPAM": {
            "Driver": "default",
            "Options": {},
            "Config": [
                {
                    "Subnet": "172.19.0.0/16",
                    "Gateway": "172.19.0.1"
                }
            ]
        },
        "Internal": false,
        "Attachable": false,
        "Containers": {
            "2deaab87b3f99c40c245c8e77480656d93cd14c55fa34229e6c062fa74d8e035": {
                "Name": "one",
                "EndpointID": "c298e545a4b68da5771be1504b3d0a5f2435afda425b3be3d47d0ff7d8f6582a",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            }
        },
        "Options": {},
        "Labels": {}
    }
]
```

The interesting part is the `Containers`:

```bash
"Containers": {
            "2deaab87b3f99c40c245c8e77480656d93cd14c55fa34229e6c062fa74d8e035": {
                "Name": "one",
                "EndpointID": "c298e545a4b68da5771be1504b3d0a5f2435afda425b3be3d47d0ff7d8f6582a",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            }
        }
```

This section list the container that are currently connected to the Docker Network we introspected, and we see that a container with `"Name": "one"` is connected, so that's a first indication that it worked!

We can also re-enter the container, and check its network interfaces: it should have gained a new one, as we mentionned before. Let's check this now:

```bash
$> docker exec one ip link show
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN mode DEFAULT group default
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
6: eth0@if7: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP mode DEFAULT group default
    link/ether 02:42:ac:11:00:02 brd ff:ff:ff:ff:ff:ff
12: eth1@if13: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP mode DEFAULT group default
    link/ether 02:42:ac:13:00:02 brd ff:ff:ff:ff:ff:ff
```

Two comments here:

1. usually, we use `docker exec -it one bash` and then we get a shell prompt and type our command(s) (here `ip link show`).  
What you must understand is that `docker exec` is a command to `exec`ute a command inside a container: so you supply the container's name and the command to execute. What we usually do is execute the command `bash`, and since it needs a tty, we use option `-t` and since we need interaction, we use option `-i` (which, concatenated gives `-it`).  
But `docker exec` is really meant to execute whatever command you want, so this time, I took a shortcut and directly ran the command I was interested in, _i.e._ `ip link show`. Since this time it doesn't need a tty and it doesn't need interaction, I ommited the options `-i` and `-o`.  
So really, this should not confuse you; from now on, I'm going to use either interchangeably!
2. back to our Docker Network topic: we can see here that we now have three network interfaces: `lo` and `eth0` which we already had, and the new `eth1`. As expected, the container now sees a new network interface, because as far as it is concerned, we have just added a new network interface.

At a _last_ confirmation (actually I'm doing all of these steps to give you _closure_: by showing you all the components on what is involved, I am trying to show you that everything is logic and there is absolutely no "black, magic box"), we are going to get the container's IP address.

What we have just done is connect our `one` container inside a Docker Network: `CustomNetwork`. Since it joined a network, it must have an IP address inside this network. I'm going to show you two ways to get it, which should confirm what is going one.

The first way is to get the container's IP address by inspecting the Docker Network. If you look closely at the `Containers` section from earlier:

```bash
"Containers": {
            "2deaab87b3f99c40c245c8e77480656d93cd14c55fa34229e6c062fa74d8e035": {
                "Name": "one",
                "EndpointID": "c298e545a4b68da5771be1504b3d0a5f2435afda425b3be3d47d0ff7d8f6582a",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            }
        }
```

We can see that for the container `one`, there is a `IPv4Address`, whose value is `172.19.0.2/16` (the value is likely to be different in your case, but it will still probably be `172.x.y.z`, and _probably_ `z` will be `2`. But this is not mandatory.)  
So it means than inside this Docker Network, this container (`one`) has IP address `172.19.0.2` The `/16` at the end is simply here to tell you that the subnetwork is a `/16`. I'm not here to make a network course, but basically, it means the first `16` bits of the IP address are fixed for this network, so the last `32 - 16 = 16` bits are free to be assigned, so it means you can connect `2^16 = 65,536` containers to this network. Anyway, this is out of the scope of this article, feel free to document on this yourself.

Let's not diverge: we just learned that our container `one` had IP address `172.19.0.2` inside the Docker Network.

To complete the loop, we will double check the container's IP address. Do you have an idea _where_ we could check this?

As usually, take a break and try to answer this. I'll be waiting (don't cheat: it's very important that you take some time to make your brain think about this).

...

...

Don't be tempted to fast forward! Make you brain tick and find that answer!

...

...

Okay so you should have found out now, or at least you should have tried. (If you did not find, it's not a big problem, but it probably means you have read too fast and did not understand every step perfectly, so scroll back up, take some sugar and go at it again).

The second place where we are going to check for the container's IP address, is by inspecting its network interfaces, like we just did earlier. As we have seen just a couple of screens up is that our container gained a new network interface, so now we're just going to check the IP address of the network interfaces, and it should match what's inside the `docker network inspect` outout.

Here is it:

```bash
$> docker exec one ip addr show                                                                   ~
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
14: eth0@if15: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:11:00:02 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.2/16 scope global eth0
       valid_lft forever preferred_lft forever
20: eth1@if21: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:13:00:02 brd ff:ff:ff:ff:ff:ff
    inet 172.19.0.2/16 scope global eth1
       valid_lft forever preferred_lft forever
```

Annnnnd the loop is closed: we can see that the IP address, as seem from inside the container is indeed `172.19.0.2`, the same `docker network inspect` gives us.

##### Now What?

Yeah, granted, we've seen quite a log, but it still lack the magical effect, with smoke, pyrotechnic effects and all that.  
We'll fix this now.

Why isn't it all magical right now? It's because what we have done, albeit cool, is rather limited: we have _just_ connected a container (`one`) to a Docker Network (`CustomNetwork`).

So, basically now we're here:

![**Fig 3**: Container "one" is now inside (connected to) Docker Network "CustomNetwork". Containers "two" and "three" are not.](/images/03-one-in-network.svg "Container 'one' is inside the network, 'two' and 'three' are outside")

So this is cool because we learned a lot, but it's limited, because it's like having only one computer in a network: you're not going to have much fun.

The fun part comes now: we're going to connect _another_ container (`two`), inside the same Docker Network. And _that_ will be something!

Let's do it right now: we've just seen how to do it, and it's really fast & simple: `docker network connect CustomNetwork two`.

Done. See? That was quick, right?

I told you that once you undertood Docker Networks, it would be very fast to use, and very powerful.

So what changed? We're now in this situation:

![**Fig 4**: Container "two" joined "one" in the Docker Network. "Three" is still outside.](/images/04-one-n-two-in-network.svg "'Two' joined 'one' inside the network")

We can make a quick check by looking at the `Containers`'s section of the `docker networ inspect` command:

```bash
$> docker network inspect CustomNetwork
[...]
"Containers": {
            "2deaab87b3f99c40c245c8e77480656d93cd14c55fa34229e6c062fa74d8e035": {
                "Name": "one",
                "EndpointID": "b090281848529d2d472f5e73350edf713d02e0480d33f006d21806ce0dbba8f3",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            },
            "77e3e44ee3cc8273e9ff30300aedc46c4dfec3e167f3cd34ea4ae81e6d56fa51": {
                "Name": "two",
                "EndpointID": "0bca67b6796de28a5666beb647e199b81b88ecbf02f6826b5f58ba99662f0a9a",
                "MacAddress": "02:42:ac:13:00:03",
                "IPv4Address": "172.19.0.3/16",
                "IPv6Address": ""
            }
        }
[...]
```

Unsurprisingly, we now have `one` **and** `two` inside this Docker Network, and you even see that `two` got IP `172.19.0.3`.

It's time for the smoke and confetti, part 1!

Let's enter container `one`: `docker exec -it one bash`.

And now.... we're going to reach to container `two`, remember how we just learned its IP address inside the Docker Network? Then let's use it!

```bash
$> ping 172.19.0.3 -c 4
PING 172.19.0.3 (172.19.0.3): 56 data bytes
64 bytes from 172.19.0.3: icmp_seq=0 ttl=64 time=0.063 ms
64 bytes from 172.19.0.3: icmp_seq=1 ttl=64 time=0.054 ms
64 bytes from 172.19.0.3: icmp_seq=2 ttl=64 time=0.056 ms
64 bytes from 172.19.0.3: icmp_seq=3 ttl=64 time=0.055 ms
--- 172.19.0.3 ping statistics ---
4 packets transmitted, 4 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.054/0.057/0.063/0.000 ms
```

Hey! Did you see that?! We have made our first contact between two containers!

Let's confirm the awesomeness of this by _disconnecting_ container `two` from the Docker Network: `docker network disconnect CustomNetwork two`. Ho by the way it shows you the command used to disconnect a container from a Docker Network (as usual, the order is first the network name (or id) and then the container's).

A quick confirmation with `docker networ inspect CustomNetwork` confirms that `two` is not part of the Docker Network anymore:

```bash
$> docker network inspect CustomNetwork
[...]

"Containers": {
            "2deaab87b3f99c40c245c8e77480656d93cd14c55fa34229e6c062fa74d8e035": {
                "Name": "one",
                "EndpointID": "476b9ea0975e728b862874457bd42f2b702687f8387c9eacdb0c73ea4e3b79ce",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            }
        }
[...]
```

And now, if we re-try our `ping` command again (from inside container `one`):

```bash
$> ping 172.19.0.3 -c 4
PING 172.19.0.3 (172.19.0.3): 56 data bytes
92 bytes from 2deaab87b3f9 (172.19.0.2): Destination Host Unreachable
92 bytes from 2deaab87b3f9 (172.19.0.2): Destination Host Unreachable
92 bytes from 2deaab87b3f9 (172.19.0.2): Destination Host Unreachable
92 bytes from 2deaab87b3f9 (172.19.0.2): Destination Host Unreachable
--- 172.19.0.3 ping statistics ---
4 packets transmitted, 0 packets received, 100% packet loss
```

It just doesn't work anymore.

We have to take a tiny break here and look back at what we have accomplished. Because I was not kidding with the smoke and the confetti: what we have done is _really_ powerful.

- We started with three separated containers: `one`, `two` and `three`.
- We created a Docker Network `CustomNetwork`
- Then we connected containers `one` and `two` inside this Docker Network _without changing **anything** else_.
- And thanks to this, we were able to make `one` and `two` communicate with each other, while still being isolated from the rest of the world (noticed how `three` was not involved here?)

This **is** powerful: with that, imagine what you can do: you can link containers running your webapp with a container running `nginx`, with a container running `postgresql`, with a container running `redis`, etc.

In other words, we have opened one of the last doors to building complex stacks. (In these articles, until I mention otherwise, by "stack" I mean a group of container, possibly interacting with each others through Docker Network(s), with some Docker Volumes).

I'd like you to take a small break (not one of those long break, where I show a cup of coffee icon, just a step back to appreciate the complexity of what we are doing) and enjoy (also make _sure_ you understood what we have just done).

This _may_ look simple to you right now, but it's:
1. because I (hopefully) explain it clearly (at least that's what I'm trying to do)
2. because the Docker developers made a wonderful job of giving up a very nice API, simple to use ; they abstracted away all the complex notions, etc.


Now that you have appreciated this, let's see how modular, quick and easy this setup is. Remember our friend `three` which was left alone up to now?  
Well it's time to connect it to our network and see that Docker Networks is **much** more powerful than the old & deprecated `--link` feature (for those who used it ; for the others, forget this last sentence).

Let's connect `two` and three in the Docker Network: `docker network connect CustomNetwork three` and `docker network connect CustomNetwork two`.

A quick check:

```bash
$> docker network inspect CustomNetwork
"Containers": {
            "2deaab87b3f99c40c245c8e77480656d93cd14c55fa34229e6c062fa74d8e035": {
                "Name": "one",
                "EndpointID": "476b9ea0975e728b862874457bd42f2b702687f8387c9eacdb0c73ea4e3b79ce",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            },
            "51e27a7cc7ba107183a4c42ba8d9d11f92f554c6e0314e2ad165b77cc51be507": {
                "Name": "three",
                "EndpointID": "8b3c8caf6a421b3512fc1ad89a4629171a5fd093a2724004bee345084e26e5a8",
                "MacAddress": "02:42:ac:13:00:03",
                "IPv4Address": "172.19.0.3/16",
                "IPv6Address": ""
            },
            "77e3e44ee3cc8273e9ff30300aedc46c4dfec3e167f3cd34ea4ae81e6d56fa51": {
                "Name": "two",
                "EndpointID": "ee9ee79d8c1a96e359700252efb66f89ebff3faf5f71afa4becee69e4f8d0254",
                "MacAddress": "02:42:ac:13:00:04",
                "IPv4Address": "172.19.0.4/16",
                "IPv6Address": ""
            }
        }
```

So we do have all three containers in our Docker Network. And you can enter any of them and ping any of the other's IP address and that will work. Disconnect any of the container from the network, and it will simply xease to be reachable by the others.

Isn't that all wondeful?

...

What? What do I hear?  
Yes I can see you in the back... what's the problem?

...

You've noticed that container `two` has now IP address `172.19.0.4` and not `172.19.0.3` like it has before we disconnected / reconnected it?  
So what, you want a medal?

Just kidding! Very nice catch actually.

As you may have guessed, Docker Networks tend to address IP addresses in an increasing number, in the order containers are connected. This is a trend, but _please_ you should **not** rely on this (again: it's not documented per see).

While it can be fine for "static" configurations, it might become a very big problems for applications that are more dynamic: what if containers can be created on the fly and join the Docker Network, then maybe become disconnected, etc.?

This is a problem.

So we need a mechanism to discover a container's IP address inside the network, etc. This seems complex.

Actually, as it's the case for a lot of things, the Docker devs have thought about this, and it's time for smoke and confetti part 2!

I'm going to say (or rather write) a sentence that will sound complex and might make you think "okay, I reached my limit with Docker, let's give up", but please don't, you'll see how ridiculously easy this actually is. Okay here it is:

> In each custom Docker Network, Docker provides an embedded, internal DNS server that allows any container to resolve the other container's IP addresses based on their container's name

Nobody moves! Stay here!

Seriously, this sentence above means exactly this: "inside a Docker Network, you can reach a container by its name, rather by its IP".

Ah?! Easy or not?! Didn't I tell you?

So picture this: we have been `ping`-ing IP addresses like crazy for half an hour, using `docker network inspect` to find the container's IP address while there was a nice, and reliable feature that was awaiting!

It's time we tried this, no?

Make sure your three containers are connected to `CustomNetwork`, enter container `two` and try pinging `two` and `three`:

```bash
$> ping one -c 4
PING one (172.19.0.2): 56 data bytes
64 bytes from 172.19.0.2: icmp_seq=0 ttl=64 time=0.070 ms
64 bytes from 172.19.0.2: icmp_seq=1 ttl=64 time=0.057 ms
64 bytes from 172.19.0.2: icmp_seq=2 ttl=64 time=0.058 ms
64 bytes from 172.19.0.2: icmp_seq=3 ttl=64 time=0.056 ms
--- one ping statistics ---
4 packets transmitted, 4 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.056/0.060/0.070/0.000 ms

$> ping three -c 4
PING three (172.19.0.3): 56 data bytes
64 bytes from 172.19.0.3: icmp_seq=0 ttl=64 time=0.085 ms
64 bytes from 172.19.0.3: icmp_seq=1 ttl=64 time=0.057 ms
64 bytes from 172.19.0.3: icmp_seq=2 ttl=64 time=0.058 ms
64 bytes from 172.19.0.3: icmp_seq=3 ttl=64 time=0.056 ms
--- three ping statistics ---
4 packets transmitted, 4 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.056/0.064/0.085/0.000 ms
```

Ah ah! Seriously, look me into the eyes and tell me: how awesome is that? As you can see here, when we pinged `one`, it resolved to `172.19.0.2`, and when we pinged `three`, it resolved to `172.19.0.3`.

Let's make sure it works: disconnect `one` and `three`: `docker network disconnect CustomNetwork one && docker network disconnect CustomNetwork three` and reconnect them, in the other reverse order (to change their IP address); `docker network connect CustomNetwork three && docker network connect CustomNetwork one`.

Let's check that hteir IP addresses have been changed:

```bash
$> docker nework inspect CustomNetwork
[...]
"Containers": {
            "2deaab87b3f99c40c245c8e77480656d93cd14c55fa34229e6c062fa74d8e035": {
                "Name": "one",
                "EndpointID": "da6c82447d34726b9e08977ea13a31411aadb238a08cd7b0fec7b293c9007c02",
                "MacAddress": "02:42:ac:13:00:03",
                "IPv4Address": "172.19.0.3/16",
                "IPv6Address": ""
            },
            "51e27a7cc7ba107183a4c42ba8d9d11f92f554c6e0314e2ad165b77cc51be507": {
                "Name": "three",
                "EndpointID": "95c3acb0ebecb52de368bea7ecb0a57f45ed0ec02606f00a7629765671fff77e",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            },
            "77e3e44ee3cc8273e9ff30300aedc46c4dfec3e167f3cd34ea4ae81e6d56fa51": {
                "Name": "two",
                "EndpointID": "ee9ee79d8c1a96e359700252efb66f89ebff3faf5f71afa4becee69e4f8d0254",
                "MacAddress": "02:42:ac:13:00:04",
                "IPv4Address": "172.19.0.4/16",
                "IPv6Address": ""
            }
        }
[...]
```

Yup. So enter `two` again, and ping the container again:


```bash
root@77e3e44ee3cc:/# ping -c 4 one  
PING one (172.19.0.3): 56 data bytes
64 bytes from 172.19.0.3: icmp_seq=0 ttl=64 time=0.048 ms
64 bytes from 172.19.0.3: icmp_seq=1 ttl=64 time=0.052 ms
64 bytes from 172.19.0.3: icmp_seq=2 ttl=64 time=0.053 ms
64 bytes from 172.19.0.3: icmp_seq=3 ttl=64 time=0.053 ms
--- one ping statistics ---
4 packets transmitted, 4 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.048/0.051/0.053/0.000 ms

root@77e3e44ee3cc:/# ping -c 4 three
PING three (172.19.0.2): 56 data bytes
64 bytes from 172.19.0.2: icmp_seq=0 ttl=64 time=0.052 ms
64 bytes from 172.19.0.2: icmp_seq=1 ttl=64 time=0.058 ms
64 bytes from 172.19.0.2: icmp_seq=2 ttl=64 time=0.057 ms
64 bytes from 172.19.0.2: icmp_seq=3 ttl=64 time=0.055 ms
--- three ping statistics ---
4 packets transmitted, 4 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.052/0.056/0.058/0.000 ms
```

Annnnd it works! Wonderful.  
So we now have a _very_ handy tool: you don't ever have to know / find a container's IP address, because you can simply use its name. Isnt' that awesome? Seriously?

Let's disconnect `three` now with `docker network disconnect CustomNetwork three` and try to ping it again:

```bash
root@77e3e44ee3cc:/# ping -c 4 three
ping: unknown host
```

As soon as the container disconnects from the Docker Network, it is not reachable anymore.

##### A Quick Gotcha

Before we make a nice little recap of all the cool stuff we have just seen, I wanted to show you a small gotcha without which, I am sure, some of you were bound to bang their head against the wall until one of the two breaks (and from experience, it's often the head!). The very last thing we have just discovered is that inside a Docker Network, you don't need to know a container's IP address to talk to it, just its name.  
This is why we can do `ping three` and it automatically resolves to the correct IP address.

This is all good and all, but just know that it only works with _explicitly-named_ containers. In other words, it doesn't work with containers that are named automatically by docker, let's see this in effect:

```bash
$> docker run --rm -itd nginx bash

$> docker ps --format "{{.Names}}"
peaceful_lovelace
one
two
three
```

Above we have created a new container without naming it, then we have used `docker ps` to list container (and a nice little Go template to only display the names, because `docker ps` is very verbose.) Obviously, your container will most likely have another name, just replace accordingly (it'd be fun if _just for this example_ your container is _also_ named like this!).

Then it's time to connect this container to our Docker Network with `docker network connect CustomNetwork peaceful_lovelace` (tip: you can use Tab-compltetion for the commands and the container names).

Then let's enter container `one` for instance and ping this new container:

```bash
$> docker exec -it  one bash

root@2deaab87b3f9:/# ping -c 4 peaceful_lovelace
ping: unknown host
```

On ma machine it takes about 30 seconds, but it eventually times out.

All of this to say that this is weird, but it's the rule (at least, for now): this automatic container name resolution only works with explicitly-named containers. Just remember it before thinking your Docker Networks has is broken.


#### Time For a Recap!

Phew!  
That was a long one, right?! I'm not talking about the article: we're about halfway through it! I'm talking about this last part. This was quite the beast.

We have learned a lot, so it's time to make a recap, because this stuff is _very_ important (seriously, Docker Networks is absolutely necessary if you want to make complex stacks and want to become a tiny bit advanced with Docker).

So what have we learned?

At the start of this article, we knew quite a lot about containers, and especially that they are isolated: containers are processes on the host, that live in different namespaces and they cannot interact with each other. Which is very good because that's what we are looking for with Docker, but can also be a problem when you start building stacks that require communication between two (or more) containers.

One obvious way to "solve" this problem would have been to run all the applications that need to communicate in the same container. But that would be going against Docker's principles if the processes are not intimately linked, and this is obviously not something we can use for large and complex stacks.

We briefly talked about a legacy way, `--link` which is a rough way to directly connect two containers. It was the only solution at some point in time, but now it should be banned.

Then we saw _the_ solution: `Docker Networks`. They are as easy to create as containers (`docker network create` and `docker network rm`) and they provide networking features for our containers. A Docker Network is totally separate from a container, which means that you can "hot-connect" an existing, running container inside a Docker Network (_i.e._ connect the container without stoppping or destroying it).  
When a container gets connected to a Docker Network, it gains a new network interface, because as far as this container is concerned, it _as if_ its host system just had a new network interface installed and joined a new local network (like a LAN).  
The container thus gains a new IP address (its IP address inside the Docker Network) that can be used to communicate with it, by the others containers in the same Docker Network.

The very handy thing about Docker Networks is that they are a separate entity (from the containers). It's something that you create an dhandle separately. And you can, at some point (_i.e._ not necessarily at the time you run your container) you can decide to connect your container to a Docker Network (it then becomes reachable by the other containers inside this Docker Network), and whenever you want, you can disconnect it from the Docker Network (thus becoming unreachable).

A nice little feature is the embedded DNS server that allows containers to talk to each other through their container's name instead of going through the process of discovering the target container's IP address. Remember, though, that it works only with explicitly-named containers.

### Slightly More Advanced Use-Cases

#### A Nice Shortcut
Now that all of this is slowly sinking in, let me show you one small option that should make you save a little time. Until now we have instantiated a container with `docker run`, we usually gave it a name with the `--name` option, and _then_ we connected the container to our Docker Network with `docker network connect <network> <container>`.

This is fine and works. But just for the sake of mentionning it, know that it is possible to _directly_ connect the container to a Docker Network thanks to the `docker run` command. This way you can do it all in one command.  
This doesn't change anything: the behavior is exactly the same, and if you find this confusing, feel free to forget about this. But if you're interested, the option to add is `--net`.

So if you want to connect your container to a Docker Network named `CustomNetwork` directly on container creation, you can do something like: `docker run -it --name my-container-name --net CustomNetwork <image-name>` and voilà: when your container is created, it is already connected to `CustomNetwork`.  
Obviously if at some point you want to disconnect it from the Docker Network, `docker network disconnect` is your friend.

Anyway, that was the easy part, I have something else to tell and show you, are you ready? This is going to be a good one.

#### Add Some More Fun!

I don't know of a better way to tell you, so I'll just blurt it out:

> You can connect a container to **several** Docker Networks **at the same time**!

Yes, you read that correctly. Remember how we said that when we connect a container to Docker Network, it gains a new network interface, because for the processes running inside the container, it is as if the machine had a new network card and just joined a new LAN network? Well what prevents you from installing _a second_ network card and connect to _another_ LAN then?  
Nothing. And this is also possible with Docker.

Until now, we had situations like this:
![**Fig 5**: Simple configuration with several containers in the same Docker Network](/images/05-simple-configuration.svg "Simple configuration: containers in one Docker Network")

Now we will do something like this:

![**Fig 6**: Complex configuration where "Container C" is connected to **two** Docker Networks simultaneously](/images/06-complex-multi-network.svg "Complex configuration: containers connected to several Docker Networks")

And we will see that it works perfectly as intended.

Let's create two Docker Networks:

```bash
$> docker network create Network1
83b7a5902813cf9891c6be314ff4ffe2ab85e3a70f1f28b203d0fb4c21a4064d

$> docker network create Network2
721f118ce184e90ca070db8b751ff15cc7fbdc9d4fff3764b4b4c9bc7e66e82e
```

And let's now create three containers:

```bash
$> docker run -itd --name ContainerA nginx
0407e1bad71112464bfc1d3cafa284549c2504400482a5103998352b5eb6aa07

$> docker run -itd --name ContainerB nginx
c12bfd2e1842404f1c66b031b8d79225375740aa6daed0123287c5f355804417

$> docker run -itd --name ContainerB nginx
f18c6c868e9a3b283b59d0b41df8696eadf9bebfd3da405117bd4313b48ccf67
```

Now, we will connect `ContainerA` to `Network1` and `ContainerB` to `Network2`, like we already did several times:

```bash
$> docker network connect Network1 ContainerA
$> docker network connect Network2 ContainerB
```

Nothing new here. The fun part comes from `ContainerC`. We want to connect it to both `Network1` and `Network2`. Well, let's do this. First, let's connect it to `Network1`: `docker network connect Network1 ContainerC`.

We can see that now both `ContainerA` and `ContainerC` are connected to `Network1`:

```bash
$> docker network inspect Network1
[...]
"Containers": {
            "0407e1bad71112464bfc1d3cafa284549c2504400482a5103998352b5eb6aa07": {
                "Name": "ContainerA",
                "EndpointID": "34ba3ffd2e059b4e9135aaf818b2946d33cf02fcc307d4fa500c88c42baf39cd",
                "MacAddress": "02:42:ac:14:00:02",
                "IPv4Address": "172.20.0.2/16",
                "IPv6Address": ""
            },
            "f18c6c868e9a3b283b59d0b41df8696eadf9bebfd3da405117bd4313b48ccf67": {
                "Name": "ContainerC",
                "EndpointID": "c75608e678849752975e1f0a64ff3b229f6964ac162fe84aba8854d285eef876",
                "MacAddress": "02:42:ac:14:00:03",
                "IPv4Address": "172.20.0.3/16",
                "IPv6Address": ""
            }
        },
        [...]
```

And they can reach each other:

```bash
$> docker exec ContainerA ping -c 3 ContainerC                                                    ~
PING ContainerC (172.20.0.3): 56 data bytes
64 bytes from 172.20.0.3: icmp_seq=0 ttl=64 time=0.073 ms
64 bytes from 172.20.0.3: icmp_seq=1 ttl=64 time=0.058 ms
64 bytes from 172.20.0.3: icmp_seq=2 ttl=64 time=0.059 ms
--- ContainerC ping statistics ---
3 packets transmitted, 3 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.058/0.063/0.073/0.000 ms
```

(Here, instead of first "entering" `ContainerA` and then run a `ping` command to ping `ContainerC`, I use `docker exec` to execute a command inside the container, this command being `ping -c 3 ContainerC`. This saves us a roundtrip ot "entering the container", making our call, exiting the container. But it's equivalent.)

Just to be extra sure, let's check that `ContainerC` is not in `Network2`:

```bash
$> docker network inspect Network2
[...]
"Containers": {
            "c12bfd2e1842404f1c66b031b8d79225375740aa6daed0123287c5f355804417": {
                "Name": "ContainerB",
                "EndpointID": "ca2a694e9acc36ffd6ab0bb71a7a4830c4c9f26a40411cd7f0a4523424b4d58a",
                "MacAddress": "02:42:ac:15:00:02",
                "IPv4Address": "172.21.0.2/16",
                "IPv6Address": ""
            }
        },
[...]
```

Perfect. All is normal at this point.

Last check before doing anything else (and to really follow what we are doing), let's check `ContainerC`'s network interfaces:

```bash
$> docker exec ContainerC ip addr show
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
12: eth0@if13: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:11:00:04 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.4/16 scope global eth0
       valid_lft forever preferred_lft forever
16: eth1@if17: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:14:00:03 brd ff:ff:ff:ff:ff:ff
    inet 172.20.0.3/16 scope global eth1
       valid_lft forever preferred_lft forever
```

So we see the usual `lo` loopback interface and the usual `eth0` interface that I briefly mentionned (remember: for now consider that when you create a container, it starts with two network interfaces: `lo` and `eth0`).  
And we can see `eth1` which corresponds to the network interface this container has in the Docker Network `Network1` (check the IP addresses, you'll see they match!)

Okay, perfect! It's time to connect `ContainerC` to `Network2` now: `docker network connect Network2 ContainerC`.

Same deal: let's check everything. First, the network:

```bash
$> docker network inspect Network2
[...]
"Containers": {
            "c12bfd2e1842404f1c66b031b8d79225375740aa6daed0123287c5f355804417": {
                "Name": "ContainerB",
                "EndpointID": "ca2a694e9acc36ffd6ab0bb71a7a4830c4c9f26a40411cd7f0a4523424b4d58a",
                "MacAddress": "02:42:ac:15:00:02",
                "IPv4Address": "172.21.0.2/16",
                "IPv6Address": ""
            },
            "f18c6c868e9a3b283b59d0b41df8696eadf9bebfd3da405117bd4313b48ccf67": {
                "Name": "ContainerC",
                "EndpointID": "572f8e062bf7ed9b0c9c9679c18076376c3e0b2c57567cbcf4e27b2cd08ffcc7",
                "MacAddress": "02:42:ac:15:00:03",
                "IPv4Address": "172.21.0.3/16",
                "IPv6Address": ""
            }
        },
[...]
```

Hey! so it seems it works the same: we previously had only `ContainerB`, but now, we also have `ContainerC`.

Let's check that `ContainerB` and `ContainerC` can talk to each other:

```bash
$> docker exec ContainerB ping -c 3 ContainerC
PING ContainerC (172.21.0.3): 56 data bytes
64 bytes from 172.21.0.3: icmp_seq=0 ttl=64 time=0.074 ms
64 bytes from 172.21.0.3: icmp_seq=1 ttl=64 time=0.053 ms
64 bytes from 172.21.0.3: icmp_seq=2 ttl=64 time=0.056 ms
--- ContainerC ping statistics ---
3 packets transmitted, 3 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.053/0.061/0.074/0.000 ms
```

Of course that works: they are part of the same Docker Network!

If everything is coherent, `ContainerC` _must_ have gained a new network interface: the one connect it to `Network2`:

```bash
$> docker exec ContainerC ip addr show
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
12: eth0@if13: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:11:00:04 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.4/16 scope global eth0
       valid_lft forever preferred_lft forever
16: eth1@if17: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:14:00:03 brd ff:ff:ff:ff:ff:ff
    inet 172.20.0.3/16 scope global eth1
       valid_lft forever preferred_lft forever
20: eth2@if21: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:15:00:03 brd ff:ff:ff:ff:ff:ff
    inet 172.21.0.3/16 scope global eth2
       valid_lft forever preferred_lft forever
```

Yes sir! It did alright!  
As you can see, `ContainerC` now has `eth2`, with the IP that matches the one in the output of when we inspected the Docker Network.

Everything seems fine, we should be in the situation depicted in **Fig. 6** now, two checks are left to be made:

1. check that `ContainerC`, being in both `Network1` and `Network2` can still communicate with `ContainerA` (via `Network1`) and `ContainerB` (via `Network2`).
2. check that `ContainerA` and `ContainerB` **cannot** communicate, because they are **not** part of the same Docker Network, which would confirm that just because they both know a container (`ContainerC`) than is in both Networks, nothing is leaked out.

Okay, check #1:  

```bash
$> docker exec ContainerC ping -c 3 ContainerA
PING ContainerA (172.20.0.2): 56 data bytes
64 bytes from 172.20.0.2: icmp_seq=0 ttl=64 time=0.051 ms
64 bytes from 172.20.0.2: icmp_seq=1 ttl=64 time=0.056 ms
64 bytes from 172.20.0.2: icmp_seq=2 ttl=64 time=0.068 ms
--- ContainerA ping statistics ---
3 packets transmitted, 3 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.051/0.058/0.068/0.000 ms

$> docker exec ContainerC ping -c 3 ContainerB
PING ContainerB (172.21.0.2): 56 data bytes
64 bytes from 172.21.0.2: icmp_seq=0 ttl=64 time=0.063 ms
64 bytes from 172.21.0.2: icmp_seq=1 ttl=64 time=0.052 ms
64 bytes from 172.21.0.2: icmp_seq=2 ttl=64 time=0.069 ms
--- ContainerB ping statistics ---
3 packets transmitted, 3 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.052/0.061/0.069/0.000 ms
```

So it works! `ContainerC` can talk to both. Notice how the _subnet_ changes (`172.21.x.y`) vs (`172.20.x.y`): the container switches network interface to switch network.  
**Note** that I did not say "note how the **IP address** changes", that is totally obvious (we're talking to a difference container, so _of course_ the IP address changes) ; I said that the **subnet** changed.

Now for check #2:

```bash
$> docker exec ContainerA ping -c 3 ContainerB
ping: unknown host
```

Perfect! As predicted (and desired!), `ContainerA` and `ContainerB` do not see each other, they can't talk to each other. Isolation is kept. Seriously, this is awesome.

#### What About This "default" eth0 Network Interface?

Hum... so you remember this, right?  
And you want me to explain it now? Well... I guess I have no choice, then.

I said I would explain later, so I might as well do it now!

Okay for those who have forgotten, if you recall correctly, when we created containers, it already had a network interface, usually named `eth0@xxx`. Then when we connected this container to a Docker Network with `docker network connect <network> <container>`, it gained a new network interface, usually named `eth1`.

So what **is** that `eth0`?

Let's make a few experiments before I explain, so that you can see for yourself.

Let's create a container, as usual and notice this phenomenon:

```bash
$> docker run --rm -itd --name C1 nginx bash
220c4e21c10e4a0f6b90e0015fc5f40f221437cd0675164eeb0a0e271e4e6c29

$> docker exec C1 ip addr show                                                                    ~

12: eth0@if13: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:11:00:02 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.2/16 scope global eth0
       valid_lft forever preferred_lft forever
```

(For this section I will remove the `lo` interface because it is irrelevant for our example, and will help make the article not too long).

Okay, so it does have this `eth0` network interface. Let's now connect it to `CustomNetwork`.

```bash
$> docker network connect CustomNetwork C1

$> docker exec C1 ip addr show
12: eth0@if13: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:11:00:02 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.2/16 scope global eth0
       valid_lft forever preferred_lft forever
14: eth1@if15: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:13:00:02 brd ff:ff:ff:ff:ff:ff
    inet 172.19.0.2/16 scope global eth1
       valid_lft forever preferred_lft forever
```

Okay. Makes sense. If we disconnect it from the Docker Network, it will lose its `eth1` interface.

Let's do this again (keep this container alive for now).

```bash
docker run --rm -itd --name C2 nginx bash
134357a53de8a5778a3a6a405321156e345563379186c638a8373bb477f52e58

$> docker exec C1 ip addr show
16: eth0@if17: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:11:00:03 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.3/16 scope global eth0
       valid_lft forever preferred_lft forever
```

So as expected this `C2` container also has a `eth0` network interface. But don't you notice _something_ peculiar?

I'll let you search for a couple of seconds.

...

Nothing? Let's make one more test and see if it ticks:

```bash
$> docker run --rm -itd --name C3 --net CustomNetwork nginx bash                                  ~
0c9f663520a53915a1e1c72dc6f3cb28cff654d0e8d266756b5419e04349020c

$> docker exec C3 ip addr show
18: eth0@if19: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:ac:13:00:03 brd ff:ff:ff:ff:ff:ff
    inet 172.19.0.3/16 scope global eth0
       valid_lft forever preferred_lft forever
```

Okay, again this test? We know this, this is nor... wait what?

Yep! Look again at this last command, this time I used the `--net CustomNetwork` option to connect `C3` directly. And what happened? The container has the familiar `eth0` network interface, but that's all.

I'll let you think about this now.

...

Okay, now's the time for the big reveal (note that you have everything you need to find the answer yourself, though this is not easy).

Okay so the last example is not that trivial. The `eth0` that we see is not the one we're used to. Look closely at `C1`, `C2` and `C3`.  
In the first two, `eth0` is in the `172.17.xx.yy` subnet, but in `C3`, it is in `172.19.xx.yy` subnet, which corresponds to the subnet `eth` is in for containers `C1` and `C2`.

So what is really going on here?

When you create a container **without** the `--net` option, it gets _automatically connected_ to a default `bridge` network. This `bridge` network you can see while listing the Docker Networks:

```bash
docker network list                                                                            ~
NETWORK ID          NAME                DRIVER              SCOPE
9aedc6a27184        CustomNetwork       bridge              local
70ff2194d229        MyFirstNetwork      bridge              local
83b7a5902813        Network1            bridge              local
721f118ce184        Network2            bridge              local
e68176304a37        bridge              bridge              local
2683c3824f8f        host                host                local
a84e731c4387        none                null                local
```

And if you inspect it:

```bash
$> docker network inspect bridge
[...]
"Containers": {
            "134357a53de8a5778a3a6a405321156e345563379186c638a8373bb477f52e58": {
                "Name": "C2",
                "EndpointID": "840aa7fa22a92b38c8e2d9b3de42999cfe5fba9a8ec7b42c5f6d676dce0e6dc6",
                "MacAddress": "02:42:ac:11:00:03",
                "IPv4Address": "172.17.0.3/16",
                "IPv6Address": ""
            },
            "220c4e21c10e4a0f6b90e0015fc5f40f221437cd0675164eeb0a0e271e4e6c29": {
                "Name": "C1",
                "EndpointID": "6445d819e6edb6209fb1272abeb8c8380313a61e44d74da8d8e7514f94e84219",
                "MacAddress": "02:42:ac:11:00:02",
                "IPv4Address": "172.17.0.2/16",
                "IPv6Address": ""
            }
        }
[...]
```

And here you can see that containers `C1` and `C2` are indeed connected to this Docker Network, _even though we did **not** specify `--net bridge`_.

And since they are connected to this Docker Network, they gain a network interface (`eth0`) which is in `bridge`'s subnet.

As for `C3`, since we created it with `--net CustomNetwork`, it got connected to `CustomNetwork` **and that's all**, this is why it only has `eth0` and that it's subnet is `CustomNetwork`'s.

The TL;DR of this is that when you create a container without any `--net` option, it gets automatically connected to the default `bridge` network. That's a golden rule of Docker that you should never forget.

This means that usually, there are _a lot_ of containers in that default `bridge` Docker Network.

Let's now kill two birds with one stone.

Since there are usually lots of containers in this `bridge` Docker Network and this can potentially lead to confusion, the embedded DNS server is **not** present in `bridge`. This means that even if you named your container with `--name <container-name>`, you **cannot** reach this container with the hostname `<container-name>`.  
In the `bridge` Docker Network, you can only reach a container by its IP address.

This is made to prevent confusion. If this seems not appropriate for production, it is normal: it is also meant to discourage you to use the default `bridge` network in production. If you want to have a clean production system, you should _always_ use a custom Docker Network.

##### Recap

Okay a _tiny_ recap section with two nice quotes, because really, you should **not** forget this (for your information, of all the time I spend on `#docker`, I repeat the two quotes at least once a week).

There are basically two gotchas that you need to remember:

> Every container **not** created with the `--net` option is _automatically_ connected to the default `bridge` Docker Network

and:

> In the default `bridge` Docker Network, there is no embedded DNS server, which means automatic name resolution does **not** work

That's all. Make sure to always use custom Docker Networks when doing serious work.

#### Can You Tell us More About the Other Networks?

Oh you are curious! That's good, I love curious people.  
Back when I introduced the Docker Networks, when we first ran `docker network ls`, we saw three default Docker Networks:

- `bridge` which we just explained (you know the default network to which all containers are connected by default, unless you specify a `--net` parameters)
- `host`
- `none`

Okay, so `none` is the easiest to understand: basically (as the name slightly implies!) it is used when you want to create a container but do not want to connect it to any network at all. In particular, creating a containe with `--net none` will prevent this container to be automatically connected to the default `bridge` network.  
But be warned: `none` is also meant to _enforce_ a container to have no connectivity, at all. So for as long as your container is part of the `none` network, it **cannot** be connected to any other Docker Network. You first need to `disconnect` it from the `none` network to connect it to another Docker Network.  
Be sure to remember this.

`host` is different and a little special. First of all, you cannnot connect _or_ disconnect a container to / from the `host` network. The only thing you can do is create a container with the `--net host` option. This way it will be automatically connected to the `host` network when it is created, and that's all.  
You cannot disconnected it later (you have to plainly destroy the container) and if you did not create the container with `--net host`, it not possible to add it later.

So what does it do then?

Well, when you make your container part of the `host` network, it means the container becomes part of the host's network stack. It means it will share all of the host's network interfaces. In other words, using `--net host` means you are **not** isolating your container (on the network side): it is a "normal" process on your host (from a network point of view), so it can see and access the host's network interfaces.

To be honest, if you end up using `--net host` for another reason that testing & curiosity, it's either because you're takign a nasty, dirty shortcut and you don't understand what you do, **or** you are really advanced and you know perfectly what you are doing.  
Personally, I have never had to use `--net host` for real, there is (almost) always a better, cleaner solution.  
So I included the explanation for consistency's sake, but I really discourage you to use `--net host`.

##### Small Recap?

This was not long, so let's make this recap short!

We have finally learned about the three default networks that come "pre-installed" when you install Docker. They are: `host`, `none`, `bridge`.  
Note that you cannot remove these three networks: if you try `docker network rm` on any of them ~~the worlds falls apart~~ you'll get insulted by Docker.

The roles of these networks are:

- `bridge`: this is the default bridge network to which all containers are connected when you create them without a `--net` option. Note that automatic name resolution (psuedo embedded DNS server) is **not** working for this network.  Do not use on production.
- `none`: used to prevent a container to have any sort of network connection
- `host`: prevent isolation of a container on the network side: it will share the host's network stack unaltered.

### Conclusion

So that was _quite_ a part, wasn't it? We have learned one of the most powerful (and one of my favorite) Docker features.  
If you recall what we had before this part, we could create separate and individual containers. We learned how to create good and well-structured Dockerfiles to eventually be built and create images.  
We learned how to make all of our containers totally ephemeral, _i.e._ we made it so that out containers could be stopped, restarted, destroyed and recreated at any time, without problems thanks to making important data persistent with Docker Volumes.

This was all good, but had a feeling of "incompleteness": it allowed only "sigle-container" applications, or, _worse_ it made you cram all your application processes inside one, monolithic container. **This is bad**. You should not do it anymore. For real.

Now, thanks to Docker Networks, we learned how to break the isolation **in a controlled way** between several containers: essentially, we learned how to make them communicate. And I hope you can see how many opportunities this now creates!

You can totally think about containerizing every part of your application stack: separate the database from the web server, from the intermediate redis cache, from a custom-implemented CND too of you wish!

But believe me, this is only half the fun (actually, this is even one _third_) of the fun!

What do I mean by that?

I mean that, network-related, we still have two **awesome** features to discover in Docker: overlay networks and the Swarm Mode. You might have heard these names before, you might even have tried to use them a little. If yes, then you should be excited, because we're going to talk about these. If no, well.. you _should_ be excited, because it's amazing.

The second part of this Part V article will be about overlay networks, because they are very very close to the bridge-based Docker Networks we have been studying up to now. As for the Swarm Mode, since this is a huge topic in itself, this is going to be covered in another Part.

So now we are going to be talking about overlay networks, but before that, you _really_ need to take a coffee break. And as usual, I mean it: a lots of the features we are going to be talking about have the same name, and deal wit hthe same concept. This is the _perfect_ way to get things mixed up. You think you understand Docker Networks, you think you mastered them bcause you've just read about them, and you end up mixing everything.  
So as usualy, here is the cup-of-coffee image, take a break, the article will still be there when you return!

![](/images/coffee_break.png "Coffee time!")

## Multi-Host Networking to Control the World!

And here we are! This section on overlay networks should complete your knowledge about Docker Networks, and let me tell you that once you've mastered this, there is _very little_ you cannot do with Docker.  
At the end of this section (and this Part), you will have all the fundamental notions needed to make anything complex in Docker.

Excited? Let's read on, then!


### Foreword

Just before I say anything else, just let me make something clear. If you look at an overview of this article, you will see that it is quite long, and the part about overlay networks comes at the end, and it relatively short, compared to the rest of the article. So I would understand if you went and think this is just a detail, it's trivial or not that important. **You'd be wrong**.  
Overlay networks are even _more powerful_ that bridge-based networks (the Docker Networks we have been talking about since the beginning of this article). But the Docker developers made their homework and made the API and most of the concepts work transparently. In orther words, there is a reason I'm talking about overlay networks _after_ bridge-based networks: it's because it re-uses all the notions we saw there.  
So really, don't think overlay networks are trivial: they simply have a lots in common with bridge-based networks, they simply have additional features, so you need to read the whole Part above to understand overlay networks.

That being said, let's proceed!

### Introduction

So with everything we have seen so far, we can have complex stacks: we can design our own images in Dockerfiles, perfectly suited to our needs, we can make data persistent, and thanks to Docker Networks, we can separate all logical aspects of our application into separate containers, make them communicate, and all is (almost) good in the universe.

Yes but, now if we scale up a little, we might encounted a situation. What if you want to deploy your setup in production and use dedicated servers to deal with the different aspects of the system?  
For instance, you can have a dedicated server to handle all incomming traffic, with a tailor-made `nginx` server running on it. If you want performance at this level, there are some choices to be made and you might end up with a server that is perfectly suited to route traffic and make load balancing, but not for handling lots of SQL requests.

So here you are, buying a second server that you fine-tune to be as performant as possible with the task of dealing with SQL requests.

Or you might forget what I just said and suppose that you prefer to keep the data in-house and use a home-hosted server for the database, but you're okay with renting / buying a server on a provider to handle the routing.

Or you might even craft other scenarii. The point is: it is very likely that some of you find themselves in a situation where the several components of their application stack do not live on the same machine ; that is the point we will address today.

In the first parts of this article, we learned how to separate our logical units (processes), for instance an `nginx` and a `postgresql` server, run them in their own container, put them in a Docker Network and have them communicate like this. But our problem _now_ is: how do we do when the `nginx` container and the `postgresql` containers are not on the same computer?!

It's not an unsolvable problem: we might totally imagine dockerizing the two (or more) services, each on one host ("host" is the Docker term we use to talk about the "machine" / "server" / "computer" on which Docker is installed), bind the ports (we know how to do this already) and provided the containers have network accessibility, they can talk to the other remote hosts like a normal application would do.  
So you are not out of options, obviously. It works. I mean, this is what people use all the time: making two (or more) computers communicate is not rocket science.

Obviously, it's your responsibility to provide either an IP address or a domain name for the dockerized applications to be able to talk one to another, it's also your responsibility to ensure the communication is encrypted, etc. But that is manageable.

But if we take a step back, we can see that our whole setup is _very_ similar to the ones we have been building in the first parts of this article, where we dockerized individual components on the same host, and that would be very nice and handy if we could somehow use the same mechanisms (Docker Networks), but on multi host (several different machines).

And yes we can! Let's see how!


### Conceptually Identical, Implementation (almost) Identical

As you must have understood from this little introduction, the docker developers have thought about this use-case, and provided us with a way. And the very nice part is that you will not need to learn much.. yes you guessed it, we will still use Docker Networks.  
So everything we have just seen together still applies (and this is why you should read the beginning of this article, otherwise it will sound like a gibberish).

So what's the difference then?

Do you remember when you used `docker network ls`? Try it now, there is a column, `DRIVER` which we have talked about before. It was one of three values: `host`, `bridge` or `null`.

Actually, there is a fourth value: `overlay`, which is part of the magic behind what we are about to see.

#### What to Expect?

Since this article is alredy very long, and you must be tired (although not _that_ much if you followed the breaks I recommended), it's best if I don't discourage you heads on. The really good thing here is that you will have almost nothing new to learn. Really!

Everything we saw still applies, and from a developer's point of view, it works almost exactly the same! Considering the time we took to study this, the length of the article, I personally think this is a **great** example of high-level API and abstraction, and I'd like to thanks the docker developers for this.

I deal with code all day long and spend a great deal of cringing on poorly-written code, well the docker devs have made a good job.

Anyway, back to business. So as I was saying, you won't need to learn much, the details, the magic is handled at lower levels. And this is great.

To be honest, I initially planned on explaining how Docker Networks worked internally in details, but the article would be much too long. So I have removed it from now, and will make a dedicated article later if you are interested ([let me know if that's the case](mailto: nschoe@protonmail.com)).  

In order to keep everything short (I can already see two or three people sleeping in the back of the class), I will only give you the bare basics (and you know I hate this...), this should be enough for you to go read some external documentation about the topics.

#### So How Does This Work?

Earlier in the article when I introduced the Docker Networks, especially the bridge-based ones, I talked about virtual network interfaces (`vethXXXX`) that were seen from inside the container. The inner working of the Docker Networks we have seen up to now (the `bridge`-based ones) is elegantly simple: this relies on `iptables`.

For those who don't know `iptables`, this is a Unix utility used to control the routing & filtering of IP packets. "This is a firewall" to put it simply. `iptables` work with `rules` and `targets`. Very roughly, you configure it to say "packets of this protocol from this source are ALLOWED / DENIED to reach this destination". Another rule is the `FORWARD` rule, which basically reroutes packets.

This is what Docker uses internally: when it want to make several containers communicate, it routes packets between the virtual network interfaces. This is elegant, simple and _very_ efficient.

But obviously, this works only locally, _i.e._ on the same host, because you need to know and access the network interfaces in order to do this. So although the API (the high-level part, the `docker network` commands) is similar, the internals are different.

Now suppose you have another computer (or "host"), say a remote server that has docker installed on it, and you have a container on it. Now you have your own host, with another container running and you'd like to estalish a Docker Network between the two. Docker needs to communicate with the remote computer and exchange information to synchronize the two docker daemons.

This is not handled internally. Why? Because when you tke a step back from this setup, it's not really docker-dependant. The problem reduces to a data synchronization problem, between two (or more) computers. And this is not a new problem, that was "solved" before. Not much data needs to be stored, basically, what container is on which networks, the IP address of the host and the internal IP adress op the Network.

In order to make this possible, Docker uses a (distributed) Key-Value store. A Key-Value store (KV-store in short) is a svery simple array-like "database". Values are relatively simple, and they are indexed by a key. There exists several popular and well-functionnign KV-stores, and thus Docker leveraged them.

So the TL;DR of all of this is: in order to set up multi-host (or "overlay") Docker Networks, you install and setup a KV-store, configure your Docker Daemon to interface with it and you're done. How awesome is that?.

I have just described the structure of the rest of the article, so let's go!

#### Hey! I don't Have a Server on Hands!

Okay okay, I know.  
So if you don't have yet a remote server on which you can install Docker, so that you can try multi-host network, fear not, we have a workaround.  
For such cases, Docker provides us with a new tool, called "docker-machine". "docker-machine" can do several things, one of which is to create optimized, small-size Virtual Machines in which Docker is installed.

I'm going to show you to setup a cluster of docker-machines to emulate some remote servers. In the rest of the article I'm going to try and show both ways: with a server and with docker-machines.

I just want to clarify things, in order to prevent confusion: in our case, docker-machine's only purpose is to create separated virtual machines that we will use as if they were remote servers. That's all.  
If you do have a remote server on hands, I suggest you use it: you just need to adapt the IP addresses and get rid of the docker-machine commands.

### [Optional] Create a Docker Machine

Do this only if you don't have a remote machine on hand with which you can play.

So before we can use docker-machine, we need to install it. The detailed instructions can be found [on the official page](https://docs.docker.com/machine/install-machine/#installing-machine-directly). But basically, you need to download the script from the github repository and `chmod +x` it.

At the time of this writing, you can find the latest version by doing:

```
$> curl -L https://github.com/docker/machine/releases/download/v0.12.2/docker-machine-`uname -s`-`uname -m` >/tmp/docker-machine
$> chmod +x /tmp/docker-machine
```
And then move the `docker-machine` executable from `/tmp` to a directory that is in your `$PATH`.

### Basic Working of Docker Machine

Before I explain quickly how we will use docker-machine, let me say that this is a more complex tool that I will present here: I am not writing an article about docker-machine (not yet at least).

Remember that docker-machine is a tool that allows you to _provision_ docker hosts (almost) wherever you want.

What does this last sentence mean? Let's examine it:

- "docker-machine is a tool": so it's something that you install _on top of_ docker. It doesn't come with it, and you don't _need_ it. It's something that helps you, but it is not mandatory.
- "_provision_ docker hosts": means that it installs a docker daemon, effectively making a "new" "host" (remember "host" is a "computer" / machine on which runs a docker daemon)
- "(almost) wherer you want": that means that you can provision these hosts (install a docker daemon) on your own machine, on remote server, on cloud providers (like Amazon, Azure, DigitalOcean, etc.)

#### Okay Got It, How Are We Going to Use It?

Here, we'r eonly going to use it to create docker hosts locally, _i.e._ on our own computer. Basically, the way to use it is very simple: we use a command to create the virtual hosts. And then when we want to run a docker command in a particular host, we first "point" the docker-machine CLI to the desired virtual host (this is just configuring some ENV variable) and run our command.  
It's as simple as that, really.

So in our case, you can think of docker-machine as a sort of "remote" which you points to the desired virtual docker host before running a command. And boom, that's it.

So don't worry: this article will still be very easy to follow, and if you don't use docker-machine because you have another computer / remote server, the commands are the same ; you just need to ignore the "make the docker-machine point to the desired host ".

Let's go back to setting up our overlay network!

### Set Up a Key-Value Store

The first thing we need to do in order to set up an overlay network is create our KV-store, which will hold the information about the overlay network, which hosts are part of it, which containers in each host, the name and IP addresses of the networks, etc.

Docker basically supports three KV-store:

- [consul](https://www.consul.io/)
- [ZooKeeper](https://zookeeper.apache.org/)
- [Etcd](https://coreos.com/etcd)

I've only used Consul, so this is the one I am going to use here. Consul (as most KV-store anyway) is a _distributed_ KV-store, which means it is meant to be used with several instances. All theses instances form a sort of cluster in which the information is stored, with redundancy so that some of the Consul instances in the cluster (some "nodes") can fail (_i.e_ disconnect, crash, die) without the information being lost.

In our case, we will use docker to install consul. Indeed, rather than installing consul directly on our machine, we will containerize it and run Consul inside a docker container. This way, at the end of this article, you just have to destroy your containers and nothing is messed up in your system, cool no?!

So now we create a virtual docker host with docker-machine, the command is: `docker-machine create`.

As I said, docker-machine is very powerful and can provision hosts in a lot of different places, so you need to specify where you want to create the host. In docker-machine jargon, this called a _driver_ and you specify it with the `-d, --driver` option. Here, we'll use driver `virtualbox`. This will actually use virtualbox to run a small Linux host system in which docker engine is installed (you need to have `virtualbox` installed on your system, refer to you distribution for the install (`sudo apt install virtualbox` for `Ubuntu`, `nix-env -iA nixos.virtualbox` for `nixOS`, etc.)).  
We will call this machine `kv-store`. So open a new terminal (you will undertand why soon) and run:

```Bash
$> docker-machine create -d virtualbox kv-store
Running pre-create checks...
Creating machine...
(kv-store) Copying /home/nschoe/.docker/machine/cache/boot2docker.iso to /home/nschoe/.docker/machine/machines/kv-store/boot2docker.iso...
(kv-store) Creating VirtualBox VM...
(kv-store) Creating SSH key...
(kv-store) Starting the VM...
(kv-store) Check network to re-create \if needed...
(kv-store) Found a new host-only adapter: "vboxnet1"
(kv-store) Waiting for an IP...
Waiting for machine to be running, this may take a few minutes...
Detecting operating system of created instance...
Waiting for SSH to be available...
Detecting the provisioner...
Provisioning with boot2docker...
Copying certs to the local machine directory...
Copying certs to the remote machine...
Setting Docker configuration on the remote daemon...
Checking connection to Docker...
Docker is up and running!
To see how to connect your Docker Client to the Docker Engine running on this virtual machine, run: docker-machine env kv-store
```

After this, we have essentially created a new (virtual) host, in which a docker daemon is running. We can list the hosts provisionned with docker-machine with `docker-machine ls`.

Let's see:

```Bash
$> docker-machine ls                                                                            ~
NAME       ACTIVE   DRIVER       STATE     URL                         SWARM   DOCKER        ERRORS
kv-store   -        virtualbox   Running   tcp://192.168.99.100:2376           v17.06.1-ce   
```

So this command lists the provisionned docker hosts, and for each of them, it displays:
- the name of the host
- wether it's active or not (more on this just in a minute)
- the driver used (remember, it's to differentiate between a local host created inside a virtual box, or a hsot provided on a remote cloud provider)
- the state (wether it's running, stopped, started, etc.)
- the URL at which we can join the host (here it's local, but it can the IP address of a remote server)
- whether it is part of a Docker Swarm (this is a very interesting topic, but for a later Part!)
- the version of the docker host
- whether there are errors

Remember that what we have essentially done here is create a "new server" (which in our case, is inside a virtualbox). But now that we have it, we need to _do things_ with it ; exactly as is we were logged on this machine.  
We _could_ use the virtualbox commands to actually log into the machine and then run the commands. But that would be **very** cumbersome. Instead, you need to remember one of the first things we have undertood about docker and the way the docker client works.

When you type docker commands in your terminal, you are using the docker client (`docker`). The docker client doesn't do much: it parses your input, check the mandatory arguments, etc. but then it sends all of this to the docker daemon (`dockerd`) which does the heavy work.

Where does the client send its data?  
The docker daemon listens to a socket, and the location of this socket can be configured. In all cases we have seen until now, your docker daemon created a socket at `/run/docker.sock`, which is a well-known location that the client knows about.

I suppose you have guessed it now, but we are going to use the exact same mechanism to talk to our provisionned docker hosts, and for that we'll use ... guess what ... the `URL` we've just seen.

So basically that's it: you can provision as many hosts as you wish with docker-machine, and then _just by setting the correct URL_ you can have the docker client send the commands to it instead of your local docker daemon. And then you have nothing else to do.  
Isn't that so cool?

Now, if you're astute you must begin to wonder / fear that you will end up chaining `docker-machine ls` commands, somehow fetching the URL (either an option to `docker-machine ls` or ugly grepping), somehow chaning the address the docker client will try to contact and then issue your actual docker command.  
This looks cumbersom, and it is...

Fortunately, the docker devs have thought about about this and they provided a subcommand `env` that displays the commands necessary to set up the docker client's environement. Yo uuse it like this: `docker-machine env <host-name>`.

Let's see what it gives with our container:

```Bash
$> docker-machine env kv-store

export DOCKER_TLS_VERIFY="1"
export DOCKER_HOST="tcp://192.168.99.100:2376"
export DOCKER_CERT_PATH="/home/nschoe/.docker/machine/machines/kv-store"
export DOCKER_MACHINE_NAME="kv-store"
# Run this command to configure your shell:
# eval $(docker-machine env kv-store)
```

As you see, it sets some variables, the most important being `DOCKER_HOST`, and you see that it matches the `URL` field we saw previously. You surely noticed the `export` clauses, and at the end, there is a comment that explains how to use: you need to use `eval` which is a shell's command, used to set / change environment variables.

So when you have several provisionned hosts, you will switch between them with these `eval $(./docker-machine env <host-name>)` commands, so it's a very simplified procedure.  
Always make sure you know which (if any) hosts is active before issuing docker commands, you can check which host is active with `docker-machine active` which returns the name of the active host.

```Bash
$> eval $(docker-machine env kv-store)
$> docker-machine ls
NAME       ACTIVE   DRIVER       STATE     URL                         SWARM   DOCKER        ERRORS
kv-store   *        virtualbox   Running   tcp://192.168.99.100:2376           v17.06.1-ce   
```

As you see, the `ACTIVE` field now contains a star which indicates that this host is the one that will receive the docker commands now.

Now that we have made a host active, you can see the effect by running traditionnal commands:

```Bash
$> docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE

$> docker ps -a
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
```

As you can see: there is nothing. And this is normal: the host has _just_ been created, so no images yet, no containers, nothing.

"Hey, but now I can't run 'normal' docker commands anymore, I can't see the containers on my machine!""

This is normal: your environement is configured so that your docker client sends the commands to a hsot provisionned by docker-machine. So you just need to unset this, so that it falls back to your local docker daemon.

In order to do this, you need to use the `-u, --unset` option of `docker-machine env`. See:

```Bash
$> eval "$(./docker-machine env --unset)"
```

We can tripple check that our commands worked and we are "back to normal":

First, check with `docker-machine active`:

```Bash
$> docker-machine active
No active host found
```

Then, check with `docker-machine ls`

```Bash
$> docker-machine ls
NAME       ACTIVE   DRIVER       STATE     URL                         SWARM   DOCKER        ERRORS
kv-store   -        virtualbox   Running   tcp://192.168.99.100:2376           v17.06.1-ce   
```

(we can see the start disappeared).

And finally, we can test to run our `docker` commands:

```Bash
$> docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
[...]
nginx               latest              db079554b4d2        6 months ago        182MB
ubuntu              16.04               104bec311bcd        8 months ago        129MB
```

Now you know the basics of how to interact with docker-machine and hte provisionned hosts. Make sure you got it, as we will use it from now on.

So! Back to our topic, we were about to set up a KV-store, so let's do that, shall we?

First, we need to activate the `kv-store` host with:

```Bash
$> eval "$(docker-machine env kv-store)"
$> docker-machine active
kv-store
```

Now we run the `progrium/consul` docker image, to create a consul server:

```Bash
$> docker run -d -p "8500:8500" -h "consul" progrium/consul -server -bootstrap
```

It will tell you that it cannot find the image locally, then it will download it and run it. You should have an output similar to:

```Bash
$> docker run -d -p "8500:8500" -h "consul" progrium/consul -server -bootstrap                    ~
Unable to find image 'progrium/consul:latest' locally
latest: Pulling from progrium/consul
c862d82a67a2: Pull complete
0e7f3c08384e: Pull complete
0e221e32327a: Pull complete
09a952464e47: Pull complete
60a1b927414d: Pull complete
4c9f46b5ccce: Pull complete
417d86672aa4: Pull complete
b0d47ad24447: Pull complete
fd5300bd53f0: Pull complete
a3ed95caeb02: Pull complete
d023b445076e: Pull complete
ba8851f89e33: Pull complete
5d1cefca2a28: Pull complete
Digest: sha256:8cc8023462905929df9a79ff67ee435a36848ce7a10f18d6d0faba9306b97274
Status: Downloaded newer image for progrium/consul:latest
5af66f2f99d6eb77d40ac61522da8c10e6c30c5b8eec6e33149db33fe8d0dc74
```

Let's just check the consul server is actually running in our `kv-store` docker host:

```Bash
$> docker ps
CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS              PORTS                                                                            NAMES
5af66f2f99d6        progrium/consul     "/bin/start -serve..."   53 seconds ago      Up 51 seconds       53/tcp, 53/udp, 8300-8302/tcp, 8400/tcp, 8301-8302/udp, 0.0.0.0:8500->8500/tcp   frosty_northcutt
```

Yes, so far so good.

### Configure the Docker Daemon Hosts

Before we move on, I'd like to make a small recap of what is going on, because that is the end of the article (which is growing in size), I have just made quite a detour to introduce basic `docker-machine` commands and I feel I'm losing you...

So here we go, small recap!

In the first (big) part of this article, we learned about Docker Networks and how it can be used to make containers communicate together. This allowed us to do great things and really unlocked a new aspect of Docker that we were previously missing.  
All of this is very good, but it is limited by the fact that you can only create Docker Networks on your machine and connect local contains to it (understand: containers that are running on the same machine), and we found that if would be _so great_ to extend this exact same mechanism (so we don't have too much _new_ to learn) so that it worked between machines that are _remotely connected_.

This is possible through the use of _Overlay Docker Networks_.  
But we understand that if we want to have several remote machines talk to each other and maintain a common state, we need a shared medium to synchronize data. For instance, which containers and on what machines are in which Overay Docker Networks, what is this container's internal IP, etc.

This is achieved by Docker by leveraging the power of distributed Key-Value stores, such as `consul`, the one we use here.

In the last section (_Set Up a Key-Value Store_), we did two things:
1. we learned how to use `docker-machine` to create virtual hosts on our own machine, to emulate the fact that we have several (physical) servers / hosts (people who _acutally_ have several servers / hosts can bypass this part obviously)
2. we actually created one instance (server) of the consul KV-store.

So that's it for the recap: we now have one instance of consul running, and it's reachable at the IP address given by `docker-machine ls`. Ho and by the way, if you need to have _just_ the IP address of a host provisionned by docker-machine, instead of running `docker-machine ls` and grepping, you can use `docker-machine ip <host-name>` and it retursn _just the IP address_, which is pretty handy ;)

A last note: `consul` (and KV-store in general) are much more complex systems that we made it appear here. This is completely different and independant from docker: you can use KV-stores to synchronize data across several machines. There is a whole topic about distributed KV-store.  
In our case, we run _just_ **one** instance of the KV-store (in the host named `kv-store` provisionned by docker-machine), but this is dangerous in real-life applications: if this machines fails and the KV-store cannot be reaches anymore, uyou lost your information.  
This is why distributed KV-stores are generally run in several instances, on several different machines and theses machines communicate together to maintain a **distributed** set of information, so that if a machine (or a number of machines) fail, the information can still be reached.

We don't do that here because the goal of the topic is to teach about (Overlay) Docker Networks, but if you are going to use Overlay Docker Networks in your production applications, I **strongly** suggest you document about consul (or whatever the KV-store you chose) and learn to properly setup a really distributed cluster, so that you don't lose your information the day one of you machines crashes.

Okay! So what's next?!

Well now that we have a KV-store up and running, we need to configure the docker daemon(s) that should be using it! Up to now we did not have anything to configure because when we created Docker Networks and connected containers to them, this information was kept locally by Docker and it did not need to be synchronized.

So now we must add **two** options to our docker daemon:

1. one IP address where it can reach a KV-store (and what type of KV-store it is)
2. one IP address / network interface on which it should _advertise_. What does that mean? It's easy: suppose we have several docker daemons (on several machines) that should communicate together. Meaning we will have Overlay Docker Networks created, some containers on these machines and some on these containers will be part of the Overlay Docker Networks. But the docker daemons need to talk to each other to synchronize runtime data (_i.e._ "can you resolve this particular IP address, because it doesn't belong to any of my containers", etc.) Well they do so though the "advertised" IP address. Easy!

In order to configure the docker daemons, we have several ways, I will talk about two:
1. one quick but dirty way (most useful to **test** if the configuration is okay)
2. the modern, clean way

AS you may know, the docker daemon is now run with `dockerd`, which you can give parameters. The two parameters we want to set now are (in order):
1. `cluster-store`
2. `cluster-advertise` (be careful, this is "advertise" and not "adverti**z**e").

The quick way is simply to launch `dockerd` by hand with the options, the clean way is to create a `systemd` unit file, and set the configuration in it.

First you need to stop you docker daemon (be warned that it will stop all of your containers). If you had launched it with `dockerd` previously, then hit `CTRL + C`, if you had launched it with `systemd`, then type `systemd stop docker`.

Now we are going to set the parameters, first by command-line (to quickly test).

We need to set the IP address of our consul server, which is the IP address of the proviosnned docker host `kv-store` so let's get it:

```Bash
$> docker-machine ip kv-store

192.168.99.100
```

(This is obviously subject to change on your machine, use your own returned IP address).  
Now we need to port on which the consul server listens ; we configured it to be `8500` in the previous `docker run` command (scroll backup, you will see `-p 8500:8500`).  
And lastly, we need to tell docker that it's a consul cluster, and not a zookeeper or etcd. So the full parameter for the first option is: `--cluster-store=consul://192.168.99.100:8500`.

That was not too hard, right?

The second option is even simpler, it takes the form: `<ip-or-interface>:<port>`. Remember that this is the `IP:port` that other (potentially remote) docker hosts will contact your host at. So make sure this `IP:port` combinaison is open an reachable from your remote servers.  
In our case, since we are only dealing with local provisionned (virtual) hosts, we can use `127.0.0.1`. The port must be `2376` (this is docker's protocol). When / If you are using a remote server (instead of a local provisionned virtual host), use `<interface:IP>`, for instance `<eth1:2376>` or `<wlan0:IP>`.

So you will have something like `--cluster-advertise=127.0.0.1:2376` in our case, adapt if you need.

### Creating And Using Overlay Docker Networks

Now that we have our options, we can either launch the daemon by hand, like this: `dockerd --cluster-store=consul://192.168.99.100:8500 --cluster-advertise=127.0.0.1:2376` or edit the systemd configuration file to add the options on the `ExecStart=` line.

Chose the option you want and start the docker daemon. When it is started, nothing looks different than before, but now the daemon knows where to join a KV-store and thus the Overlay Docker Network feature is enabled.

Let's create our first OVerlay Docker Network.

As I said earlier, everything we saw with traditionnal Docker Networks still applies ; Overlay Newtorks are built upon this. So we still create an Overlay Docker Network with `docker networks create`, but now we had the `-d, --driver` option to tell docker that is not's "bridge" anymore (it's the default we have been using until now) but "overlay". All in one, you do it like this:

```Bash
$> docker network create -d overlay TestOverlay
2c5c37a080f5592bb245ea3fd6e37653c86e56f62dd10c8e1c40ab72708b26c3
```

And then you can check with

```Bash
$> docker network ls
NETWORK ID          NAME                DRIVER              SCOPE
9aedc6a27184        CustomNetwork       bridge              local
70ff2194d229        MyFirstNetwork      bridge              local
2c5c37a080f5        TestOverlay         overlay             global
2d610b04d998        bridge              bridge              local
2683c3824f8f        host                host                local
a84e731c4387        none                null                local
```

Now you should spot the new network which differ because its `DRIVER` option now lists `overlay` and its `SCOPE` is `global`.

So far so good, but nothing too fancy. The magic will come with a second host.

### Provisionning a Second Host

For now we have all in all **two** hosts:

- our local machine, the one on which we have been experimenting with until now
- the `kv-store` (virtual) machine that we provisionned with docker-machine

But when it comes to docker, we really only have one, because `kv-store` is **only** running the KV-store (consul), but it is not a docker host. So we will now provision another docker host: one on which a docker daemon will be running and where we will create containers.

Let's do this!

This time we will call it `second-host` (I have a talent for naming...). Before I give the command, I want to clarify one or two things.

So we will provision a virtual host with docker-machine (obviously skip this part if you are using a real, remote server). If the Docker dev team went to all the trouble of creating this `docker-machine` tool, you can imagine they tailor-made it to suit docker needs.  
So when you provision a new host with `docker-machine` and the `-d virtualbox` driver, it's not just a simple Linux host ; the whole goal of `docker-machine` is to install and configure the docker daemon automatically.

Do you remember the configuration we had to do with our own docker daemon (namely setting `--cluster-store` and `--cluster-advertise`)? Well, we will obviously need to do this for the provisionned hosts too, nothing is magic: the automatically installed docker daemon _still needs_ to know about the KV-store.

But fortunately, we won't need to log into the virtual host and edit the configuration by hand: `docker-machine` has an option, `--engine-opt` which is used to pass configuration options to the docker daemon it will automatically install.

So now we can pass these two options. We need two things:

1. the IP address of the KV-store, but that is easy, it's the IP address of `kv-store`, the provisionned host. Remember that we can get it with `docker-machine ip kv-store`, so we will use this directly in the command
2. the IP / interface on which to _advertise_ the docker daemon. I will make it easy for you: when you provision a docker host with docker-machine, then the virtual host has a (virtual) network interface `eth1`. So you can use this.

Enough talk, let's do it now!

```Bash
$> docker-machine create -d virtualbox --engine-opt="cluster-store=consul://$(docker-machine ip kv-store):8500" --engine-opt="cluster-advertise=eth1:2376" second-host

Running pre-create checks...
(second-host) Default Boot2Docker ISO is out-of-date, downloading the latest release...
(second-host) Latest release for github.com/boot2docker/boot2docker is v17.07.0-ce
(second-host) Downloading /home/nschoe/.docker/machine/cache/boot2docker.iso from https://github.com/boot2docker/boot2docker/releases/download/v17.07.0-ce/boot2docker.iso...
(second-host) 0%....10%....20%....30%....40%....50%....60%....70%....80%....90%....100%
Creating machine...
(second-host) Copying /home/nschoe/.docker/machine/cache/boot2docker.iso to /home/nschoe/.docker/machine/machines/second-host/boot2docker.iso...
(second-host) Creating VirtualBox VM...
(second-host) Creating SSH key...
(second-host) Starting the VM...
(second-host) Check network to re-create \if needed...
(second-host) Waiting for an IP...
Waiting for machine to be running, this may take a few minutes...
Detecting operating system of created instance...
Waiting for SSH to be available...
Detecting the provisioner...
Provisioning with boot2docker...
Copying certs to the local machine directory...
Copying certs to the remote machine...
Setting Docker configuration on the remote daemon...
Checking connection to Docker...
Docker is up and running!
To see how to connect your Docker Client to the Docker Engine running on this virtual machine, run: docker-machine env second-host
```

We can check that we have a new host:

```Bash
$> docker-machine ls

NAME          ACTIVE   DRIVER       STATE     URL                         SWARM   DOCKER        ERRORS
kv-store      -        virtualbox   Running   tcp://192.168.99.100:2376           v17.06.1-ce   
second-host   -        virtualbox   Running   tcp://192.168.99.101:2376           v17.07.0-ce   
```

(Funnily enough, it seems to have updated the docker version between the time I created `second-host`!).

Now I am going to print a startled cat image because I want your attention:

![](/images/startled-cat.jpg "Pay Attention Now!")

Okay, now I've got your attention, please follow what we we are about to do, and what happens.

First, we are going to _activate_ the `second-host` host with `eval "$(docker-machine env second-host)"`.

Let's make sure it worked:

```Bash
$> dokcer-machine active
second-host
```

Okay, so now our docker client "points to" (_i.e._ send its commands to) the docker daemon inside `second-host`, which **let me remind you** must be considered like a total other machine, one that might be on a server provider 10,000 km from your.

As we would expect from this newly-provisionned docker host, there is nothing (no containers, no images, no volumes, nothing):

```Bash
$> docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE

$> docker ps -a
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES

$> docker volume ls
DRIVER              VOLUME NAME
```

Let's check the networks for completeness:

```Bash
$> docker network ls

NETWORK ID          NAME                DRIVER              SCOPE
2c5c37a080f5        TestOverlay         overlay             global
2e745f1f19da        bridge              bridge              local
6cf2d0234769        host                host                local
bbec139d7ee0        none                null                local
```

So we've got our traditional 3 basic networks (`none`, `host` and `bridge`) and that's about ... wait what?!

...

(hint: you should be very excited now).

So yes, what just happened is very cool: we have essentially created a new host with a fresh-new docker installation, but _somehow_ it already knows about the `TestOverlay` Docker Network that we created:

1. **earlier**
2. **on a _completely_ different machine!**

Obviously, the _somehow_ is explained easily: the docker daemon contacted the KV-store thanks to the `--cluster-store` option we passed earlier.

But the very cool thing to take away from this, is that by configuring a KV-store and settin up the docker daemons correctly, _we can have Docker (Overlay) Networks shared amonst **completely independant** machines (hosts)_!

Please, **please** take a few _seconds_ (minutes if you need) to re-read this last sentence and make _absolutely sure_ you understand the consequences and the _power_ it will give us.

Seriously, I know it seems like I do a big deal out of this, but this **is** a big deal. Think about everything we learned in this Part: breaking the isolation between containers in a controlled manner, embedded DNs server that automatically resolves the containers' names to their correct IP etc... well everything is still working, and on top of that... _it is now **spread across several servers**_.

We have just (almost freely) extended our nice little comfy zone of isolated-yet-communicating containers _between servers_. Now, from a docker point of view, when you have containers `C1` and `C2` inside Docker Network `N`, it doesn't matter if `C1` and `C2` are on the same server `S1` or if one is on server `S1` and one of server `S2`.

That is **awesome** and I'm not continuing the article until I hear everyone of you say that it is awesome.

I'm waiting.

...

Good. Now we can wrap up things.

### Creating Containers And Be Amazed

We are not going to make _extended_ tests because as I said several times, the Overlay Networks work exactly the same as Bridge Network. And we have covered Bridge Networks.  
We will still make one or two basic tests to convince ourselves that it does appear to work, but aside from this, you can apply what you already know.

But _still_, it deserves a small test nonetheless. We are going to create a container on `second-host` and a container on our own local machine, obviously we are going to connect them to the `TestOverlay` Docker Network and we are going to see if they can talk to each other.

Make sure `second-host` is `active` with `docker-machine active`. If not, make it active with `eval $(docker-machine env second-host)`.

Then create a container, as usual, connecting it to the network:

```Bash
$> docker run -itd --name remote_cont --net TestOverlay nginx
Unable to find image 'nginx:latest' locally
latest: Pulling from library/nginx
94ed0c431eb5: Pull complete
9406c100a1c3: Pull complete
aa74daafd50c: Pull complete
Digest: sha256:788fa27763db6d69ad3444e8ba72f947df9e7e163bad7c1f5614f8fd27a311c3
Status: Downloaded newer image for nginx:latest
4f5771a78fd9cdc59b982228a7de8410bc6d7f26f473b0d63a6c57523845f5d
```

So we created a container named `remote_cont` on the host `second-host` and it is part of the `TestOverlay` network. We can quickly check it is indeed part of the network:

```Bash
$> docker network inspect TestOverlay
[...]

"Containers": {
            "4f5771a78fd9cdc59b982228a7de8410bc6d7f26f473b0d63a6c57523845f5d5": {
                "Name": "remote_cont",
                "EndpointID": "e78ea760c30bb3cfff942b26a1182a24d2c4555cbd0e6417b08594c29c0a5bd3",
                "MacAddress": "02:42:0a:00:00:02",
                "IPv4Address": "10.0.0.2/24",
                "IPv6Address": ""
            }
        },
[...]
```

So that's good: the container _is_ connected.

Now, let's "get back" to sending commands to our own local docker daemon, so we _de-activate_ `second-host`. `eval $(docker-machine env -u)`.

A quick `docker ps` shows you that the `remote_cont` is not listed anymore.

What would happen if we ran `docker network inspect TestOverlay` on this host? Remember this is our local host now, it is **not** the host on which the container `remote_cont` was created and exists?

Easiest way to find out out is to try:

```Bash
$> docker network inspest TestOverlay

[...]
"Containers": {
            "ep-e78ea760c30bb3cfff942b26a1182a24d2c4555cbd0e6417b08594c29c0a5bd3": {
                "Name": "remote_cont",
                "EndpointID": "e78ea760c30bb3cfff942b26a1182a24d2c4555cbd0e6417b08594c29c0a5bd3",
                "MacAddress": "02:42:0a:00:00:02",
                "IPv4Address": "10.0.0.2/24",
                "IPv6Address": ""
            }
        },
[...]
```

So... we **do** see that it lists a container `remote_cont` that is connected to this network, but if you look closely, the ID is not the same. It starts with `ep-` which it typical of containers listed that are not present on ths current docker host (it's not important for the moment, just keep this in a corner of your mind).

So this is still pretty interesting: we are on a docker host where container `remote_cont` does not exist, it was never there but _still_ it is listed when inspecting the Docker Network. All of this is possible, you guessed it, thanks to the KV-store that stores information.  
You would note however, that the `EndpointID` is the same on both output, and this is what is used internally by docker to actually identify the container.

Oh and one last thing you might have noticed: the IP address if very different thant the IP addresses we have dealt with until now. They used to be in `172.X.Y.Z`, but now they are in `10.X.Y.Z`. So that is another way of telling you are in an Overlay network.

Now let's add a container to this network, on this host!

```Bash
$> docker run -itd --name local_cont --net TestOverlay nginx
e9bf582914beb21901955d0961b67443b6f2f634f86b4a3d1e1af7e2b196bc04
```

A quick inspection of the network shows that it worked as expected:

```Bash
$> docker network inspect TestOverlay

[...]
 "Containers": {
            "e9bf582914beb21901955d0961b67443b6f2f634f86b4a3d1e1af7e2b196bc04": {
                "Name": "local_cont",
                "EndpointID": "6ff26f6dad3fc64fbbd54a1f99cd88b10713d07acb6a7996f61a4dc460627bfb",
                "MacAddress": "02:42:0a:00:00:03",
                "IPv4Address": "10.0.0.3/24",
                "IPv6Address": ""
            },
            "ep-e78ea760c30bb3cfff942b26a1182a24d2c4555cbd0e6417b08594c29c0a5bd3": {
                "Name": "remote_cont",
                "EndpointID": "e78ea760c30bb3cfff942b26a1182a24d2c4555cbd0e6417b08594c29c0a5bd3",
                "MacAddress": "02:42:0a:00:00:02",
                "IPv4Address": "10.0.0.2/24",
                "IPv6Address": ""
            }
        }
```

And now's the real test: we will enter container `local_cont` and try to ping `remote_cont`, which is on a different host, remember.

```Bash
$> docker exec -it local_cont bash
#> ping -c 3 remote_cont
PING remote_cont (10.0.0.2): 56 data bytes
64 bytes from 10.0.0.2: icmp_seq=0 ttl=64 time=0.387 ms
64 bytes from 10.0.0.2: icmp_seq=1 ttl=64 time=0.383 ms
64 bytes from 10.0.0.2: icmp_seq=2 ttl=64 time=0.380 ms
^C--- remote_cont ping statistics ---
3 packets transmitted, 3 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.380/0.383/0.387/0.000 ms
```

And it works!

So basically, containers `local_cont` and `remote_cont` are talking to each other as if they were in the same private network (LAN) even though they are on two different hosts (computers) that can be anywhere in teh world.

This is fantastic. Seriously, you don't understand how fantastic this is.

What that means is that you can keep thinking and designing your Docker stacks correctly, _i.e._ separate the different logical units (processes) in different containers (one container for the web server, one container for the database, one container for the redis cache, one container for service monitoring, etc.), but now you don't have to cram all of these containers into a single machine...  
You can have as many machines (hosts) as you want, you can fine-tune the configuration of each machine to fit its use case and on top of this, "all you have to do" is configure your docker daemons and your KV-store correctly, and thanks to this, you can create Docker Networks across these machines and connect the containers to them.  
The containers are just contacting other containers by using their container names, the embedded DNS server takes care of the routing for you and you can essentially keep thinking and designing your stack as a big LAN network (minus the network latency of course).

This is incredibly useful.

I mean a small overview of this might be something like: you buy a server which is optimize for bandwidth but doesn't have lots of disk space to put all your `nginx` containers on it, you can buy a server which has a lot of RAM to put your redis cache on it and you can buy servers with _a lot_ of disk space to put your `postgresql` containers on it.  
Once you have done that, configure the docker daemons, create your Overlay Docker Networks, connect your containers and This. Just. Works.  
_Everything_ is abstracted for you.

You don't have to take care of routing, of matching container name to the IP address of the _host_ **nor** macthing the container's IP address _inside a host_.  
All of this is done for you, by docker. This is great, really.

### Gotchas And Common Errors

As I said before, Overlay Docker Networks work exactly as Bridge-based Docker Networks as far as the user (_it's you_ is concerned), provided you managed to setup the system. I want to give you a few hints to diagnose your setup in case it doesn't work.  
It's not much, but I found that it was very easy to avoid these and a good part of the time, it solved the problem.

So here they are:

- The first obvious thing to know / remember is to have setup your docker daemon to work with the KV-store, for that, you need to have the two options `--cluster-store` with:
    1. a protocol (for instance `consul://`)
    2. an IP address (the IP address of one of the KV-store node, make sure this address is reachable by your host)
    3. the KV-store port (for consul this is `8500` if you did not change the default configuration)
Make sure the IP address and the port are opened by your firewall.
The second option is `--cluster-advertise` which needs the network interface (`eth0`, `wlan0`, etc.) that your docker host will use to advertise its presence, and the port (this is `2376` if you did not change Docker's settings)
- If you set these up with a systemd unit file (most likely `/usr/lib/systemd/docker.conf.d/docker.conf`), then **make sure you added a newline at the end of your file**. I have spent countless hours reviewing everything: the addresses were correct, the ports were correct, the access were authorized, everything should be okay yet nothing worked, etc. And then I started "looking the keys inside the fridge" and one day I opened the systemd file with another editor which automatically added the newline and it worked. So... always, **always** end your config files with a newline at the end. This one, I guarantee **will** save you hours!
- Make sure to look at the docker daemon logs (either by starting it by hand, or looking at `journald`), but the logs might indicate. For instance, if you get the `--cluster-advertise` option wrong, you will see a message like this:

```Bash
WARN[0001] Multi-Host overlay networking requires cluster-advertise(x.x.x.x) to be configured with a local ip-address that is reachable within the cluster
```

And that indicates that it doesn't work.
- Another thing that is worth knowing is that the two options `--cluster-store` and `--cluster-advertise` are **both** necessary for Overlay networks to work, but they are not related to the same thing, and they work independently of each other.  
To make it short, `--cluster-store` allows the docker daemon to contact the KV-store and fetch information. This is what makes `docker network ls` shows the Overlay Docker Networks (if you misconfigured it, only the Bridge-based Docker Networks will show up), and this is what makes `docker network inspect <name-of-overlay-network>` shows the information about which contains are connected, their IP addresses inside the network etc. So even if you only configure `--cluster-store` or if you misconfigured `--cluster-advertise`, this will still show you and you might think that everything is working as it should.

The second option, `--cluster-advertise` is what makes the docker hosts able to communicate and synchronize together. This is what makes containers in different hosts able to talke to each other. It handles the routing. So if you have configured `--cluster-store` together but `--cluster-advertise` is misconfigured, then when trying to ping a container in another host, you will get `92 bytes from 523ed8b20522 (10.0.0.4): Destination Host Unreachable` for instance.  
This is very important because you need to be able to diagnose your problem. The TL;DR is that:

- if you don't see the Overlay Networks with `docker network ls`, then it's a problem of `--cluster-store` (so either it points to the wrong IP address, the wrong port, or the IP address / port are not opened by the firewall, or the KV-store is simply not running, etc.)
- if you can see the Overlay Networks but your containers fail to talk to each other (`Destination Host Unreachable`, `No route to Host`, `Timeout`), then it's a problem of `--cluster-advertise` (wrong network interface, wrong port (`2376`), the network interface is not opened on the outside, firewall blocking (port `2376`), etc.).

Here you go, with these sets of indications, I'm confident that you should be able to diagnose the vast majority of the problems you might be having with Overlay Networks, and if you still can't, either [should me an email](mailto:nschoe@protonmail.com), and go to `#docker` and I'm here (@nschoe).

And once you managed to setup the Overlay Networks, with them and everything we saw in the first 4 Parts, you should be able to make very nice container stacks.

## Conclusion

Ok-kay! So here we are: we have _finally_ reached the end of this Part V about Docker Networks. I know it was long, and I know it took a lot of time to write (and thus publish), and I'm sorry about that.  
But Docker Networks are the last fundamental Docker concept. Now you truly have everything you need to build very complex stacks The Right Way.

Also, I found (hanging on IRC and reading Stack Overflow questions) that it was (as with the other fundamental Docker concepts) greatly misunderstood and misused. This is why I wanted to make an article that explained the Docker Networks the best I could and hopefully clear any misconceptions about them. Even if it meant taking my time.


So what did we learn in this Part V?

We learned to break the isolation between containers that we have been talking about since Part I, but in a _controlled_ way. In other words, we learned how to open a small door between chosen containers so that they can know about each other, and then communicate, exchange data, etc.  
All the other aspects are still completely hidden (a container still can't know about another container's processes, resources, filesystem, etc.), and obviously only the containers we chose can know about each other. So we have the best of both worlds.

Part V is also a sort of milestone, because with it we have finsihed learning about the fundamental concepts of Docker, we know all the "basics". By "basics" here I don't mean "basic usage", as in "simple", but rather the core concepts, the things that we need to make real use of Docker. Let's recap them briefly:

- In Parts I and II we learned about Docker itself, what this is (because it's always important to know what we are talking about) and we learned about the difference between an image and a container: an image is a "recipe" to setup an environment and a container is one instance of it. We also learned that containers should be ephemeral, they are designed to be short-lived: A. Container. Is. Just. A. Process.  
We always should be able to stop, destroy and rebuild a container.
- In Part III, we learned about how to write Dockerfiles and how to build our custom images, with this we can now design a tailor-made environment that suits the software you want to run.
- Until Part IV we had a problem: if we were making short-lived container, container that could be shut down at any moment, even destroyed, we would lose data. So we needed to have a way to "save" data somewhere. This is where Part IV comes with Docker Volumes.  
We learned how to make data persistent by storing it in Docker Volumes (which have a different life cycle than containers), and we also lerned how to share data between the host and the containers.  
I emphasized on the fact that even though storing data in a volume and sharing data with the hosts were both grouped under the name "Docker Volumes", it was two different things and one should not be used in the place of the other.
- But even then we still had a problem: we were able to make ephemeral containers ("just processes", remember?), we could store our important data in Volumes, but if we wanted to have only one (logical) process per container, we were limited to very simple setup, because if out setup involved several containers, it was impossible to make them communicate one with another. Part V solves this by introducting the concept of Docker Networks, which -as the name implies- is a way to make several containers communicate with each other.  
We then extended this concept to several machines, so that distant, remote servers running a docker daemon (and running containers of their own) could **easily** have networked container, basically for free ("for free" here means that we don't have anything new to learn and almost nothing new to setup).

Now, after those 5 Parts you can do (almost) anything, and you can sure make very complex stuff. For information, I have been working with Docker for a couple of years now, we have a pretty complex client stack and they rely on what we have been learning on these first 5 Parts.

So feel free to experiment now, build complex stacks, and enjoy.

### So This is It? That's How It Ends?...

Yes...  
I mean... for **this article**, yes, but not for the series, no! Fear not.  
We are not done with Docker yet, not even close, we have lots of cool stuff to discover, and I want to keep writing about them (as long as you still want to read!).

There are in particular two very important topics that I want to discuss, and I'm sure you have heard of them, they have:

- Docker Swarm, and
- docker-compose

Both of them are very powerful tools, which open your world of possibility even more, but they are (alas) too often, _much_ too often confused with what I call "vanialla docker". They sound attractive (and they are!) but beginners almost always jump to these tools too early. And then they must deal with all the notions we have covered in these 5 Parts, **plus** docker-compose and Docker Swarm.  
As logn as they follow "click-here, do this" tutorials, it's fine, but when they start havign problems on their own, they can't diagnose where the problems comes from.

This is why I haven't talked about any of these tools yet. I tried to give you all the tools that I could think of to really understand what docker does, what docker is composed of and what docker can do. Now that you have these tools, I feel that we are ready to talk about Docker Swarm and docker-compose. So I think the next articles are going to be about them.  
I'm not sure of the order yet, so I prefer not to give that away (in fear of disappointment), but "It Is Coming.".

In the mean time, I hope you will play with Docker and what we have learned together in these first 5 Parts, and I urge you to email me if anything is still not clicking, or if you feel the way I introduced a topic is still misleading (also I take "thank-you" email as well!).  
If you have any topic that you would like me to cover in upcoming Parts, also feel free to suggest by email.  
Also, last piece of news: I'm thinking about going freelance and take on some consulting missions regarding docker (maybe advise people on the way to build their stacks, maybe actually build the stack, review some already exising stack, etc.).

If you think you might be interested, don't hesitate to contact me. I still don't have the official status, etc, it's something I have been thinking about for quite some time. Now that this series is taking some traction, I feel it would be a good time. Anyway, for whatever reason, yo ucan contact me on [nschoe@protonmail.com](mailto:ns.schoe@protonmail.com).

I hope you liked the article, and see you in Part VI!
