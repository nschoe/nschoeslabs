---
title: "Docker: Taming the Beast - Part II"
description: In this second article, it's about time we began playing with docker commands. We will begin by seeing how to install docker, and then we will cover the fundamentals. This is very important because everything else will rely on the good understanding of Docker fundamentals / terminology.
toc: yes
tags: docker,container,continuous-integration,continuous-delivery
---

## Introduction
Welcome to the **second** part of the _Docker: Taming the Beast_ series. In [Part I](/articles/2016-05-26-Docker-Taming-the-Beast-Part-1.html) we have talked about the core principles behind docker: what is going under the hood.  
This sharpened our intuition, and now we are ready to go in: let's play with Docker!  
In this post, we will briefly see how to install Docker, and then we will focus on what I call the _Docker Fundamentals_. These will be the Docker concepts (and commands!) that you will use on a daily basis.

![](/images/warning.png "") Be warned: this part _is_ dense, and long. Here, everything will be new (depending, of course, on your level) and everything will be important. I suggest you take time to practice the examples and _spend time_ on each section and new concept, until you fully understand it. It **will** come bite you _you-know-where_ later if you try to take shortcuts.

Now, let's get ready and see how to install Docker!

## Installing Docker

As you should recall from [Part I](/articles/2016-05-26-Docker-Taming-the-Beast-Part-1.html), Docker runs on the Linux kernel, so it _needs_ the Linux kernel. Hence, it should be installed... in Linux.  
Now what do you do when you are running Windows or Mac OS X? Well, this is simple: you create a Virtual Machine running Linux and you install Docker in it!  
Simple, isn't it?!  
No. Well yes, but... no. _Basically_ this is what the Windows and Mac OS X installers do when you install Docker on them. Only the Docker guys have automated the process (so you don't need to manually create the VM, and install Linux and install Docker and launch it.) Additionally, they take care of a lot of other stuff, which deal with network, sharing data, etc. These concepts are covered below.

All of this is to say that currently, the safest and easiest way of running Docker is from a plain-old Linux. So do that if you can and chose the Windows / Mac OS X install if you really have to. Full disclosure: I run under Linux and have never used Docker under Windows or Mac OS X.

Now, before we really begin, let me say a few word about _which_ Linux distribution is the "best" for docker. Newcomers on the `#docker` channel often come ans ask this question: "what's the best Linux distribution for docker?".  
Actually the answer is: _it doesn't matter_. In fact, docker was _designed_ not to care. As you will learn in this article, docker is all about creating a controlled and reproducible environment, _precisely_ to be independent from the host's, so the answer is: _it doesn't matter_.

Now there **is** one thing however, that should be considered: the version of the `docker-engine` software. Docker is fairly new and thus moving rapidly. Some distributions have a stricter policy for upgrading packages and very often, the default package provided by your distribution is outdated. So _for now_ you should _really_ install the latest release of docker, grabbed from their site.

### Linux

The installation on Linux is fairly easy, especially on the most popular distributions. The [docker documentation](https://docs.docker.com/engine/installation/linux/) page covers the installation. Basically it boils down to installing the packages fetched from the docker site, and installing the necessary driver for storage.

By default, docker needs to be run as root, and every subsequent commands too. Since this is a pain,the `docker` group is available, just make your user a member of this group with `usermod -aG docker <user>` (run as root), log out and log back in to make it effective and you are good to go.

### Max OS X

The Docker for Mac project has been moving _a lot_ lately, it is changing very often. I'm not using Docker for Mac, so it would not make much sense for me to tell you how to install, for now, as far as I know, the updated doc is [here](https://docs.docker.com/docker-for-mac/), so I suggest you follow that link.

### Windows

For the Windows users, there is a project in beta (same as Docker for Mac), I have not been following it because I don't develop on Windows, so your best chance is [to follow this](https://docs.docker.com/docker-for-windows/) and hope for the best.

Understand that Docker runs on the Linux kernel, so the Docker for Mac and Docker for Windows are ambitious projects, still in beta phase.

## Docker Fundamentals

Okay, that was the boring part; but now that we have Docker installed on our system, it's time we talked about it!  
**That's the part where you should start paying real attention** :-)

The first thing we need to understand is that Docker follows a daemon / client model. Every command we will call makes use of the docker client. That docker client sends the command to the docker daemon, and it's the one doing the hard work.

"Why?" you should be wondering. Well in order to understand why, let's examine a _slightly_ bigger picture, and see _how_ the client and the daemon communicate.  
The way the docker client and daemon exchange messages is through a `socket`, classic. Most of the time---this is the case if you have just installed Docker and did not do any fancy configuration---they communicate through a Unix socket or file descriptor.

Now something interesting should begin to pop in your mind (don't worry if it's not), since the client and the daemon communicate through a socket, it should be possible to separate them physically. And indeed, this is a **huge** deal in serious Docker setups: you can configure your docker daemon to listen to a TCP socket on your server, and you can talk to this remote with your local docker client.

Confused? Let me say it again: suppose you have a remote server, hosted "on the cloud"---seems to be the cool word these days. You install docker on this remote server and you configure the docker daemon so that it listens to a TCP socket (an open port on your server and firewall).  
Now on your laptop or local computer, you also install docker, and you configure the docker _client_ to send its commands to the remote host's (address, port) pair. In this fashion, you can administrate the Docker on your server from the command-line on your local computer. This comes in very handy to administrate several remote servers.

### Images vs. Containers

Now that we have understood the Docker daemon / client model, let's talk about the _most confused concept of Docker_ (in my opinion). This is **the** topic that is at a vast majority of problems beginners have.

I am talking about those three magic words: **images**, **containers** and **Dockerfiles**.  
What are those, and what's the problem with them?

This whole thing is actually fairly **easy** to understand, you just need to pay attention and make _sure_ you _understand_ and not have that feeling of "yeah yeah I kinda see the story here".

Now that I have warmed you up, here it is:

> Dockerfiles are used to build images

That's it. Seems like nothing, but here it is again: _Dockerfiles are used to build images, **not containers**_.

Stay with me, I'm aware it doesn't seem like a big deal right now, and it (probably) doesn't make much sense.  
Let's talk about all of that in details.

The first, top-most things that Docker is concerned about are **images**. If you come from Object-Oriented Programming (OOP), you can think of an image as a class. The same way a class in OOP is nothing in itself, a Docker image doesn't _do_ anything; it is just built and stored. It's nothing more than a model for your containers.

The _Dockerfile_ is simply the _recipe_: it describes _how_ to _build_ the _image_. It's no chance that "Dockerfile" sounds much like "Makefile", because that's really what this is: a "Docker - Makefile". And as we will see later in the article, `build` is the command we use to... build an image.

![](/images/warning.png "") I know that this all sounds trivial and you don't think you are learning much right now or that what I'm talking about is a big deal, but even though you do not realize it yet, I am introducing _crucial_ vocabulary _right at this moment_.  
I suggest you scroll back a few paragraphs, and read again, pay attention to _every vocabulary word_ I used. Trust me, this will make your life much easier, _especially_ when you will be asking for help in forum, Stack Overflow or IRC.  
When I used "build", this is what I meant, **not** "create", "start" or something else. Likewise, I will use "create", "start", "run" later, these all have a _specific_ meaning, so make sure you notice them :-)

Let's get back to our OOP analogy!

Now what do you do when you have a class in OOP? You create _instances_ of that class, which are called "objects". Well in Docker, this is _the exact same thing_: from an image, you create **containers**.  
Said differently,

> A container is a running instance of an image

If you are not familiar with OOP, consider it like this: what do you do from a model, a set of blueprints? You replicate it, you create objects based on the blueprints. Well this is what containers are to images.

If you think you understand, good! Take a break (really, get the hell out of that screen for a while) and when you come back, scroll back a screen and read all that part about containers and images again. Really, you _have_ to, because it seems so simple, that I **know** for a fact, that you want to _skip_ it, and go on. But you will end up coming on IRC and ask "Can I do XXX in my Dockerfile?" and I will most likely answer "This does **not** make sense".

And then I will probably tell you this---so I decided to write it here, so it serves as another layer of explanations----what all of this means is that we are talking about two **different**, **separate**, **unrelated** times here.  
There is the time of Dockerfiles and images: this is the _build time_. And there is the time of the containers: this is the _run time_.

Why? Because an image is _built_ while a container is _run_---see what I meant about using specific vocabulary?.

Let's see a concrete example and play with images and containers. For now, we will not build our own image---we **will** cover is later, though. We will use the official Ubuntu image for now.  
Let's download it first: `docker pull ubuntu`.

Hey hey hey! This was our first docker command! This is great. If this was not obvious, `docker pull` is used to fetch ("pull") an image from...  from where? By default, when you don't specify anything---like we just did---it pulls from the _Docker Hub_. This is a big repository hosted by Docker where dozens of people can push their newly-built images. You will be able to do it too, after reading the articles.

#### A small, But Crucial Parenthese
Notice something interesting in the output of your terminal: there are some talk about "pulling layers". This should ring a bell: from [Part I](/articles/2016-05-26-Docker-Taming-the-Beast-Part-1.html) we talked a bit about layers.  
Well, the guys at Docker are very smart and they did not design images as a big blobs. Instead an image is comprised of stacked layers.

"What's the big deal?" you might be wondering. Well I should have said: "an image is comprised of **reusable**, stacked layers". This is much more interesting indeed: it means that layers in images are _shared_ and _reused_ when possible.

Suppose you build a 500 Mb image for your development environment. Now you need a _slight_ variation of the environment to try a different version of a particular software: this new software is 1Mb in size. So you create the same image, but add this 1Mb file in it. Well if images where dumb blobs, you would now have two images in your computer, and a total used disk spaced of 500 Mb + 501 Mb = 1.001 Gb. This is ... costly.  
It's safe to say that your little 1 Mb file cost you 501 Mb of disk space.

But because the docker guys are smart and designed images as reusable layers, when you build your image, docker "sees" that most of it (the first 500 Mb) are already present in the system, so it only adds the 1 Mb layer.  
You end up with two images, but 500 Mb + 1 Mb of disk used. Pretty smart!

Anyway, before we head back to our experiment, just remember that when you pull an image, you are actually pulling all the layers that make up this image.

So we have just pulled the Ubuntu image, is it running yet? Try to answer before reading on.

Two possibilities from here:

1. you thought either "yes I think this is running" or "no I don't think it is running yet"
2. you scratched your head (and possibly started insulting me)

Well if you in the second category, **I congratulate you!**, and even more if you thought about insulting me because I was talking nonsense (did you _actually_ thought I had slipped here?!). If you are, however, in the first category, I **highly** suggest you scroll back two or three screens and start reading about [Images vs. Containers](d) again.  
Because I **was** talking nonsense: what we have just pulled is _an image_, it is inert, it serves as a model. Am image doesn't "run", doesn't "execute". We have just acquired blueprints; we haven't built anything with them yet.

Now it's time to create a container from this image. Do `docker run ubuntu`.

Tadaaaa!

Well that was disappointing... What happened here?

Lots of things happened actually: when we run `docker run ubuntu` we instructed docker to create and start a container from the image `ubuntu`. What it first did is check if it had that said image.  
In our case, it did, since we had just pulled it moments before.  
So then it created a container from this image.

What does this _mean_ and how can you verify that?

Let's start with the latter question, as it is the simplest. In docker, we use `docker ps` to list the containers. go ahead and do it.

"What? Are you kidding me? It says here that there is nothing!"

I know, I forgot to tell you that `docker ps` lists the _running_ containers. If you want to list _all_ containers (so "running" and "non-running" containers), you have to run `docker ps -a`, the `-a` is, as in many Linux commands, short for "all". Now go ahead do it, you should see your container.


**Just a side note:** `docker ps` suspiciously looks like `ps`, which a Linux command listing _processes_. And once again, this is not by chance, and I'll use this side note as an opportunity to tell you my favorite Docker sentence:

> A Container is Just a Process

It is so important, so _fundamental_ that I suggest you actually write it down to a post-it and stick it on your computer's screen. For the first two months when I used Docker, I literally had that post-it snapped on my screen. For real.  
As I usually say on IRC, "_90% of the time, when you [as a beginner] have a problem with Docker, read that sentence again, and it should solve it_".

Back to our container, why did we have to use `docker ps -a` which lists all containers (so in our case our _stopped_ container) to see it?  
Well it's because it is stopped, really. Now the real question is: why is our container stopped, then?

"_And that, my friend, is the right question_" (bonus point for those who caught the reference). The truth is: I have already answered this question, but totally indirectly. And you need to be _very_ good to find that out.

I answered it when I said that "a container is just a process". Let's look again at how we started our container: `docker run ubuntu`. Decomposing it, we have:

- `docker`, this is the docker client, ok.
- `run` is the docker command we use to instantiate a container from an image and run a command inside.
- `ubuntu`, this is the name of the image _from which_ we instantiate the container.

And... where is our command / process? There is none! Indeed, this is why our container is stopped: since we did not specify anything to run into, well... it did not run anything.

Okay I sense that some of you might get confused at that time, and this is perfect, because it allows me to revisit something we have talked about before.  
The reason you might be confused right now is because your intuition makes you consider docker containers the same way you consider virtual machines.

When you have a virtual machine, you start the supervisor (_e.g._ Virtual Box), select your machine and click "start". Then your virtual machine boots up, starts and then you are ready to use it: you're _inside_.

Docker containers are **not** like that, **at all**. This is what I was talking about with the post-it, go read it again. In my very early docker days, someone on IRC told me a slightly modified version of my favorite docker sentence: "_A container is just a fancy way of running a process_". And I think this captures the idea very well: do not overthink containers. Containers are just a slightly different (_improved_ one may well say) way of running a process. When you run a process inside a docker container, it's the exact same thing as running the process outside, only you add some isolation, and everything we saw in the first post.

So back to our `docker run ubuntu`, we created and started a process from the `ubuntu` image, but we did not actually specify which program/process we wanted to run; so it doesn't run anything. It is as if, on your terminal you typed `exec`. Okay, you wanted to run a program... but which one?! It's the same thing happening now.

This allows me to highlight another specificity of docker containers: they stop when their PID 1 process terminates. This is important: when the process you are running inside your container exits/returns/terminates/crashes, the docker containers stops, because it has nothing to do anymore.

Here it is, I hope I was crystal-clear about the differences between images and containers, because that is arguable the _most important part_ of docker for a newcomer. If you have _any hesitation_, **do not** pursue further: take a break, re-read this part and [shoot me an email](mailto:ns.schoe@gmail.com) if you have to.

![](/images/warning.png "") I have been using the class / object analogy quite a lot, but this is just a simple analogy to give you the feeling. It doesn't mean that they are implemented in terms of class and objects. Besides, don't push the analogy further than what I intended: do not look for concepts of heritage, polymophism, etc.  
It should be obvious, but one is never careful enough and I just wanted to highlight that.


### Docker Networks

#### Introduction
Okay, this part, some people might tell you that it is not a docker _fundamental_ and therefore should not be in it. In essence, I think they're right, but the _facts_ are: docker has a History and with History comes reflexes. Some of these reflexes are now deprecated and _bad_, people should not do it anymore.  
But people _still do it_, and I see people coming on IRC asking about a problem that _clearly_ shows they lack updated knowledge about docker networks.  
Besides, docker networks can solve some tough problems _very easily_ and thus I've decided to include it in the fundamentals.

That being said, I will only describe and show some very basic examples here, sort of like an introduction. After all, the goal is not to make complex stacks in this part, it is to introduce you to the fundamentals, meaning "the thing you will use all the time".  
Then I will make more advanced articles focusing on networks.

#### Docker Networks: Why and What?
So what is this "docker network thing"?  
Let's consider what we have learned about docker right now: it can execute processes in an isolated manner, meaning it hides the filesystem and the other processes.

That's good, but very limiting: complete isolation doesn't really serve us; how do we make our processes communicate with each other? Here come the docker networks.

Suppose we are running a basic webserver infrastructure, comprised of:

- some static HTML files we want to serve
- an nginx acting as a Web Server
- a postgreSQL database to store data

**Some notes about this:**  

- First, it's an example, so if you're more used to apache instead of nginx and/or mySQL instead of postgreSQL, you can swap those terms, it remains valid!
- Seconds, it's an example(!), so the accute reader might wonder what we can _possibly_ store in our database if we only have a static website. This is true, but I don't want to introduce another difficulty by talking about PHP or another backend. Let's keep this simple and imagine that we _somehow_ fetch data from the database.

Okay so what's the deal?  
Docker is about containerization, isolation, and so we will containerize! The principle is to start container#1 that will run the nginx instance, and container #2 that will run the postgreSQL instance.

So in this setup, we would have containerized / separated the instances. If hypothetically, if the nginx server is compromised or crashes, it will not affect the database, which is pretty awesome.

The problem in this setup is: the nginx instance has _no way_ to know about (let alone _contact_) the SQL instance, running completely isolated. We want to break that isolation _in a controlled manner_: this is what docker networks are for.

The principle is very simple: we will create a private, isolated network of which the nginx and postgreSQL instances will be a part. Since they will be in the same virtual network, they can see and talk to each other; and since this is a private network, other containers will not be able to see them. This is very handy.  
You can think about it as a VLAN between the two containers :-)

#### How is This Done?
Ah! I'm glad you asked, because it's important to have a least a notion of the things going on.  
I won't describe down the very low-level, but enough so that you can have a pretty solid intuition about docker networks.

When you installed docker, if you were curious and looked at your network interfaces (id you did not, do it now), you should have found a surprise: run either `ip addr show` (if you are modern) or `ifconfig` (if you are an outdated caveman).

You should see a _new network interface_ named `docker0`. It's like your had plugged in another network card, only this is a _virtual network interface_ handled by the kernel.  
This interface is said to be a `bridge` (this is the official term). Why? Because it acts as a "bridge" between several interfaces. At the moment, it doesn't bridge anything, but it will eventually.

Said simply, bridges are ways of grouping several network interfaces in one. What this will do is group all interfaces in our docker containers into one, `docker0`: this is how our containers can have Internet access!

Run `docker network ls` (this is a new command by the way!). You should normally see 3 networks:  

- none
- host
- bridge

The `none` network is when you want to explicitly disable networking for a container. In this case, it will specifically not use the bridge.  
The `host` network is a bit special. What this does is un-containerize the network part of the container. Basically, it makes all networks interfaces of the host available to the container directly. You are not likely to use it, unless you have a very specific use-case.  
The third, `bridge` is the default private network every container joins when you don't specify another network. There is a specificity however that we will revisit in a later article, but I'll say it here anyway: even though they are part of the `bridge` network by default, the containers inside it are neither _reachable_ nor _discoverable_ by their name; which means you need to use their IP to contact them.  
I won't say more for now because that implies some other things and I will receive that for the article about networks.

Back to our network interfaces. The idea is that when you create a container that is part of this bridge network, you will create yet another _virtual network interface_. From inside the container, this network interface will be named `eth0`! For it, this is a normal, Ethernet-based network interface.  
From the host's point of view, it will be a virtual Ethernet interface and will have a name something like `veth-xxx`. The "veth" part if of course for "virtual Ethernet", and the `xxx` is a unique name.

Since I said that every container you create---and for which you do not specify a custom setting---will join the default `bridge` interface, it is not very secure or private...

This is why we will actually create our _own private, isolated bridge-based network_ and will use _that_ network for our setting.  
That was a lot of words, let's check their meaning one at a time, to be sure we don't have _any_ obscure part:

- "private": means that we will explicitly choose which containers join it and thus won't be polluted by other containers
- "isolated": means that the containers which are not part of the network won't be able to see, reach or communicate with our containers inside
- "bridge-based": this is tiny bit more complex to explain right now, but it has to do with the fact that there is a _fourth_ docker network type that we have not talked about yet: `overlay` and this is to highlight that it's not one of this. Overlay is _by far_ the most interesting docker network, and we **will** definitely talk about it in the article about networks!

Although this doesn't have any crucial implications, make sure you understand the difference between "private" and "isolated". The former states that containers won't implicitly join the network, the latter states that containers outside and inside the network can neither talk to nor see each other.

#### A Bit of Practice
You won't be entirely satisfied now---I know it---because I will only show you administration commands and not how to use networks. But this is because we haven't yet see the basics of containers manipulation. So be a little more patient, keep reading and you will be pleased later!

As you might have guessed, `docker network` is the command we will use to interact with docker networks. `docker network --help` will give you a list of subcommands it accepts---this is also true for every docker command.

We can list the networks that we have created with `docker network ls`. As always with docker, you will see both a "Name" and "ID" column. The name is the name you give when you create a network. But docker uniquely identifies objects (whether it's networks, images, containers or volumes---I will come back on volumes later, don't worry---) by IDs, so you can always replace names with IDs in your command. Unless in the "create" commands of course: you cannot choose the ID.

We can create a network with `docker network create my-private-network`. Again, some help on the subcommand with `docker networks create --help`. This is golden, I should, once again, congratulate the docker guys for their very nice documentation.

Okay, so now that we created our network, you can check it with `docker network ls` again. As you see, there is a third column, "driver" which states the driver---or "type"---of the network. As I said before, without any additional options, new networks are created with the "bridge" driver.

What can we do next with networks? Delete them of course. Easy: `docker network rm my-private-network`. Easy.

**Two important notes**:

- You can delete the networks you created, but it's not possible to delete the three default networks: `none`, `host` and `bridge`.
- you cannot delete a network if there are containers connected to it, you have to disconnect them first.

Okay but... how do I do the second point?

Easy: `docker network connect my-private-network my-container` and `docker network disconnect my-private-network my-container`. Mind the order: first the network, then the container.

### Making Data Persistent

Okay that part is **very** important and not everybody gets it, partly because people tend to skip the basics and want to use docker as fast as possible and partly because this is relatively new, even in docker (there was another way of doing this, but it's now deprecated; at least it should be). So I aim to change that and make it crystal clear.

Again, this is an article about fundamentals, so I will mainly _talk about_ and _explain_ the possibilities, but not show you a full-blown example just yet. I know this is frustrating, but docker being very powerful, you can do _a lot_ of things with it. And I have material to make a complete article for every aspect I introduce here.  
This is what I'd like to do, and not a very sparse, incomplete little paragraph.

#### The Problem
OK, here we go, what's the problem?  
Let's remember [part I](/articles/2016-05-26-Docker-Taming-the-Beast-Part-1.html) where I was talking about layers, remember?

I'll recap the idea phrasing it in another way, which should help clarify any remaining doubts. This is highly related to [Images vs. Containers](#images-vs.containers).

What's an image? We saw that an image was a custom _environment_ that we created (or other people created) like we want it to be: _i.e._ install necessary tools, softwares and libraries, define custom environment variable, create custom directory architecture, etc. This is a Linux environment shaped like we need it to be.

From such an image, we instantiate---or create---_containers_ which are running instances of this image---think objects instantiated from class in OOP.

But we already saw that images were not stored as big chunks of obscure, binary data, but rather as _layers_, small, potentially-reusable parts of images, which stack up a little like `git diff`s. This allows for smart re-usability and space saving.

Alright, but we never really talked about what a **container** actually was. How do you make a "running instance" of an image? The answer is very simple and uses the same idea that images do: they use layers.

Here is how it happens: when you have an image, which is composed of several layers (three in the example below), they are stacked like this:

```bash
----------------------------------------
|              LAYERS #3               |
----------------------------------------
|              LAYERS #2               |
----------------------------------------
|              LAYERS #1               |
----------------------------------------
```

The second layer is based on layer 1 and makes some changes, then layer 3 is based on layer 2 and make some other changes. Alright, that is an image.  
All these layers are **read-only** because together they make an immutable image. You can't write or modify any of these layers.

Now witness the magic behind creating a container out of this image:

```Bash
----------------------------------------
|              RW LAYER                |
----------------------------------------
|              LAYERS #3               |
----------------------------------------
|              LAYERS #2               |
----------------------------------------
|              LAYERS #1               |
----------------------------------------
```

Wait whuuuut?!  
No really, this is _that_ smart: a container is simply an additional, **read-write** layer on top of the image's layers. So yes, a container is based on an image, but _any_ modification you make (write a new file, modify an existing file, add user, remove files, create a new user, etc.) goes into a separate, read-write layer. This is _very_ smart because once again: you share the space again. If you have an 500MB image right now and instantiate 2,000 containers of it, if you don't write data or modify anything, your disk space has not changed!  
This is because each image use the _same_ read-only image's layers, and currently their read-write layers---also called the container's layer---is empty.

See how it compares to copying $2,000 \times 500Mb$? This is awesome. Take some time to appreciate the beauty of it.

So please _always keep that in mind_: whenever you create or modify data in a container, it goes into this layer's, read-write layer. By the way, just as a reminder, all of this is rendered possible by the union filesystem.

#### So You Were Talking About a Problem?
Yes yes, I'm coming to it!

Say you create a container (we briefly saw that it was done with `docker run <image-name>` in a previous part, but we'll come back to it in more details later), then you write some data in it: `touch test.txt` at the root `/`.  
Running `ls` will give you several directories---the typical ones you find at the root of a Linux's system, `etc/`, `home/`, `usr/`, etc.---and `test.txt`.  
If you followed, you know that `test.txt` is in the container's layer (note that from now one, when I use "container's layer" it means the top-most container, which is read-write and sits on top of the image's read-only containers).

Now I hope you have not forgotten that

> A Container is Just a Process

and so there must be a process running in the container (otherwise it would quit). For our particular example, it doesn't matter and has actually nothing to do with making data persistent, but let's always keep in mind that a container must have a process running inside, so let's say, for the sake of it, that an `nginx` HTTP server is running ans forget about it.

Now suppose you stop the container---either because you issued `docker stop <container>` from the outside or because your `nginx` crashed for some reason. So the container is stopped: it's not a running process on your host anymore.

Well, let's start it back with `docker start <container>`, if all is okay, this container should start. And if you run `ls` at the root, what are you supposed to have?

Yes, same as moments ago: the structure and the `test.txt` file. I am insisting on this point because in my early docker days, some people told me the contrary and it misled me---in other words, they told me that once I restart my container, my data inside it will be lost; **this is wrong** as we've just confirmed it.  
Why is it the same, then? In other words, why is our file/data still here?

It's because we only **stopped** the container: we just made it "non-running". The process inside the container was killed (either gracefully or brutally). But in our host, the container's layer is still present, it still has data in it.

So let me say it once and for all:

> Stopping a container keeps your data

And that's perfectly logic afterall. Don't let anybody talk you out of it: you can have 10GB worth of data in your container, you can stop and/or kill it any number of times you want, when you start it back up, the data will still be there.  
Because the container's read-write layer will still exist and be used by your container.

"What's the fuss about 'making data persistent' then?"  
I'm coming to it.

Well, here is the problem: docker was made so that---and **you** should **always** have that in mind as well---

> Container Should be Ephemeral

What does this means? It means that at any moment, you should be able to **destroy** a container---I did not say "stop", I said "destroy"---and recreate the container, with little to no consequences.

We saw that the hard and heavy step was building the image through the Dockerfile, but once that image is built and stored in your host's hard drive, it's instantaneous to create a container from it; or a thousand. There's a reason this is so fast: the _implementation_ reason is because it's just _firing a process_ and the _logical_ reason is because containers are---and should be---only processes. It's a utility, it's a software.

What happens when you destroy a container---pay attention now, we will often use "stop" and "destroy" containers, _this is not the same thing_---?

Destroying a container is easy: first you need to stop---or kill---it. If you try to destroy a running container, docker will insult you, telling you it's not nice to kill running container, it's like killing little puffy kittens, you know?  
So stop it before. Once your container is stopped---data is still there at this point, remember---destroying it is just a matter of deleting its container layer. Simple!

And _now_ the data in it is gone: it was in the container's read-write layer, and we have just deleted it!

#### The Solution(s)
So now you should be confused: how can we keep data? Because honestly, you cannot think this is a limitation of docker!

The fact that containers should be able to be destroyed and recreated on demand seems to contradict that data is stored in the container layer.

Remember that pretty much everything that has to do with files in docker is implemented with a union filesystem ("unionFS")? Well this is both very good for all the advantages we've seen so far, but this is where it begins posing problems. So what we'd like is the ability to bypass that unionFS, and Docker gives you three ways to do that.

Now that's another area where some people and I disagree on IRC---not so many, don't worry---Docker provides you with **three** ways to bypass the unionFS, and they are meant for **three _different_** use cases, they are **not** equivalent!

Either you are in use case #1 and use solution #1, either you are in use case #2 and use solution #2 or you are in use case #3 and thus use solution #3. But you have to know.  
Note that the three solutions are compatible, so you _can_ be in use cases #1 _and_ and #2 and use both solutions---or any mix for that matters!.

Here are the three use cases:

1. you need to make **data persistent** and by that I mean that you need to keep your data even after your container is destroyed and you want to be able to recreate a container that can use this data
2. you need to **share data with your host** and by that I mean that you need your container(s) to access files or directories that are on your host and/or vice-versa---remember that the unionFS shadows the host's filesystem and was designed _specifically_ to prevent you from doing this.
3. you need to **share data between containers** and by that I mean that you need several containers to be able to read/modify the same data, without the host having anything to do with that.

You need to carefully consider these 3 use cases, and understand how they are different, and when the time comes, know which one(s) you are into.

I will now describe the solutions, and give you a basic example of when you are in this use case.

Let me begin with the solution #2, because it's the easiest one.

#### Sharing a Host's Directory

Suppose you are building an image to test your code: so that you can compile, execute and test your code in a contained, controlled way. You have your code in `/home/nschoe/workspace` (I'm talking about source files, like `*.c`, `.hs`, `*.js` files, etc.) And you want to compile and test it.  
When we see Dockerfiles later, you might be tempted to simply `COPY` the `/home/nschoe/workspace` directory inside your image, so that each time you create a container from that image, you have a new copy of the code. This might work indeed, but the problem here is that if you test the code inside the container, find out there's a bug and change it on your host, then you have to _rebuild_ the image, because the code changed. This is very inefficient because remember that building an image is the hard and long part.

No, the good solution is to build your image with all necessary tools (`gcc`, `ld`, `valgrind`, `ghc`, `nodejs`, etc) to build and execute your code and then _share the_ `/home/nschoe/workspace` directory with the container.  
Note that it's different than having a _copy_ of it, I'm talking about sharing (_i.e._ giving them both [the host and the container] access to the _same_ files).

In docker, this is called _mounting_ the directory. And you can think of it exactly like mounting a partition. The idea is that you will _mount_ the host's directory `/home/nschoe/workspace` into a _mountpoint_ inside the container, for instance `/app/code`. You _can_ chose the same mountpoint---thus mounting is to `/home/nschoe/workspace` inside the container, but you don't _have to_.

To do that, it's the `-v` option of `docker run`. Something like this: `docker run -v /home/nschoe/workspace:/app/codee <image-name>`. As you have guessed, the syntax is `-v /path/on/host:/mount/point/on/container`.

So from now one, both the host and the container share the _same_ data:

- for the host this data is located in `/home/nschoe/workspace`
- for the container, the data is located in `/app/code`

Be **very careful** with this, because as they share the data, any modifications from one will affect the other. We are **bypassing** the protective union filesystem. In our case, this is very handy: if we found a bug when compiling or testing our code in the container, we can have our favorite editor opened, modify the file `/home/nschoe/workspace/Main.hs`---for instance---and it will be immediately changed in the container; no need to restart it, or do anything: it-is-the-same-data!

But this can be very dangerous too: what if we delete a file---or the whole directory !---from inside the container? Well congratulations, you've just lost your entire code!

For this reasons, you can---and I strongly encourage you to do so---make the mount read-only, like such: `docker run -v /home/nschoe/workspace:/code/app:ro <image-name>`. Mind the `:ro` for "read-only". In that case, it all works exactly the same: when you modify files on the host, the change will be effective immediately in the container, **but** your container cannot modify/delete/erase anything in his `/code/app` directory.  
Think of it as mounting a cdrom (yes that existed, okay!) or a protected usb key: you can see and access the content, but not modify it.

Sharing code with your container for execution is one common use case, sharing `/etc/localtime` with your container is also a common use case, to set the time. But you may find some others!

#### Named Volumes

As for solution #1, we've talked about keeping data persistent with a database storage. Let's use postgreSQL. Typically, when you have a software that needs to talk to a database, you will use at least two containers.  
We can take the example of a webserver (_e.g._ nginx) which talks to a PostgreSQL database. Let's focus on the PostgreSQL instance. It's a software (a SQL database server) which stores some data (the database data). That data better be persistent because that's PostgreSQL's job!

But said like that, it seems to break the assumption that containers should be able to be destroyed and recreated at any moment. To fix that, we will separate the process running the psql server and the actual location of the data. We'll use a Named Volume.  
The syntax is _almost_ the same as before, with one minor difference: rather than using a path in the first part of the mount command, you specify a name, so you would do: `docker run -v my-website-data:/path/on/container <image-name>`.

What the above command does it create an instance of image \<image-name\> and mounts a Named Volume at location `/path/on/container` in the container. The analogy with a partition is event more valid in this case: in this case, every data written on `/path/on/container` inside the container will bypass the unionFS and be carried out in a special directory on the host. When you destroy the container, the data will be safely kept somewhere on the host, and the Named Volume "my-website-data" will still exist with its data in it. And you will be able to create another container that uses this Named Volume.

This is very handy, if you want to update your container for instance. Let's suppose you'd like to activate a new option in PostgreSQL file, change it's `pg_hba.conf` conf file, or update the PostgreSQL version; you will need to destroy that container, update the Dockerfile (or use a newly-created PostgreSQL image from the docker hub) and then recreate this container, instructing it to use this Named Volume so the data gets automatically restored!

Isn't that awesome?

You need to precisely understand the difference between sharing a host's directory and using a Named Volume.

There is nothing "magic" about all of this, when using a Named Volume mounted to a mount point inside a container, all it does is instruct docker **not** to use the union filesystem when writing to the mount point, but rather to directly write on disk. Even if you don't _need_ to know, I will tell you where those Named Volumes are stored in your host's directory, because I hate when there are some "magical" things happening.

All your Named Volumes are stored in: `/var/lib/docker/volumes` by default. So there should be a `/var/lib/docker/volumes/my-website-data`, this directory---and every Named Volumes directory---contains exactly one folder, name `_data` which contains the data in the mountpoint.

Now be **very careful** here: I told you about this location so that you understand what's going on---making data persistent is not _magic_,it's simply storing data in a known location and bypassing the unionFS---but you should **never**, **ever** edit, modify, write, delete from the folders in `/var/lib/docker/volumes`. Never. Treat that as black boxes, well actually _gray_ boxes since I've explained to you how this works.

If you have been playing with docker more than this article does, you might notice some weird, very long names in `/var/lib/docker/volumes`. Yes these are volumes too. (Spoiler for other articles: you can see all your Named Volumes with `docker volume ls`; mind that it's `volume` (singular) and not `volumes` (plural), this is weird and inconsistent with `docker images`, but it's like this).

Those weirdly-named volumes are called "Anonymous Named Volumes" (which is a weird concept ^^). It happens when you ask a container to use (create) a Named Volume, but don't specify the name, something like `docker run -v /path/on/container`, here you see that there are neither a path nor a name before the mountpoint. In this case, the docker daemon generates a unique hash and use that a the name for this volume.  
It's exactly as if you had done `docker run -v 792e7d8e336b133e1675b24c0ead99605e62a98ad30fdd107200b5be3c9db3658:/path/to/container <image-name>`, only you did not chose the `792e7d8e336b133e1675b24c0ead99605e62a98ad30fdd107200b5be3c9db3658`.

#### Sharing Data Between Containers

It's time to think about solution #3: sharing data between containers. What could be a use case for this and how does it work? It's very simple.

Suppose we are inside a company that produces code. We need a way to centralize this code and do versionning, we'll use git. We want a git repository, similar to github, but private, internal, something like gitlab. Don't overthinkg this if you don't know gitlab, it's not where the interesting part is, for the sake of it, gitlab = github on local network.

So the developers in your company create some code, and upload it to your gitlab server when they use `git push`. Now, the interesting part: since you are a very concerned developer leader and care a lot about your code being robust, you want automatic testing done when people upload code to your gitlab account.  
So the workflow is as such: a developer uploads some code with `git push`, it's stored in your gitlab, then _automatically_ you perform some tests on the code, and depending on the success or error, you either accept the push or reject it, possibly sending this developer an insult-filled email.

That's the workflow, now the pseudo-implementation. Since you've understood the concept and importance of Named Volumes, you use a Named Volume to store the actual code in your gitlab container, so that you can safely update gitlab when a new version comes out. Let's say, for the sake of this example, that the code repository is stored in `/var/gitlab/code`. What you would do is create the container with the option `-v gitlab-code:/var/gitlab/code`. Now you're safe.

What about the automatic testing of the code, we _could_ make is run in the same container, but we are going to use another docker container, in this context, it makes more sense. So the principle is to run another container, that will read the newly-pushed code, test it, and send an email to the developer when he pushed some broken code.

How can this container access the code, because it's currently stored in a Named Volume but should work in harmony with the container running the gitlab server? One possibility is to run it with `-v gitlab-code:/path/in/container`. But that's not what we are going to do, for several reasons: first it's a pain to write the same command twice (in this case, "option"), and second, suppose the gitlab server uses 10 Named Volumes (maybe one Named Volume per project), then we'd have to re-write the 10 `-v` options!

No way! And semantically, it would make more sense to say "I want to use the same volume as this other container". And that's exactly what docker allows us to do: we will run our second container with the `--volumes-from <first-container-name>` option.

It's rather verbose: it means to use the same volumes as the specified container, at the same mount points. So in our second container---the one we will use to make code testing---we will have the available code at path `/path/in/container`. If the first container has 10 Named Volumes, at 10 different mount points, we can re-use all of them, with one, simple option: `--volumes-from`. Awesome!


#### The Plague of So-Called "Data-Only Containers"

Let's stop playing now; and let's start being serious for a moment. I'm about to talk about something that I don't want to talk about. It's a plague, a real one.

I want to be extra clear on that: I don't want to talk about Data Only Containers, because it should not exit anymore. It's a concept about the past, the ancient Docker times (which in actual time is still very close ^^). It's like talking about floppy disks in 2016, do you get the idea? The concept is still valid (floppy disks still work and still store data), but you would never use it today, would you?

I have hesitated a long time before deciding whether or not to write about them, but it's still heavily talked about, still documented on websites, and _unfortunately_ still used by people (and not only beginners...).  
So I've decided that it was best to tell you about it so you can **not** use them, rather than hiding it from you, leaving the chance that you will use it, not knowing how bad it is.

Let me be clear: I'm talking about them because you might be using them, and in this case I'd like you to understand how much and why you are wrong, you may have heard the term and be wondering whether to use it or not, or you might simply be curious and want to know about them.

So here I am, I'll make a small paragraph about them, but I want you to "forget" all about it immediately afterward ("forget" as in "_never_-use-it-but-keep-it-in-mind-so-that-you-know-what-not-to-do").

Data-only containers is what people used to make data persistent before the Named Volumes API were functional. The idea is simple, albeit a bit twisted: if you are designing a custom image, you write your Dockerfile, put the instructions you want to build an image from it and use a `VOLUME` statement. That statement will create an Anonymous Named Volume (which is a Named Volume with a hash for a name) containing the data you want to have persistentwhen you create a container from it.

Now the _trick_ is to create a container from that image (read: from the image built from this Dockerfile) and do nothing with it: don't specify any process running into it. Since you did not specify a process to run, the container will simply stop---like we've seen before. Yes but there's a catch: when you "started" it, it did create its Anonymous Named Volume. And the idea behind all of this is that now, to create your "real" container---the one you will have running, you will use the `--volumes-from` option.  
Doing this will make it so that the same volume will be used for running your container. When/if you destroy and recreate this container, provided you re-run it with this option, your data will be safe.

So really, this idea is bloated, I perfectly understand that it was needed at some point, but now that we have proper Named Volumes, this should be banned, so please, don't do this.

Okay, so that was a pretty big part, but the _Docker Fundamentals_ that we have learned here are very important, even if they sound boring or abstract: I have laid some bricks in your mind that we will reuse in more in-depth articles and I did my job correctly---and you read carefully, this will be much easier.

You should probably take a break now, because you need to make a _context switch_ in your brain to let it process what you have learned so far---I know this sounds like a pain in the butt, but knowing when to take a break is an integral part of learning.

## Interacting With Images

It's now time to take a little step forward and see _slightly_ more concrete examples, in this part, we will learn how to interact with images, so we will see some docker commands (yay!).

As we know by now, an image acts as a model or base instance from which we can instantiate---or create---containers. I know I've said this several times already but let's see it again:

- there is a **build time**: it's were the image lies---and its Dockerfile. This is considered the heavy part, where it takes some time to build.
- there is the **run time**: it's the container, it is created **from** an image and creating a container is instantaneous: it can be---and actually is---done on the fly.

In this part, we will focus on the images.

### Listing Images

So what can we do with images on our system, what are some useful docker commands?

Well, first, it's good to know what images we have on our host, we can list them with:

```Bash
docker images
```

(_note that this is_ `images`_, plural; docker lacks some uniformity here, remember it was_ `docker volume`_, singular)_  
The output is pretty straightforward, on my machine it currently gives:

```Bash
$> docker images

REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
solita/ubuntu-systemd   latest              58676da6fce1        2 weeks ago         122 MB
nginx                   latest              0d409d33b27e        3 weeks ago         182.7 MB
```

Here you can find the image name (column `REPOSITORY`) and its unique ID. We will come back on this just now, let's have a quick overview of the other, more trivial fields.  
There is `CREATED` which gives you the date at which the **image** was created. I insisted on "image" because what it gives you is the date at which the image was built from its Dockerfile, **not** the date at which the image ended up on your computer (it's not the "last modified" timestamp on your file). If the image was created by somebody 10 months ago and you have just downloaded the image 10 minutes ago, the `CREATED` field will be set to 10 months ago.  
Then, `SIZE` is the size of all layers composing the image.

If you're like me, one, two or three things should be bothering you right now:

- why a field `REPOSITORY` rather than `NAME`
- why an `IMAGE ID`
- what's that `TAG` thing?

#### Why a REPOSITORY field and not a NAME field, Also What's that TAG Thing?

I could tell you that an image name is the combination of the `REPOSITORY` and the `TAG`, but that would piss you off even more---but it's true, so keep it in mind. While that doesn't explain the rest, it does explain why there isn't a `NAME` field: the image's name _is_ the combination of the `REPOSITORY` and the `TAG`, which are separated by a colon, so the two images names on the above example would be `solita/ubuntu-systemd:latest` and `nginx:latest` respectively.

Note that this is a technicality, most of the time---unless explicitly stated---when I refer to the image name, I will in fact refer to the `REPOSITORY` so I can say things like "the image named nginx".

Let's begin with the `TAG` as it is the simplest. The `TAG` is really a "version" of the image. For instance, if you search the docker hub for an image named "ubuntu", you will find several `TAG`s (versions): `14.04`, `16.04`, `latest`, etc. Please keep in mind that from now on, on docker syntax, a colon separated an image's "name" from its tag. So when I say that you will find three `TAG`s or version named `ubuntu:14.04`, `ubuntu:16.04` and `ubuntu:latest` it should be obvious what part is _actually_ the `TAG`.  
Please take a minute to familiarize yourself with that because from now on, I will take some language shortcuts.

So a `TAG` is really a "version" of the image. Suppose you build a custom environment for your software, so you install the needed libraries and dependencies. You might want to make two tags: `development` and `production`. In the `development` version you will install everything the `production` environment has, plus all `-dev` version of the libraries (I suppose here ubuntu is the base image) and some additional debuggers, valgrind, etc. The `production` version will only container the runtime libraries to distribute to clients.  
Since it's basically the same environment that you build, it makes sense to call the image by the same "name", and make it a different version. This is the first purpose of the `TAG` field.

The second purpose is to ensure immutability. Let me explain: we've already seen that docker was about creating some environment that you completely control: you know exactly what is installed and where. This is very handy: you develop and test your software in that controlled environment, then you can ship your code/app in that same docker environment to your client and you're guaranteed it will work.

Let's suppose you built your environment from the `ubuntu` image and it's 2015, so the ubuntu version is 14.04. Everything works well and you're happy. Now comes April, 2016 and ubuntu releases their 16.04 version. Surely some people will be interested to have a docker version of that. But in the same time it's not wise to "update" the image so that `ubuntu` is now a 16.04 system, because your code might very well break. So it's important that we have this `TAG` system. And unless you know what you are doing, when you write Dockerfiles to build some images, you should **always** specify the image's `TAG` from which you base your image.  
This is because everywhere in docker, when you refer to an image, if you don't specify its `TAG`, it's considered `:latest` by default. So if you want to download an image from the docker hub and you don't specify the `TAG` you want, it will download the `latest` tag.

This is all very nice, but what's this weird thing about a `REPOSITORY`? It has to do with several things, and I'm not sure which one is predominant, if any. So let's see them all.

First of all, if you hang out on the docker hub and search for some images, you will find a pattern: some images are just one name---like "ubuntu" or "nginx"---and some others are with a slash, like "solita/ubuntu". The reason is that _usually_, single names like "ubuntu" and "nginx" indicate that these are **official** images. So you "know" that when you download the `ubuntu` image or the `nginx` image, it's a docker image that was built by the official ubuntu or nginx team, so you should be able to expect a decent default configuration.  
Note that as far as I know, this is not enforced by Docker, there is no real guarantee, it's just that it seems to be a pattern, so take this statement with care and always double check. On the Docker hub, you have a a number of "stars" that were given to an image: the more stars an image has, the more it is popular. Likewise, docker tells you how many times this image was downloaded, so it's an indicate of the image's quality too.

When a user---like you or me---wants to upload one of its images on the hub, he could very well be uploading some shady shit, imagine if that guy named his image `ubuntu`: you would be thinking you're safe where you are in in fact using some unknown code. This is why you need to specify a "prefix" name. If you want to push on the docker hub, you first need to create an account, say your account is 'peroxide', then the images you will push should be named 'peroxide/image-name'. Another bonus is that it allows to direclty see all images uploaded by a same user: all of your images will be prefixed by `peroxide`, so that's another reason.

Note that on your computer, you can name your image however you want: you can build an image and name it `ubuntu` or `nginx` and this will be perfectly valid. It's just that you won't be able to push it on the docker hub, in order to do that, you will need to rename it.

So that's about it for now---we may come back to revisit this when/if we talk about docker registries, but this is a topic for later.

Again, let me insist on the fact that you need to be clear about these concepts, because from now on, I will use "name" to denote the `REGISTRY` field, so I will say things like "the image's name is nginx", where I should really be saying "the image's name is nginx:latest", but most of the time, it's clear enough so we don't need it.

**Summary:** technically, the image's _name_ is the combination of the `REPOSITORY` and the `TAG`, but most of the time, when it's not ambiguous, we'll simply use "name". The `TAG` is a _version_ of your image, so instead of having several images named `web-app-dev` and `web-app-prod` you can have the same `web-app` image name but wit ha different `TAG` like `web-app:dev`, `web-app:prod`.  
Note that you can use it to tag your image based on some version of your libraries, like `web-app:1.0`, `web-app:1.5`, etc.

#### What is an IMAGE ID and Why Do We Need It?

The `IMAGE ID` serves a very useful and important role: it _uniquely_ identifies an image. In fact, everything in docker---_read: images, containers, networks, volumes_---all have an `ID` that _uniquely_ identifies it. It's a way to identify or refer to objects (images in our case) so you can know _for a fact_ that you are dealing with the right thing.

It must seem redundant at this point because from what we have seen so far, the name should suffice, but if you have been paying attention, you already have the answer to that.

Remember that I said you could name your image however your want on your computer, you could even name it `ubuntu` if you wanted? Well then, when you run `docker images`, and you see an image named `ubuntu`, how can you know if it's yours, or the official one? This is where the `IMAGE ID` comes into play.

I said that the `IMAGE ID` uniquely identified an image, but I never said it was random, right? It's because it's not: this `IMAGE ID` is a hash that is computed from the Dockerfile content, and it's deterministic: with the same inputs, you will create the same image, and this you will have the same `IMAGE ID`. So if you are in doubt, you can check the official `ubuntu`'s `IMAGE ID` and compare it with your own. Actually, this is exactly what docker does when you try to download an image: it looks at your `IMAGE ID`s and see if you already have it or not, it doesn't care about the name.

Another use case: suppose you built your image and named it `web-app` (since you did not specify a `TAG`, it's `latest` by default---get used to it!). Then you find out that you need to change something in your image (perhaps install another library, or change an environment variable, etc) such that it doesn't make sense to keep using the image without the modifications.  
So you make the change, and you want to rebuild the image, with the same name `web-app` (and same `TAG`, `latest`). What should happen? Since you already have one image named `web-app:latest`, should it conflict? Should it tell you that you already have an image named like this? It very well could have been like that, but that would mean you'd have to delete the image before trying to rebuilt it. It's possible, but trust me: when you are a bit more advanced in docker and you start building your own images, this scenario will happen _a lot_! Build your image, check it, notice something is missing, change it and rebuild again. It would be a huge drawback to have to delete the image manually everytime.

Besides, it's dangerous: suppose that you cannot, for the hell of god, manage to find out the missing library and you always fail to build the new image; or it's taking you longer that expected. What if you'd like to temporarily fallback to your previous, suboptimal but still working image? Well you can't because you'd have deleted it!

All of this to say that you **don't** have to delete your old image in order to rebuild one with the same name. What happens when you rebuild a `web-app:latest` image and you already have one? Simple: your previous image will lose its name, and your new image will be called `web-app:latest`.

What does it mean for an image to lose its name? It means that:

```Bash
$> docker images

<none>                  <none>              ed0206fc5a9c        11 days ago         353.5 MB
<none>                  <none>              4e5c2e3d6118        11 days ago         122 MB
solita/ubuntu-systemd   latest              58676da6fce1        2 weeks ago         122 MB
nginx                   latest              0d409d33b27e        3 weeks ago         182.7 MB
```

Here you can see that when I run `docker images` to list images on my computer, I have two images named `<none>`: they don't have a name anymore. It's because they had a name, but I built new images with the same name, so they lost theirs.

Of course, there's nothing special about them, you can still create a container from them, but you'd use their `IMAGE ID` to instantiate, so rather than doing `docker run web-app`, you would do `docker run 4e5c2e3d6118` for instance. This way it allows you to still use the image.

Now you know what this `IMAGE ID` is and what it is for.

**Note:** the `IMAGE ID`s you see when running `docker ps` are _shortened_ version of the `TAG`, exactly like git abbreviates the commits hashes. If you want to see the real, complete ones, you have to use the option `--no-trunc`: `docker ps --no-trunc`. It's useless most of the time, but still good to know, just in case.

### Pulling and Pushing Images

We have been talking about images for quite some time now, but we need a way to _get_ some images. Two ways: you can either build them or download them. Downloading an image is called _pulling_ it---and uploading is called _pushing_ it.

By default, when you don't do anything fancy, images are pulled and pushed from the global Docker Hub registry. This is a giant repository of publicly-available images.

You can search for images with `docker search <name>`, this will search and return a list of images that match your (partial) name on the Docker Hub.

There a _a lot_ of images on the hub, so you might want to filter them a bit. You can add the `--automated` option which will only show you automated builds; use it like this: `docker search --automated <name>` or you can use `-s`---or its long equivalent `--stars`---with a number to filter images that have received some stars from the community.

Personally I like to search with `docker search -s 1 <name>` to filter out images that have been uploaded as a test, never used an which are not meaningful. This already filters out quite a lot.

In order to push images to the docker hub, you first need to create an account on it, and then you need to log in, this is done with `docker login`. Once you're logged in, you can push your image with `docker push account/image-name[:tag]`. If you don't specify the tag, as usual, it will push `:latest`. Once your image is pushed to the repository, it's now available to the docker hub, everybody can search for it and pull it.

**Note:** it's possible to create a private account on the Docker Hub, in which you can push your images but they can only be seen from you or authorized accounts. I won't cover it because it's not really useful for the majority of people, but go read about the Docker Hub private accounts on the docker site if you want.

### Keeping a Clean System

This part is important because docker is notorious for eating disk space faster than an thirsty English can drink a pint.

Disk space is consumed mostly in two ways: images and volumes.

Remember what we have seen about the ability to built an image and name it the same as another image, in which case it loses its name and become `<none>`? Well as we have seen, these images still exist and still take some space. Even thanks to the layer system, when you use docker for quite some time, there are always some layers that end up being unused.

The `docker images` command has a pretty useful option `-f` or `--filter` that you can use to, err... filter out displayed images. Now I will admit that docker does lack some documentation about which filter you can use, but there is one particularly useful: `dangling=true`. This will display only images that are not used anymore by any---running or stopped---containers.

Note that it doesn't necessary mean that you should delete it: it might just be that _right now_ no containers use this image, but you still want it. Hopefully this is very rare and should be temporary.

What you want, most or the time, is delete these dangling images because you don't need them anymore and they pollute your system. The command to delete an image is:

```Bash
docker rmi <image>
```

"rmi" stands for "rm" (remove), "i" (image). In place of `<image>` you can put the image name (**be careful: if you don't specify a tag, it will delete :latest**) of the `IMAGE ID` (safer: you don't have any chance of mixing). Personally, I never use the image name when I delete, I always use the image's ID (that you can copy/paste with the mouse's middle button, you know...).

To keep your system clean, it is advised to periodically remove the unused---or dangling---images. Sometimes, running `docker images -f dangling=true` will give you a lot of images, and this is a pain to select each one of them and delete them one by one. There is a shortcut: the docker guys anticipated everything. There is an option `-q` or `--quiet` which outputs **only** the `IMAGE ID`s.

Try it now: `docker images -q` and you will see. This is much less readable, but incredibly useful for a machine. Now you can pass this list to `docker rmi` to delete all dangling images in one line.

Two ways (depending on your preferences):

You can generate the list of `IMAGE ID`s only (with the `--quiet` option) and use that as an argument to `docker rmi`, as such: ``docker rmi `docker images -qf dangling=true` ``.

Or you can generate the list of `IMAGE ID`s only (still with the `--quiet` option) and pipe that to `xargs`, as such: `docker images -qf dangling=true | xargs docker rmi`.

Both commands achieve the same result: remove the dangling images.

Okay, we are done with images for now, I believe you have the basic tools needed to deal with images for your docker operations.

## Interacting With Containers

So we saw how to interact with images, this is cool. But images by themselves are pretty uninteresting: we can't do much without _containers_, instances of images.  
Let's see some cool commands!

### Creating a Container

Well obviously the first thing to do with containers is _create_ them, or _instantiate_ them from an image. By the way, for a quick vocabulary checkpoint: we already said---albeit never formally defined---that an image is **_built_**.  
A container, however is said to be either **_created_**, **_instantiated_** or **_run_**. Now that it is said, be ready to read all three terms indifferently.

So how do we create a container? It's the command `docker run`. It takes two parameters: the first one is mandatory: this is the image to instantiate the container from, the second one is optional and is the command to run inside the container.

#### Wait What? I Thought You Said Container Were Just a Process?
Ah! I am _very_ glad you noticed this: it means you are following!

Indeed, when we build an image, we usually specify the command or process that it should be running. Except we don't _have to_.

How come? Well it's easy to understand, and besides we have already seen this when we created some containers based on the ubuntu image.

The idea is that Docker is a tool that allows us to define and build a controlled, determined environment: a set of tools and variables defined according to your preferences or needs. So building an image is really defining all of these settings. But once you have that, nobody forces you to actually run a process inside it. Just having an image without a process running inside it is pointless in itself, but maybe you have several processes that you want to test and they all need to run in the same environment, in which case you will create several Dockerfiles that will use this one---and these _will_ have a defined process to run.  
So what you do is create the environment in your Dockerfile, build the image and you will only run the process when you create the container: this is one valid use case of an image without a defined process.

This is why `docker run` can take an additional parameter: the command---or process---to run inside the container. Note that in the case of an image with a defined process, you _can_ override it with that additional parameter, **but** it takes some getting used to, because there are some peculiarities that we will address a bit later.  
For the time being, let's use the second parameters only on images without a defined process.

So let's say we want to run a process based on the ubuntu image, the command would start with `docker run ubuntu` and since the ubuntu image doesn't define a process by itself, we have to give it a command. Let's do that, let's start `bash` in it, so we can have a shell:

```Bash
$> docker run ubuntu bash
```

TADAAaaa...argh! What just happened here?

We don't have a shell, and try running `docker ps`: our container is not listed here, which means it is not running. We can check this with: `docker ps -a`:

```Bash
$> docker ps -a

CONTAINER ID        IMAGE                   COMMAND             CREATED             STATUS                        PORTS               NAMES
78b952473c3f        ubuntug                   "bash"              12 seconds ago      Exited (0) 11 seconds ago                         big_panini
```

So what happened?  
Let's take a little look about what information is available to us: first column is `CONTAINER ID`, as we previously saw, this is just the unique ID for this container.  
Then `IMAGE`: ubuntu, so far so good, this is what we wanted.  
`COMMAND` tells us the container was running the program `bash`, again: so far so good.  
`CREATED` 12 seconds ago, seems alright, obviously your value here might change depending on when you ran `docker ps -a`.  
`STATUS` gives us the state our container is int, currently it is `exited`---so it's not running anymore---and the number between parentheses `(0)` indicates the return code the running process returned when it exited. In this case 0, which usually means that the program returned without any error.  
`PORTS` is empty and this is perfect because that's out of this article's depth for now, but we **will** come back to it, promise. By the way, if you are a little adventurous---which I recommend you to be!---and you tried with other base image you might have something written in `PORTS`. Don't worry, we'll come back to this.  
And last column, `NAMES`---I have no idea why sudden plural here---gives you the name of the container. Note that we did not specify any, so docker generated an random one. It's always two words, separated by an underscore and it's often something humorous. So it's a nice feature.

#### This is All Very Good, But I Still Don't Have a Running Container!
Ho yes, that is perfectly right.

So what happened here? Because of the return code being `0`, everything seems fine. In fact it turns out we did something "special". As the running process we asked to run `bash`, which is a shell. And a shell doesn't do much by itself, just like that. It needs a command to run, something to run, something to process.

We are not wary of this because usually when we run shell, it waits for us, with a blinking pointer. Most of the time, it's because we started the shell through a _terminal emulator_---Gnome Terminal, Konsole, Guake, etc. And these terminal emulators run the shell in _interactive_ mode.  
I'm not here to make a course about shell and terminals, but for the sake of this article, let's assume that all "interactive mode" means is that the shell waits for commands on `stdin`---the keyboard.

`docker run` has an option just for doing that, and it's the `-i` option---like bash's. As always, with every docker commands, you can get information with `--help`, like such: `docker run --help`.

Here we see the option: ` -i, --interactive               Keep STDIN open even if not attached`. It's pretty clear: by running a container with `-i` you will attach `stdin` to it, so the keyboard.

So here we go:

```Bash
$> docker run -i ubuntu bash
```

And it fails again... Sorry.  
But this time, it fails differently, right? Different means new information and so it's interesting. You must have noticed that this time it seems to "hang": you don't get a shell, but you don't get back your original shell either. It's seems to be stuck.

But the good news is that if you open another terminal **don't kill the one which seems stuck** and run `docker ps` you should have something like:

```Bash
$> docker ps

CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
1e68fd3f988b        ubuntu               "bash"              3 minutes ago       Up 3 minutes                tiny_engelbart

```

Not that I did not need to use the `-a` option this time: the container **is** running. As it is confirmed by the `STATUS`: `Up 3 minutes ago`. Which means that it's been up for about 3 minutes.

So what happened _again_? It all works as intended: we have a running process `bash` inside a container. This shell is waiting for user input on it's standard input `stdin` and we have bound the container's `stdin` with our own terminal. It's just missing _one_ little thing: a TTY!

And it makes sense: if you have a remote server, VPS or something, try logging in ssh to it, and once you're logged in, run `who`. It's a command which tells you who is connected to the server. And you should see your user (potentially others, if others are logged in) and a TTY number.

Again, no there to make a course about how Linux works, but you can think of a TTY roughly as a "connection", a "slot" in the server.

To continue the rough analogy, since we haven't allocated a TTY to our container, you can think of what is happening with our container as this: it is running a shell which is waiting for input from the keyboard, but there are no users connected, no slots taken. So well, there's no chance it receives anything.

By the way, if you want to "unstuck" your stuck container, try `CTRL + A; P` (this is `CTRL + A`, release, then `P`). The documentation says it allows you to "detach" from your container, but it almost always fail on me. If it fails for you, then you have to either kill your terminal, or from another terminal run `docker stop` on your stuck container.

So last but not least---I promise this will work after that---we have to allocate a (pseudo-)TTY. And this is with the `-t` option. In fact this is _so common_ that you will always group `-i` and `-t` together with `-it` or `-ti`.

And **now** we can do some serious stuff: `docker run -it ubuntu bash`.

Ho yeah! It works now: we have a shell inside our container!

We can check it as usual:

```Bash
$> docker ps

CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
d493691e64bc        ubuntu               "bash"              13 seconds ago      Up 11 seconds             insane_carson
```

From here on, everything you do in this shell is done _inside the container_, so it should not affect your system. Try playing a little, `cd` to soem locations, create, modify and delete files, have fun!

You can exit the container with `CTRL + D` which exit the shell. And since the shell is the only running process, it will stop the container.

#### Just Some Bonus About Naming Containers

Unless you recently received molten lava in your eyes, you should have noticed that every time you create a new container, it is given a (funny) name. It's fun when you are debugging or beginning with docker, but pretty quickly you'd want to name containers yourself.

In order to do that, it's as simple as passing the `--name your-custom-name-here` option to `docker run`. So you can write `docker run -it --name something ubuntu bash` and we will have the same working example as before, only named 'something'.

A little bit of warning, though: you cannot reuse a name, so if you create your container with `--name something` just like we did, and then you stop your container, you cannot simply re-run the command as-is, because docker will complain and tell you the name "something" is already taken by a stopped container.  
You can then delete the container (we'll see below how to do that) and then you can reuse the name. Or you can be clever and automatically delete the container when it stops. This feature (again: huge thanks to the docker guys!) is very useful when you want to debug your image or when you want to perform one-off commands on a image.

Let me explain: suppose you have a fairly complex image that you set up correctly with your Dockerfile, then you want to check something, like whether your host and the containers based on your image have the same time. What you want to do is create a container from the image, run `date` and exit the container, then you can destroy it because you don't need it anymore. You can do all of this with the `--rm` option. This option deletes the container when it exits. So it is never "Stopped" or "Exited". It's done with `docker run --rm --name foo ubuntu bash` for instance (the `--rm` option is the important bit of course).  
After you did this, if you open a new terminal and type `docker ps`, you will see your container with the name "foo". But when you exit the container with `CTRL + D`, the container will exit---since it has no other running processes---and it will immediately be destroyed. So you can use "arrow up" and rerun the exact same command and it will work---no more conflicting name.

Be careful with the `--rm` option: it **does** come handy to keep a clean system---and avoid having dozens of stopped, useless containers---but make sure never to use it with container from which you'd like to recover data before destroying it!

So that was the basics of container creation. The two most common forms you will use are `docker run --name container-name <image>` for images who have a defined process and `docker run --name container-name -it <image> <command>` for when you want to interactively run a specific command inside the container. Most of the time, the command will be `bash`.

### Deleting a Container

Now that we have been playing a bit with docker and containers, you should have a number of stopped containers, leaving your system in a pretty messy state. Try `docker ps -a` to list them, and you'll see what I'm talking about.

It's time to learn how to remove containers, well this is easy: `docker rm <container-name>`, of course, as always, `<container-name>` can either be the container name or the container ID.

Though it is fairly easy as we've just seen, a couple of things of importance:

- you cannot remove a running container, if you try this, docker will complain with  
```Bash
Failed to remove container (<container-name>): Error response from daemon: Conflict, You cannot remove a running container. Stop the container before attempting removal or use -f
```
As it is explained by the error message, you should stop the container before, with `docker stop <container-name>`. In the case where you want to be messy, or you need to be fast, or you don't care if some baby dear die, you can use the `-f` option to _force_ the removal---in which case docker will issue a `SIGKILL` to terminate your running process quick and dirty.  
Bottom line: stop your containers before destroying them, but there _exits_ the possibility not to do so.
- When a container makes use of Named Volumes, they won't be deleted when you run `docker rm` on it. This is a security feature so that even though you managed to mess your typing and deleted a container, its data is safe. At least the data stored inside the Named Volumes, but hey, since you've been paying attention, you know that containers should be ephemeral and should be able to be stopped, destroyed and restarted with little to no consequences, which means---among other things---that all important data should be kept in Named Volume; it's all fine, **right**?

When you are **absolutely _sure_** that you **want** and **can** destroy **everything** regarding the container, **including** the **persistent data inside the Named Volume(s)**, then you can pass the `-v` option (for "volumes") to `docker rm`, and it will delete everything about the container, persistent data included.  
I have made a couple of wrappers to deal with docker commands for the people in my company, and I have not enabled the possibility of running the `docker rm` with the `-v` option for several containers at once, **and** I have actually been so far as to name the command `nuke` **and** ask the user for a confirmation password before executing.: this is how dangerous this command is.

I'm insisting _a lot_ on this point because when you are more familiar with docker and you work with it everyday. You start to really enjoy the containerization it provides, the isolation and the fact that your data is always securely stored inside a Named volume. And there comes a moment when after too much debugging, too much coffee, and too much staying-late-or-rather-early you will issue that `docker rm -v <container-name>` on an important container. And you will regret it. A lot.

### Starting And Stopping Containers

This will be a short little paragraph, because it's pretty straightforward. When you have a running container, you can stop it with `docker stop <container-name>`. What's going on behind the scenes is that docker sends a `SIGTERM` signal to the running process of your container -more importantly, but we will see it later, it sends the `SIGTERM` to the process that has PID 1.  
After a given time, if the container has not actually stopped---`SIGTERM` is a signal that can be caught by a program, so it can be ignored---docker will send a `SIGKILL` which cannot be dealt with.  
By default, this timeout is 10 seconds, which you can override with the `-t` or `--timeout` option: `docker stop -t 3 <container-name>`.

Starting a container is done with the `docker start <container-name>` command. Simple. When it's a container running a server or a process on its own, this is enough: `docker start` and it's running.  
When you need to feed some input---think interactive shell like before, you need to do the same kind of wizardry as you did before with the interactive and tty thing.  
There is the same `-i` or `--interactive` option we saw with `docker run`, annnnnd now we have a severe case of amnesia! For a reason that I don't understand, the `-t` option from `docker run`, which allowed us to allocate a (pseudo-)tty to attach to the container has transformed and is now named `-a` or `--attach`. I can't explain it, so you'd just have to remember it.

To sum it up: it's `docker run -it <image> <container-name>` to _create_ the container, and it's `docker start -ai <container-name>` to _start_ it when it's stopped. Just remember it and you'll be fine!

#### Run is Actually Two Commands
We saw that `docker run` was used to create or instantiate a container from an image. Actually I have been slightly lying to you. `docker run <image>` actually does two things: it **creates** the container from the image (at which points the container is in `Created` state) and _then_ it **starts** it.  
You can only create the container without starting it right away if you need to do that. It's done with `docker create <image>`. When you do that, your container will be created, and when you want to start it, use `docker start`.  
So really, `docker run` is a combination of `docker create` and `docker start`.

#### It's Possible to be More Radical
We saw that in order to stop a container we had to use `docker stop <container-name>`. But it takes a timeout, by default 10 seconds. Sometimes, the application we dockerized doesn't handle `SIGTERM` well. It may be that it's simply not designed to---in which case you should go back, change and handle it!---or it might be that the application crashed to a point where there's no more hope.  
In these case, you can save a little time and _directly send a_ `SIGKILL`. Note that it's dirty, because it leaves _no chance_ at all for the application to gracefully shutdown. So you should only use that in cases of emergency.

The command is `docker kill <container-name>`.

Another more graceful use of this command is if you want to send _another_ signal to your containerized process, like `SIGUSR1` or any other UNIX signal, in which case it's the `-s` or `-signal` option, used like this: `docker kill -s SIGUSR1 <container-name>`.

Not everyone needs it, but it's still good to know that Docker has some tools to do that.

### Getting a Listing of Containers

We saw how to create containers from an image, how to stop a running container and we saw how to delete a container. Let's see how to list containers.

There is the vanilla `docker ps` which lists all running containers. We can add the `-a` -or `--all` option to list them _all_, running and non-running containers.

We already saw the `-f` -or `--filter` option that allowed us to filter the output, we used it to display dangling---_i.e._ non-used---images. Well there is this option for containers too. You can filter by name, etc.

Another option that you will find useful for debugging is the `-l` or `--latest` option. It only shows you the latest created container. This useful when you are doing tests and debugging and you find yourself starting, stopping deleting the same container all over again and you want to inspect some states inbetween.  
In the same spirit, you can use the `-n <number>` option to display the `<number>` latest created container. It's like `-l`, but for several containers. I never used it myself, though.

Another nice option for inspection is the `-s` or `--size` option. When you use this option, `docker ps` will add another column to the output, `SIZE` to display the size your container takes on disk.  
Two values are included: the size on the container's read-write layer, and the size of the base image---it's the one between parentheses.  
It's a nice feature because it allows you to see if you have images growing out.

Let's take an example, first we create a container from a base image, let's use `nginx` to change.

```Bash
$> docker run -it --name size-test nginx bash
```

Now you should have a prompt like that:

```Bash
root@ac71e120b023:/#
```

you're inside the container. Let's exit it with `CTRL + D`. Now we want to display it, with its size. Since this is the last created, we will use this opportunity to use the `-l` flag that we just learned about:

```Bash
# docker ps -ls

CONTAINER ID        IMAGE               COMMAND             CREATED              STATUS                      PORTS               NAMES               SIZE
ac71e120b023        nginx               "bash"              About a minute ago   Exited (0) 20 seconds ago                       size-test           0 B (virtual 182.7 MB)
```

**Note:** just so there is no ambiguity here: the `docker ps` is the command to display the processes, and the `-ls` thing is really two short options: `-l` and `-s` that are chained together to avoid repeating the dash. It has nothing to do with the `ls` Linux command, right?

Back to business. So we have our container, which is based from the `nginx` image, who's named `size-test` as we intended.  
Now for its size: it's `0B (virtual 182.7 MB)`. What that means is that the container's read-write layer's size is 0 bytes, and its base image is 182.7 MB. It simply means that we have added _nothing_ compared to the base image.

We can quickly check that that we are saying is correct:

```Bash
# docker images

REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
[...]
nginx                   latest              0d409d33b27e        3 weeks ago         182.7 MB
```

Yes, that is correct: the base `nginx` image _is_ 182.7MB.  
Okay so we have a container, that is, for now, a _perfect_ copy of the image. So perfect in fact, that for now, thanks to this layer thing, the container is actually nothing more than the image itself: it uses its layers, and---at the moment---nothing else.

Let's add some "data" in that container. First, let's start it, since we want to have a shell inside, we'll use the `-a`and the `-i` options:

```Bash
$> docker start -ai size-test

root@ac71e120b023:/#
```

So we used this opportunity to make use of what we previously saw: we just started a container and we _attached_ (`-a`) to it, and started an _interactive_ (`-i`) session. We are presented with the shell.

A quick `ls` inside shows that we are at the root of the file system---the prompt indicated this, but now we can see for ourselves how the filesystem perfectly mimics the filesystem in an actual Linux installation.

```Bash
root@ac71e120b023:/# ls

bin   dev  home  lib64	mnt  proc  run	 srv  tmp  var
boot  etc  lib	 media	opt  root  sbin  sys  usr
```

Let's create a file: `touch testfile.txt`. It creates an empty file. Let's exit `CTRL + D` and `docker ps -ls` again:

```Bash
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                     PORTS               NAMES               SIZE
ac71e120b023        nginx               "bash"              16 minutes ago      Exited (0) 3 seconds ago                       size-test           25 B (virtual 182.7 MB)
```

I think the new size you will have here may vary depending on the backing filesystem. It doesn't matter: it simply shows that creating an empty file does add size to the container, because there must be _something_ in the read-write layer of the container. As you see, creating an empty file doesn't take much space, if we had copied a 1kB file in the container, it would have grown by approximately 1kB.

So that `-s` or `--size` option is important and you should use it regularly to check on your containers.

Let's get back to our `docker ps` options. There is the same `-q` or `--quiet` option that we saw for images, it's useful to pass that to `docker stop` or `docker start` commands for instance.  
If you want to stop all running container in one command, how would you do that? Easy: `docker ps -q | xargs docker stop` or ``docker stop `docker ps -q` ``. Simple!

There is one last interesting option, `--format` but we will see it at the end of the next section about inspecting a container.

We are approaching the end of this article! This is good news---or not?---but usually, when it's the case, people tend to "rush" the last paragraphs to get it over with. This is bad, so I suggest you take a break---remember what I said about letting your brain process the information a little?

And just for that, I'll display my usual cup-of-coffee-picture:

![](/images/coffee_break.png "Breaks are important!")

#### Inspecting a Particular Container

Hi back, still with us?!  
So we learned how to get some useful information with `docker ps`. But sometimes, we want _really_ complete information about _one_ particular container. Let's see how.

For that purpose exists the `docker inspect` command. This command outputs _a lot_ of information, and I mean it: a lot.  
Try it: `docker inspect <container-name>`. I won't paste the output here because that would grow the article's length for nothing.

Basically, `docker inspect` gives you all possible information about a container. Often, there are too much, so it's a good idea to be able to filter the information.

In order to do that, you can add the `-f` or `--format` option, with a Go template. You can go read and learn about Go templates, but as a start, know that it's usually used like this: `--format "{{<template>}}"`.  
For simple uses, `<template>` is generally the name of the section you want to display, preceded by a dot.

For instance, if you run `docker inspect <container-name>` and you scroll back to the top, there should be a section called "State". You can filter and display only this section with `--format "{{.State}}"`. Try it: `docker inspect --format "{{.State}}" <container-name>` you will get something like:

```Bash
{running true false false false false 12216 0  2016-06-27T17:07:19.517535765Z 0001-01-01T00:00:00Z}
```

Now of course, you need to know the fields to know what corresponds to what. And if you are interested only in knowing the status of a container, you can do `docker inspect -f "{{.State.Status}}" <container-name>` and will get `running`.

You might have noticed that the full output of `docker inspect <container-name>` is a huge JSON string. This is very helpful because other programs---like custom wrappers or 3rd party tools---can call `docker inspect` and directly parse the output as a valid JSON object.  
But when we filtered with `docker inspect --format "{{.State}} <container-name>"` all we got is this senseless string: `{running true false false false false 12216 0  2016-06-27T17:07:19.517535765Z 0001-01-01T00:00:00Z}`.

We lost the JSON format, and we lost the meaning of each field. To get it back, you can use the `json` Go template function and insert it just before the filter, like this: `docker inspect -f "{{json .State}}" <container-name>` and now you should have output like this:   `{"Status":"running","Running":true,"Paused":false,"Restarting":false,"OOMKilled":false,"Dead":false,"Pid":12216,"ExitCode":0,"Error":"","StartedAt":"2016-06-27T17:07:19.517535765Z","FinishedAt":"0001-01-01T00:00:00Z"}`.

It makes much more sense this way.  
Making good use of the `--format` with Go template is a science on its own, but it's not the focus of the article. In fact, I seldom use more complex settings that what I've just explained, and I'm fine. So I think it's okay to start with that and let you experiment with chaining fields. Basically, you can format any section.

### Copying Files In/From Containers

The list of features is long and I could _easily_ have doubled the length of this article. But you've got to stop somewhere, don't you?  
I decided to include this section because it's something that I have discovered once I started using Docker in a bit more advanced way, but since it's very simple and potentially life-saving, let's see it here.

Containers are meant for isolation. So there's little communication possible between the host and the containers, because that's what they were meant for.  
But sometimes, you need to transfer some files. You might want your container to use a new dump of your database, which you did not include in the Dockerfile, or conversely, you might want to extract some logs from the container. It all boils down to be able to run `cp` between the host and the container.

The docker guys invented the `docker cp` commands for that. It's easy and its syntax is the same as copying over ssh with `scp`. There is no difference between copying a file and a directory, no need for any `-r` option. So in this section, when I use `<file>` it means either a file or a directory.

To copy a file from the host to the container, it's `docker cp </path/to/file/on/host> <container-name>[:</path/to/destination/in/container>]`. So for instance, `docker cp sql_dump.sql size-test:/home` will copy the file `sql_dump.sql` from the current directory (in the host) into container "size-test", and put it in location `/home` inside that container.

To "extract" some files out of the container, the syntax is simply reversed: `docker cp size-test:/var/log/syslog ./` which copies `/var/log/syslog` from container "size-test" to the current directory on the host.

This should help you deal with almost all use cases.

### Running a Command Inside a Container

So we have learned quite a few useful docker commands, right?  
When you start playing a little with docker, there should be a use case that will start bothering you. It's about maintenance.

Picture a dockerized application---this is the term we use to say we run a particular application/software/process inside a docker container; for instance if you run your postgreSQL database inside a docker container, we say you dockerized postgreSQL---on which something went wrong or on which you'd like to perform a check.  
Your application is self contained, so it runs its own process (say postgreSQL, or nginx, or apache) and you need to go take a look---at the log file for instance.

How would you go about that? What you really want to do is _run a command inside a running container_. Actually take a second or two to think about this, and try finding how you would do that. I'm waiting.

...

The reason I gave you time and made you explicitly think about it is because I wanted you to think "ah yes, we have already done that: it's `docker run`" and then scold you very hard.

They say you learn better by making mistakes, I was trying to have you make a mistake so you can better understand the difference I am going to talk about.

- If you did not actually take time to think about this, well I can't force you, you've just missed an opportunity to learn. It's no big deal, you will still learn the right way to do it, but it won't be printed in your brain as hard as I had hoped.
- If you did take some time, but had really no idea---and did not even think about `docker run`---it probably is because you read the article too fast and did not take the proper breaks each time. Remember that even though docker makes things look easy, it is a complex and difficult beast---the article is not named _"Taming The Beast"_ for nothing!
- If you did take some time and thought that `docker run` would do the trick, it's perfect, you got it wrong like I intended, but now you will be more receptive and I think you will never mix the two commands again---I'll introduce the real command in a second.
- If you did take some time, thought about `docker run` and rejected it for the reasons I am about to explain, then congratulations! Either you're not a complete docker beginner, or you are very astute, followed and understood the article very well. Congratulations to you!

Okay so all of this to say that `docker run` is **not** the right solution. Why?

Remember how and why we used `docker run`? We used it in two flavors: `docker run -it ubuntu bash` or `docker run nginx`. The former specifies the command to run---`bash`---while the latter doesn't---it's because the command, or process to run is already included in the Dockerfile. But both flavors do the same thing: _they run a command in a **new** container_.

After we did `docker run`, we had a new container created from the base image, here `ubuntu` or `nginx`. Remember: we even used `--name` to name this new container so it doesn't get a random---yet funny---name!

What we want here is run a command inside an _already running container_, this is **very** different!

If you have a running postgreSQL container and you want to check its `/var/log/syslog`; if you do `docker run postgres-base-image cat /varlog/syslog`, what you will do is create a new container based on the `postgres-base-image`, and display the content of this brand new `/var/log/syslog`. We don't care about that, what we want is to see the content of `/var/log/syslog` for the `<already-running-postgresql-container>`.

So the command to run a command in an existing, **running** container is: `docker exec <container-name> <command>`.  
This is a new docker command, and one that you will use _a lot_.

So how to use it?  
The easy part first: if we want to see the content of `/var/log/syslog` inside our container named `website-db`, we will run `docker exec website-db cat /var/log/syslog`. Simple.  
Likewise, you can run anything, want to check the disk space? `docker exec website-db df -h`. Easy.

These are one-off, non-interactive commands. What if you want to start a shell inside the already running container? You can't simply do `docker exec <container-name> bash` for the same reason we already saw: you need your session to be interactive and you need to emulate a (pseudo-)tty. Well good news: it's the `-i` and `-t` options again.

So to "log in" in a container---by "log in" I mean start a shell inside the container, on which we can perform operations---you just have to run `docker exec -it <container-name> bash`. Simple!

You will most likely use this command _a lot_, so I advise you to make an alias. I have aliases `docker exec -it` with `dexit` myself, because it saves so much time.  
In the other articles, unless obviously not the case, `docker exec -it <container-name> bash` will be what I mean when I talk about "logging in a container".

### Monitoring Application Inside a Container

We now have a pretty big set of tools and commands to use with docker, but we are still missing an important aspect: logging.  
Let me say right now: logging inside a docker container can be done in a multiple number of ways and is a science in itself. I am not talking about advanced logging in this section, only the basics.

Docker provides the `docker logs` command. It is pretty useful but not "magic"; by that I mean that it won't "know" what logs _are_. People usually expect `docker logs` to show them _the error_---whatever that means---or the _interesting logs_. But it doesn't. Not necessarily.

Let me write it once and for all, and please keep that in mind at all time: what `docker logs` does is redirect the container's `stdout` and `stderr` to your `stdout`. In another world, everything that was written or is being written to the container's `stdout`and `stderr` is displayed on your console.

It's not the content of `/var/log`, nor the content of the syslog, nor anything. By default---_read: unless you made explicit settings changes_---this will all `docker logs` will do.  
_Usually_ when you design some applications that you know will be dockerized, you redirect the logs output to `stdout` or `stderr` so that you can fetch them with `docker logs`.  
But if you designed your application to write logs to a file only, then `docker logs` won't give them. So you need to keep this in mind: not because `docker logs` doesn't show you the error doesn't mean it did not appear.

It is possible to set up more advanced, complex and comprehensive logging systems with docker, but this is for another article.

Now some useful options. By default, `docker logs <container-name>` gives you a snapshot: it's like running `cat` on a file. If you want to _follow_ the logs, meaning that new entries will be displayed as soon as they appear---rather than the next time you call `docker logs`---you need to pass the `-f` or `--follow` option.

The "problem" with that, is if your container has been running for quite some time and/or is verbose, when you run `docker logs -f <container-name>` it will flood your console with thousands of log entries and before you actually reach the end, it might be a loooooong time.

Once again, the docker guys thought about this and implemented another option: `--tail` which you give a number and `docker logs` will only show you the last `N` entries. So I always advise you to use `--tail N` when you look at some logs, because you now in advance the number of lines it will display.  
A typical call might look like: `docker logs -f --tail 50 <container-name>`. It will show you the last 50 entries, and stay in "live" mode.

Another useful option is the `-t` or `--timestamps` (mind the plural) that will show the date before each entry, pretty indispensable if you ask me!

An alternative to specifying the last `N` number of lines might be to display logs since a certain date. Well I'll give it to you: the docker guys already thought about that too!

They provide a `--since` option, which takes a timestamp.  
Well it sucks because timestamps are not human-friendly. We would like to be able to say "show me the logs since yesterday". Well it turns out, the `date` option is very usefull here.

A quick introduction if you don't know about this feature. `date` can convert a date into the format you want, it's done with the `+` option, and then you pass it the standard date symbol. So you can do `date +%m` to get the current month, or `date +%m/%d/%Y` to get the month/day/year.  
It turns out there is the `%s` option that returns the timestamp. So if you do `date +%s` you will get the timestamp (with seconds resolution) of the current date.  
Now it turns out that `date` can take an additional parameter `-d` to apply the previous operation on a specific date. And the magic is that a lot of parsing is done so that `-d` can not only support timestamps, but also string. Like "yesterday". So if you do `date +%s -d yesterday` it will give you the timestamp (`+%s`) of "yesterday".

Now that you have that, it's possible to use that ouput as the input for `docker logs`. Look:

``docker logs --since `date +%s -d "yesterday"` <container-name>`` and quite logically it will dispay the logs since yesterday. Replace "yesterday" by "2 days ago" or "3 weeks ago", experiment a little!

This is very handy. The commands being a bit verbose, it's advised to alias them, of course.

## Conclusion
Here we are: we've come a long way, haven't we?  
The first two articles were---I think---the most theoretical. But it was necessary and I tried to include as much examples and actual commands as I could, to make it the less boring possible. I am aware that the article is long, but there was a lot to learn!

I hope I made the docker concepts clear, this was the goal of this article. From now one, the articles will be shorter---at least I think!---and _definitely_ more applied. I believe I have covered the majority of the docker concepts that can be applied to build complex setups. It's important that you keep everything we saw in mind, before now, in the following articles, we will apply all of these concepts.

As usual I will try to give as many examples as possible to give you _insights_ and material to go on. I'm not yet settled on the next article, but there's a high probability that we will cover the process of building images and writing Dockerfiles. If you have a personal suggestion, don't hesitate to [shoot me an email](mailto:ns.schoe@gmail.com) and I'll see what I can do!

**Part III is available to read [here](/articles/2016-10-12-Docker-Taming-the-Beast-Part-3.html)**!
