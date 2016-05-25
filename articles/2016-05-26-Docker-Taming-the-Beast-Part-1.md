---
title: Docker: Taming the Beast - Part I
description: Docker is the new hype these days. Claiming to replace virtual machines, it promises faster delivery times, better software separations, reproducible development -and production- environments. This first post aims at presenting Docker, the concepts of containers, the differences between Docker and virtual machines, and lay the basics for the following posts.
toc: yes
tags: docker,container,continuous-integration,continuous-delivery
---

## Introduction
[Docker](https://www.docker.com/) is one of those _things_ that, one day, started popping in forums, in IRC channels discussions and on [Hacker News](https://news.ycombinator.com/) and making _the buzz_, but I never took time to really know what that was _really_ about.  
Of course I don't live in a cave: I had heard of it, but whenever I read something that talked about Docker, ideas like "software containers", "sandboxes" and "virtual machines" were lighting, but that was about it.  
And then one day I wanted to learn Docker, and I immediately faced a problem: everybody "knew" about Docker, everybody talked about Docker, but nobody was actually _explaining_ Docker anymore. It was as if it was so obvious, that the only topics were about Docker implementations, people raging that Docker did not bring anything knew, that containers had been around for decades already, etc.  
Furthermore, the Docker landing page itself was not much more verbose:

> An open platform for distributed applications for developers and sysadmins.

This is very abstract, and for somebody who "missed the train", it was not that easy to find some useful explanations. The only thing I knew, was that Docker dealt with "containers", which, in my mind, was assimilated to a sandbox (in particular, I come from the Haskell world, so the closest analogy I had was the cabal sandboxes).  
But that sounded like magic to me: make a software in a completely enclosed "environment" (whatever that means), without affecting the computer.  
The idea seemed very attractive to me, even more because I recently switched to [nixOS](http://nixos.org/), a Linux distribution built around the [nix package manager](http://nixos.org/nix/) where this concept of isolation is paramount.

In another word, my goal in this first post is to spare you the trouble of fishing for information about Docker to understand it as well as sharing the love about Docker, of course! 5 days a week I'm hanging on the #docker IRC chan (I'm **nschoe** by the way, if you want to come and say hello :-)) and too many times I see newcomers asking questions that clearly indicates they are missing some very important base notions: if you're in that case, I hope to help you in this series of articles. Let's fix that together!

![](/images/warning.png "Notice")I just want to clarify that Docker (and more generally _software containers_) is a complex topic, that goes back all the way down to the _kernel_. I'm not a Docker developers and I'm not an expert on the field. I just like to _understand_, and I work better if I really get what's going under the hood.  
I'll do my best to transmit what I understand of all of this, but if you spot any mistakes, **please** don't hesitate to [shoot me an email](mailto:ns.schoe@gmail.com) to let me know, I'll fix it.  
Likewise, if you're a newcomer and think I missed a point or left a gray area, tell me so I can fill in the gap!

In this first post, we won't play with Docker directly, we won't issue any `docker` commands, but we will understand the basics, the _fundamentals_: what's going on under the hood. But don't worry, we _will_ start playing with Docker right from part 2!  
Trust me: Docker is not an easy thing, and you will benefit from reading some theory about it.

Now let's go!

## What is Docker?

### Docker in a Sentence
It's always better to have an idea, however vague it may be, when you want to learn something. So let me give you some _intuition_ about Docker, right now, so that we have the rest of the post to actually explain it.  
But before I give you a sentence (which would not make much more sense that the previous quote), let me give you some facts, in no particular order:

- Docker is free and open source (which is awesome), if you missed it before, the website is [here](https://www.docker.com/)
- Once you have an idea of what Docker is, its documentation is fairly well written and understandable, it's available [here](https://docs.docker.com/)
- "Docker" can mean many things, if you check the documentation's home page, you will see
    - Docker Engine
    - Docker Swarm
    - Docker Compose
    - Docker Hub
    - Docker Trusted Registry
    - Docker Cloud
    - Docker Machine
    - Docker Toolbox  
That's **a lot**! Don't worry, we'll deal with each of them in our articles.  
What you are interested in, and what "Docker" should mean to you right now is "Docker Engine". Unless stated otherwise (or plainly obvious!), from now on, when we talk about "Docker", we mean "the Docker Engine".
- Docker is a **software**, meaning it's not a library, it's not a web app. It's a command-line software.
- Docker allows you to create _containers_ (the divine word!); as unsatisfactory as it sounds, at the moment, think about containers as software _sandboxes_: an isolated (we will see _how_) environment (_i.e._ set of tools, files, environment variables, etc.) in which your software lives.  
This will be the **only** time when I will tell you this, but **for now** (and only for now), you can think of containers as _a sort of_ virtual machine. But this analogy is dangerous because people tend to stick to it, and Docker is **not** a virtual machine, at all. So keep this analogy fresh, and don't store it in your long-term memory.

Ok, so now for the sentence, even if that doesn't clarify much, we can say that

> Docker is a software that allows developers to create isolated and possibly inter-dependant environments for controled software execution.

That's it for now, let's keep reading to make sense of all of this.

### Docker Use Cases
Docker is a complex tool, and it can be useful in several scenarii. Let's quickly examine some of them (I don't pretend to list them all) and see if you can find some similarities with your situation.

#### Easy Portability
This is easily one of the most common use cases I can find for Docker.  
Docker allows you to setup (we'll even say _build_) your environment as you wish, and this makes it very portable. To save testing and development time, people can focus on a specific environment, and distribute their binaries with the Docker image (we'll see what this is a bit later, for the moment, think of an image as the "environment" we keep talking about).  
This way, the target customer needs only to install Docker and the image will run smoothly (whether the customer runs the same Linux distribution than you, Windows or OS X).  
Keep in mind that Docker runs on the Linux kernel, so it's a simple matter of installing Docker if your target runs Linux, but it's currently a bit more complex under Windows and OS X (namely, the customer installs Docker inside a virtual machine running Linux).

Anyway, that's a common use case for Docker.

#### Testing Your Software Under Several Environments
Another popular Docker use case is _testing_.  
This is very popular because Docker images can be very easily instantiated and disposed of. When developing a software, it's a very delicate task to ensure that it will work on your customer's system configuration. Does he use the same library versions? Does he have the same tools than you? Will your Web App still work with PHP4 and PHP5? Does your Haskell project compile with older GHC version? Etc.

Docker is very useful here because you can create an environment with PHP 4 or GHC 6 installed and test your installation inside it _without messing with your system_.

![](/images/warning.png "I know Docker is not the only one") **I know**.  
I anticipate some advanced users that may be thinking right now that I missed an important point about Docker. The use case I just described is somehow controversial: some people think that Docker is not the best tool for this.  
There are other solutions for this, such as [the nix package manager](https://nixos.org/nix/) on which the nixOS Linux distribution is based off.  
I know. It just happens that there are a lot of people using Docker for this, and despite being a huge fan of nix and nixOS, I still think it's worth mentioning.

#### Continuous Integration and Continuous Delivery
I know that C.I. and C.D. are not the same thing, but they can be grouped together for this section's sake. If you plan on setting up a C.I. development environment, Docker can be very easily grouped with Jenkins (see [here](https://jenkins.io/solutions/docker/) for a quick access).  
You can setup git hooks that will start your Jenkin pipeline inside a Docker container and push your code if the tests pass.

#### Save Money on Infrastructure Without Compromising Security
If you're in the Saas (Software As A Service) business, you have several clients, running the same service you offer. And if you're not too bad, you care (_a lot_) about security and in particular, about making sure your clients instances are well separated, and as independent as possible. For instance (this is just an example), you might spawn several servers of your custom app, and if one crashes or gets compromised, you'd like to reduce the risk of your other clients being infected.  

#### Full Separation of Services
Typically, your service is based on several softwares: you may have an nginx running as an HTTP server, to serve your Web App files, you may have a WebSockets server, you may have a postgreSQL database storing your clients' information. All of these are separate entities that may crash or need to be updated separately.  
Docker allows your to containerize each of these services, making them isolated and independent, while controlling precisely the way they communicate (understand: chose and control which ports and/or UNIX sockets they talk with one another).

#### Scaling Up
Docker, through the use of Docker Swarm (this is for another post) can quite easily help you setup a High Availability server/database to prevent a node that crashed to ruin your entire setup.  
Or you can quickly and easily setup a load balancer between several nginx instances to relieve your over-crowded server and/or smoothly take down a server while replacing it with another, possibly updated one.  
Or you can setup a computing cluster and use Docker (Swarm) to easily spread your computing power.

As you see, Docker has many, many possible use cases, and surely I am still missing some very interesting applications. Anyway, before we dive deeper into Docker, it's important to keep our head cool and not mistake Docker for what it's not.

### What Docker is Not
Now that we have seen some cool Docker features, let's give some warning words and see what Docker is **not**.

- _Docker is **not** a virtual machine._  
This one may be the most difficult concept to grasp for newcomers, Docker-is-not-a-virtual-machine. It doesn't do _virtualization_, it doesn't do _emulation_.  
The problem is that every article that talks about Docker also talk about virtual machines, and make analogies. In this article, I will try to avoid talking about virtual machines as much as I can, and make you forget about that parallel.  
A bit later in the article, we'll see some differences between the two.
- _Docker is **not** magic._  
Too often I see newcomers come on the #docker IRC chan asking about quick instructions to achieve X, Y and Z. Docker makes it relatively easy to _use_ very complex notions such as overlay file systems and control groups, but that doesn't mean it's trivial.  
One particular point on which I insist is that Docker does **not** replace system comprehension. If you find something difficult to understand, do not think that Docker is the solution. More often than not, Docker will actually add a layer of complexity and it might not be clear whether the problem comes from the application or from the fact that it's dockerized.  
Two days don't pass without someone coming on the #docker IRC chan and ask why its postgreSQL data were lost when he recreated the container, or why the Docker image he is building takes half his disk space.  
In a word, you need to understand the underlying concept before using Docker. I have spent hours in the Docker documentation, and I have have doubts every time I use Docker for something new: am I really good enough in XYZ to replace the common method by Docker containers?  

If it was not concrete enough: don't try to containerize a postgreSQL database if you're not familiar with postgresql (can you do common administration tasks with `psql`? Do you know how to create, list, alter and delete tables? do you know how to save and backup your postgreSQL table?).  
Do not try to setup a containerized front-end nginx, if you don't know the basics of nginx configuration. I've paid this particular price myself and I was lucky enough to find someone on #nginx to help me with that.  
Don't try to isolate your compilation chain if you can't write basic Makefiles.

Well, you get the idea :-)

## How does it Work?
While this part is _technically_ not needed to start using Docker, I will talk about some _fundamental_ concepts about Docker and how it works, so it's really worth it.  
I will talk about what's under the hood of Docker and how it makes the magic happen.

### How does it Not Work?
In order to get rid of false ideas and our intuition (which, in this case, is most likely playing us), we'll talk about how Docker does _not_ work.

As I said before, Docker is **not** a virtual machine; let's see how one roughly works, then.  
A _virtual machine_, as its name implies is like a real machine, only it's "virtual", as in "inside another machine". It behaves very much like a real, fully-fleged machine (computer).

When using a virtual machine, you generally create a virtual hard disk, which is a big file on the host's filesystem, you allocate some of the host's RAM and video memory to the virtual machine, by making it accessible through special, low-level routines.  
On the virtual hard drive, you install a _complete operating system_ (Windows, Linux, etc.) from scratch. It means that from the point of view of the installer inside the virtual machine, it's like it's really writing to a disk (only it's a file on the host, but it -the installer- doesn't know it). But that doesn't change much: you still partition it, you still create filesystems on the partitions (FAT, ext3, ext4, etc.).  
Then you typically write the MBR in the first few sectors of this file (or now you write your UEFI loader in the correct partition), and when you "start" your virtual machine, it read the host's file as its hard drive, reading the partitions, the OS bootsector, etc.  

Since your virtual machine is "only" reading the hard drive's data and executing it's instructions, it can basically run "anything", and in particular, it doesn't matter what OS you install and run. This is the big strength of virtual machines. And with CPUs' virtualization features, you can even run different processor _architectures_.

Let's come back quickly to the virtual hard drive: as I said before, the virtual machine uses a (big) file on the host's filesystem. When the OS inside the virtual machine writes data to disk (_e.g._ creates a file), it calls low-level, kernel routines (drivers) to write to the file. In a real machine, these kernel routines will call the hard drive driver and actually writes the data to the physical disk.  
In a virtual machine, this process is sort of "hijacked": when the OS calls the low-level routines to write to disk, the virtual machine software will "catch" these calls, write the data to the host's (big) file serving as the virtual hard drive and send back the appropriate answer to the virtualized OS.

All of this is very _very_ simplified and I'm sure some specialists are hating me right now, but I think it's enough to understand the concept. And even most importantly, to understand the differences with Docker containers.

So everything we just saw is roughly how a virtual machine work and it's really not the case of Docker. This is important, because you have to "forget" this, or at least, remember that this is different.

### The Kernel and User lands

Now we are getting serious; Docker relies on several features to make the magic happen. And most of them have to do with the Kernel.  
To summarize quickly, the kernel is the "core" of Linux (and any OS, for that matter, but Docker runs on Linux, so now I'll mostly talk about Linux. If you are in OS X or Windows, don't be sad and keep reading, all of this still applies. We'll see how Docker works for these OSes).  
By "core" I mean that very low-level routines are implemented in the kernel: drivers to communicate with external peripherals, process schedulers, filesystems, etc. Everything that makes the OS work, is in the kernel.  
As you see, this is an important, crucial and heavy part of an OS.

In a running OS, there are generally two _lands_ or _spaces_: the **kernel space** (_a.k.a._ "kernel land") and the **user space** (_a.k.a._ "user land"). You can see the spaces as two levels -or _rings_- of privileges.  
There are a small number of programs running in privileged mode (_i.e._ in kernel space), these are called "system calls": `stat`, `splice`, `read`, `close`, `exit`, `mmap` as well as device drivers. These programs have a complete, unrestricted access to everything: this is why there are only a very few allowed.

All other programs -the one you use, web browsers, terminal emulators, mail clients, etc- are all running in the user land, with unprivileged and restricted access. These are the "user programs", this is why this is called the "user land".

Part of the magic behind Docker relies on this notion of user lands. To put it simply, each Docker container creates its own user land, that is separated from the host's and the other containers'. A bit as if you booted your computer a second time on the same kernel.  
We'll explore how it is achieved in the following sections.

### Namespaces
So what are those _namespaces_? We keep hearing about them, but what _are they_?  
To answer this question, we need to have a little understanding of how the Linux kernel works. Especially about **processes**.

Processes are instances of a program. When you run a program, it created a process. Now there are some programs that creates several processes, but you can think of it at if the program itself launched other small programs. Note that I am not talking about threads, threads are another beast. A single process can spawn several threads to make use of multithreading, to parallelize operations. Here, I am talking about processes, programs.

So, at any given time, there are a lot of processes running on your computer, you can have an idea: try running `ps -A | wc -l`. It will return a number, this is close to the number of processes running at that time. Right now, I have `149` processes running.

It's important to have an understanding of how these processes interact with each other.

Let's play a little bit. Launch your favorite terminal, it should present you a shell. For the vast majority of people, this will be `bash`; I'm using `zsh` myself, but the principle will be exactly the same.  
Now that you are in your shell, run `touch test` to create a file named `test`. Then run `tail -f test`. What this does is launch the program `tail` in "follow" mode, which mean that it keeps watching the content of `test` (currently empty) for new output. We don't really care about that, all that we care about is that `tail` won't terminate: it will keep running.

Now run another terminal, we will try to see what's happening. As you probably already know, `ps` is what we can use to see the running processed. We will format a bit its output so that it is more readable. Run `ps -Ao ppid,pid,user,fname`. This launches `ps` to print a snapshot of the current running processed, and format the output, to display, in the order: the "parent PID", the PID, the user who executed the process and then the process name.  
It should return a pretty long list, but toward the end, you should see something like this:

```
...
1067  7379 nschoe   zsh
7379  8308 nschoe   tail
2189  8862 nschoe   ps
```

Remember that the left-most column is the parent PID, and the second is the PID. Here we see something interesting: the `zsh` process has PID `7379` and the `tail` process has Parent PID `7379`. The numbers will be different that mine, **but** you will still have those two numbers equal.  
This is a very basic, and very important notion of processes: a process can have child processes and processes (the children) can have parent processes. This is pretty easy to understand: when we launched `tail` from our `bash/zsh` shell, it created a new, child process.  
That's one important concept. Let's see immediately a new one, go back to your first terminal, the one from which you ran `tail` and hit `CTRL + C`. It should stop `tail`.  
Now, launch this command: `tail -f test &`. The `&` sign that we appended means that the command we launched, `tail`, runs in the background: indeed, you can see that your terminal is now available, even though `tail` is still running.

Let's check that: `ps -Ao ppid,pid,user,fname`:

```
...
1067 12242 nschoe   zsh
12242 13267 nschoe   tail
2189 13346 nschoe   ps
```

Now from that terminal, hit `CTRL + D`. It is possible that it answers with somehting along the line of

```
zsh: you have running jobs
```

In which case, hit `CTRL + D` again. It should quit the terminal. Now that it is exited, let's run our `ps` command again (in a new terminal): `ps -Ao ppid,pid,user,fname`:

```
...
2189 15131 nschoe   ps
```

There will be plenty of output of course, but toward the end, where we usually saw `tail` and `bash/zsh` we don't see them. Let's `grep` the result to be sure: `ps -Ao ppid,pid,user,fname | grep tail` you should see nothing.

This a second, very important concept of processes: when you kill the parent (which we did by killing `bash/shell`), the child process is generally killed too. How does it work and how can this be possible?  
To create the child process, the parent process generally called `fork`, which creates the child process (this is a basic summary, that will be enough to understand Docker). `fork` returned the PID of the child process.  
When we kill the parent process, by sending it a SIGNAL, the parent can (and should) forward that SIGNAL to its child(ren): this is how the parent process can kill its child process.

When a child process dies (either because its parent forwarded it a SIGNAL or because it received itself a SIGNAL), what _really_ happens is that the child's _return code_ is set to a special code, called `EXIT_ZOMBIE` (this is the real, actual, official name!). At that point, the process still technically exists (it takes a slot in the maximum number of processes, etc), and a signal called `SIGCHLD` (for SIGnal CHiLD) is sent to the parent. This signal basically tells the parent process that its child just died, and that it should do something about it. The parent then must `reap` the dead process child. Then, and only then, does the child process cease to exist.

But _what if_ the parent never gets a chance to reap the child process? Well, we will emulate this behavior: open a terminal, and run this command: `nohup tail -f test&`. As before, the `&` will make `tail` run in the background. the `nohup` directive here prevents the parent (`bash/shell`) from forwarding the SIGNAL to its child.  

Let's check: `ps -Ao ppid,pid,user,fname`:

```
...
1067 16127 nschoe   zsh
16127 16950 nschoe   tail
2189 16959 nschoe   ps
```

We're getting used to it: we can see `zsh` has PID `16127` and `tail` had PID `16950` and has PID `16127` for parent, which is `zsh`. Classic.

Now hit `CTRL + D`. Your terminal will most likely complain with something like `zsh: you have jobs running`. Ignore that and hit `CTRL + D` again, it should work this time, and your terminal should exit, as before.

Now, let's see what happened to `tail`: `ps -Ao ppid,pid,user,fname`:

```
...
1 16950 nschoe   tail
2189 17003 nschoe   ps
```

First, we see that `zsh` doesn't appear anymore, which is normal, because we killed it. But interestingly, we can see that `tail` still exists! It was not killed. We know it's the same `tail` process, because it has the same PID (even though PID numbers can be re-used, in this case, this _really_ is the same!). Even more interestingly, we can see that now `tail`'s parent PID is `1`. And this time, you should have `1` too.

This is another key concept of how Linux processes work: there really is _one process to control them all_. In Linux, there always is a top-most, parent-most process, called "the init process". It used to really be called "init", but it's very likely yours is called "systemd" now. You can see it with `ps -o ppid,pid,user,fname -p 1` (be sure to remove the `A`). It should return something like:

```
0     1 root     systemd
```

The first three columns should be identical: we displayed the process with PID `1`. This process is launched by `root` and has no parent (hence `0` as parent PID). What's susceptible to change, is the name of the process. Most likely you should have `systemd`, but it's still possible that you have `init`. Anyway, the very first process in a Linux system, is always the init process. And there can be only one.

This process is the first one launched when the system boots, and the last one killed when the system shuts down. This is his first role.  
His second role is precisely what we've just seen: his role to become the parent process of children who do not have a parent anymore. This just happened to `tail`: we killed `bash/zsh` (and prevented the forwarding of the SIGNAL) and so `tail` became `orphaned` (this is also the correct, official term!). Then, `systemd/init` `adopted` it, and it became its parent.

You can always "go back" to the first process, the init process: run `ps -Ao ppid,pid,user,fname` again, pick a PID, whichever you want, for me, `5318  6194 nschoe   site` so PID `6194`. It has parent PID `5318`. Now I'll display information about the parent process: `ps -o ppid,pid,user,fname -p 5318` (replace with your parent PID).  
It show `1067  5318 nschoe   zsh`. So the parent was `zsh`. This `zsh` has parent PID `1067`, let's print its information: `ps -o ppid,pid,user,fname -p 1067` which returns: `1  1067 nschoe   .urxvtd-`.  
And here we are! The parent process is `urxvtd` (this is my terminal emulator, yours might be `gnome-terminal` or `konsole` for instance), and this time, the parent PID is `1`, init.

**Every processes** have `systemd/init` as a distant parent, may it be the direct parent, the grandparent, the great-grandparent, etc.

#### What Does it Have to do With Namespaces?
You thought I had forgotten?  
We haven't been avoiding namespaces, actually we have been laying the bricks to understanding them. Keep in mind everything we have seen about child and parent processes, and the init process as it will be useful in a minute.

Now, there is something you and I have been doing for some time now and which will be crucial to understanding Docker containers and isolation. We have launched several terminals and several programs (like `tail` for instance). Then we have run `ps` which allowed us to **observe** (I should say "spy on", really) other processes. And with `kill` we have, well, killed other processes.

And believe it or not, this is the key to understand all that: we have made processes _interact with each other_. Which is fabulous because it allowed us to do everything and which is a disaster, because it means that if we have some process that we want to have running, others could kill it, or inspect it. And this is the **opposite** of isolation!

Well, all of this is possible, because all of these processes run in the _same namespace_. To put it simply, we can consider that a namespace is an init tree. Here, we have one `init` process, which is the (more or less distant) parent of every other processes running: it defines one namespace. One key concept of Docker and process containerization in general is to create _separate namespaces_.

A typical namespace, like you have right now on your computer looks like this:

```
1 init
    |
    |-- 6728 zsh
    |   |
    |   |-- 6729 tail
    |
    |-- 7839 firefox
    |   |
    |   |-- 7840 firefox (tab 1)
    |   |-- 7841 firefox (tab 2)
    |   |-- (...)
    |
    (...)
```

The top-most process has PID `1` and is the init process (most likely called `systemd` on your machine). Then this init process has direct children, here we can see two: `zsh` with PID `6728` and `firefox` with PID `7839`.  
both `zsh` and `firefox` have children of their own, as you can see. The figure above forms a tree.

Now what happens with containerization and Docker? Well, if you want to run isolated processes, the first thing you need is for these processes **not** to be able to do what we have been doing up until now, _i.e._ spy on other processes and interact with them. You need to completely isolate them.  
The way it is done is that we create a _second init process tree_, _i.e._ a second namespace.  
Let's say we want to containerize `nginx`, a web server. Nginx is started from a shell, bash for instance. We'd like `bash` and `nginx` to be isolated from the rest of the system, so we have to "make them believe" they are in their own namespace. So based on what we've seen so far, they need their own PID `1` init process. In this case, `bash` can be the PID `1` init process, and it will be `nginx`'s parent process.

But of course, we actually have only one machine (the host) and one operating system (our Linux distribution), because **we are _not_ running a virtual machine**, so whatever program we launch (that includes `bash` and `nginx`), they will be child processes of the "real" PID `1` init process, the one running on our system, _i.e._ `systemd`. Here is how the processes tree will look like:

```
1 init
    |
    |-- 6728 zsh
    |   |
    |   |-- 6729 tail
    |
    |-- 7839 firefox
    |   |
    |   |-- 7840 firefox (tab 1)
    |   |-- 7841 firefox (tab 2)
    |   |-- (...)
    |
____|___________________________________________
|   |                    isolated process tree  |
|   |                                           |
|   |-- 8937 (1) bash                           |
|   |   |                                       |
|   |   |-- 8938 (4539) nginx                   |
|   |       |                                   |
|   |       |-- 8939 (4540) nginx (worker)      |
|   |                                           |
|___|___________________________________________|
    (...)
```

You recognize the first items of the tree: we have our machine PID `1` process, `init`. It started `zsh` and `firefox` as it previously did, and them have started child processed themselves.  
Now the new part: we have our isolated process tree _or -namespace_ which I have artistically ASCII-art-decorated. In this isolated tree, `bash` has PID `1` (the number enclosed in parentheses). This `bash` started another process, `nginx`, which has PID `4539`. Nginx is usually comprised of a core process which read its configuration and creates children as needed to handle requests, here it created a child -called a "worker"- whose PID is `4540`.

When we are more advanced in Docker, we'll come back to this and actually see it for ourselves, but now, believe me when I say that if we "logged in" this isolated environment and ran `ps`, we would _only_ see this.

But facts are, all these `bash` and `nginx` (everything that is part of the isolated process tree) actually runs on the host Linux system, right? They are "classical" processes, and they **must** have PIDs. This is the number I wrote before the parentheses. This _extremely important and useful feature_ which allows a process to have several PIDs has been introduced in version 2.6.24 of the Linux Kernel, in 2008!

So this is what we are talking about when we mention namespaces: nested sets of process trees in which processes can't "get out". From inside the isolated process tree, you cannot observe processes outside of it by running `ps` and you definitely can't kill them with `kill`. This is the first step of program isolation which Docker uses.

Why "the first step"? Why isn't it enough? Well, there still are plenty of ways that these isolated processes can interact with the host system: we haven't protected the filesystem, so they can read/write to the host's files, they can run very expensive computing operations and take all CPU and RAM, etc. As for now, we have isolated the processes from seeing and _directly_ interacting with each other, but Docker goes even further.

Let's take a little break and enjoy the fact that we _finally_ can put something concrete on the notions of "namespace" and "isolation".

### The Control Groups
Control Groups (_a.k.a._ "cgroups") are another feature of the Linux kernel that Docker uses for providing isolation. They solve the problem that we have just introduced about computer resources.  
Basically, cgroups is a tool to allocate and monitor the resources that a group of processes (for instance, our isolated namespace) can use.

If you are running several isolated process trees, you'd like to control their resource usage: for instance, the first group of processes may be running a critical stack of softwares, so you might want to allocate 75% of the CPU and 80% of the RAM; while your second process tree might be expendable, so limit it to 10% of these resources.

All of this is possible thanks to cgroups.

We won't go into much details for cgroups because they are less essential to understand Docker than namespaces. Usually when using Docker you create containers and care about how to make them communicate, etc. Only when you begin running serious stacks of containers do you care about controlling the resources.  
But it was still important to talk about cgroups, because when we'll use the `docker stats` command, this will rely on cgroups.

### It's all About Layers
Layers are the other "magic" component of Docker and solve the other problem we talked about: processes can still read and write on the filesystem, somehow breaking the isolation.

![](/images/warning.png "") Talking about layers with Docker without talking about _images_ and _containers_ would be a challenge, and a pretty useless one in my opinion.  
However, I'd like to avoid talking too much about Docker images and containers in this post, because the second article will be about them.  
So try to focus on the _meaning_ and not on the detail for this part. I hate articles that say "trust me" and obsure things, but in this case, I don't really have a choice, otherwise this article will grow in size.  
I _promise_ I'll talk _in details_ about images and containers in the next article, and I'll even clarify this notion of layers, but for the moment, focus on the notion behind it.

If we want to present a truly isolated environment to our processes, we must hide the host filesystem from it and expose a clean, predictable one.

What does this mean exactly?

It means that for our containerized process, the filesystem should look like a fresh install: there must be no `/home/nschoe`, there must not be files under `/tmp` that are used by the host's applications, etc. Conversely, it means that when the containerized process writes a file in `/etc/something.conf`, it should not appear in the host's filesystem.  
The containerized process should even be able to run `rm -rf /*` without impacting the host's filesystem **at all**.  
How is this magic possible?

Docker makes use of **union filesystems**. What's that?

A union filesystem is a filesystem that makes use of **union mounts**.

Wait wait wait! Put down that weapon, I'm just about to explain that!

You know what a filesystem is? Examples of filesystem are `ext2,3,4`, `fat`, `ntfs`, `reseirFS`, etc. Filesystems is a way to manage data on the hard drive. It handles the notion of `files`, `directories`, `symbolic/hard links`, etc. This might seem trivial, but it's really not.  
When you have a big file on your computer, like 10GB big, how do you store it? Do you find a slot on your hard drive big enough to fit your 10GB or do you break up the files in 10 1GB smaller parts? But then, how do you keep track of the locations of each of these parts? When your OS asks the size of your file, do you have it stored as meta-data or do you compute the size by iterating through the blocks each time? etc.

All of these are handled by the filesystem. If you're running a classic Linux installation, you're very _very_ likely to have either `ext3` or `ext4` as a filesystem; unless you manually specified another, in which case you probably know what you're doing.

Let's get back to our topic. A _union filesystem_ is not a filesystem in the same sense that the ones I cited previously are. It rather relies on one of those, and then implement union mounts. Union mounts are conceptually simple yet very useful: it takes two or more directories and present a _unified_ view of them at a specified mount point.  
Let's take a simple example, suppose we have two directories `dir1/` and `dir2/`, each containing files, as such:

```
dir1/           dir2/
|               |
|-- file1.txt   |-- file4.mp3
|-- file2.ods   |-- file5.txt
|-- file3.iso   |-- file2.ods
                |-- file6.jpg
```

Well, a union mount of `dir1/` and `dir2/` at mount point `/path/to/mnt/` would give:

```
/path/to/mnt/
|
|-- file1.txt
|-- file2.ods
|-- file3.iso
|-- file4.mp3
|-- file5.txt
|-- file6.jpg
```

So you would _transparently_ see the contents of both `dir1/` and `dir2/` at location `/path/to/mnt/`.  
Union mount brings two good features to the table:

- transparently making several directories content available to a single, unified mount point (directory)
- _shadowing_ files

This second notion probably answers the question at your lips right now: "what about `file2.ods`?!"  
If you look closely at the example before, you can see that both `dir1/` and `dir2/` have a file named `file2.ods`. So what happens?  
Well it depends on the union filesystem, but most of the time, there is a notion of precedence. In [OverlayFS](`https://en.wikipedia.org/wiki/OverlayFS`) for instance, when unifying two directories, you have the "upper" and "lower" directories, and the "upper" directory takes precedence.  
So when there are two files with the same name, the one in the "upper" takes precedence.

Without going into more details, this precedence thing solves one problem: "what do we do when two files are named the same?" but it raises another, more subtle problem. What happens if I want to delete `file2.ods`? Simply removing it won't work: because i will remove it from the "upper" directory, but then, there won't be a name conflict between the two directories and then `file2.ods` will still be visible, but this time, it will be the `file2.ods` from the "lower" directory. To solve this problem, union filesystems generally use a "without file" (the implementations can vary), but typically, what this means is that when you delete a file, rather than physically deleting it, it simply adds a third layer, which take precedence, and that "masks" the file to delete.

Anyway, this is a detail of implementation that we are not yet ready for.

#### What Did You Talk About Layers in the First Place?
Because it's very very important to Docker, and is arguably the core feature (well, not really, processes isolation is too). When you create a docker container, as I said before, it has to run in its own "place", with its own set of processes (we've already covered that) and _its own filesystem_.

And union filesystem (along with union mounts) are how it's done: when you create a new container, Docker makes use of union mounts to mount all essential files and directories (this is why/how you **do** get a classic, linux filesystem architecture inside your containers: `/`, `/etc`, `/var`, `/usr`, `/home` etc.) and by making extensive use of shadowing, it can effectively "delete" or "mask" everything that's related to the host. This is why you do get a `/etc` directory in your container, but you don't get your specific, host-related `/etc/file.conf`.  
In the same sense, this is how it allows you to write files in your container and not "pollute" your host environment and other containers.

Actually, union filesystem is used extensively in another place in Docker: containers and images. But this is a topic of its own, which is very often confused so I'd like to take some proper time to explain it. In another post.

## Conclusion
Depending on your personality and expectations, you might be frustrated after reading this first article because I did not use any `docker` command.  
Too often I see people coming on the #docker IRC chan and ask questions which show that they are trying to use docker like they are trying to use a new text editor. Docker is not simple. The tools on which Docker relies are not trivial. Docker does a wonderful job of packaging complex, low-level tools into a simple, beautiful API (and it has very [well-written documentation](https://docs.docker.com/engine/)). But it still remains that the overall system is a beautifully complex one.  
It is relatively easy to get a few containers running with Docker, because it makes such a beautiful job of abstracting the complexity away, but if you don't take the time to analyze, document and _understand_ what is going on under, you will quickly run into walls: you won't understand the notion of images vs. containers (topic of next post!), you won't be able to share your environment, you will have your disk space used by up unnecessary redundancy, etc.

On the contrary, you don't need to be an expert in every of the details we saw: I am not myself. This is why I did no go into too much details. Feel free to document more on the topics you want, and if there are topics you'd like me to talk about more in-depth, [email me](mailto:ns.schoe@gmail.com).

I hope I was clear enough and that I shed some lights on some concepts that were obscure, if you still have gray areas, feel free to [email me](mailto:ns.schoe@gmail.com) or poke me (**nschoe**) on IRC (`#docker`).  
In the next article, I will relieve some of the frustration and we will begin playing with "real" `docker` commands. In the meantime, you don't need to do anything specific, I'll begin the next article with the installation instructions.
