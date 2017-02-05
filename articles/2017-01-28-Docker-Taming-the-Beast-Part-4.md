---
title: "Docker: Taming the Beast - Part IV: Docker Volumes"
description: In part IV we will see how to _correctly_ deal with persistent data and how to make sure we never lose data that's important to us. We will explore the Docker (Named) Volumes features deeply, so that you're able to use them in the correct manner.
toc: yes
tags: docker,container,continuous-integration,continuous-delivery
---

## Introduction
Hi again, and welcome to Part IV of _Docker: Taming the Beast_!  
While the beast is not tamed yet, I reckon we have done a pretty good job at it already.

I'm glad to see you here, in Part IV, I hope you have read and liked the first three. In case you haven't, I suggest you read the articles in order, it will be more convenient for you as each articles builds on the base laid out by the previous one.

[Here](/articles/2016-10-12-Docker-Taming-the-Beast-Part-3.html) is Part III if you missed it, and [here](/articles/2016-05-26-Docker-Taming-the-Beast-Part-1.html) is the very first part if you're new here.  
Enjoy the read!

### What we Have so Far
We are now comfortable enough in Docker that the notions of container and image are clearly set, and we understand the fundamental difference between them. We have seen a fair deal of docker commands to interact with the docker daemon (such as list containers, list images, delete them, etc.).  
In [Part III](/articles/2016-10-12-Docker-Taming-the-Beast-Part-3.html)---which was a big one!---we have learned about Dockerfiles, which are basically Makefiles, but for Docker, and we have learned almost everything there is to know about the art of writing Dockerfiles, _i.e._ to **create** our own images.

In early articles, I have insisted that containers instantiated from an image must be ephemeral, which means that it should be ok for the container to be stopped, paused, even **destroyed** and recreated; at (almost) any time. When I say (almost) at any time, it's because we are not dealing with real-time programs with real time issues. But the idea is that a container is not (and thus must not be treated as) a virtual machine; on the contrary it must be treated as (because it _is_) a process.

We have already seen the commands to stop, start, destroy a container, they are respectively: `docker stop`, `docker start` and `docker down`.

### Our Fundamental Problem
We do have a huge problem on our hands right now: if stopping a container merely stops the process from running, thus keeping our data in the container, **destroying** it, on the other hand wipes out any data in it. So it sorts of clashes with the idea that containers should be ephemeral. We need a way out of this.

In other hand: **we need a way to make (some) data persistent**.

I have already talked & hinted about it in previous articles: this will be solved with **Docker Volumes**, which is an _extremely_ important aspect of Docker, and the topic of this article.

I have just reminded you of what we have done so far, well, at the end of this article, we will be able to have truly ephemeral containers, with a safe way of keeping important data!

Grab a drink, grab some sugar and let's do this together!

## Reminders on Theory
Don't flee!

I know that such a title is not very appealing, but stay with me, I just need to remind you a little of how things work together so that I can introduce the Docker Volumes, otherwise you'll just think it's a 'new feature' and you'll start using them without understanding them. And you know it's not how we do things in here.

Don't worry, it will be a short reminder.

One of the main advantages of docker, what is at the core of the images / containers scheme and makes it so performant is the concept of layers, the fact that they are re-usable and the unionFS.  
We have seen that, roughly speaking, unionFS is a sort of filesystem that is applied on top of 'real' filesystem (like NTFS, ext4, reiserFS, etc.) whose main goal is to provide the content of two or more separate directories, unified at a specific location. UnionFS, through union mounts, makes `/path/to/dirA` and `/path/to/dirB`'s content available at `/path/to/unified/view` without the user knowing where each of the files and directories inside initially comes from.

Then we have seen the concept of layers: when you write a Dockerfile, you start with a base image, and each statement in the Dockerfile runs a series of operations on the image, and when it succeeds it creates a new layer, which, chained with the previous image, gives another image.

Similarly, _instantiating_ a container from an image is done very quickly because it only consists in creating a _Read-Write Layer_ which is linked on top of all the layers that compose the image. This is very cool because it makes instantiating containers so fast and so easy that it is virtually _free_ (meaning you can instantiate containers on the fly, issue some operations in it and shut it down).  
Also it makes it easy to destroy a container: only the RW layer need to be removed.

Alas, this is also the source of the problem: how do we delete a container but still keep some important data from it? When we delete the container, we delete the RW layer and thus lose all modifications from the image.

A first idea for a solution would be to simply save the RW layer, but this is a bad idea, for several reasons. First, we can say that the RW layer _is_ the container, so saving it actually means 'not destroying the container'.  
Second, the RW layer, being a _layer_ is a modification that applies on top of another layer (a bit like a git diff that applies to a commit). This means that if we backed up this RW layer, we could only re-apply it to the same image we initially applied it to. This is rather lame because we would not be able to share data between two different images.  
And third, since all modifications in a container are stored in the RW layer, saving the RW layer would essentially save all data, and we could not cherry pick and save just the data we want.

So we need to find another solution, and what is done with Docker Volume is simply to _bypass_ the unionFS. Let's see this in more details.

## Handle Persistent Data Correctly
Okay, here we are, the real stuff begins now. Let's see how we can overcome the limitation of the unionFS.


### Presentation of Docker Volumes
I have talked about them enough, it's time to fully introduce them: the solution will come from Docker Volumes.

What **are** they? Well, strictly speaking, they are a directory, stored in a specific, Docker-reserved location in the host (for the curious, this location is `/var/lib/docker/volumes`).


Now, this is not very helpful, so let's talk what they are conceptually (and how you should think about them, really). You must think of Docker Volumes as a partition, or an external volume from the point of view of your containers ("volumes" as in USB-stick, external hard-drive, etc.).

The idea behind Docker Volume is quite simple: you **mount** them at a specific, chosen location in your container, and when you do that, operations in the mountpoint inside the container are actually performed in the directory in the host's `/var/lib/docker/volumes/`.

The previous sentence is, without a doubt, the most important sentence in the whole article. So make sure you read & understood it carefully.

Let's take a step back for a minute: until now, every operation (say create a file, modify it, rename it, delete it, etc.) was done in the container's RW layer, the one that disappears when you delete the container. All of these modifications happen entirely in the RW layer.

Docker Volumes, on the other hand, **bypass** this mechanism. When you mount a Docker Volume in a container, it is really like a traditional UNIX mount: there is no unionFS nor union mounts (or even layers) anymore.
Let's say we mount a volume in our container, at location `/code/workspace/projects`. What this means is now, every operation that is done on the container's `/code/workspace/projects` is actually done in the directory stored in the special place `/var/lib/docker/volumes/xxx`. Really. So strictly speaking, `/code/workspace/projects` is not part of the container anymore (in the sense that it's not in the RW layer anymore).

THe container's `/code/workspace/projects` and the host's `/var/lib/docker/volumes/xxx` **are** the _same_ directories, exactly like when you create symbolic links with `ln`.

This is **very** powerful, because now, when you **delete / destroy** the container, its RW layer will be destroyed as we have seen before, but this directory at the specific location (`/var/lib/docker/volumes/xxx`) will not!  
And then, what prevents us from recreating our container, and mount this directory again (the host's `/var/lib/docker/volumes/xxx`) at the same location (`/code/workspace/projects`) ; or even at _another_ location inside the container? Nothing! And this is **exactly** what we are going to do!

Congratulations, you have just understood how to make data persistent!

Just to give it another go and make absolutely sure you understand what we are doing, I'll say it one last time: every modification that one makes inside a container is stored in this container's RW layer, a bit like git commits which apply diffs from one state to reach another state.  
When we use & mount a Docker Volume at a location in the container, we are actually (like **really**, there's no intermediary step) linking the target location inside the container with a directory on the host. Exactly as if we made a Linux symbolic link with `ln`. The consequence of this is that the target location inside the container now bypasses the uinionFS mechanism, and changes (such as adding a file, deleting it, rename it, etc.) are actually performed by the host.

This is powerful **and convenient** because it runs at native speed: the directories are linked, so there is no intermediary steps, caching or whatever else.

#### Okay Fine, Got It. How & When do I Create a Docker Volume?
That's a legitimate question, I'll answer later, but basically, either you create a Docker Volume and then mount it when you instantiate a container, or you can create your container and specify the docker volume at the same time.

Let's dive in!

### Practice Time
#### Create Docker Volumes Separately
As I previously hinted, there are usually two ways to create a Docker Volume, and we will first look at making a Docker Volume separately. This will allow us to discover a number of useful commands that deal with Docker Volumes, and then we shall see how to mount the volume in a container.  
Quite logically, docker comes with several commands pertained to volumes, they all take the form `docker volume <sub-command>`.


##### Listing Volumes
The first and simplest of all is to _list_ all the volumes present on the host, the command is `docker volume ls`:

```Bash
$> docker volume ls                                                                               ~
DRIVER              VOLUME NAME
local               06d1cbdb687c167842a7423970e950617935dec140e8740ab26712b8fabd5001
local               1c6bf0f75ef901439e10e05461937e02902e0861086715185524d23845b06756
local               459129cc5e2a7655d0dc1337f06b54402cdc3fb916ae994bb124990288061f6a
local               792e7d8e333b133a1675b24c0ead99605e62a63ad30fdd107200b5be3c9db356
local               866a904c82ecd37216564105de0a073f409b5b10535554728c15865475c85567
local               e16576939ac83f75f1de1eec688cd4413b824ca888b7e77808ddc9826d530d70
local               ff7b2a404c14186909c0cfcf613e820fc097aac9dbf3c21555468f1f471e3476
local               my-volume
```

As you can see on my output, some volumes have a cryptographic hash for a name while others have a real, intelligible name: this is why Docker Volumes are called Named Volumes (I will use both of the terms interchangeably from now on).  
As it quickly becomes difficult to maintain, we will aim at always naming our volumes.

##### Inspecting Volumes
Another sometimes useful (but not that often) command is `docker volume inspect <volume-name>`. The `inspect` subcommand gives more detailed information on one particular Named Volume, as you can see here:

```Bash
$> docker volume inspect my-volume                                                                ~
[
    {
        "Name": "my-volume",
        "Driver": "local",
        "Mountpoint": "/var/lib/docker/volumes/my-volume/_data",
        "Labels": {},
        "Scope": "local"
    }
]
```

##### Creating Volumes
We were initially interested in _creating_ a Named Volume, so let's do this now, the command, quite logically is named `docker volume create`.  
You can type it as-if: `docker volume create` and doing so should return a cryptographic hash on the console, like so:

```Bash
$> docker volume create                                                                           ~
ffa05ef362bf6126d500490ed5db5b64b0c480cacb320062d7e9e251380c4913
```

Congratulations, you've just created your first Docker Volume! To confirm this, let's list the Docker Volumes with `docker volume ls`, and if there are too many, let's `grep` the result with the first few characters of the hash you've just been returned: `docker volume ls | grep <beginning-of-hash>`

```Bash
$> docker volume ls | grep ffa05                                                                  ~
local               ffa05ef362bf6126d500490ed5db5b64b0c480cacb320062d7e9e251380c4913
```

And sure enough, we find our new volume.  
But I said before that we will try to name our volumes so that it doesn't quickly becomes a mess, the option for this, is `--name`, so let's do something like this: `docker volume create --name MyFirstNamedVolume`.

```Bash
$> docker volume create --name MyFirstNamedVolume
MyFirstNamedVolume
```

And voilà! You have create your first Docker _Named_ Volume. Usually I say "Named Volume" when the volume is indeed named, and I call Anonymous Volume when the volume only has a cryptographic hash for a name.  
Fundamentally it doesn't change anything: they both work _exactly_ the same, it's just that it's easier to list, inspect and identify volumes that have a proper name. Besides, taking the good habits of naming your volumes will help when we later see `docker-compose` (in a later article), so... you should really do it!

##### Deleting Volumes
We have seen how to list, inspect and create Named Volumes, what remains is logically how to delete them. Sure enough, the command is `docker volume rm <volume-name>`.

Be careful with that command! The goal of the article is to explain how to make data persistent. We showed that mounting a Named Volume in a container allowed us to bypass the unionFS mechanism, so that our files inside the mounted directory are kept _outside_ of the container, safely in a Docker Volume.  
This allows us to delete the container (along with any modifications), maybe recreate it and mount the Named Volume again, to find our persistent data again.

This whole scheme works on the fact that the data we want to make persistent is stored inside a Named Volume, so if you **delete** this Docker Volume, _then_ your data will be gone **for sure** this time.

The thing to take out from this is that you must be very careful when deleting a Named Volume because you will lose all data inside. This is where it helps to have a proper name for the volume, indeed when deleting a volume named `postgresql_data`, I know what I am about to delete and I can cancel it if I don't want to. But if I'm to delete a container named `<hash>`, well it's more difficult!

I believe we can conclude this part for now, as we have seen all essential `docker volume` commands. They are, after all, quite logic and follow both the docker and linux ways.

#### Create the Volume at Run Time
In the previous part, we saw how to create a Named Volume, but not how to mount it inside the container.

Let's see it right now.

There are actually **three** different syntaxes to mount a Named Volume inside a container. All of them have a different meaning, so pay attention!

##### 1. Mount a Named Volume
The first syntax is: `docker run -v MyFirstNamedVolume:/path/inside/container <image-name>`.

As you have probably guessed, first comes the name of the Named Volume (here I chose the one we created in the previous part), then comes the separator (`:`) and last comes the path _inside the container_ where we want to mount our volume.  
This is the directory that will bypass the unionFS (so if you write a file inside this directory, it won't be stored in the container's RW layer, but it will be written in the special location on the host; we've talked about this.).

**If the Named Volume doesn't exist already**, _i.e._ if the name you put before the colon `:` is not the name of a Docker Volume (let me remind you that you can check this with `docker volume ls`), then **it will be created**, exactly as if you had previously run `docker volume create` to create it.

So this is how we create a Named Volume at run time (as the title of this section describes!).


##### 2. Mount an Anonymous Volume
The second syntax to mount a Docker Volume, is to omit the first part of the command (the name), and thus only provide the path at which we want to mount our volume.

We do it like this: `docker run -v /path/inside/container <image-name>`.

When you do this, _i.e._ only provide a path to the `-v` option, and no colon (`:`), docker will create a Docker Volume for you, but since you did not specify a name, it will just have its cryptographic hash. Then it will mount it at the specified location inside the container.

It works exactly the same, it's just that, as I said before, it won't be obvious what this volume corresponds to when you list it with `docker volume ls`.

##### 3. Share a Host's Directory with the Container
Alas, there comes the third syntax to mount _something_ in a container. This third option is very often confused by newcomers and this is one of the two reasons (don't worry, I will explain later what the second is!) why Docker Volumes are misunderstood...

I will again, make it very clear what this third syntax does, so that you can see for yourself how easy and obvious it is, and so that you never make the mistake, ever.

The third syntax is: `docker run -v /path/on/host:/path/on/container <image-name>`. As you have guessed, this syntax does **not** mount a **Docker Volume** in a container, **but** it mounts a **directory of the host** inside the container.

Let me repeat this so that it is _crystal clear_: the two previous syntaxes we saw were used to mount Docker Volumes inside a container, linking a directory on the container with a Docker Volume, _i.e._ a directory stored in a special place under `/var/lib/docker/volumes`.

This third option does not do this, instead, it links a directory inside the container with a directory on the host.

"What's the difference with the previous one?" you will surely ask me. Well, strictly speaking (_i.e._ in term of low-level implementation), not much: both of them are merely symbolic or hard links, and they both bypass the unionFS mechanism.

But there _are_ big differences that make them both interesting, let's see them:

First of all, with this third syntax, you can _chose_ the host's directory that you are mounting on the container. On the first syntax, when you specified a name and a path, you had no control of it, and had I not told you, you would not have known it was stored under `/var/lib/docker/volumes`.  
You would have been fine without this information, but I feel that it helps to understand what's going one: I did not want you to think that Docker Volumes were an ancient form of black magic. It is really simple, and I wanted to show you so that when you use Docker Volumes, you understand what you are _really_ doing.

Since you can share an arbitrary directory, it is possible that you can forget doing so. It is possible, then, that you modify the directory's content (even, delete it) causing some unexpected problems within the containers.
It is unwise, then, to use a host's directory to make data persistent. I **strongly** recommend against that.

Just to make sure it's clear, when you use this syntax, you can say that your host's `/home/nschoe/tests` is mounted on the container's `/tmp/foo`. You can literally mount any directory---provided you have access to it, obviously---to the container.  
But will you remember in six months that your host's directory `/home/nschoe/tests` is actually mounted and needed by a container? I don't think so. This is why it's dangerous.

Another difference is that, since you can chose any host's directory to share with your container, you can chose one that already has contents. Doing so would make the contents available inside the container.  
And this is a very interesting thing to do: you can make data on the host available inside the container, or the converse: easily taking data out of the container.

Suppose your container is set up to have a toolchain, perform some action, and produce some output files that are of interest to you. How do you get these back? Well, mounting a host's directory and putting those file in it are a pretty good solution.

Another use case that is used very often is when you set up your container to be your development environment: compilers, linkers, libraries etc.  
You need to get your code in the container. Rather than using a `COPY` Dockerfile statement ---which makes it static, at build time--- you can mount the host's directory in which your source code resides into the container, this way, when you edit and save your code from the host, with your preferred editor, the changes are immediate in the containers and you can start rebuilding / recompiling straight away!

And what's even better, is that you can specify options defining access mode for the mounted host directory. By appending `:ro`, `:rw` you can specify if the directory is `read only` or `read write` respectively. The whole syntax is as such: ` docker run -v /path/on/host:/path/on/container:ro <image-name>`.

I use this very often to share source code from my host to the container, as I just described. And by using the `:ro` flag, I make sure that my code cannot accidentally modify and/or delete any host's files. Note that this is not a permission problem, and even root inside the container can't make modifications: when using the `:ro ` flag, the directory is mounted as a read-only file system, exactly like a locked SD card, or, back in the days, a CD, or DVD ROM.

##### Recap
This is very important to understand the three possibilities that are offered to you, and so it deserves its own title, paragraph and all.

Here are the three syntaxes recapped and their main, most common use-cases:

- `-v volume-name:/path/in/container` : used to mount a Named Volume inside the container. Use this when you want to make data persistent, i.e. keep it across container destruction and re-creation. Most common examples are: database's data files, log files from server, config directory, credentials (private, public key pair)
- `-v /path/in/container` : used to mount an Anonymous Volume inside the container. Since you did not specify a name, it will create the volume before mounting it. The volume will not have a name, only a hash, which is hard to remember and identify. Use this for temporary & short-lived containers or if you have good reasons not to want a Named Volume (for instance in a script, if you don't use `docker-compose`). I advise against this form.
- `-v /path/in/host:/path/in/container:{ro,rw}` : used to mount a host's directory in the container. Use this when you need to _share data_ between the two. For instance to pass information to the container or take information out of it. Used with the `read-only` flag (`:ro`), it is useful to share `/etc/localtime` to synchronize the times between host and containers, or `~/.ssh/` to share your ssh keys in your container. Or it can be used to share source code in a building or testing environment.

Always make sure you know your use-case before choosing the method.

#### Difference between Mounting a Host's Directory and a Volume
This deserves a special mention because when I introduce people to Named Volumes, I _always_ get this question: "what's the difference?".

From a technical standpoint, nothing: whether you mount a host's directory or a named volume, what happens is that a Linux directory on your host is linked from inside the container. And it's good news, performance-wise.
So why do I always frown when I see people mounting host's directories when they want to make data persistent (like a database)? It's because it's dangerous.

There are two fundamental aspects that make mounting a host's directory for persistent data a bad solution:

1. When you mount a host's directory, you generally have access to it. So it's very easy to forget about it and accidentally change and/or delete it later. This would affect your container, and you would potentially lose 'persistent' data.

2. Mounted host's directories are not Docker Volumes, so they are not tracked by `docker volume` commands. If you followed my advice from earlier and your name your volumes sensibly, by running `docker volume ls` you would see names such as `db-data`, `ssh-keys`, `backup-files`, etc. So it's relatively easy to know what a volume contains.
On the other side, if you mount a host's directory, you don't have access to this information, and you have to manually search your directories on your host's filesystem to know this. Since directories can be _anywhere_, you will surely be tempted to create a sort of 'root' directory, like `~/docker-mounts/` in which you would place all the mounted host's directories...  
But this is exactly what docker already does for you!

Anyway, it would be pointless to insist more, I strongly encourage you to re-read the three use-cases I described under the 'Recap' section, and use Docker Named Volumes as much as you can.

### Verifying That it Works

It's time for practice, don't you think?!

First of all, let's create a container and a named volume with it. Let's mount this volume to `/home/data` for instance: `docker run -it --name test-1 -v test-volume:/home/data ubuntu bash`.

This is becoming quite a verbose command, but you should be able to understand every part of it. Now you must have a terminal in your newly-created Ubuntu container.

Let's fire a new terminal (don't exit the one in ubuntu) and check that our docker volume was created: `docker volume ls`, and you should be an entry `test-volume`:

```Bash
$> docker volume ls
DRIVER              VOLUME NAME
local               06d1cbdb687c167842a7423970e950617935dec140e8740ab26712b8fabd5001
local               1c6bf0f75ef901439e10e05461937e02902e0861086715185524d23845b06756
local               459129cc5e2a7655d0dc1337f06b54402cdc3fb916ae994bb124990288061f6a
local               792e7d8e333b133a1675b24c0ead99605e62a63ad30fdd107200b5be3c9db356
local               866a904c82ecd37216564105de0a073f409b5b10535554728c15865475c85567
local               MyFirstNamedVolume
local               e16576939ac83f75f1de1eec688cd4413b824ca888b7e77808ddc9826d530d70
local               ff7b2a404c14186909c0cfcf613e820fc097aac9dbf3c21555468f1f471e3476
local               ffa05ef362bf6126d500490ed5db5b64b0c480cacb320062d7e9e251380c4913
local               my-volume
local               test-volume
```

Perfect.

Now let's go back to the terminal in our container, and let's check that we have a `/home/data/` directory, the mount point of the docker volume: `ls -l /home`:

```Bash
root@e39ac1afe0ae:/# ls -l /home/
total 4
drwxr-xr-x 2 root root 4096 Dec 27 07:25 data
```

Here it is, looks good for now.

If you followed correctly from the beginning, you know (or else, I remind you here) that when you mount a docker volume at a mount point inside a container, this mount point now escapes (or bypasses) the unionFS mechanism, right? So data you put in the mount point is not stored in the container's RW layer, but in the docker volume instead.

It's time to verify this by ourselves.

#### Verifying the Read-Write Layer Conspiracy

Let me introduce a new option to the `docker ps` command: `-s, --size`. By appending this option, docker will inform you of the size your container takes. Let's try it:

```Bash
$> docker ps --size
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES               SIZE
e39ac1afe0ae        ubuntu:latest        "bash"              4 minutes ago       Up 4 minutes                            test-1              0 B (virtual 124.8 MB)
```

As you can see, the output is a little strange: `0 B (virtual 124.8 MB)`. There are two parts to this.

Take a minute here and try to guess what those mean, you _should_ be able to guess without me explaining.

...

Found it? Let's check!

The first part (the non-virtual size) is the size of your container's RW layer. It is the size of all data and modifications you made to your container. You _do_ remember how a container is just comprised of the read-only image's layers with an additional read-write layer? Well the first size shown here is precisely this RW layer's size.  
And the 'virtual' part of it, is simply the size of the image. This is called 'virtual' because suppose you have 2 containers instantiated from the same image; suppose the image is 500MB large; suppose the first container's RW layer is 100MB and the second container's is 250MB.

What is then the total consumed disk space? It's 100MB (first's container's RW layer) + 250MB (second container's RW layer) + 500 MB (size of the image, included once, thanks to the layer's reusability). So the total disk space consumed is `100 + 250 + 500 = 850MB` and **not** `100 + 500 + 250 + 500 = 1,350MB`.

This is why `docker ps '-size` will give you this output. Generally, you want to watch out for containers having a 'big' (non-virtual) size. Why?

Let me return the question to you, and take a minute to try and understand why (this is fundamental that you understand this): _why is it (generally) bad to have a container with a 'big' non-virtual size?_

...

Seriously, don't cheat, actually take a minute and think; hint: this has to do with the topic of this article.

...

I really hope you tried, and it's good if you succeeded.

Now for the answer: the non-virtual container's size is, as I just said, the size of its RW layer. Which means that if you have a container with a 'big' RW layer, there's a lot of data that is not persistent: data which will be lost when you delete your container!

Depending on your container's purpose, this might not be a problem: maybe it's a container that is making some tests on your code and there is a lot of output data (like compilation logs, etc.), maybe this 'big' data is temporary data that you don't care about, etc. In these cases, it's fine.

But if your container possesses important data, which you should not lose should your container be deleted, make sure to put it in a Named Volume.

Anyway, let's go back to making sure the RW layer thing was real and not a conspiracy. A bit earlier, you had read the size of your container (in this article it was `0` bytes).

Let's now create a 15MB file in `/tmp`. Since we did not mount a Named Volume or a host's directory at `/tmp`, this file we're about to create should go into the container's RW layer.

You can create a garbage file like this: `dd if=/dev/zero of=/tmp/garbage-file bs=1M count=15 iflag=fullblock`.  
Don't try to `cat` the file because since we took random bytes, it will surely mess up your terminal. If you did `cat` it and messed up your terminal, then:

- press `<Enter>` to clear any remaining command
- then type `reset` (even if you can't see what you're typing, just make sure to type `r-e-s-e-t`
- then press `<Enter>` again, and your terminal should come back to life (this is a useful UNIX tip: wherever your terminal is messed up, you can restore its state by typing `reset` and pressing `<Enter>`, make sure to remember it!)

It's time to look at the size of our container again to see if what I have been telling you all along was a lie or not!

```Bash
$> docker ps --size
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES               SIZE
e39ac1afe0ae        ubuntu:16.04        "bash"              14 minutes ago      Up 14 minutes                           test-1              15.73 MB (virtual 140.5 MB)
```

And voilà!  
We have added a `15MB` file and our container grew by `15MB`. Quite logic. You'll notice that if you remove the file (`rm /tmp/garbage-file`), the image will shrink. It seems logic, right? Not so fast, let's consider this Dockerfile:

```Dockerfile
FROM ubuntu:16.04

RUN dd if=/dev/zero of=/tmp/garbage-file bs=1M count=15 iflag=fullblock
```

Let's build it, instantiate a container from it and "log in":

```Bash
root@54099e08c610:/# ls -l /tmp/
total 15M
-rw-r--r-- 1 root root 15M Dec 27 17:32 garbage-file
```

So we have our 15M file, we can verify the image's size:

```Bash
$> docker ps --size
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES               SIZE
54099e08c610        test-img            "bash"              3 minutes ago       Up 3 minutes                            mad_kare            0 B (virtual 140.5 MB)
```

So the image's size is the same as before, when we had manually created the file after the container was created. So far so good.

The fun comes when we delete it, do it: `rm /tmp/garbage-file` check the size again:

```Bash
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES               SIZE
54099e08c610        test-img            "bash"              7 minutes ago       Up 7 minutes                            mad_kare            0 B (virtual 140.5 MB)
```

And yeah, this is the fun part: it's _still_ the same size, can you explain why?

...

You know what? I think I won't answer this right now and I'll let you think about it until next article. Maybe I can create a sort of trend like this when I give you some work for the next article.

So back to out topic.

#### Verifying the Docker Volume Persistency

We wrote our file in `/tmp/` and so it went into the container's RW layer. Fine. We have been doing this since the beginning, now we want to see Docker Volumes in action!

Let's do the same thing: create a garbage file, but put it in the Docker Volume this time.  
Let's instantiate a container with a Named Volume mounted on it: `docker run -it --name test-volume -v TestVolume:/data ubuntu:16.04 bash`

Take a look at the size:

```Bash
$> docker ps --size
CONTAINER ID        IMAGE               COMMAND             CREATED              STATUS              PORTS               NAMES               SIZE
dca1b241ab47        ubuntu:16.04        "bash"              About a minute ago   Up 59 seconds                           test-volume         0 B (virtual 124.8 MB)
```

And now, create our `15MB` file at the mount point (_i.e._ in the volume):

```Bash
root@dca1b241ab47:/# dd if=/dev/zero of=/data/persistent-garbage bs=1M count=15 iflag=fullblock
15+0 records in
15+0 records out
15728640 bytes (16 MB, 15 MiB) copied, 0.0149029 s, 1.1 GB/s

```

**Test #1**: confirm that the container's RW size did not change:

```Bash
$> docker ps --size
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES               SIZE
dca1b241ab47        ubuntu:16.04        "bash"              4 minutes ago       Up 4 minutes                            test-volume         0 B (virtual 124.8 MB)
```

Ah ah! So we created a `15MB` file and the container did not grow up an inch! So that's perfect: I haven't lied either (good news, right?!).

**Test #2**: let's destroy that container, wiping out its non-persistent data and see if we can get our `persistent-garbage` file back!  
Exit the container in your container with `CTRL + D`, stop and destroy it: `docker stop test-volume && docker rm test-volume`.
Now it's gone.

I'm about to show your where your `persistent-garbage` file is hidden because I want you to understand how it works under the hood, I don't want any part of docker to appear as 'dark magic' for you. **But**, what I'm about to show you is for educational purpose **only**. Do **not**, _ever_ modify the files directly from outside the containers. This is _asking_ for trouble.

On your host's system, docker resides under a very nice, warm and sunny place called `/var/lib/docker`. This is where you will find a lot of absolutely incomprehensible garbage, but you will find some useful information as well :)

In particular, there is a nice directory called at `/var/lib/docker/volumes/` containing all your Named Volumes. Remember earlier when I was explain the difference between sharing a host's directory and mounting a Named Volume? One of the reason was that when using Named Volumes, docker stores them at an internal place, well this is it.

What is cool with `/var/lib/docker/volumes/` is that:

1. it's 'well-hidden', so there's much less chance that you accidentally change / delete a file in there
2. it's owned by `root` and so, _even if you were to wander around here_ you would not be able to do anything (unless of course you were root, in which case, I mean, natural selection, Darwin, etc. You understand, surely that for the sake of the species, you must not be permitted to continue your operations :)).

So, in there you should find a directory called `test-volume` (the name of the docker volume) and inside it, a directory named `_data/`. It's in this `_data/` that resides the data inside the container. Go ahead, take a look: `ls /var/lib/docker/volumes/test-volume/_data/`. Again, don't change anything, just know and remember it's stored here, and that's not dark magic.

It's time to see the real thing in action, we will instantiate another container, and we will mount the Docker Volume so that you can see your persistent data being persistent.

For a change (and to really insist on the fact that the Docker Volumes are separated from the container's RW layer, we will instantiate a container from a different image, like `nginx` for instance).

`docker run -it --name test-volume-2 -v TestVolume:/home/test/other/path nginx bash`. Here, we've changed the image from which we instantiate the container and the path at which we mount the volume. That's to be extra-sure.  
You should have a shell inside your container now, you can check your data is here:

```Bash
root@8450afc561d4:/# ls -lh /home/test/other/path/
total 15M
-rw-r--r-- 1 root root 15M Dec 29 19:14 garbage-file
```

And tadaa!

You have just successfully:

- created  a Docker Volume
- mounted it inside a container
- put some data on it (bypassing the unionFS mechanism)
- destroyed a container that was using it
- mounted it inside _another_ container, and witness your data still being here!

How awesome is that?!

#### Take a Step Back

I really need you to take a step back here, relax and let it sink. I'll ask you to take a break in a couple of minutes now, and I really hope you'll do it.

What we've just done might seem unimportant and almost too easy, but we have just dived into an extremely important aspect of docker (seriously) and _a lot_ of people using docker get that wrong. Simply understanding the concept of Docker Volume and data persistency, and using them correctly already makes you a better Docker user that a lot of people.

To recap and be a little heavy on the matter, what we just did is 'pierce a hole' in the unionFS / layer mechanism to store some of the container's data elsewhere, in a secured location. This effectively separates the data from its container, enforcing the ephemeral aspect of the container which I keep talking about.

It's like the first time your learned MVC and you thought "great! now I can change some aspect of my application without changing everything!".

Well here, it's a bit like this too. Every time your container should 'keep' or 'archive' something, use a Docker Volume. If your data is linked to the session and don't need to persist then don't use a Docker Volume.

Typical uses of Docker Volumes include:

- database storage
- user preferences
- configuration options
- ssh keys
- certificates
- user's data (suppose you're dockerized a sort of web service that provides users with a storage space where they can upload files)
- logs (if they need to be kept to be audited later)
- etc.

Take some time to actually think about this (this is called software architecture!) because you should not need to fall into the opposite trend and making dozens of Docker Volumes per container to keep _everything_.

Let me remind you that as everything you put in docker volume bypass the unionFS mechanism, they won't be deleted when the container is destroyed and thus their disk space consumption won't be freed either!

This is perfect because we've just finished a major part, so it's _really_ time to take a break. Please do it, really, and come back a little later, after your brain had had time to process all that. Then scroll back a couple of screens, and re-read.

This topic is paramount.

![](/images/coffee_break.png "Coffee time!")

## Going Further

We have seen quite a few things about Docker Volumes, and by now I hope I managed to imbue you with their importance and usefulness.  
But I haven't shown you everything, there are still a couple of interesting things left to do with the Docker Volumes, I'm offering to cover some of them in the following sections.

### Sharing Volumes Between Containers

Ah! This one is important and is likely to come up if you're ever designing a complex architecure. Is it possible to mount a Docker Volume in more than one container? At the same time? What are the implications?

#### Mounting a Volume in Multiple Containers

As we have seen previously, it is no problem to mount a Docker Volume in several containers, in fact, we just did it: we instanciated a container with a Volume, put a file in the Volume, stopped and destroyed the container and _then_ we mounted it in another container (even from a different image) and we had our file just fine.  
No, the real question is: can we do it, with both containers alive at the same time?

Let's try it now!

Create a first container, mounting a Docker Volume inside it:

```Bash
$> docker run -it --name multiple-mounts-1 -v multiple-mounts:/path/one ubuntu:16.04 bash
root@e82d3a7d221a:/# ls /path/
one
root@e82d3a7d221a:/# ls /path/one/

```

Quite easy: we instanciate a container based on `ubuntu:16.04` and give it name `"multiple-mounts-1"`. We mounted a volume named `"multiple-mounts"` (and even created it because it dit not exist).

Open a new terminal (to keep this one alive and running) and create a second container, mounting the same Docker Volume:

```Bash
$> docker run -it --name multiple-mounts-2 -v multiple-mounts:/path/two ubuntu:16.04 bash
root@af8e1c99ac33:/# ls /path/
two
ls /path/two/
```

We did the same thing.

As seen from these two outputs, the Docker Volume `"multiple-mounts"` is mounted in the first container at location `/path/one` and in the second container a location `/path/two`.  
As still demonstrated by the above outputs, the Volume is seen empty from both containers (which seems quite normal).

Let's create a file at the mounpoint (hence in the Named Volume) from each of the containers, and see if the other can see it.

From container #1:

```Bash
root@e82d3a7d221a:/# touch /path/one/file1  
root@e82d3a7d221a:/# ls /path/one/
file1
```

From container #2:

```Bash
root@af8e1c99ac33:/# touch /path/two/file2
root@af8e1c99ac33:/# ls /path/two/
file1  file2
```

All right! So it behaves as intended: the Named Volume is really mounted in both containers at the same time, and it works wonderfully!

I'll be tempted to say that my part here is done: for this topic, there is no more Docker-related things to say: the questions is answered, yes it is possible to mount a Named Volume in several containers, at the same time. It really does works just fine.  
And this was to be expected, right? Why?  
Because Docker Volumes bypass the unionFs mechanism, so it's really just a symbolic link in the Linux host. So what we've just done is basically create two symbolic links that resolve to the same target. And of course that should work.

Before I leave this part, let me warn you of a small things here: Docker does not perform any magic. All it does here is create links so that your containers can have data bypassing the unionFS mechanism and store data in a persistent manner.  
If you decide to have several containers use the same Named Volume, you expose yourself to race conditions: what if container #1 is deleting a file in the Volume whereas container #2 is writing to it?  
This is not docker-related, it's a pattern in programming that you have to solve differently: you would have the **exact, same problem** if you were developping your app without Docker.  
All of this to say that you should be careful if you have several containers that use the same Volumes.

#### Have a Container Use Volumes From Another

So we have just seen that if a container `C1` used a Docker Volume `V1`, we could also mount `V1` in a another container `C2`. By extension, it would work for any number of Volumes mounted on `C1`.  
In this case it was easy because:

1. we had just created the first container `C1`
2. it had only one container

So there was no problem.

But what if the container was created "a long time ago", you might not remember which Volumes are mounted, and where. Besides, it is even more difficult if the container has _a lot_ of mounted Volumes.

What would be great, is if we could instantiate a container `C2` and "use all the same volumes as `C1`".

It turns out that the Docker devs have thought about this use case, and provided us with a way to do _just_ that, it's an option that you pass to `docker run`: `--volumes-from`. You can specify a list of containers to this option, and you container will use the same Docker Volumes mounted on the specified containers.

Let's see an example of this now.

First, we instantiate a simple container with two volumes (to spice things up):

```Bash
$> docker run --name C1 -it -v Vol1:/path/to/vol1 -v Vol2:/other/path/to/vol2 ubuntu:16.04 bash
root@cf69d0f5a139:/# root@cf69d0f5a139:/# echo 'hello' > /path/to/vol1/first
root@cf69d0f5a139:/# echo 'world' > /other/path/to/vol2/second
```

So we created a container that we named `"C1"`, created two Docker Volumes and mounted them, respectively to `/path/to/vol1` and `/other/path/to/vol2`.

No we instantiate a second container (in another terminal) with just the option `--volumes-from C1`:

```Bash
$> docker run --name C2 -it --volumes-from C1 ubuntu:16.04 bash
root@8a1952878296:/#
```

Well, first of all, the command succeeded. So it's a first good sign.  
Let's confirm that we can see the volumes and their identical mount point in `"C2"`:

```Bash
root@8a1952878296:/# cat /path/to/vol1/first
hello
root@8a1952878296:/# cat /other/path/to/vol2/second
world
```

Hell yeah! So you see, it's very easy: the `--volumes-from <container-name>` allows to mount all volumes from the specified container(s) at the same mount points.

I hope you appreciate the power of this option and its value. But be careful not to use it wrongly: since you cannot filter or chose volumes that you mount (you mount them all), you should do this only when you _need_ to mount all volumes. If you need to share a single volume, then you should find the volume name and mount it manually.  
Mounting all volumes with `--volumes-from` is especially useful when you want to backup a container: you instantiate a container than uses all volumes from another, make you backups, and then exit.

By now, I'm sure you have at least four questions regarding `--volumes-from`, which must be:

1. Can we can "chain" the `--volumes-from` calls, _i.e._ create a container `"C3"` that uses `--volumes-from C2`?
1. Can we use `--volumes-from` from a stopped container?
3. How do we specify several containers in `--volumes-from`?
4. Since we can't chose the mountpoints, what happens when I specify several containers to `--volumes-from` and two of them should be mounted on the same mountpoint?

Well let's tackle them one by one.

##### Can We Chain `--volumes-from` Calls?

It's easy enough to figure out, we have our two containers `"C1"` and `"C2"` right now, let's create a third right now:

```Bash
$> docker run --name C3 -it --volumes-from C2 ubuntu:16.04 bash
root@1805c2d98705:/# cat /path/to/vol1/first
hello
root@1805c2d98705:/# cat /other/path/to/vol2/second
world
```

And the answer is... yes! We can chain calls to `--volumes-from`. So that's done, and will work as you expect. In retrospection it's quite normal, using `--volumes-from` is only a shortcut to manually re-typing every `-v <volume-name>:<volume-mountpoint>`. So essentialy, after you instantiated your container with `--volumes-from`, the Docker Volumes become your new container's as much as they are the initial container's.

##### Can We Use `--volumes-from` From a Stopped Container?

That's easy enough to test too, let's stop `"C1"` by exiting (`CTRL + D` the terminal inside it, check with `docker ps`) that the container doesn't show up, and let's create a `"C4"` that uses Volumes from `"C1"`:

```Bash
$> docker run --name C4 -it --volumes-from C1 ubuntu:16.04 bash
root@b055d4c61178:/# cat /path/to/vol1/first
hello
root@b055d4c61178:/# cat /other/path/to/vol2/second
world
```

And that's a "yes" again! And again, that's quite normal: the mounted Volumes for a container are "meta-data", it's written somewhere in the container's meta information that it uses Volumes `V1`, `V2`, etc.  
It doesn't need to be running for us to get that information (for your information, this data is accessible with the `docker inspect <container-name>` command, in the `.Mounts` section).

##### How Do We specify Several Containers in `--volumes-from`?

Okay, that one is a bit tricky because the `--help` section doesn't really give us explanations, but we can be smart about it: we know that when we want to mount several volumes, we need to specify the `-v` or `--volume` several times instead of having a sort of "separator". We'll just be gone and try the same trick with `--volumes-from`.

First, we create two containers, each with a Named Volume ; these will be the ones whose volumes we will try to use:

```Bash
$> docker run --name Origin1 -it -v Vol-Origin1:/path/to/vol/origin1 ubuntu:16.04 bash
root@743b2e54dea2:/# touch /path/to/vol/origin1/file1
root@743b2e54dea2:/#
```

and

```Bash
$> docker run --name Origin2 -it -v Vol-Origin2:/path/to/vol/origin2 ubuntu:16.04 bash
root@526745d5e810:/# touch /path/to/vol/origin2/file2
root@526745d5e810:/#
```

Now, we create a third container and use two `--volumes-from` with the two containers whose volumes we want to use:

```Bash
$>
docker run --name UserOfBoth -it --volumes-from Origin1 --volumes-from Origin2 ubuntu:16.04 bash
root@33411c74bd7b:/# ls /path/to/vol/origin
origin1/ origin2/
root@33411c74bd7b:/# ls /path/to/vol/origin1
file1
root@33411c74bd7b:/# ls /path/to/vol/origin2
file2
```

And here we are: both files, from both volumes from both containers. Easy, right?!

##### What Happens When Two `--volumes-from` Mountpoints Overlap?

Same things: it's an easy setup to test.

First, we create a container with a volume mounted at `/path/to/overlap` and create a file `file1` inside it:

```Bash
$> docker run --name Overlap1 -it -v Overlap1:/path/to/overlap ubuntu:16.04 bash
root@5bd7498bc75e:/# touch /path/to/overlap/file1
```

Then, we create another container with another volumes, **mounted on the same mountpoint**:

```Bash
$> docker run --name Overlap2 -it -v Overlap2:/path/to/overlap ubuntu:16.04 bash
root@4bc0ac46e271:/# touch /path/to/overlap/file2
```

And now we create a container, use `--volumes-from` with both containers, and see what happens (will we break the world?):

```Bash
$> docker run --rm -it --volumes-from Overlap1 --volumes-from Overlap2 ubuntu:16.04 bash                                                                                                    ~
root@19c45ec6b129:/# ls /path/to/overlap/
file2
```

Okay! So there are only a `file2` in here, which comes from the `"Overlap2"` container... if you have a touch of intuition, you will notice that we specified `--volumes-from` with `"Origin1"` first, then `"Origin2"`. Could it be the mounts are applied in the order specified by the command-line?

Let's log out of the container ---which will destroy it, because I used `--rm` option--- and recreate one, with the order swapped:

```Bash
root@19c45ec6b129: CTRL + D
$> docker run --rm -it --volumes-from Overlap2 --volumes-from Overlap1 ubuntu:16.04 bash                                                                                                    ~
root@90a7e7d4641d:/# ls /path/to/overlap/
file1
```

Ah ah! Yes, it seems that, indeed, the order in which it is applied corresponds to the order in which the `--volumes-from` are specified.

![](/images/warning.png "Careful of the order!")**Note:** as you can see, Docker is still a young project and thus lacks some documentation sometimes. This is one of those times. I haven't seen this particular behavior documented anywhere ---if it _is_ documented somewhere and I missed it, don't hesitate to [report it to me](mailto:nschoe@protonmail.com) ---.  
So even though we just saw how this works and how to use this feature ---_i.e._ specify the `--volumes-from` options in the _increasing_ order of importance--- we cannot **rely** on it. Nowehere is it written that this behavior will be consistent across releases.  
Obviously the docker developers are serious and nice guys, so it's unlikely that they will change it on purpose, but an accident might happen.  
So my best advice is to always be careful and not re-use same mount-point when you can avoid it, but then, always try to list the `--volumes-from` in the order of importance.

So!  
We have seen a great deal about how to share volumes among containers: we've seen that we can mount a volume in several containers (be careful of concurrent access!) and we saw how a container can re-use all volumes of some other containers.

I think it's great, and that we have seen all important aspects.

Now that you have become a Wise in the topic, I can tell you about something horrible. I don't really want to talk about it, because it's bad, but then I know the best way to eradicate it is to tell you about it and now hide it from you.

It's something of the past, an ancient rite that was necessary before the Named Volumes which is now obsolete. Alas there are still a lot of articles that talk about them and then a lot of people that still use them.

You have to promise me that you will read this part only to know that you should **not** do this and will help to eradicate this practice of the past.

#### The Pleague of "Data-Only Containers"

Here we are...

Before volumes could be named, and docker volumes were only beginning to be used for persistent data, all you could do was instantiate a container and supply a `-v /path/to/mount/point` option. This would create what I have called an Anonymous Docker Volume (a Docker Volume that has no other name than it's ID).

This was a pain, because if you wanted to use Docker correctly and thus create short-lived, ephemeral containers that you could destroy and recreate at anytime, you were hard pressed to find a solution. Indeed, when destroying the container, you would not destroy the volume, but upon recreating it, you would need to remember the volume's ID. It was clearly ill-fitted.

So a solution that was found was to create what is called "Data-only Containers". The idea was to create a container from a dumb image (usually the smallest possible, like `alpine`) and have it create a volume at the same time. One would name the container with the `--name` option, smth like: `docker run -d --name database-data -v /var/lib/postgres alpine`. The container would immediately shut down because it was no `CMD` or `ENTRYPOINT` but the important thing is that it had mounted a volume (whose only identifier is an obscure-looking ID at location `/var/lib/postgres` in our example).

Then you would create your real postgres container and use the `--volumes-from` option. Since you had named your containers, it was easy to script this and always use the same `--volumes-from` (as opposed to store and remember the Docker Volume ID). You had to carefully plan the mountpoint as `--volumes-from` mounts the volumes from one container into another **at the same mountpoint**.  
So you would do `docker run -d --volumes-from database-data postgres` for instance. Then you could truly destroy and recreate your containers (making them effectively short-lived and ephemeral) and you still made your data persistent, because they were stored in a volume.

In other words, since you could not name the volumes to re-use them, you would name the containers---which was supported---and use `--volumes-from` to get the volume(s). It was a claver trick, but we don't need it anymore, and it's bad, it's dirty to do it like this now. So please don't. Ever.

I totally understand the need to do that before volumes could be named, but this is now obsolete, and **should _never_ be used again**.

- This is "dirty" because you are accessing an anonymous volume through a named container
- You can't easily change the mounpoints
- There is always a risk that you named your "Data-only Container" incorrectly, thus later forget what it is for and run `docker rm -v <container-name>` (the `-v` option instructs to remove the volume as well, this is a dangerous option!)
- There were always the risk than while listing your volumes with `docker volume ls` you decide to erase some on them, and accidently erase one your wanted to keep.

I won't talk much longer about those "Data-only Containers" because I don't want you to memorize them and end up using them, just know that if you follow some ancient guide about Docker and they instruct you to create a "Data-only container", **please don't**, you can still follow the guide, but _at least_ replace this step by creating a proper Docker Named Volume (with `docker volume create --name <volume-name>`) and mount it at the correct path.  
This will be strictly equivalent.

Anyway, back to Docker Named Volumes!

## Use Cases

We have talked a lot about Docker Volumes and provided some snippets of code to illustrate the different points we were exploring, but this might still be a bit confusing as _when_ you should use them.  
This section explores some common use cases, by all means this is not an exhaustive list and I'm only trying to give you an _intuition_ about them, so you can _guess_ when you need them or not.  

### Make Data Persistent

I know I have talked about this a lot, but Volumes are primarily used to make data persistent. By that I mean ---again, I am repeating myself--- that your container should not be this "beast" that has been running for 5 months and you're afraid that you docker daemon crashes or your server restarts because you don't know what's inside the container, what you will lose etc.

Very roughly, you should have "computations", "processing" and "logic" as applications in your containers, and "data" in volumes. For instance:

- your webserver (apache, nginx, other) should run inside a container, but:
    * the directory they are serving ---the `/var/www`--- should be inside a Docker Volume
    * the directory holding the files users uploaded on the server should be in a Docker Volume
    * I'll even argue that your config files _might_ be in a separate Docker Volume (I admit that this will depend on your setup)
- your database instance (postgres, MySQL, other) should run inside a container, but:
    * the directory holding the database data (`/var/lib/postgres` for instance) should be inside a Docker Volume
    * same remark as for the webserver for the config files

Where I work, I've designed the production stack to be like this. All our server instances are dockerized and the data made persistent in Docker Volumes. I frequently `docker stop` and `docker rm` containers from active clients and everything is happening without a problem because the _data_ (the important part) is stored in Volumes that are mounted by the new containers.
The downtime is only a few seconds for the time of the `docker run` to return!
When you reach this setup, you know your stack is sound and will be robust (I havent' said "perfect", I'm sure there is a lot to criticize about my stack).

### Sharing Data Between Containers

Sometimes when you have a complex stack of containers, connected to various Docker Networks, you need to share data between containers. Maybe you have containerized your database instance and put the database data files inside a Docker Volume.  
Then you have several scripts that run periodically, inspecting database integrity, making stats, dumping and exporting it to make backups, etc.  
Here, since the data is inside a Volume, it's very easy to have a separate container to run all those maintenance scripts (or even better, to have several containers, each running a maintenance script!) which also mounts the Volume containing the data.

And you don't even need to fear that these scripts will corrupt your data: you can mount a Docker Volume as a read-only filesystem! This way nothing you do inside the container can change anything in the files inside the volumes. Simply append the `:ro` after the mount point to mount the Volume as read-only. Like `docker run -d --name maintance-scripts -v sql-data:/data:ro custom-maintenance-image`. Perfect!

All of this is possible thanks to the awesomeness of Docker Volumes.

Obligatory example to demonstrate this, let's first create a container with a Docker Volume mounted in it:

```Bash
docker run --rm -it -v TestRO:/path/to/vol ubuntu:16.04 bash                                                                                                                             ~
root@eb375a453975:/# touch /path/to/vol/file1
```

Now we create another container and mount the same volume, with the `:ro` flag: `docker run --rm -it TestRO:/other/path:ro ubuntu:16.04 bash`:

```Bash
root@11a963bafa1b:/# ls /other/path/
file1
root@11a963bafa1b:/# touch /other/path/file2
touch: cannot touch '/other/path/file2': Read-only file system
root@11a963bafa1b:/# echo 'modifications' >> /other/path/file1
bash: /other/path/file1: Read-only file system
```

As you can see, it is not possible for the container to modify a volume mounted with the read-only option. It's true for the root user as well, it goes beyong write permissions: as far as the container is concerned, the volume is not modifiable.

By the way, you can also use `--volumes-from` and the `:ro` tag, the syntax is as follow: `--volumes-from=<container-name>:ro`, and this will do just what you expect.

As a safety precaution, always mount the Volumes your container is not supposed to modify (such as backup containers, etc.) with the `:ro` flag to prevent any mistake.

### Making Backups

Here, we're going to see how nice and useful Docker Volumes and the `--volumes-from` option are for creating backups.

Suppose you have a container, `server-db` which runs a database instance (for your webserver for instance). Let's suppose the DB is postgresql and that the data directory location is `/var/lib/postgresql/data`.  
Since you want to make this data persistent, you used a Named Volume to hosts this directory.

Now, you can make a backup with a one-liner that looks like this: `docker run --rm --volumes-from=server-db:ro -v ~/db-backups:/backup ubuntu:16.04 tar cvjf /backup/backup-$(date +%m-%d-%y).tar.gz /var/lib/postgresql/data`.

This is a big one-liner, but still a one-liner. Let's examine it:

- `docker run --rm`: with this we launch a container (instantiate from the `ubuntu:16.04` image, as seen further in the command). Since we want this container only for _creating_ the backup, we use `--rm` so that the container is destroyed when it exists. This way, it doesn't leave our system with useless, not-running containers (especially useful is you script this command and run it regularly with a cron!).
- `--volumes-from=server-db:ro`: as demonstrated in the previous section, we mount the same volumes as the container `server-db`, we add the read-only option so that our script cannot modify any of the data from the volumes (especially useful if this is a production server database!)
- `-v ~/db-backups/backup`: here we mount the host's `~/db-backups/` directory inside the container, at location `/backup`. This is because in my example, I want to store all the backups I make in my `~/db-backups/`, maybe I have another cron job that `rsync` this directory to another server, to prevent data loss in case my computer dies.
- `ubuntu:16.04 tar cvjf /backup/backup-$(date +%m-%d-%y).tar.gz /var/lib/postgresql/data`: this is a long string, but it's nothing we havent already seen. It simply tells that we want to instantiate the container from the `ubuntu:16.04` image, and that we want to run a command. Usually we run `bash` as a command,  and we specify options `-it` so that we can "log in" the container. Not today, today we don't specify `-it` so it's not interractive (perfectly suited to be scripted) and the command we specify is `tar`, which is the Linux tool to create archives.  
We specify `cvjf` as `tar`'s first argument:
    - `c` means "create archive"
    - `v` is for "verbose" (you can remove it if you don't log your script's output)
    - `j` means to compress the archive with the `bzip2` algorithm
    - `f` is the option to specify the (target, archive) file

Then we give the target, archive file: `/backup/backup-$(date +%m-%d-%m).tar.gz`. As you can see, we place the target archive file in the container's `/backup` directory which is mounted in the host's `~/db-backups/` (this is the reason why we can delete (with the `--rm` option) the container afterward, because the backup will not be inside the container, but in the host). We then include the date in the filename, which is handy when making backups.  
The last argument `/var/lib/postgresql/data`, is obviously the name of the directory we want to create an archive / backup from.

When you need to restore the data later, in another container, say `"new-server-db"`, you can run `docker run --rm --volumes-from=new-server-db -v ~/db-backups:/backup:ro -w /var/lib/postgresql/data ubuntu:16.04 tar -xvjf /backup/backup-MM-DD-YY.tar.gz`.

The idea is exactly the same, only here we extract a backup. Let's examine this command:

- `docker run --rm`: same as before, we use the `--rm` option so that the container disappears after it's done
- `--volumes-from=new-server-db`: so we mount the Volumes from the new container---which we called `"new-server-db"` here---but this time, we don't use the `:ro` flag, as we **will** be writing data inside the volume.
- `-v ~/db-backups:/backup:ro`: same as before: since we stored the backup of the other server in our hosts's `~/db-backups/` directory, we need to mount it again. But this time, since we will only read from it, it's safe to use the `:ro` flag (and since it's safe to do, we should do it, to be _sure_ our "restore" script cannot mess with our backup files)
- `-w /var/lib/postgresql/data`: since we are going to extract our backup file in this location, we need to go there first. Two possibilities:
    1. we could pass `bash -c "cd /var/lib/postgresql/data && tar ..."` as a command
    2. we can use the `-w` or `--workdir` option that `docker run` provides us. When you specify this option with a path, it becomes the current working directory for the command you specify.

So really, both options are fine, but the latter is better because that makes use of the option that was made _precisely_ for such purpose, it allows you to specify `tar` as a command and not `bash`-which-runs-`tar`. And this will keep working whatever the image (maybe you don't want to use `ubuntu` but a smaller image which does not have `bash`, only `sh`?)  
So basically, thanks to `-w /var/lib/postgresql/data`, when the command is run, it's done from the `/var/lib/postgresql/data` directory, oh and another better reason to use `-w` instead of `bash -c "cd ..."` is that if the directory doesn't exist, docker will create it (you would need to run `bash -c mkdir -p /var/lib/postgresl/data && cd /var/lib/postgresql/data && tar ...` if you wanted to be resilient to this, which makes the command even bigger...)

- `ubuntu:16.04 tar -xvjf /backup/backup-MM-DD-YY.tar.gz`: simply means that we want to e**x**tract a file, make sure to replace 'MM-DD-YY' by the actual values of the file you want to backup.

And voilà! It's simple enough: when `tar` finishes (returns), the container will exit, and thanks to `--rm`, it will disappears and leave us a clean system.

These were only some basic use cases but I'm sure you can find a lot of other ones if you start experimenting with Volumes. I hope by now that I have convinced you of two things:

1. Docker Named Volumes are awesome and very useful
2. Mounting a Docker Named Volume and mouting a host's directory are two separates cases. Make sure you re-read the examples above, because in them I use both volumes and host's-shared directory. _They are not made for the same use cases_!  
I don't want to see information made persistent by sharing it with a directory in your home (do **not** do things like `-v ~/persistent_data:/var/lib/postgresql/data`, use a Volume!).

## Additional Info

We are almost done with Docker Named Volumes!  
A few things remain for us to see and understand before we can go play with Volumes in our own images and containers.

### What About Dockerfiles?

The most astute of you might have noticed that everything we've done so far with Volumes were done at **runtime**, _i.e._ with `docker run`, when we instantiated a container from an image.  
If you have read some Dockerfiles on the web or read the documentation, you might have seen a `VOLUME` Dockerfile statement. What is it for?

Take a few seconds to think about it, to think if it makes sense for you. What would that mean at _build_ time?

...

So, what do you have?  
Let's see if you were on the right tracks.

Normally, we would expect that an _image_ doesn't care about Volumes. From the point of view of the container that is instantiated from this image, when it writes data to `/path/to/data/`, it doesn't care whether this data goes into the container's RW layer, if it goes into a directory mounted on the host or inside a Named Volume. In fact, I'd even argue that it doesn't even _know_ this.

The decision to mount a directory inside the container (whether it's to a Named Volume or to a host's directory) is decided at run time, and given the same image, it can be different from one container to another.

That being said, _sometimes_, when you design your images cleverly or for specific purposes, you can sort of _enforce_ this. This is where the `VOLUME` Dockerfile statement comes into play.

Suppose you're writing an image for a web server which allows for users to upload files. Most likely you'll want to save those files, _i.e._ make the data persistent. If you're building the image precisely for this task, you _know_ that the directory `/var/www/user-upload/` will have to be persistent and saved into Volumes.

When you want to enforce this, you can use the `VOLUME` statement. It takes an array of directories, like this: `VOLUME ["/data"]`.  
When you instantiate a container from this image, docker will automatically create an Anonymous Volume, which will be mounted on the path you specified (obviously, if you specified several paths, it will create several volumes, each mounted to a path), and this Volume (or Volumes) will be filled with the data present at the mount point.

This has the advantage that even if you don't specify a Volume to mount at run time (with `docker run -v`), your data will still be made persistent, but the disadvantage is that it will create an Anonymous Volume, so it's not easy to find it back for a given container.  
Obviously, if you specify a Volume name with the `-v` option, then the Docker Volume will be named according to what you specified.

**An important thing to note** is that the location of the `VOLUME ["/path/to/mountpoint"]` statement in your Dockerfile is **meaningful**: it will create the Volume and populate it with all the data that is inside the mountpoint at this moment.  
Every modification that is made to this path _afterward_ this statement in the Dockerfile, **is lost**.

It's time for some examples to clarify this.

First, a basic Dockerfile:

```Dockerfile
FROM       ubuntu:16.04
MAINTAINER nschoe <nschoe@protonmail.com>
RUN        mkdir /data
RUN        echo "Hello, world!" > /data/file1
VOLUME     ["/data"]
```

This is simple enough: we create a `/data` directory and write something in a file in `/data`. Next we call `VOLUME ["/data"]` in the Dockerfile.  
This will have the effect of creating (at run time, obviously) a Named Volume mounted on `/data` with some data in it: the `file1` with the string `"Hello, world!"`.

Let's check this out, first let's build the image: `docker build -t test-vol:1 .`.

For now, we have no Docker Volumes:

```Bash
$> docker volume ls
DRIVER              VOLUME NAME
```
Let's instantiate a container from the image: `docker run --rm -it test-vol:1 bash`.

In another terminal window, we can see that a new Docker Volume was created:

```Bash
$> docker volume ls
DRIVER              VOLUME NAME
local               d7a4ed94061b0754286c76cda157ba25d6d936898c164e15f2dc4039e672457f
```

So we can indeed see an Anonymous Volume created (so this is good: the `VOLUME` statement **does** have the effect of creating a Volume, _even though we did not specify the `-v` option_)

Inside the container, we can check that the volume does contain the `file1`:

```Bash

root@73456d7c3f36:/# ls /data/
file1
root@73456d7c3f36:/# cat /data/file1
Hello, world!
```

Perfect. Since we used the `--rm` option, the container's volume will be deleted when we exit the container (I'll leave you check this by exiting the container and list the Volumes again.)

So we are back with no Volumes on the system. You can make the check that if you specify a Volume name with `-v`, like `-v MyVolume:/data`, then you won't have an Anonymous Volume, but a Named Volume, and that you will need to delete it manually with `docker volume rm MyVolume`.  
When we specify the `-v` option, we have the same behavior we saw earlier in the article, so I won't make new example for this.

Now we will verify that whatever it written in the location specified in the `VOLUME` statement **after** the volume is discarded:

```Dockerfile
FROM       ubuntu:16.04
MAINTAINER nschoe <nschoe@protonmail.com>
RUN        mkdir /data
RUN        echo "Hello, world!" > /data/file1
VOLUME     ["/data"]
RUN        echo "Discarded data" > /data/file2
```

Let's build this: `docker build -t test-vol:2 .` and instantiate a container from it:

```Bash
$> docker run --rm -it test-vol:2 bash
root@19310ad44bbb:/# ls /data/
file1
```

See? No `file2`. It's because the Volume is initialized with the data that is present in the image at the moment the `VOLUME` statement is.

So the rule is that you should generally put your `VOLUME` statements toward the end of the Dockerfile, so that you don't accidentally miss some data.

Is that all right? I hope so.

Now that's good and all, but what about mounting a host's directory with a `VOLUME` Dockerfile statement?

Actually take a minute to think about it: how would yo ugo about doing this?

...

It's time to check if you've been listening---or rather _reading_---and thinking. The answer is: **you can't**. And that's logic: remember that Dockerfiles are used to _build images_. These images are then destined to be stored in a registry and then pulled by clients, which can then instantiate containers from it.

So the bottom line is---again---_portability_: suppose you specified a path `/home/nschoe/data` to mount. The other computers on which the image is pulled will not have a `/home/nschoe/data`.

This is _textbook_: a classical example of non-portability. So the answer is simple: when using `VOLUME` inside a Dockerfile, you create a Volume that is mounted on this volume. Period.

### Locating a Volume

So you have a score of containers that have been running for some time and a score of volumes. Now you need to identify which volume a container uses, how would you go about that?

There is a handy `docker inspect` command that you can use on a container, and it gives _a lot_ of information.

To filter out some information, we can use _go templates_. In particular, there is a "Mounts" section that we can use. We will use the `json` function to format output nicely.

For example, on my machine:

```Bash
$> docker inspect --format "{{json .Mounts}}" <container-name>

[{"Type":"volume","Name":"MyVolume","Source":"/var/lib/docker/volumes/MyVolume/_data","Destination":"/data","Driver":"local","Mode":"z","RW":true,"Propagation":""}]
```

Here we can see `Type: "volume"`, which means that it's a Docker Volume and not a host's directory that has been mounted (otherwise it would be `Type: "bind"`).

The second interesting part is `Name: "MyVolume"` which gives the name of the Docker Volume. So this is a way we can find out what volume a container mounts.

Then we have `Destination: "/data"` which tells us the mountpoint **inside the container** for the volume.

The option `RW: true` tells us that we did not specify the `:ro` flag, and that we can write in the container.

Now let's see the output for a container which doesn't mount a Named Volume, but a host's directory:

```Bash
$> docker inspect --format "{{json .Mounts}}" <container-name>

[{"Type":"bind","Source":"/home","Destination":"/data","Mode":"ro","RW":false,"Propagation":""}]
```

Here we see that `Type` had taken value `"bind"` instead of `"volume"`.

Instead of `Name`, we have an attribute `Source` which gives us the host's directory that is mounted inside the container.

Unfortunately, there is no easy way for a given Volume to know by which containers it is used. So you just have to iterate through all your containers.

### Keeping a Clean System

We have introduced the Docker Volumes which---in a sentence---are used to make data persistent. With it, we need to introduce a new docker command: `docker volume`.

We can create a volume by hand with `docker volume create` and list them with `docker volume ls`.

As usual, we strive to keep a clean system. Two solutions for this:

- Prior to version `1.12`, we can use the same command we used with images: we can list _dangling_ volumes with `docker volume ls -qf dangling=true` and then use this in `docker volume rm`
- Starting with version `1.12`, you can simply use `docker volume prune`, which has several advantages:
    * it prompts for confirmation, so less chance to mess it up by error
    * when it's done, it prints the list of volumes it deleted and the space freed, so that's always nice


![](/images/warning.png "")**Warning**: Volumes, which are generally used to keep data persistent tend to take up space (since they store data, so we tend to find them holding database data, user-uploaded files, etc.).  
So it's important to run this `prune` command (or the equivalent prior to version `1.12`) **regularly** to free some disk space.

## Conclusion

Ah! So here we meet, finally!  
I hope this article about Docker Named Volumes was good enough for you and I hope I explained it clearly enough so that Volumes have no more secrets for you.

Let us recap _briefly_ what we have seen about Volumes, so that it serves as a reminder when you seek something.

Images are basically a set of read-only layers, which correspond to Dockerfile statements and represent individual increments (installing programs, creating directories, files, etc.) from a base image.  
When we have an image, instantiating a container from it is as simple as---and as fast as---creating a new layer, with  read-write permissions and stacking it on top of the image layers. From here on, everything you do inside a container is recorded in this container's RW layer. This is great because it makes containers _so cheap_ you can instantiate hundreds of them in the blink of an eye, but the cons are that when you destroy the container, you loose everything (because destroying a container is simply a matter of destroying the container's RW layer), and thus, this mechanism is ill-fitted to store permanent data, such as the data from log files or from a database.

Then we discovered the Docker Volumes, which are a mechanism to _bypass_ the unionFS. When you create a Volume and mount it inside a container's path, every operations that is done at this mounpoint will **not** be recorded in the container's RW layer, instead it will be recorded straight on the host's disk, in a special, docker-specific, root-protected location (`/var/lib/docker/volumes/` by default).  
The advantage of this is that when you destroy the container, the data inside the volume remain on disk, this is called _making data persistent_. The cons of this is that it's easy to forget about a Volume after the container(s) that use it is (are) deleted, and it can end up taking a lot of space on your hard drive---but this is where the `docker volume prune` command becomes useful.

Then we saw a basic set of commands to deal with volumes (list them, mount them, delete them, delete all unused volumes, etc.).

We saw that docker provided a very easy way for a container to mount all volumes that another container uses. This is particulary useful when you need to mount the volumes of another container and the first container uses either a lot of volumes or uses Anonymous Volumes. It's also very useful when you want to have temporary containers popping up, making management actions (such as backups, pruning, etc.) and then popping out.

Last but not least, we saw that we could also mount a host's directory inside a container, to share data directly from the host to the container. We insisted that creating and mounting a Volume and sharing / mounting a host's directory inside the container **is not the same** and they correspond to two **different** use-cases.  
I invite you to re-read the section about this if it's not absolutely clear, but the TL;DR is that:

- Mounting a Docker Volume is meant to store data persistently (database's data, log files, etc.)
- Mounting a host's directory is meant to share data between the host and the container (share some developement code, share `/etc/localtime`, etc.)

Obviously these are only two very common use-cases, but you can find some others. This very distinction is the **core** of this article, really. It's **the** thing you need to understand, because when talking or dealing with Docker Volumes on the IRC `#docker` chan, it's the remark I make most often : "you're not using Volumes correctly". Now I made an (hopefully clear) article about them, so I hope it will help clear the confusion.

I think it's all good for now, as usual, **any** remark is welcomed at [nschoe@protonmail.com](mailto:nschoe@protonmail.com), whether they are thank-you messages, positive or negative feedbacks, a request for clarifying some unclear notions, etc. Do not hesitate.

In the next article (Part V), we will see "the last" part of Docker before we can make really complex setups and become real Docker hackers: it will be about Docker Networks. Along with the Docker Volumes, the Docker Networks are---in my opinion---the greatest feature that Docker brings on the table. In my own stacks, I use them _extensively_ and they allow you to ramp up your stacks, it's almost undecent how cool they are.  
But as often with Docker, they are a _very_ misundertood feature, so I want to make the article extra-clear, with lots of examples etc. I will work as hard as I can not to take too long to write the article, but it's most likely to be a big one too (but I think you're accustomed now, and you seem to like it, don't you?!).

So this is a good-bye, and see you in Part V!
