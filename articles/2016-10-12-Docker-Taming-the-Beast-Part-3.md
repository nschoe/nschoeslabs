---
title: Docker: Taming the Beast - Part III: Building Images
description: In part III, we will explore as much as we can of the BUILD time, that is, how to write correct Dockerfiles, how to build images, what happens when images are built, the good practices, the traps to avoid, etc.
toc: yes
tags: docker,container,continuous-integration,continuous-delivery
---

## Introduction
Hi and welcome in Part III! In [Part II](http://nschoe.com/articles/2016-07-03-Docker-Taming-the-Beast-Part-2.html), we have been learning about the fundamental concepts of Docker, the difference between images and containers (I really hope you have not forgotten about this, this is _fundamental_).  
We have also seen basic commands and concepts about images, containers, we have briefly talked about volumes, etc.  
If you found that it lacked examples, you will be happy with this article I think. In the few examples that we saw, we have always _used_ images, that we have pulled from the Docker Hub---I think we have only used `ubuntu` and `nginx`.

Today, in this article, we will see how to _create_ images! It means we will be able to create whatever environment we want to host our applications, our tests, etc.  
Excited?!

### The Process
Before we really begin, we need to talk a bit about the process of building an image, what is going on, etc.  
Although we have never said it _formally_, I've hinted at this several times, but I'm going to say it for good:

> An image is built from a Dockerfile

Now, it's said. Cool.

What's a Dockerfile?!

Ho yeah, that...  
So a Dockerfile is like a Makefile, but for Docker, really. It is a file that describes the steps to build your image. These steps are called **statements**, so remember this: a Dockerfile is made of statements.  
I'll even go further and say that the **unit** of Dockerfile---_i.e._ the smallest action---is the statement.

Why do we care? Because of layers.  
You certainly remember that thing with layers? An image is comprised of layers, each layer brings a _diff_ to the previous one. Ever wondered how these layers get created? Well, here we are:

> Each statement in a Dockerfile creates a new layer

Conceptually---we will see the real thing in a minute---a Dockerfile is written as:

```Dockerfile
STATEMENT 1
STATEMENT 2
STATEMENT 3
```

When the docker daemon reads this Dockerfile, it will create a first layer after `STATEMENT 1`. This layer will be actually be an **intermediate image**. Then, from this intermediate image, the docker daemon will run `STATEMENT 2`, and if it succeeds, it will create a second layer, this second layer will be another intermediate image.  
this process repeats itself until the last statement is run, and if it succeeds, it creates the final layer, which is the real image.  
And voilà!

You know when you are reading something and then some concepts get magically introduced, with its load of black magic. I hate it, and I always try to avoid that in my articles.

Here you might be feeling the same thing about this "intermediate image" thing. Well don't. This is easy: intermediate images are nothing more that unnamed images---they are all called `<none>` and have an `ID`---that docker "hides" from us. By "hiding" I mean that they don't appear when you run `docker images`, to see them, you need to add the `-a` or `--all` parameter: `docker images -a`.

Be warned: there will usually be _a lot_ of images when you run `docker images -a`. There is one intermediate image for each layer for each Dockerfiles.  
So if you have been playing a bit with Docker, this means a lot. Now you're glad `docker images` hides them by default, aren't you?!

### Build Context

What did you just say?  
Don't worry it all has to do with the Docker philosophy of immutability.

![](/images/not_sure.jpg "")

That's me. Sorry. Let's take a step back.  
Why was Docker invented? What problem did it---initially at least---solve?  
Docker was meant to make it easier for developers to ship their applications to their customers. You know the drill: you develop locally, you test locally, it works. Then you send the source code to your customer, but it fails to build or to run. This sucks.  
Usually it's because your system, the one on which you developed, had a particular library, or version of a library that made it work. Maybe this is something you did at the very beginning, to quick fix a compilation error, you know a "temporary hack". Well it became permanent and your forgot about it. It may be something so simple as to define an environment variable or something more complex.  
Or maybe it just happens that your customer has a particular set of library versions that collides with your software. Or maybe it's a quantuum fluctuation---in which case Docker won't help you!  
Anyway, the hard truth is that **your** system and **your customer's** system are **different**, and sometimes, it breaks things.

Docker was meant to solve that problem: "what if we could make it so that both the developer and the customer have the _exact_ same environment?".  
This would be The Answer, the dream. Of course, you could achieve this by running a VM (Virtual Machine) that you would set up as you wish. This worked. Actually this is how it was done by many, for a long time.  
But it's heavy. We will not enter the software container vs. virtual machine in this article, but starting a whole VM just to run a small software is costly: the host computer has to do a lot of virtualization and stuff. We needed a better way.

Basically this is why docker was born---or so I believe, let me remind you that I am not a Docker developer myself, and I'll happily fix that part if a Docker developer gives me a better version!

Anyway, what do you need to build an environment that would be the same on your machine and on your client's machine? This problem is partially solved by compilation toolchains: you have Makefiles that, given your code, will create the "same executable". So that's a start: a Makefile; here it's called a Dockerfile.  
Very roughly speaking, Makefile use two kinds of library files: static and dynamic. Still very roughly speaking---this is not a course on compilation! I'm just making analogies here---the Makefile can use some provided, static library files that it directly uses or refers to; and it can be configured to make your software rely on exiting, dynamic libraries that your customer has already installed. These libraries are generally shared among several softwares, so that there is no need to install them several times.

Let's forget about those dynamic libraries and focus on the static ones, they are generally `*.a` files. The point of my analogy here is to show that for source code, the Makefile only is not enough: sometimes, you provide other files---in this case, static library files---that is part of the _build context_.

In Docker this is similar---we're back to real Docker talk now---the Dockerfile only is generally not enough. In your Dockerfile---which is a sort of Makefile, remember---you will write statements, instructions to build the environment you need: create directory, set environment variables, change permissions, etc.  
But what if you need to copy data inside your environment? What if you need to "import" something in the image you are building?

Docker allows you to do that, we will see it a bit later, but we have a `COPY` statement, which copies something from the host to the image you are building.

#### Yeah, so What?
I know, by now you should be pissed and thinking "what the heck? all that fuss for a copy instruction?". And actually yes, all that fuss for a copy instruction, but this is _crucial_. I'm not talking about the copy instruction that is crucial---of course it is, seems logical---but I'm talking about what I'm going to explain.  
If you are _very_, **very** good and very astute---or if you already have some Docker knowledge---something might be bugging you at this point. Especially if you read and understood very well my paragraph about the reason why Docker was invented.  
Rather than giving it to you plainly, I'd like to try and make your _see_ it.  
Let's suppose that you have a fairly good idea of images: we have talked quite a lot about them. We still don't know much about Dockerfiles, but for the sake of it, just think of it as a Makefile, a recipe to build an image.

Suppose we have a folder of icons that we need to put in our image, because we are building a web application inside the docker image and we need those icons. Seems simple?

I will fully introduce the statement later, but for now, know that the `COPY` statement takes two paths: the first one is the path of the file or directory in the host, and the second is the path at which we want to copy it inside the image.

How would you go about copying that `icons/` directory?  
`COPY icons /var/lib/icons` right? Seems simple. It requires one thing, though: that there is an `icons/` folder in the current directory, because, really, the previous statement is equivalent to `COPY ./icons /var/lib/icons`.

What if you are an organized person and structured your workspace by separating resources: a directory for the source code, a directory for the resources, with inside it one for the fonts, one for the images, etc:

```Bash
.
├── code/
├── Dockerfile
└── resources/
    ├── images/
    │   └── icons/
    └── sounds/
```

You would simply do `COPY ./resources/images/icons /var/lib/icons`, seem simple enough.

Yeah but the Dockerfile can be viewed as part of the source code, right? So the structure might be:

```Bash
.
├── code/
│   └── Dockerfile
└── resources/
    ├── images/
    │   └── icons/
    └── sounds/
```

How do you copy the icons here? Simple: `COPY ../resources/images/icons /var/lib/icons` this is classic Unix, and it is starting to become quite repetitive, I know. Bear with me for a second.

If we keep this logic, what prevents us from doing something like `COPY ../../../../something /path/to/file/in/container`?  
Nothing, and this is a huge problem: how will that translate when it's the client doing this? What is supposed to be the current directory? What is supposed to be at `../` and at `../../` and at `../../../`?

You don't know, and you have no ways of knowing!

This is why Docker actually prevents that: it doesn't allow you to refer to parent directory in the Dockerfile. And there comes the concept of _build context_.

What you ship to your client is not the Dockerfile, but the build context.

> The build context is the directory that contains everything that is needed to build the image

So in your build context, you will place all files (code, images, fonts, resources, etc.) you need to build your image, as well as the Dockerfile. Usually, the Dockerfile is placed at the root of the build context, but this is not mandatory.

When you decide to place the Dockerfile deeper in the architecture, you need to remember that **every path in the Dockerfile instructions are relative to the build context, _not_ the Dockerfile**.

So if we consider the last architecture shown, with the Dockerfile inside the `code/` directory, if we want to copy the `images/` directory somewhere in our container, the statement would be `COPY resources/images /path/in/container` and not `COPY ../resources/images /path/in/container` because:

1. I've already said that you can't access a parent directory in a Dockerfile
2. The paths are relative to the (root of) the build context

#### Do I Really Care About this Context Thing?
Yes you do, because for a starter we love to Make Things Rights, then if you do it wrong you just won't be able to build your image---because you would not be able to access your files for instance---and to explain the third reason, we need to examine rapidly what happens when you (try to) build an image.

An image is not built by the docker client but by the daemon, but this daemon knows nothing about your Dockerfile nor the build context it's in. So,

> The first thing that is done when building an image is the build context being sent entirely to the docker daemon

So yeah, that lazy docker client bastard simply takes the whole build context and sends it to the daemon, which absorbs it and tries its best to build thing. But here is the thing: if you were lazy and did not want to think about your directory architecture for a minute---because, really, that is all it takes to get it right---well, you might send some huuuge directory.
Ho yeah, especially, do not use `/` (root) as a build context, because it would send the entire content of your hard drive...

So, take a minute to organize your build context, and you'll be fine.

#### You Know, Sometimes I Don't Ship to Clients but Build for Me!

Okay you are right, Docker is not only about shipping applications to clients, you might want to integrate docker in the core of your code, your application. And it would suck to have to change your code design just for that.  
Or sometimes, there is no _perfect_ place for the Dockerfile, and it _will_ end up sending hundreds of useless file to your docker daemon, which will then take ages to build.

The solution is to use a `.dockerignore`. This is a file whose purpose is the same as a `.gitignore`: it instructs the Docker client to skip files and/or directories _i.e._ not to send them to the daemon.  
For those who don't know about `.gitignore`, the syntax is:

- one entry per line
- simple patterns are allowed, like `*.png` to ignore all PNG images
- include whole directories with a trailing slash: `images/`

With that in mind, you should be ready for the rest!  But please, do no make the mistake of forgetting this or thinking that it is trivial, because soon enough you will be building images that are GB large, and if you mess up this, it _will_ take forever to build your image!

I think we are ready now, so it's time to do real work!  
At this point, since the first part was fairly dense and the next part **will** be even denser, I suggest you take a break.

![](/images/coffee_break.png "Coffee time!")

## Building Images and Writing Dockerfiles

Hello again everyone, are you ready? Let's get started!

Here we are: we are about to create our first Docker image, and thus write our first Dockerfile. Before we begin, let's see some basic rules, because we'll need that to write our Dockerfiles. After we've seen this rules, we will begin writing a Dockerfile and build an image.  
We will try to take things one at a time, building several images, each one introducing some new concepts. I think it's more easily understandable than a big blob of information.

So, what are the basic rules? Not much really. As we have seen previously, a Dockerfile is like a Makefile, for Docker images. Generally you will call the file `Dockerfile`, but it's not mandatory: you _can_ call it otherwise. But in this case, you will need to specify the new to `docker build`, the command we'll use to build image.

As I have explained before, a Dockerfile is made of 'statements', _one statement per line_. Each statement is comprised of an instruction and the arguments. The instruction is written in capital letters. As we've seen before, a Dockerfile is read sequentially, from top to bottom. Each statement makes a new image, and each statement is run against the previously made image.  
That's it, you cannot "create" something on your own: you take an image, run a statement against it, and if it succeeds, you get a new image. This new image is either your final image or the base image for your next statement.

I'll stop you right here, because I _know_ you are about to ask "if we can only run statements against an image, what image does our _first_ statement run against, we need to have _something_ to begin with".

This is perfectly true, and this is a fundamental aspect of image building: _every_ Dockerfile must begin with a "special" instruction: `FROM`. The `FROM` instruction describes which image you build from, _i.e._ which image your first statement is based on. By the way, this first image, the one passed as argument to `FROM` is called the _base image_.

So for instance, if you want to base your image on an Ubuntu 16.04 image, the first line in your Dockerfile will be `FROM ubuntu:16.04`. Simple.

Ho yes this is not mandatory, but still useful when reading Dockerfiles: usually, we indent the Dockerfile so that all parameters to statements are aligned. You will see what I mean in the examples.

So essentially, we've created the smallest Dockerfile possible: one statement, `FROM <base-image:tag>`.

It's not very helpful, because, if you remember images being layers, Your image will be _exactly the same_ as the base image at that point.

...

Okay I see people moving in the back, I think I know why. You must be feeling that I did not _really_ answered your question about the beginning of a Dockerfile. Okay, I said Dockerfiles began with a `FROM` instruction and took a base image as argument, like `ubuntu` or `nginx`, but you are wondering "how do these people create the ubuntu image? FROM which image do they begin?".  
If you want to build something _very_, _very_ minimalistic, like you want to create a base image as `ubuntu` or `debian` or `alpine`, there is one special Docker image that you can start `FROM`, it's called `scratch`. Literally. This image is _empty_.  
So if you want to do that, you start your Dockerfile with `FROM scratch`, and you go on. But we won't be doing this. Because you need to understand: there is _nothing_ in this image, no `apt`, no `yum`, no `pacman` nor `emerge`. You have to build everything, literally.

I've never done it, by the way, because I have never had the need for that. If you want to build an image that is a bit generic, you will generally start from `ubuntu` or `debian`. If you begin to care for space consumption and image size optimization, you will start from `alpine`. This is a very very small and minimalistic image that many people use.  
If you need to dockerize a "classic" application, something mainstream, you will generally find images just for that: like `nginx`, `mariadb`, `postgres`, etc.

### A Basic Example

Let's start writing a basic docker image. Let's think about what we want. To make things simple, we want to build an environment based on `ubuntu:16.04`, we will create some directories and copy a file at a specific location. Sounds ok?

As we extensively saw earlier, it all begins with a build context. Rather than starting from wherever you are and creating a `.dockerignore` file, let's create an empty directory and start from here:

`mkdir basic_context && cd basic_context`

Let's create the Dockerfile: `touch Dockerfile && $EDITOR Dockerfile`.  
First statement ever: `FROM`, so ours will be `FROM ubuntu:16.04`. There is a Docker statement that I like to _always_ put in second, even though it's not mandatory, this is the `MAINTAINER` statement. It's used to indicate who maintains / develops this image. This is metadata, but is always useful, especially if you push your image on the Docker Hub later. Anyway it's not mandatory, but I'll always include it, because it set things: `MAINTAINER nschoe <nschoe@protonmail.com>`.  
By the way, there is no special syntax for this instruction, so you can put your name, your alias, your email, anything you want.

Now, the file, we said we wanted to copy a file, so we need to create it, on the host before: `touch README`. Write anything you want in the file: `echo "This is my first Docker image, be indulgent!" > README`.

Then we wanted to create some architecture---and for the fun of it---copy our file into this architecture.

So far we have seen two specific Dockerfile statements: `FROM` and `MAINTAINER`. It's time to see a new statement, this statement is very generic as it's the statement that allows us to run any command inside the image, so logically this command is named `RUN`. It's used simply as `RUN <cmd>` and `<cmd>` will be called in the container. If the return code is 0, it's treated as a success and the next statement is run, otherwise, Docker stops the build because it encountered an error.

So, to create our directory structure, it's as easy as writing `RUN mkdir -p /workspace/test/path`. Easy, isn't it?

The statement we will use to copy our `README` file inside the image will be `COPY`. If you've heard or read about another command and you're wondering, wait a bit, I'll give a definitive answer later in this article. At the moment, use `COPY`. So it's `COPY README /workspace/test/path/`.

By now, your Dockerfile should look like this:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/test/path
COPY          README /workspace/test/path/
```

As you can see, I have aligned the arguments of all statements. This is much more readable, and I suggest you do the same.

Now that the Dockerfile is redacted, its time to build the image. In order to build the image, `cd` at the root of the build context. The command to build the Docker image is `docker build`. Usually it takes an additional argument `-t` or `--tag` which is used to tag the image.  
We need to stop for a second here, because there is some abusive language going here: even though the parameter is called `--tag`, it is used to give either an image's name only---technically the image's `REPOSITORY`---or the image's complete name---including `REPOSITORY` and `TAG`.  
So really, when we're talking about "tagging an image" it means "give it a name or a name:tag".

Now that this is cleared up, let's build the image: `docker build -t first-image .`.
Everything in the previous command is important:

- `docker` is the command used to use the docker cli
- `build` is the subcommand
- `-t first-image` is the parameter that instructs the docker daemon to name our image `first-image:latest` when it succeeds---remember that when we omit the tag, it always means `:latest`
- and the final dot `.` is necessary because it indicates the docker cli what is the build context; and if you read carefully above, what directory it will _recursively_ send to the docker daemon.

When used like this, the `docker build` command will use the Dockerfile named `Dockerfile` _at the root of the build context_. If your Dockerfile was not named `Dockerfile` or if it was in a subdirectory of the build-context, your would need to specify the `-f` or `--file` argument.

#### What's That Gibberish on The Console?

We can learn quite a few things from the output of the `docker build` command, you should have something like this:

```Bash
$> docker build -t first-image .

Sending build context to Docker daemon 3.072 kB
Step 1 : FROM ubuntu:16.04
16.04: Pulling from library/ubuntu
43db9dbdcb30: Pull complete
2dc64e8f8d4f: Pull complete
670a583e1b50: Pull complete
183b0bfcd10e: Pull complete
Digest: sha256:c6674c44c6439673bf56536c1a15916639c47ea04c3d6296c5df938add67b54b
Status: Downloaded newer image for ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Running in 31ac04bfc8ee
 ---> 82a2788a424a
Removing intermediate container 31ac04bfc8ee
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Running in 4260358a8c05
 ---> 8bec456786b9
Removing intermediate container 4260358a8c05
Step 4 : COPY README /workspace/test/path/
 ---> 226f6ed031e4
Removing intermediate container 19ce755dc4d8
Successfully built 226f6ed031e4
```

The first line is interesting: "Sending build context to Docker daemon 3.072 kB" illustrates what I was talking about about sending context to the daemon. The client takes the whole context and send it to the daemon: this is always the first step. This can take quite a while if there is 5 GB to send.

Then if you look carefully, you'll see `Step 1` to `Step 4`, these four steps correspond to the 4 dockerfile statements. Each statement is a step in the build process and creates an image.  
The first step for me was to download---or `pull` in the docker language---the image for ubuntu, we can see the four layers that comprise this image.  
Then the pull is complete, we have ` ---> 42118e3df429` which means "the previous step created an image whose `ID` is `42118e3df429`".

And now we will see a pattern that repeats itself, this pattern is:

```Bash
Step N: <Dockerfile statement>
 ---> Running in <ID #1>
 ---> <ID #2>
```

This is rather verbose, it means that the `N`th step (_i.e._ `N`th statement in the Dockerfile) is being run. But remember what a step really is: it's an action that is run against the previous image. So the process is roughly this one: create a container from the previous image, run the action  described by the current statement, if it succeeds, save the new container's state as a new image.

Well guess what? This is _exactly_ what is going on, and it's shown on the console: the `Running in <ID #1>` means that the current statement is run in a container whose ID is `ID #1`. And the `---> <ID #2>` means that the statement was successful and it created an new image whose ID is `ID #2`.

How can we check that? Easy, we will get the ID of an intermediate image, and see if it corresponds. Let's take this:

```Bash
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Running in 31ac04bfc8ee
 ---> 82a2788a424a
```

(At this point, you need to take **your** Step 2). It seems to mean it created an image with ID `82a2788a424a`. Let's check it: `docker images`. Nothing.  
Yes I think I have mentionned this earlier, but by default, `docker images` do not show intermediate images (because it would then be very verbose), if you want to see _all_ images, including intermediate images, you need to pass the `-a`or `--all` option, let's do this: `docker images -a`.  
And _now_ you should see an image with your ID.

So now that we checked the intermediate image exists, we will create a container from it, and "log in" inside the container---let me remind you that when I said "log in" into a container, it really means to start a shell inside it. So let do just that: `docker run --rm -it 82a2788a424a bash`. This command should have no secrets for you anymore, but _for the last time_ I'm going to refresh your memory if needs be:

- `docker run` is used to create a container from an image
- `--rm` is used so that when the container stops (when we exit it), it will be automatically deleted, we're doing this because otherwise we will have a container hanging around that we don't care about
- `-it` is really two short parameters: `-i` and `-t` which respectively runs an interactive session and emulates a (pseudo-)tty
- `82a2788a424a` or your ID is the name (or ID) of the image from which you want to create a container
- `bash` is the command your want executed when your container starts.

So execute the command, and you should be presented with a prompt: `root@57477214227c:/# ` (the number will be different). So you are in a shell (bash in fact), you're the user `root`, the hostname of the container is `57477214227c` and you are at the root of the filesystem `/`.  
Everything looks fine.

Time to check our statement, remember it was Step 2, which is `MAINTAINER <name-you-put-there>`. At this point, Step 3 was **not** run, right? And Step 3 is `RUN mkdir -p /workspace/test/path`, so at _this_ point, since Step 3 was not run, the directory `/workspace/test/path` should **not** exit, right? Easy enough to check: `ls / | grep workspace`.  
Nothing. Good.

Of course now we need do the same thing with Step 3, to make sure I'm not saying gibberish. So let's exit the shell with `exit` or `CTRL + D`.  
Now let's do the same thing with the ID of the intermediate image created after Step 3 was successful, we had:

```Bash
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Running in 4260358a8c05
 ---> 8bec456786b9
```

So the command is `docker run --rm -it 8bec456786b9 bash` (again, use **your** ID, obviously). You should be presented with a new prompt, which looks very much like the previous one, in fact only the id after the `root@` should change: when you don't explicitly specify a hostname for your container, docker generates a random one.  
Now we are in a container instantiated from an image that was created after Step 3 was successful, and Step 3 was `RUN mkdir -p /workspace/test/path` so let's check it:

```Bash
root@0ef0d246be38:/# ls / | grep workspace

workspace
```

Ah! So we _do_ have a directory `/workspace/` and sure enough the whole path is there:

```Bash
root@0ef0d246be38:/# ls /workspace

test
```

So we have our `/workspace/test/` and then

```Bash
root@0ef0d246be38:/# ls /workspace/test/

path
```

we have our `/workspace/test/path/`. Everything's fine. Do we dare take a look inside `path/`? Of course:

```Bash
root@0ef0d246be38:/# ls /workspace/test/path
```

Nothing. Of course it is empty! We are only after Step 3, where we only created the directory!  
It's only in Step 4 that we copy the README file into this directory. Come on, last effort, let's check it!  
First `exit` or `CTRL + D` to exit the shell and quit the container, then "log in" into a container created from the intermediate image created after Step 4 (you should know how to do it by now). `docker run --rm -it 226f6ed031e4 bash`

```Bash
root@548bf3623cba:/# ls workspace/test/path/

README
```

Ho yeah! Everything's working according to predictions, and you can `cat` the README, you'll find the text your wrote earlier.

So now we understand almost everything that `docker build` outputs on the console, the only remaining lines are those `Removing intermediate container <ID>` as shown here for Step 3 for instance:

```Bash
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Running in 4260358a8c05
 ---> 8bec456786b9
Removing intermediate container 4260358a8c05
```

This is very easy to understand: by now we've seen that what this means is that in order to run Step 3, docker creates a container from the intermediate image created after the previous step (Step 2), this container has ID `4260358a8c05`. Here, the build succeeds and thus creates a new intermediate image, whose ID is `8bec456786b9`. After it did so, docker exits the container used to build this new intermediate, and since it doesn't need it anymore, it simply deletes it. You can see that the ID in `Removing intermediate container` is the same at the ID in `Running in`.  
Think of it as docker running the container with the `--rm` option. It helps keep the system clean.

If for some reason you don't want docker to remove intermediate container when it builds an image, use the option `--rm=false` to `docker build`, something like `docker build -t <image-name> --rm=false .`. It will then keep all intermediate containers. It might be useful for debugging or if you're curious. Be careful as it will leave as many containers as there are steps in your Dockerfile.

So you should be proud: we have just built our first real Docker image!  
We can create a container from it if we want: `docker run --rm -it first-image bash` and explore it a bit.

### A Quick Word About Tags

We have just learned a lot of things, and I'll give you time to digest it, so let's talk about something less difficult, still useful to know.

When we built our image, we tagged it with `-t first-image` which gives the final image a name. First good thing to know is that you can give your image several names: `-t foo -t bar` will build your image, and when it's successful, it will create 2 images: `foo` and `bar`. Remember that since these images are identical and since images are built of re-usable layers, it won't change space consumption on your drive. This is cool.  

#### What If My Finger Slipped and I Forgot to Tag?

It's not a problem. If you forgot to specify the tag name---or if you changed your mind afterwards---you can tag an image after it's created. The command is `docker tag <image> <name>`. Simply get the `IMAGE ID`, and use it as `<image>`.  
With this, you can even tag an intermediate image and make it a fully-featured one!

If we scroll up a bit, we talked about intermediate image `8bec456786b9` which was the image generated after Step 3 was run. If we decide that this image is worth naming---to create a container from, later---we can do it with `docker tag 8bec456786b9 sensible-image`. And voilà!

### Recover From a Broken Build

I am sorry to tell you this, but sometimes... builds fail. It may not seem like a big deal to you, but keep in mind that we have only made one small, basic image which literally took seconds to build.

But if you use Docker in a bit more advanced manner, you will see that images tend to grow both in size and complexity. So it might fail. When it fails, sometimes it is easy to see why: the output from `docker build` is enough, sometimes it is quite a mystery---for the info, what I'm about to show you _really_ saved my butt when I faced the `/etc/hosts` problem (more on this later in the article).

When the failure is a mystery, you _can_ blindly edit your Dockerfile, try again and hope that it works; **or** you can be smart and actually do something about it. Actually... before I tell you, **actually** take a minute to think about what's a smart thing to do (if last time you did not actually take time to think and find the solution, here's your second chance!).  
You _do_ have everything your need, at this point, to finds the solution.

...

So? Have you found a correct answer?  
What we want to do is run the steps in the Dockerfile manually, so we want to start from as far as the build succeeded, and proceed from here.

To illustrate this, let's take our previous Dockerfile and change it slightly, deliberately make an error:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/test
RUN           echo "test" > /workspace/test/path/foo.txt
```

As you can see, in the 4th step, we are trying to echo something into a file, but the directory containing the file doesn't exit: `/workspace/test` exists but not `/workspace/test/path/`.  
Suppose we did not see this, and try to build:

```Bash
$> docker build -t second-image .

Sending build context to Docker daemon 3.072 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Using cache
 ---> 82a2788a424a
Step 3 : RUN mkdir -p /workspace/test
 ---> Using cache
 ---> 3e99a5614e5a
Step 4 : RUN echo "test" > /workspace/test/path/foo.txt
 ---> Running in 34807c687d3f
/bin/sh: 1: cannot create /workspace/test/path/foo.txt: Directory nonexistent
The command '/bin/sh -c echo "test" > /workspace/test/path/foo.txt' returned a non-zero code: 2
```

**Note:** for _this_ particular example, I'd like you to use `-t second-image` if possible, or at least an image's name that you did not already have---check with `docker images`. Bear with me, I'm explaining myself in a couple of paragraphs.

Okay, it clearly shows the error, and on my screen, it evens appears in read, but _suppose_ we have a non-informative log, or maybe that we did not see it because there's so much logs that we missed it. Anyway, my goal is to show you how to do it, so obviously this is a trivial example.

You saw that we tried to tag our image with `second-image`, or at least with a non-already-existing image name. The build clearly failed: the docker daemon stopped building after it encountered an error---_i.e._ return code different that `0`.

Try to check images with `docker images` now. You should see something like this:

```Bash
REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
<none>                  <none>              3e99a5614e5a        4 minutes ago       124.8 MB
first-image             latest              226f6ed031e4        23 hours ago        124.8 MB
```

Your second line might be different if you played a bit before, trying to build other images---which I strongly encourage you to do!---but the first, which is the last, should look similar. You see that it's **not** tagged `second-image`, but `<none>` instead.  
Indeed, as I previously hinted---but now it's formally said---Docker only tags the image if the build is successful.  
If you look more closely: the `IMAGE ID` is the ID of the _last intermediate image whose built succeeded!_ Indeed, the `3e99a5614e5a` is the same as the result of Step 3:

```Bash
Step 3 : RUN mkdir -p /workspace/test
 ---> Using cache
 ---> 3e99a5614e5a
```

The Step 4 failed so it stopped there. And this is very neat, because now we can recover cleverly from a broken build: we can create a container from the last intermediate image, try to run the command that failed in the last step and see what goes wrong!

Let's do that: `docker run --rm -it 3e99a5614e5a bash`.  
Now that we are "logged in", let's try to run the step 4's command:

```Bash
root@342819172ca9:/#  echo "test" > /workspace/test/path/foo.txt

bash: /workspace/test/path/foo.txt: No such file or directory
```

And now the message is generally clearer---of course here it's trivial so the error message is the same, but sometimes your commands are more complex and thus it's easier to debug inside the container.

Now you can exit the container, change your Dockerfile and try the build again: you've got the basics of recovering from failed build and debugging.

### More Specific Examples

Okay, so we have played with Dockerfiles that were 4-lines long, and with the same statements, it's time to step up!  
As always in my articles, I prefer to make you _feel_ things, so that you can remember them and understand them better. I could give you the comprehensive list of Dockerfile statements, but you'd read them quickly and forget about them. Besides, if you do want the full list, it's available on the docker documentation. So it's not the place for that.

Instead, we will iteratively ask questions, and answer them with an example, each time introducing some new use case, some new statements.

#### Am I Always Root in Docker Images?

That's a good question, as you have seen in the few Dockerfile examples we have been writing so far, every time we "logged in", we were the root user. This may be a problem sometimes.  
In fact, it's not _really_ a problem, because the root user inside a container is not the root user of the host system. But still, it's bad practice to be root.

There are two parts to solving this problem, the first part is: create the non-root user(s) that you need in your image. This is easy: we want to run `useradd`---or `adduser` in debian-based images. I'll let you decide if you need more specific options, but a classic use of `useradd` is `useradd -ms /bin/bash <user-name>`. The `-m` option creates the user's home directory, and the `-s` option defines this user's shell.  
In the Dockerfile, you run this command with `RUN`, obviously.

But now we need the second part, but this second part is not necessary obvious as it is, let's consider this Dockerfile, for instance:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/test/path
RUN           useradd -ms /bin/bash nschoe
COPY          README /workspace/test/path/
```

Quite similar to our first Dockerfile, except that we don't want the README file to be root's, so we create the new user `nschoe` in my case, but feel free to change it, and then we copy the README file.  
Let's build that Dockerfile, create a container from it and inspect the file---by now you should be used to this sequence of commands, so I'm just going to show you the shell session, not the individual commands and their results each time, you should be able to follow quite easily.

```Bash
 $> docker build -t non-root-image .

Sending build context to Docker daemon 3.072 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Using cache
 ---> 82a2788a424a
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Using cache
 ---> 8bec456786b9
Step 4 : RUN useradd -ms /bin/bash nschoe
 ---> Running in 3d40e25da97a
 ---> 7b7449b35750
Removing intermediate container 3d40e25da97a
Step 5 : COPY README /workspace/test/path/
 ---> f3104542c278
Removing intermediate container a5ed4ce0904d
Successfully built f3104542c278
```

**Notes:** just before I go back to showing you the rest of the shell session, I just want to make a point, look at the previous output, can you see the lines `---> Using cache`?  
For instance in

```Bash
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Using cache
 ---> 8bec456786b9
```

We have already built images whose first three steps were identical, and now you can see Docker's magic in effect: it will not recreate the images to run those statements, since it has already run them before. So exactly as it says here, the Docker daemon uses cached intermediate images. Remember again that images are layers, so it really is a matter of re-using a previously built image.  
Smart, right?!

Anyway, back to our use case:

```Bash
$> docker run --rm -it non-root-image bash

root@58074c87bc22:/#
```

Well I have a bad feeling about this... We've just logged in and we are still `root`, despite the fact that we created a new, non-root user.

```Bash
root@58074c87bc22:#/ ls -lh /workspace/test/path/

total 4.0K
-rw-r--r-- 1 root root 45 Jul 28 06:37 README
```

Hum okay, this seems like a total fail: our README file is still `root`'s.  
But actually this is perfectly normal---I'm sure you were not fooled by this, so I'll go fast with this one---we have just created the user, not switched to it. We can check that the user does exist:

```Bash
root@58074c87bc22:#/ cat /etc/group | grep nschoe

nschoe:x:1000:
```
So yes, the user exists and has `UID` 1000, which is common for the first UNIX user to be created. We can even `su nschoe`, so everything worked as intended.

The second part to really solve our problem is a new Dockerfile statement: `USER`. This statement takes one argument: a user.  
When running `USER nschoe` in a Dockerfile for instance, it basically means that at this point, it's as if we had `su nschoe`, and all subsequent UNIX commands will be run as this user. Additionally, it means that `nschoe` will be the default user with which we will execute commands and/or log in.

Let's try it, change your Dockerfile to:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/test/path
RUN           useradd -ms /bin/bash nschoe
USER          nschoe
COPY          README /workspace/test/path/
```

And let's check if it worked---note that I'll build this image with the same name `non-root-user`, but I'll use a different tag: `2`. This is to use what we previously learned about tags for images: it's the "same" image, but a different version. Tags are _perfect_ for this!

```Bash
$> docker build -t non-root-image:2 .

Sending build context to Docker daemon 3.072 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Using cache
 ---> 82a2788a424a
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Using cache
 ---> 8bec456786b9
Step 4 : RUN useradd -ms /bin/bash nschoe
 ---> Using cache
 ---> 7b7449b35750
Step 5 : USER nschoe
 ---> Running in 094bc1767151
 ---> 510ab0f832c2
Removing intermediate container 094bc1767151
Step 6 : COPY README /workspace/test/path/
 ---> a0f0f1f00c2d
Removing intermediate container b488db3044a8
Successfully built a0f0f1f00c2d

$> docker run --rm -it non-root-image:2 bash

nschoe@5eea9d1ec4ff:/$
```

Ah! Look at the prompt: it's `nschoe@...` and not `root@...` anymore! So we _did_ something here! It looks good: our `USER` statement worked, at least for this part.

```Bash
nschoe@5eea9d1ec4ff:/$ ls -lh /workspace/test/path

-rw-r--r-- 1 root root 45 Jul 28 06:37 README
```

Oh?! So README is still `root`'s.  
Yes, and this is perfectly understandable. If you scroll back up and read again, what I said is that after running `USER nschoe`, it's as if we had run `su nschoe` and thus, subsequent UNIX commands would be run as this user. This is true, but this is not what we are doing here: in order to copy our README file, we use the Dockerfile statement `COPY`.  
This has nothing to do with UNIX, it's just Dockerfile internally modifying the filesystem.  
So it's not like there is a `cp` command that was run as `nschoe`. You need to understand the difference.  
To make it clearer, after a `USER` statement, it's the `RUN` statement that will be affected, because that's the statement used to run UNIX commands inside the container.

If we somehow needed README to be `nschoe`'s and not `root`'s, we'd have to `chown` it, with a `RUN` statement. But be careful! You cannot do the `RUN chown` after you did `USER nschoe`, because _now_ the `chown` would be run as `nschoe` and obviously, non-root `nschoe` cannot change ownership of `root`'s files.

This why I'll advise you to write your `USER` statements as late as possible in your Dockerfile. As close as possible to the end of the Dockerfile: you set up your whole environment first, then instruct the user.

#### The Application I Want to Dockerize Reads Custom Environment Variables!

Is that so?  
So that's just a matter of exporting a variable, right?

Seems easy:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/test/path
COPY          README /workspace/test/path/
RUN           export FOO=bar
RUN           echo "FOO: ${FOO}"
```

Here, in the Dockerfile, we use a `RUN` statement to export an environment variable and then a second `RUN` statement to echo it. Let's build it---depending on your level with UNIX, you might see in advance what is about to happen here, in which case, good for you, skip this or read fast and focus on the rest---:

```Bash
$> docker build -t test-env:1 .

Sending build context to Docker daemon 3.072 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Using cache
 ---> 82a2788a424a
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Using cache
 ---> 8bec456786b9
Step 4 : COPY README /workspace/test/path/
 ---> Using cache
 ---> 226f6ed031e4
Step 5 : RUN export FOO=bar
 ---> Running in f82a560fe0b0
 ---> 65dff5e37c74
Removing intermediate container f82a560fe0b0
Step 6 : RUN echo "FOO: ${FOO}"
 ---> Running in a6758ed42563
FOO:
 ---> 0d8ee4fc2fd4
Removing intermediate container a6758ed42563
Successfully built 0d8ee4fc2fd4
```

So the build appears to be successful: which means no commands returned a non-zero return code. But the result from Step 6 is disturbing: it doesn't show anything after `FOO:` which hints there might be a problem.  
We can indeed check that it did not create the environment variable:

```Bash
$> docker run --rm -it test-env:1 bash

root@4a323a7cf4d0:/# env | grep FOO

```

Nothing, so yes, we don't have an environment variable named `FOO`. But this is normal: it's basic UNIX. Almost.

What _exactly_ is happening here? You remember how I said that `COPY` was not affected by the `USER` statement? It's because with `COPY`, it's "just" Docker messing with the container's filesystem. But then I said that `RUN` was affected by `USER` statements, because `RUN` was like running UNIX commands. Actually it _is_ running UNIX commands, literally. When you use `RUN cmd`, what docker _actually_ does is use the container's shell `/bin/sh` to run the command, like this: `/bin/sh -c <cmd>`.

So in order to run your command, the `USER` (`root` if you did not specify any custom user) fires a shell and run the command inside it. And it's clear now: UNIX shells cannot change the global environment, only theirs. So when you run `RUN env FOO=bar` what this does it starts a shell, changes its environment and exits---since it has nothing else to do. When it exits, the environment disappears.

So if you want to change the environment, you need to do something like `COPY`: you need _outside_ access, you need to mess with something to change the environment variables.

So it's logically a Docker statement: `ENV`. `ENV` is used like this: `ENV variable value`. What this does is modify the global environment.

Let's change our Dockerfile:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/test/path
COPY          README /workspace/test/path/
ENV           FOO bar
RUN           echo "FOO: ${FOO}"
```

**Note:** the syntax is `ENV FOO bar` and not `ENV FOO=bar`.


```Bash
$> docker build -t test-env:2 .

Sending build context to Docker daemon 3.072 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Using cache
 ---> 82a2788a424a
Step 3 : RUN mkdir -p /workspace/test/path
 ---> Using cache
 ---> 8bec456786b9
Step 4 : COPY README /workspace/test/path/
 ---> Using cache
 ---> 226f6ed031e4
Step 5 : ENV FOO bar
 ---> Running in edb72c10efb3
 ---> d48557b2ddbb
Removing intermediate container edb72c10efb3
Step 6 : RUN echo "FOO: ${FOO}"
 ---> Running in 9c5425ae108b
FOO: bar
 ---> 820bb457b1fc
Removing intermediate container 9c5425ae108b
Successfully built 820bb457b1fc
```

Now we are happy, because we see the output: `FOO: bar` from the build. Let's sanity-check:

```Bash
$> docker run --rm -it test-env:2 bash

root@c724cdff8a94:/# env | grep FOO
FOO=bar
```

And voilà! It worked.

#### Don't Lose Your Sanity!

Suppose now we want to set up an environment in which we want to do many operations on the same location, for instance, copy a lot of files in a directory. Let's see a---very dumb, I'll admit---example that will illustrate this:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/this/is/a/deliberately/long/path
COPY          file_1 /workspace/this/is/a/deliberately/long/path/
COPY          file_2 /workspace/this/is/a/deliberately/long/path/
COPY          file_3 /workspace/this/is/a/deliberately/long/path/
COPY          file_4 /workspace/this/is/a/deliberately/long/path/
COPY          file_5 /workspace/this/is/a/deliberately/long/path/
COPY          file_6 /workspace/this/is/a/deliberately/long/path/
COPY          file_7 /workspace/this/is/a/deliberately/long/path/
COPY          file_8 /workspace/this/is/a/deliberately/long/path/
COPY          file_9 /workspace/this/is/a/deliberately/long/path/
COPY          file_10 /workspace/this/is/a/deliberately/long/path/
```

So we have 10 files that we need to copy at a---seriously retarded---directory. Sure enough, the above will work:

```Bash
$> docker build -t many-files:1 .

# Skipped output

$> docker run --rm -it many-files:1 bash

root@e86ec644ef63:/# ls /workspace/this/is/a/deliberately/long/path/
file_1  file_10  file_2  file_3  file_4  file_5  file_6  file_7  file_8  file_9
```

So it works. But it sucks, because that's a pain to write 10 times this long, stupid file name---don't worry I used a shell `for` loop to generate those!

I know this sucks at even greater levels: yes I could have copied the directory directly, etc. But this is for the sake of the example.  
A much better solution would be to `cd` into the directory and copy the file at the current location. But `COPY` is not a UNIX command, it's Docker, messing with the filesystem, so we can't `RUN cd`.

Instead we can use `WORKDIR`. This statement makes it as if you had effectively `cd`ed into the directory you pass as an argument. Two effects to that: commands are now run from this directory, and when you "log in" into the image, your shell will start in the directory specified by the last `WORKSDIR` statement. Neat, right?

Let's change our Dockerfile so that it uses `WORKDIR`, at this point we will be in the target directory, so we just have to copy the files to `.`, the current directory:

```Dockerfile
FROM          ubuntu:16.04
MAINTAINER    nschoe <nschoe@protonmail.com>
RUN           mkdir -p /workspace/this/is/a/deliberately/long/path
WORKDIR       /workspace/this/is/a/deliberately/long/path
COPY          file_1 .
COPY          file_2 .
COPY          file_3 .
COPY          file_4 .
COPY          file_5 .
COPY          file_6 .
COPY          file_7 .
COPY          file_8 .
COPY          file_9 .
COPY          file_10 .
```

As you can see, it is much more readable.  
I agree that there is still a lot of boilerplate code here, the goal was not to make the smartest Dockerfile possible, but we did remove all the `/workspace/this/is/a/deliberately/long/path` as a second argument to `COPY`.

Of course, build the image to check that is works:

```Bash
$> docker build -t many-files:2 .

# Skipped output

$> docker run --rm -it many-files:2 bash

root@70f7f7eb32f1:/workspace/this/is/a/deliberately/long/path# ls
file_1  file_10  file_2  file_3  file_4  file_5  file_6  file_7  file_8  file_9
```

As you can see, first the build succeeds, then when we run as as usual, we're immediately dropped into the `/workspace/this/is/a/deliberately/long/path` path---if by chance you would not want that, you would just have to write another `WORKDIR /` afterwards---and of course, our files are here.

So `WORKDIR` can help in the situation we have just shown, but not only: sometimes you will have several commands that you will want/need to run in the same directory, and you _might_ end up doing something like:

```Dockerfile
RUN cd /path/to/the/dir && command-1
RUN cd /path/to/the/dir && command-2
RUN cd /path/to/the/dir && command-3
RUN cd /path/to/the/dir && command-4
```

This is the other case where `WORKDIR` helps:

```Dockerfile
WORKDIR /path/to/the/dir
RUN command-1
RUN command-2
RUN command-3
RUN command-4
```

And you're all set. This is cleaner, and you should always do it.

**Note:** you've seen here that every time I output the log of a build it begins with:

```Bash
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
```

In your system, however, you should have a lot of `using cache` lines instead. It's normal, so don't get frustrated because you don't get the same values I do. The way I do it is I use the `--no-cache` option to `docker build`. As it names implies, this option makes Docker ignore the intermediate images on your system, and always rebuild every step.

You should not use it normally, but sometimes, it helps. Keep that in mind.

#### The Case of apt-get

So far, the Dockerfile examples we have seen were very simple. This is normal: we lacked a fundamental operation: installing---and updating--- softwares.

Technically this is not true: we saw the `RUN` statement, and the `COPY` statement' so in theory it's possible to `COPY` installers and either `RUN` then---if they are `*.sh` executable files---or `RUN dpkg -i` on them. Oh and not because we only used `ubuntu` as a base image means it's the only one available. So you should already be able to install softwares.

The facts are: _a lot_ of images are based on either `ubuntu` or `debian` images, both using `apt` to install softwares, so it's important that we cover this.

Why do we even need to talk about this?  
A simple, intuitive way is doing something like:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>
RUN         apt-get update
RUN         apt-get install nano emacs-nox
```

This is supposed to install the latest `nano` and `emacs` (without X support) packages in the container. There are several problems however.  
First, this is simple but it's still a gotcha: `apt-get install` usually tells you a summary of what's going to be installed and asks for confirmation. When building an image, you cannot "give confirmation": you can't press `y`.  
So you need to pass the `-y` or `--yes` option to `apt-get` so it answers `yes` automatically.

This is easy, we thus have:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>
RUN         apt-get update
RUN         apt-get install -y nano emacs-nox
```

It _seems_ fine now, but actually we are getting into the real problem, we're getting serious, so pay attention.  
What we are doing here is _fundamentally_ dangerous, even more because if you try to build this image _it will work_. It's the kind of "silent" problem that is going on.

Before I explain, it's time for a little try-to-find-it-yourself. Technically, you have all information to find out what's wrong and how to solve it, so take a 1-minute break and actively try to find the problem. I'll just go fetch something to drink, because I'm thirsty.

...

So? I hope you really tried. Let's see what is wrong.

Suppose we are in January, 2016 and we build this image, what is going to happen?

- The docker daemon is going to read the Dockerfile, and begin with the first statement: `FROM ubuntu:16.04`.  
If it already has a `ubuntu:16.04` it's going to `use cache <hash #1>`, otherwise, it will pull it from the docker hub and make an image out of it: `<hash #1>`.
- Then, _from `<hash #1>`_ it's going to run statement 2: `MAINTAINER xxxx`, this is just updating metadata, but will give rise to a second image, with a second hash: `<hash #2>`.
- Then, _from `<hash #2>`_ it's going to execute statement 3: `RUN apt-get update`. So it downloads the latest package list from the Ubuntu servers---the servers listed in `source.list` really---and create a third (intermediate) image: `<hash #3>`
- Then, _from `<hash #3>`_ it's going to execute statement 4: `RUN apt-get install -y nano emacs-nox`, so it will look at the list package files that was downloaded previously in `<hash #3>` and install the latest version of `nano` and `emacs`. I don't know the release cycle, let's suppose it's `nano 2.5.3` and `emacs 24.5.2`. It creates image `<hash #4>` (which might be the name you gave `docker build` with the `-t` option).

Everything's all right, you have created your image with your software versions.

Now suppose it's now... September, 2016. You know that there is `nano 2.6.0` and `emacs 24.6.0` that have been released and you need them in your containers. So you decide to rebuild the image. Besides, you take this opportunity to add a new software: `vim` so that you can compare---and see for yourself the superiority of `emacs`, just kidding :-).  
So you change just the last statement to `apt-get install -y nano emacs vim`.

What will happen _then_?

- The docker daemon is going to read the Dockerfile and see the first statement: `FROM ubuntu:16.04`, it sees that it already has this image: it's `<hash #1>`, so it uses it, cleverly using it's layer system to avoid repeating operations and wasting space and resources.
- Then it sees the `MAINTAINER` statement and again, sees that it already has done that previously: it's `<hash #2>`. So it uses the cache
- Then it sees the `RUN apt-get update`, and _again_, sees that it has already done that before: it's `<hash #3>`! So it uses the cache.
- Then it sees the `RUN apt-get install -y nano emacs vim`, and this time, it's something new, so it runs the statement and installs the packages.  
How exactly does apt do that? To install `nano` and `emacs`---any package actually---it look into the files downloaded by `apt-get update` to get information about the location of the sources, etc.  
But if you followed closely, you'll notice that these files, the ones fetched by `apt-get update` dates back to January! Yes because they are the result of `apt-get update` and this layer was not rebuilt since docker used the cache!

So building this image in September---in our example---, you will install `nano 2.5.3` and `emacs 24.5.2` because that's the information given in your list files.

All of this happens because `RUN apt-get update` can---and actually is---cached by the docker daemon: it's the same command, so abiding by its own rules, it doesn't repeat it.  
We would need a way to specifically burst this cache, but we don't.

What's the correct way to do it, then? Easy: include `apt-get update` and `apt-get install` in the same `RUN` statement: this way, the `apt-get install` always follows a freshly new `apt-get update`. We do it like this:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>
RUN         apt-get update \
            && apt-get install -y \
            nano \
            emacs-nox
```

Simple enough. So remember, because this is a _very_ common error:

> 'apt-get update' should never be run in its own RUN statement

There is one little detail that is worth mentioning: when you run `apt-get update`, the files are placed under `/var/lib/apt/lists/`. These are the files read by `apt` when you want to install some other softwares.

When we run the command as we've just seen---in the same `RUN` statement, remember---what this does it: fetch those files and store them at this location, then install the softwares. But then what?  
We don't need those files anymore, especially not in other layers, because that would repeat the problem we've _just_ solved!  
So the _good_ practice is to remove those files after you're done installing your softwares, still in the same `RUN` statement, for the same reason, so the Dockerfile should really look like this:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>
RUN         apt-get update \
            && apt-get install -y \
            nano \
            emacs-nox \
            && rm -rf /var/lib/apt/lists/*
```

This way, after installing your softwares, it cleans up after itself:

1. ensuring subsequent layers don't get outdated files
2. leaving the image smaller (these files are no present in the image, so it's smaller, which is good for shipping!)

### ADD or COPY? The Definitive Answer

Sometimes there are problems that are open questions: there is no real answer, and you can argue about all solutions. This is both interesting and frustrating, I'm talking about emacs vs. vim, Linux vs. Windows, Gnome vs. KDE, etc. In each case, there are very clever people that use either one.

Sometimes, however, problems are solved and have an answer. Sometimes---in very rare cases---there is a "hard truth": one case is _not_ possible. But more often, there is a _good practice_ and a _bad practice_.

This is about that. You remember the `COPY` statement that allows your to copy files from your host in your image? There is another one that can achieve that too: `ADD`. Both `COPY` and `ADD` can insert files into your image.  
But they **should not** be used interchangeably.

The TL;DR is that you should probably use `COPY`, like in 99% of the cases. `COPY` does exactly what you're expecting it to do: copy a file---or directory---from your host to your image. Period.

`ADD` is more powerful---and thus more dangerous. If you use `ADD` to copy just a simple file or a directory, it acts as `COPY`---and this is why many people mistaken and use `ADD`, the usefulness of `ADD` come when you want to copy an archive---a compressed tarball---and extract it in the image.

When you `ADD` an archive, not only does it copy it in the specified location inside the image, but it also _automatically_ extracts it. Sometimes, it's what you want to do---these are the cases when you should use `ADD`---as it saves you a:

```Dockerfile
#...
COPY    file.tar.gz /code/
RUN     tar -xvzf /code/file.tar.gz
```

allowing you to do it with one statement. But sometimes, you don't want that and you just need to copy the tarball, and it's not possible with `ADD`. So really, it's better to use `COPY`.

There is another interesting use case for `ADD`: instead of passing a local file in the build context to copy inside the image, you can pass it a URL, and it will download the file into the image. So, basically, it's like running `wget`.  
**However**, if you pass `ADD` an URL, it will **only** download the file, period. Even if that file is an archive, it **will not** extract it!  
So it's sort of tricky, and it's very prone to error. This is why you should understand these cases very well, and be sure to never use `ADD` when you don't need to. Use `COPY`.  
Besides, it's a little bit "dirty" to use `ADD` and pass it a URL, because that's getting out of the build context in a certain way, and it weakens the portability: imagine your client is behind a firewall that has stricter rules than yours: it is possible that he cannot reach the URL and so can't build your image.  
You should really include it in the build context.

So, here is a nice table recapitulating the "`ADD` vs. `COPY` problem", and this is a **definitive** answer:

|           | `COPY` | `ADD` |
| ----------|:------:|:-----:|
| Copy local file to image **(90% of the cases**) | X | |
| Copy **and extract** local archive to image | | X |
| Copy remote file from URL into image **but don't extract** it | | X |

So now you know what to do, the TL;DR is that you _most likely_ need `COPY`, **only** use `ADD` if you cannot do otherwise.

### EXPOSE or Not EXPOSE? The Definitive Answer

Let's tackle another one of these questions that everybody has an opinion about but nobody has an opinion about. It's about the `EXPOSE` Dockerfile statement.

So what is this `EXPOSE` statement? What is it used for?

I'd like to begin by saying that `EXPOSE` is not _critical_: using it or not using it will not change whether your image works or not. `EXPOSE` is a matter of _convenience_. That being said, I don't want you to rush out of this section, because even if it's not critical, it's _very_ convenient sometimes, so you should really pay attention.

We know by now that Docker is all about containerizing applications, _i.e._ isolating them one from another **and** isolating them from the host---I remind you that the "host" is the computer running docker.  
This is all very cool when you want perfectly containerized, isolated instances; but sometimes, you want to allow a small, controlled communication between your host and your container.  
Imagine you are containerizing a database---say, postgreSQL---and you want to be able to query it from your host. How do you do?

What you can do is open a port between your container and your host, a bit like a firewall. PostgreSQL's port is `5432` for instance, so you can open the container's `5432` port and map it to a port on your host, this way you can query the database.

This is the essence of the `EXPOSE` statement.

Now docker allows you several ways to do that, in order to understand _why_ and _how_, we first have to think about just one thing: I have explained roughly that we can "open a port" on the container. This is all very cool, but ports are opened on network interfaces, machines on the network, etc. Not individual softwares. And since the container is actually running on the host, what does it even mean to "open a port"?

Well, remember when we talked about bridge network interfaces? We have already seen, albeit quickly, that for each container running, docker creates a _virtual network interface_, often named something like `vethxxxx`, where `veth` is for "virtual ethernet".

Based on this, the container _can be considered_ like an independent, remote machine on a separate network. It's like it's a remote server, only it has basically 0 latency since it really is on the same machine.

But that is interesting because it allows separated stacks of networks, very very convenient. So you _can_ open a port. In order to do that, not unlike a firewall does you need to map a port on your container to a port on your host, so that when they are mapped, bytes from one end will be available on the other end.

Let's take our PostgreSQL example from earlier: inside the container, there is a PostgreSQL database running, it listens to port `5432`. So in order to communicate from the host with this PostgreSQL, we need to map the container's `5432` port to a port on the host. Which one?

**That**, my friend, is the right question! And this is where `EXPOSE` _can_ play a role.

#### Let's Make It Simple

I have written quite a paragraph here to introduce `EXPOSE`, and I am worried that it starts to confuse you and make you believe that this is complicated, **it's not**. It is all actually very simple.

In Docker, you can map ports in exactly two ways:

- automatic, random port mapping
- manual port mapping

See? This is easy.

Let's explain the second one right now: manual port mapping. As its name implies, it means that you can map ports manually. Where should that happen? At what moment do you think it is done?

It's the moment when I want you to actually take a minute to think about it. This one is another indicator if you followed and really understood the article. Please think about it and try to find the answer.

...

I hope you tried, so where/when should this happen? If you understood really everything we have been talking about, it's easy. The choice is between: Dockerfile / `docker build` and Run time / `docker run`.

It **cannot** be on the Dockerfile / `docker build`. It's **impossible**. Why? Because we are talking about _manual_ port mapping, so it implies that it needs your input. It would mean that you needed to change the Dockerfile before building. It would also mean that this is very dependent on the machine it's being built. So it cannot be.

Of course it's on run time! So when you run `docker run`, you can map ports with the `-p` option, _a.k.a._ `--publish`. This option is used as such: `-p HOST_PORT:CONTAINER_PORT`. It means to map the `CONTAINER_PORT` to the `HOST_PORT`. This is as simple as that.

So if want to map your PostgreSQL's `5432` container to port `5000` on your host, you will run a command like `docker run -p 5000:5432 <image-name>`. This is easy and you can repeat for as many ports as you want to expose! Like `docker run -p 8000:80 -p 5000:5432 <image-name>`.

Note that docker will execute that blindly: even if there is no application listening on the port you specified inside the container, it will still map them, the `-p` / `--publish` option lets your map ports, regarding of whether there is actually something using it.

Let's see the other possibility: automatic port mapping. As its name implies, it's a feature that makes Docker automatically binds ports. This sounds great, but let's take a second to think about what that means: how can Docker automatically bind ports? We have just seen that with the `-p` or `--publish` option, you can bind container ports to host's ports, even though no application is using the port inside the container.  
The problem is that Docker doesn't know what ports are used, so purely automatic port binding makes no sense: it's not like it can map **all** `65535` ports!

What 'automatic port mapping' means is that you specify docker the list of ports you want it to automatically map---these are container's ports---and it will automatically and randomly binds them to an available port on your host. Typically, you provide the list of ports that you containerized application uses.

_This time_ it can happen in the Dockerfile---at least one part, why? That's easy to understand: when you create a Dockerfile to create an image, it's because you want to dockerize an application. It's always the same, remember: the image is like a mold. So if your application uses ports `5432` and `5000`, then you know it. So you can specify them in the Dockerfile, it basically tells docker "here is the list of ports that I'm interested in".

The way you specify Docker the ports is through the `EXPOSE` statement

Now this is just _one part_: we've just told Docker what ports we are interested in. There is a _second part_, in which we instruct docker "yes please, bind the interesting ports at random". This part is done at run time. Instead of using the `-p`or `--publish` option, we use `-P`, this is capital `P` or `--publish-all`.

Let's do our tests so that we can fully familiarize ourselves with the behavior. We will be using the most simplistic Dockerfile we can have, because we are only interesting in the port mapping for now.

First, we start with no options, no ports:

```Dockerfile
FROM ubuntu:16.04
```

Then we build, as usual: `docker build -t port-mapping:vanilla` and we create a container from it: `docker run -dt --name test port-mapping:vanilla bash`.

We check the container with `docker ps -l`, nothing unusual:

```Bash
CONTAINER ID        IMAGE                  COMMAND             CREATED              STATUS              PORTS               NAMES
58ece1ab5a6a        port-mapping:vanilla   "bash"              About a minute ago   Up About a minute                       test
```

You can delete `docker stop test && docker rm test`. Using the same image, we will now try to manually bind a port, let's say we want to bind the container's port `80` to our host's port `7777`: `docker run -dt --name test -p 7777:80 port-mapping:vanilla bash`

Let's see if something changed: `docker ps -l` gives:

```Bash
CONTAINER ID        IMAGE                  COMMAND             CREATED             STATUS              PORTS                  NAMES
b31294cd50a5        port-mapping:vanilla   "bash"              4 seconds ago       Up 3 seconds        0.0.0.0:7777->80/tcp   test
```

So now there is something new: a new column, `PORTS` appeared, which shows the port mapping. We can see that the host's `7777` and the container's `80` ports are now bound. As we saw before, it works even though we don't really use the port.  
As you can see, by default, when we just specify the host's port, it binds on interface `0.0.0.0`, which means "all interfaces". In case your host has several interface / ip addresses, you can specify one. This is useful for instance when you containerize databases, and you want to map a port to your host to be able to query it. But since this is critical information, you don't want to allow outside access, so you can bind only on localhost, or `127.0.0.1`. This is done like this (don't forget to stop and delete your previous `test` container if you want to try the example): `docker run -dt --name test -p 127.0.0.1:7777:80 port-mapping:vanilla bash`.

Then, `docker ps -l` gives:

```Bash
CONTAINER ID        IMAGE                  COMMAND             CREATED             STATUS              PORTS                    NAMES
9dafaf2cbe28        port-mapping:vanilla   "bash"              2 seconds ago       Up 1 seconds        127.0.0.1:7777->80/tcp   test
```

This time, the `7777` port is opened only on interface `127.0.0.1`, you can verify with `nmap`, if your computer ip address is `192.168.1.99` on your network, try `nmap 192.168.1.99` and you should not see port `7777` as opened. Then try `nmap 127.0.0.1` and you should see it opened.

Stop and delete your `test` container.

We can confirm that automatic port mapping is useless because we did not `EXPOSE`d any port: `docker run -dt --name test -P port-mapping:vanilla bash` and `docker ps -l`:

```Bash
CONTAINER ID        IMAGE                  COMMAND             CREATED             STATUS              PORTS               NAMES
7f2acc5db090        port-mapping:vanilla   "bash"              6 seconds ago       Up 5 seconds                            test
```

The `PORTS` column does appear, but it's empty. `docker stop test && docker rm test`.

Now let's expose a port in our Dockerfile:

```Dockerfile
FROM          ubuntu:16.04
EXPOSE        80
```

We build it as usual: `docker build -t port-mapping:exposed .`

What happens if, even after using `EXPOSE` we do not specify any `-p` or `-P` option? Let's try it: `docker run -dt --name test port-mapping:exposed bash` and let's see with `docker ps -l`:

```Bash
CONTAINER ID        IMAGE                  COMMAND             CREATED             STATUS              PORTS               NAMES
6cf84a926376        port-mapping:exposed   "bash"              3 seconds ago       Up 2 seconds        80/tcp              test
```

Okay this is interesting, even though we did not specify any binding option, we still have the `PORT` column, and it displays the port we have exposed. This is useful because when you use other people's image, it indicates what ports are used by the application. Note that at this point, this is merely an indication: no port mapping was performed. Stop and delete `test`.

Note that I won't show you the case when we use `-p`to manually bind a port: this is completely analogous to the previous case, instead let's jump directly to what happens if we use the `-P` option: ` docker run -dt --name test -P port-mapping:exposed bash`.

This time, `docker ps -l` gives:

```Bash
CONTAINER ID        IMAGE                  COMMAND             CREATED             STATUS              PORTS                   NAMES
aa5a2d80618b        port-mapping:exposed   "bash"              29 seconds ago      Up 28 seconds       0.0.0.0:32769->80/tcp   test
```

As you can see, docker found an available port and used it to bind with the container's 80 port. When you use the automatic port mapping, Docker binds the ports to the `0.0.0.0` interface, and start using port around `327xx`. These are legal, free-to-use Linux ports.

So now we have seen all use cases for `EXPOSE`, and as you see, this is very flexible: you can `EXPOSE` ports or you can omit them, and whether you did or not, you can manually bind ports. If you want to use automatic port mapping, you need, however, to have some `EXPOSE`d ports.

So what's the definitive answer for `EXPOSE` or not `EXPOSE`?  
Bottom-line is: if you write a dockerized application, and if your application uses ports, you should `EXPOSE` them. For several reasons:

- for users that review your Dockerfile, it helps them immediately identify the useful ports
- for users that use your image, it allows them to see the useful ports---by creating a container from your image and inspecting the `PORTS` column, and it allows them to use automatic port mapping for test and debugging purposes.

When you are the user of an image, using automatic port mapping is fine for testing and debugging, but you should generally avoid it in production.  
Why?  
First, because it binds on the public interface `0.0.0.0` and it's easy to forget and expose sensitive data to the outside, and second because it binds at random ports, so another application that needs to communicate with it won't be able to easily know what port is opened.

That being said, it's not a big deal: `EXPOSE` and `-p` or `-P` is only meant to allow the **host** and the container to talk to each other. This should be pretty rare. All of time doesn't cover communication **between containers**. This is another topic entirely---namely: Docker Networks.

So really, I can see **3** legitimate use cases to make a container and the host communicate---of course you can find others, I'm just describing the most common:

- you have containerized an application as a service or your host, for instance, you dockerized a database instead of installing a local database, same thing with a webserver. In these cases, it's important to `EXPOSE` their ports and bind them---usually manually, because you need to have a reliable access to your database/webserver; in these precise cases, you can even bind the ports to their equivalent: `-p 80:80` or `-p 5432:5432` so that it's transparent!
- you have dockerized an application and are trying to debug / test it. For instance you have dockerized databases, that will, later, be part of a more complex stack---inside Docker Network---but for now, you need access from the host to query the database, instead of "logging in" the container and query from there every time. This is a temporary solution. Usually, you're fine with automatic port mapping here.
- you have dockerized an application to use in production---again, take the example of your website's database---it is on your remote server. But you need to make regular (automated or not) backups. In this case, rather than "logging in" the container and make the backup from there, you can expose the port, but you will only expose the port on a controled, private interface, either `127.0.0.1` if you want to be able to make backups only from the host itself, or you can bind it to an interface that is part of a private, encrypted VPN, for instance `10.0.0.2`. This is a valid use case as well.

### ENTRYPOINT or CMD? The Definitive Answer
This one is perhaps the most important. Up until here, our Dockerfiles were very simple---albeit tailor-made to demonstrate our examples. All they did, until here, was define an environment. This is one goal of Docker, but another goal is to dockerize an application, meaning that we need a way to start an application in the image.

This is where the `ENTRYPOINT` and `CMD` Dockerfile statements come in handy. They are powerful, but as every powerful tool, you must be careful in how you use them.

So, similar to `COPY` and `ADD`, it seems we have two ways to achieve the same thing: run a program. But they are not equivalent, and I'll explain this so that you never mix them again.

Remember how we always specified a command to run inside the container up until now, most of the time it was `bash`, like this: `docker run ubuntu:16.04 bash`. It means to run the command `bash` inside a container created from the `ubuntu:16.04` image. When we're done with this section, we will be able to simply create a container from an image, and it will run it's dockerized application all alone, like a big boy: `docker run -d myCustomDatabase`. The `-d` or `--detach` option is meant so that the container is run in the background and you have your console back.

As I have repeated several times, the two main use cases of docker are creating a controlled environment and running isolated applications. Well, `CMD` and `ENTRYPOINT` were more or less made for these two goals. Let me explain myself.

![](/images/warning.png "") I will talk about these two Docker goals to explain `CMD` and `ENTRYPOINT`. In order to do this, I will, obviously, simplify a lot the use cases. So don't take it _as-is_ and think that these are the only two use cases of Docker. My goal is to make you _feel_ the difference between the two statements, once you've got it, you will figure out which one you need.

Before I begin to explain the use cases, let me give the three possibilities---because there are three--- with a blunt sentence for each, which you should not understand, but which I would like you to keep in mind when you read the explanations, it will all become clear at the end, so:

- `CMD` is easily overridden by the user
- `ENTRYPOINT` is "hard" to override for the user
- using `CMD` **and** `ENTRYPOINT` means you probably get it and are doing this the right way

Don't expect to understand what these means yet, I just want you to keep this in your head, not too far, and we will understand it.

#### The Controlled Environment
So one goal of Docker is to be able to have a controlled, reproducible environment for our softwares. There might be lots of use-cases for this: suppose you have a developed a set of softwares that need a common environment (something specific for your company, that needs custom repository and/or environment variables, etc.).  
Once your have developed these several softwares, you might want to make an image for each of them, but in this case, the images---the environment---end up being the same, and only the installed software changes. Which is fine in itself---thanks to Docker images being made of reusable layers---but it might be tiresome.

What you might want to do in this case is use the one image you made with your environment and simply run the several programs inside it, one at a time, but without having the trouble of creating a new image for it and instantiating a container from it every time.

This is what we're familiar with, actually. Images like the ubuntu image are built this way. This is the use of `CMD`.  
So what does `CMD` do anyway?

Generally, `CMD` is the last Docker statement in your Dockerfile, and what this does is simple: it runs the command you pass it as an argument when you instantiate a container from it. Remember how "a container is just a fancy way of running a process"? Well the _process_ it runs is specified by the `CMD` statement.

So it's simple: if the command/process you specified to `CMD` terminates (returns), then your container will exit, because in a sense, the container _is_ the process. If your command/process is foreground and hangs up---like a server for instance---then your container will stay alive.

Both cases are fine.

Consider this Dockerfile:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

ENV         FOO "bar"

CMD         /bin/bash -c "echo 'FOO is: ${FOO}'"
```

It's very simple, it defines an environment variable named `FOO` whose value is "bar". Then the `CMD` statement instructs to run `/bin/bash` (so the command runs a shell), with the `-c` option, which is used to run a command that we specify after. And in this case, the command is `echo` and we print the value of the `FOO` environment variable.

Simply build this image: `docker build -t cmd-statement:terminates` and run it: `docker run --rm cmd-statement:terminates` ---note that there isn't `bash` appended at the end!---and you should see `FOO is: bar`.

In this example, `bash` terminates, so does your container. If you had run a non-terminating command, it would have stayed alive.

Now the cool thing about `CMD` is that it is easily overridden, which means that once your image is built, it is very easy for the user to run _another_ command that the one specified in `CMD`. For instance, we can run `bash` alone---_i.e._ without the `-c 'echo ...'` thing.  
Actually, this is what we have been doing all along, you specify the command you want to run as the last parameter to `docker run`, as such: `docker run -it --rm cmd-statement:terminates bash`. This effectively runs the (vanilla) `bash` command: you should be presented with a prompt:

```Bash
root@5080b17d8305:/#
```

So we have _everything_ the image (Dockerfile) is setting up, _expect_ the `CMD` is not run. For instance, in our case, we have the `FOO` environment variable:

```Bash
root@5080b17d8305:/# env | grep FOO
FOO=bar
```

We can run the command `ls` inside this image, for instance: `docker run -it --rm cmd-statement:terminates ls` and you should get:

```Bash
bin   dev  home  lib64	mnt  proc  run	 srv  tmp  var
boot  etc  lib	 media	opt  root  sbin  sys  usr
```

So this is actually cool. Why? In which use-case is this used?

This is used when you want to provide the user with a set of tools (software, programs, commands) that you want the user to be able to run _almost_ as if the command was installed in the user's host system.  
Then, rather that running `customProgram1`, `customProgram2` as if they were installed directly, you can provide him with the Dockerfile, and once the image is built (or better: put in the Docker Hub), the user can run it as `docker run imageName customProgram`, `docker run imageName customProgram2`.

Both of these use the same environment (image), but you can chose the command to run.

So that's about the gist of `CMD` used alone: it provides a default command to run if you don't specify one on the `docker run` command-line, but if you do provide one,  this one gets used instead. Easy.


#### The Dockerized Application
The second Docker goal we've talked about is about Dockerizing applications, meaning you want to provide an application isolated from the host. But usually this application is a "long-running" application. A server, a database, etc.  
In this use-case, you created a Dockerfile that set up an environment that is tailored to your application, and you don't want the user to change the application, because it would not make sense.

In this case, you use `ENTRYPOINT`. It works the same as `CMD`: whatever process/command your specify to `ENTRYPOINT` will be run as the, err... entrypoint of the image.

Let's consider this small script, that acts as a "server" (I just named it to emulate when you run a real server application, the important point here is that the "server" doesn't return):

```Bash
#!/bin/bash

line=''
nb=0

while [ "${line}" != "exit" ]; do
	read -p "Enter text ('exit' to quit): " line

	if [ "${line}" == "exit" ]; then
		printf "Good, bye! (${nb} runs)\n"
	else
		((nb++))
		printf "You entered: '${line}'\n"
	fi
done
```

All it does is continuously ask you for input until you write 'exit', at this point it tells you good-bye and tells you the number of times you wrote. It's not a revolution but it does the job.

Now, the Dockerfile:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

COPY        server.sh /server.sh

ENTRYPOINT  /server.sh
```

What it does is copy the script file to the root of the fs inside the image, and execute it. **Don't forget to make the file executable with `chmod +x server.sh`**.

Let's build it `docker build -t entrypoint-statement .` and run it: `docker -it --rm run entrypoint-statement`:

```Bash
Enter text ('exit' to quit): Hello
You entered: 'Hello'
Enter text ('exit' to quit): World!
You entered: 'World!'
Enter text ('exit' to quit): exit
Good, bye! (2 runs)
```

The important thing to note here is that we simply instantiated the container, without passing a command argument: `docker run -it --rm entrypoint-statement`. Since there was an `ENTRYPOINT`statement inside the Dockerfile, it was executed. Now since `ENTRYPOINT` is supposed not to be overriden, it is purposedly "hard". Simply passing a parameter on the command-line won't work. Try: `docker run -it --rm entrypoint-statement bash` as we usually do, and you should get the same:

```Bash
Enter text ('exit' to quit):
```

So it apparently did not do anything! So when you write a Dockerfile and chose to use `ENTRYPOINT` rather than `CMD`, it sends the message "this image is tailor-made for this software, don't do something fancy with it". If you _still_ want to override the entrypoint, you can do it with the `--entrypoint` flag: `docker run -it --rm --entrypoint "bash" entrypoint-statement` and now you get your shell.

This is verbose and long and there is no equivalent short option. This is done on purpose, to discourage people from overriding `ENTRYPOINT`s.

But what if you want to pass some parameters to your long-running softwares? How would you got about that?

There's the "bad" way: environment variable. `docker run` has the `-e` or `--env` parameter that you can use to define environment variables inside the container you are instantiating. Then you can have your server/application look for these environment variables.  
This works. But I don't like it, it's not the role of environment variables. Environment variables are "global" for a reason: they are shared for the entire environment. It doesn't make sense to make something global if it's specific to one application. It might even be dangerous in some cases.

No I prefer to pass proper, real parameters, as you'd do on the command line 'command param1 param2'.

When you want to do that, you use the third configuration: `ENTRYPOINT` **and** `CMD`.

#### The Smart Dockerized Application
So the smart way is to run both `ENTRYPOINT` and `CMD`. This is quite easy to understand: when you use both, the command/process being run is the one specified in `ENTRYPOINT`, and cannot be overridden (or yes it can, with the `--entrypoint` flag). What's inside the `CMD` statement will be the default parameters.  
If your user simply instantiates the image, they will be used and passed to the process specified in `ENTRYPOINT`, but if the user specifies something at the end of `docker run`---at the place we usually put the command we want to run---then _that_ will be used instead (as parameter for the command specified in `ENTRYPOINT`).

Let's take a concrete example, remember our "server" application? What if we want to be able to chose the word used to exit? We want to pass it as a parameter.

Here is the changed script:

```Bash
#!/bin/bash

line=''
exitSequence="exit"
nb=0

# Check if we have a parameter to replace the exit sequence
if [ -n "$1" ]; then
	exitSequence="${1}"
fi

while [ "${line}" != "${exitSequence}" ]; do
	read -p "Enter text ('${exitSequence}' to quit): " line

	if [ "${line}" == "${exitSequence}" ]; then
		printf "Good, bye! (${nb} runs)\n"
	else
		((nb++))
		printf "You entered: '${line}'\n"
	fi
done
```

I'm not here to give a shell programming course, but basically, we now store the exit sequence---previously "exit"---in a variable, `exitSequence`, which we give "exit" as default value. Then we check if the `$1` variable is set (it's the first argument on the command-line); if it is, it replaces the default content for `exitSequence`.  
Then, the loop checks user input against `exitContent` rather than the hard-coded "exit" string. That's all.

You can test it, first without parameter (same behavior as before):

```Bash
$> ./server.sh
Enter text ('exit' to quit): test
You entered: 'test'
Enter text ('exit' to quit): test2
You entered: 'test2'
Enter text ('exit' to quit): exit
Good, bye! (2 runs)
```

So as before, since I did not specify any parameter, it's "exit" that breaks the loop.

But now we can pass it an argument and it will take this instead:

```Bash
$> ./server.sh stop
Enter text ('stop' to quit): test
You entered: 'test'
Enter text ('stop' to quit): test2
You entered: 'test2'
Enter text ('stop' to quit): exit
You entered: 'exit'
Enter text ('stop' to quit): stop
Good, bye! (3 runs)
```

Okay so now we see that "exit" now doesn't do anything, but "stop" does. Easy.

All of this to say: we now have a "server"---simply a program that runs without exiting right away---which can take parameters.

Suppose it's an important server program whose environment---the Docker image---is carefully designed for its particular use, so we use `ENTRYPOINT` to run our server. This is good because now the user "can't" (easily) modify it, but in the current state of things, we can't have the user pass an argument. So what do we do?

This is where we will use both `CMD` and `ENTRYPOINT`. Actually, it's more exact to say that we will use both `ENTRYPOINT` and `CMD`.  
This is very easy and straightforward: when we use both of them, what's inside the `ENTRYPOINT` statement behaves the same, _i.e._ it is the process/command to execute when a container is instantiated from the image, and it cannot be overridden easily, and what's inside the `CMD` acts as **default parameters** for the command inside `ENTRYPOINT`.

What this means is that if the user simply runs the container with `docker run -d <image-name>`, the command inside `ENTRYPOINT` will be executed, with whatever is written inside `CMD` passed as argument ; and if the user wants to pass arguments, he can do so by appending what he wants to pass at the end of the `docker run` command, like `docker run -d <image-name> param1 param2`.

Easy, right?!

Actually it's very natural and easy: passing `--entrypoint <whatever>` overrides the content of the `ENTRYPOINT` statement, and passing additional arguments at the end of the `docker run` command overrides the `CMD` command. It's always like this.  
So when only `CMD` is used inside the Dockerfile (first case we saw), then the user can easily override the `CMD` statement by appending arguments.  
When only `ENTRYPOINT` is used, the user must user `--entrypoint` to override, as we saw in our second case. If you recall, we even tried to override it by passing additional arguments at the end of the command line, but it did not seem to do anything. Actually it did! It passed those arguments as parameters for the command inside `ENTRYPOINT`! But since we did not care about them, it did nothing.

In our third case, however, we designed our "server" to care about arguments it is being passed, so now we can try it, let's do this now!

Here's the new Dockerfile:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

COPY        server.sh /server.sh

ENTRYPOINT  /server.sh
CMD         "exit"
```

Let's build it with `docker build -t entrypoint-and-cmd:1 .` and run it:


```Bash
$> docker run -it --rm entrypoint-and-cmd:1 "exit"
Enter text ('exit' to quit): test1
You entered: 'test1'
Enter text ('exit' to quit): exit
Good, bye! (1 runs)
```

So as you can see, "exit" exits the server. Let's try with another keyword:

```Bash
$> docker run -it --rm entrypoint-and-cmd:1 "stop"
Enter text ('exit' to quit): test1
You entered: 'test1'
Enter text ('exit' to quit): stop
You entered: 'stop'
Enter text ('exit' to quit): exit
Good, bye! (1 runs)
```

Uh oh. It seems we have a problem... not only the "('exit' to quit)" did not change, which seemed weird, but we confirmed that "stop" doesn't work and it's still the "exit" keyword that works. What's this? Have I been lying to you since the beginning?

No I haven't, I just had another thing to teach, but you would not have caught the importance of it unless it smacked you in the face, as it just did. So as you see, things behave strangely, and it could take a looooong time to find out why. Lucky you, I'm going to tell you, but this is a very _very_ important point, so pay attention, understand it, and never make that mistake again!  
Let's talk about `exec` vs. `shell` form.

##### Exec vs. Shell Form

Have you ever wondered **how** the commands were executed in your containers? I'm talking about the command / processes you specify as a starting point with either `ENTRYPOINT` or `CMD`? I mean, they are **run**, so there must be _something_ to run them, right?

What is it? Can we control it?

Logically enough, what runs your commands inside the containers is a shell. But what's more interesting is that there are two possibilities for that: the exec and the shell form.

Pay attention here, because this is one of those points in Docker that newcomers always get wrong, and confused about. But the truth is that it's actually quite simple. Let's see the differences:

- the **shell** form is the form we have used until here, it appears like this in our Dockerfiles: `CMD /server.sh` for instance, and more generally, `CMD command arg1 arg2 arg2`---it works the same with `ENTRYPOINT`.  
As we have seen before, this works and our command gets run, but there are some things to know. To understand that, let's consider our first example of the "server", the one with this Dockerfile:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

COPY        server.sh /server.sh

ENTRYPOINT  /server.sh
```
Let's run it in a terminal: `docker run --rm -it entrypoint-statement` which gets us: `Enter text ('exit' to quit): `. So now our `server.sh` program is running.  
Don't exit the server, and open another terminal, we are going to explore the list of running processes inside our container. We could "log in" our container from another terminal (you know by running `bash`), but we are going to use a new docker command instead: `docker top`. This is basically like running the `top` command to list running processes, only you don't need to "log in" a container before, simply run `docker top <container-name>` and it will run it for you.

After you used `docker ps -l` to get the name of your container, run the `docker top` command:

```Bash
docker top naughty_goldwasser

UID                 PID                 PPID                C                   STIME               TTY                 TIME                CMD
root                6694                5199                0                   08:37               pts/5               00:00:00            /bin/sh -c /server.sh
root                6703                6694                0                   08:37               pts/5               00:00:00            /bin/bash /server.sh
```

So there are two lines, two processes running. And this is where you should pay attention, let's look at the first one: `/bin/sh -c /server.sh`. **Here** lies all the difference between the shell vs. exec form. When running in **shell** form like we did: `ENTRYPOINT ./server.sh`, in order to run the process we've asked `/server.sh`, Docker uses `sh`. `sh`is a very simple shell, that dates back to the beginning on the Linux / Unix development and is usually used to run scripts. `sh` has a parameter `-c` which accepts a command and it will run the command passed to it.

This seems cool---and it is!---but there is one caveat: by doing this, the program **actually** running inside our container **is** `/bin/sh` and **not** `/server.sh`. To be clear, the program `/bin/sh` is running and it happens to make `/server.sh` run. But this has practical implications---lots of them, and some pretty serious to be honest, but this is the topic for another post!  
One of them is that the program with `PID` 1 inside the container is `/bin/sh`. You need to "log in" the container to actually see, this, you can do it, here is the output from running `top` from inside the container:

```Bash
  PID USER      PR  NI    VIRT    RES    SHR S  %CPU %MEM     TIME+ COMMAND                                                                                                                  
    1 root      20   0    4508    616    540 S   0.0  0.0   0:00.02 sh                                                                                                                       
    6 root      20   0   18032   2676   2456 S   0.0  0.0   0:00.00 server.sh                                                                                                                
    7 root      20   0   18240   3180   2740 S   0.0  0.0   0:00.02 bash                                                                                                                     
   15 root      20   0   36676   2996   2532 R   0.0  0.0   0:00.00 top
```

So the `PID` 1 program is `sh`. And this is a problem why exactly?

This is a problem because this is the `PID` 1 program that receives Unix signals (like `SIGTERM`, `SIGINT`, `SIGUSR1` etc.). So when you run `docker stop <container>` for instance, or if you have another program sends signals to this container, `sh` will receive it, not **/server.sh**. And this is even a bigger problem because it turns out that `sh` _won't forward Unix signals_ to the program it runs with its `-c` command.

Basically, that means that you cannot properly exit your programs when running in shell mode, for instance if you need your program to clean resources, or send a message before it actually exits, you can't do that.

There is _another_ interesting point---I told you there were several!---in running `ENTRYPOINT` in shell mode: _it doesn't allow you to override parameters with CMD_!

This is **precisely** what we were trying to do when it blew in our faces! So I had to make this introduction to shell vs. exec mode. Basically, if the vast majority of cases, you do **not** need or want to run your `CMD`s or `ENTRYPOINT`s in shell mode, always use exec mode. If you _need_ to use shell mode---sometimes it's useful---it's because you're doing advanced things and by this time, you'll be familiar enough with Docker that you can figure it out.

Let's see how to run things in exec mode!

- **exec** mode is the way we will write our Dockerfiles now, and this is what you should do to avoid headaches. I feel that you are beginning to be nervous and are thinking about insulting me: don't worry, there is _very few_ to change to switch from shell mode to exec mode. Observe:

- shell mode: `CMD command arg1 arg2`
- exec mode: `CMD ["command", "arg1", "arg2"]`

And voilà! More seriously, exec mode is used with brackets `[` and `]` surrounding the parameters of either `CMD` or `ENTRYPOINT`. And use double quotes because this is parsed as a JSON array---seriously, don't forget the quotes, they are needed. That's all!

Let's take back our simple Dockerfile from before:
```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

COPY        server.sh /server.sh

ENTRYPOINT  ["/server.sh"]
```

Let's build it: `docker build -t entrypoint-statement:exec .` and run it: `docker run --rm -it entrypoint-statement:exec` and you should get the usual `Enter text ('exit' to quit): `.

As before, don't leave this terminal, and in another, after running `docker ps -l` to get the container's name, run `docker top <container-name>`.
And this time, the output should be something like:

```Bash
UID                 PID                 PPID                C                   STIME               TTY                 TIME                CMD
root                9054                5199                0                   18:43               pts/5               00:00:00            /bin/bash /server.sh
```

This is _fundamentally_ different, here the `PID` 1 program is our server (for those who wonder why it's `/bin/bash /server.sh` it's because of the shebang we put at the beginning of our script `#!/bin/bash` which makes bash run our script).
The difference here is that our program is not a subprogram of `sh`, so it will receive the UNIX signals and such.

And the other big difference with the exec form is that in exec form, `ENTRYPOINT` **does** allow us to override its default parameters---those given with `CMD`---on the `docker run` command-line!

So from now, unless you **explicitly** _need_ to use the shell form---and if you don't know if you need it, it means you probably don't---I'd like you to always use the exec form.

What do you say we go back to our `ENTRYPOINT` **and** `CMD` thing, showing that I have not been lying to you from day 1?

#### The Smart Dockerized Application - Take 2

Remember our old Dockerfile? Let's change it to exec form now:

```Dockerfile

FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

COPY        server.sh /server.sh

ENTRYPOINT  ["/server.sh"]
CMD         ["exit"]
```

The slight modification is simply to add brackets and double quotes, now we can build it `docker build -t entrypoint-and-cmd:2 .` and let's run it without any overriding command:

```Bash
docker run --rm -it entrypoint-and-cmd:2

Enter text ('exit' to quit): test
You entered: 'test'
Enter text ('exit' to quit): exit
Good, bye! (1 runs)
```

Until here, no surprises. As a small reminder, "/server.sh"---our "server"---is run because it's the value in the `ENTRYPOINT` statement in the Dockerfile. Since we did not specify anything at the end of the `docker run` command, the default parameter `exit`, written in `CMD` was passed to the command.

Now let's run it with overriding the default `CMD` value:

```Bash
docker run --rm -it entrypoint-and-cmd:2 stop

Enter text ('stop' to quit): test
You entered: 'test'
Enter text ('stop' to quit): quit
You entered: 'quit'
Enter text ('stop' to quit): stop
Good, bye! (2 runs)
```

And tadaa! It worked as expected! Phew! So here, when we added "stop" at the end of the `docker run` command, it replaces the value in `CMD` and became the exit value. Simple.

As a side note, you might have noticed we use the same default value in both our `server.sh` and the `CMD` value. It's pretty useless: since we already defined a default value in our server, there is no need to provide a default `CMD` value. So it still works if you completely remove the `CMD` statement from the Dockerfile.  
When you add parameters after the `docker run` command, it **will** pass them to the command in `ENTRYPOINT`, even if there are no `CMD` in your Dockerfile.

That being said, I'd recommend against that because if a user reads your Dockerfile and doesn't see a `CMD` but only an `ENTRYPOINT`, it might not be obvious what parameters he can override. So I suggest that you always provide a full `CMD`, with all default parameters, so that your user can easily know what parameters are tunable.

So the TL;DR of this part is (now's the time to recall the three blunt sentences I said, and you should be able to understand them):

- `CMD` is to provide a default command to run when you instantiate the container, but as you can see it is very easy to override. So use it when you _intend_ the user to modify the executed process, maybe because your Dockerfile is a custom environment that allows several commands to run inside it
- `ENTRYPOINT` works the same as `CMD`---it provides a default command to run when instantiating the container---but it makes it a pain to override---one must use `--entrypoint <command>`. So use it when your Dockerfile is built around and a for a specific application, it's called Dockerizing an application.  
Keep in mind that it's strict, so prefer the last solution.
- `ENTRYPOINT` and `CMD` means you got it all! You provide your user with a process to run at instantiation, but you provide default, overridable arguments in `CMD`, that the user knows he can override.

That's easy right? Keep that in mind when designing images!

It's time to take a coffee break, because we've seen a lot, give it time to sink in.

![](/images/coffee_break.png "Let it sink it, will you?")

### Passing Build Arguments

Here we are beginning to reach pretty advanced Dockerfile features. Up until now we have written straight Dockerfiles, that were read from top to bottom, executing each statement in a deterministic manner and in the end, we ended up with our image. Perfect.

What we are interested in now is the ability to provide _build-time_ arguments to our docker daemon. Which means that a part of a Dockerfile statement can depend on some values we give in the command-line.

![](/images/warning.png "") Let's take a moment to understand our new power. Remember that one of the goals of Docker is uniformity and reusability. Meaning you write a Dockerfile, describing an image and when you or your client builds it, it works the same. This is the reason why the _build context_ is passed entirely to the docker daemon when running `docker build`, and why you can't refer to files in parent directories.  
Here we are going to introduce a way to slightly change that behavior, with built-time parameters, pay attention, and as usual, make sure you _really_ understand what we are doing, because here starts the real potential to mess things up---as in lose the benefits Docker brings to the table.

Okay, so let's suppose you have developed a software and are building and image for it. You are still in experimenting stage and your software behaves slightly differently based on a global variable, say `VERSION` which can be "development" or "production"---one can make the software more verbose and logs errors on "development" for instance.  
Aside from that, your software is the same, so it doesn't _really_ make sense to create **two** images for that.  
I'm not saying that it's what you should always do, I'm just talking about this particular case.

One solution---that we have just discarded---is to create two Dockerfiles, with just this statement changing: `ENV VERSION development` or `ENV VERSION production`. As a reminder---but I'm sure you don't need this by now!---the `ENV`statement defines an environment variable in the image, its first parameter is the name of this environment variable and the second argument is its value.  
So what we've just done is possible, let's see how we can do it with only one Dockerfile, and build-time argument.

There are two sides to build-time arguments, ~~the dark side and the white side~~ there is _passing the build-time argument(s)_ and _getting the build-time argument(s)'s value_.

#### Passing Build-Time Arguments to Docker Build
As their name implies, "build-time" arguments are passed when we build the image, _not_ when we instantiate the container from the already-built image---this should be ab-so-lute-ly clear!

This part is very easy, to pass a build-time argument, we need to use the `--build-arg` flag, which takes the form `argName=value`. To re-use our previous example, we would do something like this: `--build-arg version=development` or `--build-arg version=production`. See? Easy!

When you have several arguments to pass, you simply the `--build-arg name=value` several times.

This is all very simple from this side---don't worry, the other side is cool too---but there is a (small) list of gotchas, let's see them, so they don't bite your in the butt:

- Docker is quite picky on its build arguments: if you specify an argument to `docker build` but don't use it in your Dockerfile, it will abord the build with a message "One or more build-args [test] were not consumed, failing build.". Here it gently but firmly tells me that the argument `test` was provided to the command-line, but not used in the Dockerfile, so it abords the build. This is to prevent unwanted, silent behavior.
- Names of build arguments cannot contain dashes (-), so you can make them `camelCase`, `CamelCase` or `with_underscores`, but not `with-dashes`. Argument names `myArg`, `MyArg`, `myarg` and `my_arg` are valid, while `my-arg` or `3myArg` are not (names can't begin with numbers either). This is pretty much what you'd expect, but still, you have it in writing.
- If you want to pass strings or arguments with spaces, make sure you enclose them in quotes (either double or single), otherwise the command will fail as syntax / parsing error. Thus you don't write `--build-arg welcome_msg=Hello Guys!`, but more like `--build-arg welcome_msg="Hello Guys!"`. Again, pretty obvious, but still worth being sure.
- Lastly, another obvious thing, variables names are case-sensitive, so `--build-arg myArg=value` is different that `--build-arg MyArg=value`. Make sure you respect the case.

That's about it to _pass_ build-time arguments, let's see now how to _get and use_ them.

#### Getting and Using Build-Time Arguments in Dockerfiles
This part is _slightly_ more complicated, but really, this is just because Docker allows some flexibility, this is nothing too hard.  
Rather than making a long text, let me write in bullet points, so it's quick to read and you don't mix anything.

- The statement to **get** the value of a build-time argument is `ARG`, that one's easy!
- The argument exists and has a value in the Dockerfile **only from the line it is fetched**, which means that if you use the `ARG` statement to get the value of the build-time argument in line 23 of your Dockerfile, the 22 previous lines won't know about it, and all the following lines will. This is important.  
- You can---and personally I think you should almost always, unless in specific case---give a default value to a build-time argument ; which means that passing build-time arguments is not mandatory---**but** _using_ a build-time argument that is passed to `docker build` **is** mandatory, make sure you get the difference.
- The syntax to **use** a build-time argument---after you **fetched** it---is `${argName}`. You must respect the case and you must enclose it in brackets (`{}`) with a dollar sign in front (`$`). This is mandatory.

So it's really all there is to it, now we will discuss some of the bullet points above to give some more information, and we will see examples.

In the first point we said that `ARG` was the statement to get build argument, it's really all there is to it. You write `ARG myArg` in your Dockerfile, and from this line on, the variable `myArg` is available to all subsequent statements. To use it, we saw in the last point that is was `${myArg}`.

Let's make a test right now:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

ARG         myArg
RUN         echo ${myArg}
```

Build it with `docker build -t test-arg:simple --build-arg myArg="Hello, world!" .` an you should see:

```Bash
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Running in 63044eb0d993
 ---> 952a8096b875
Removing intermediate container 63044eb0d993
Step 3 : ARG myArg
 ---> Running in ba350080ca9e
 ---> e0509d4a087b
Removing intermediate container ba350080ca9e
Step 4 : RUN echo ${myArg}
 ---> Running in da669ef4d138
Hello, world # <------------------------------------ this is the important line
 ---> f1679af5a3a4
Removing intermediate container da669ef4d138
Successfully built f1679af5a3a4
```

Right after step 4 you can see the "Hello, world" being echoed. Easy!

What would happen if we _didn't_ pass a build-time argument---which is valid, I remind you, this is the opposite which is not---? Easiest way to find out is to try: `docker build -t test-arg:simple .`

```Bash
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Running in a9aa11908ef1
 ---> bebbb4c44ef6
Removing intermediate container a9aa11908ef1
Step 3 : ARG myArg
 ---> Running in b0998c06b734
 ---> 35fb779fff86
Removing intermediate container b0998c06b734
Step 4 : RUN echo ${myArg}
 ---> Running in 09875a925e95
                            # <------------------------------------ this is the important line
 ---> 2ae039632c4b
Removing intermediate container 09875a925e95
Successfully built 2ae039632c4b
```

If you don't specify a build argument, its value will be null. This is why I'm suggesting to always---wherever possible---provide a sane, default value.

Before we see how to do that, let's verify that a build argument exists only after its value has been fetched.  
Let's slightly change the Dockerfile:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

RUN         echo "Before: ${myArg}"
ARG         myArg
RUN         echo "After: ${myArg}"
```

As you can see, we first try to display the content of `myArg` before fetching its value and then after. Let's see what it gives: `docker build -t test-arg:simple --no-cache --build-arg myArg="Hello, world" .`

```Bash
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Running in bb4b710494d3
 ---> 10b944bb0734
Removing intermediate container bb4b710494d3
Step 3 : RUN echo "Before: ${myArg}"
 ---> Running in d097d2eaf610
Before:   # <------------------------------------ this is important line #1
 ---> 15e24d5f64e4
Removing intermediate container d097d2eaf610
Step 4 : ARG myArg
 ---> Running in e83ba7b4479e
 ---> 415385176e0a
Removing intermediate container e83ba7b4479e
Step 5 : RUN echo "After: ${myArg}"
 ---> Running in 27df524b9bd8
After: Hello, world # <------------------------------------ this is important line #2
 ---> 2825774ae74a
Removing intermediate container 27df524b9bd8
Successfully built 2825774ae74a
```

So as previously stated, `myArg` just doesn't exist before we fetch it with the `ARG` statement. Period.  
If you followed well, you should now be thinking---and you'd be right---that there is a trade-off to make here: putting the `ARG` statement higher in the Dockerfile makes the build argument available to more of the file, **but** in the meantime, it means more of the image will have to be rebuilt the next time `docker build` is called with a different `--build-arg`, losing an important advantage of Docker: the re-usability of its layers.

So really, what you should do is put the `ARG` statements as low as possible, just before you _actually_ need it.

Ok, now, on to providing a default value for the `ARG` statement, so that we are not left hanging dry when the user doesn't provide build argument to `docker build`.

The syntax is really easy: you provide a default value with `=` and then the value. This gives something like this: `ARG myArg="default value"`.

![](/images/warning.png "") Now be careful, it _looks_ like a variable assignment, giving the value "default value" to the `myArg` variable, but it's not. It feels obvious now, but please keep this in mind, there will come a time when you haven't seen `ARG` statement in a Dockerfile for a long time, and when you see one, you will think it's a variable assignment. It's not. It is nothing less that giving a default value to a build argument.

Let's feel it:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

ARG         who="world"
RUN         echo "Hello, ${who}!"
```

Let's first run it normally, like we always did: `docker build -t test-arg:defaultValue --build-arg who=nschoe .`

```Bash
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Using cache
 ---> 10b944bb0734
Step 3 : ARG who="world"

 ---> Running in 53061b006f67
 ---> dd7ad1065c39
Removing intermediate container 53061b006f67
Step 4 : RUN echo "Hello, ${who}!"
 ---> Running in 8d149011920e
Hello, nschoe! # <------------------------------------ this is the important line
 ---> 0d2e4f1ecc87
Removing intermediate container 8d149011920e
Successfully built 0d2e4f1ecc87
```

As we _did_ provide a `--build-arg who=...`, the default value was completely ignored and `${who}` took the value we gave it. Now let's try it without `--build-arg`: `docker build -t test-arg:defaultValue .`

```Bash

Sending build context to Docker daemon 2.048 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Using cache
 ---> 10b944bb0734
Step 3 : ARG who="world"
 ---> Using cache
 ---> dd7ad1065c39
Step 4 : RUN echo "Hello, ${who}!"
 ---> Running in 0695d16836af
Hello, world! # <------------------------------------ this is the important line
 ---> da5da8dbde09
Removing intermediate container 0695d16836af
Successfully built da5da8dbde09
```

And voilà! As planned, since we did not specify a value for the build argument, it took the default. Nice.

##### Some Words of Advice
Personally, I try to have as little environment variables as possible, and especially not for configuration. This is just not the way they should be used for. With build-arguments you have a way of providing some value that can be quite secure: the value exists only in the Dockerfile, and you are in charge of passing and/or using the value only where it is needed in the code. So try to use this when you can.

As a quick tip---useful when you want to pass confidential information---you can prefix your command-line with a space so that it's not stored in your shell history. This is not a Docker feature, but a UNIX shell one.

Let's have a look, type `echo 'str1'`, then press `<Enter>`, then `<Space>echo 'str2'` (with a space before `echo`), again press `<Enter>` and finally `echo 'str3'` and `<Enter>` a last time.

Now check your history with `history`:

```Bash
$> history
[...]
100  echo 'str1'
101  echo 'str3'
```

As you see, the ` echo 'str2'` was not logged in the `history` because it was prefixed with a space. You can still see it if you go back with arrow up, but if you kill your terminal and start another, the command will be gone!  
So you can do this when you want to build an image, passing a private information to it.

![](/images/warning.png "") We have discovered a new power: passing build time arguments and I bet you want to play with it a lot. Well, be careful because there is potential to break the universality of Docker images, _i.e._ have an image build on your machine but not on your client's. I'm not saying it is always the case, but not used careful, it can.  
Remember how the docker daemon cannot "go back up" outside the build context? Here there is a possibility, you can pass the result of a command as a `--build-arg`, so you can do something like ``--build-arg var=`cat /absolute/path/to/file` `` and it _may_ be a problem.  
Anyway, be careful not to depend on your host's setup.

### A Few Gotchas

Now that we have seen a fair amount of examples and common practices for building images and writing Dockerfiles, let us take a small break in the form of a few gotchas worth mentioning. This part will be pretty small and is not revolutionary, but this is a collection of a few things that I have either learned by chance when reading something---_i.e._ not necessarily well documented---or because it bit me in the butt---_i.e._ not documented at all!

#### Changes to /etc/hosts Are Not Persistent
This one is by far the trickiest one and it took me quite some time to realize what was going on. At some point I was building images for the company I work for and these images were for us ; it was an image that I was to give to all of our developers, not ship to client.  
In our company network we have a couple of servers, one of which hosts an APT repository. So I added the `*.list` file under `/etc/apt/sources.list.d/` and since we usually all have an alias for the IP address of this server, I added in the `/etc/hosts` file.  
Let's examine what such a Dockerfile could look like:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

RUN         echo "Before:" && cat /etc/hosts
RUN         echo "8.8.8.8 google-dns" >> /etc/hosts
RUN         echo "After:" && cat /etc/hosts
```

This looks simple enough: we print the content of `/etc/hosts` before and after appending a line to it. Let's build it: `docker build -t test-hosts .` and see what we've got:

```Bash
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Running in 2e8eea8eb9e8
 ---> b456316f4ace
Removing intermediate container 2e8eea8eb9e8
Step 3 : RUN echo "Before:" && cat /etc/hosts
 ---> Running in 9dc426124633
Before: # <------------------------------------ Before
127.0.0.1	localhost
::1	localhost ip6-localhost ip6-loopback
fe00::0	ip6-localnet
ff00::0	ip6-mcastprefix
ff02::1	ip6-allnodes
ff02::2	ip6-allrouters
172.17.0.2	827f45722fd6
 ---> 114121a6464b
Removing intermediate container 9dc426124633
Step 4 : RUN echo "8.8.8.8 google-dns" >> /etc/hosts
 ---> Running in 5ca20bdc01fc
 ---> 02f5a8fcbe53
Removing intermediate container 5ca20bdc01fc
Step 5 : RUN echo "Before:" && cat /etc/hosts
 ---> Running in d7879ab318cc
After: # <------------------------------------ After
127.0.0.1	localhost
::1	localhost ip6-localhost ip6-loopback
fe00::0	ip6-localnet
ff00::0	ip6-mcastprefix
ff02::1	ip6-allnodes
ff02::2	ip6-allrouters
172.17.0.2	827f45722fd6
 ---> e31a02dae837
Removing intermediate container d7879ab318cc
Successfully built e31a02dae837
```

And surprise! The "Before" and "After" content of `/etc/hosts` are exactly the same---it might be different that mine on your computer, but the two will be identical. Here this is all and shiny because I've introduced that to you and specifically used `RUN cat /etc/hosts`. When I did it, I took this for granted, seriously this is simply an `echo` with a stream redirection `>>`. What could go wrong.  
Add to this that we were experiencing network issues by then, and I just-could-not-understand why the `apt-get` commands kept failing.  
Anyway just to say: keep this one in mind, seriously.

Just to prove that there is not some form of dark magic going on, let's slightly modify the Dockerfile:

```Dockerfile
FROM        ubuntu:16.04
MAINTAINER  nschoe <nschoe@protonmail.com>

RUN         echo "Before:" && cat /etc/hosts
RUN         echo "On the same RUN statement:" && echo "8.8.8.8 google-dns" >> /etc/hosts && cat /etc/hosts
RUN         echo "After:" && cat /etc/hosts
```

Here, we chain several commands in the same `RUN` statement: first we write a string on the console (to easily see on the console output), then we add our line to `/etc/hosts` and then we display the content of this file.  
Let's see that it works: `docker build -t test-hosts:2 .`

```Bash
Sending build context to Docker daemon 2.048 kB
Step 1 : FROM ubuntu:16.04
 ---> 42118e3df429
Step 2 : MAINTAINER nschoe <nschoe@protonmail.com>
 ---> Running in e868b84b48d5
 ---> eb18d97115bd
Removing intermediate container e868b84b48d5
Step 3 : RUN echo "Before:" && cat /etc/hosts
 ---> Running in 35e0f7d0f999
Before: # <------------------------------------ Before
127.0.0.1	localhost
::1	localhost ip6-localhost ip6-loopback
fe00::0	ip6-localnet
ff00::0	ip6-mcastprefix
ff02::1	ip6-allnodes
ff02::2	ip6-allrouters
172.17.0.2	827f45722fd6
 ---> ed7412764867
Removing intermediate container 35e0f7d0f999
Step 4 : RUN echo "On the same RUN statement:" && echo "8.8.8.8 google-dns" >> /etc/hosts && cat /etc/hosts
 ---> Running in 92239e131e9c
On the same RUN statement: # <------------------------------------ same RUN statement
127.0.0.1	localhost
::1	localhost ip6-localhost ip6-loopback
fe00::0	ip6-localnet
ff00::0	ip6-mcastprefix
ff02::1	ip6-allnodes
ff02::2	ip6-allrouters
172.17.0.2	827f45722fd6
8.8.8.8 google-dns
 ---> fb1dfbc1099d
Removing intermediate container 92239e131e9c
Step 5 : RUN echo "Before:" && cat /etc/hosts
 ---> Running in 5aecaf88e302
After: # # <------------------------------------ After
127.0.0.1	localhost
::1	localhost ip6-localhost ip6-loopback
fe00::0	ip6-localnet
ff00::0	ip6-mcastprefix
ff02::1	ip6-allnodes
ff02::2	ip6-allrouters
172.17.0.2	827f45722fd6
 ---> ca084e296758
Removing intermediate container 5aecaf88e302
Successfully built ca084e296758
```

And yes, that works. So when we change the content of `/etc/hosts`, the changes persist for **this** `RUN` statement. Then it gets reset to the default one.

At the very first I was "what is this non-sense?", but then I realized it was actually pretty smart and made _much_ more sense that what I initially thought.

This is the moment where you should stop for a minute and think about it, really. Why do you think it _is_ smart, why do you think it is actually the right behavior? The Docker guys were _genius_ on this one. Take a minute, think about it, formulate an answer and then move on, I'll be fetching something to drink, because I don't know if you've noticed, this article has been growing in size :-)

...

Okay, so have you found a reason? Let me tell you: you have _everything_ you need to find the solution, we have covered this before, albeit _very_ implicitly.

It all has do to with layers and their reusability. If you modify the `/etc/hosts` in a statement and it persists for _all_ subsequent statements, then the _behavior_ of some of these statements is affected---thus can _change_---based on the fact that you have changed the `/etc/hosts`.  
For the commands where you do need it, this is intended, but not for the others. And it's far too easy to forget this, when Dockerfiles grow in size and complexity.  
Imagine then if you create Dockerfile #1 with the changed `/etc/hosts`, install some things, set up the environment, etc. Then, much later, you make Dockerfile #2 that begins with `FROM <Image#1>` where `<Image#1>` was built with the Dockerfile #1.  
It would inherit the changed `/etc/hosts` and you might have no clues...

Hence the fact that `/etc/hosts` is not persistent. When you need to use it, you _have to_ do like I did above: run the commands in the same `RUN` statement, and chain commands with `&&`.

#### The 'Non-interactive' Trick

This one is pretty simple. When you based your image from a ubuntu or debian, you will most likely have to install packages with `apt`. Sometimes, apt asks for confirmation or requires user interaction, which is not possible with Dockerfiles, since the Dockerfile statements are run automatically.

The trick for that is to instruct `apt` that it's not a user that is running the command, you typically do that by setting an environment variable. So typically, I'd advise you to put this statement near the top of your Dockerfiles: `ENV DEBIAN_FRONTEND='noninteractive'`.

Just to be clear, this has nothing to do with Docker, it's a debian feature, simply to indicate that the system is run non-interactively.

#### Maximum Number of Layers

Docker is not a virtualization system, it is not meant to emulate an entire OS. It is made to create custom, controlled environments and isolate running instances of the applications. The recommendation is to keep the images as small as possible, as concise as possible.  
Therefore, Docker limits your Dockerfile to 127 statements, this is a hard limit that you cannot overcome. Usually this won't be a problem and I've never come anywhere near that. But in case you're experimenting with this, now you know.

#### Use Intermediate Images

This is more of a general pattern piece of advice, but you should factor everything that makes sense as a group. When you write your Dockerfiles, you will update them, try the new images, make some changes, etc.  
When you feel you reached the perfect Dockerfile, take a step back and look what can be factored, group all of this into a separate Dockerfile, and build it separately. Then you can make your Dockerfile use `FROM` that image.

For instance, in the beginning, I found that I was always installing some basic tools in my images, things like `ping`, `less`, `curl`, `https-transport-utils`. Then I made some modifications, and usually installed some custom, company packages.  
Then, a bit later, to test some new designs, I was building the image again, but with the `--no-cache` flag, which explicitly disable caching of the intermediate layers. The problem is that I was using the `--no-cache` primarily because of the custom, company package, but the `--no-cache` doesn't discriminate: it starts from the every top. So I lost time reinstalling these basic packages.

You can solve this by making intermediate images, for instance an image with all the tools we've talked about, something that is general and used in all your images, name it `basic-env` (or anything, really) and then start your Dockerfiles with `FROM basic-env`.  
Then, when you use `--no-cache`, you will start at the top of you Dockerfile, it won't recurse.

You can create a little library of base images for different purposes and it can become quite handy, this is a nice feature, use it!

## Keeping a Clean system

When you build a lot of images, constantly changing your Dockefiles and rebuilding the images, or when you are in production and periodically rebuild the images to update some softwares you will end up with `<none>` images. These are images whose name have been stolen, for instance when you rebuild the same image after changing it.

Overtime, they will accumulate. They don't necessarily take lots of space because of the reusability of layers, but overtime they might stack up to a few gigabytes, especially if during the rebuild you made changes that broke layer caching soon in the Dockerfile.

But anyway, it's not clean to have dirt in the system.

We have already talked about this, but this seems like the right place for a reminder. You can list the "dangling" images---because it's their names---with `docker images -f dangling=true`.  
The `-f` or `--filter` flag is, well, a way to filter images. Here we filter images that are dangling.

In my system, after spending time building and rebuilding the examples here, I've got something like this:

```Bash
<none>              <none>              072cf077a900        10 hours ago        124.8 MB
<none>              <none>              6b8d30e38a09        24 hours ago        124.8 MB
<none>              <none>              cacab835af94        24 hours ago        124.8 MB
<none>              <none>              da5da8dbde09        24 hours ago        124.8 MB
<none>              <none>              0d2e4f1ecc87        24 hours ago        124.8 MB
<none>              <none>              27de2bb5b9cd        24 hours ago        124.8 MB
<none>              <none>              72364ccb3f67        24 hours ago        124.8 MB
<none>              <none>              2ae039632c4b        24 hours ago        124.8 MB
<none>              <none>              f1679af5a3a4        34 hours ago        124.8 MB
<none>              <none>              68dd81fe8bd6        34 hours ago        124.8 MB
<none>              <none>              b07ec7785cb1        34 hours ago        124.8 MB
<none>              <none>              68a5a19467de        34 hours ago        124.8 MB
<none>              <none>              154a86f325ae        34 hours ago        124.8 MB
<none>              <none>              0b3e63469e85        34 hours ago        124.8 MB
<none>              <none>              01dc6d9a0564        34 hours ago        124.8 MB
<none>              <none>              580293539e12        34 hours ago        124.8 MB
<none>              <none>              b75d703386ff        34 hours ago        124.8 MB
<none>              <none>              ea1a780308d0        34 hours ago        124.8 MB
<none>              <none>              d37781371091        6 weeks ago         125.1 MB
<none>              <none>              3e99a5614e5a        6 weeks ago         124.8 MB
```

We already know that we can remove an image with `docker rmi`. So it's a matter of chaining the commands. But `docker images -f dangling=true` is too verbose: the lines it displayed contain too much information and won't be directly parsable by `docker rmi`. So we need to add the `-q` or `--quiet` flag, as such: `docker images -qf dangling=true` which returns:

```Bash
072cf077a900
6b8d30e38a09
cacab835af94
da5da8dbde09
0d2e4f1ecc87
27de2bb5b9cd
72364ccb3f67
2ae039632c4b
f1679af5a3a4
68dd81fe8bd6
b07ec7785cb1
68a5a19467de
154a86f325ae
0b3e63469e85
01dc6d9a0564
580293539e12
b75d703386ff
ea1a780308d0
d37781371091
3e99a5614e5a
```

**This** is directly parsable by the docker daemon. You can then chain the commands in two different ways ---pick one, both are equivalent here:

- `docker images -qf dangling=true | xargs docker rmi`
- ``docker rmi `docker images -qf dangling=true` ``

And you should have a clean system. There will usually be an error message at the end, telling you that docker cannot remove image `<image-id>` because it is used by either running or stopped container `<container-id>`.

- If this is a **running container**, it means that you have a container that was instantiated from an image which you rebuilt inbetween. Usually it means you can update the container by recreating it from the same image name (provided you did not create another, completely different image, with the same name!), you can do this with `docker stop <container> && docker rm <container> && docker run [<your options] <image>` or `docker-compose up` if you happen to use `docker-compose` (which we will talk about in another article!)

- If this is a stopped container, make sure you checked that you can remove the container because you no longer need it before doing so.

This is all for now about keeping a clean system, we will talk about cleaning again when we talk about Docker Networks and again when we talk about Docker Volumes!

## Advanced Use Cases: Optimizing Your Images

Now that we have taken a couple of shots at writing Dockerfiles and building images by going through th basics, you're armed. You can do a pretty great deal of things, now. There are still two things of the outmost importance to see in Docker, namely Docker Networks and Docker (Named) Volumes---which are the next two articles by the way!---but right now I'd like to give you some advice and show you some use cases that you might want to follow if you want to write quality Dockerfiles.

### The PID 1 And Zombie Reaping Problem

This one we have already hinted at when we talked about using the shell or exec form, but I'd like to go into a little more details, because this is an important problem, but it is not obvious.

Whether you passed a command to run in your Dockerfile's `CMD` or `ENTRYPOINT` statement or you used `--entrypoint` in `docker run` or you specified the command at the end of the `docker run` command, there will be a process running inside your container---otherwise you'd not have a container at all.

If you "log in" the container---or simpy run `docker ps <container>`, you should see that your command has `PID 1` inside the container.  
"So, what's the deal" you may be thinking. Well... you know that `PID`s which stand for "Process IDentifier" is a number that identifies any given process on the system. These numbers are given more or less at random---actually not, but for now we don't care about that---except for the `1`.  
It's true and seems even logical that the first process started on your machine has `PID 1`, but that's not the whole story.

You know that "with great powers come great responsibility", well, it's the same in the process world. The `PID 1` process has a great responsibility and is usually called the `init` process.

One of his jobs, as its name implies is to "initialize" the system, by launching and starting various other processes (SSH server, graphical user interface, logging daemon, docker daemon, etc.). Sounds cool, it seems to be the boss. And that's true.

To understand the responsibility part---because so far we have only seen the good, _I-m-looking-like-the-boss-of-the-system_---we have to understand a little more about how UNIX processes are created.  
Don't worry this won't be long, and we will only scratch the surface, but this will be interesting---I'm sure some of you already know this, in which case you can skip this part ; if "reaping zombie" doesn't speak to you, you should keep reading^^.

Ho just before we begin, my goal here is clearly not to give a fully-fledge UNIX lesson, but merely to illustrate a problem and it's solution, so don't hold a grudge because I will greatly oversimplify. Do not hesitate, though, if what I say is point-blank wrong!

Okay, back to our little processes. Basically, what happens is that `init`---which may be called `systemd`, `upstart`, `systemV` or `init` on your distribution---creates and starts a certain number of processes (those whom we talked about previously: SSHD, Docker daemon, the windows / desktop manager, etc.). By doing so, `init` becomes these processes' `parent` (<- terms I'll write in this font means it's the official term, and not a term I'm making up to simplify ; you'll see in a minute why I say that).

So as a parent, `init` has rights over its children (basically to `kill` or terminate them them, ugh!) but it also has responsibility. Let's see what these are.  
Let's see a use-case when everything goes as planned first: suppose we have a desktop manager and we create another process: start the file manager (thunar, nautilus, etc.) or our terminal emulator (urxvt, terminator, etc.). These are new processes, and since it's the desktop manager that created them, it's their parents. 'Simple as that.  

When you close the the window you've just opened (thunar, nautils, etc.), the process behind exits and dies. It now becomes a `zombie` (see? I told you I had to write in this font, this is the _real_ term). A zombie is merely a stopped process that just awaits to be called in. It doesn't take up much resources (except for a slot in the maximum number of processes that can run at the same time) and just does nothing.  
When all goes well, its parent (in which case, the X server) calls `wait_pid` on its zombie-child and **at that moment** can the zombie process effectively disappear and go to the processes heavens---_you know, this place with unlimited CPU power, RAM space, etc._

Another important use case, suppose you send `SIGINT` signal to the X server---either with `kill` or `CTRL + C`. The X server `traps` this interruption and knows that it was just asked to gracefully shut down. So what he does is `forward` that `SIGINT` to all of its children, basically to tell them 'Daddy's got to stop, so please stop yourself so that we can all go die peacefully'. He then waits for all the children to exit, call `wait_pid` on each of them, and when _all_ of its children stop, it stops itself.

That's when everything happens for the best. And that's what happens with `init` too when you stop the system.

Let's look at some failure cases now: suppose the X server---to come back to our example---crashes unexpectedly. It so happens that all of its children are now `orphans` (mind the font!): they don't have a parent anymore. So nobody is going to call `wait_pid` on them when they are about to die, and nobody will forward them signals.  
Surely it doesn't _seem_ like a big deal right now, and usually it's not. **But** what if this happens _a lot_? When they die, these processes will take up slots in the total number of processes that can run together, and you _might_ end up not being able to start any new processes (admitedly, this doesn't happen a lot, but the principle is still correct).  
Even worse: suppose this process is currently writing a file on disk and you decide to stop the system. Since it doesn't have a parent, it will never receive any signal telling it it's time to stop, so it will keep writing on disk and BHAM: it will get brutally interrupted, which may corrupt your disk. If it had received a signal, it would have gently stopped writing and your disk would not be corrupted---again, OSes have **greatly** improved and there are _loads_ of mechanisms to cope with that, but the principle still applies.

Anyway, you can have a feeling that this is bad, and to cope with that, `init` has one great responsibility: `adopt` `orphan` processes. So in our example, if X crashes and had children, his children will be `adopted` by `init` and `init` will then becomes the parent. It **will** call `wait_pid` when they die, it **will** forward signals and wait for them, etc. This is the huge responsibility of the `PID 1` process.

And lots of things in Linux make that assumption. This is _supposed_ to be like that. And that applies in containers too since it's just "a fancy way of running a process".

So when you have processes run in a container, there is nothing special for these processes, they don't even know they are containerized. They just see an `init` tree as usually (it's much smaller of course), and at the top of this `init` tree is the process that was started in `CMD` or `ENTRYPOINT`.

Suppose we containerized an `nginx` to run a web server. `nginx` has now `PID 1` in the container. Now , that for some reasons, we started some other processes in this container, maybe processes for `postgreSQL`, or a monitoring or logging daemon, etc.  
All of these processes will be in the container's `init` tree, _i.e._ will be (direct or indirect) children of `nginx`. And they will _expect_ `ngnix` to behave like a proper `init` process. If they crash, they _expect_ `nginx` to `adopt` their now `orphan` children.  
They also _expect_ `nginx` to tell them when it's time to shut down the system so they can stop writing on disk and prevent disk corruption.

But hell, `nginx` is definitely **not** designed to do this. The team that developed `nginx` made a server, not an `init` system. And this goes for every program you containerize.

I won't go any further because it becomes complicated and it's not my area of expertise, but you have the gist of it: not every program is designed to run as `PID 1`. In fact very _few_ are: `init`, `systemd`, `systemV` are.

It's a currently heated debate in the Docker community: some people think it's Docker's responsibility to handle this and it means that Docker should embed a minimal `init` that will transparently handle this, and in the meantime we have to sit tight and wait.  
Some other think it's not Docker's responsibility, they consider that Docker's job is to create this new `init` tree for us, and we go from there. So it means it's our job to instantiate a minimal init system.  
Some images have been modified to be able to run `systemd` (like the image `solita/ubuntu-systemd`) for instance. Some people think it's overkill, some people think it's bad practise since you don't really containerize your process, rather `systemd` **and** them you run your process in the container. Which means there are **two** processes running and they don't like it.

There is a less overkill solution, which is an image developed by phusion. This image is `Baseimage-docker`, and it includes a lightweight, well designed init system. That might be a good start to experiment.

I am not giving you what's the "best" way to do things, because:

1. I'm not a UNIX expert, so I haven't carefully searched for every implication that might impose
2. I'm not a Docker developer, so I can't speak for them
3. I don't think I have settled my mind on this yet. For some small applications that I know I restart often, I usually don't bother with init ; for some more heavy applications, that I intend to let run for longer, I have tested both `systemd` with the `solita` image and the `Baseimage-docker` image.

I'll let you make your own research and decide, but now you are aware of the problem :-)


### Logging to Stdout

There is one command that when people first discover they think will solve all of their problems, it's `docker logs` which, as its name implies, should print the logs of a container.  
It does, but under some circumstances.

It's really simple, really and I don't know why people keep mixing this up: all `docker logs` does is print the container's `stdout` and `stderr`. That is all.  

This is why, if you want to have good quality dockerized applications, you should follow this rule: in a dockerized application, make sure to log everything to `stdout` and/or `stderr` instead of files. You can log to `/var/log/*` files if you want or need it, but you should _really_ log to `stdout` and `stderr` too because that way, you can quickly diagnose a problem with `docker logs`.

Now mind you: there is an internal Docker file somewhere whose purpose is to keep the content of `stdout` and `stderr`. What this means is that _everything_ that's output to these two streams is kept, so that, _at any point later on_ you can view it with `docker logs`.  
The obvious disadvantage of this is that when your container has been running for a long time, `docker logs` will print _a lot_ of result.

To solve this, we can use the `--tail <N>` option. Instead of showing _every_ line, it will only display the `N` last line. This is _very_ handy, because when your container has been running a long time and you need to check a bug that happened recently, you can just do `docker logs <container-name> --tail 50` and it will show the last `50` lines, which is instantaneous.  
Likewise, another useful option is to use the `-f` or `--follow` which works like the `-f` option in `tail -f`: it keeps the terminal opened and will display in real time the new entries. This is handy when you are trying to reproduce a bug.

Ho and some people have asked me what they should do if the application they dockerize doesn't natively prints to `sdtout/stderr`. This is easy, find the log file(s) they output to, and (in your Dockerfile) make a symbolic link to `stdout / stderr` with `ln`. Easy.

## Conclusion

Well I think we're reached the end of this article. I think I taught pretty much everything I could to help you write good quality Dockerfiles. I have tried to put it as much as I could, in an organized way so that you don't have to browse through 4 resources when you have a question.  
Unless I forgot something, you should have everything you need for quite some time to write Dockerfiles. As always, do not hesitate to [ask me directly](mailto:nschoe@protonmail.com) or come on the `#docker` IRC chan, I'm usually there, by the name @nschoe.

Please make any remark concerning this article, all I want is for it to be extremely clear and give you everything you need. If you feel that I have forgotten something or if a part is unclear, do no hesitate to tell me, I'll do whatever I can to make it less confuse.

I hope you liked the article, the next one will be about correctly handling data in Docker containers with Docker Named Volumes. This will be another _very_ important part and I hope you'll like it too!
