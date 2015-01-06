---
title: Peer-to-Peer Made Easy With WebRTC [1]
description: First part of a series of articles explaining how to do develop simple peer-to-peer applications with WebRTC. In this article, we will talk about the mains ideas and concepts behind WebRTC, how it works in the inside, why this is the greatest innovation since the creation of the web browser and the different protocols involved in WebRTC.
---

## Introduction
A couple of months ago, while documenting to solve real-time video transmission, I came across that new, emerging technology called _WebRTC_. I liked it so I read about it. A lot. There wasn't a lot of thorough documentation available back then, so this was one of these projects where you have to fetch the information right at the source: RFCs and raw protocols documentation. It was tedious -but this is the only way, sometimes- and I'd like to share what I have learned and gathered so far.  
There are more documentation sources available now and there are a lot of aspects that are still blurry to me, but I aim at providing a decent overall explanation of WebRTC, in a _not-so-boring_ way and with as many **examples** as possible!  
Because, _c'mon_, we all love examples!

Oh and, while there may be other sources of information or tutorials available out there, several of which more precise and more exact on the theory, **I** will always try to focus on one *critical* point: make you _truly_ **understand**.  
Back when I was in [preparatory school](https://en.wikipedia.org/wiki/Classe_pr%C3%A9paratoire_aux_grandes_%C3%A9coles), my friend [Alp Mestan](http://www.alpmestan.com/) taught me that when dealing with something new, it was much more important to create a mental representation, with _meaning_ (through the use of associations, or comparisons for instance) rather than "bulk-learning" something by heart. So that's what I'll focus on in these articles.

I will _have to_ begin with a bit more theory than practice, bear with me, the examples and the "fun" part are coming. I will try to keep the "boring" parts short.

![WebRTC Logo](/images/webRTC_logo.png)

### The Need for WebRTC
Before diving into WebRTC, let me explain why WebRTC is actually welcomed in this world where dozens of so-called "new technologies" pop up every now and then.  
We are in a era where (tele)communications are everywhere. This has been true for the past decade and this trend is about to grow exponentially with the arrival of the Internet of Things, in which every little objects will be connected and will transmit data.  
We are alway more demanding: speed, reliability, real-time and now privacy (sort of: I will leave apart the average Facebook user who ~~inflicts~~ exposes his life to everyone).

One of the solutions to this is "The Cloud": you would ~~give~~ store your data on the servers of decidated companies, and your devices would sync with these servers so that your information is always available from any of your devices. The Cloud is not new: we all have been doing this for years; may it be via FTP, SSH, rsync, scp, etc. It was just recently made easy via better integration and ease of use (Dropbox, Google & Microsoft Drive, etc).  
This is about _data storage_, but what about _data sharing_ and _multimedia communication_?

That is where WebRTC comes in!

### What WebRTC Brings to the Table
Contrary to "basic" data storage and synchronization (which is not that easy after all), we did not have a suitable method for data sharing and multimedia communication.  
Let's review the possibilities:

- data sharing:
    + How often did you wonder how to send your holidays' photos to your family?
        * Send several emails? They are quite limited in the weight of the enclosed document.
        * Upload the photos on Google Picasa? It means create one account for you, one account for your friends; and you know, when the photos are on Google's servers... (they tend to stay here)
        * Then came Facebook: no comment here... we are looking for a way to share the photos with our family, not the 645 "friends" we have on Facebook. Besides, it's the same as with Google: once Facebook has them, they have them for good.
        * Upload your photos on your personal server and give FTP access to your family? That's possible, but it requires a server (physical), a FTP server installed on it, your family needs to have a FTP client, etc.
    + Have you ever needed to send confidential files to somebody?
    + Maybe you just need to do a one-time file sharing and don't want to set up a whole thing for it, and you don't necessary want somebody else to host the file.
- Multimedia Communication:
    + We have been using text-based communication software for a very long time, and this is very fine: AIM, MSN, Jabber, etc; but what about **multimedia** communication? I mean video and/or audio?
    + You have things like TeamSpeak or equivalent for audio communication. But forget about privacy.
    + Skype is another solution, and it works not so bad. Virtually I think Skype was the only solution.

All these solutions only partially solve the problem. And most of them need external (or even third-party) softwares. This is far from optimal.

What WebRTC offers is:

- P2P (hence _decentralized_): the communication is between you and your peer, your data don't transit through a centralized server. \[1\]
- audio and/or video real-time transmission (bonus: WebRTC handles track synchronization)
- data transmission: may it be your photos, a movie, backup data, basically anything.
- privacy: WebRTC makes encryption mandatory, so anything you transmit (video, audio, data) is encrypted!

And the real turn-up is that current implementations allow you to perform all of these, right from your web browser. It means that you don't have to install any additional software (nor does your targeted user)!  
Among the examples that we'll build together are:

- a browser-based Skype-like application (_i.e_ make audio and/or video call to a peer, from your browser)
- a simple file sharing application, with which you will be able to send a file to a peer, securely, without the file being hosted on any privately-owned server.

In a word, WebRTC allows you to do answer these questions _just the way it should_.

We will obviously come back on each and every of these points in the following articles.

### The Workflow of a Full-Stack WebRTC Application
In order to build a WebRTC application, we will follow these steps (which will be more or less the structure of the upcoming articles):

- acquire audio and/or video media stream
- set up a _signaling channel_
- use ICE framework with a STUN server to discover its public IP and gather candidates
- create and send an offer
- receive the offer and create & send the answer
- receive answer and start transmitting audio/video/raw data

This is a raw list of steps and there is obviously more at play. We will take time to study each steps in order to understand fully what's involved.

Now! The "boring" part is over, I hop this was not too long, but I think it was necessary. We are going to begin the concrete part of this series of articles.  
Before we dive right into it, I'd like to mention a point: WebRTC is designed to be **peer-to-peer**, not **browser-to-browser**. However, the first stable implementations were designed for the web browsers, and this is indeed the fastest way to use and introduce the concepts. In these articles I will provide browser-based examples, because this is what I am the most familiar with. (I will try to further the _native_-implementation if you wish).

That being said, let's discover WebRTC together.

## Acquire Audio and Video Media Streams
Remember that we want to avoid using third-party softwares or plugins. We are thus going to use WebRTC from the browser, and our weapons will be Javascript and HTML5.  
The obvious first thing we need to do to transmit multimedia data is to _acquire_ it. And this is already a challenge!  The "usual" method of acquiring video or audio is through the use of Adobe Flash, but that would suit us because we want to avoid plugins. We were stuck right here until recently, when a new Javascript API `MediaStream` appeared ([doc here](http://www.w3.org/TR/mediacapture-streams/)).

### Description of the MediaStream API
This API describes a stream of video or audio data. It contains methods and callback to create the streams, manipulate them and use them with other APIs (including the WebRTC API of course).  
A `MediaStream` object can be empty or contain several `MediaStreamTrack`s. A `MediaStreamTrack` is what you can expect from the name: a track, like an audio track from a CD; except that it can be either a video or audio track (described by the `kind` attribute). What is really neat is that all `MediaStreamTrack`s inside a `MediaStream` are synchronized: this proves very useful to keep video and voice synced. Quite naturally, a `MediaStreamTrack` can be manipulated and queried. We denote a number of interesting attributes:

- `muted`: self-explanatory. Unless there is some weirdness with WebRTC: a muted `MediaStreamTrack` doesn't stop transmitting data, it "just" transmits _meaningless_ data; we'll come back on this later.
- `remote`: says if the track comes from (one of) our peer

Each `MediaSTreamTrack` contains one or more `channels` (for instance, an audio track might contain a channel for the left speaker, one channel for the right speaker, etc). This is all just for documentation, because in our use case (web browser), we'll get video from the webcam and audio from the microphone. We won't be dealing with several microphones, so it will all be transparent for us and handled by WebRTC.

A `MediaStream` has an input and an output. The input depends how you got the stream (local file, webcam, microphone, ...) and the output will typically be a HTML5 `<audio>` or `<video>` for the remote streams (you display the remote stream in your web page) or a WebRTC `RTCPeerConnection` (you send your stream to your peer), though you can record the streams to files in theory.

### Acquire a Stream (and Display It)
To capture the video from the webcam and/or the audio from the microphone, we use `getUserMedia()`. It takes three parameters:

- A `MediaStreamConstraints` which instructs the browser what resolution, format, quality, etc it must query
- A success callback, which will be called if the capture succeeds (the user granted permission and the device works correctly)
- A failure callback, called if the user denies access ot its devices or another problem occurs.

You will find the API documentation [here](http://www.w3.org/TR/mediacapture-streams/#dom-navigator-getusermedia) if you want to know every settings combination you can make.

If the call succeeds, your callback is passed a `MediaStream` object, with which you can work. Let's first display it in a `<video>` element to have a mirrored view.

-------
\[1\]: I don't take TURN servers into account here (explanations on what a TURN server is in a next article)