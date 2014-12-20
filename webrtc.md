---
title: WebRTC
---

### WebRTC in a nutshell
WebRTC signifies _Real Time Communication_ for Web technologies. WebRTC is the answer to the expansion of digital communication and the rise of everyone being connected.  
I believe WebRTC will play a very important role in the IoT, even though it is not yet visible.

### WebRTC Forces
In this world where everyone talks about the "Cloud" and centralization of data, WebRTC is quite innovative.  
The key and main points of WebRTC are :

#### Peer-to-Peer
This is what I was talking when I said WebRTC was innovative with respect to the Cloud, _for once_ we have something that doesn't transit through a server...  
In WebRTC, you can make calls between peers, directly. Note this: WebRTC is **peer**-to-**peer**, _NOT_ **browser**-to-**browser**. This is hard to keep in mind because for now, the only easy and useful APIs are mainly written in Javascript, for web browsers. WebRTC evolves rapidly though, and the trend may change quicker than I expect.

#### Multimedia
The second true power of WebRTC is that it deals with multimedia streams. I am not merely saying that you can use multimedia with WebRTC (that would not make it much more interesting than other technologies), I am saying that WebRTC actually _deals_ (very well) with video and/or audio.

In web browsers for instance, WebRTC does [**Adaptive Bitrate Streaming**](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming) whenever you are making a call (may it be audio or video). It basically means that it will take care of measuring the quality of the call, and when the bitrate fluctuates, instead of lagging or cutting the conversation, WebRTC will decrease the quality of the encoding to keep your call alive.  
Another great feature that makes WebRTC extremely handy is that it performs automatic audio / video synchronization, preventing the quite unfortunate delay between the movement of your mouth and the actualy sound. I'm sure you know what I mean if you ever watched a movie in (bad) streaming.

#### Raw Data Too !
When you browse websites that talk about WebRTC, you always see audio, video or both. This is great indeed. But they always seem to neglect the third "channel" (WebRTC name): raw data.  
WebRTC allows you to open a UDP channel between the two peers, to transmit raw data. And for me, this is the most awesome part: you can just define your own protocol (or use JSON, XML, whatever you want) and transmit, real time, anything you need.

- It can be a file, this way you can share a file peer-to-peer (you don't always want to host your file to an unknown server). How many times did you wonder how to send photos from your latest holidays, now WebRTC made it easy.
- It can be ascii strings, maybe you are implementing a streaming service and you want to provide the user with the subtitles.
- It can be any other raw data, maybe you are are implementing a multimedia video game and you transmit the positions of the players (or any other related data for all that matters).

Anyway, the possibilites are infinite, and maybe you can see why data channel are much more interesting than audio/video.

#### Secured
Ah! Now the real thing. WebRTC _ensures_ that your connections are secured. And by ensure, I mean that it is **mandatory**. You cannot have WebRTC connections happening in plaintext.  
And in this world where robbers are now digital hackers or big companies use targeted ads, this sounds good.

### Basic Principle
I won't go into much details here as I'll try to write posts that explain better, but I'm going to introduce you to the basic principles behind WebRTC.

#### Signalling Channel
Lots of things happen in WebRTC between two peers: they need to negociate encryption settings, encoding settings, multimedia settings (do we transmit audio, video, data ?), etc.  
In order for these parameters to be negociated, the two peers need to communicate with each other.  
WebRTC makes use of a _signalling channel_ to transfer this information. Signalling channel is basically a "classic" channel between a peer and a central server. So yes, to implement WebRTC, you need a central server to initiate the calls. The peers use this signalling channel to negociate all the parameters they need and then start the conversation peer-to-peer, forgetting the central server.

#### SRTP
Underneath the UDP protocol, WebRTC makes use of the [Secure Real-time Transport Protocol](https://en.wikipedia.org/wiki/Secure_Real-time_Transport_Protocol). Basically, the data is sent via packets, which contain headers to give information. In particular (for audio and video), the timestamp at which the sample was captured is saved, so that the receiver can perform jitter protection and audio / video synchronization.  
The header contains a _Payload Type (PT)_ which describes the kind of data it transports.

#### SDP
To negociate the parameters, the peers need to use a common format. [Session Description Protocol (SDP)](https://en.wikipedia.org/wiki/Session_Description_Protocol) is used in case of WebRTC. There is currently lots of discussion as wether JSON should be used too, but at the moment, SDP is chosen. Basically, SDP contains information about the IP address of the peer, the payload type, the bitrate used, etc.

#### STUN server
In order for peers to communicate directly, they need to tell each other (among other things) their IP addresses. But today, personal computers are all hidden behing NAT (router, Internet Box, etc). So they don't even know their IP address. [Session Traversal Utilities for NAT (STUN)](https://en.wikipedia.org/wiki/STUN) is a server used for peer to discover their own IP address, exactly the same way you do when you open your browser and go to a website to know your IP address.

#### TURN servers
For some reasons sometimes, peer-to-peer connection fails to establish (Google's stats say something aroun 8%). In these cases, WebRTC is not possible directly, you have to use [Traversal Using Relays around NAT (TURN)](https://en.wikipedia.org/wiki/Traversal_Using_Relays_around_NAT) servers, this is basically a server which acts as a relay between the two peers.  
It removes the peer-to-peer aspect of WebRTC connections, since it now goes through a relay server, so you have to be careful.

This is all for WebRTC presentation, I will try to write detailed posts about the differents aspects and show you some implementations.