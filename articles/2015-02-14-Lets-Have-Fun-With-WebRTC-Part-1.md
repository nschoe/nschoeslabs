---
title: Let's Have Fun With WebRTC! - Part 1
description: WebRTC is a new technology that allows peer-to-peer media communication. It is fast (it relies on UDP), it is secure (all payload is encrypted -mandatory), and the best, simplest APIs have been implemented in web browsers, so all of this is available right now, without the need of any external softwares or plugins! In this series of articles, we will build some cool applications together to demonstrate the power of WebRTC!
---

## Introduction

### So, what's the deal?
Today we are going to build an audio/video communication app with WebRTC, and the best part is that it will be used from the browser: no additional softwares, no additional plugins.  
WebRTC is a new technology that allows peer-to-peer (multimedia) communications. It is not intended for browsers in particular, but it is true that the simplest (_read "easiest to use"_) APIs are implemented in Javascript and thus, we will use these.

### What does WebRTC bring to the table, and what can we do with it?
In this world today, all we can hear is about "[The Cloud](http://betabeat.com/2013/03/chrome-extension-replaces-every-instance-of-the-cloud-with-the-far-superior-my-butt/)". While it has some advantages (data is synchronized across all our devices, it is backed up in case of failure, etc) it does have some drawbacks (data is hosted on private companies that can analyze and sell it, these big servers are the target of some criminal attacks, etc).  
WebRTC is **not** a replacement for the Cloud, it has nothing to do with it, but it brings some interesting features:  

- **Fast**: WebRTC uses [UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol) as the [transport protocol](https://en.wikipedia.org/wiki/Transport_layer), so it is intended to be quick. Besides, it is implemented using only native Javascript (and HTML5) APIs, so forget the load of Flash Player...
- **Peer-to-peer**: WebRTC is designed to be peer-to-peer, so data transit from your computer to your peer's. That's it. There is no central (and privately-owned) server that can intercept your data.
- **Secured**: WebRTC _makes it mandatory_ for **all** payload to be encrypted. You simply cannot initiate a WebRTC call without your data to be encrypted. That is an important aspect that I really like.
- **Media and real-time**-enforced: WebRTC is designed to be a real time protocol (uses [RTP](https://en.wikipedia.org/wiki/Real-time_Transport_Protocol) on top of UDP) and has been designed to handle audio/video streams of data. For instance, the browser's implementations have built-in [adaptive bitrate streaming](https://en.wikipedia.org/wiki/Adaptive_bitrate_streaming): the quality and the compression of audio and video is altered on-the-run to compensate variation in communication's strength (rather than having your connection hanged up, you simply have a decrease in quality until your communication gets back to its top quality).

#### What are we going to do anyway?
Together, we will build some interesting stuffs. I'll explain how to do it along the way and I'll introduce the underlying concepts as we need/meet them.  

- For this article, we will build a **Skype-like** application, right into the browser: no more ads, and no more spying. This is gonna be fun!
- For the next article, we will build a **file-sharing** application: this will allow you to share any file with anyone of your friends without uploading your file to a server. Pretty handy: you don't want the photos from your last night out to end up on 9gag, do you?

### Are we good to go?
Well... ready when you are!  
Just go grab a bottle of Coke, go buy some candies (_sugar my friend, sugar..._) and listen to some good music (_may I suggest Led Zeppelin, Kashmir?_) and then we are good to go!

## Let's do This: Skype-like Application in Browser!

### A Little Word on the Workflow
You can find the code for that article on my [github repo](https://github.com/nschoe/webrtc-fun-part-1).  
Here is what we are going to need to do to build our application:

- **Acquire audio and/or video media stream**: if we want to transmit video and audio, we first need to acquire it, and remember: we are in the web browser, and we don't want things like Flash or Java plugins.
- **Set up a _signaling channel_**: I will describe what it is, what it means and why we needs this. Remember now that it allows the peers to negociate the parameters of the connection.
- **Peer discovery**: We will use ICE framework with a STUN server to discover our public IP and gather candidates. Same thing: I will explain what that means, but basically, it has to do with the fact that you don't know where you peer is on the Internet.
- **Create and send an offer**: WebRTC jargon here. In an _offer_, the _caller_ lists a number of parameters for the connection.
- **Receive the offer and create & send the answer**: This is the previously created offer, the other peer does the same (except this is called an answer this time) and sends it.
- **Receive answer and start transmitting audio/video/raw data**: This is where the real peer-to-peer starts. Previously, the answer and the offer were transmited using the _signalling channel_.

This is basically the steps we will follow. We will learn together the underlying technologies used by WebRTC (not everything is new in WebRTC) and detail some aspects of the protocols.

**Bonus point**: writing a project-based paper would not be the same without some code example. So along the articles I will post & comment samples of code, but you will find the whole code [here.](https://github.com/nschoe/fun-with-WebRTC-part-1)

### Acquire Audio and Video Media Streams
Remember our workflow from earlier? The very first thing we need to do to transmit our pretty face & voice is to acquire the streams. Sounds easy, but until recently, you had to rely on either Adobe Flash, Java (or Microsoft Silverlight?).  
A new, neat Javascript API, that integrates well with HTML5 and WebRTC helps us now : 
[The MediaStream and MediaCapture API](http://www.w3.org/TR/mediacapture-streams/).

Let's describe this API briefly first, so that we know what we are dealing with.

#### Description of the MediaStream API
This API describes a stream of video or audio data. It contains methods and callback to create the streams, manipulate them and use them with other APIs (including the WebRTC API of course).  
A `MediaStream` object can be empty or contain several `MediaStreamTrack`s.  
A `MediaStreamTrack` is what you can expect from the name: a track, like an audio track from a CD; except that it can be either a video or audio track (described by the `kind` attribute). What is really neat is that all `MediaStreamTrack`s inside a `MediaStream` are synchronized: this proves very useful to keep video and voice synced. Quite naturally, a `MediaStreamTrack` can be manipulated and queried. We denote a number of interesting attributes:

- `muted`: self-explanatory. Unless there is some weirdness with WebRTC: a muted `MediaStreamTrack` doesn't stop transmitting data, it "just" transmits _meaningless_ data; we'll come back on this later.
- `remote`: says if the track comes from (one of) our peer(s) or from us.

Each `MediaSTreamTrack` contains one or more `channels` (for instance, an audio track might contain a channel for the left speaker, one channel for the right speaker, etc). This is all just for documentation, because in our use case (web browser), we'll get video from the webcam and audio from the microphone. We won't be dealing with several microphones, so it will all be transparent for us and handled by WebRTC.

A `MediaStream` has an input and an output. The input depends how you got the stream (local file, webcam, microphone, ...) and the output will typically be a HTML5 `<audio>` or `<video>` for the remote streams (you display the remote stream in your web page) or a WebRTC `RTCPeerConnection` (you send your stream to your peer), though you can record the streams to files in theory.

#### How to Actually Capture That Stream Now?
Okay, so to capture the video from the webcam and/or the audio from the microphone, we will use `getUserMedia()` function. It takes three parameters:

- A `MediaStreamConstraints` which instructs the browser what resolution, format, quality, etc it must query
- A success callback, which will be called if the capture succeeds (the user granted permission and the device works correctly)
- A failure callback, called if the user denies access or its devices or if another problem occurs.

You will find the API documentation [here](http://www.w3.org/TR/mediacapture-streams/#dom-navigator-getusermedia) if you want to know every settings combination you can make.  
**Careful**: do check the browser's compatibility, because for example, as we speak, Firefox doesn't respect the resolution constraints for the webcam...

The idea is to create a set of `MediaStreamConstraints` to describe what we want to acquire, then call `getUserMedia()` with it. If it succeeds, the success callback will be called and the `MediaStream` will be passed as an argument. If it fails, the error callback is called.  
We will test that right now: acquire the video and audio and display it back to a HTML5 `<video>` element.

First, the (most basic) HTML5 document. It just contains the `<video>` element that will contain the stream from our webcam.
``` html
<!doctype html>
<html>
    <head>
        <meta charset="UTF-8">
        <title>Mirrored view of webcam</title>
    </head>

    <body>
        <h1>Mirrored View with getUserMedia()</h1>
        <video id="localVideo" autoplay style="border: 1px solid black;"></video>

        <script src="1_mirror.js"></script>
    </body>
</html>
```

It is worth noting the `autoplay` attribute of the `<video>`. **This is important**: without it, the video won't start after being attached in Javascript. I once spent quite some time looking for a problem in my Javascript code, just because I had forgotten that attribute -you can omit it if you use `localVideo.play()` directly in Javascript.

Then comes the Javascript part, which uses the API:
``` javascript

//Makes it portable across all browser until the API is standardized and well-supported.
navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;

/*
Build our constraints, here we keep it simple: yes to audio and video.
But we might be more specific: 
var constraints = {
    mandatory: {
        height: {min: 720}
    },
    optional: [
        {frameRate: 60},
        {facingMode: "user"}
    ]
};

But then again: be careful, the browsers don't (yet?) follow the documentation on that one... that is a shame.
Currently Chrome follows the standards much more closely that Firefox does.
*/
var constraints = {
    video: true,
    audio: true
};

var localVideo = document.getElementById ('localVideo');

// This is the actual call, passing the constraints and the callbacks as parameters.
navigator.getUserMedia (constraints, captureOK, captureKO);

/*
Success callback: when the capture succeeds, we create a "ObjectURL" from the stream and assign it as the source of
the <video> element. This is where you need to do 'localVideo.play()' if you did not use the "autoplay" attribute in
the HTML5 <video> element.
*/
function captureOK (stream) {
    console.log ('capture was ok');
    localVideo.src = window.URL.createObjectURL (stream);
}

/*
Error callback: in case the capture failed. Be careful of the message, under Firefox for instance, it can display
PERMISSION_DENIED when it cannot satisfy the constraints.
*/
function captureKO (err) {
    console.log ('Capture failed with error: ' + err);
    alert ('Capture failed with error: ' + err);
}
```

And voilà! We have it now.  
If you try this at home (don't forget that you can clone the github repo to get the code) you should see your pretty face in the web page. Talk or pass your finger on your microphone to verify that the sound is correctly captured.  
What we have here is already something, albeit it doesn't look like it. A couple of months / a year ago, just doing so was impossible: you *had* to rely on external plugins (Java, Flash), so this is a neat API.  
Another bonus point: this is compatible with Android devices, so your web app can be used from a mobile!

The code is available [here](https://github.com/nschoe/fun-with-WebRTC-part-1/tree/master/1_mirror).

**Just for fun** and even if this deviates from the WebRTC topic, let me show you how to add a few lines of code to this basic application to let you grab the picture at the click of a button (or the stroke of a key) and manipulate it. That way you can have your custom "photo booth" application to grab pictures from your webcam, apply a couple of filters and save it on disk. Handy if you don't want to start a dedicated program for that (like Cheese on Linux).

_You can safely pass this section if you want to focus on WebRTC, [jump here to pass.](#set-up-a-signalling-channel)_

This is actually very easy to do: we capture the video (we are not interested in audio here) with `getUserMedia()` and attach the stream to a `<video>` element.  
We will have a event on a button (or capture the enter key press) that will grab the image currently displayed on the `<video>` and draw it into a `<canvas>`.  
For simplicity's sake, we will use the _right click > save as_ option to save the image on disk. We will draw the picture on the canvas thanks to a special feature of the canvas API: `toDataURL()`.  
Let's take a look how it's done!

First, the html:
``` html
<!doctype html>
<html>
    <head>
        <meta charset="UTF-8">
        <title>Simple Photo Booth</title>
        <link rel="stylesheet" type="text/css" href="1_1_photo_booth.css">
    </head>

    <body>
        <h1>Simple Photo Booth</h1>
        <p>
            <em><strong>Instructions</strong></em>:
            <br>
            Press spacebar or click the capture button to record the picture. Then use right click > save as to save
            the picture on disk.
            <br>
            Easy !
        </p>

        <!-- The HTML video element, to display the webca stream in real time -->
        <video id="localVideo" autoplay></video>

        <!-- The canvas on which we will be display the picture -->
        <canvas id="canvas" width="640" height="480"></canvas>


        <!-- The button to capture the picture, spacebar can also be used -->
        <button id="captureBtn" disabled>capture!</button>


        <script src="1_1_photo_booth.js"></script>
    </body>
</html>
```

Nothing special here, we have our `<video>` element, as seen previously, to display the stream from the webcam; it has a `canvas` element to receive the captured image.

**Note about canvas size**: there is a small yet important thing to know about canvas' sizes. There is a `width` and `height` **HTML attribute** and a `width` and `height` **CSS properties**: _they are different_. The HTML properties define the size of the image (the data). We set it to 640x480 which means the image will have 640 columns and 480 rows of pixels. And as every DOM element, you can define the size at which you want to display this element, here we chose 640 pixels for the width and 480 pixels for the height. Which is quite logic, but you can totally display a 640x480 image with a different size: if bigger the image will be slightly blurred, if smaller, the image will look a bit "enhanced". Well you know what I mean.  
But please remember that the HTML `width` and `height` serve a different purpose that the CSS properties (this is important, because if you don't set the right HTML values, you may display only a fraction of the capture image!).

Then comes the very minimalistic CSS:
``` css
body {
    width: 1300px;
    margin: auto;
}

video, canvas {
    /* So that we can have the video and the image next to each other */
    display: inline-block;
    vertical-align: top;
    border: 1px solid black;
    /* Set the size this time, just for alignment */
    width: 640px;
    height: 480px;
}

button {
    display: block;
    width: 1000px;
    height: 200px;
    margin: auto;
    font-size: 4em;
    text-transform: uppercase;
}
```

Nothing particular here...  
The `body` part is just to center the page horizontally. Read the previous paragraph for the `width` and `height` (last reminder: you _can_ omit the CSS `width` and `height` properties, the canvas will grow to match the correct size, but you _have_ to set `width` and `height` in the html file).

An last but not least, the Javascript:
``` javascript
// Same as previously: make the call portable across browser
navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;

// We don't need audio for the photo booth
var constraints = {
    video: true,
    audio: false
};


var localVideo = document.getElementById ('localVideo');
var canvas = document.getElementById('canvas');

// Get the canvas context from which we can extract data
var ctx = canvas.getContext("2d");

// Actualy call to capture media
navigator.getUserMedia (constraints, captureOK, captureKO);

// Start our application
function captureOK (stream) {
    console.log ('capture was ok');

    // Attach the stream
    localVideo.src = window.URL.createObjectURL (stream);

    // Register an event on the enter key
    document.addEventListener("keyup", function(evt) {
        // 13 : enter key
        if (13 == evt.keyCode ) {
            recordImage();
        }
    });

    // Register the event on the button
    document.getElementById('captureBtn').disabled = false;
    document.getElementById('captureBtn').addEventListener("click", recordImage);
}

function captureKO (err) {
    console.log ('Capture failed with error: ' + err);
    alert ('Capture failed with error: ' + err);
}

// Our function to record an image in the canvas
function recordImage(evt) {
    console.log("event: ");
    ctx.drawImage(localVideo, 0, 0);
}
```

The comments are pretty self-explanatory.  
We get the canvas' context which allows us to draw on it.  
We use the `drawImage` function, which is the magic line here: it first takes the image to display, here our `<video>` element, then if you only use three parameters, the second and third parameters are the destination coordinates in our `<canvas>`.  
To illustrate my previous comments about the width and the height, the `x` coordinate ranges from `0` to the HTML's `width`, and you guessed it for `y`; independently of the CSS properties.

Back to Javascript: we simply define two event listeners to catch the click on the `<button>` and the press of the `enter` key.  
And that's it, we're done.  
That closes the small detour I took to explain that photobooth app.

The code is available [here](https://github.com/nschoe/fun-with-WebRTC-part-1/tree/master/1_1_photo_booth).

### Set up a Signalling Channel
Okay so we have acquired our webcam stream, good. Now what?  
The goal is to transmit it to our peer, right? So we need to tell him, in WebRTC terminology you say _call_ him. But before that, we need to tell our peer's client a set of parameters: what audio and video codecs we are going to use to encode our stream, he certainly needs to know that in order to be able to understand it.  
But _how_ do we do that? How do we _contact_ our peer: we are not yet in a conversation with him!  
That is where the _signalling channel_ comes in. As its name implies, it is used to _signal_ the other peer that we want to call him, what kind of data we will transmit, what are the codecs used, our IP address, etc.

#### Okay, so How do we do it? What Does the Doc Say About That?
Well that's one interesting point: the documentation leaves it to us. Really. When I first read books and articles about WebRTC, they said that "the signalling channel was up to you", but what does it _mean_ concretly?  
And it is only after playing with it that I understood: it is _really_ up to us: we can just shout out the parameters to our peer and he will write them down manually and feed the Javascript object by hand; you can send them via email, you can write them on paper, etc.  
All of this is no joke: it works. But this is dumb, as you might guess: your peer will have to write them manually, and believe me, when you will see them, you won't want to write them by hand...  
So we do need some kind of automation, a much clever solution that using email.

What we need is a way to transmit text data and bind events from the Javascript, so that we can automate this. So there are several candidates: you can use HTTP with AJAX or long polling. What I like to use, though, is WebSockets. The WebSockets API is very well supported and easy to use from the browser, and you will see that the WebRTC API is **very** similar to the WebSockets' (it was designed to be almost transparent).  
For the backend part, you can use a PHP implementation, or a C, or anything you want, really. For simplicity's sake and because I really love it, I will use Haskell.

#### Wait whut?! Aren't You Talking About a Central Server?
Well... yes. I admit. But I did not lie to you: WebRTC **is** peer-to-peer, the actual call will be peer-to-peer. But you have to understand that some parameters need to be transmitted, and you need a support for that -a bit like you need to give someone your phone number face to face the first time before you can be called.  
And on the Internet, the simplest way to do this, is to host a central server, with a well-known IP address (or better: a domain name).  
So what we will do is connect to that signalling channel, transmit the parameters needed to initiate the call and as soon as we can, we will make the (peer-to-peer, secured, etc) WebRTC call. At this point, we can purely and simply close our connection to the signalling channel.

While the documentation leaves it to us to implement the signalling channel of our choice, it does impose the format for transmitting the parameters. There was a candidate: [JSON](https://en.wikipedia.org/wiki/JSON), but this is [SDP](https://en.wikipedia.org/wiki/Session_Description_Protocol) that was retained. At least for now (the documentation is evolving quite rapidly).

Ready? Let's do this!

#### Some Words about SDP
In your WebRTC applications, you are very likely to have to examine SDP in your browser console, so I am going to describe it rapidly for you.  
Oh by the way, SDP means "Session Description Protocol" so it has been designed for it.

SDP files are a list of lines that take the form: `k=<value>`.  
The key `k` is a single character, the `<value>` is a UTF-8 string.  
_There cannot be a space on either side of the `=`_.  
Here is a list of common keys you will encounted in your SDP debugging sessions (yay!):

| k=         | exemple     | explanations |
|:-----------|:------------|:-------------|
| v=         | 0 | version number, for now must be 0 |
| o=         | 579453792423642384 2 IN IP4 127.0.0.1| session_id (to uniquely identify the session) session_version (count the number of exchanges between the two peers) IN (specifies the network is **IN**ternet) IP4 (IP version 4) 127.0.0.1 (IP address of the sender, us) |
| s=         | -           | session name: it is mandatory with one UTF-8 character. You will typically see a dash here |
| t=         | 0 0         | start and end time of the session, 0 means forever valid |
| a=         |             | The `a` attribute is the most common key, it is the "generic" key. It is application-specific, this is where we will pass attributes. |
| a=         | a=group:BUNDLE audio video | Means that we will transmit both audio and video data. |
| a=         | a=rtcp:34069 IN IP4 129.56.34.223 | Specifies IP address and port on which RTCP will be used |
| a=         | a=candidate:4022866446 1 udp 2113937151 192.168.0.197 36768 typ host generation 0 | This line you will see often. It describes a _candidate_, we will see what it means later, but remember that this is an network interface from which data can be transmitted (eth0, wlan0, etc) + a protocol (here you can see "udp") + port number |
| a=         | a=rtpmap:100 VP8/90000 | The "rtpmap" parameter tells information about the payload. It says that the payload is of type 100 (see [RTP payloads list](https://www.iana.org/assignments/rtp-parameters/rtp-parameters.xhtml)), that the code for that payload is [VP8](https://en.wikipedia.org/wiki/VP8) (thus this is video payload) and 9000 is the bitrate. |
| a=         | a=sendrecv | Specifies that we will send and receive. This line is useful because it will allow you to check whether your negociation happened correctly or not |

There are many other keys, you can check them [here](https://en.wikipedia.org/wiki/Session_Description_Protocol).

#### Alright, That Was Boring. What Now?
Now we need to think.  
We need a strategy to map the peers between them on our channel. Let's go back to how we will use our Skype-like application:

- Alice wants to call Bob.
- She connects to the signalling channel server.
- Bob does the same. Now, how can Alice give Bob the parameters? Because they might be lots of other users connected to that signalling server, and you don't want to talk to the wrong person.

It's not WebRTC now, it's pure thinking, a strategy. I am going to show you one implementation. It will be **very simple**, **not scalable** and **not secure**. There, I said it. My purpose is **not** to show you how to build a good signalling channel: this is an entirely different topic. There exists several (more or less good) solutions to signalling, you can perfectly use them: it is (almost) application-independant.  
Here I present you a simple WebSocket-based signalling server that I implemented in Haskell, thanks to [Jasper Van der Jeugt's Websocket Haskell library](http://hackage.haskell.org/package/websockets).

It works as follows:

- it maintains a list of currently connected users
- when you go to the server's home page (which I will show you in a short moment), you are asked for a nickname, once you entered it, it is sent to the server
- when the server receives a new nickname, it sends every connecter users (you included) the list of all connected users (see: **not** scalable :-))
- you can then click on a user's name to initiate the call. At that point, we are back at WebRTC programming.
- Then, when your browser generates the SDP, the candidates (I'm explaning this in a minute) and any other piece of textual data, you will send them to the server, JSON-formatted, with a field indicated the nickname of the peer you are trying to reach.
- The server will simply relay that textual data to the intended peer and after a couple of exchanges (that we will describe, of course), the call will either fail or happen. At this point, we will close the connection to the server for two reasons:
    - to show you that the signalling channel is indeed only used during signnaling and your peero-to-peer connection **is** peer-to-peer
    - (_lazy alert !_) it will prevent us from implementing a system to check if a user is currently in a call and thus cannot be called (though it would fairly easy).

**/!\\** I wrote the signalling server code fairly quickly. The goal was to have something working, so we could focus on the WebRTC part. Don't judge me on that code :-)  
I am including the code here for the curious, but this is not the core of this post.

First, the list of imports:

``` haskell
{-
This example is *greatly* inspired from jaspervdj's github example of his WebSockets Haskell package:
https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

Thanks to him for providing understandable documentation and example.
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.Aeson
import Control.Monad (mzero, forever, forM_)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
```

Nothing fancy here. I chose `Network.WebSockets` library because it is well documented, and I had previously worked with it. We use `Data.Aeson` to deal with JSON data, but really, it is just a matter of parsing. The server itself will be content-agnostic. Again, this is a simple version.

Then we define some types, mainly for signature clarity:

``` haskell
-- For simplicity, a client is just his username
type Client = (Text, WS.Connection)

-- The server will simply keep a list of connected users
type ServerState = [Client]

-- Simple type to define a nickname
data Nickname = Nickname Text
    deriving (Show, Eq)

-- Define JSON instance for Nickname
instance ToJSON Nickname where
    toJSON (Nickname nick) = object ["nickname" .= nick]

instance FromJSON Nickname where
    parseJSON (Object n) = Nickname <$> n .: "nickname"
    parseJSON _          = mzero

-- Simple type that defines the users list, to send the user
data UserList = UserList [Text]
    deriving (Show, Eq)

instance ToJSON UserList where
    toJSON (UserList xs) = object ["userlist" .= xs]

instance FromJSON UserList where
    parseJSON (Object o) = UserList <$> o .: "userlist"
    parseJSON _          = mzero

-- Simple type to define SDP message, used only to provide a JSON instance to map the user it should be sent to
data SDP = SDP
    {
        sdp    :: Text
      , target :: Text
    } deriving (Show, Eq)

instance ToJSON SDP where
    toJSON (SDP s t) = object ["sdp" .= s, "target" .= t]

instance FromJSON SDP where
    parseJSON (Object o) = SDP <$> o .: "sdp" <*> o .: "target"
    parseJSON _          = mzero
```

The `Client` type is just for the server to keep track the list of connected users. The `ServerState` is just the list of `Client`s. In a more detailed implementation, you could be tempted to keep more information about a particular user, but I suggest you don't: WebRTC is about privacy (too) and you should really keep it light.  
The `Nickname` type is simply here to provide a JSON instance. When the client connects to the signalling server, the first thing he does is send his nickname, so he can be added to the contact list.  
The `UserList` data type is built the same: when a new user connects, after sending his nickname, it receives the whole contact list; this type is here for that.  
At last, we will wrap (client-side) all SDP contents within a JSON instance, with a "target" and a "sdp" field. The "target" is for the server to know whom to relay the data to.

We then include some handy functions, mostly for clarity:

``` haskell
-- Initially, the server is empty
emptyServerState :: ServerState
emptyServerState = []

-- Get the number of connected users
numUsers :: ServerState -> Int
numUsers = length

-- Check if a user is connected
isUserConnected :: Client -> ServerState -> Bool
isUserConnected client = any ((== fst client) . fst)

-- Return the connection of the user whose nickname is the parameter
getConnection :: Text -> ServerState -> Maybe WS.Connection
getConnection _ [] = Nothing
getConnection nick (x:xs) | nick == fst x = Just (snd x)
                          | otherwise     = getConnection nick xs

-- Add a user if he is not already connected
addUser :: Client -> ServerState -> Either ServerState ServerState
addUser client state | isUserConnected client state = Left state 
                     | otherwise = Right $ client : state

-- Remove a user from the server
removeUser :: Client -> ServerState -> ServerState
removeUser client = filter ((/= fst client) . fst)

-- Our main function : create new, empty server state and spawn the websocket server
main :: IO ()
main = do
    putStrLn "===== .: WebSocket basic signalling server for WebRTC :. ====="
    state <- newMVar emptyServerState
    WS.runServer "0.0.0.0" 4444 $ application state
```

The `main` function simply creates a new, empty state (empty list, really), that it wraps in an `MVar`. This is so that there is not concurrent modifications. It then spawns the WebSockets server.

Now, the `application` function, which acts on incomming connections: 

``` haskell
-- Application that will do the signalling
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    -- Accept connection
    conn <- WS.acceptRequest pending
    users <- liftIO $ readMVar state
    putStrLn ("New connection!")

    -- We expect the client to send his nickname as a first message
    msg <- WS.receiveData conn -- :: IO Text
    case decode msg of
        Just (Nickname nick) -> flip finally (disconnect (nick, conn)) $ do
            liftIO $ modifyMVar_ state $ \s -> do
                case addUser (nick, conn) s of
                    Right newState -> do
                        putStrLn $ "New user added, now " ++ (show . numUsers $ newState) ++ " connected."
                        return newState
                    Left oldState  ->  do
                        putStrLn $ "User is already connected!"
                        return oldState
            handleUser conn state nick
        _                    -> do
            putStrLn "Wrong data received."
            WS.sendClose conn (""::Text)
    where disconnect c = do
            putStrLn $ "Diconnecting user " ++ show (fst c)
            liftIO $ modifyMVar_ state $ \s -> do
                let newState = removeUser c s
                pushUserList newState
                return newState
```

So here, we accept all incomming connections. This is not very secure, you could check the incomming peer and do other security checks, but here this is sufficient. Our server will expect the client to send his nickname in a JSON-formatted payload, right when the connections opens. This is what the `receiveData` and `case decode msg of` are about. If we receive the nickname, we add it to the state (with `MVar` mechanism).  
We then pass the control to the `handleUser` function, which handles a client/connection until it fails, or closes.

Let's see what `handleUser` does:

``` haskell
-- Process incomming messages from the user
handleUser :: WS.Connection -> MVar ServerState -> Text -> IO ()
handleUser conn state nick = do
    -- Upon new connection, send the new list to every connected users
    users <- liftIO $ readMVar state
    pushUserList users

    -- Then, process incomming message from that client
    forever $ do
        msg <- WS.receiveData conn
        let json = decode msg :: Maybe SDP
        case json of
            Just s  -> do
                let who = target s
                users <- liftIO $ readMVar state
                let c   = getConnection who users
                case c of
                    -- If we found the user, relay the SDP
                    Just co -> WS.sendTextData co (sdp s)
                    -- If we did not find the user, close the connection (very poor error handling)
                    Nothing -> WS.sendClose conn ("" :: Text)
            Nothing -> do
                putStrLn "Did not get SDP"
```

As we can see from the first two lines, the very first thing it does is send the user the contact list so that you can know who is connected -thus, who you can call.  
It then loops on receiving messages. In our (simple) implementation, we don't allow any other payload to transit between the peers, only the SDP thing. The goal is to keep the connection to the signalling server as short as possible: clients connects, they negociate call parameters and initiate it. Then it disconnects from the server and everything happens peer-to-peer, encrypted.

Really, all that code about the signalling server can be summed up to that piece of code:

``` haskell
case c of
    -- If we found the user, relay the SDP
    Just co -> WS.sendTextData co (sdp s)
```

All the signalling server does is _relay_ the data from peer A to peer B, that data being SDP parameters. All the other pieces of code around that are just establishing connection, and finding a way to map the peers. This is _all_.

**/!\\ Notes:** in **every** sensible signalling server implementation, all communications should be **encrypted**. In our case, we should use `https` for presenting the web page, and use the `wss` WebSockets encrypted protocol rather than `ws` (this last 's' stands for "secure", in case you missed it). Again, here I did not bother using encryption, because this is simply a test server.

### Peer Discovery
Alright, so back to "real" WebRTC stuff now. Let's take a look at what we have now:

- We have acquired our media stream: the (live) video from the webcam.
- We have set up (or chosen, if you don't want to code it yourself) a signalling server to negociated parameters with our peer.

Then we have a problem: we have a way of exchanging informations with our peer, but we don't know _where_ he is. And that is the major problem with peer-to-peer. It is easy to reach central server thanks to static IP address and DNS. But a peer's IP address is likely to change over time, and you don't know on which port he is listening to.

That is easily solved: we went to all the troubles of setting up a signalling server _precisely_ for the peers to exchange this kind of information, so just ask him and he will tell you.  
Yes, that is (almost) true. Now let's consider this: _you (probably) don't know your IP address_, so you can't (yet) tell the other peer.  
Most of the time, you will be behind a router. So you have a _local_ IP address: your Internet packets go to your rooter and only your rooter is visible from the outside (only he has a `public` IP address).

#### STUN server
This is where a STUN server will come useful. STUN means "Session Traversal Utilities for NAT".  
Before you begin telling me that it is yet another central sever (because it is indeed) let me tell you how _minimal_ (yet _crucial_) a role it has. A STUN server has only **one** purpose: tell you your IP address.  
If you ever had to Google "what is my ip address" and click on the first link so that the website can tell you your IP address, well you've done the manual equivalent of a STUN request.

It works as follows: when you issue a STUN request, it leaves your computer to your rooter. Then your rooter updates its NAT table and forward the request to the STUN server you requested.  
On its side, the STUN server sees an incomming connection, it has access to the sender (you)'s IP address and port which it simply echoes back.  
Upon returning, the request reaches your rooter which looks at its NAT table and "remembers" that the incomming request should be rooter to your computer (and not your sister's computer, your smartphone or your printer which are all on the same network).  
_We now have a path to the exterior, **which we know**_.

I have good news: STUN servers are so lightweight and consume so few resources, that Google keeps a public STUN server that you can use for your applications. The address is: `stun:stun.l.google.com:19302`.

Let's see now how we can use ICE in Javascript.

#### The ICE framework
[ICE](https://www.wikiwand.com/en/Interactive_Connectivity_Establishment) stands for "Interactive Connectivity Establishment". It is a framework that helps to establish a peer-to-peer connection.

You have actually several ways to contact your peer: if he is on the same network, the data packets should transit through that network directly: useless to go on the Internet then come back. Besides, you might have several network interfaces: two wired connections (eth0 and eth1) and one wireless connection (wlan0) for instance. How would you connect, then?  
This is what the "ICE Agent" is here for. And this is the notion of "ICE candidates". Formally, a "candidate" will be a (IP address, port number) pair.

The Javascript ICE agents are well implemented and very easy to use: it generates an event `onicecandidate` every time it discovers a candidate. This is very neat: just listen on that event and you can send the candidate to your peer.

#### The RTCPeerConnection API
Ah! Now we are getting at something.  
That API is the **core** of the WebRTC API, that's the real connection to the outside. This is the object that represents your connection to your peer. It contains the ICE Agent that we just saw.

Let's describe a few of its aspects. **Attention:** this part is important as here lies the majority of your code. We are directly dealing with WebRTC here, so pay attention, and get ready: this is getting interesting.

It is instanciated with: `var pc = new RTCPeerConnection();`. You can pass a parameter containing the addresses of your STUN and TURN servers (scroll down a few lines for a quick word about TURN servers).  

Some properties of the `RTCPeerConnection` object that are useful: 

- `iceConnectionState`: tells you how you connection is, at the moment. Whenever it changes, it is supposed to emit a `iceconnectionstatechange` event. The return value is a `RTCIceConnectionState` and can be:
    + `new`: just created, waiting for candidates to become available
    + `checking`: the agent has at least one candidate, but still no valid connection.
    + `connected`: has found a valid connection it could establish. It will continue checking for better candidates.
    + `completed`: candidates have been checked and the best one is currently in use, the connection is established and valid.
    + `failed`: the agent was not able to find a valid candidate
    + `disconnected`: when a connection is established and used, periodic "liveness" checks are issued to monitor the network. The status is `disconnected` when one of such checks failed. It can be temporary: sometimes to network is simply messed up for a second.
    + `closed`: the ICE agent has shutdown.
- `iceGatheringState`: tells you where the agent is with respect to candidate checking.
    + `new`: just created, nothing was done yet
    + `gathering`: the agent is currently gathering candidates
    + `complete`: the agent finished getting all candidates

That was about ICE. You can (but you don't _have to_) monitor these events. They give you information on what's going on.  
Note than when I say "is supposed to" it means that the documentation says so, but some browsers don't do that. Be careful to test individual events and functions before relying on them.

Then come some events for buiding your WebRTC application:

- `onaddstream`: called when your remote peer adds a stream (audio, video, or both) to the connection. You have to listen for this even if you want to be able to display your peer's webcam.
- `ondatachannel`: same thing, but when your peer adds a data channel. Remember that WebRTC is about transmitting real time of video and audio streams (webcam in our case)? Well you can transmit any raw data you want, this is done through a `RTCDataChannel`.
- `onicecandidate`: called when your ICE agent finds a new candidate. You have to listen to that event so that you can grab these candidates and send them to your peer.

These were the main events to listen for. Check the documentation for others.

And now the methods, you will have to call them to build your application:

- `createOffer()`: the **caller** calls that functions. It is the _starting point_ of any WebRTC application. It generated the very first SDP.
- `createAnswer()`: well... that's the same, but the **callee** calls that functions.
- `setLocalDescription()`: called by both peers. When you generate **your** description (with one of the two previous methods)
- `setRemoteDescription()`: again called by both peers. When you receive your peer's description.
- `addIceCandidate`: this is the function you will use when your ICE agent finds a new candidate, to make this candidate available to your WebRTC application.
- `addStream()`: when your multimedia stream becomes available (you accepted your webcam use, in our case) you have to add is to your `RTCPeerConnection` so that it will trigger the `onaddstream` event in your peer (and so he can access and display it).

**Important**: this is quite honestly hidden too deep in the documentation, but I'll say it here: the whole ICE process (gathering candidates, etc) will not begin before you called `setLocalDescription()`. You might spend some awful lot of time trying to debug your WebRTC application with `console.log([insert insulting debugging messages here])` and the answer might just be that: `setLocalDescription()` is not called (at the right time). So keep that in mind.

### Some Code Now, Please
Is this the moment? The one?  
Yes it is! Enjoy!

#### The Main Page
It is basically the same page as the previous page, we will only add a section to display the two videos (ours and our peer's). And for convenience, we will add a bit of Javascript to hide the signalling stuff once we established connection. It does like this:

```html
<!doctype html>
<html>
    <head>
        <meta charset="UTF-8">
        <title>Signalling Server</title>
        <link rel="stylesheet" type="text/css" href="2_skype-like.css">
    </head>

    <body>
        <!-- This section is visible only during signalling -->
        <section id="signalling">
            <h1>Signalling Server</h1>
            <p>
                <strong id="nickname"></strong>, you are now <span id="status"><strong class="red">disconnected!</strong></span>
                <br>
                <strong id="calling_status"></strong>
            </p>

            <h2>List of currently connected users</h2>
            <ul id="userlist">
            </ul>
        </section>

        <!-- This section is visible only during a cal -->
        <section id="oncall">
            <div class="video_frame">
                <!-- Be careful to use 'autoplay'! -->
                <video id="localVideo" autoplay></video>
                <br>
                <span>Local Video</span>
            </div>
            <div class="video_frame">
                <!-- Be careful to use 'autoplay'! -->
                <video id="remoteVideo" autoplay></video>
                <br>
                <span>Remote Video</span>
            </div>
        </section>
        <script src="2_skype-like.js"></script>
    </body>
</html>
```

So, nothing too fancy here. We have the same thing as previously, to display the list of currently connected users, and the new part is the block with id `oncall`. We included two `<video>` tags to display both _our_ video and _our peer's_, Skype-like!  
Please note that the `<video>` have `autoplay` attribute. This prevents us from forgetting to call `video.play()` in Javascript: it can cause headache. Some web devs are against HTML videos to autoplay, and I'm among them: how annoying it is to visit a page and have some video starting somewhere. But in this case, I believe this makes sense.

I'll include the css below, but there's very little in it :

```css
.red {
    color: red;
}

.green {
    color: green;
}

.video_frame {
    text-align: center;
    display: inline-block;
    vertical-align: top;
}

video {
    border: 1px solid black;
}

#oncall {
    display: none;
}

#calling_status {
    font-size: 1.2em;
    color: blue;
}
```

Okay, the real, interesting part is the Javascript.  
So, we wrap up everything in an event, that waits for the DOM to be loaded. It roughly corresponds to jQuery's main function, although the latter performs more checks. I found that it was a bit overshoot to depend on jQuery for our simple application, so I won't use it.

```javascript
document.addEventListener("DOMContentLoaded", function(event) {
    // Everything (Javascript-related) will be placed here, from now on
}
```

So we begin by defining some variables that we will use all along:

```javascript
var nickname = prompt("Enter a name for the contact list");

if (nickname === null || nickname === "") {
    alert ("You must enter a name, to be identified on the server");
    return;
}

// Will hold our peer's nickname
var peer = null;

// Here we are using Google's public STUN server, and no TURN server
var ice = { "iceServers": [
            {"url": "stun:stun.l.google.com:19302"}]
          };
var pc = null; // This variable will hold the RTCPeerConnection

document.getElementById('nickname').innerHTML = nickname;

var constraints = {
    video: "true",
    audio: "true"
};

// Prevent us to receive another call or make another call while already in one
var isInCall = false;

// Specify if we have to create offers or answers
var isCaller = false;

var receivedOffer = null;
```

The `nickname` is important, because this is the name that will be sent to the server, to maintain a contact list.

**Warning**: as you can see here, I only check if the user actually entered a nickname, but I don't check for some kind of format validity nor unicity in the server.  
It is obvious that you _should_ perform both of these checks in any serious application.

We then define a configuration variable, `ice`, which holds the host information about the STUN server that we will use. As you can see this is Google's public STUN server. This is fine for tests. But I advise that you use one of your own for production (besides, there are several easy-to-implement solutions; a STUN server is really not much).  
The variable `pc` is our entry point to manipulate WebRTC. This is our socket/handle to send and talk to the other peer.  
And then we define a set of contraints for requesting the media, here, I'll keep it simple by using both audio and video (again, Skype-like). You can obviously play a bit with these settings, but keep in mind that Firefox and Chrome treat constraints diffently. By the time I am writing this article, Chrome has been updated and now complies with the documentation and will try to honour your constraints, but Firefox still doesn't. To be clear, Firefox is okay with `true` and `false`, but you can't chose the video's width and height.  
Then `isCaller` will specify if we are the one who initiated the call or if we received the call. This has some importance in the the order in which we call functions.  
At last, `receivedOffer` will contain the offer sent by the caller (this is used when you are the callee), you'll see in a minute why.

The next piece of code is temporary (hopefully). As of now, WebRTC is still a new technology and browser manufacturers still use prefixed functions rather than the names given by the documentation. So the next few lines are here to make the calls portable accross browsers:

```javascript
// For portability's sake
    navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;
    window.RTCPeerConnection = window.RTCPeerConnection || window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
    window.RTCSessionDescription = window.RTCSessionDescription || window.mozRTCSessionDescription || window.webkitRTCSessionDescription;
    window.RTCIceCandidate = window.RTCIceCandidate || window.mozRTCIceCandidate || window.webkitRTCIceCandidate;
```

- `getUserMedia` is to request media (video and sound) to the user, we've talked about this earlier.
- `RTCPeerConnection` is to create the WebRTC connection with the other peer.
- `RTCSessionDescription` is to handle the remote and local session description, we'll see it in use in a few moment.
- `RTCIceCandidate` will be used when we deal with the ICE Agent, to send our candidates to our peer.

From now on, rather than explaining the code in the order it appears in the file, I'll describe it the way I have built it, following logic. I find it makes much more sense and helps to focus. A the tend of the article, I'll provide a link to the code in the repo, so you don't have to worry about the order.

So, first, let's open a WebSockets connection to our signalling server:

```javascript
// Open a connection to our server
var socket = new WebSocket('ws://192.168.1.35:4444');

// Display an error message if the socket fails to open
socket.onerror = function(err) {
    alert("Failed to open connection with WebSockets server.\nEither the server is down or your connection is out.");
    return;
};

// Provide visual feedback to the user when he is disconnected
socket.onclose = function (evt) {
    document.getElementById("status").innerHTML = "<strong class=\"red\">disconnected!</strong>";
};

// When the connection is opened, the server expects us to send our nickname right away
socket.onopen = function() {
    document.getElementById("status").innerHTML = "<strong class=\"green\">connected!</strong>";
    socket.send (JSON.stringify ({"nickname": nickname}));
};
```

As you are probably aware, WebSockets hosts begin with `ws://`. The `onerror` handler event occurs if the connection is refused. The `onclose` event is fired when the connection is closed at some point. Note that `onerror` does call `onclose`. I simply write a visual feedback for when we are disconnected.  
When the connection is opened successfully, the servers expects us to send our nickname immediately, this is what is done here.

So we have our conneciton opened to the server and we have sent our nickname, we will get registered and the server will send us (and every connected user) the contact list, so this is the first piece of code we'll write for when receiving a message:

```javascript
// Parse message, JSON is used for all message transfer
try {
    var dat = JSON.parse (msg.data);
} catch(e) {
    console.log ("ERROR - Received wrong-formatted message from server:\n" + e);
    socket.close();
    isInCall = false;
    isCaller = false;
    return;
}

// Process userlist : display the names in the contact list
if (dat.userlist) {
    var l = dat.userlist;
    var domContent = "";

    // Add each user on the list and register a callback function to initiate the call
    l.forEach (function (elem) {
        // Filter out our name from the list: we don't want to call ourselve!
        if (elem !== nickname) {
            domContent += "<li><button onclick='navigator.callUser(\"" + elem + "\");'>" + elem + "</button></li>";
        }
    });

    // Add the generated user list to the DOM
    document.getElementById("userlist").innerHTML = domContent;
}
```

We use [JSON](https://en.wikipedia.org/wiki/JSON) for all data exchange, so the first thing we do when we receive a message is try to parse it (this is done in the `try`/`catch` block).  
First test in the `onmessage` handler is if we received the userlist from the server. In this user list, there is quite simply the list of all nicknames currently connected (including ours, hence the condition to exclude ourselves from the list), then we build a HTML list (`<ul>`) in which we add the contacts with a button. On each contact button, the `navigator.callUser()` event is bound.

So the logical next step now is to see what this `callUser()` function does: 

```javascript
// Initiate a call to a user
navigator.callUser = function (who) {
    document.getElementById('calling_status').innerHTML = "Calling " + who + " ...";
    isCaller = true;
    peer = who;
    startConv();
};
```

One _particular_ thing I want to emphasize is that in our design, the page (hence the Javascript code) is the same for the caller and the callee; but the functions to call (especially their orders) are different for the caller and the callee. For this to work, and to write beautiful, elegant code, we will make extensive use of functions and conditions on whether we are the caller (checked with `isCaller`) or not.  
So what do we do here ?  
First we simply give the user some visual feedback that _something_ is happening by writing "Calling XXX...". Then, since we are the one to call, we set the boolean `isCaller` to `true` (it is `false` by default).  
After that we simply register our peer's name in the `peer` variable, which is available globally: we do that because we will need it later.  
And then we call `startConv()`, the function which will, obviously, start the conversation.

Let's take a look at that function. This function (as many others) will actually be called by both peers, so we have to check if we're calling it because we are initiating a call or because we are answering one:

```javascript
// Start a call (caller) or accept a call (callee)
function startConv() {
    if (isCaller) {
        console.log ("Initiating call...");
    } else {
        console.log ("Answering call...");
    }

    // First thing to do is acquire media stream
    navigator.getUserMedia (constraints, onMediaSuccess, onMediaError);
}; // end of 'startConv()'
```

What we need to do to _start a conversation_ is create a channel between the peers (the `RTCPeerConnection`), acquire media and transmit it.  
**/!\\ Attention: WebRTC dirt here**  
It turns out that for your WebRTC application to work, you have to add your media stream (with `pc.addStream()`) **BEFORE** setting your local description (and idem for the callee). I call this **dirty** because I have yet to find a _good_ explanation of why this is needed, and because I don't recall the documentation to ever specify that...

Anyway, back to our code sample. the first few lines are just debugging stuff, you can omit them. It simply outputs on the console if we are _making_ a call or _answering_ one. As usual, it might be a good idea to display visual (or audio) feedback to the user, like a phone ringing or a picture of a phone shaking; universal signals that we are placing a call.  
The last line is where the real fun begins: since I've told you we had to add the stream before doing anything else, we call `getUserMedia()` to get the media stream. In case of error, `onMediaError()` is called:

```javascript
function onMediaError (err) {
    alert ("Media was denied access: " + err);
    document.getElementById('calling_status').innerHTML = "";
    socket.close();
    isCaller = false;
    isInCall = false;
    return;
};
```

which simply notifies the user that the access to the webcam was denied (or it failed for whatever else reason).

When it (hopefully) succeeds, `onMediaSuccess` is called. Again, this process of acquiring stream is a common task between the caller and the callee; this is why

1. we define an external function, called `onMediaSuccess` and use it as a callback rather than using an anonymous function
2. inside this callback, we check on `isCaller`

So here is the caller part of `onMediaSuccess`:

```javascript
function onMediaSuccess (mediaStream) {
    // Hide the contact list and show the screens
    document.getElementById("signalling").style.display = "none";
    document.getElementById("oncall").style.display = "block";

    // Display our video on our screen
    document.getElementById("localVideo").src = URL.createObjectURL(mediaStream);

    // Create the RTCPeerConnection and add the stream to it
    pc = new window.RTCPeerConnection (ice);

    // Stream must be added to the RTCPeerConnection **before** creating the offer
    pc.addStream (mediaStream);

    pc.onaddstream = onStreamAdded;
    pc.onicecandidate = onIceCandidate;

    if (isCaller) {
        // Calling 'createOffer()' will trigger ICE Gathering process
        pc.createOffer (function (offerSDP) {
            pc.setLocalDescription (new RTCSessionDescription (offerSDP),
                                    function () {
                                        console.log ("Set local description");
                                    },
                                    function () {
                                        console.log ("Failed to set up local description");
                                    });
        },
                        function (err) {
            console.log ("Could not build the offer");
        }, constraints);

    }
```

As we've seen in the mirror example, the success callback is passed the `MediaStream` object.  
First, there _are_ a couple of common tasks to do for both the caller and the callee: we first hide the HTML part that displayed the user list (since we are in a call, we won't be calling someone else, so we might as well hide it) and show the "on call" part of the window, the one with the two screens.  
Then, since we now have our local stream: our pretty face (or your sister under the shower, if you have a wireless webcam... and you're a pervert - _don't do that by the way_), we can just show it. This step was already discussed in the mirror example so there should not be anything new.

It is now the time to instance our `RTCPeerConnection` and make store it in the globally available `pc` variable. Remember our STUN server's IP address (stored in the `ice` variable)? Well if you want to use it, you shall pass it as a parameter.  
Okay now that the `RTCPeerConnection` is created, _the very first thing we do now_ is add our stream (since WebRTC _silently_ requires so); this is done with `pc.addStream()`.  
Then we define the callbacks for `pc.onaddstream()` and `pc.onicecandidate()` events. The former will be triggered when our peer will itself call `pc.addstream()` and the latter is fired whenever our ICE Agent will gather candidates.

**/!\\ Attention:** if you're like me and usually write a few lines of code/ functions (with debugging `console.log()` calls), then try it to see what's happening (in your console), **do not stop here**. I tried it, and obviously spent time trying not to bang my head against the wall. I believe I said it earlier but nothing will happen before we called `pc.setLocalDescription()`! Now see how ironic this is? In order to get some results, you would be tempted to create your `RTCPeerConnection`, then register the callback and call `setLocalDescription()`? You _would_ have something on the console now, but it would eventually fail because _you need to add your stream beforehand_.

Now comes the dependant part. We are the caller in this case, so what we need to do now, is create the offer, this is done with `pc.createOffer()`. Since the recent WebRTC 1.0 review, the API slightly changed. Most of the functions now take a success and an error callback (which are mandatory; well stricly speaking they are not _yet_, but calling these functions without the callbacks triggers the browser to display a warning in the console, telling you that soon, it would result in an error, thus breaking yor code. This is quite new actually, so you might be surprised if you have already read some WebRTC code example before and did not see callbacks). In addition to the success and error callbacks, some functions take an additional `MediaConstraints` object as the third parameters. Quite frankly, I don't really understand why, since the contraints are already available inside the `RTCPeerConnection`.  
The function `createOffer()` follows that rule. Our error callback is simply a log on the console.  
The success callback is passed the offer that was successfully just created.

If you want to inspect it, you can call `console.log (JSON.stringify (offerSDP))`. You would see that it is a JSON-formatted object which contains two fields: "type" which can be "offer" or "answer" and "sdp" which contains the actualy SDP information, namely the IP address + port of your interface, the codecs available, etc. It would be pretty small for now, since we haven't gathered any candidates yet.

It is time to _actually_ start the whole WebRTC procedure by calling `setLocalDescription()`. You need to give him a `RTCSessionDescription` which you can build inline from a the SDP offer we have just created. This function, too, comes with a success and error callback (which we only use here to notify the user on the console.)  
If you read some WebRTC examples elsewhere, you might see some people _sending the offer_ in the success callback of `setLocalDescription()`. It is called **trickle ICE**. Let me explain quickly the difference:

What we are going to do in this application might be summarized like this: 

1. create the offer and set it as the local description (it will then trigger the ICE Agent which will start gathering candidates)
2. monitor this ICE Agent; when it is done gathering candidates, we will send our peer our local description which will contain everything in one batch: the codecs we support, the media we are ready to transmit and receive as well as all our candidates (remember a candidate is bound to our network interface, and contains protocol (UDP or TCP), IP address, port number, etc)
3. our peer will then receive that offer, inspect it, create its answer from that and send it back to us, then the conversation will begin with the best parameters that can be supported by both peers.

This is _text-book_ WebRTC. This is what the documentation specifies and this is what is supported by all browsers. But this is not the best way to do WebRTC: you have to wait for the ICE Agent to finishing gathering _every_ candidates before you can send your offer and, then, you need to wait for your peer to gather himself all its candidates. This takes time.  
What you might see is "trickle ICE". The principle is different:

1. create the offer, set it as the local description and _immediately_ send that almost empty offer to our peer.
2. when our peer receives the offer, it will do the same and immediately send his answer.
3. during that time, the ICE Agent would have started gathering candidates, everytime one candidate is found, we send it to our peer. This new candidate will be inspected by the WebRTC engine and if it is better, the communication will be transparently switched to use that new candidate (for instance if the first one was TCP, pretty long delays so bad quality and the new candidate uses UDP, which is faster, the conversation will then use this new UDP candidate) otherwise it will stay the same.  
People who use trickle ICE usually send their offer as soon as it is created, in the success callback.

This trickle ICE procedure has some benefits: the conversation can start immediately, usually in low quality / low bitrate and can be then enhanced when better candidates are found.  
The downside of this is that this is not defined in the documentation, so browser don't have to implement trickle ICE. Back before some recent updates, I believe Firefox did not support trickle ICE. I am unsure now. So for this article, I prefer to stick to the documentation: this is why we don't do anything in the success callback.

Okay so what now? We have covered everything in that `onMediaSuccess()` callback. Well since we called `setLocalDescription()`, the ICE Agent has started gathering candidates, and everytime it finds a candidate, it calls our `onIceCandidate()` callback. Let's inspect it:

```javascript
function onIceCandidate (evt) {
    // Wait for all candidates to be gathered, and send our offer to our peer
    if (evt.target.iceGatheringState === "complete") {
        console.log ("ICE Gathering complete, sending SDP to peer.");

        // Haven't found a way to use one-line condition to substitute "offer" and "answer"
        if (isCaller) {
            var offerToSend = JSON.stringify ({ "from": nickname,
                                                "offer": pc.localDescription
                                            });
            socket.send( JSON.stringify( {"target": peer, "sdp": offerToSend}));
            console.log ("Sent our offer");
        }
```

As said previously, we don't use trickle ICE here, so we need to wait for the ICE Agent to finish gathering all the candidates. The state of the gathering process, if you recall from an earlier paragraph, is given by the `iceGatheringState` properties of the ICE Agent.  
The callback is given an `Event` (here `evt`). So in order to access the ICE Agent, we need to call `target`, which is the object that is responsible for the event. I mention it here because it is easy to forget it and write `evt.iceGatheringState` and get insulted by Javascript.  
So we simply wait for the last candidate to be gathering, which is indicated by the state being on `complete`.

That gathering process will be done by the callee too, so here is the time to check if we are the caller or the callee. As the caller here, we will send our offer.  
Remember how we built our signalling server? We send the server an JSON object which contains exactly two fields: "target" to say whom we send our data to, and "sdp" which contains our data. I admit now that "sdp" is ill-chosen: we don't always send an SDP, but it is just a matter or name. We build our data, since it will be transmitted to our peer, we need to JSON-format it too (the signalling server will simply fetch whatever is inside the "sdp" field and send it to our peer, so what is inside the "sdp" field **must** be JSON-formatted).  
In that data, we indicate that the data comes from us with the "from" field (this is actually the first time the callee will know it is being called, and by whom) and the actual data which is a SDP (really SDP this time) offer. This offer is obtained from our `RTCPeerConnection`, with the `localDescription` attribute.

Now what happens?  
Well it is time to go check what happens to our peer. It will receive our offer, so let's inspect a second chunk of code from the `onmessage` **WebSockets** handler (remember, the first one was to check if we received the userlist form the server):

```javascript
if (dat.userlist) {
    // We already did that part, this is just to remind the context
}

// If the message is from a peer
else if (dat.from) {
    // When we receive the first message form a peer, we consider ourselved in a call
    if (!isInCall) {
        isInCall = true;
        peer = dat.from;
        document.getElementById('calling_status').innerHTML = peer + " is calling...";
    }

    if (dat.offer) {
        receivedOffer = dat.offer;
        startConv();
    } else if (dat.answer) {
        // We will see that in a few moments
    }
}

// Otherwise, this is an error
else {
    alert ("Received a non-intended message.");
    socket.close();
    isInCall = false;
    isCaller = false;
    return;
}
```

If the message doesn't contain the `userlist` attribute (which would indicate it comes from the signalling server) it must contain the `from` attribute, which indicated it comes from a peer, this is our `else if`, otherwise, simply fail.  
Well first thing we do is store our peer's name! We make it available in the `peer` variable and set the `isInCall` boolean to true. Then.. visual feedback: so we now we are being called.

Okay, time to pay attention here. We just received an offer from our peer, so we have to start all the WebRTC mechanism (remember: we are on the callee's side now). But the same rule applies: we need to add our stream to the `RTCPeerConnection` before we do anything else. This is why we store the offer we just received in the global `receivedOffer` variable. Then we call `startConv()`, like we did when the caller initiated the call. If you remember, that function was not much different for the caller and the calle, we just had a test case to write "Answering call..." rather than "Initiating call..."

```javascript
function onMediaSuccess (mediaStream) {
    // Create the RTCPeerConnection and add the stream to it
    pc = new window.RTCPeerConnection (ice);

    // Stream must be added to the RTCPeerConnection **before** creating the offer
    pc.addStream (mediaStream);

    pc.onaddstream = onStreamAdded;
    pc.onicecandidate = onIceCandidate;

    if (isCaller) {
        // We alreayd saw that part for the caller

    } else {
        pc.setRemoteDescription (new RTCSessionDescription (receivedOffer),
                                 function () {
                                    pc.createAnswer (function (answerSDP) {
                                        pc.setLocalDescription (new RTCSessionDescription (answerSDP),
                                                                function () {
                                                                    console.log ("Set local description");
                                                                },
                                                                function () {
                                                                    console.log ("Failed to set up local description");
                                                                });
                                    },
                                                    function (err) {
                                        console.log ("Could not build the answer");
                                    }, constraints);

                                 },
                                 function () {
                                    console.log ("Failed to set up remote description");
                                });
    }
}; // end of 'onMediaSuccess()'
```

First part is common, we already saw it: we have the user's webcam stream available, we created our `RTCPeerConnection`, **we added our stream to it** (ah?! now that will trigger the `onaddstream()` event in our caller, I'll come back to this in a minute) and we registered our callback for the ICE Agent. Now what happens in that `else` case?  
Well now that we added our stream to the `RTCPeerConnection`, we can set our remote description.

Just to be sure everybody follows: we are on the _callee's_ side now, so the description we just received from the _caller_ (which was his _local_ description) is for us, the _remote_ description. Similarly, our _local description_ will become his _remote description_ when we will have sent it, right?

Okay now that it is clarified, let's move on. `setLocalDescription()`, as we saw it takes callbacks and constraints. In our success callback, we can do something, now that we are on the callee's side. It is time we create our SDP answer, done with `pc.createAnswer()`. Exactly like seen previously, if you inspect the object passed to the success callback, you will find the "type" field set to "answer" and an almost empty "sdp" field.  
Inside that callback (yes, it's Inception here: we are inside `setRemoteDescription()`'s sucess callback, we called `createAnswer()` and we are now inside its own callback, and now there will be a third success callback...  
With our answer successfully created, we need to set our own local description. Phew!

What now?
Before continuing with the callee, let's not forget that since we called `pc.addStream()`, the `onaddstream()` event will be triggered in our caller, let's take a quick look:

```javascript
function onStreamAdded (evt) {
    console.log ("Remote stream received");
    document.getElementById("remoteVideo").src = URL.createObjectURL(evt.stream);
}; // end of 'onStreamAdded()'
```

This event is common to both the caller and the callee, so it is quite simple: we simply take our media stream, transform it into something that can be plugged into a HTML `<video>` `src` attribute and it's done. (Don't forget to call `play()` if you omitted the `autoplay` attribute).  
Oh and by the way, on the callee's side, the `onaddstream()` event was fired as soon as we called `setRemoteDescription()`: this is what actually connected our `RTCPeerConnection` with the caller's.  

Okay, back to the callee. Since we called `setLocalDescription()`, the callee's ICE Agent started to work, let's take a look at the callee's part of the ice callback:

```javascript
function onIceCandidate (evt) {
    // Wait for all candidates to be gathered, and send our offer to our peer
    if (evt.target.iceGatheringState === "complete") {
        console.log ("ICE Gathering complete, sending SDP to peer.");

        // Haven't found a way to use one-line condition to substitute "offer" and "answer"
        if (isCaller) {
            // already covered in the caller part
        } else {
            var answerToSend = JSON.stringify ({ "from": nickname,
                                                "answer": pc.localDescription
                                            });
            socket.send( JSON.stringify( {"target": peer, "sdp": answerToSend}));
            console.log ("Sent our answer");
            // Once we sent our answer, our part is finished and we can log out from the signalling server
            socket.close();
        }
    }
};
```

This should comme as no surprise: this is exactly the same code than we used for the caller, except that we now send a JSON object with the name "answer".  
If you followed precisely when I was talking about the generated SDP that contained a "type" field whose value was either "offer" or "answer", you would note that we could simplify this code by creating only one JSON object, and call the field "data" or "sdpData" rather than "offer" and "answer". Then, in the `onmessage()` handler, the test would not be performed directly on `dat.offer`/`dat.answer` but rather `dat.sdpData.type`.  
Well done if you picked this. Oh and don't think "Yeah I would've picked it", either you did, or you did not.

Last but not least, once the callee sent his answer, there is nothing else that will be transmitted through the signalling server, so we just disconnect from it. From now on (well after a small remaining step on the caller's side), everything will be transmitted peer-to-peer, with WebRTC (so, securely).

Now it's time to go see that `onmessage()` handler in the caller, because we just received an answer!

```javascript
// Process incomming messages from the server, can be the user list or messages from another peer
socket.onmessage = function (msg) {
    // Parse message, JSON is used for all message transfer
    try {
        var dat = JSON.parse (msg.data);
    } catch(e) {
        // error is not JSON-formatted
    }

    // Process userlist : display the names in the contact list
    if (dat.userlist) {
        // we already saw the case for when we receive the user list
    }

    // If the message is from a peer
    else if (dat.from) {
        // When we receive the first message form a peer, we consider ourselved in a call
        if (!isInCall) {
            // not relevant here
        }

        if (dat.offer) {
            // this is the callee
        } else if (dat.answer) {
            pc.setRemoteDescription (new RTCSessionDescription (dat.answer),
                                     function () {
                                        console.log ("Set remote description - handshake complete.");
                                        // As we are now in a call, log out from the signalling server
                                        socket.close();
                                     },
                                     function () {
                                        console.log ("Failed to set remote description - handshake failed.");
                                     });
        }
    }

    // Otherwise, this is an error
    else {
        // error if not one of the two cases that we handle
    }
}; // end of 'socket.onmessage()'
```

It should also come as no surprise: we receive an offer from our callee, it contains the SDP that we need to use in `setRemoteDescription()`. Once this is done, the WebRTC handshake is officially completed!  
Same as the callee: we have no business anymore with the signalling server, so we might as well close it.

Side note: on tests, while examining the console, I noted that `"Set remote description - handshake complete."` was indeed logged once, but that several `"Set remote description - handshake complete."` were logged too (on the caller's console), while several `"Sent our answer"` (from `onIceCandidate()`) were logged in the callee's console. So for some reason that I have yet to understand, the callee sends several times its answer and thus the caller receives it several times too. I'm not sure why. I'll try and come back to you when I found the reason.

## Conclusion
And tada!  
Now you should have a basic, Skype-like application in your browser. It comes with comparable bitrate and quality (especially since the new WebRTC 1.0 review, where H.264 can be used as a video codec, VP8 was the only one before). The media is truly peer-to-peer, so your media data flow from your computer to your peer's, with no proxy (see the following paragraph on that note).  
I hope you enjoyed reading the article, and that I was clear enough. I am aware this is a pretty long article, but I did not want to simply give three lines of codes and "voilà!". I wanted to give you a sense of what was happening _under the hood_ and **more importantly** I wanted to write down the tricky parts like the fact that you have to add the stream to the `RTCPeerConnection` before calling `set{Local,Remote}Description`, because it is often omitted in the code examples that you might have seen on the web.

[Any feedback is appreciated!](mailto:ns.schoe@gmail.com)

### What we did
In this simple application, we used the most talked about feature of WebRTC: peer-to-peer _media_ communication. We created a simple Skype-like application to transmit video and audio data between two peers.  
We used `WebSockets` as the signalling channel, remember that the WebRTC documentation voluntarily does not provide/impose a technology for the signalling channel. I used WebSockets because I am quite familiar with it, it works well and the WebRTC's API was built to look like WebSockets'.  

To make a quick reminder, we used a STUN server to discover our public IP address and ensure NAT traversal. Our ICE Agent took care of gathering network candidates. We notified the person whom we wanted to call via a common signalling channel, through which we negociated the parameters necessary to initiate a call and then we issued that WebRTC call.  
After the WebRTC handshake was completed, we cut conection with the signalling channel to rely only on WebRTC.

### Note about TURN servers
Google's statistic say that, even with using STUN servers to ensure NAT traversal, about 8% of WebRTC connections fail. This is unlikely to happen to your home network, but it appears most likely in companies where a tight security is kept on the network, which prevents the NAT the be bypassed. For this case scenario, WebRTC provides an answer: a TURN server (yes, you read that right, **T-U-R-N**, and not STUN).

A TURN server is a **centralized** server which will act as a relay between the two peers. When they can't establish contact with a UDP connection, the two peers will establish TCP connection with the TURN server which will relay date between the two.  
To use that feature, you have to include the TURN server's IP adress, port number and credential information into the `ice` parameter that is passed when constructing the `RTCPeerConnection` object.  
If that solution has the advantage of ensuring connection, is has the disadvantage that it is not a truly peer-to-peer connection, since the TURN server acts as a relay. I did not cover in this article, but this is something you might want to document about.  
There are several companies on the web that will provide you with credentials to use for TURN servers, but you will most likely have to pay; while a STUN server is pretty easy to implement and can support a lot of connections (it's really nothing more than a "ping" server) you will find some public ones, a TURN server is another story. It has to implement all WebRTC capabilities and will fail to support heavy loads : we are talking about continuous video and audio streaming. You can't have a public TURN servers because for more than a few users connected simultaneously, it would break down.

### A note about multi-cast
WebRTC is truly an amazing technology, but it had one "problem". Well this is not a problem, but this is one thing your need to know before thinking you can conquer the world. What we did it one-to-one media conversation. If you try it, you'll see that it works pretty fine. If you and your peer have a pretty decent connection, you should have good quality. But don't forget that uplinks are generally much slower (often by an order of magnitude) than downlinks. So if you want to stream you full HD Webcam to your peer, you'd better have a decent uplink!  
Now consider multi-cast (_i.e._ one-to-many, many-to-one or many-to-many). If you want to make a conversation with three people, A, B and C.  
One of the very good aspects of WebRTC, if you remember, is encryption: everything (including media) is encrypted with your peer's key. So when you send your stream to your peer, only him can decipher it. If anyone else would take a look at the data, it would look scrambled.  
This is good for privacy.  
This is bad for multi casting.  
When A and B are in business, and C joins the conversation, it has to go through a new negociation with A. A new key will be negociated, which means that A will send its stream **twice**: once for B and once for C. The same goes for B and C obviously. Which means the already weak uplinks will be divided by two since it has to upload data for B and for C. Now you follow me?  
Past 3 users, it is almsot impossible to do multi-cast with WebRTC _as is_.

This is all due to the fact that our uplinks are pretty bad, our downlink is way better.  
For your information, there **is** a solution. I will most likely talk about it in a later post, but I'll give you some hints: it is called a WebRTC Gateway.  
Remember when I said our home uplinks where bad while our downlinks were good? Well guess whose uplinks are good?  
Servers, of course. Servers send us data, so they use their good uplinks and we use our good downlinks.  
Well to be quick, a WebRTC Gateway is a server (software) that can speak WebRTC, and is running on a server (hardware).

When you want to multi-cast, say make a conversation with A, B and C. Each peer connects to this WebRTC Gateway, via classic WebRTC. Each of them sends their stream (via their uplinks) **once** to the server (gateway). That gateway is in charge of duplicating thee data and stream them back to A, B and C. This way, each peer only upoads its stream once, and get the other peers' stream.  
Let's face it: you won't be able to multi-cast with 100 peers, streaming Full HD, not yet: your download link is good but is not infinite :-).

Anyway, more on this later.

Hope you enjoyed!