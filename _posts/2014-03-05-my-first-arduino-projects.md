---
layout: post
title: My First Arduino Projects
created: 1394020501
tags: programming
---
I've been an owner of an Arduino board for some time already, but
until very recently I didn't really find some time to play with it.

I do have experience with computing and other IT stuff, but honestly I
wouldn't say that I'm very experienced when it comes to electronics
and microcontrollers, so that was something new for me to learn, and
while doing it I'm having lots of fun! :)

I find the idea of making things which can interact with the
environment around you very interesting, so I've decided it's about
time for me to start creating things with my Arduino!

When starting with something new, you usually start with the basics,
and that's what I did. My first projects involved turning LEDs on and
off, adding buttons to my circuits and in general playing with every
component I found in my kit. It was fun! :)

One day I saw a video on Youtube where some guy created an Arduino
project for home automation -- now that was cool. I decided that I
would also give it try, but first it was time to do some prep work...

So, I've decided that I would do things one at a time, small simple
projects which would later on be merged into the final one -- lights
automation @ home!

## IR Decoder

The first thing I had to do is an IR decoder, which I would use in
order to decode the IR codes of my remote control, which will be used
later for controlling the lights at home.

For making this I've used one [IR Receiver Diode -
TSOP38238](https://www.sparkfun.com/products/10266) and the awesome
[IRremote library](https://github.com/shirriff/Arduino-IRremote).

[![]({{ site.baseurl }}/images/ir-decoder.png)]({{ site.baseurl }}/images/ir-decoder.png){:.glightbox}

Once putting everything in it's place I was able to decode my remote
control IR codes -- the result looked like this:

[![]({{ site.baseurl }}/images/ir-decoder-ready1.jpg)]({{ site.baseurl }}/images/ir-decoder-ready1.jpg){:.glightbox}

## IR Remote Control

Having an IR decoder is cool, but what is even cooler is creating an
IR remote control for your TV using the Arduino!

So, I've decided that I would try to build an IR remote control. For
this project I've used one [IR
LED](https://www.sparkfun.com/products/9349) with the [IRremote
library](https://github.com/shirriff/Arduino-IRremote) and the IR
codes I've already decoded for my remote control.

[![]({{ site.baseurl }}/images/ir-remote.png)]({{ site.baseurl }}/images/ir-remote.png){:.glightbox}

Putting things together on the Arduino and the breadboard and I got my
Arduino TV remote control! :)

The result looked like this:

[![]({{ site.baseurl }}/images/ir-remote-ready1.jpg)]({{ site.baseurl }}/images/ir-remote-ready1.jpg){:.glightbox}

## Next Projects

Code and Arduino sketches for my projects you can find at [my Arduino
Github repository](https://github.com/dnaeon/arduino-projects).

I've only just touched the surface and have a lot to learn, but doing
things with Arduino is so much fun!

I've already ordered all components for my next project -- lights
automation, so when I get these I'm starting off with my next project
right away! :)
