---
title: Internet of Things (IoT) Setup for a TI SensorTag
description: How to setup your system to access a TI `SensorTag` using Bluetooth and Node.js
primary_tag: topic>internet-of-things
tags: [  tutorial>beginner, topic>internet-of-things ]
---
## Prerequisites  
 - **Device:** [`TI SensorTag`](http://www.ti.com/ww/de/wireless_connectivity/sensortag/)
 - Bluetooth connectivity

## How-To Details
The following describes the general procedures necessary to setup your local environment to communicate and work with a Bluetooth `TI SensorTag`.

### Time to Complete
**10 Min**.

---


1. Install the following tools:
    - [Node.js 4.4.3](https://nodejs.org/en/blog/release/v4.4.3/) or higher
    - Install an HTML/JavaScript editor &#151; if you don't have one, simply search Google for something appropriate like [Notepad++](https://notepad-plus-plus.org/), [Sublime Text](http://www.sublimetext.com/), etc...

2. Install the Node.js native add-on build tool from the command prompt (or Terminal)

    `npm install -g node-gyp`

    For more information see [Installation instructions](https://github.com/nodejs/node-gyp)

3. Install the Node.js `SensorTag` library from the command prompt (or Terminal)

    `npm install -g sensortag`

    For more information see [Installation instructions](https://www.npmjs.com/package/sensortag)

4. You may also need to check the prerequisites for [`Noble`](https://github.com/sandeepmistry/noble#prerequisites)
