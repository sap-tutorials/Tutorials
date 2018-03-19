---
title: Internet of Things (IoT) Setup the Tessel
description: Setup and configure a Tessel device
primary_tag: topic>internet-of-things
tags: [  tutorial>beginner, topic>internet-of-things, products>sap-hana, products>sap-cloud-platform, tutorial>how-to ]
---
## Prerequisites  
- **How-To:** [Internet of Things (IoT) Setup up your Environment](http://www.sap.com/developer/tutorials/iot-tessel-setup.html)

## Next Steps
- [Internet of Things (IoT) Explore the SAP Cloud Platform IoT Services](http://www.sap.com/developer/tutorials/iot-part6-hcp-services.html)
- [Internet of Things (IoT) Setup SAP HANA XS (On-premise or stand-alone server)](http://www.sap.com/developer/tutorials/iot-part2-hanaxs-setup.html)


## How-To Details
Now to connect and configure your Tessel 1 device (further calls simply `Tessel`).

Prior to starting this tutorial you should have your Tessel device and a USB cable. For Windows machines simply plugging in the device will often result in the drivers be installed whereas a Apple Mac OS X or Linux machine require a bit more effort to install.

You should also be familiar with working with the command prompt in Windows or the Terminal or Shell in Mac or Linux.

After this tutorial you can choose either the On-Premise or the Cloud route.


### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Get your Tessel connected)]

Refer to the Tessel website <https://t1-start.tessel.io/install> for detailed information on getting your Tessel connected for your operating system.

>In rare cases of problems with automatic installation of the driver on Windows machines please use [`Zadig` utility](http://zadig.akeo.ie/) to manually install `WinUSB` driver for the Tessel device in Windows OS.
>If Tessel is not visible on the list of devices in `Zadig`, then go to **Options** and select **List All Devices**.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test Tessel communication with your computer)]

Once you have correct version of Node.js installed and the Tessel driver installed, ensure your Tessel is plugged in via the USB cable. Then you should be able to run the following command (this will be from either the CMD prompt in Windows or the Terminal or Shell in Mac or Linux)

```
tessel list
TM-00-04-f000da30-0062434f-4a8145c2
```
If it shows the serial number of the connected Tessel device, then you are good to proceed with exercises.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Check and update firmware)]

Once your Tessel has been connected and you've checked to ensure the firmware is up to date you can run other commands such as:

`tessel version --board`
will tell you the version of your Tessel device. Currently the latest version of the firmware is
```
C:\nodecode\tessel-npm>tessel version --board
TESSEL! Connected to TM-00-04-f000da30-005e474e-3020e5c2.
INFO Serial #: TM-00-04-f000da30-005e474e-3020e5c2
INFO Wifi Version: 1.28
INFO Firmware Commit: 3b8c922
INFO Runtime Commit: dd434be
```

If your version is different, then
`tessel update --force`
will update the Tessel's firmware to the latest version.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Install text editor)]

Now that your device is connected you can write your first JavaScript application and execute it on our device.

The first application to write is `blinky.js` this makes the on-board LED's of the device blink rapidly. The code is rather simple and more information can be found here: <https://t1-start.tessel.io/blinky>.

Before you run this code, you do need to ensure that you have a proper text editor or preferably a code editor installed. There are several options for this such as `Chocolat` for Mac OS X or `Notepad++` you can use any but something designed for code and syntax highlighting is ideal.

Code for this small JavaScript application is simple and straight forward but you do need to understand some basics of the language.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create JavaScript file)]

Create a new folder named `tessel-code` on your hard drive in some place easy to find (e.g. in a `dev` folder on Linux or `C:\` on Windows).

Inside the `tessel-code` folder, create a new file named `blinky.js`.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add JavaScript code)]

Once you create the file add the following code to it:

```javascript
// Import the interface to Tessel hardware
var tessel = require('tessel');
// Set the led pins as outputs with initial states
var led1 = tessel.led[0].output(1);
var led2 = tessel.led[1].output(0);
setInterval(function () {
    console.log("I'm blinking! (Press CTRL + C to stop)");
    // Toggle the led states
    led1.toggle();
    led2.toggle();
}, 100);
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](View Tessel code files)]

Now go to your command prompt, Terminal or shell and change to the `tessel-code` folder. Often times this is accomplished with the `cd` command for change directory.

Once in that directory, run the command `dir` (Windows) or `ls` (Mac/Linux) to show the files in the folder and ensure your `blinky.js` file is there.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run the app)]

If it is then you are in the right place and you can now execute the command to have the Tessel device run the application.

`tessel run blinky.js`

If you are successful then you should see two small LEDs on the device rapidly turning on and off. If so you are now ready to explore different modules with your Tessel device.

>For extra credit, modify `blinky.js` so the lights blink in unison.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Connect your Tessel to Wi-Fi)]

Tessel device has a Wi-Fi chip installed on the board. Connect your Tessel device as described at <https://t1-start.tessel.io/wifi> and do `wifi.js` example from that web page.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Try sensors)]

Tessel's modules are sensors and actuators that connect to a device and provide additional functionality and produce data.

Read how to work with modules at https://t1-start.tessel.io/modules. Then pick available module and follow code installation and examples on corresponding module page, like:
- <https://t1-start.tessel.io/modules/climate>
- <https://t1-start.tessel.io/modules/ambient>
- <https://t1-start.tessel.io/modules/accelerometer>

Three of these modules are Climate, Ambient and Accelerometer. While you can try any of the sensors on hand, it is highly recommend you start with one of these 3 sensors.

In particular the Climate sensor you will enhance later in the exercises to provide data to your SAP Cloud Platform account.

> When working with the different modules you should create a separate folder under your `tessel-code` folder, the reason for this is that when you install the libraries for the module it's best to do this in a new directory because when you create your JavaScript code and then run that on the device it deploys the libraries at the same time and thus best to not have everything inside the same exact folder.


[ACCORDION-END]



## Next Steps
- [Internet of Things (IoT) Explore the SAP Cloud Platform IoT Services](http://www.sap.com/developer/tutorials/iot-part6-hcp-services.html)
- [Internet of Things (IoT) Setup SAP HANA XS (On-premise or stand-alone server)](http://www.sap.com/developer/tutorials/iot-part2-hanaxs-setup.html)
