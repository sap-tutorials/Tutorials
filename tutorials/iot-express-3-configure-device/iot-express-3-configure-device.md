---
title: Assemble and Configure Device Hardware
description: Assemble device hardware, configure device firmware and deploy it in the field.
primary_tag: topic>internet-of-things
tags: [  tutorial>beginner, topic>leonardo, topic>internet-of-things, products>sap-iot-application-enablement, products>sap-cloud-platform, products>sap-cloud-platform-iot]
---

## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** [Create a Simple IoT Device Model](https://www.sap.com/developer/tutorials/iot-express-2-create-device-model.html)
 - **Hardware:** Laptop, monitor, HDMI cable, USB mouse and keyboard


## Details
### You will learn
- How to connect sensors to a device
- How to read sensor values from device firmware
- How to securely send data to the cloud using certificates

### Time to Complete
**30 Min**

## Next Steps
  - [Create a Thing Model and Bind Your Device into a Business Context](https://www.sap.com/developer/tutorials/iot-express-4-create-thing-model.html)

---
[ACCORDION-BEGIN [Step 1: ](Assemble the hardware)]

  If you are using the [SAP Leonardo IoT Foundation Starter Kit](https://www.sparkfun.com/products/14659?custom_code=SAP) or a similar kit, assemble it based on the following pictures and instructions.

  ![Pinout of the Raspberry Pi](pipinout.png)

  ![Sensors](sensor.jpg)

  In the above pictures, you see a Raspberry Pi and the logical pins that allow you to connect to other hardware and sensors. The second image shows the temperature/humidity sensor and a light sensor. Instead of the DHT22, you can also use the DHT11 on a break-out board -- simply change the code below from DHT22 to DHT11.

  Using the provided wires, connect GND (or - on some boards) from the temperature/humidity sensor board to GND on the Pi (e.g. pin 9). Also connect VCC (on + on some boards) to the `3.3V PWR` on the Pi (e.g pin 1). Lastly connect the DATA pin of he temperature/humidity sensor to GPIO4 (pin 7) on the Pi.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Install operating system for the Pi)]

  The last step missing before you can use the Raspberry Pi is to install an operating system on the micro SD card. On Windows and Mac, we recommend you use the etcher application available from [http://etcher.io](http://etcher.io). Download the latest `Raspbian` image (with desktop) from [https://www.raspberrypi.org/downloads/raspbian/](https://www.raspberrypi.org/downloads/raspbian/) and burn it onto the micro SD card using Etcher. Then, plug the micro SD card into the Raspberry Pi at the bottom.

  Your Pi is now in a factory default state. The next step will be to install firmware and configure it based on a device created in IoT Service for Cloud Foundry.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Log into Pi and install certificate and firmware)]

  To be able to program the Raspberry Pi, you need to hook it up via USB to a mouse and a keyboard and via a standard HDMI cable to a monitor. When you now power it up, you should see the `Raspbian OS` desktop on the monitor.

  If you are working on this tutorial from your company's office, getting an internet connection via LAN or WLAN might require to contact your network administrator, for example, to whitelist your MAC address for WLAN or approve your device in the LAN. For fast testing, you can also use your mobile phone's hotspot.

  Once you have established a connection to the internet open a browser window and access the IoT Service for Cloud Foundry cockpit (check the previous tutorial on where to find it; the fastest way to get there is to open [http://hana.ondemand.com/](http://hana.ondemand.com/) and then navigate to your Cloud Foundry sub-account, from there to your space and in there you can find the `dashboard` in the service instances list). Download the certificate from your device to the `Downloads` folder on the Pi and take note of the secret provided.

![IoT Cockpit and Certificate](certificate.png)

  Access this tutorial from the browser on the Raspberry Pi and copy the following node files as `app.js` and as `certificate_parser.js` into the `Downloads` folder:


  Content of `app.js`:


```javascript
const fs = require("fs");
const mqtt = require("mqtt");
const certificateParser = require("./certificate_parser.js");
const usonic = require('pigpio');
const Gpio = require('pigpio').Gpio;
const rpiDhtSensor = require('rpi-dht-sensor');


const settings = {
    tenant: "<instance name>",
    certificate: {
        file: "./<certificate_filename>.pem",
        passphrase: "<certificate_secret>"
    },
    deviceAlternateId: "<device_alternate_id>",
    sensorAlternateId: "<sensor_alternate_id>",
    capabilityAlternateId: "<capability_alternate_id>",
};

var dht = new rpiDhtSensor.DHT22(4);

const certificate = certificateParser.parse(settings.certificate.file);

const mqttClient = mqtt.connect({
    host: `${settings.tenant}.eu10.cp.iot.sap`,
    port: 8883,
    clientId: settings.deviceAlternateId,
    protocol: "mqtts",
    clean: false,
    key: certificate.privateKey,
    cert: certificate.certificate,
    passphrase: settings.certificate.passphrase
});

function readSensor() {

    var readout = dht.read();

    var sensorData = {
        "_time": Date.now(),
        "temperature": Math.round(readout.temperature),
        "humidity": Math.round(readout.humidity),
        "light" : 0
    };

    return sensorData;
};

mqttClient.on("connect", () => {

    console.log("[mqtt] connected");

    setInterval(async function () {

        let sensorData = await readSensor();

        let payload = {
            "capabilityAlternateId": settings.capabilityAlternateId,
            "sensorAlternateId": settings.sensorAlternateId,
            "measures": [sensorData]
        };

        // publish sensor values
        console.log(`[mqtt] publishing: ${JSON.stringify(payload)}`);
        mqttClient.publish(`measures/${settings.deviceAlternateId}`, JSON.stringify(payload));

    }, 5000); // every 5s
});
```

  Content of `certificate_parser.js`:


```javascript
const fs = require("fs");

const states = {
    PK_START: "-----BEGIN ENCRYPTED PRIVATE KEY-----",
    PK_END: "-----END ENCRYPTED PRIVATE KEY-----",
    CERTIFICATE_START: "-----BEGIN CERTIFICATE-----",
    CERTIFICATE_END: "-----END CERTIFICATE-----",
};

function determineState(line) {
    for (state in states) {
        if (line == states[state]) {
            return states[state];
        }
    }
    return null;
}

function parse(file) {
    let certificate = fs.readFileSync(file, 'utf8').split("\n");

    let parsePrivateKey = "";
    let parsedCertificate = "";
    let state = null;

    for (line of certificate) {
        state = determineState(line) || state;

        if (state == states.PK_START || state == states.PK_END)
            parsePrivateKey += (line + '\n');
        else if (state == states.CERTIFICATE_START || state == states.CERTIFICATE_END)
            parsedCertificate += (line + '\n');
    }

    return {
        privateKey: parsePrivateKey.trim(),
        certificate: parsedCertificate.trim()
    };
}

module.exports = {
    parse: parse
};
```

  To download the required libraries, execute the following commands from a terminal window:

```
sudo apt-get update
curl -sL https://deb.nodesource.com/setup_9.x | sudo -E bash –
sudo apt-get install -y nodejs
sudo apt-get install npm
sudo apt-get install pigpio
cd Downloads
npm install --save pigpio
npm install --save mqtt
npm install --save rpi-dht-sensor
```

> **IMPORTANT:** Check before running that you have configured the device model in SAP Cloud Platform Internet of Things Service as it is mentioned in the prerequisite tutorial.

  Finally, adjust the node script in the following places:

    1. The tenant name in `settings` variable - this is the instance name for IoT Service (in `https://76eae3d3-9de6-4345-b26c-ff35be052652.eu10.cp.iot.sap/` it's `76eae3d3-9de6-4345-b26c-ff35be052652`)
    2. The alternate IDs of your device, sensor and capability in the `settings` variable.
    3. The secret for your certificate
    4. The name of your certificate file

  Now the firmware is ready to be run. Normally, it would be nice to be executed automatically when the device boots but in this case we will start it manually in the next step.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Run the firmware)]

  To run the firmware execute the following commands in a terminal or command line window:

```
cd Downloads
sudo node app.js
```

  This will now continuously read sensor values, print them and send the data to IoT Service for Cloud Foundry. The data is not beeing picked up by anyone. It is forwarded to IoT Application Enablement but while there is no thing mapped the data is not persisted.

  ![Output on the PI](sensoroutput.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Add a light sensor (optional))]

  In our standard kit, an additional light sensor is included. Hooking this up requires more effort. As there is no library for node for this sensor, you would have to build up code yourself that takes other implementations on how to read the sensor data from this sensor via the `I2C bus` and translates this how to do it in node. There are some I2C libraries to make your life simpler.

[ACCORDION-END]

---
