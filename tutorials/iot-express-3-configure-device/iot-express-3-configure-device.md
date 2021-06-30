---
title: Assemble and Configure Device Hardware
description: Assemble device hardware, configure device firmware and deploy it in the field.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform]
time: 30
---

## Prerequisites  
 - **Tutorials:** [Create a Simple IoT Device Model](iot-express-2-create-device-model)
 - **Hardware:** Laptop, monitor, HDMI cable, USB mouse and keyboard


## Details
### You will learn
- How to connect sensors to a device
- How to read sensor values from device firmware
- How to securely send data to the cloud using certificates

---
[ACCORDION-BEGIN [Step 1: ](Assemble the hardware)]

Assemble a raspberry pi based on the following pictures and instructions.

![Pinout of the Raspberry Pi](pipinout.png)

![Sensors](sensor.jpg)

In the above pictures, you see a Raspberry Pi and the logical pins that allow you to connect to other hardware and sensors. The second image shows the temperature/humidity sensor and a light sensor. Instead of the DHT22, you can also use the DHT11 on a break-out board -- simply change the code below from DHT22 to DHT11.

Using the provided wires, connect GND (or - on some boards) from the temperature/humidity sensor board to GND on the Pi (e.g. pin 9). Also connect VCC (on + on some boards) to the `3.3V PWR` on the Pi (e.g pin 1). Lastly connect the DATA pin of he temperature/humidity sensor to GPIO4 (pin 7) on the Pi.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Install the operating system for the Pi)]

The last step missing before you can use the Raspberry Pi is to install an operating system on the micro SD card. On Windows and Mac, we recommend you use the etcher application available from [http://etcher.io](http://etcher.io). Download the latest `Raspbian` image (with desktop) from [https://www.raspberrypi.org/software/](https://www.raspberrypi.org/software/) and burn it onto the micro SD card using Etcher. Then, plug the micro SD card into the Raspberry Pi at the bottom.

Your Pi is now in a factory default state. The next step will be to install firmware and configure it based on a device created in IoT Service for Cloud Foundry.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Log into Pi and install certificate and firmware)]

To be able to program the Raspberry Pi, you need to hook it up via USB to a mouse and a keyboard and via a standard HDMI cable to a monitor. When you now power it up, you should see the `Raspbian OS` desktop on the monitor.

If you are working on this tutorial from your company's office, getting an internet connection via LAN or WLAN might require to contact your network administrator, for example, to whitelist your MAC address for WLAN or approve your device in the LAN. For fast testing, you can also use your mobile phone's hotspot.

Once you have established a connection to the internet open a browser window and access the IoT Service for Cloud Foundry cockpit (check the previous tutorial on where to find it; the fastest way to get there is to open [http://hana.ondemand.com/](http://hana.ondemand.com/) and then navigate to your Cloud Foundry sub-account, from there to your space and in there you can find the `dashboard` in the service instances list). Download the certificate from your device to the `Downloads` folder on the Pi and take note of the secret provided.

![IoT Cockpit and Certificate](certificate.png)

Access this tutorial from the browser on the Raspberry Pi and copy the following node files as `app.js` and as `certificate_parser.js` into the `Downloads` folder:

1. Content of `app.js`:

    ```JavaScript
    const fs = require("fs");
    const mqtt = require("mqtt");
    const certificateParser = require("./certificate_parser.js");
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
                "timestamp": Date.now(),
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

2. Content of file `certificate_parser.js` :


    ```JavaScript
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

3. To download the required libraries execute the following 9 commands from a terminal window:

    ```Shell
    sudo apt-get update
    curl -sL https://deb.nodesource.com/setup_9.x | sudo -E bash –
    sudo apt-get install -y nodejs
    sudo apt-get install npm
    cd Downloads
    npm install --save mqtt
    npm install --save rpi-dht-sensor
    ```

> **IMPORTANT:** Check before running that you have configured the device model in SAP Cloud Platform Internet of Things Service as it is mentioned in the prerequisite tutorial.

Finally, adjust the node script in the following places:

1. The tenant name in `settings` variable, which is the instance name for IoT Service (in `https://76eae3d3-9de6-4345-b26c-ff35be052652.eu10.cp.iot.sap/` it's `76eae3d3-9de6-4345-b26c-ff35be052652`)

2. The alternate IDs of your device, sensor and capability in the `settings` variable

3. The secret for your certificate

4. The name of your certificate file

Now the firmware is ready to be run. Normally, it would be nice to be executed automatically when the device boots but in this case we will start it manually in the next step.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Run the firmware)]

To run the firmware execute the following commands in a terminal or command line window:

```Shell
cd Downloads
sudo node app.js
```

This will now continuously read sensor values, print them and send the data to IoT Service for Cloud Foundry. The data is not beeing picked up by anyone. It is forwarded to SAP IoT but while there is no thing mapped the data is not persisted.

!![Output on the PI](sensoroutput.png)

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Add a light sensor (optional))]

In the following picture you have a diagram of how the final connection configuration between the Raspberry Pi and the sensors should be.
![Connection diagram of pi and the sensors](finalelectricaldiagram.png)

Proceed by connecting the light sensor to your Raspberry Pi. If you use a different sensor (based on the `i2c` bus) then the one shown in the picture above then simply change `i2cAddress` below and the address of the `d0low`/`d1high` below based on the sensors data sheet and wire it up accordingly. For many sensors you can find instructions and tutorials on how to hook them up with a raspberry pi online.

Start by hooking up the `3.3V PWR` of the sensor to pin no. 1 of the pi and connect the GND of the sensor to the GND of the Raspberry.
Next, you want to connect the SCL pin of the sensor (pin no. 4) to the SCL pin of the pi (pin no. 5) and the SDA pin of the sensor (pin no. 5) to the SDA pin of the pi (pin no. 3).

You will need to add the `raspi-i2c` package to you project. Do this by running the following commands in a terminal while beeing in the app folder.

```Shell
npm install --save raspi-i2c
```

1. Lastly, you will want to modify the code by adding the following snippets in order to read data from the sensor.

    ```JavaScript

    // Add this to the required packages. This package enables us to access the pins and read the data
    const I2C = require('raspi-i2c').I2C;

    // Add this right bellow the packages. This const represents the address of the pin that we will want to read
    const i2cAddress = 0x39;

    // Instantiate a new I2C object
    var i2c = new I2C();
    i2c.writeByteSync(i2cAddress, 0x81, 16); // high gain , integration time 0 - configuration command found in sensor datasheet

    ```

2. Add this `getLuxValue` function to compute the sensor reading value. This formula is described in the sensor documentation file.

    ```JavaScript

    function getLuxValue(d0low, d1low){
        var lux = 0;

        if(d0low === 0) return 0;

        d0low = parseFloat(d0low / 0.034);
        d1low = parseFloat(d1low / 0.034);

        var rap = d1low * 1.0 / d0low;
        console.log("rap=",rap);

        if(rap > 0 && rap <= 0.5){
            lux = 0.0304 * d0low - 0.062 * d0low * Math.pow(rap, 1.4);
        } else if(rap > 0.5 && rap <= 0.61){
            lux = 0.0224 * d0low - 0.031 * d1low;
        } else if(rap > 0.61 && rap <= 0.8){
            lux = 0.0128 * d0low - 0.0153 * d1low;
        } else if(rap > 0.8 && rap < 1.3){
            lux = 0.00146 * d0low - 0.00112 * d1low;
        } else if(rap > 1.3){
            lux = 0;
        }

        lux = parseInt(lux);

        return lux;
    }

    ```

3. Replace the previous implementation of the **`readSensor`** function with the definition below:

    ```JavaScript

    // This definition adds the required functionality for reading the light sensor and returning its value
    function readSensor() {
        var readout = dht.read();

        var d0low = i2c.readWordSync(i2cAddress, 0xAC);
        var d1low = i2c.readWordSync(i2cAddress, 0xAE);
        var lux = getLuxValue(d0low, d1low);

        var readResp = {
            "_time": Date.now(),
            "temperature": parseInt(readout.temperature),
            "humidity": parseInt(readout.humidity),
            "light" : parseInt(lux)
        }

        return readResp;
    };
    ```

4. Your final **`app.js`** should look like this:

    ```JavaScript
    const fs = require("fs");
    const mqtt = require("mqtt");
    const certificateParser = require("./certificate_parser.js");
    const rpiDhtSensor = require('rpi-dht-sensor');

    const I2C = require('raspi-i2c').I2C;
    const i2cAddress = 0x39;

    var i2c = new I2C();
    i2c.writeByteSync(i2cAddress, 0x81, 16); // high gain , integration time 0

    const settings = {
        tenant: "76eae3d3-9de6-4345-b26c-ff35be052652",
        certificate: {
            file: "./testDevice-device_certificate.pem",
            passphrase: "M18C0iyd6BKYappW"
        },
        deviceAlternateId: "b85f72725bb1b745",
        sensorAlternateId: "f6d49a6a91b45992",
        capabilityAlternateId: "envData",
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

    function getLuxValue(d0low, d1low){
        var lux = 0;

        if(d0low === 0) return 0;

        d0low = parseFloat(d0low / 0.034);
        d1low = parseFloat(d1low / 0.034);

        var rap = d1low * 1.0 / d0low;
        console.log("rap=",rap);

        if(rap > 0 && rap <= 0.5){
            lux = 0.0304 * d0low - 0.062 * d0low * Math.pow(rap, 1.4);
        } else if(rap > 0.5 && rap <= 0.61){
            lux = 0.0224 * d0low - 0.031 * d1low;
        } else if(rap > 0.61 && rap <= 0.8){
            lux = 0.0128 * d0low - 0.0153 * d1low;
        } else if(rap > 0.8 && rap < 1.3){
            lux = 0.00146 * d0low - 0.00112 * d1low;
        } else if(rap > 1.3){
            lux = 0;
        }

        lux = parseInt(lux);

        return lux;
    }

    function readSensor() {
        var readout = dht.read();

        var d0low = i2c.readWordSync(i2cAddress, 0xAC);
        var d1low = i2c.readWordSync(i2cAddress, 0xAE);
        var lux = getLuxValue(d0low, d1low);

        return {
                    "_time": Date.now(),
                    "temperature": parseInt(readout.temperature),
                    "humidity": parseInt(readout.humidity),
            "light" : parseInt(lux)
                }
    };

    mqttClient.on("connect", () => {
        console.log("[mqtt] connected")
        setInterval(async function () {
            let measure = await readSensor();

            let payload = {
                "capabilityAlternateId": settings.capabilityAlternateId,
                "sensorAlternateId": settings.sensorAlternateId,
                "measures": [measure]
            };

            // publish sensor values
            console.log(`[mqtt] publishing: ${JSON.stringify(payload)}`);
            mqttClient.publish(`measures/${settings.deviceAlternateId}`, JSON.stringify(payload));

        }, 5000); // every 5s
    });

    ```

[DONE]

[ACCORDION-END]

---
