---
title: SAP Cloud Platform IoT for Neo: Sending messages from TI SensorTag device
description: Internet of Things (IoT) Connecting your TI SensorTag to IoT Services
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [topic>internet-of-things, products>sap-cloud-platform-internet-of-things, tutorial>beginner ]
---
## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [SAP Cloud Platform IoT for Neo: Configuring the device for environmental sensors data](http://www.sap.com/developer/tutorials/iot-part7-add-device.html)

## Next Steps
- [SAP Cloud Platform IoT for Neo: Viewing your environmental sensors data from device](http://www.sap.com/developer/tutorials/iot-part13-hcp-services-viewdataui5.html)

## Details
### You will learn  
In the previous tutorial you saw how to add your message type, device type and devices to the IoT Services of the SAP Cloud Platform. Now those instructions were based on using a Tessel device however you can just as easily use a different device such as a TI `SensorTag`.

### Time to Complete
**10 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Review your previous settings)]

You can use the same settings you have for the Tessel or you can repeat the previous tutorial with the TI `SensorTag` in mind.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new JavaScript file)]

Next create a new Node.js file called `readTags.js`

Insert this code:

```javascript
/* 	sensorTag IR Temperature sensor example
*  Craig Cmehil, SAP SE (c) 2015
*/

/* Choose the proper HTTP or HTTPS, SAP Cloud Platformrequires HTTPS */
var http = require('https');

var SensorTag = require('sensortag');
var lv_temp;
var lv_humid;
var lv_deviceid = "";
var DEBUG_VALUE = true;
var xtimestamp;
var date = new Date();
var time = date.getTime ();

// SAP Cloud Platform connection details
var portIoT = 443;
var pathIoT = '/com.sap.iotservices.mms/v1/api/http/data/';
var hostIoT = 'iotmmsXXXXXXXXXXtrial.hanatrial.ondemand.com';
var authStrIoT = 'Bearer XXXXXXXXXXXX';
var deviceId = 'XXXXXX-XXXX-XXXX-XXXX-XXXXXXXXX';
var messageTypeID = 'XXXXXXXXXXXX';

var options = {
    host: hostIoT,
  port: portIoT,
    path: pathIoT + deviceId,
    agent: false,
    headers: {
       'Authorization': authStrIoT,
       'Content-Type': 'application/json;charset=utf-8',
	 'Accept': '*/*'
    },
    method: 'POST',     
};

/***************************************************************/
/* Coding to access TI SensorTag and values of various sensors */
/***************************************************************/

console.log("If not yet activated, then press the power button.");
SensorTag.discover(function(tag) {
tag.on('disconnect', function() {
	console.log('disconnected!');
	process.exit(0);
});

function connectExecute() {
	console.log('Connect Device and Execute Sensors');
	tag.connectAndSetUp(enableSensors);
}

function enableSensors() {
	/* Read device specifics */
	tag.readDeviceName(function(error, deviceName) {
		console.log('Device Name = ' + deviceName);
	});
	tag.readSystemId(function(error, systemId) {
		console.log('System ID = ' + systemId);
		lv_deviceid = systemId;
	});
	tag.readSerialNumber(function(error, serialNumber) {
		console.log('Serial Number = ' + serialNumber);
	});
	tag.readFirmwareRevision(function(error, firmwareRevision) {
		console.log('Firmware Rev = ' + firmwareRevision);
	});
	tag.readHardwareRevision(function(error, hardwareRevision) {
		console.log('Hardware Rev = ' + hardwareRevision);
	});
	tag.readHardwareRevision(function(error, softwareRevision) {
		console.log('Software Revision = ' + softwareRevision);
	});
	tag.readManufacturerName(function(error, manufacturerName) {
		console.log('Manufacturer = ' + manufacturerName);
	});
	/* Enable Sensors */
	console.log("Enabling sensors:");
	console.log('\tenableIRTemperatureSensor');
	tag.enableIrTemperature(notifyMe);
	console.log('\tenableHumidity');
	tag.enableHumidity(notifyMe);
	console.log("*********************************************");
	console.log(" To stop press both buttons on the SensorTag ");
	console.log("*********************************************");
}

function notifyMe() {
	tag.notifySimpleKey(listenForButton);
	setImmediate(function loop () {
		tag.readIrTemperature(function(error, objectTemperature, ambientTemperature){
      		lv_obj = objectTemperature.toFixed(1);
      		lv_ambient = ambientTemperature.toFixed(1);
    		});
		tag.readHumidity(function(error, temperature, humidity) {
			lv_temp = temperature.toFixed(1);
			lv_humid = humidity.toFixed(1);
		});
		if(DEBUG_VALUE)
			console.log("Sending Data: " + lv_deviceid + " " + lv_temp + " " + lv_humid);
		setSensorData(lv_temp, lv_humid);
		setTimeout(loop, 10000);
	});
  }

function listenForButton() {
	tag.on('simpleKeyChange', function(left, right) {
		if (left && right) {
			tag.disconnect();
		}
   });
}

connectExecute();
});

/******************************************************************/
/* FUNCTION to get Temperature from the Sensor & update into HANA */
/******************************************************************/
function setSensorData(lv_temp,lv_humid){
date = new Date();
  time =date.getTime();

var data = {
	"mode":"sync",
	"messageType": messageTypeID,
	"messages": [{
		"timestamp": time,
		"temperature": lv_temp,
		"humidity": lv_humid
	}]
  };
var strData = JSON.stringify(data);
if(DEBUG_VALUE)
	console.log("Data: " + strData);
if(strData.length > 46){
	if(DEBUG_VALUE)
		console.log("Sending Data to server");
	/* Process HTTP or HTTPS request */
	options.agent = new http.Agent(options);
	var request_callback = function(response) {
		var body = '';
		response.on('data', function (data) {
			body += data;
		});
		response.on('end', function () {
			if(DEBUG_VALUE)
				console.log("REQUEST END:", response.statusCode);
		});
		response.on('error', function(e) {
			console.error(e);
		});    
	}
	var request = http.request(options, request_callback);
	request.on('error', function(e) {
		console.error(e);
	});
	request.write(strData);
	request.end();
}else{
	if(DEBUG_VALUE)
		console.log("Incomplete Data");
}
}
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add Message Type and Device parameters)]

Add in your parameters from your newly added **Message Type** and **Device**

```javascript
var hostIoT = 'iotmmsXXXXXXXXXXtrial.hanatrial.ondemand.com';
var authStrIoT = 'Bearer XXXXXXXXXXXX';
var deviceId = 'XXXXXX-XXXX-XXXX-XXXX-XXXXXXXXX';
var messageTypeID = 'XXXXXXXXXXXX';
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Save and run)]

Save and execute.

```sh
node readTags.js
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Check the results)]

![results](5.png)


[ACCORDION-END]

## Next Steps
- [SAP Cloud Platform IoT for Neo: Viewing your environmental sensors data from device](http://www.sap.com/developer/tutorials/iot-part13-hcp-services-viewdataui5.html)
