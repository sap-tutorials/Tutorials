---
title: Internet of Things (IoT) Connecting your Tessel to IoT Services
description: Part 8 of 10, Now connect your Tessel device to the IoT Services for sending data
tags: [products>sap-hana, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Internet of Things (IoT) Adding a new device to the IoT Services](http://go.sap.com/developer/tutorials/iot-part7-add-device.html)

## Next Steps
 - [Internet of Things (IoT) Viewing your Tessel data from IoT Services](http://go.sap.com/developer/tutorials/iot-part9-hcp-services-viewdata.html)


## Details
### You will learn  
Now its time to insert data from your Tessel to SAP IoT Services. If you have already worked through Part 5 "Inserting Tessel Data" then this will be a quick modification of your code.  


### Time to Complete
**15 Min**.

---

1. Open your existing `climate.js` file in your editor and make these changes to the section after you assign the climate module to port “A” and before you actually read the temperature from the module itself.

    These changes set the parameters needed to post your data. Notice you will need to make the changes for your information for the TOKEN, Account, Device ID, etc.
    If you are using the same code from the previous sections you will need to modify it to match your message type definition now.  
    In this example instead of sending a single value as you did in the previous section you are sending multiple values.

    ```javascript
    // Connect to SAP IoT Services parameters
    var hostIoT = 'iotmms########trial.hanatrial.ondemand.com';
    var portIoT = 443;
    var pathIoT = '/com.sap.iotservices.mms/v1/api/http/data/';
    var authStrIoT = 'Bearer e352bcc9dfdec1cdc6fc3e5ea0b2a66b';
    var deviceId = '78863a3a-e7b0-47df-acd3-b0d8ca67067e';

    // which device are you?
    var stationId = 1;

    var xTemp;
    var xHumid;
    var xbright;
    var xtimestamp;
    var date = new Date();
    var time = date.getTime ();
    ```

    ![parameter definition](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part8-hcp-services-tessel/p8_1.png)

2. Comment out the Connect to HANA parameters:

    ![HANA parameters](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part8-hcp-services-tessel/p8_2.png)

3. Comment out the entire `updateHANA` function.

    ![HANA function](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part8-hcp-services-tessel/p8_3.png)

4. Insert the new `updateIoT` function at the bottom of your `climate.js` file.


    ```javascript
    function updateIoT(temp, humid,brightness) {
    var http = require('https');
    var options = {
      host: hostIoT,
      port: portIoT,
      path: pathIoT + deviceId,
      agent: false,
      headers: {
         'Authorization': authStrIoT,
         'Content-Type': 'application/json;charset=utf-8'
      },
      method: 'POST',     
    };
    options.agent = new http.Agent(options);
    callback = function(response) {
      var body = '';
      response.on('data', function (data) {
        body += data;
      });
      response.on('end', function () {
        console.log("END:", response.statusCode, JSON.parse(body).msg);
      });
      response.on('error', function(e) {
          console.error(e);
     });    
    }
    var req = http.request(options, callback);
    req.on('error', function(e) {
    console.error(e);
    });
    console.log ("time was:");
    console.log (time);
    date = new Date();
    time =date.getTime();
    console.log(time);

    req.shouldKeepAlive = false;
      var jsonData = {
        "mode":"sync",
        "messageType":"6c7a02f24cc32ee07174",
        "messages": [{
            "Humidity": humid,
            "Temperature": temp,
            "timestamp": time
            }]
    }
    var strData = JSON.stringify(jsonData);
    console.log("POST jsonData:" + strData);
    req.write(strData);
    req.end();
    }
    ```

    >NOTE: `messageType` needs to be changed to match your `messageTypeID`.


5. Comment out the call to `updateHANA` and insert the call to `updateIot` just below it.

    ```javascript
    updateIoT(temp.toFixed(4), humid.toFixed(4));
    ```

6. Check that your Tessel is still connected to Wi-Fi by running the command `tessel wifi -l`. If you do not get a response like that below, you should reconnect to the network with the command below with the correct SSID and password. If your access point has spaces in the name then just put it inside of quotes like this: `“Wifi name”`.

    `tessel wifi -n SSID -p password`

    ![Acquiring IP](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part8-hcp-services-tessel/p8_6.png)

7. Run your code, and you should see an output like the one below:

    ```bash
    tessel run climate.js
    ```

    ![Posting to HCP](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part8-hcp-services-tessel/p8_7.png)


8. Return to the **IoT Services Cockpit**, click the **View messages received, use sample clients, etc.** tile, click the **View stored messages** tile, then select your **table**. You will see the latest messages showing up there. You can also click the **REFRESH** button as your script runs.

    ![Viewing new values in HCP](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part8-hcp-services-tessel/p8_8.png)

## Next Steps
  - [Internet of Things (IoT) Viewing your Tessel data from IoT Services](http://go.sap.com/developer/tutorials/iot-part9-hcp-services-viewdata.html)
