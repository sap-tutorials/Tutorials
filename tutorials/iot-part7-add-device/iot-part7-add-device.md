---
title: Internet of Things (IoT) Adding a new device to the IoT Services
description: Part 7 of 10, Add a new device to your IoT Services
primary_tag: topic>internet-of-things
tags: [products>sap-hana, products>sap-cloud-platform, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Internet of Things (IoT) Explore the SAP Cloud Platform IoT Services](http://www.sap.com/developer/tutorials/iot-part6-hcp-services.html)


## Next Steps
- [Internet of Things (IoT) Connecting your Tessel to IoT Services](http://www.sap.com/developer/tutorials/iot-part8-hcp-services-tessel.html)
- [Internet of Things (IoT) Connecting your TI SensorTag to IoT Services](http://www.sap.com/developer/tutorials/iot-part11-hcp-services-ti.html)

## Details
### You will learn  
With the MMS service now deployed, and your user assigned the appropriate role it's time for to add your device(s) so you can communicate with it. To do this you will add your device to the service.

### Time to Complete
**10 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Open Message Management Service Cockpit)] ￼

From the SAP Cloud Platform cockpit, select **Java Applications** then select the `iotmms` application. Open the **Message Management Service Cockpit** by clicking on the **Application URL**.

![Application URL](p7_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open IoT Services Cockpit)] ￼

Click on the **View registered devices and device types** tile to open the **IoT Services Cockpit**. You will use this page frequently, so it is worth bookmarking it.

![View registered devices](p7_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add a new message type)] ￼

Click the **Message Types** tile, then the **+** symbol to add a new message type and define the structure of the data you will to collect. In the **Information** section, enter a name "`climateData`".

![Device definition](p7_4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create new field)] ￼

In the **Fields** section, click the **+ Add Field** button to add in two more fields, then enter the following for name and data types. Click the **Create** button (bottom right corner) when complete. When the message type is created, copy the **ID** string. You will need it later.

> Note: Be sure to follow the names and capitalization specified below.

Name            | Type
--------------- | -------------
`timestamp`     | `date`
`Humidity`      | `double`
`Temperature`   | `double`

Copy and save the message type ID, which you will need later:

![ID value](p7_5b.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a new device)] ￼

Click the `<` ("back arrow") again to return the the IoT Services Cockpit. Click on **Device Types**, then the **+** symbol to create a new device. Give it a simple name that makes sense for what you are doing, like "`TesselClimate`". Now click the  **+ Add Message Type** to attach your previously created message type "`climateData`". Make sure the **Direction** is "From Device" and click **Create**.

![New Device Type](p7_3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add the device)] ￼

Finally you can now click the `<` ("back arrow") icon and click **Devices**. Click `+` to add a new device, name it `DevelopmentTessel` and make sure its device type is `TesselClimate`.

> This is extremely important, once you click **Create** a pop-up will appear that will display the **OAuth access token** for this new device. Copy that and save it somewhere, as you will need it soon.  If you lose it, click into the **Authorization** tab and generate a new token.

![Device](p7_6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Copy token ID)] ￼

When the OAuth Access Token is displayed, copy the token ID and save it. Click **Close**.

![Token](p7_7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Save device ID)] ￼

Now copy and save the **Device ID** string

![Device ID](p7_8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Send and view messages)] ￼

With the device type, message type and device configured, it is time to send some data. Go to the **IoT Services Cockpit** and click the **Send and view messages...** tile.

![View Messages](p7_9a.png)

Then click the **Messaging through HTTP** tile in **Data Services** group.

![View stored messages](p7_9b.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Change device ID)] ￼

On the **HTTP API** page, you will see a **Sample Client** to **Send Messages** with an **HTTP endpoint** like this:

`https://iotmmsYOURUSERtrial.hanatrial.ondemand.com/com.sap.iotservices.mms/v1/api/http/data/d000-e000-v000-i000-c000-e001`

Change `d000-e000-v000-i000-c000-e001` to your device's ID (it is a GUID) you copied in step 8 above. Note: the GUID is the device ID, not its authorization token.

Example: https://iotmmsp1234567trial.hanatrial.ondemand.com/com.sap.iotservices.mms/v1/api/http/data/85e0ce4e-09bf-47ea-a5a9-a0469f642c20

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Edit message to post)] ￼

You will next formulate your HTTP POST payload. Since you are sending OData, numbers (`float`, `int`, `double`, etc) are not placed in double quotes, while strings and the Key of the Key/Value pair are in quotes.

Under **Message to post** replace the existing content with the following which matches the message type your used earlier.

```json
{"mode":"sync", "messageType":"m0t0y0p0e1", "messages":[{"Humidity":25.7, "Temperature": 21.5, "timestamp":"2016-11-15T08:45:37.930Z"}]}
```

> NOTE - the `messageType` value `"m0t0y0p0e1"` must be changed to match your `message type ID`.

> The date format is a combined date and time representation in ISO 8601 format. It is in UTC (Coordinated Universal Time) as indicated by a `Z` directly after the time without a space.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Execute the POST)] ￼

Click the POST button. If everything goes OK, you should see a response code of `200` similar to this screen shot or response code `202` when you post for the very first time.

![Server Reply](p7_12.png)

You receive response code `202` when your request was not immediately processed, but instead was placed in a processing queue. You should copy returned HTTP endpoint into **Receive acknowledgments** field to receive the status of your queued message.

Response codes `4xx` or `5xx` indicate that post request has failed.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Display stored messages)] ￼

To verify that the posting worked, switch back **IoT Services Cockpit**, click **View messages received, use sample clients, etc.** tile, then click the **Display stored messages** tile. When the page updates you should see something like this:

![Viewing stored messages](p7_13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Post from an external REST client)] ￼

Now you will post from an external REST client. In this step you will use [Postman Application](https://chrome.google.com/webstore/detail/postman/fhbjgbiflinjbdggehcddcbncdddomop) for Google Chrome.

>There is no need to create Postman account, if you are asked during the first run of the Postman application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Set header)] ￼

Copy the HTTP endpoint to it and select **POST** from the drop down menu.

Open the **Headers** section and set:

- the "`Authorization`" value to "`Bearer [TOKEN ID]`" using the token id you copied when you added your device to the system,
- the "`Content-Type`" to "`application/json`".

![Authorization values](p7_15v.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Paste message content)] ￼

Select then the **RAW** type and copy and paste in the same content you just had (with a few value changes to make it easier to spot this insert. Make sure you change the `messageType` to your ID.

```json
{"mode":"sync", "messageType":"m0t0y0p0e1", "messages":[{"Humidity":25.8, "Temperature": 21.6, "timestamp":"2016-11-15T08:46:37.930Z"}]}
```

![Raw message](p7_16v.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Send message)] ￼

Click **Send** and you should receive a message similar to when you used the SAP Cloud Platform simple client. This indicates that your POST was successful and if you return to the "Display Stored Messages" you will see your new entry.

If you receive `40x` authorization error in return, then check if you are running Postman client in Chrome's incognito mode.

[DONE]
[ACCORDION-END]



## Next Steps
- [Internet of Things (IoT) Connecting your Tessel to IoT Services](http://www.sap.com/developer/tutorials/iot-part8-hcp-services-tessel.html)
- [Internet of Things (IoT) Connecting your TI SensorTag to IoT Services](http://www.sap.com/developer/tutorials/iot-part11-hcp-services-ti.html)
