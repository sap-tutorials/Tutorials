---
title: Internet of Things (IoT) Viewing your Tessel data from IoT Services
description: Part 9 of 10, Now it is time to display your stored data from your Tessel device
primary_tag: topic>internet-of-things
tags: [products>sap-hana, products>sap-cloud-platform, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:**  [Internet of Things (IoT) Connecting your Tessel to IoT Services](http://www.sap.com/developer/tutorials/iot-part8-hcp-services-tessel.html)
- [Getting Started with the SAP Cloud Platform Tools for Java](http://www.sap.com/developer/tutorials/hcp-java-eclipse-setup.html)
- Note: JDK 1.6 or 1.7 are required. If you have a later version of Java installed, please install JDK 1.7 and temporarily change your `JAVA_HOME` environment variable to point to it.

## Next Steps
- [Internet of Things (IoT) Connecting IoT Services to SAP HANA XS](http://www.sap.com/developer/tutorials/iot-part10-hcp-services-hanaxs.html)


## Details
### You will learn  
Now that you have data stored in the system its time to display that data within an application. To do that you need to create a new application that can interface with data service and display the data our device has been generating. What you will find next are the steps necessary to do just that. You will need your Eclipse environment setup, basic knowledge in using Eclipse as well as GitHub set up and some basic Java programming skills. It will also assume that you have already connected your Eclipse environment to your SAP Cloud Platform trial account.
For more information on that please [see tutorial](http://www.sap.com/developer/tutorials/hcp-java-eclipse-setup.html)


### Time to Complete
**20 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Open Eclipse)] ￼

Open Eclipse and first switch over to the Git Repository Perspective.

![git perspective](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Clone Git repository)] ￼

Choose "Clone a Git repository" using the following URL
[https://github.com/SAP/iot-starterkit.git](https://github.com/SAP/iot-starterkit.git) which is the SAP official IoT Starter Kit.

![Clone repo](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Select master branch)] ￼

3. Select `master` then next.

![Clone repo](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Choose directory)] ￼

Choose your directory location then `finish`

![Clone repo](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Import projects)] ￼

Select the repository at the highest level, right click, and select Import Projects.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Edit index file)] ￼

Once that has finished you will need to open your `index.html` file and make some small modifications. You will find this file in the following directory.
`iot-starterkit/src/apps/java/consumption/com.sap.iot.starterkit.ui/src`

For starters you will need to change your settings to match your newly connected device.

```javascript
oSettingsModel.setData({
"deviceId" : "dcc6d8b5-fec2-43a2-96be-1a85b0a1912b",
"deviceTypeId" : "a87bb50d9b3789aa4c2f",
"fromDeviceMessageTypeId" : "1",
"toDeviceMessageTypeId" : "2",
});  
```

Don't remember just go back to your `iotmms` application and you can get them from there. Once you have that in place you will need to adjust the `graph` settings to match the values of your message type as well as your data values, change Slider Value to Temperature F.

```javascript
function createMeasureFeed() {
return new sap.viz.ui5.controls.common.feeds.FeedItem({
"uid" : "primaryValues",
"type" : "Measure",
"values" : ["Temperature F"]
});
}  
```

Now change Slider Value to Temperature F, and add in your column name.

```javascript
function createDataSet() {
return new sap.viz.ui5.data.FlattenedDataset({
dimensions : [{
		name : "Timestamp",
		value : {
			path : "sensor>C_TIMESTAMP",
			formatter : function (oValue) {
				//can be a string primitive in JSON, but we need a number
				if ((typeof oValue) === "string") {
					oValue = parseInt(oValue);
				}
				//ensure that UNIX timestamps are converted to milliseconds
				var oDate = new Date(oValue * 1000);
				return oDate.toLocaleString();
			}
		}
	}
],
measures : [{
		name : "Temperature",
		value : "{sensor>C_TEMPERATURE}"
	}
],
data : {
	path : "sensor>/"
}
});
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Build the project using Maven)] ￼

Now in Eclipse, build the project using Maven. If you don't have Maven, click on Help install new software. In the Work with line, start typing eclipse.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Open Collaboration)] ￼

Open Collaboration, and look for `m2e-Maven Integration for Eclipse`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Accept terms)] ￼

Then click next, next, agree to the terms, then finish.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Build the project using Maven)] ￼

In Eclipse, build the project using Maven. Click on the project name in the project explorer, right click Run As Maven Build (your screen might look different than this)

![Run As](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/8.png)

`iot-starterkit/src/apps/java/consumption/com.sap.iot.starterkit.ui`

![Clean Install](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/9.png)

For the goals, enter `clean install`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Deploy applications from SAP Cloud Platform trial accoun)] ￼

Now jump back over to your SAP Cloud Platform trial account Cockpit and choose `Java Applications` then select the `Deploy Applications`.

![Deploy Applications](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/11.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Deploy was file)] ￼

Find your `.war` file, give it a name and click deploy. When it has been loaded, click Done (not Start)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Import file)] ￼

Back inside of Eclipse, find the `iotmms` file in the destinations file. You are going to import that into the HCP. Modify the file so that it uses your account.

![destination file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Choose destinations)] ￼

Now in the SAP Cloud Platform cockpit select your new application under your `Java Applications` then choose the `Destinations`.

![destinations](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Import the file)] ￼

Click on Import From File, and browse to your file that you've created.

![Import](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/14.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Add your password)] ￼

Add in your password. Save the entry. Stop the application and restart.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Select data bindings)] ￼

Now select the `Data Bindings`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 18: ](Change the Schema ID)] ￼

Leave the Data Source as default and change the Schema ID to your `iotmms` (`XXXX.iotmms.web`) and click save e.g. `pXXXXXXXtrial.iotmms.web`

![Bindings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/16.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 19: ](Click start)] ￼

Click on Overview, and click the Start button under State. This will take a minute or two.

![Bindings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/17.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 20: ](Click the URL)] ￼

When it has started up, click on the URL that is under Application URLs.

![Bindings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/18.png)

![Bindings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part9-hcp-services-viewdata/19.png)

[DONE]
[ACCORDION-END]




## Next Steps
- [Internet of Things (IoT) Connecting IoT Services to SAP HANA XS](http://www.sap.com/developer/tutorials/iot-part10-hcp-services-hanaxs.html)
