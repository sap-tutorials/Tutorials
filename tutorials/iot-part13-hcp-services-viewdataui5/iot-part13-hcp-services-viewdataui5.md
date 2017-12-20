---
title: Viewing data from environmental sensors (IoT for Neo)
description: Now it is time to display your stored data from your Tessel device using SAPUI5 and SAP Web IDE
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [topic>internet-of-things, products>sap-cloud-platform-internet-of-things, products>sap-hana, tutorial>beginner ]

---

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:**  [SAP Cloud Platform IoT for Neo: Sending messages from Tessel1 device](https://www.sap.com/developer/tutorials/iot-part8-hcp-services-tessel.html), or
- [SAP Cloud Platform IoT for Neo: Sending messages from TI SensorTag device](https://www.sap.com/developer/tutorials/iot-part11-hcp-services-ti.html)

## Next Steps
- [SAP Cloud Platform IoT for Neo: binding to and developing on SAP HANA XSC](https://www.sap.com/developer/tutorials/iot-part10-hcp-services-hanaxs.html)


## Details
### You will learn  
Now that you have data stored in the system its time to display that data within an application. To do that you need to create a new application that can interface with data service and display the data our device has been generating. What you will find next are the steps necessary to do just that. You will need basic knowledge in using GitHub.


### Time to Complete
**10 Min**.

---
[ACCORDION-BEGIN [Step 1: ](Download IoT Starter Kit)]

Go to [SAP IoT Starter Kit](https://github.com/SAP/iot-starterkit). Choose either to download the ZIP file.

![Repo](1.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Unpack and create new archive)]

Unpack the archive and navigate to the folder `iot-starterkit-master > src > apps > ui5 > consumption > src` here you will find a file `neo-app.json` and a folder `webapp` you will need to compress or pack these two items into a new archive file (`ZIP`).

![Folders](2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import file)]

Now in the SAP Cloud Platform cockpit select "Connectivity" in the left sidebar menu and now choose the Destinations. Then click on "Import From File", and through your directory structure to the folder `iot-starterkit-master > src > apps > ui5 > consumption > destinations` and select the file `iotmms`.

![Destinations](3.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Replace account and user ID)]

Be sure to replace `%account_id%` with your Account ID e.g `d045495trial`. As well as `%User_id%` with your user User ID e.g `d045495`, enter your password and then save. Then repeat with the file `iotrdms`![Destinations](4.png)
[ACCORDION-END][ACCORDION-BEGIN [Step 5: ](Open SAP Web IDE)]Now under the "Services" section in the sidebar menu and choose the "SAP Web IDE"

![Destinations](5.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Import a new project)]

Next import a new project from your local file system, this will be the new archive you created. Then select the path.

![import](6.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Change OData field name)]

Open the `	main.view.js` file and you will need to modify the name of the OData field to match that of your Message Type. In the case of the climate example that would be changing line 120 to `value: "{odata>C_TEMPERATURE}"` instead of the `value: "{odata>C_VALUE}"`

![main view](8.png)

![code change](9.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Launch the index file)]

Open the `index.html` file and you can launch it as is.
![running app](7.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Add code to project)]

You can now use your expertise with SAPUI5 and start making modifications. Such as the following lines (158 to 164) controlling the `y axis`.

```
		yAxis: new sap.viz.ui5.types.Axis({
		scale: new sap.viz.ui5.types.Axis_scale({
			fixedRange: false,
			minValue: 20,
			maxValue: 40
		})
	}),
```


[ACCORDION-END]



## Next Steps
- [SAP Cloud Platform IoT for Neo: binding to and developing on SAP HANA XSC](https://www.sap.com/developer/tutorials/iot-part10-hcp-services-hanaxs.html)
