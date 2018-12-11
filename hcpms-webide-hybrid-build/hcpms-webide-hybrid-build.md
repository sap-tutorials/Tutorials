---
title: Build and run a hybrid app with SAP Web IDE and Hybrid App Toolkit
description: Utilize the Hybrid App Toolkit to build a downloadable hybrid app and run it on a device or simulator
primary_tag: topic>mobile
tags: [ products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>sapui5, tutorial>intermediate ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:**
- [Connecting SAP Web IDE to the Hybrid App Toolkit](https://www.sap.com/developer/tutorials/hcpms-webide-hat-connection.html)
- Additionally, you must have completed the tutorials below in the Web IDE series.  
- [Create a Destination on SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-destination.html)
- [Build an app from an SAP Web IDE template](https://www.sap.com/developer/tutorials/hcp-template-mobile-web-app.html)
- [Deploy your mobile web app to SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html)



## Next Steps
- Continue with the SAP Web IDE series [Add labels and a new field to your app](https://www.sap.com/developer/tutorials/hcp-webide-add-labels-field.html), and return to this tutorial to redeploy your updated mobile web app as a hybrid app.

## Details
### You will learn  
When compiled, your Web IDE generated mobile web app will be packaged using the Apache Cordova container along with any Cordova or SAP plug-ins for Cordova you selected in the [hybrid configuration tutorial]. The output will be a binary file for iOS or Android operating systems that you will run on a simulator, emulator or actual device.


### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Open Web IDE console)]

In SAP Web IDE, view the Console by selecting **View > Console**.

![open console](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/1.png)

The Console is displayed in the lower part of the window and can be resized by dragging the top of the frame up or down. The console messages will allow you to monitor the compilation process.

![console](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/2.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deploy)]

Right-click on the `northwind` project folder. Select **Deploy > Deploy to local Hybrid App Toolkit**.

![start deployment](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/3.png)

Deployment will start. You can view the progress in the console, and when it is complete you will see the dialog box below. Click **OK** to close it.

![start deployment](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/4.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the app in the emulator)]

To run the hybrid app, right click on the `northwind` project folder, then select **Run > Run on > Android Emulator** (or **iOS Simulator** if you are using a Mac). You can also run it on an Android or iOS device if you prefer.

![run on Android Emulator](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/5.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Use test key)]

At the prompt for the signing key, select **Test Key** and click **OK**.
HAT will launch the emulator/simulator and load the app.

![Local signing key](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/6.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Register your app)]

The app will launch on the emulator, simulator or device, and then display the registration screen. Enter your SAP Cloud Platform Username and Password, verify that **Secure** is enabled,  then click on **Register**.

Field Name         | Value
:----------------- | :-------------
Username           |  `<your SAP Cloud Platform account without "trial" at the end>` e.g. `p12345678`
Password           | `<your SAP Cloud Platform account password>`

![Registration screen](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/7.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Choose password)]

You can enter an App Passcode if you would like. To run the app without a passcode, click **Disable Passcode**, then click on **Submit**.

![Passcode screen](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/8.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](View data)]

The app will retrieve the data through SAP Cloud Platform mobile services and display the master (list) view.

![master view](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/9.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](View details)]

Click on one of the items in the list to view the details page, and scroll down to view the rest of the details. Depending on far you have gotten in the Web IDE tutorials, your screen may look different that shown here.

![detail view](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/10.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](View data on other tab)]

If you have implemented a [second tab](https://www.sap.com/developer/tutorials/hcp-webide-add-tab.html) in your app, click it to see the data there.

![detail view](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-build/11.png)



[ACCORDION-END]


## Next Steps
- Continue with the SAP Web IDE series [Add labels and a new field to your app](https://www.sap.com/developer/tutorials/hcp-webide-add-labels-field.html), and return to this tutorial to redeploy your updated mobile web app as a hybrid app.
