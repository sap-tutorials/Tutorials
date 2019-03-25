---
title: Test the Custom Java Toolkit Adapter
description: Test the MQTT Input Adapter we have created using Mosquitto and HANA Studio.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Tutorial:** [Where to Place the Custom Adapter Files](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part5.html)

## Next Steps
 - [Further Readings on Custom Adapters](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part7.html)

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Run the `Mosquitto` broker)]

Open a command prompt(`CMD`) and navigate to the install directory of `Mosquitto`. The default location is in **`C:\Program Files(x86)\Mosquitto`**. Run the `Mosquitto` broker by typing **`mosquitto.exe`**

![Run Mosquitto Exe](runMosquittoExe.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure your Streaming Project)]

Set up a streaming project like the one shown below if you haven't already set up one in the previous tutorial (Where to Place the Custom Adapter Files). The `ccl` code is provided in the Appendix section.

![Custom Adapter Streaming Project](cclproject.png)

Right click on the "Properties" to enable editing.

Choose the **topic** for the new `MQTT_Input_Adapter1` to subscribe to. In this tutorial, we will use "test".

![Edit Topic Test](editTopicTest.png)

Replace **`<your-ip-address>`** with the `ip address` of the machine you will be running the `Mosquitto` `MQTT` message broker.

![Edit Your IP Address](editYourIpAddress.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Set Default Server URL)]

Ensure that the server you have placed your custom adapter, including the required configuration files on is set as the `Default Server URL`. The `Streaming Server` being used to compile the project that is using the custom adapter needs to have access to the adapter configuration files during the compile process.

  1. Select Window > Preferences in your `IDE`.

    ![Setting Default URL Preferences](window-preferences.png)

  2. With the Preferences window now open, select SAP HANA streaming analytics and, if necessary, change the `Default Server URL` to match the `ip address` of the machine you are running the Streaming Server. Click `Apply` and then `OK`.

    ![Setting Default URL Preferences](preferences-streaming-analytics.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run your Streaming Project)]

In the SAP HANA Streaming Development perspective right click on the project in the Project Explorer, go to SAP HANA streaming analytics, and click on **Compile Streaming Project** to compile your project.

![Compile your Streaming Project](compile-streaming-project.png)

To run your project, right click on the project in the Project Explorer, go to SAP HANA streaming analytics, then run, and click on **Run Streaming Project in Workspace** to run your project.

![Run your Streaming Project](run-streaming-project.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open the Input Window)]

Switch to the SAP HANA Streaming Run-Test perspective from the Quick Access toolbar in the top right corner of your `IDE`.

![Switch to Run-Test Perspective](switchToRunTest.png)

Open the input window by double clicking `InputWindow1` in the Run-Test perspective of HANA Studio. This is where the `MQTT` message will appear.

![Select Input Window 1](selectInputWindow1.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Send an MQTT Message using Mosquitto)]

 Open another command prompt (`CMD`) window and navigate to the install directory of `Mosquitto`. The default location is in `C:\Program Files (x86)/Mosquitto`.

Run the `mosquitto_pub.exe` program with <pre>`mosquitto_pub.exe -t `"<b>`<topic>`</b>" `–m` "<b>`<message>`</b>" `–h` "<b>`<your-ip-address>`</b>".</pre>
 E.g.  `mosquitto_pub.exe –t "test" –m "Hello World!" –h "1.0.100.200"`

![Send an MQTT Message with Mosquitto](send-mqtt-message.png)

You should now be able to view the published `MQTT` message in `InputWindow1`.

![View your MQTT Message](view-mqtt-message.png)

In the box below, enter the string you have received in the `MQTT` message on HANA Studio, and click **Validate**.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix ](&nbsp;)]

```SQL

CREATE INPUT WINDOW InputWindow1 SCHEMA ( Message string )
PRIMARY KEY ( Message ) KEEP ALL ROWS ;

ATTACH INPUT ADAPTER MQTT_Input_Adapter1
TYPE mqtt_input TO InputWindow1
PROPERTIES  mosquittoServerAddress = 'tcp://<your-ip-address>:1883' ,
topic = 'test' ;

```

[DONE]

[ACCORDION-END]

