---
title: Introduction to Writing a Custom Java Toolkit Adapter for SAP HANA Streaming Analytics
description: Gain a comprehensive overview on Custom Adapters for SAP HANA Streaming Analytics, and follow detailed instructions for developing a custom adapter using the Java Toolkit.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
time: 15
---

## Prerequisites  
 - **Tutorial:** [Get started with SAP HANA streaming analytics for SAP HANA, express edition](group.sds-hxe-get-started)
 - A running SAP HANA Streaming Analytics Server built on SAP HANA 2.0 with a minimum version SPS02
 - [Java Runtime Environment (JRE)](http://www.oracle.com/technetwork/java/javase/downloads/index.html) version 8 or newer
 - A Java IDE, such as [Eclipse](https://eclipse.org/)
 - [`Mosquitto`](http://mosquitto.org/) MQTT Broker
 - Java [PAHO](https://eclipse.org/paho/clients/java/) Library

## Details
### You will learn
- About Custom Adapters for SAP HANA Streaming Analytics
- About the architecture of Custom Adapter file structure
- About MQTT and `Mosquitto` Broker


---

[ACCORDION-BEGIN [Introduction ](Introduction to Custom Adaptors)]

This set of tutorials provides instructions on building a custom adapter for SAP HANA Streaming Analytics. We will be using the **SAP HANA Streaming Analytics Adapter Toolkit** to create a custom input adapter that receives `MQTT` messages via the `Mosquitto` broker.

The Adapter Toolkit supports writing custom adapters in Java and uses a modular architecture with each adapter consisting of a *Transporter* module, a *Formatter* module, and a streaming *Connector* module.

This tutorial does not cover building a custom adapter using the SAP HANA Streaming Analytics SDK, which provides lower level support for building custom adapters using `C` or `.NET`.

The **complete custom adapter** will consist of:

* A `.jar` file (`mqtt-input.jar`) that implements the Transporter, Formatter, and Connector modules for the Adapter
* A supporting Java library - `org.eclipse.paho.client.mqttv3-<version>.jar`
* The `adapter_config.xml` file which defines configuration parameters for the custom Adapter
* The `mqtt_input.cnxml` file which defines which parameters will be offered to the user and how Studio will start and stop the Adapter
* Edited versions of the `modulesdefine.xml`, `custommodulesdefine.xml`, and `parametersdefine.xsd` files which will define our custom modules and define what elements are valid in an adapter configuration file.

The Adapter Toolkit provides some standard transporter and formatter modules which can be leveraged in building your custom adapter. In order to provide a broader example of building a custom adapter, this tutorial includes writing custom transporter and formatter modules. Note that it is possible to combine existing modules with custom ones.

Once the modules are complete, we will choose an Streaming Analytics connector for our adapter. This will allow our new adapter to communicate with Streaming Analytics. We will then create a configuration file that will define a path to the adapter code, allow users to pass specified values to the adapter for use during runtime, and define the order of modules to call when running the custom adapter.

To finish the Adapter, we will place all of these files in their corresponding locations and access the adapter via HANA Studio.


**What is `MQTT`?**
MQ Telemetry Transport (MQTT) is a machine-to-machine (M2M)/Internet of Things connectivity protocol. It was designed as an extremely lightweight publish/subscribe messaging transport. It is useful for connections with remote locations where a small code footprint is required and/or network bandwidth is at a premium. For example, it has been used in sensors communicating to a broker via satellite link, over occasional dial-up connections with healthcare providers, and in a range of home automation and small device scenarios.

**What is the `Mosquitto` Broker?**
`Mosquitto` is an open source message broker that implements the `MQTT` protocol versions 3.1 and 3.1.1.
A message broker is an intermediary program module which translates a message from the formal
messaging protocol of the sender to the formal messaging protocol of the receiver.

**Standalone vs. Managed Mode**
An adapter can be started in either standalone or managed mode. In standalone mode, the adapter is started separately from the Streaming Analytics project, and in managed mode, the adapter is started with the Streaming Analytics project.

The steps following this this tutorial will assume the adapter will be running in managed mode, however, more information regarding standalone mode will be provided in the Further Reading section. Ensure you have installed the necessary prerequisites.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 1: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
