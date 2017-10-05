---
title: Writing a Transporter Module
description:
primary_tag: products>sap-hana-smart-data-streaming
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorial**: [Creating a `cnxml` Configuration File](http://www.sap.com)


## Next Steps
**Tutorials:** [Writing A Formatter Module](http://sap.com)

## Details
### You will learn
 - About the purpose of a transporter module
 - How to build a transporter module
 - How to implement the five methods needed for the transporter module

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Introduction)]

A transporter module is the interface that interacts with external data sources by obtaining data from a data source or outputting data to a data destination.

SAP HANA smart data streaming supports two types of transporters: row-based and stream-based.

Row-based transporters obtain and output data in row format, such as a database transporter. These transporters work with `AdapterRow` instances, which are containers for one or more records or rows as they flow from one module (transporter, formatter, or smart data streaming connector) to the next. You can add multiple records as objects within a List of a single `AdapterRow` object. The `AdapterRow` has a timestamp and block flags that control how its records are communicated to and from smart data streaming.

Stream-based transporters deal with streaming data, such as a socket transporter. These transporters work with `ByteStream` or `ByteBuffer` instances, which represent a continuous stream of data.

In this tutorial, we will be creating a row based transporter module as it lends itself well to `MQTT`.

Before we begin, you can check out the `$STREAMING_HOME/adapters/framework/examples/src` directory for source code of sample transporters.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up the Custom Adapter project)]
**a.** Start by opening your IDE and creating a new java project called `mqtt-input`

**b.** Create a package `com.sap`

**c.** Create a Java class called `MqttTransporter.java`

**d.** Create a Java class called `MqttCB.java`. The code for this file is provided in the appendix section of this tutorial.

**e.** We will now add a number of `.jar` dependencies to our class path:

* PAHO library (See the pre-requisites section of [Writing a Custom Java Toolkit Adapter for SAP HANA Smart Data Streaming](http://www.sap.com) if you have not yet downloaded it)
* The other dependencies will be from the Adapter Toolkit and can be found in `%STREAMING_HOME%\adapters\framework\libj`
    - `Commons-configuration-<version>.jar`
    - `Streaming-client.jar`
    - `Streaming-system.jar`
    - `Streaming-adapter-framework.jar`


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Have MqttTransporter extend the Transporter class)]
We will start by defining a number of instance variables, which will be assigned values in the `init()` method (more on that later).

* `MqttClient client`;
* `String topic`;
* `MqttCB cb`;

Having done this, we will need to implement a number of abstract methods in Transporter. We will cover the methods in the same order they will be called by the adapter framework.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Implement the init() function)]

The first abstract method we will implement is `void init()`. The purpose of this method is to prepare the module for the actions it is responsible for performing. We will use this method to initialize
various global variables as well as grab the user defined parameters for the adapter.

**a.** First, we want to get the Topic parameter value. This value is set by the streaming developer when configuring the adapter in Studio.

* We can get the value of Topic by calling

```java
utility.getParameters().getString("MQTTInputTransporterParamet
ers.Topic");
```
*Note:* The `MQTTInputTransporterParameters` prefix is defined in our adapter configuration file.


**b.** Next, create an `MqttClient`. The constructor takes `serverURI` - the address of the server to connect to, specified as a `URI` and `clientId` - a client identifier that is unique on the server being connected to.

* We will use the `MosquittoServerAddress` defined by the streaming developer and a unique string

```java
client = new
MqttClient(utility.getParameters().getString("MQTTInputTranspo
rterParameters.MosquittoServerAddress"), "MQTT_ESP");
```

**c.** Connect the `MqttClient` with `client.connect();`

**d.** Subscribe the `MqttClient` to the topic with `client.subscribe(topic);`

**e.** Instantiate an `MqttCB` object and assign it to our `MqttClient`. `MqttCB` is a custom `MqttCallback` class written for this adapter. The code for it is provided in the appendix section of this tutorial.

```java
cb = new MqttCB();
client.setCallback(cb);
```

The second abstract method we have to implement is `void start()`. The purpose of this method is to perform any necessary tasks when the adapter is started. For our purposes, it is not necessary to
include any instructions in this method so we will leave it empty.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Implement the execute() method)]
The third and most important method to implement is `void execute()`. When the adapter framework calls this method, it is expected to run continuously until the adapter is requested to stop or until the adapter completes its work.

**a.** As such, we will wrap our functionality in a loop that iterates until the adapter has been issued a stop request. Following this loop – and ending the method – is an instruction to change the adapter `RunState` to done.
```java
while(!utility.isStopRequested())
{
//steps b-d
}
utility.setAdapterState(RunState.RS_DONE);
```

**b.** While the adapter has not been requested to stop, we will continuously check for new `MQTT` messages. The `takeNewMsg()` method will return `null` if there are no new messages, or take the message out of the message queue and return it. When a new message is received, we will process it within the `if` statement.

```java
String msg;
if ((msg = cb.takeNewMsg()) != null){
//steps c-d
}
```

**c.** Once we have received a message, we need to create an `AdapterRow` and send it to our `Formatter` module.
```java
AdapterRow row = utility.createRow(cb.getRcvdMsg());
utility.sendRow(row);
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](void stop() and void destroy())]

The fourth overridden method is `void stop()`. Its purpose is to perform any necessary tasks when the adapter is stopped. We will use this method to disconnect our `MqttClient` by issuing
```java
client.disconnect();
```
The fifth and last method is `void destroy()`. Its purpose is to perform any cleanup tasks for your input or output transporter. For our purposes, it is not necessary to include any instructions in this method so we will leave it empty.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Appendix)]

`MqttCB.java`

```java
package com.sap;
import java.io.IOException;
import javax.security.auth.login.LoginException;
import com.sybase.esp.sdk.exception.EntityNotFoundException;
import com.sybase.esp.sdk.exception.ServerErrorException;
import com.sybase.esp.adapter.framework.utilities.*;
import com.sybase.esp.adapter.framework.RunState;
import com.sybase.esp.adapter.framework.module.Transporter;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttException;
public class MqttTransporter extends Transporter{
 MqttClient client;
 String topic;
 MqttCB cb;

 @Override
 public void init() throws MqttException {
  utility.getAdapterLogger().info("MqttTransporter is initializing:"+utility.getParameters().getString("MQTTInputTransporterParameters.Topic") + "," + utility.getParameters().getString("MQTTInputTransporterParameters.MosquittoServerAddress"));
  topic = utility.getParameters().getString("MQTTInputTransporterParameters.Tppic");
  client = new MqttClient(utility.getParameters().getString("MQTTInputTransporterParameters.MosquittoServerAddress"), "MQTT_ESP");
  client.connect();
  client.subscribe(topic);
  cb = new MqttCB();
  client.setCallback(cb);
 }

 @Override
 public void start() throws IOException {
   utility.getAdapterLogger().info("MqttTransporter is starting.");
 }

  int count = 0;

  @Override
  public void execute() throws IOException, EntityNotFoundException,
  LoginException, ServerErrorException, InterruptedException {
   while(utility.isStopRequested() == false){
      String msg;
      if ((msg = cb.takeNewMsg()) != null){
        utility.getAdapterLogger().info("Got message: " + msg);
        AdapterRow row = utility.createRow(msg);
        utility.sendRow(row);
        utility.getAdapterLogger().info("Sent row to formatter");
      }
  }
  utility.setAdapterState(RunState.RS_DONE);
  }
  @Override
  public void stop() throws MqttException {
    utility.getAdapterLogger().info("MqttTransporter is stopping");
    client.disconnect();
  }
  @Override
  public void destroy() {
    utility.getAdapterLogger().info("MqttTransporter is destroying");
  }
}

```

`MqttCB.java`

```java
package com.sap;
import java.util.LinkedList;
import java.util.Queue;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;

import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttMessage;
public class MqttCB implements MqttCallback{

 private final Queue<String> msgs = new LinkedList();

 @Override
 public void connectionLost(Throwable arg0) {
  System.out.print("Connection lost...");
 }
 @Override
 public void deliveryComplete(IMqttDeliveryToken arg0) {
  System.out.println("Delivery complete.");
 }

 public String takeNewMsg(){
  return msgs.poll();
 }
 @Override
 public void messageArrived(final String topic, final MqttMessage msg) {
  System.out.println("Received message: " + msg);
  msgs.add(msg.toString());
 }
}

```

[ACCORDION-END]

## Next Steps
**Tutorials:** [Writing A Formatter Module](http://sap.com)
