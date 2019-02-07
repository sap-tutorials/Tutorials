---
title: Creating Custom Adapter Modules
description: Create a Transporter Module and a Formatter module for your Custom Adapter using Java PAHO Library.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Tutorial:** [Introduction to Writing a Custom Java Toolkit Adapter for SAP HANA Streaming Analytics](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part1.html)

## Next Steps
 - [Creating Custom Adapter Configuration Files](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part3.html)

## Details
### You will learn
 - How to create Transporter and Formatter modules

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create a Transporter Module)]

A transporter module is the interface that interacts with external data sources by obtaining data from a data source or outputting data to a data destination.

SAP HANA Streaming Analytics supports two types of transporters: row-based and stream-based.

Row-based transporters obtain and output data in row format, such as a database transporter. These transporters work with `AdapterRow` instances, which are containers for one or more records or rows as they flow from one module (transporter, formatter, or Streaming Analytics connector) to the next. You can add multiple records as objects within a List of a single `AdapterRow` object. The `AdapterRow` has a timestamp and block flags that control how its records are communicated to and from Streaming Analytics.

Stream-based transporters deal with streaming data, such as a socket transporter. These transporters work with `ByteStream` or `ByteBuffer` instances, which represent a continuous stream of data.

In this tutorial, we will be creating a row based transporter module as it lends itself well to `MQTT`.

Before we begin, you can check out the `$STREAMING_HOME/adapters/framework/examples/src` directory for source code of sample transporters.

> The full source code for the Transporter Module is provided in the `Appendix` Section

First, we will set up the Custom Adapter Project.

  1. Start by opening your IDE and creating a new java project called `mqtt-input`
  2. Create a package `com.sap`
  3. Create a Java class called `MqttTransporter.java`
  4. Create a Java class called `MqttCB.java`. The code for this file is provided in the appendix section of this tutorial.
  5. We will now add a number of `.jar` dependencies to our class path:

      - Java [PAHO](https://eclipse.org/paho/clients/java/) library
      - The other dependencies will be from the Adapter Toolkit and can be found in `%STREAMING_HOME%\adapters\framework\libj`

        - `Commons-configuration-<version>.jar`
        - `Streaming-client.jar`
        - `Streaming-system.jar`
        - `Streaming-adapter-framework.jar`

Then, have `MqttTransporter` extend the `Transporter` class.

We will start by defining a number of instance variables, which will be assigned values in the `init()` method (more on that later).

  - `MqttClient client`;
  - `String topic`;
  - `MqttCB cb`;

Having done this, we will need to implement a number of abstract methods in Transporter. We will cover the methods in the same order they will be called by the adapter framework.

The first abstract method we will implement is `void init()`. The purpose of this method is to prepare the module for the actions it is responsible for performing. We will use this method to initialize various global variables as well as grab the user defined parameters for the adapter.

  1. First, we want to get the Topic parameter value. This value is set by the streaming developer when configuring the adapter in Studio.

    We can get the value of Topic by calling:

    ```java
    utility.getParameters().getString("MQTTInputTransporterParamet
    ers.Topic");
    ```
    > The `MQTTInputTransporterParameters` prefix is defined in our adapter configuration file.


  2. Next, create an `MqttClient`. The constructor takes `serverURI` - the address of the server to connect to, specified as a `URI` and `clientId` - a client identifier that is unique on the server being connected to.

    We will use the `MosquittoServerAddress` defined by the streaming developer and a unique string

    ```java
    client = new
    MqttClient(utility.getParameters().getString("MQTTInputTranspo
    rterParameters.MosquittoServerAddress"), "MQTT_ESP");
    ```

  3. Connect the `MqttClient` with `client.connect();`

  4. Subscribe the `MqttClient` to the topic with `client.subscribe(topic);`

  5. Instantiate an `MqttCB` object and assign it to our `MqttClient`. `MqttCB` is a custom `MqttCallback` class written for this adapter. The code for it is provided in the appendix section of this tutorial.

    ```java
    cb = new MqttCB();
    client.setCallback(cb);
    ```

The second abstract method we have to implement is `void start()`. The purpose of this method is to perform any necessary tasks when the adapter is started. For our purposes, it is not necessary to
include any instructions in this method so we will leave it empty.

The third and most important method to implement is `void execute()`. When the adapter framework calls this method, it is expected to run continuously until the adapter is requested to stop or until the adapter completes its work.

  1. As such, we will wrap our functionality in a loop that iterates until the adapter has been issued a stop request. Following this loop – and ending the method – is an instruction to change the adapter `RunState` to done.

    ```java
    while(!utility.isStopRequested())
    {
    //steps b-d
    }
    utility.setAdapterState(RunState.RS_DONE);
    ```

  2. While the adapter has not been requested to stop, we will continuously check for new `MQTT` messages. The `takeNewMsg()` method will return `null` if there are no new messages, or take the message out of the message queue and return it. When a new message is received, we will process it within the `if` statement.

    ```java
    String msg;
    if ((msg = cb.takeNewMsg()) != null){
    //steps c-d
    }
    ```

  3. Once we have received a message, we need to create an `AdapterRow` and send it to our `Formatter` module.

    ```java
    AdapterRow row = utility.createRow(cb.getRcvdMsg());
    utility.sendRow(row);
    ```

The fourth overridden method is `void stop()`. Its purpose is to perform any necessary tasks when the adapter is stopped. We will use this method to disconnect our `MqttClient` by issuing

```java
client.disconnect();
```

The fifth and last method is `void destroy()`. Its purpose is to perform any cleanup tasks for your input or output transporter. For our purposes, it is not necessary to include any instructions in this method so we will leave it empty.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Formatter Module)]

A formatter module converts between the data format of the transporter module and Streaming Analytics.

SAP HANA Streaming Analytics supports two types of formatters: row-based and stream-based formatters.

Row-based formatters obtain and output data in row format. They work with `AdapterRow` instances, which are containers for one or more records or rows as they flow from one module (transporter, formatter, or Streaming Analytics connector) to the next. You can add multiple records as objects within a List of a single `AdapterRow` object. The `AdapterRow` has a timestamp and block flags that control how its records are communicated to and from Streaming Analytics.

Stream-based formatters deal with streaming data. These formatters work with `ByteStream` instances, which represent a continuous stream of data. Full documentation on writing a Formatter module can be found in the section [Building a Custom Formatter Module](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789b6606f0f10149915c3cb6a302153.html).

Before we begin, you can check out the `$STREAMING_HOME/adapters/framework/examples/src` directory for source code of sample formatters.

> The full source code for the Formatter Module is provided in the `Appendix` Section

  1. Start by opening your IDE and navigating to the java project called `mqtt-input` that we created for our Transporter Module (complete the pre-requisite tutorial if you have not yet completed this). Note that it is also valid to create a new project and create your Formatter module separately but for simplicity, we will be creating them in the same project.

  2. Create a Java class called `Mqttformatter.java`

  3. Have `MqttFormatter` extend the `RowFormatter` class. Note that it is also possible to create a custom Formatter module that extends the `StreamingFormatter` class. For the purposes of this tutorial, we will be extended the `RowFormatter` class. Similar to our Transporter module, we will have to implement a number of abstract methods (only 3 this time though).

The purpose of the `void init()` method is to prepare the formatter module to convert between data formats. For example, obtain properties from the adapter configuration file and perform any required initialization tasks. Our Formatter module is very simple and does not require any initialization instructions.

The second method is `AdapterRow convert(AdapterRow in)`.

  1. First, we will test whether the received `AdapterRow` is non-empty. If this is the case, we will simply send the `AdapterRow` back.

    ```java
    if (in.getDataList().isEmpty()){
     return in;
    }
    ```

  2. If we have reached this point in the method, the received `AdapterRow` is non-empty. Our particular Formatter will convert a `MQTT` message (String) to something usable by Streaming Analytics - an `AepRecord`. First, we will create the desired `AepRecord.`

      - Create a new `AepRecord`.

        ```java
            AepRecord tempRecord = new AepRecord();
        ```

      - Set the operation of the record.

        ```java
          tempRecord.setOpCode(Operation.INSERT);
        ```

      - Get the data list inside the `AepRecord`, and add the first object of the data list in the `AdapterRow`.

        ```java
          tempRecord.getValues().add(in.getData());
        ```

Now that we have the `AepRecord` and would like to send it to Streaming Analytics, we will convert the received `AdapterRow` by replacing its data list value at index 0 with the new `AepRecord` - `tempRecord`.

```java
in.setData(0, tempRecord);
```

We have now finished converting the `MQTT String` to an `AepRecord` so we will return it.

```java
return in;
```

The last method to implement is `void destroy()` which is intended for performing clean-up actions for the formatter. Our formatter does not require any destroy instructions.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Package Modules into a .jar file)]

Now that we have written our `Transporter` and `Formatter` modules, we need to package them in a `.jar` file. If you have been following this tutorial, you should have a single java project containing `MqttTransporter.java`, `MqttFormatter.java` and `MqttCB.java`. Build a `.jar` containing all of these files. The process for doing so varies with `IDE` so, if you have questions, it is best to consult your `IDE's` help pages. Before building the `.jar` file, verify that you will be building it with the same `JRE` version as included in your Streaming Analytics install. Name the newly created `.jar` file `"mqtt-input.jar"`.

For the question below, select all of the correct answers, and click **Validate**.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix ](&nbsp;)]

`MqttTransporter.java`

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

`MqttFormatter.java`

```java
package com.sap;

import com.sybase.esp.adapter.framework.AepRecord;
import com.sybase.esp.adapter.framework.module.RowFormatter;
import com.sybase.esp.adapter.framework.utilities.AdapterRow;
import com.sybase.esp.sdk.Stream.Operation;

public class MqttFormatter extends RowFormatter {

    @Override public void init() {
        utility.getAdapterLogger().info("MqttFormatter is initializing");
    }

    @Override public AdapterRow convert(AdapterRow in ) {
        utility.getAdapterLogger().info("MqttFormatter is converting");
        if ( in .getDataList().isEmpty()) {
            return in;
        }
        AepRecord tempRecord = new AepRecord();
        tempRecord.setOpcode(Operation.INSERT);
        tempRecord.getValues().add( in .getData());

        in .setData(0, tempRecord);
        utility.getAdapterLogger().info("MqttFormatter is done converting");
        return in;
    }

    @Override public void destroy() {
        utility.getAdapterLogger().info("MqttFormatter is destroying");
    }
}

```

[DONE]

[ACCORDION-END]

