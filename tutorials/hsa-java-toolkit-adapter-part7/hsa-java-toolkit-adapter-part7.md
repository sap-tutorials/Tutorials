---
title: Further Readings on Custom Adapters
description: Learn more about features of Custom Adapters like enabling guaranteed delivery, creating an output adapter and implementing schema discovery.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
time: 30
---

## Prerequisites
 - [Test the Custom Java Toolkit Adapter](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part6.html)

## Details
### You will learn
 - How to enable guaranteed delivery
 - How to implement schema discovery
 - How to create an output adapter
 - How to create stream based Transport and Formatter Modules
 - Pre-defined Transporter and Formatter modules
 - How to debug a Custom Adapter


---

[ACCORDION-BEGIN [Enable Guaranteed delivery](&nbsp;)]

This is done in the custom transporter module. SAP documentation regarding the process can be found in section [Enabling Guaranteed Delivery for an Input Transporter](https://help.sap.com/doc/saphelp_esp_51sp10_adapt/5.1.10/en-US/e7/924c076f0f10149b76975e225dfdf7/frameset.htm).

[DONE]

[ACCORDION-END]
[ACCORDION-BEGIN [Implement Schema Discovery](&nbsp;)]

  1. Add the following two commands to your `.cnxml` file:
    ```html
    <Internal id="x_unixCmdDisc"
      label="Discovery Command"
      type="string"
      default="&quot;$STREAMING_HOME/adapters/framework/bin/discover.sh&quot;
      &quot;$STREAMING_HOME/adapters/framework/instances/mqtt_input/adapter_config.xml&quot;"/>
    <Internal id="x_winCmdDisc"
      label="Discovery Command"
      type="string"
      default="&quot;%STREAMING_HOME%/adapters/framework/bin/discover.bat&quot;
      &quot;%STREAMING_HOME%/adapters/framework/instances/mqtt_input/adapter_con fig.xml&quot;"/>
    ```

  2. Implement sampling or non-sampling schema discovery in your custom transporter module. Consult section [Implementing Schema Discovery in a Custom Adapter](https://help.sap.com/doc/saphelp_esp_51sp11_bca/5.1.11/en-US/e7/8a48b56f0f10149134ab152de88342/frameset.htm) for more details.

For the question below, select the correct answer, and click **Validate**.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Create an Output Adapter](&nbsp;)]

If you have followed this tutorial to create an input adapter, only minor changes will have to be applied to convert it into an output adapter.

  1. `EspConnector`

      - You will need to choose either [`EspSubscriber`](https://help.sap.com/viewer/b5a4b8b1574f48a383f7a1e42e63d4d9/1.0.12/en-US/e789cecb6f0f1014bdc7ccdcbde12dd6.html?q=espsubscriber) or [`EspMultiStreamSubscriber`](https://help.sap.com/doc/saphelp_esp_51sp11_bca/5.1.11/en-US/e7/855da66f0f1014a43ad7ab965faf16/frameset.htm) instead of the `EspPublisher` we have used.

      -  To make this change, edit the `<Module type="espconnector">` element of your `.cnxml` file. Specifically, specify the chosen `EspSubscriber` in the `<InstanceName>` element

  2. `Formatter module`

      The convert(`AdapterRow in`) method will need to be edited to convert Streaming Analytics objects to `Strings`. You can change it to something along the lines of the following:

      ```java
      Object obj = in.getData(0);
      in.setData(0, obj.toString());
      return in;
      ```

  3. `Transporter module`

      Instead of the following in execute():

      ```java
      AdapterRow row = utility.createRow(msg);
      utility.sendRow(row);
      ```

      You will need something along the lines of the following:

      ```java
      AepRecord record = (AepRecord)row.getData(0);
      if(record != null) {
          String value = record.getValues().toString();
          myDataSink.send(value);
      }
      ```

      Where `myDataSink` is the object you are outputting your data to.

  4. `Mqtt_input.cnxml`

      - Particularly the type attribute for the Adapter element should be changed to **"output"**

        ```html
        <Adapter type="output" …>
        ```

      - You may consider `find > replace all` from input to output. This will require changing the `mqtt_input` folder name in **`%STREAMING_HOME%/adapters/framework/instances/mqtt_input`**

      - Consider changing the name of this file to `mqtt_output.cnxml`

  5. `Adapter_config.xml`, `modulesdefine.xml`, `parametersdefine.xsd`

      You may consider changing "Input" to "Output" in names

      - `<MQTTInputTransporterParameters>`

      - `<Name>MQTT Input</Name>`

  6. `Mqtt.ccl`

      This project assumes the adapter is an input adapter, you will need to make a new project. However, doing so is outside the scope of this tutorial.

[DONE]

[ACCORDION-END]
[ACCORDION-BEGIN [Create Stream Based Transporter and Formatter Modules](&nbsp;)]

  1. Transporter

     Change Execute() function according to following:

     - For input stream based transporters:

        - Create a `ByteBuffer` object and load data into it by calling `<ByteBuffer>.put<Type>(data)`

        - Call `utility.sendRowsBuffer()`

     - For output stream based transporters:

        - Call `utility.getRowsBuffer()` which will return a `ByteBuffer` object.

        - Call `ByteBuffer.get<Type>()` to get data from the object.

Full `ByteBuffer` documentation can be found at [`ByteBuffer`](https://docs.oracle.com/javase/7/docs/api/java/nio/ByteBuffer.html)

Documentation regarding custom `Transporter` modules can be found in section [Building a Custom Transporter Module](https://help.sap.com/viewer/a29f649d1a0d4b3f88c634250e42af77/5.1.12/en-US/e789b1b56f0f1014a969ccf833602144.html).

  2. Formatter

    - Have your custom formatter class extend `com.sybase.esp.adapter.framework.module.StreamingFormatter`

    - Implement the following functions:

      - The `init()` function.

        Prepare your formatter module to convert between data formats; for example, obtain properties from the adapter configuration file and perform any required initialization tasks.

      - The `start()` function.

        Perform any necessary tasks when the adapter is started.

    - The `execute()` function.

        Here is an example of the execute() function for a formatter that converts row-based data into stream-based:

        ```java

        public void execute() throws Exception {
          OutputStream output = utility.getOutputStream();  while(!utility.isStopRequested())  {
            AdapterRow row = utility.getRow();
            if(row != null) {
              AepRecord record = (AepRecord)row.getData(0);
              String str = record.getValues().toString() + "\n";   output.write(str.getBytes());
            }
          }
        }

        ```

    - For a formatter that converts from stream-based data into row-based, use:

        - `utility.getInputStream()` to obtain the `InputStream`

        - `utility.createRow()` to create the `AdapterRow` objects

        - `utility.sendRow()` to send the rows to the next module specified in the adapter configuration file

    - The `stop()` function

        Perform any necessary tasks when the adapter is stopped.

    - The `destroy()` function.

        Perform clean-up actions for your formatter.


Documentation regarding custom Formatter modules can be found in section [Building a Custom Formatter Module](https://help.sap.com/viewer/b5a4b8b1574f48a383f7a1e42e63d4d9/2.0.00/en-US/e789b6606f0f10149915c3cb6a302153.html?q=custom%20formatter%20module).

See the `$STREAMING_HOME/adapters/framework/examples/src` directory for source code for sample modules

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Pre-Defined Transporter and Formatter Modules](&nbsp;)]

If your transporter or formatter modules don't require very complex implementation, they may already be provided with your Streaming Analytics installation.

You can find a listing of the predefined Formatter modules in section [Formatters Currently Available from SAP](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.02/en-US/e785c5536f0f10148ce5edaa93952636.html) as well as their location.

You can find a listing of the predefined Transporter modules in section [Transporters Currently Available from SAP](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.02/en-US/e785c1236f0f1014bd14db27ffcf687b.html) as well as their location.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Start and Stop an Adapter in Standalone Mode](&nbsp;)]

  1. Comment out the `ATTACH INPUT ADAPTER` statement in the `ccl` code provided in the Appendix of this tutorial. The `ccl` should simply be as follows:

    ```sql

    CREATE INPUT WINDOW InputWindow1 SCHEMA (
      Message string ) PRIMARY KEY ( Message ) KEEP ALL ROWS ;

    ```

  2. Uncomment the `<ProjectName>` and `<StreamName>` elements in `<EspPublisherParameters>`

  3. Start the project in Studio by pressing the deploy button.

    ![Deploy Streaming Project](deploy-project.png)

  4. Start the adapter by running `start.sh` and pass it the full path to your `adapter_config.xml`.

    `%STREAMING_HOME%\adapters\framework\bin\start.bat`
    `%STREAMING_HOME%\adapters\framework\instances\mqtt_input\adapter_config.xml`

  5. The adapter will use the `<EspProject>` element properties set in your `adapter_config.xml` file to connect to the project in studio and will use the `<MQTTInputTransporterParameters>` element properties as arguments.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Debug a Custom Adapter](&nbsp;)]

There are two main ways to debug custom adapters:

  1. The first is to use the `Eclipse` debugger. Steps for accomplishing this are outlined in this documentation. This debugger can be used to set break points and/or step through code.

  2. The second is to simply use print statements.

     - For adapter run using the `ATTACH` statement

         Use `utility.getAdapterLogger().info(String)` in your custom `Transporter` and `Formatter` modules to log information.

         Streaming Analytics log info can be found using HANA Studio.

         - In the `SAP HANA Administration Console` perspective, double click your `SYSTEM` in the Systems panel

         - Navigate to the `Diagnosis Files` tab.

         - Look for a `.out` file corresponding to the project you are working on. You can open the file for read in HANA Studio by double clicking it.

         - `utility.getAdapterLogger().info()` will log messages to `<working-directory>\ projects\default.mqtt.0\logs\adapterframework.log.`

     - For standalone adapter run without the `ATTACH` statement

        - `utility.getAdapterLogger().info()` can also be used

        - `utility.getAdapterLogger().info()` will log messages to the console in which the adapter start command was issued as well as `%STREAMING_HOME%/adapters/framework/bin/frameworkadapter.log`

        - `System.out.println()` messages will also appear in the console.

Click the **Done** button below once you have completed this tutorial.

[DONE]

[ACCORDION-END]

