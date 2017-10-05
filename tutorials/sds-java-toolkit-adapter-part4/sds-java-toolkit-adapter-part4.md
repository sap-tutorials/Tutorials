---
title: Writing a Formatter Module
description:
primary_tag: products>sap-hana-smart-data-streaming
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorial**: [Writing a Transporter Module](https://www.sap.com)


## Next Steps
**Tutorials:** [Editing Module Configuration Files](https://www.sap.com)

## Details
### You will learn
 - About the purpose of a formatter module
 - The types of formatters supported by SDS
 - How to implement the three methods in row-based formatters.
 - How to choose an `EspConnector` Module
### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Introduction)]

A formatter module converts between the data format of the transporter module and smart data streaming.

SAP HANA smart data streaming supports two types of formatters: row-based and stream-based formatters.

Row-based formatters obtain and output data in row format. They work with `AdapterRow` instances, which are containers for one or more records or rows as they flow from one module (transporter, formatter, or smart data streaming connector) to the next. You can add multiple records as objects within a List of a single `AdapterRow` object. The `AdapterRow` has a timestamp and block flags that control how its records are communicated to and from smart data streaming.

Stream-based formatters deal with streaming data. These formatters work with `ByteStream` instances, which represent a continuous stream of data. Official SAP documentation for writing Formatter modules can be found in the Official SAP [documentation](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789b6606f0f10149915c3cb6a302153.html).

Before we begin, you can check out the `$STREAMING_HOME/adapters/framework/examples/src` directory for source code of sample formatters.



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the Custom Adapter project)]

**a.** Start by opening your IDE and navigating to the java project called `mqtt-input` that we created for our Transporter Module (complete the pre-requisite tutorial if you have not yet completed this). Note that it is also valid to create a new project and create your Formatter module separately but for simplicity, we will be creating them in the same project.

**b.** Create a Java class called `Mqttformatter.java`

**c.** Have `MqttFormatter` extend the `RowFormatter` class. Note that it is also possible to create a custom Formatter module that extends the `StreamingFormatter` class. For the purposes of this tutorial, we will be extended the `RowFormatter` class. Similar to our Transporter module, we will have to implement a number of abstract methods (only 3 this time though).


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Implement the void init() method)]

The purpose of the `void init()` method is to prepare the formatter module to convert between data formats. For example, obtain properties from the adapter configuration file and perform any required initialization tasks. Our Formatter module is very simple and does not require any initialization instructions.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Implement the AdapterRow method)]
The second method is `AdapterRow convert(AdapterRow in)`.

**a.** First, we will test whether the received `AdapterRow` is non-empty. If this is the case, we will simply send the `AdapterRow` back.

```java
if (in.getDataList().isEmpty()){
 return in;
}
```

**b.** If we have reached this point in the method, the received `AdapterRow` is non-empty. Our particular Formatter will convert a `MQTT` message (String) to something usable by `SDS` - an `AepRecord`. First, we will create the desired `AepRecord.`

 - **i.** Create a new `AepRecord`.
```java
    AepRecord tempRecord = new AepRecord();
```
 - **ii.** Set the operation of the record.
```java
  tempRecord.setOpCode(Operation.INSERT);
```
  - **iii.** Get the data list inside the `AepRecord`, and add the first object of the data list in the `AdapterRow`.
```java
  tempRecord.getValues().add(in.getData());
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Convert the received AepRecord)]
Now that we have the` AepRecord` and would like to send it to Smart Data Streaming, we will convert the received `AdapterRow` by replacing its data list value at index 0 with the new
`AepRecord` - `tempRecord`.

```java
in.setData(0, tempRecord);
```

We have now finished converting the `MQTT String` to an `AepRecord` so we will return it.

```java
return in;
```

The last method to implement is `void destroy()` which is intended for performing clean-up actions for the formatter. Our formatter does not require any destroy instructions.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Once Both Modules are Complete)]

Now that we have written our `Transporter` and `Formatter` modules, we need to package them in a `.jar` file. If you have been following this tutorial, you should have a single java project containing `MqttTransporter.java`, `MqttFormatter.java` and `MqttCB.java`. Build a `.jar` containing all of these files. The process for doing so varies with IDE so, if you have questions, it is best to consult your IDE help pages. Before building the `.jar` file, verify that you will be building it with the same `JRE` version as included in your `SDS` install. Name the newly created `.jar` file `mqtt-input.jar`.

> Note: It is also possible to create two separate `.jar` files (one for `MqttTransporter` and one for `MqttFormatter`), however, since `MqttTransporter` relies on `MqttCB`, the two must be packaged together.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Choosing an EspConnector Module)]

The `EspConnector` modules are responsible for connecting to Smart Data Streaming. Connector module types include `EspSubscriber`, `EspMultiStreamSubscriber`, `EspPublisher`, and `EspMultiStreamPublisher`.

For output adapters, refer to the [`EspSubscriber` module](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789a6ef6f0f1014b1e1cea4b3addee8.html) or the [`EspMultiStreamSubscriber` module](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e7855da66f0f1014a43ad7ab965faf16.html) for an
output adapter that subscribes to multiple streams.

**`EspPublisher` - for Input Adapters**
The `EspPublisher` module obtains data from a transporter or formatter module and publishes it to the Smart Data Streaming project. Specify values for the `EspPublisher` module in the adapter configuration file. Specify this module for the input adapter only. We will be using `EspPublisher` for our `MQTT` input adapter but please refer to the [`EspMultiStreamPublisher` documentation](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e785596a6f0f1014b7feaa83516a2b58.html) for an input adapter that publishes to multiple streams.

To configure the adapter for standalone mode, refer to the [`EspPublisher` documentation](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789d78e6f0f101481a4c25c27b34613.html). Since we will be running our adapter in managed mode, we do not need to worry about this. We will, however, have to specify which `EspConnector` we would like to use in the configuration file in the next tutorial.

[ACCORDION-END]
## Next Steps
**Tutorials:** [Editing Module Configuration Files](https://www.sap.com)
