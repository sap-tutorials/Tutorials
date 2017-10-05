---
title: Creating a cnxml Configuration Files
description: Create a .cnxml configuration file for your custom external adapter so that you can configure the adapter in the SAP HANA Streaming Development perspective, and start and stop it with a project for smart data streaming.
primary_tag: products>sap-hana-smart-data-streaming
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **System:** Ensure you have access to a HANA System with the Smart Data Streaming option installed and configured.
 - **Tutorial**: [Writing a Custom Java Toolkit Adapter for SAP HANA Smart Data Streaming](http://www.sap.com)

## Next Steps
- **Tutorial:** [Writing a Transporter Module](http://www.sap.com)

## Details
### You will learn
 - How to create a `.cnxml` configuration file for your custom external adapter
 - How to specify attributes for the Adapter, Library, Special and Section sections.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Specify Attributes for the Adapter Section)]

We will now be writing a configuration file that we will name `mqtt_input.cnxml`. This follows the loose naming convention of `<adapter>_<{input,output}>.cnxml`.

The purpose of this `.cnxml` file is to define which parameters will be offered to the user and how Studio will start and stop the adapter.

First, we will create an `<Adapter>` element and specify attributes for it.

| Attribute   |  Description |
|---|---|
|type| (Required) Specify whether this is an input or output adapter  |
|external| (Required) Set to true, as this is an external adapter   |
|id|  (Required) Specify a unique identifier for your adapter. This value is listed in the type parameter within the `ATTACH ADAPTER` statement |
|label| (Required) The name of your adapter. This name appears in the SAP SDS Authoring perspective if you hover over the adapter icon in the visual editor  |
|description| (Required) Specify the purpose of the adapter. This is also visible in the SAP SDS Authoring perspective.  |

Our adapter is an external input adapter and we will not be supporting schema discover.

```html
<Adapter type>="input" external="true"
  id="mqtt_input"
  label="MQTT Input Adapter"
  descr="Listens for MQTT messages published in a specified topic."
  supportsDiscovery="false"
>
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Specify Attributes for the Library Section)]

|Attribute|Description|
|---|---|
|file|(Required) Always specify `simple_ext` as the value for external adapters|
|type|(Required) Specify `binary` as the value.|

```html
<Library file="simpleext" type="binary"/>
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Specify Internal Parameters for the Special Section)]

See the `$STREAMING_HOME/lib/adapters/simple_ext.cnxml.template` sample `cnxml` file for a complete list of internal parameters and usage details.

*Note*: Any commands specified in the `cnxml` file for an adapter cannot contain the following strings: `mv`, `rm`, or `del`.

**a.** Create an `x_allocateLocalSessionId <Internal>` element and specify attributes for it. This element will allow the adapter to connect to the project using a session id of its choosing.

```html
<Internal id="x_allocateLocalSessionId"
 label="Request a new local session id"
 descr="Request the project to allocate a new local session
id."
 type="boolean"
 default="true"
/>
```

**b.** Create an `x_unixCmdExec <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to start the adapter in a Unix environment.

```html
<Internal id="x_unixCmdExec"
 label="Execute Command"
 type="string"
 default="&quot;$STREAMING_HOME/adapters/framework/bin/start.sh&quot;&quot;$STREAMING_HOME/adapters/framework/instances/mqtt_input/adapter_config.xml&quot; &quot;DMQTTInputTransporterParameters.MosquittoServerAddress=$mosquittoServerAddress&quot; &quot;DMQTTInputTransporterParameters.Topic=$topic&quot;"
/>
```

**c.** Create an `x_winCmdExec <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to start the adapter in a Windows environment. The Smart Data Streaming server is not supported on Windows, but our custom adapter can work with SDS systems.

```html
<Internal id="x_winCmdExec"
 label="Execute Command"
 type="string"

default="&quot;%STREAMING_HOME%/adapters/framework/bin/start.bat&quot; &quot;%STREAMING_HOME%/adapters/framework/instances/mqtt_input/adapter_config.xml&quot; &quot;DMQTTInputTransporterParameters.MosquittoServerAddress=$MosquittoServerAddress&quot; &quot;DMQTTInputTransporterParameters.Topic=$Topic&quot;"
/>
```

**d.** Create an `x_unixCmdStop <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to stop the adapter in a Unix environment.

```html
<Internal id="x_unixCmdStop"
 label="Stop Command"
 type="string"

default="&quot;$STREAMING_HOME/adapters/framework/bin/stop.sh&quot; &quot;$STREAMING_HOME/adapters/framework/instances/mqtt_input/adapter_config.xml&quot;"
/>
```

**e**. Create an `x_winCmdStop <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to stop the adapter in a Windows environment.

```html
<Internal id="x_winCmdStop"
 label="Stop Command"
 type="string"
 default="&quot;%STREAMING_HOME%/adapters/framework/bin/stop.bat&quot; &quot;%STREAMING_HOME%/adapters/framework/instances/mqtt_input/adapter_config.xml&quot;"
/>
```

**f.** Close off the `<Special>` tag.

```html
</Special>
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Specify Adapter parameters for the Section element.)]

These parameters are visible and configurable in studio and the elements contained in this element will be the parameters
streaming developers can set for the adapter.

For each parameter, specify:

|Parameter|Description|
|---|---|
|id|(Required) The property ID that you can reference in `<Internal>`command calls with `$<varname>ID name</varname>`. This is what you reference when specifying adapter properties for an adapter in CCL.|
|label|(Required) The property name that appears in Studio.|
|`descr`|(Required) A description of the adapter property.|
|type|(Required) The property datatype.|
|use|(Required) Whether the property is required, optional, or advanced. In Studio, required properties appear in red and advanced ones appear on a separate tab.|
|default|(Optional) The default value to use, if you do not set a value for the property.|

**a.** Create a `mosquittoServerAddress <Parameter>` element and specify attributes for it.
```html
<Parameter id="mosquittoServerAddress"
 label="Mosquitto Server Address"
 descr="The address of the mosquitto server used as a broker"
 type="string"
 use="required"
 default="tcp://<your-ip-address>:1883"
/>
```

**b.** Create a topic `<Parameter>` element and specify attributes for it.
```html
<Parameter id="topic"
 label="MQTT Subscription Topic"
 descr="Topic to listen to for MQTT messages."
 type="string"
 use="required"
default="test"
/>
```


[ACCORDION-END]

## Next Steps
- **Tutorial:** [Writing a Transporter Module](http://www.sap.com)
