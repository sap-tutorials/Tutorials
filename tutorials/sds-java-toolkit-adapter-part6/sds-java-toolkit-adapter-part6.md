---
title: Creating an Adapter Configuration File
description:
primary_tag: products>sap-hana-smart-data-streaming
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorial**: [Editing Module Configuration Files](http://www.sap.com)


## Next Steps
**Tutorials:** [Accessing and Testing your Custom Adapter in Studio](http://sap.com)

## Details
### You will learn
 - How to create an Adapter Element
 - How to Specify a Connection to SDS
 - Where to place the files you created
### Time to Complete
**25 Min**

---

[ACCORDION-BEGIN [Step 1: ](Introduction)]

In this tutorial, we will write a file called `adapter_config.xml`, which will define the ordering of modules. For those interested in running a custom adapter in standalone mode, this configuration file will be the place to specify project, project security, server and parameter information. Since we will be running our adapter in managed mode, we will create a HANA Studio configuration file in the next section that will allow the user to specify these parameters.


[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Create an Adapter Element)]

**a.** Create an `<Adapter>` element and include all the elements from the steps below within it.

**b.** Add a `<Name>` element and specify a name for the adapter instance.
```html
<Name>MQTT Input</Name>
```

**c.** Add a `<Description>` element and specify the purpose of the adapter.
```html
<Description>Adapter to receive MQTT messages for a specified topic, transforms to SDS data format, and publishes to the SDS stream.</Description>
```

**d.** Add a `<Modules>` element. This element will contain the modules we have written for our adapter instance as well as define the `EspConnector` module we chose in the last step.

**e.** We will now define our modules. For each module, specify (below this table is step by step):

|Parameter| Description |
|---|---|
| `InstanceName` | Type: *string* <br/> (Required) Instance name of the specific module to use. For example, `MyInputTransporter`.|
| `Name` | Type: *string* <br/> (Required) The name of the module as defined in the `modulesdefine.xml` file. This should be a unique name. For example, `MyCustomInputTransporter`.|
| `Next` | Type: *string* <br/> (Required if another module follows this one) Instance name of the module that follows this one |
| `BufferMaxSize` |Type: *integer* <br/> (Advanced) Capacity of the buffer queue between this module and the next. The default value is 10240.|
| `Parallel` |Type: *boolean* <br/> (Optional; applies only to row-based formatters) If set to true, the module runs as a separated thread. If set to false, the module shares a thread with other modules. The default value is true. |
| `Parameters` | (Required) Parameters for the current module. For a custom module, the sub-element can reflect the name or type of the module, for example `<MyCustomInputTransporterParameters>`. <br/> `EspPublisher`, `EspMultiStreamPublisher`, `EspSubscriber`, and `EspMultiStreamSubscriber` all have set parameters that are configured specifically. |

**f.** Transporter module

  - **i.** First, create a `<Module>` element with a transporter type attribute.

```html
<Module type="transporter">
```

  - **ii.** Create an `<InstanceName>` element and specify the name of your transporter module class.
```html
<InstanceName>MqttTransporter</InstanceName>
```
  - **iii.** Create a `<Name>` element and specify the name of the module as defined in the `modulesdefine.xml` file
```html
<Name>MqttTransporter</Name>
```
  - **iv.** Create a `<Next>` element to specify which module will be called after the transporter.
```html
<Next>MqttFormatter</Next>
```
  - **v.** Create a `<Parameters>` element that will contain our `<MQTTInputTransporterParameters>` defined in `parametersdefine.xsd`. Note that these parameters will only be used when the adapter is started in standalone mode!
```html
<Parameters>
 <MQTTInputTransporterParameters>
```

  - **vi.** Specify parameter elements and values for them.
```html
<MosquittoServerAddress>tcp://<your-ip-address>:1883</MosquittoServerAddress>
<Topic>test</Topic>
```

  - **vii.** Close off elements of the `<Module>`.
```html
    </MQTTInputTransporterParameters>
 </Parameters>
</Module>
```

**g.** Formatter module

  - **i.** Follows steps `i` - `iv` of the Transporter module section.
```html
<Module type="formatter">
  <InstanceName>MqttFormatter</InstanceName>
  <Name>MqttFormatter</Name>
  <Next>EspPublisher</Next>
  <Parameters/>
</Module>
```
  - **ii.** We do not need to specify any parameters this time because our Formatter module does not require any.

**h.** `EspConnector` Module
The table below is specifically for `EspPublisher`. For `EspMultiStreamPublisher`, `EspSubscriber` or `EspMultiStreamSubscriber`, visit [SAP Official documentation](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789bb9c6f0f101490bef9e3c7dd5186.html)

| Parameter | Description |
|---|---|
| `ProjectName` | Type: *string* <br/> (Required if the adapter is running in unmanaged mode; optional if it is running in managed mode) The name of the smart data streaming project that the adapter is connected to. For example, `StreamingProject2`. <br/> This is the same project tag that you specify later in the adapter configuration file in the Name element within the smart data streaming (`EspProjects`) element.<br/> If you are starting the adapter with the smart data streaming project that it is attached to (that is, running the adapter in managed mode), you can simply comment out this element as the adapter automatically connects to the project instance that started it. |
| `EspPublisherParameters` | (Required) The element containing elements for the ESP publisher. |
| `StreamName` | Type: *string* <br/> (Required if the adapter is running in unmanaged mode; optional if it is running in managed mode) The name of the stream that the adapter publishes data to. <br/> If you are starting the adapter with the smart data streaming project that it is attached to (that is, running the adapter in managed mode), you can simply comment out this element as the adapter automatically connects to the project instance that started it. |
| `MaxPubPoolSize` | 	Type: *positive integer* <br/> (Optional) The maximum size of the record pool. Record pooling, also referred to as block or batch publishing, allows for faster publication since there is less overall resource cost in publishing multiple records together, compared to publishing records individually. <br/> Record pooling is disabled if this value is 1. The default value is 256. |
| `UseTransactions` | Type: boolean <br/> (Optional) If set to true, pooled messages are published to smart data streaming in transactions. If set to false, they are published in envelopes. The default value is false. |
| `SafeOps` | Type: boolean <br/>  (Advanced) Converts the opcodes `INSERT` and `UPDATE` to `UPSERT`, and converts `DELETE` to `SAFEDELETE`. The default value is false. |
| `SkipDels` | Type: boolean <br/> (Advanced) Skips the rows with `opcodes DELETE` or `SAFEDELETE`. The default value is false.|

  - **i.** Follow steps `i` – `iii` of the Transporter module section.
```html
<Module type="espconnector">
  <InstanceName>EspPublisher</InstanceName>
  <Name>EspPublisher</Name>
```
  - **ii.** There is no `Next` element because the `EspConnector` module is the end of our chain of calls. Create `<EspPublisherParameters>`.
```html
<Parameters>
  <EspPublisherParameters>
  <!--these are only to be used in standalone mode-->
  <!--<ProjectName>mqtt</ProjectName>
  <StreamName>InputWindow1</StreamName>-->
  </EspPublisherParameters>
</Parameters>
```
> Note: Set the values of `<ProjectName>` and `<StreamName>` to correspond with the project you are working with and uncomment them if you will be starting the adapter instance in standalone mode. These values will override the ones set by the streaming user in Studio if they are uncommented even if the adapter instance is started in managed mode.

  - **iii.** Close the `<Module>` element
```html
</Module>
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3 :](Specify a Connection to SDS)]
The information specified here will only be used if the `EspPublisherParameters` in the previous section are uncommented. The information is to be used when starting the adapter instance in standalone mode.

| XML Element | Description |
|---|---|
| `EspProjects` | (Required) Element containing elements for connecting to Smart Data Streaming. |
| `EspProject` | (Required) Element containing the `Name` and `Urielements`. Specifies information for the SDS project to which the adapter is connected. |
| `Name` | Type: *string* <br/> (Required) Specifies the unique project tag of the SDS project which the `EspConnector` (publisher/subscriber) module references.|
| `Uri` | Type: *string* <br/> (Required) Specifies the total project URI to connect to the SDS project. For example, `esps://<host>:3<instance-number>26/ws1/p1.` |
| `Security` | (Required) Element containing all the following authentication elements. The details for the authentication method used for smart data streaming. |
| `User` | 	Type: *string* <br/> (Required) The username required to log in to smart data streaming (see `AuthType`). No default value. |
| `Password` | Type: *string* <br/> (Required) The password required to log in to smart data streaming (see `AuthType`). <br/> Includes an "encrypted" attribute indicating whether the Password value is encrypted. The default value is false. If "encrypted" is set to true and `EncryptionAlgorithm `is set to RSA, the password value is decrypted using `RSAKeyStore` and `RSAKeyStorePassword`. If "encrypted" is set to true and `EncryptionAlgorithm` is not set, the password is decrypted using the cipher key. |
| `AuthType` | Type: *string* <br/> (Required) Method used to authenticate to smart data streaming. Valid value is `user_password` for SAP HANA username and password authentication. |
| `RSAKeyStore` | Type: string <br/> (Dependent required) The location of the RSA keystore, and decrypts the password value. |
| `RSAKeyStorePassword` | Type: string <br/> (Dependent required) The keystore password, and decrypts the password value. Required if the encrypted attribute for Password is set to true and `EncryptionAlgorithm` is set to RSA. |
| `EncryptionAlgorithm` | Type: *string* <br/> (Optional) Used when the encrypted attribute for Password is set to true. If set to RSA, Password is decrypted using `RSAKeyStore` and `RSAKeyStorePassword`. If not set, Password is decrypted using the cipher key. |

**a.** Create `<EspProjects>` and `<EspProject>` elements.
```html
<EspProjects>
 <EspProject>
```

**b.** Create a `<Name>` element and specify the name of your project in Studio. This must be the same value as specified in the `EspPublisherParameters` `<ProjectName>` element.
```html
<Name>mqtt</Name>
```

**c.** Create a `<URI>` element and specify the `URI` used to connect to the streaming project. The address of the streaming server should be followed by `/workspace/project-name`.
```html
<Uri>esps://<streaming-server-address>:<port 3XX26>/default/mqtt</Uri>
```

**d.** Create a `<Security>` element. This element contains all of the authentication details for your streaming server.
```html
<Security>
<User>studio</User>
<Password encrypted="false">password</Password>
<AuthType>user_password</AuthType>
```

 </Security>
**e.** Close off the `<EspProjects>` element.
```html
 </EspProject>
</EspProjects>
```

**f.** Add a `<GlobalParameters>` element. We will not be specifying any global parameters.
```html
<GlobalParameters></GlobalParameters>
```

**g.** Close off the `<Adapter>` element.
```html
</Adapter>
```
Official SAP documentation for writing an adapter configuration file can be found in the Official SAP [documentation](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789bb9c6f0f101490bef9e3c7dd5186.html).


[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ](Where to Place the Files for the Custom Adapter)]
The files for the custom adapter include the:
- `mqtt-input.jar` and `org.eclipse.paho.client.mqttv3-<version>.jar` files
- `adapter_config.xml` file
- `mqtt_input.cnxml` file
- edited versions of the `modulesdefine.xml`, `custommodulesdefine.xml`, and `parametersdefine.xsd` files

*Note*: If you want to run a project from a streaming server that uses the custom adapter, these files need to be present (same paths) on the streaming server as well as the local machine from which you are accessing Studio.

The directory paths for these files will be relative to the `STREAMING_HOME` environment variable. The streaming home environment variable is `%STREAMING_HOME%` for Windows and `$STREAMING_HOME` for Unix operating systems (please edit the below paths according to your intention). The rest of the directory path relative to the `STREAMING_HOME` environment variable is the same on both `Linux` and `Windows`.

> Note: Assuming you used the default install location, then the local Windows SDS `Streaming_Home` will be `C:\ProgramFiles\SAP\hdbstudio\plugins\com.sybase.cep.studio.native.bins_<version>.v<timestamp>`.

| Files | New Path |
|---|---|
| `mqtt-input.jar` <br/> `org.eclipse.paho.client.mqttv3-<version>.jar`|`$STREAMING_CUSTOM_ADAPTERS_HOME/libj` |
| `adapter_config.xml`  |`$STREAMING_HOME/adapters/framework/instances/mqtt_input` <br/> *Note:* You will need to create the `mqtt_input` folder |
| `mqtt_input.cnxml`  | `$STREAMING_CUSTOM_ADAPTERS_HOME /cnxml ` |
| `modulesdefine.xml` | `$STREAMING_HOME/adapters/framework/config` |
| `custommodulesdefine.xml` <br/> `parametersdefine.xsd` | `$STREAMING_CUSTOM_ADAPTERS_HOME /config` |




[ACCORDION-END]
## Next Steps
**Tutorials:** [Accessing and Testing your Custom Adapter in Studio](http://sap.com)
