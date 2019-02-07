---
title: Creating Custom Adapter Configuration Files
description: Create a .cnxml file and an Adapter Configuration File(.xml) for your Custom Adapter.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Tutorial:** [Creating Custom Adapter Modules](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part2.html)

## Next Steps
 - [Editing Adapter Configuration Files](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part4.html)

## Details
### You will learn
 - How to create a `.cnxml` configuration file
 - How to create an Adapter Configuration File(`.xml`)

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create a `.cnxml` configuration file)]

We will now be writing a configuration file that we will name `mqtt_input.cnxml`. This follows the loose naming convention of `<adapter>_<{input,output}>.cnxml`.

The purpose of this `.cnxml` file is to define which parameters will be offered to the user and how Studio will start and stop the adapter.

> The full source code for the `.cnxml` configuration file is provided in the `Appendix` Section

First, we will create an `<Adapter>` element and specify attributes for it.

| Attribute   |  Description |
|---|---|
|type| (Required) Specify whether this is an input or output adapter  |
|external| (Required) Set to true, as this is an external adapter   |
|id|  (Required) Specify a unique identifier for your adapter. This value is listed in the type parameter within the `ATTACH ADAPTER` statement |
|label| (Required) The name of your adapter. This name appears in the SAP Streaming Analytics Authoring perspective if you hover over the adapter icon in the visual editor  |
|description| (Required) Specify the purpose of the adapter. This is also visible in the SAP Streaming Analytics Authoring perspective.  |

Our adapter is an external input adapter and we will not be supporting schema discover.

```html
<Adapter type="input" external="true"
  id="mqtt_input"
  label="MQTT Input Adapter"
  descr="Listens for MQTT messages published in a specified topic."
  supportsDiscovery="false"
>
```

Then, create a `<Library>` element and specify attributes for it.

|Attribute|Description|
|---|---|
|file|(Required) Always specify `simple_ext` as the value for external adapters|
|type|(Required) Specify `binary` as the value.|

```html
<Library file="simpleext" type="binary"/>
```

Create a special `<Special>` element and specify internal parameters for it. See the `$STREAMING_HOME/lib/adapters/simple_ext.cnxml.template` sample `.cnxml` file for a complete list of internal parameters and usage details.

```html
<Special>
```

> Any commands specified in the `.cnxml` file for an adapter cannot contain the following strings: `mv`, `rm`, or `del`.

Create an `x_allocateLocalSessionId <Internal>` element and specify attributes for it. This element will allow the adapter to connect to the project using a session id of its choosing.

```html
<Internal id="x_allocateLocalSessionId"
 label="Request a new local session id"
 descr="Request the project to allocate a new local session
id."
 type="boolean"
 default="true"
/>
```

Create an `x_unixCmdExec <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to start the adapter in a Unix environment.

```html
<Internal id="x_unixCmdExec"
 label="Execute Command"
 type="string"
 default="&quot;$STREAMING_HOME/adapters/framework/bin/start.sh&quot;&quot;$STREAMING_HOME/adapters/framework/instances/mqtt_input/adapter_config.xml&quot; &quot;DMQTTInputTransporterParameters.MosquittoServerAddress=$mosquittoServerAddress&quot; &quot;DMQTTInputTransporterParameters.Topic=$topic&quot;"
/>
```

Create an `x_winCmdExec <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to start the adapter in a Windows environment. The Streaming Analytics server is not supported on Windows, but our custom adapter can work with Streaming Analytics systems.

```html
<Internal id="x_winCmdExec"
 label="Execute Command"
 type="string"

default="&quot;%STREAMING_HOME%/adapters/framework/bin/start.bat&quot; &quot;%STREAMING_HOME%/adapters/framework/instances/mqtt_input/adapter_config.xml&quot; &quot;DMQTTInputTransporterParameters.MosquittoServerAddress=$MosquittoServerAddress&quot; &quot;DMQTTInputTransporterParameters.Topic=$Topic&quot;"
/>
```
Create an `x_unixCmdStop <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to stop the adapter in a Unix environment.

```html
<Internal id="x_unixCmdStop"
 label="Stop Command"
 type="string"

default="&quot;$STREAMING_HOME/adapters/framework/bin/stop.sh&quot; &quot;$STREAMING_HOME/adapters/framework/instances/mqtt_input/adapter_config.xml&quot;"
/>
```

Create an `x_winCmdStop <Internal>` element and specify attributes for it. This is the command that the adapter framework will use to stop the adapter in a Windows environment.

```html
<Internal id="x_winCmdStop"
 label="Stop Command"
 type="string"
 default="&quot;%STREAMING_HOME%/adapters/framework/bin/stop.bat&quot; &quot;%STREAMING_HOME%/adapters/framework/instances/mqtt_input/adapter_config.xml&quot;"
/>
```

Close off the `<Special>` tag.

```html
</Special>
```
Now, we will specify adapter parameters for the Section element.

These parameters are visible and configurable in studio and the elements contained in this element will be the parameters streaming developers can set for the adapter.

For each parameter, specify:

|Parameter|Description|
|---|---|
|id|(Required) The property ID that you can reference in `<Internal>`command calls with `$<varname>ID name</varname>`. This is what you reference when specifying adapter properties for an adapter in CCL.|
|label|(Required) The property name that appears in Studio.|
|`descr`|(Required) A description of the adapter property.|
|type|(Required) The property datatype.|
|use|(Required) Whether the property is required, optional, or advanced. In Studio, required properties appear in red and advanced ones appear on a separate tab.|
|default|(Optional) The default value to use, if you do not set a value for the property.|

Create a `mosquittoServerAddress <Parameter>` element and specify attributes for it.

> Don't forget to replace `<your-ip-address>` with your client's `ip address`.

```html
<Parameter id="mosquittoServerAddress"
 label="Mosquitto Server Address"
 descr="The address of the mosquitto server used as a broker"
 type="string"
 use="required"
 default="tcp://<your-ip-address>:1883"
/>
```

Create a topic `<Parameter>` element and specify attributes for it.
```html
<Parameter id="topic"
 label="MQTT Subscription Topic"
 descr="Topic to listen to for MQTT messages."
 type="string"
 use="required"
default="test"
/>
```

Close the `<Section>` and `<Adapter>` tags.
```html
</Section>
</Adapter>
```

For the question below, select the correct answer, and click **Validate**.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an Adapter Configuration File(.xml))]

In this step, we will write a file called `adapter_config.xml`, which will define the ordering of modules. For those interested in running a custom adapter in standalone mode, this configuration file will be the place to specify project, project security, server and parameter information. Since we will be running our adapter in managed mode, we will create a HANA Studio configuration file in the next section that will allow the user to specify these parameters.

> The full source code for the Adapter Configuration File(`.xml`) is provided in the `Appendix` Section

  1. Start by creating an `<Adapter>` element and include all the elements from the steps below within it.

  2. Add a `<Name>` element and specify a name for the adapter instance.

    ```html
    <Name>MQTT Input</Name>
    ```

  3. Add a `<Description>` element and specify the purpose of the adapter.

    ```html
    <Description>Adapter to receive MQTT messages for a specified topic, transforms to Streaming Analytics data format, and publishes to the Streaming Analytics stream.</Description>
    ```

  4. Add a `<Modules>` element. This element will contain the modules we have written for our adapter instance as well as define the `EspConnector` module we chose in the last step.

  5. We will now define our modules. For each module, specify (below this table is step by step):

    |Parameter| Description |
    |---|---|
    | `InstanceName` | Type: *string* <br/> (Required) Instance name of the specific module to use. For example, `MyInputTransporter`.|
    | `Name` | Type: *string* <br/> (Required) The name of the module as defined in the `modulesdefine.xml` file. This should be a unique name. For example, `MyCustomInputTransporter`.|
    | `Next` | Type: *string* <br/> (Required if another module follows this one) Instance name of the module that follows this one |
    | `BufferMaxSize` |Type: *integer* <br/> (Advanced) Capacity of the buffer queue between this module and the next. The default value is 10240.|
    | `Parallel` |Type: *boolean* <br/> (Optional; applies only to row-based formatters) If set to true, the module runs as a separated thread. If set to false, the module shares a thread with other modules. The default value is true. |
    | `Parameters` | (Required) Parameters for the current module. For a custom module, the sub-element can reflect the name or type of the module, for example `<MyCustomInputTransporterParameters>`. <br/> `EspPublisher`, `EspMultiStreamPublisher`, `EspSubscriber`, and `EspMultiStreamSubscriber` all have set parameters that are configured specifically. |

  6. Transporter module

      - First, create a `<Module>` element with a transporter type attribute.

        ```html
        <Module type="transporter">
        ```

      - Create an `<InstanceName>` element and specify the name of your transporter module class.

        ```html
        <InstanceName>MqttTransporter</InstanceName>
        ```

      - Create a `<Name>` element and specify the name of the module as defined in the `modulesdefine.xml` file

        ```html
        <Name>MqttTransporter</Name>
        ```

      - Create a `<Next>` element to specify which module will be called after the transporter.

        ```html
        <Next>MqttFormatter</Next>
        ```

      - Create a `<Parameters>` element that will contain our `<MQTTInputTransporterParameters>` defined in `parametersdefine.xsd`. Note that these parameters will only be used when the adapter is started in standalone mode!

        ```html
        <Parameters>
         <MQTTInputTransporterParameters>
        ```

      - Specify parameter elements and values for them. Don't forget to replace `<your-ip-address>` with your client's `ip address`.

        ```html
        <MosquittoServerAddress>tcp://<your-ip-address>:1883</MosquittoServerAddress>
        <Topic>test</Topic>
        ```

      - Close off elements of the `<Module>`.

        ```html
            </MQTTInputTransporterParameters>
         </Parameters>
        </Module>
        ```

  7. Formatter module

      - Follows steps `i` - `iv` of the Transporter module section.

        ```html
        <Module type="formatter">
          <InstanceName>MqttFormatter</InstanceName>
          <Name>MqttFormatter</Name>
          <Next>EspPublisher</Next>
          <Parameters/>
        </Module>
        ```

      - We do not need to specify any parameters this time because our Formatter module does not require any.

      - `EspConnector` Module

        The table below is specifically for `EspPublisher`. For `EspMultiStreamPublisher`, `EspSubscriber` or `EspMultiStreamSubscriber`, visit section [Configuring a New Adapter](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789bb9c6f0f101490bef9e3c7dd5186.html)

        | Parameter | Description |
        |---|---|
        | `ProjectName` | Type: *string* <br/> (Required if the adapter is running in unmanaged mode; optional if it is running in managed mode) The name of the Streaming Analytics project that the adapter is connected to. For example, `StreamingProject2`. <br/> This is the same project tag that you specify later in the adapter configuration file in the Name element within the Streaming Analytics (`EspProjects`) element.<br/> If you are starting the adapter with the Streaming Analytics project that it is attached to (that is, running the adapter in managed mode), you can simply comment out this element as the adapter automatically connects to the project instance that started it. |
        | `EspPublisherParameters` | (Required) The element containing elements for the ESP publisher. |
        | `StreamName` | Type: *string* <br/> (Required if the adapter is running in unmanaged mode; optional if it is running in managed mode) The name of the stream that the adapter publishes data to. <br/> If you are starting the adapter with the Streaming Analytics project that it is attached to (that is, running the adapter in managed mode), you can simply comment out this element as the adapter automatically connects to the project instance that started it. |
        | `MaxPubPoolSize` | 	Type: *positive integer* <br/> (Optional) The maximum size of the record pool. Record pooling, also referred to as block or batch publishing, allows for faster publication since there is less overall resource cost in publishing multiple records together, compared to publishing records individually. <br/> Record pooling is disabled if this value is 1. The default value is 256. |
        | `UseTransactions` | Type: boolean <br/> (Optional) If set to true, pooled messages are published to Streaming Analytics in transactions. If set to false, they are published in envelopes. The default value is false. |
        | `SafeOps` | Type: boolean <br/>  (Advanced) Converts the opcodes `INSERT` and `UPDATE` to `UPSERT`, and converts `DELETE` to `SAFEDELETE`. The default value is false. |
        | `SkipDels` | Type: boolean <br/> (Advanced) Skips the rows with `opcodes DELETE` or `SAFEDELETE`. The default value is false.|

      - Follow steps `i` – `iii` of the Transporter module section.

        ```html
        <Module type="espconnector">
          <InstanceName>EspPublisher</InstanceName>
          <Name>EspPublisher</Name>
        ```

      - There is no `Next` element because the `EspConnector` module is the end of our chain of calls. Create `<EspPublisherParameters>`.

        ```html
        <Parameters>
          <EspPublisherParameters>
          <!--these are only to be used in standalone mode-->
          <!--<ProjectName>mqtt</ProjectName>
          <StreamName>InputWindow1</StreamName>-->
          </EspPublisherParameters>
        </Parameters>
        ```
        > Set the values of `<ProjectName>` and `<StreamName>` to correspond with the project you are working with and uncomment them if you will be starting the adapter instance in standalone mode. These values will override the ones set by the streaming user in Studio if they are uncommented even if the adapter instance is started in managed mode.

      - Close the `<Module>` element

        ```html
        </Module>
        ```

        The information specified here will only be used if the `EspPublisherParameters` in the previous section are uncommented. The information is to be used when starting the adapter instance in standalone mode.

        | XML Element | Description |
        |---|---|
        | `EspProjects` | (Required) Element containing elements for connecting to Streaming Analytics. |
        | `EspProject` | (Required) Element containing the `Name` and `Urielements`. Specifies information for the Streaming Analytics project to which the adapter is connected. |
        | `Name` | Type: *string* <br/> (Required) Specifies the unique project tag of the Streaming Analytics project which the `EspConnector` (publisher/subscriber) module references.|
        | `Uri` | Type: *string* <br/> (Required) Specifies the total project URI to connect to the Streaming Analytics project. For example, `esps://<host>:3<instance-number>26/ws1/p1.` |
        | `Security` | (Required) Element containing all the following authentication elements. The details for the authentication method used for Streaming Analytics. |
        | `User` | 	Type: *string* <br/> (Required) The username required to log in to Streaming Analytics (see `AuthType`). No default value. |
        | `Password` | Type: *string* <br/> (Required) The password required to log in to Streaming Analytics (see `AuthType`). <br/> Includes an "encrypted" attribute indicating whether the Password value is encrypted. The default value is false. If "encrypted" is set to true and `EncryptionAlgorithm `is set to RSA, the password value is decrypted using `RSAKeyStore` and `RSAKeyStorePassword`. If "encrypted" is set to true and `EncryptionAlgorithm` is not set, the password is decrypted using the cipher key. |
        | `AuthType` | Type: *string* <br/> (Required) Method used to authenticate to Streaming Analytics. Valid value is `user_password` for SAP HANA username and password authentication. |
        | `RSAKeyStore` | Type: string <br/> (Dependent required) The location of the RSA keystore, and decrypts the password value. |
        | `RSAKeyStorePassword` | Type: string <br/> (Dependent required) The keystore password, and decrypts the password value. Required if the encrypted attribute for Password is set to true and `EncryptionAlgorithm` is set to RSA. |
        | `EncryptionAlgorithm` | Type: *string* <br/> (Optional) Used when the encrypted attribute for Password is set to true. If set to RSA, Password is decrypted using `RSAKeyStore` and `RSAKeyStorePassword`. If not set, Password is decrypted using the cipher key. |

  1. Create `<EspProjects>` and `<EspProject>` elements.

    ```html
    <EspProjects>
     <EspProject>
    ```

  2. Create a `<Name>` element and specify the name of your project in Studio. This must be the same value as specified in the `EspPublisherParameters` `<ProjectName>` element.

    ```html
    <Name>mqtt</Name>
    ```

  3. Create a `<URI>` element and specify the `URI` used to connect to the streaming project. The address of the streaming server should be followed by `/workspace/project-name`. Don't forget to replace `<streaming-server-address>` and `<port>` with your server's `ip address` and `port number`.

    ```html
    <Uri>esps://<streaming-server-address>:<port 3XX26>/default/mqtt</Uri>
    ```

  4. Create a `<Security>` element. This element contains all of the authentication details for your streaming server.

    ```html
    <Security>
    <User>studio</User>
    <Password encrypted="false">password</Password>
    <AuthType>user_password</AuthType>
    </Security>
    ```

  5. Close off the `<EspProjects>` element.

    ```html
     </EspProject>
    </EspProjects>
    ```

  6. Add a `<GlobalParameters>` element. We will not be specifying any global parameters.

    ```html
    <GlobalParameters></GlobalParameters>
    ```

  7. Close off the `<Adapter>` element.

    ```html
    </Adapter>
    ```

>Full documentation on writing a Formatter module can be found in the section [Configuring a New Adapter](https://help.sap.com/viewer/8280db55429040f8b665db30cf05a88e/2.0.01/en-US/e789bb9c6f0f101490bef9e3c7dd5186.html).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix ](&nbsp;)]

`mqtt_input.cnxml`

```XML
<?xml version="1.0" encoding="UTF-8"?>

<Adapter type="input" external="true"  
  id="mqtt_input"   
  label="MQTT Input Adapter"   
  descr="Listens for MQTT messages published in a specified topic."
  supportsDiscovery="false">

  <Library file="simpleext" type="binary"/>

  <Special>

    <Internal id="x_allocateLocalSessionId"        
      label="Request a new local session id"       
      descr="Request the project to allocate a new local session id.
      Then, the adapter could connect to the project using the new
      local session id depending on the adapter's choice"          
      type="boolean"       
      default="true"/>

    <Internal id="x_unixCmdExec"        
      label="Execute Command"        
      type="string"
      default="&quot;$STREAMING_HOME/adapters/framework/bin/start.sh
      &quot; &quot;$STREAMING_HOME/adapters/framework/instances/mqtt_input/adapter_config.xml &quot; &quot;
      DMQTTInputTransporterParameters.MosquittoServerAddress=$mosquittoServerAddress
      &quot; &quot;-DMQTTInputTransporterParameters.Topic=$topic&quot;"/>

    <Internal id="x_winCmdExec"
      label="Execute Command"
      type="string"
      default="&quot;%STREAMING_HOME%/adapters/framework/bin/start.bat
      &quot; &quot;%STREAMING_HOME%/adapters/framework/instances/mqtt_input/adapter_config.xml
      &quot; &quot;DMQTTInputTransporterParameters.MosquittoServerAddress=$MosquittoServerAddress
      &quot; &quot;-DMQTTInputTransporterParameters.Topic=$Topic&quot;"/>

    <Internal id="x_unixCmdStop"
      label="Stop Command"
      type="string"
      default="&quot;$STREAMING_HOME/adapters/framework/bin/stop.sh
      &quot; &quot;$STREAMING_HOME/adapters/framework/instances/mqtt_input/adapter_config.xml &quot;"/>

    <Internal id="x_winCmdStop"
      label="Stop Command"
      type="string"
      default="&quot;%STREAMING_HOME%/adapters/framework/bin/stop.bat
      &quot; &quot;%STREAMING_HOME%/adapters/framework/instances/mqtt_input/adapter_config.xml
      &quot;"/>

  </Special>

  <Section>  
    <Parameter id="mosquittoServerAddress"
      label="Mosquitto Server Address"
      descr="The address of the mosquitto server used as a broker"
      type="string"
      use="required"
      default="tcp://<your-ip-address>:1883"/>
    <Parameter id="topic"
      label="MQTT Subscription Topic"
      descr="Topic to listen to for MQTT messages."
      type="string"
      use="required"
      default="test"/>
  </Section>
</Adapter>

```

`adapter_config.xml`

```xml

<?xml version="1.0" encoding="utf-8"?>
<Adapter>
    <Name>MQTT Input</Name>
    <Description>Adapter to receive MQTT messages for a specified topic, transforms to Streaming Analytics data format, and publishes to Streaming Analytics.</Description>
    <Modules>
        <Module type="transporter">
            <InstanceName>MqttTransporter</InstanceName>
            <Name>MqttTransporter</Name>
            <Next>MqttFormatter</Next>
            <Parameters>
                <MQTTInputTransporterParameters>
                    <MosquittoServerAddress>tcp://<your-ipaddress>:1883</MosquittoServerAddress>
                    <Topic>test</Topic>
                </MQTTInputTransporterParameters>
            </Parameters>
        </Module>
        <Module type="formatter">
            <InstanceName>MqttFormatter</InstanceName>
            <Name>MqttFormatter</Name>
            <Next>EspPublisher</Next>
            <Parameters/>
        </Module>
        <Module type="espconnector">
            <InstanceName>EspPublisher</InstanceName>
            <Name>EspPublisher</Name>
            <Parameters>
                <EspPublisherParameters>
                    <!--these are only to be used in standalone mode-->
                    <!--<ProjectName>mqtt</ProjectName><StreamName>InputWindow1</StreamName>-->
                </EspPublisherParameters>
            </Parameters>
        </Module>
    </Modules>
    <EspProjects>
        <!-- The information here will only be used if the <EspPublisherParameters> are uncommented -->
        <EspProject>
            <Name>mqtt</Name>
            <Uri>esps://<streaming-server-ip-address>:<port 3XX26>/default/mqtt</Uri>
            <Security>
              <User>studio</User>
              <Password encrypted="false">password</Password>
              <AuthType>user_password</AuthType>
            </Security>
        </EspProject>
    </EspProjects>
    <GlobalParameters></GlobalParameters>
</Adapter>

```

[DONE]

[ACCORDION-END]

