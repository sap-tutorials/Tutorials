---
title: Editing Adapter Configuration Files
description: Edit a set of adapter configuration files in order to make the custom adapter available to both the HANA Streaming Analytics server at run time and in HANA Studio at design time.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Tutorial:** [Creating Custom Adapter Configuration Files](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part3.html)

## Next Steps
 - [Where to Place the Custom Adapter Files](https://developers.sap.com/tutorials/hsa-java-toolkit-adapter-part5.html)

## Details
### You will learn
 - Locate which files to edit locally on HANA Studio and on the Streaming Server
 - How to edit the `modulesdefine` and `custommodulesdefine` `.xml` files
 - How to edit the `parametersdefine.xml` file
 - To transfer the files edited on the Streaming Server to the client where the HANA Studio is running

### Time to Complete
**15 Min**

---

> **IMPORTANT!** Since you're running a `multidb` SAP HANA environment, `$STREAMING_CUSTOM_ADAPTERS_HOME` will refer to <pre> `/hana/shared/`<b>`<SID>`</b>`/streaming/cluster/`<b>`<tenant db>`</b>`/adapters`</pre> assuming you have used the default location of `/hana` as the root directory for the installation.

 For this step, we will be adding our custom modules to the `modulesdefine.xml` file and `custommodulesdefine.xml` file, and adding parameter definitions from the `.cnxml` file to `parametersdefine.xsd`. The `modulesdefine.xml` file is located in the `$STREAMING_HOME/adapters/framework/config` directory, the `custommodulesdefine.xml` file is located in the `$STREAMING_CUSTOM_ADAPTERS_HOME/config `directory, and the `parametersdefine.xsd` is located in the `$STREAMING_CUSTOM_ADAPTERS_HOME/config` directory. We are not creating these files from scratch. The purpose of the `modulesdefine.xml` file is to define our custom modules (`Transporter` and `Formatter`) so HANA Streaming Analytics knows what they do and where they reside.

 The `parametersdefine.xsd` file simply defines what elements are valid in an `adapter_config.xml` file (adapter configuration file). Since we have used a custom element – `MqttInputTransporterParameters` – in our `adapter_config.xml` file, we must define it in `parametersdefine.xsd`.


[ACCORDION-BEGIN [Step 1: ](Edit modulesdefine.xml and custommodulesdefine.xml)]

> The full source code for the `modulesdefine.xml` and `custommodulesdefine.xml` files are provided in the `Appendix` Section

  1. Open `modulesdefine.xml` in a text editor.

  2. We will add our Transporter module to the `TransporterDefnList`

      - Create a `<TransporterDefn>` element.

        ```XML
          <TransporterDefn>
        ```

      - Create a `<Name>` element. Specify the name of our custom Transporter class.

        ```XML
          <Name>MqttTransporter</Name>
        ```

      - Create a `<Class>` element.

        ```XML
          <Class>com.sap.MqttTransporter</Class>
        ```

      - Create an `<OutputData>` element.

        ```XML
          <OutputData>String</OutputData>
        ```

      - Close off the `<TransporterDefn>` element.

        ```XML
        </TransporterDefn>
        ```

  3. We will add our formatter module to the `formatterDefnList`

      - Create a `<FormatterDefn>` element.

        ```XML
          <FormatterDefn>
        ```

      - Create a `<Name>` element. Specify the name of our custom `Transporter` class.

        ```XML
          <Name>MqttFormatter</Name>
        ```

      - Create a `<Class>` element.

        ```XML
          <Class>com.sap.MqttFormatter</Class>
        ```

      - Create an `<InputData>` element.

        ```XML
          <InputData>String</InputData>
        ```

      - Create an `<OutputData>` element.

        ```XML
          <OutputData>Esp</OutputData>
        ```

      - Close off the `<FormatterDefn>` element.

        ```XML
          </FormatterDefn>
        ```

  4. Repeat steps 1 to 3 for `custommodulesdefine.xml`.

For the question below, select the correct answer, and click **Validate**.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Edit parametersdefine.xsd)]

> The full source code for the `parametersdefine.xsd` file is provided in the `Appendix` Section

  1. Open `parametersdefine.xsd` in a text editor.

  2. Add the following line to the `<xs:choice>` element

    ```XML
      <xs:element name="MQTTInputTransporterParameters" type="MQTTInputTransporterParametersDefn"/>
    ```

  3. Since we have defined the `MQTTInputTransporterParameters` type to be `MQTTInputTransporterParametersDefn`, we must now set up a rule for it.

    - Create a <xs:complex> element and specify the name attribute.

      ```XML
        <xs:complexType name="MQTTInputTransporterParametersDefn">
      ```

    - Create an `<xs:all>` element.

    - Create an `<xs:element>` element for each `MQTTInputTransporterParametersDefn` parameter (`Topic` and `MosquittoServerAddress`) and specify name and type attributes for each.

      ```XML
        <xs:element name="MosquittoServerAddress" type="xs:string"></xs:element> <xs:element name="Topic" type="xs:string"></xs:element>
      ```

    - Close off the `<xs:all>` and `<xs:complexType>` elements.

      ```XML
          </xs:all>
        </xs:complexType>
      ```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix ](&nbsp;)]

`modulesdefine.xml`

> This is not the full markup of the file. Open your `modulesdefine.xml` file in a text editor and add the `TransporterDefn` and `FormatterDefn` elements of the markup shown below.

```XML

<?xml version="1.0" encoding="utf-8"?>

<ModulesDefinition>
  <TransporterDefnList>
    …
    …     
     <TransporterDefn>
       <Name>MqttTransporter</Name>
       <Class>com.sap.MqttTransporter</Class>
       <OutputData>String</OutputData>
     </TransporterDefn>

  </TransporterDefnList>

  <FormatterDefnList>
     …
     …
     <FormatterDefn>
       <Name>MqttFormatter</Name>
       <Class>com.sap.MqttFormatter</Class>
       <InputData>String</InputData>
       <OutputData>ESP</OutputData>
     </FormatterDefn>

  </FormatterDefnList>

 …

</ModulesDefinition>

```

`custommodulesdefine.xml`

> This is not the full markup of the file. Open your `custommodulesdefine.xml` file in a text editor and add the `TransporterDefn` and `FormatterDefn` elements of the markup shown below.

```HTML
<?xml version="1.0" encoding="utf-8"?>

<ModulesDefinition>
  <TransporterDefnList>
    …
    …
    <TransporterDefn>
      <Name>MqttTransporter</Name>
      <Class>com.sap.MqttTransporter</Class>
      <OutputData>String</OutputData>
    </TransporterDefn>

  </TransporterDefnList>

  <FormatterDefnList>
    …
    …
    <FormatterDefn>
      <Name>MqttFormatter</Name>
      <Class>com.sap.MqttFormatter</Class>
      <InputData>String</InputData>
      <OutputData>ESP</OutputData>
    </FormatterDefn>

 </FormatterDefnList>

…

</ModulesDefinition>

```

`paramtersdefine.xsd`

> This is not the full markup of the file. Open your `custommodulesdefine.xml` file in a text editor and add the `xs:element` and `xs:complexType` elements of the markup shown below.

```HTML

<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified">
  <xs:include schemaLocation="standard_module_parametersdefine.xsd"/>
  <xs:complexType name="DefinedTypeParameters">
    <xs:sequence>
    <xs:choice minOccurs="0" maxOccurs="1">

      …

        <xs:element name="MQTTInputTransporterParameters"
        type="MQTTInputTransporterParametersDefn"/>

        …

      </xs:choice>
    </xs:sequence>
  </xs:complexType>

 …

  <xs:complexType name="MQTTInputTransporterParametersDefn">
    <xs:all>
      <xs:element name="MosquittoServerAddress" type="xs:string"></xs:element>
      <xs:element name="Topic" type="xs:string"></xs:element>
    </xs:all>
  </xs:complexType>

…

</xs:schema>

```

[DONE]

[ACCORDION-END]

