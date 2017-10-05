---
title: Editing Module Configuration Files
description:
primary_tag: products>sap-hana-smart-data-streaming
tags: [  tutorial>intermediate, topic>internet-of-things, products>sap-hana-smart-data-streaming, products>sap-hana\,-express-edition   ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorial**: [Writing a Formatter Module](http://www.sap.com)


## Next Steps
**Tutorials:** [Creating an Adapter Configuration File](http://sap.com)

## Details
### You will learn
 - Where to find various module files
 - The purpose of various module files
 - How to add Transporter and Formatter modules

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Introduction)]

For this tutorial, we will be adding our custom modules to the `modulesdefine.xml` file and `custommodulesdefine.xml` file, and adding parameter definitions from the `.cnxml` file to `parametersdefine.xsd`.

The `modulesdefine.xml` file is located in the `$STREAMING_HOME/adapters/framework/config` directory, the `custommodulesdefine.xml` file is located in the `$STREAMING_CUSTOM_ADAPTERS_HOME/config` directory, and the `parametersdefine.xsd` is located in the `$STREAMING_CUSTOM_ADAPTERS_HOME/config` directory. We are not creating these files from scratch. The purpose of the `modulesdefine.xml` file is to define our custom modules (Transporter and Formatter) so HANA Studio knows what they do and where they reside.

The `parametersdefine.xsd` file simply defines what elements are valid in an `adapter_config.xml` file (adapter configuration file). Since we have used a custom element – `MqttInputTransporterParameters` – in our `adapter_config.xml` file, we must define it in `parametersdefine.xsd`.



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add our Transporter Module to the TransporterDefn List)]

**a.** First, open `modulesdefine.xml` in a text editor.

**b.** Create a `<TransporterDefn>` element.

**c.** Create a `<Name>` element. Specify the name of our custom Transporter class.
```html
<Name>MqttTransporter</Name>
```

**d**. Create a `<Class>` element.
```html
<Class>com.sap.MqttTransporter</Class>
```

**e.** Create an `<OutputData>` element.
```html
<OutputData>String</OutputData>
```

**f.** Close off the `<TransporterDefn>` element.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add our Formatter Module to the FormatterDefnList)]

**a.** Create a `<FormatterDefn>` element.

**b.** Create a `<Name>` element. Specify the name of our custom `Transporter` class.
```html
<Name>MqttFormatter</Name>
```

**c.** Create a `<Class>` element.
```html
<Class>com.sap.MqttFormatter</Class>
```

**d.** Create an `<InputData>` element.
```html
<InputData>String</InputData>
```

**e.** Create an `<OutputData>` element.
```html
<OutputData>Esp</OutputData>
```

**f.** Close off the `<FormatterDefn>` element.

Repeat steps 2 and 3 with ` custommodulesdefine.xml`.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Edit parametersdefine.xsd)]
**a.** Open `parametersdefine.xsd` in a text editor.

**b.** Add the following line to the `<xs:choice>` element
```html
<xs:element name="MQTTInputTransporterParameters"
type="MQTTInputTransporterParametersDefn"/>
```

 Since we have defined the `MQTTInputTransporterParameters` type to be
`MQTTInputTransporterParametersDefn`, we must now set up a rule for it.

**c.** Create a `<xs:complex>` element and specify the name attribute.
```html
<xs:complexType name="MQTTInputTransporterParametersDefn">
```

**d.** Create an `<xs:all>` element.

**e.** Create an `<xs:element>` element for each `MQTTInputTransporterParametersDefn` parameter
(`Topic` and `MosquittoServerAddress)` and specify name and type attributes for each.
```html
<xs:element name="MosquittoServerAddress"
type="xs:string"></xs:element>
<xs:element name="Topic" type="xs:string"></xs:element>
```

**f.** Close off the `<xs:all>` and `<xs:complexType>` elements.
```html
 </xs:all>
 </xs:complexType>
```


[ACCORDION-END]
## Next Steps
**Tutorials:** [Creating an Adapter Configuration File](http://sap.com)
