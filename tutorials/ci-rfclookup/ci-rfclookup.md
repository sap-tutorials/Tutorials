---
author_name: Rafaela Nunes
author_profile: https://github.com/rafaelalvn
parser: v2
auto_validation: true
time: 30
tags: [  software-product>sap-integration-suite, software-product>cloud-integration, software-product>sap-process-integration, software-product>sap-process-orchestration, tutorial>intermediate  ]
primary_tag: software-product>sap-integration-suite
---

# How to migrate RFC Lookups from SAP Process Orchestration to Cloud Integration
<!-- description --> This tutorial explores two different solutions to implement RFC Lookup using the Cloud Integration capability of the SAP Integration Suite. 


## Intro

In SAP Process Orchestration, a Remote Function Call (RFC) Lookup is a feature that enables communication between different systems or components within the SAP landscape and it is used during the execution of a mapping program. The SAP Java Connector (SAP JCo) is a development library that enables a Java application to communicate with SAP systems using SAP's RFC protocol and it will be used in both solutions. For more information refer to the [SAP Java Connector](https://help.sap.com/doc/saphelp_gbt10/1.0/en-US/48/70792c872c1b5ae10000000a42189c/content.htm?no_cache=true).

## Prerequisites
- Ensure that the function module is available in your SAP system.

## You will learn
- How to implement RFC Lookup on Cloud Integration using Groovy Script and UDF (User-Defined Function) on Message Mapping.

### Implement RFC Calls in Cloud Integration Using Groovy Script

The first approach can be done by using a Groovy Script on Integration Flow. This Script dynamically retrieves the RFC destination and BAPI names from the properties, sets the RFC Request fields parameters from properties, executes the RFC call, and converts the response into XML format. 

Using a Groovy Script provides flexibility in implementation by allowing dynamic adjustment of import parameters based on exchange properties in the Integration Flow. This capability allows developers to adapt to many different RFC scenarios without change the Groovy Script. However, this Groovy Script just execute a single RFC call per call, so any need for executing multiple RFC calls would require changes to the code.

The implementation of this solution can be done by following the next steps:

1.  Create an Integration Flow on Cloud Integration.

2.  Add a content modifier called “Set Properties & RFC Fields” to define the following Exchange Properties:

    ![image1](Image1.png)  

    - RFCDestination: name of the RFC Destination you will use. (For more information refer to the: [Create RFC Destinations](https://help.sap.com/docs/connectivity/sap-btp-connectivity-cf/create-rfc-destinations)).

    *In this tutorial, the solution was implemented using the BAPI_FLIGHT_CHECKAVAILABILITY BAPI. Feel free to adjust the properties according to your specific requirements.*

    - BAPIName: Name of the BAPI you will use.
    - payload: After the request-reply step, it is necessary to retrieve the original incoming message. This step ensures that subsequent operations have access to the initial data. You can achieve this by using ${in.body}.
    - InputParameter_*: This is the prefix for the name of the RFC Request field. Each property should start with this prefix followed by the specific name of the RFC Request field. For each property, specify the XPath of the corresponding field to map from the source structure of the message mapping.

    ![image2](Image2.png) 
    
    *Note: You can add or remove properties as needed.*
    
3.  Create a new Groovy Script in the Integration Flow and paste the code below or import the following Groovy Script on the References.
    
    ![image3](Image3.png)

    The *runRFC* Groovy Script is the following:

    ```Groovy
        import com.sap.gateway.ip.core.customdev.util.Message
        import com.sap.conn.jco.*

        def Message processRFCCall(Message message) {
            def destinationName = message.getProperty("RFCDestination")
            def bapiName = message.getProperty("BAPIName")

            try {
                
                JCoDestination jcoDestination = JCoDestinationManager.getDestination(destinationName)
                JCoFunction function = jcoDestination.getRepository().getFunction(bapiName)

                if (function != null) {
                    def parameterList = function.getImportParameterList()

                    message.getProperties().each { key, value ->
                        if (key.startsWith("InputParameter_")) {
                            def paramName = key.substring("InputParameter_".length())
                            try {
                                parameterList.setValue(paramName, value)
                            } catch (Exception e) {
                                throw new Exception("Error setting parameter '${paramName}': ${e.getMessage()}")
                            }
                        }
                    }

                    function.execute(jcoDestination)

                    def result = function.toXML()
                    message.setBody(result)
                } else {
                    throw new Exception("ERROR: RFC function '${bapiName}' not found")
                }

            } catch (JCoException e) {
                throw new Exception("ERROR executing RFC function: ${e.getMessage()}")
            } catch (Exception e) {
                throw new Exception("Unexpected error: ${e.getMessage()}")
            }

            return message
        }
    ``` 

4.  Define a content modifier called 'RFC Response Fields' to capture and store the values of response fields extracted from the RFC.

    ![image4](Image4.png)
    
5. Add exchange properties (as needed) and specify the XPath of the RFC response fields.

    ![image5](Image5.png)

    *Note: You can add or remove properties as needed.*

6.  Create a message mapping by adding the source and target messages previously imported in the 'References' section of the Integration Flow. 

    ![image5](Image5B.png)

7. In this example, the source message is 'flightConnectionRequest' and the target message is 'flightConnectionResponse'. It was done a 1:1 mapping on the Connection node from 'flightConnectionResponse' and additionally, on the 'Seats' node, the 'getProperty' function was added, and a constant was defined using the output parameter from the previous step for the RFC response fields.

    ![image5](ImageMaping.png)

8.  Next, for the target fields required, add a constant from the standard functions and enter the name of the Exchange Property defined in the previous step.

    ![image6](Image6.png)

9.  Save the integration flow, deploy it, and trigger a message.

    ![image7](Image7.png)

    The mapping result is the following:

    ```XML
        <?xml version="1.0" encoding="UTF-8"?>
            <ns0:flightConnectionResponse xmlns:ns0="http://pimas.com">
                <Connection>
                    <Airline>AA</Airline>
                    <Connection>0017</Connection>
                    <Date>20230921</Date>
                </Connection>
                <Seats>
                    <Economy>15</Economy>
                    <Business>0</Business>
                    <First>2</First>
                </Seats>
            </ns0:flightConnectionResponse>
    ``` 

### Implement RFC Calls in Cloud Integration Using a UDF on Message Mapping

The second approach in Cloud Integration is done by using UDF on Message Mapping within an Integration Flow. This UDF performs similar tasks to the Groovy Script described in the previous steps. It retrieves the RFC destination and BAPI names dynamically from the exchange properties. However, the developers still need to edit and customize the UDF code to align with their specific requirements, for example, the RFC request and response fields. Additionally, the UDF just execute a single RFC call per execution. Consequently, if there's a need for multiple RFC calls, it would require changes to the UDF code.

On the other hand, if the message mapping includes multiple RFC calls, each call will require its own UDF code. As a result, the developers will need to create and adjust the UDF code for each RFC call, ensuring that it aligns with the specific requirements of the corresponding RFC request and response fields.

The implementation of this solution can be done by following the next steps:

1.  Create an Integration Flow on Cloud Integration.

2.  Add a content modifier called “Set Properties” and define the following properties:

    ![image8](Image8.png)
    - RFCDestination: Name of the RFC Destination you will use.
    - BAPIName: Name of the BAPI you will use.
    - payload: After the request-reply step, it is necessary to retrieve the original incoming message. This step ensures that subsequent operations have access to the initial data. You can achieve this by using ${in.body}.

    ![image9](Image9.png)

3.  Create a message mapping by adding the source and target messages previously imported in the 'References' section of the Integration Flow. 

    ![image12](ImageMapping2.png)

4. In this example, the source message is 'flightConnectionRequest', the target message is 'flightConnectionResponse' and it was done a 1:1 mapping on the Connection node.

    ![image12](ImageMap2.png)

5.  Open the Message Mapping, search for the fields you would need to implement the RFC Lookup.

6.  On Functions, click on “Create” and put the name “runRFC”. Paste the code below.

    ![image10](Image10.png)

    The *runRFC* code is the following:

    ```Groovy
        import com.sap.it.api.mapping.*;
        import com.sap.conn.jco.*;

        public void runRFC(String[] AIRLINEID, String[] CONNECTIONID, String[] FLIGHTDATE, Output ECONOFREE, Output BUSINFREE, Output FIRSTFREE, MappingContext context) throws JCoException {
            
            String destinationName = context.getProperty("RFCDestination");
            String bapiname = context.getProperty("BAPIName");

            JCoDestination jcoDestination = JCoDestinationManager.getDestination(destinationName);
            JCoFunction function = jcoDestination.getRepository().getFunction(bapiname);

            if (function != null) {
                function.getImportParameterList().setValue("AIRLINEID", AIRLINEID[0]); 
                function.getImportParameterList().setValue("CONNECTIONID", CONNECTIONID[0]); 
                function.getImportParameterList().setValue("FLIGHTDATE", FLIGHTDATE[0]); 

                function.execute(jcoDestination);

                JCoParameterList outputParams = function.getExportParameterList();
                JCoStructure availibility = outputParams.getStructure("AVAILIBILITY");

                String econoFree = availibility.getString("ECONOFREE");
                String businFree = availibility.getString("BUSINFREE");
                String firstFree = availibility.getString("FIRSTFREE");

                ECONOFREE.addValue(econoFree);
                BUSINFREE.addValue(businFree);
                FIRSTFREE.addValue(firstFree);
            } else {
                ECONOFREE.addValue("ERROR: RFC function not found");
                BUSINFREE.addValue("ERROR: RFC function not found");
                FIRSTFREE.addValue("ERROR: RFC function not found");
            }
        }
    ``` 

7. Edit the UDF code, define and replace the following parameters:

    - RFCRequestField1, RFCRequestField2, RFCRequestField3 = Name of the RFC Request fields.
    - OutputParameter1, OutputParameter2, OutputParameter3 = Name of the RFC Response fields

    >*Note: The getStructure method is important to handle the response structures, however, it’s necessity may depend on the specific RFC that is being used. If a particular RFC doesn’t include a structured response, the getStructure method may not be needed. In this tutorial, it was used the BAPI_FLIGHT_CHECKAVAILIBILITY RFC where the getStructure method was needed to process the “AVAILIBILITY” structure in the response data, as you can see on the image below. It's essential to understand the requirements of each RFC and adjust the UDF code, accordingly, using methods like getStructure only when necessary.*

    ![image11](Image11.png)

    >*Note: This configuration depends on the number of the RFC Request fields, RFC Response fields and the mapping you are developing. You can add or remove properties as needed.*

8. In this case, map the 'flightConnectionRequest' source fields (Airline, Connection and Date) to the RFC request fields (AIRLINEID, CONNECTIONID and FLIGHTDATE). Regarding the output fields, map the RFC output fields (ECONOFREE, BUSINFREE and FIRSTFREE) to the Seats node on the 'flightConnectionResponse' target structure (Economy, Business and First) in the message mapping.

    ![image12](Image12.png)

9.  Save the integration flow, deploy it, and trigger a message.

    ![image13](Image13.png)

    The mapping result is the following:

    ```XML
            <?xml version="1.0" encoding="UTF-8"?>
                    <ns0:flightConnectionResponse xmlns:ns0="http://pimas.com">
                        <Connection>
                            <Airline>AA</Airline>
                            <Connection>0017</Connection>
                            <Date>20230921</Date>
                        </Connection>
                        <Seats>
                            <Economy>15</Economy>
                            <Business>0</Business>
                            <First>2</First>
                        </Seats>
                    </ns0:flightConnectionResponse>
    ``` 

### Test yourself