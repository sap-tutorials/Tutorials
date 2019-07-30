---
title: Call a Remote Function Module From ABAP Envionment
description: Call a remote function module located in an on-Premise system, such as an S/4HANA System, from the ABAP Environment
auto_validation: true
time: 60
tags: [ tutorial>advanced, topic>abap-development, products>sap-cloud-platform, tutorial>license]
primary_tag: products>sap-cloud-platform--abap-environment
---

## Prerequisites
-	A SAP CP Neo subaccount
-	An ABAP on-premise system, such as [SAP S/4HANA 1809 fully activated appliance](https://blogs.sap.com/2018/12/12/sap-s4hana-fully-activated-appliance-create-your-sap-s4hana-1809-system-in-a-fraction-of-the-usual-setup-time/)
-	In this on-premise system, a SAP Cloud Connector, configured with your Neo sub-account. See: [SAP Help Portal: SAP Cloud Connector: Installation](https://help.sap.com/viewer/cca91383641e40ffbe03bdc78f00f681/Cloud/en-US/57ae3d62f63440f7952e57bfcef948d3.html)



## Details
### You will learn
  - How to open a secure tunnel connection between your SAP Cloud Platform ABAP Environment and an on-premise SAP System, e.g. SAP S/4HANA
  - How to create a destination service instance with HTTP and RFC connections
  - How to create a communication arrangement to integrate this destination service
  - How to create a communication arrangement to integrate SAP Cloud Connector
  - How to test the connection using an ABAP handler class

For more information on setup, see:

- [SAP Help Portal: What is SAP Cloud Platform](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/73beb06e127f4e47b849aa95344aabe1.html) - basic concepts

- [SAP Help Portal: Connect to the ABAP System](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/7379dbd2e1684119bc1dd28874bbbb7b.html)

- [SAP Help Portal: SAP Cloud Connector](https://help.sap.com/viewer/368c481cd6954bdfa5d0435479fd4eaf/Cloud/en-US/642e87f1492146998a8eb0779cd07289.html)

- [SAP Help Portal: Integrating On-premise Systems](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/c95327fbed6c4efeb1855f12f826301d.html)

To see this tutorial group as a blog series by Andre Fischer, see:

- [How to call a remote function module in your on-premise SAP system from SAP Cloud Platform – ABAP Environment](https://blogs.sap.com/2019/02/28/how-to-call-a-remote-function-module-in-your-on-premise-sap-system-from-sap-cloud-platform-abap-environment/)

Throughout this tutorial, replace `XXX` with your initials or group number.

**The problem:**

There are two problems when setting up connectivity between the Cloud Platform ABAP Environment and an on-premise:

- The ABAP Environment "lives" in the Internet, but customer on-premise systems are behind a firewall.
- RFC is not internet-enabled.

**The solution**:

- Set up a connection from the on-premise system to the SAP Neo Environment using SAP Cloud Connector.
- Set up a connection from the SAP Neo to SAP Cloud Foundry Environment.

**Specifically**:

1. Fetch the destination, i.e. from Cloud Foundry to on-premise  (using a Cloud Foundry destination service)
2. Send request to open a tunnel, from Cloud Foundry (i.e. ABAP) to Neo
3. Send request to open a tunnel, from Neo to the on-premise system
4. Open a secure tunnel for HTTP and RFC
5. Communicate through the tunnel via HTTP or RFC

![Image depicting overview](overview.png)

---

[ACCORDION-BEGIN [Step 1: ](Configure SAP Cloud Connector)]
First, you need to connect your ABAP on-premise system to a Neo subaccount by means of SAP Cloud Connector.

1. Log on to SAP Cloud Connector:
    - Address = `https://localhost:<port>` (Default = 8443)
    - User = Administrator
    - Initial password = Manage (You will change this when you first log in)

2. Choose **Add Subaccount**:
  - **Region** = e.g. Europe (Rot). You can find this in SAP Cloud Cockpit (see screenshot below)
  - **Subaccount** = "Neo Technical Name". You can find this by choosing your SAP Cloud Platform, NEO, subaccount in SAP Cloud Cockpit and choosing the **information (i)** icon. (see screenshot below)
  - **Display Name** = (Neo Subaccount) Display Name. You can find this in by choosing your SAP Cloud Platform, NEO, subaccount in SAP Cloud Cockpit (see screenshot below)
  - **Subaccount User** = for the Neo Subaccount
  - **Password**
  - **Location ID** = Optional here. However, it is mandatory if you want to connect several Cloud Connectors to your subaccount. This can be any text, e.g. your initials, or your location

  ![Image depicting step1g-create-subacc](step1g-create-subacc.png)
  ![Image depicting step1f-choose-subacc](step1f-choose-subacc.png)
  ![Image depicting step1b-neo-tech-name ](step1b-neo-tech-name.png)

3. Your configuration should now look like this:

  ![Image depicting step1-check-scc](step1-check-scc.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Specify the remote function module)]
Now, still in the Cloud Connector, enter the resource you need, `RFC_SYSTEM_INFO`.

1. In your subaccount, choose **Cloud to On-Premise > Access Control**.

2. Check the **Mapping Virtual to Internal System**. Here, `myHost` represents an external hostname, so that you can hide the internal hostname from the outside world. You will need this external hostname and port later. Check that the status = `Reachable`. If not, check that you chose the correct port, or whether an internal firewall is preventing communication.

    ![Image depicting step1a-scc-resources-sid](step1a-scc-resources-sid.png)  

3. Add the resource **`RFC_SYSTEM_INFO`** by choosing **+**.

    ![Image depicting step1d-new-rfc](step1d-new-rfc.png)

4. Enter the name of the RFC, e.g. **`RFC_SYSTEM_INFO`** and choose **Save**.

    ![Image depicting step1e-name-rfc](step1e-name-rfc.png)

The RFC appears in the list of resources.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open the destination service instance)]
You will now create two destinations in the ABAP Environment. These must be created at Space level, e.g. `Dev`, not subaccount level.

1. In SAP Cloud Cockpit, open your **ABAP Environment > your space, such as Dev**, then choose **Service Instances**.

    ![Image depicting step2b-service-marketplace](step2b-service-marketplace.png)

2. Open the destination service you created in [Create a Communication Arrangement for Outbound Communication](abap-env-create-comm-arrangement-api)

    ![Image depicting step2c-destination-service-instance](step2c-destination-service-instance.png)  

3. Create a new destination for the destination service instance

    ![Image depicting step3a-new-destination](step3a-new-destination.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a new HTTP destination for the destination service instance)]
1. In the Destination Configuration pane, enter the following values:
    - Name, e.g. : `S4HTEST_HTTP_XXX` (You should include the suffix `HTTP` and replace `XXX` with your initials or group number.)
    - Type: `HTTP`
    - Description, e.g. Test S4H HTTP connection
    - URL: `http://<myHost>:51080/sap/bc/ping` - where `myHost` = the external hostname of your on-premise ABAP System
    - `OnPremise`
    - `BasicAuthentication`
    - User: Your user for the on-premise ABAP system
    - Password: Your password

    The IP address and port must be the one you specified in the Cloud Connector earlier.

2. Choose **Save**.

The destination appears in the list.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a new RFC destination for the destination service instance)]
1. Again, choose **New Destination** and enter the following values:
    - Name, e.g. : `S4HTEST_RFC_XXX` (You should include the suffix `RFC` and use the same prefix as for your HTTP connection)
    - Type: `RFC`
    - Description, e.g. Test S4H RFC connection
    - Location ID: same as in step 1, e.g. your initials
    - User: Your user
    - Password: Your password

2. Add the following additional properties and values, by choosing **New Property**:
    - `jco.client.ashost` = `<myHost>` - again, the external hostname of your on-premise ABAP System in lower-case
    - `jco.client.client` = `<Your ABAP System client`, e.g. 100
    - `jco.client.sysnr` = `<Your ABAP System number>`, e.g. 10

[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 6: ](Create a Communication System for SAP Cloud Connector)]
1. Again, in the Dashboard, choose **Communication Systems > New**.

2. Enter the credentials for the SAP Cloud Connector administration user for your SAP CP Neo account (i.e. the same user as in step 1 above):
      - Hostname

        ![Image depicting step8b-comm-system-1](step8b-comm-system-1.png)

      - User and Password

        ![Image depicting step8c-comm-system-2](step8c-comm-system-2.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create a Communication Arrangement for SAP Cloud Connector)]
1. Go back to the Dashboard Home, choose **Communication Arrangements** again, then choose **New**.

      ![Image depicting step6b-comm-arrangement](step6b-comm-arrangement.png)

2. Enter the following values and choose **Create**.
    - Scenario: `SAP_COM_0200` (SAP Cloud Connector Integration)
    - Name: `SAP_COM_0200_XXX`

3. Enter the Communication System you have just created.

4. In the details screen, enter the Account Name: **`<NEO Technical Name>`**

      ![Image depicting step7-comm-arrangement-0200-details](step7-comm-arrangement-0200-details.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create an ABAP class for the RFC connection)]
1. Create a new ABAP class: Choose **File > New > Other... > ABAP Class**.

2. Enter a name and description. The name should be in the form `Z_...RFC_XXX`. Replace `XXX` with your group number or initials.

3. Create or assign a transport request.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Add the interfaces statement; implement the main method)]
1. Implement the interface by adding this statement to the public section:

    `interfaces if_oo_adt_classrun.`

    This allows you to test your class by displaying the output in the ABAP Console.

2. In the implementation section, add the METHOD and ENDMETHOD statements:

    `METHOD IF_OO_ADT_CLASSRUN~MAIN.`
    `ENDMETHOD.`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Create variables)]
1. Create the data types that specify your remote connection information:

    ```ABAP
    DATA(lo_destination) = cl_rfc_destination_provider=>CREATE_BY_CLOUD_DESTINATION(
                             I_SERVICE_INSTANCE_NAME = 'OutboundComm_for_RFCDemo_XXX'
                             I_NAME                  = 'S4TEST_RFC_XXX'
                           ).

    DATA(lv_destination) = lo_destination->get_destination_name( ).

    DATA lv_result type c length 200.
    ```

2. Replace the `i_service_instance_name` with your service instance name, specified in the Communication  Arrangement (which you created in [Create a Communication Arrangement for Outbound Communication](abap-env-create-comm-arrangement-api)).


3. Replace the `i_name` with your the name of the specific **RFC** destination (which you created in SAP Cloud Cockpit in the tutorial [Create a Communication Arrangement for Outbound Communication](abap-env-create-comm-arrangement-api)).

    ![Image depicting step10b-i-name](step10b-i-name.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Call the remote function from the on-premise system)]
```ABAP
CALL function 'RFC_SYSTEM_INFO'
destination lv_destination
  IMPORTING
    RFCSI_EXPORT      = lv_result.

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Output the result)]
Output the result of the RFC to the ABAP Console

```ABAP
out->write( lv_result ).
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Wrap the method in an exception)]
Wrap the whole method in an exception using TRY...CATCH.

```ABAP
catch cx_root into data(lx_root).
  out->write( lx_root->get_text( ) ).
endtry.
```    
![Image depicting step15-try-catch](step15-try-catch.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Check your code)]
You code should look roughly like this:

```ABAP
class Z_A4C_RFC_XXX definition
  public
  final
  create public .

public section.
  interfaces if_oo_adt_classrun.
protected section.
private section.
ENDCLASS.



CLASS Z_A4C_RFC_XXX IMPLEMENTATION.
  METHOD IF_OO_ADT_CLASSRUN~MAIN.
    TRY.
      DATA(lo_destination) = cl_rfc_destination_provider=>CREATE_BY_CLOUD_DESTINATION(
                               I_SERVICE_INSTANCE_NAME = 'OutboundComm_for_RFCDemo_XXX'
                               I_NAME                  = 'S4TEST_RFC_XXX'
                             ).

      DATA(lv_destination) = lo_destination->get_destination_name( ).

      DATA lv_result type c length 200.

      CALL function 'RFC_SYSTEM_INFO'
      destination lv_destination
        IMPORTING
          RFCSI_EXPORT      = lv_result.

        out->write( lv_result ).
    catch cx_root into data(lx_root).
      out->write( lx_root->get_text( ) ).
    endtry.
  ENDMETHOD.

ENDCLASS.
```
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Test the class)]
1. Save and activate the class (**`Ctrl+S, Ctrl+F3`**).

2. Run the class by choosing **`F9`**. Some system information, such as the hostname, the System ID ( `<SID>` ), and the IP address should be displayed.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Optional: Create an ABAP class for the HTTP connection)]
This class allows you to troubleshoot the RFC connection by checking that the HTTP connection works.

```ABAP
class Z_A4C_HTTP_TEST_XXX definition
  public
  final
  create public .

public section.
  interfaces if_oo_adt_classrun.
protected section.
private section.
ENDCLASS.

CLASS Z_A4C_HTTP_TEST_XXX IMPLEMENTATION.
  METHOD IF_OO_ADT_CLASSRUN~MAIN.
    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_cloud_destination(
          i_name                  = 'S4HTEST_HTTP_XXX'
          i_service_instance_name = 'OutboundComm_for_RFCDemo_XXX'
          i_authn_mode = if_a4c_cp_service=>service_specific ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
        DATA(lo_request) = lo_http_client->get_http_request( ).
        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
          out->write( lo_response->get_status( ) ).
          out->write( lo_response->get_text( ) ).

      CATCH cx_root INTO DATA(lx_exception).
        out->write( lo_response->get_status( ) ).
        out->write( lx_exception->get_text( ) ).
    ENDTRY.

  endmethod.
ENDCLASS.

```
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 18: ](Add error handling to the class for the RFC connection )]
1. Go back to your RFC class. Remove the period (.) after the IMPORTING parameter and add the following exception parameters to the function call `RFC_SYSTEM_INFO`:

    ```ABAP

    EXCEPTIONS
      system_failure        = 1 MESSAGE msg
      communication_failure = 2 MESSAGE msg
      OTHERS                = 3.

    ```

2. Now evaluate `sy-subrc` by adding the following CASE...ENDCASE statement:

    ```ABAP

    CASE sy-subrc.
       WHEN 0.
         out->write( lv_result ).
       WHEN 1.
         out->write( |EXCEPTION SYSTEM_FAILURE | && msg ).
       WHEN 2.
         out->write( |EXCEPTION COMMUNICATION_FAILURE | && msg ).
       WHEN 3.
         out->write( |EXCEPTION OTHERS| ).
     ENDCASE.

    ```

    ![Image depicting step20-error-handling](step20-error-handling.png)

[DONE]
[ACCORDION-END]
---
