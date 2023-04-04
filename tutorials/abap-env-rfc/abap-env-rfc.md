---
parser: v2
title: Call a Remote Function Module (RFC) From SAP BTP, ABAP Environment
description: Call a remote function module located in an on-premise system, such as a SAP S/4HANA System, from the ABAP Environment
auto_validation: true
time: 30
tags: [ tutorial>intermediate, software-product>sap-btp--abap-environment, software-product>sap-business-technology-platform, software-product-function>s-4hana-cloud-abap-environment, tutorial>license]
primary_tag: programming-tool>abap-development
author_name: Julie Plummer
author_profile: https://github.com/julieplummer20

---


## Prerequisites

- **IMPORTANT**: This tutorial cannot be completed on a trial account. If you want to explore some of the concepts of this mission on a trial account, using OData or SOAP rather than RFC, see the following workshop: [SAP BTP, ABAP Environment: Connectivity and Integration](https://github.com/SAP-samples/teched2020-DEV268).
- You have set up SAP Business Technology Platform (BTP), ABAP Environment, for example by using the relevant booster: [Using a Booster to Automate the Setup of the ABAP Environment](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/cd7e7e6108c24b5384b7d218c74e80b9.html)
- **Tutorial**: [Set Up SAP BTP, ABAP Environment and create Your First Console Application](abap-environment-trial-onboarding), for a licensed user, steps 1-2
-	You have developer rights to an ABAP on-premise system, such as:
    - [AS ABAP developer edition, latest version](https://blogs.sap.com/2019/07/01/as-abap-752-sp04-developer-edition-to-download/) or:
    - [SAP S/4HANA 1809 fully activated appliance](https://blogs.sap.com/2018/12/12/sap-s4hana-fully-activated-appliance-create-your-sap-s4hana-1809-system-in-a-fraction-of-the-usual-setup-time/) or:
    - [The SAP Gateway Demo System (ES5)](https://blogs.sap.com/2017/12/05/new-sap-gateway-demo-system-available/)
- You have configured **SAP Cloud Connector**, connecting your BTP and on-premise systems and have added the appropriate resources
- You have assigned the business role **`SAP_BR_DEVELOPER`** to your user in both systems; you will need it to create communication artifacts

  
## You will learn 
  
  - How to create a secure RFC connection between your SAP BTP, ABAP Environment and an on-premise SAP System, e.g. SAP S/4HANA
  - How to test the connection using an ABAP handler class

This tutorial mission was written for SAP BTP ABAP Environment. However, you should also be able to use it in SAP S/4HANA Cloud Environment in the same way.

Throughout this tutorial, replace `000` with your initials or group number.

**The problem:**

There are two problems when setting up connectivity between the SAP BTP, ABAP Environment and an on-premise ABAP System:

- The ABAP Environment "lives" in the Internet, but customer on-premise systems are behind a firewall
- Remote Function Call (RFC) is not internet-enabled

**The solution:**

Set up a secure connection from the on-premise system to the SAP BTP, ABAP Environment. To do this, you will create:

  - SAP Cloud Connector, to set up the secure tunnel connection
  - The **`create_by_comm_arrangement`** method of the **`cl_http_destination_provider`** class
  - Communication artifacts
  - ABAP class that outputs the retrieved data to the Console

**Technical information:**

1. The ABAP environment tenant fetches the destination from the Destination service instance.
2. The ABAP environment tenant requests to open the tunnel connection through the Connectivity service.
3. The Connectivity service tells the Cloud Connector to open the connection to this specific ABAP environment tenant using the admin connection.
4. The Cloud Connector opens a tunnel connection to the ABAP environment tenant using its public tenant URL.
5. After the tunnel is established, it can be used for actual data connection using the RFC or HTTP(S) protocols.

<!-- border -->
![overview-cf-only](overview-cf-only.png)

---


### Check SAP Cloud Connector Configuration

First, you need to connect your ABAP on-premise system to your BTPsub-account by means of SAP Cloud Connector.

1. In your browser, log on to SAP Cloud Connector:
    - Address = e.g. `https://localhost:<port>` (Default = **`8443`**)
    - User = Administrator
    - Initial password = manage (You will change this when you first log in)

2. Your configuration should look like this. 

  <!-- border -->
  ![step1b-scc-location-id](step1b-scc-location-id.png)

  Note down the **Location ID**, e.g. **`SAPDEV`** as here. **You will need it later.**


### Add On-Premise System

1. In the menu in the left pane, expand the sub-account and choose **Cloud To On-Premise > Access Control**.
    
2. Note down the **Virtual Host, e.g. `s4h_rfc:sapgw<instance>`** as here. This represents an external hostname, so that you can hide the internal hostname from the outside world. **You will need this external hostname and port later, when creating a destination from SAP BTP cockpit**.

3. Check that **Result** = **`Reachable`**. If not, check that you chose the correct port, or whether an internal firewall is preventing communication.

    <!-- border -->
    ![step2a-scc-virtual-host](step2a-scc-virtual-host.png)

4. Check that the list of resources should looks roughly like this.

    <!-- border -->
    ![step2b-scc-resources](step2b-scc-resources.png)


### Check connectivity from SAP BTP cockpit

In the SAP BTP cockpit of your Cloud Foundry sub-account, choose **Cloud Connectors**:

<!-- border -->
![step3a-btp-cockpit-cloud-connectors](step3a-btp-cockpit-cloud-connectors.png)

> The **Location ID** points to the correct SAP Cloud Connector (located in the on-Premise system); The **Virtual host** points to the on-Premise connection mapped in SAP Cloud Connector. Later, in your **Communication System** in Fiori Launchpad, you will use these values, as **SCC Location ID** and **Target Host** respectively.


### Create package

Now, in SAP BTP ABAP Environment, you need to create the necessary ABAP artifacts in ABAP Development Tools (ADT), starting with a package.

1. In ABAP Development Tools (ADT), select the ABAP Cloud Project and choose **New > ABAP Package** from the context menu.

2. Enter the following and choose **Next**:
    - Name = **`Z_API_OUTBOUND_RFC_000`**
    - Description = **Call API from S/4HANA Using RFC**
    - Package type = **Development**

    <!-- border -->
    ![step1a-create-package](step1a-create-package.png)

3. Choose **Create new transport request**, enter a description, such as **Get product data from S/4HANA using RFC**, then choose **Finish**.

    <!-- border -->
    ![step1b-transport-request](step1b-transport-request.png)


### Create outbound service

Next, you will establish outbound communication from the BTP instance to the S/4HANA instance.

1. Select your ABAP package and choose **New > Other Repository Object** from the context menu; then enter **Outbound Service**, then choose **Next**.

    <!-- border -->
    ![step2a-new-repo-object](step2a-new-repo-object.png)
    
    <!-- border -->
    ![step2a-new-outbound-service](step2a-new-outbound-service.png)

2. Enter the following and choose **Next**.
    - Outbound service: **`Z_OUTBOUND_HANA_RFC_000`**. The **`SRFC`** suffix will be added automatically.
    - Description: **Get data from HANA System using RFC**
    - Service type: **RFC Service**

    <!-- border -->
    ![step2b-outbound-service-name](step2b-outbound-service-name.png)

3. Choose the transport request you just created, then choose **Finish**.

The outbound service appears. Add the relevant RFC, **`RFC_GET_SYSTEM_INFO`**, then choose **Save**.

  <!-- border -->
  ![step2c-new-outbound-service-rfc](step2c-new-outbound-service-rfc.png)


### Create communication scenario

1. Still in ADT, choose **Cloud Communication Management > New > Communication Scenario** from the context menu, then choose **Next**.

    <!-- border -->
    ![step3a-new-comm-scen](step3a-new-comm-scen.png)

2. Enter the following and choose **Next**.
    - Name: **`Z_OUTBOUND_HANA_RFC_000_CS`**
    - Description: **`Comm Scen: Call API from HANA using RFC`**

    <!-- border -->
    ![step3b-new-comm-scen-name](step3b-new-comm-scen-name.png)

3. Choose the transport request you just created, then choose **Finish**.

    The communication scenario appears in a new editor. Ensure the following have been entered:

    - Communication Scenario Type: **Customer Managed**
    - Allowed Instances: **One instance per scenario and communication system**
        
4. On the **Outbound** tab, add your service by choosing **Add**.

    <!-- border -->
    ![step3c-add-service](step3c-add-service.png)

5. Choose **Browse**, select your outbound service **`Z_OUTBOUND_HANA_000_SRFC`**, then choose **Finish**. The service type **RFC** is entered automatically.

6. Choose **Finish**. Your communication scenario should look like this.
Make sure that **Supported Authentication Methods > Basic** is ticked, since you will use this method later.

  <!-- border -->
  ![step3d-comm-scen-finished](step3d-comm-scen-finished.png)

7. Leave the other default settings, choose **Save**, then choose **Publish locally**.


### Create communication system

Now, you need to create the necessary communication artifacts in Fiori Launchpad (of the BTP instance), starting with the communication system.
This artifact specifies the URL of the API (minus the HTTP(S) protocol) and port.

1. In the Fiori Launchpad home page, choose **Communication Systems**, then choose **New**.

    <!-- border -->
    [step4a-comm-system](step4a-comm-system.png)
    <!-- border -->
    ![step4b-new-comm-system](step4b-new-comm-system.png)

2. Enter the following and choose **Create**.

    - System ID: **`Z_OUTBOUND_HANA_CS_000`**
    - System Name: **`Z_OUTBOUND_HANA_CS_000`**

3. In **General** and below, enter:

    |  Field Name             | Value
    |  :----------------------| :-------------
    |  Host name | *the host name of your ABAP on-premise system, found in the URL of that system's Fiori Launchpad, e.g.: `myhost.s4hana.ondemand.com`*
    |  Port | Default = **443**
    | Destination | **Off**
    | Cloud Connector | **On**
    | SCC Location ID: |**`SAPDEV`**
  
4. In **RFC Settings** 
  | Load Balancing | Off
  | Target Host | **`s4h_rfc`**
  | Client | *Client of your ABAP on-premise system, also found in the URL e.g. 100*
  | Instance Number | Default = 00

  <!-- border -->
  ![step4c-comm-system-settings]( step4c-comm-system-settings.png)    

5. Under **Users for Outbound Communication**, enter the following, then choose **Close**:
    - Authentication method: **User Name and Password** (basic authentication)
    - User name: *Any name in the form ``BTP_S4H_CU_000``, where* **BTP** *= SAP BTP ABAP Environment and* **S4H** *= ABAP on-Premise System*
    - Password: *should be generated*. Store this password, e.g. locally, because you will need it later in the on-premise system

    <!-- border -->
    ![step4d-comm-user-btp-s4h-cu-000](step4d-comm-user-btp-s4h-cu-000.png)

6. Leave the other default settings as they are and choose **Save**.


### Create communication arrangement

Next, you will create communication arrangement, pointing to the communication scenario and communication system.

1. Choose **Communication Arrangements**, then choose **New**.

    <!-- border -->
    ![step4a-comm-management-tiles](step4a-comm-management-tiles.png)

2. Choose your communication scenario, **`Z_OUTBOUND_RFC_000_CS`**. This name is also entered automatically for the communication arrangement. You can accept this default.

    <!-- border -->
    ![step4a-new-comm-arr](step4a-new-comm-arr.png)

    <!-- border -->
    ![step4b-select-comm-scenario](step4b-select-comm-scenario.png)

3. Choose the communication system you created, **`Z_OUTBOUND_HANA_CS_000`**.
    The other details, e.g. **Communication User**, **RFC Outbound Service**, and **RFC Function Module** are entered automatically.

4. Check the connection.


### Create communication system on-premise

Now you will create the necessary communication artifacts in the on-premise system, starting with the **Communication System**.

1. In **Communication Systems**, choose **New**.

2. Enter the following and choose **Create**.

    - System ID: **`Z_INBOUND_RFC_CS_000`**
    - System Name: **`Z_INBOUND_RFC_CS_000`**

3. In **Technical Data**, choose **Inbound Only**.

4. In **Users for Inbound Communication**, choose **Add**, then choose your existing communication user from the drop-down list.


### Create ABAP class for RFC connection in SAP BTP, ABAP Environment

1. In ABAP Development Tools (ADT), in **Project Explorer**, open your ABAP Environment instance.
For more information, see [Set Up SAP BTP, ABAP Environment and create Your First Console Application](abap-environment-trial-onboarding).

2. In the project, create a new ABAP package.

3. Create a new ABAP class: Choose **File > New > Other... > ABAP Class**.

3. 2. Enter the following for your class, then choose **Next**. 
    - Name: **`ZCL_SYSTEM_INFO_RFC_000`**
    - Description for your class, e.g. **Test RFC BTP to on-premise** 
    - Interface: **`if_oo_adt_classrun`**. *This enables you to run the class in the console.*

Replace `000` with your group number or initials.

4. Create or assign a transport request.


### Create variables

In the method **`if_oo_adt_classrun~main`**, create the data types that specify your remote connection information

<!-- REVISE THIS, replacing the `i_name` with your the name of the specific **RFC** destination, which you created in SAP BTP cockpit (in step 5 of this tutorial). -->

    ```ABAP
    DATA(lo_destination) = cl_rfc_destination_provider=>create_by_comm_arrangement(
                            comm_scenario          = Z_OUTBOUND_RFC_000_CS     " Communication scenario
                            service_id             = Z_OUTBOUND_HANA_000       " Outbound service
                            comm_system_id         = Z_OUTBOUND_HANA_CS_000    " Communication system

                           ).

    DATA(lv_destination) = lo_destination->get_destination_name( ).

    DATA lv_result type c length 200.

    ```


### Call remote function from on-premise system

```ABAP
CALL function 'RFC_SYSTEM_INFO'
destination lv_destination
  IMPORTING
    RFCSI_EXPORT      = lv_result.

```


### Output result

Output the result of the RFC call to the ABAP Console

```ABAP
out->write( lv_result ).
```


### Wrap method in an exception

Wrap the whole method in an exception using TRY...CATCH.

```ABAP
catch cx_root into data(lx_root).
  out->write( lx_root->get_text( ) ).
endtry.

```    


### Check your code

Your code should look roughly like this:

```ABAP
CLASS ZCL_A4C_RFC_ DEFINITION
  public
  final
  create public .

public section.
  interfaces if_oo_adt_classrun.
protected section.
private section.
ENDCLASS.

CLASS ZCL_A4C_RFC_ IMPLEMENTATION.
  METHOD IF_OO_ADT_CLASSRUN~MAIN.
    TRY.
      DATA(lo_destination) = cl_rfc_destination_provider=>create_by_comm_arrangement(
                              comm_scenario          = Z_OUTBOUND_RFC_000_CS     " Communication scenario
                              service_id             = Z_OUTBOUND_HANA_000       " Outbound service
                              comm_system_id         = Z_OUTBOUND_HANA_CS_000    " Communication system

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


### Test the class

1. Save and activate the class, using **`Ctrl+S, Ctrl+F3`**.

2. Run the class by choosing **`F9`**. Some system information, such as the hostname, the System ID ( `<SID>` ), and the IP address should be displayed.


### Test yourself


### Add error handling to the class for the RFC connection

1. Go back to your RFC class. Remove the period (.) after the IMPORTING parameter and add the following exception parameters to the function call `RFC_SYSTEM_INFO`:

    ```ABAP

    EXCEPTIONS
      system_failure        = 1 MESSAGE msg
      communication_failure = 2 MESSAGE msg
      OTHERS                = 3.

    ```

2. Now evaluate `sy-subrc` by adding the following `CASE...ENDCASE` statement:

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


### More Information

This tutorial mission is based on a blog post series by Andre Fischer:
- [How to call a remote function module in your on-premise SAP system from SAP BTP ABAP Environment](https://blogs.sap.com/2019/02/28/how-to-call-a-remote-function-module-in-your-on-premise-sap-system-from-sap-cloud-platform-abap-environment/)

OData services:

- [Mission: Take a Deep Dive into OData](mission.scp-3-odata)

SAP Gateway in general, see:

- [OData service development with SAP Gateway using CDS](https://blogs.sap.com/2016/06/01/odata-service-development-with-sap-gateway-using-cds-via-referenced-data-sources/) - pertains to on-premise Systems, but contains lots of useful background information on the relationships between CDS views and OData services

Connectivity in this context, see:
- SAP Help Portal: [SAP Cloud Connector](https://help.sap.com/viewer/368c481cd6954bdfa5d0435479fd4eaf/Cloud/en-US/642e87f1492146998a8eb0779cd07289.html)

- SAP Help Portal: [RFC Communication via Communication Arrangements](https://help.sap.com/docs/btp/sap-business-technology-platform/rfc-communication-via-communication-arrangements)

SAP Business Technology Platform (BTP):

- SAP Help Portal: [What is SAP Business Technology Platform (BTP)](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/73beb06e127f4e47b849aa95344aabe1.html)

- SAP Help Portal: [Getting Started With a Customer Account](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/e34a329acc804c0e874496548183682f.html) - If you use the booster, these steps are performed automatically for you, but you may be interested in the background information







---
