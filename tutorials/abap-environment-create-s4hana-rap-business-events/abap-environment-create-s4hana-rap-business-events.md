---
parser: v2
auto_validation: true
time: 45
tags: [ tutorial>beginner, tutorial>license]
primary_tag: programming-tool>abap-development
author_name: Niloofar Flothkoetter
author_profile: https://github.com/niloofar-flothkoetter
---

# How to Create RAP Business Events in an On-Premise system
<!-- description --> Create and exposure of business events in an On-Premise system

## Prerequisites
 - You need to have access to an On-Premise 2022 system with authority to create and maintain a channel. 
 - You need to prepare an event mesh instance in your SAP business technology platform system and download the service key of this instance. For more information see the [Create Instance of SAP Event Mesh] (https://developers.sap.com/tutorials/cp-enterprisemessaging-instance-create.html)


## You will learn
  - How to create Business Events with RAP in an On-Premise system
  - How to set up a channel to connect to SAP Event Mesh

## Intro
>Always replace `####` with your initials or group number.

The ABAP RESTful Application Programming Model (RAP) now supports the native consumption and exposure of business events from release 2022. For exposure, an event can be defined and raised in a RAP business object or in the behavior extension and then published via Event Bindings.


### Create database table

To produce and raise an event you need first to define your RAP Business Object which produce the event. For this, you will create a booking application. The event will be sent whenever a booking flight is cancelled.

  1. Open ADT and open your system.

  2. If you do not have an **ABAP Package** create a new one. Please be sure if your package name is started with **Z** like

    - Name: `ZEVENT_BOOKING_####`
    - Description: `define a RAP event`

    ![new](1-1.png)

  3. Right-click on your package and create a new database table

    - Name: `ZBOOKING_####`
    - Description: `DB for booking`

    ![database](1-2.png)

    ![database](1-3.png)

  4. Replace the default code with the code snippet provided below and replace all occurences of the placeholder `####` with your group ID using the **Replace All** function `Ctrl+F`.


    ```ZBOOKING_####
    @EndUserText.label : 'DB for booking'
    @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
    @AbapCatalog.tableCategory : #TRANSPARENT
    @AbapCatalog.deliveryClass : #A
    @AbapCatalog.dataMaintenance : #RESTRICTED
    define table zbooking_#### {
    key client            : abap.clnt not null;
    key travel_id         : /dmo/travel_id not null;
    agency_id             : /dmo/agency_id;
    customer_id           : /dmo/customer_id;
    begin_date            : /dmo/begin_date;
    end_date              : /dmo/end_date;
    @Semantics.amount.currencyCode : 'zbooking_####.currency_code'
    booking_fee           : /dmo/booking_fee;
    @Semantics.amount.currencyCode : 'zbooking_####.currency_code'
    total_price           : /dmo/total_price;
    currency_code         : /dmo/currency_code;
    description           : /dmo/description;
    overall_status        : /dmo/overall_status;
    attachment            : /dmo/attachment;
    mime_type             : /dmo/mime_type;
    file_name             : /dmo/filename;
    created_by            : abp_creation_user;
    created_at            : abp_creation_tstmpl;
    last_changed_by       : abp_locinst_lastchange_user;
    last_changed_at       : abp_locinst_lastchange_tstmpl;
    local_last_changed_at : abp_lastchange_tstmpl;
    }  

    ```

  5. Save and activate your table.

    ![database](1-4.png)


### Create data generator class

1. Right-click your package and select **New** > **ABAP Class** from the context menu.

    ![cds](2-1.png)

2. Maintain the required information and click **Next**

    - Name: `ZCL_BOOKING_GEN_DATA_####`
    - Description: `Generate demo data`

    ![demo](2-2.png)


3. Select a transport request and click **Finish** to create the class.

4. Copy the code below to your CDS data model and replace `####` with your number


        ```ZCL_BOOKING_GEN_DATA_####

        CLASS zcl_booking_gen_data_#### DEFINITION
        PUBLIC
        FINAL
        CREATE PUBLIC .

        PUBLIC SECTION.

            INTERFACES if_oo_adt_classrun.
        PROTECTED SECTION.
        PRIVATE SECTION.
        ENDCLASS.

        CLASS zcl_booking_gen_data_#### IMPLEMENTATION.

        METHOD if_oo_adt_classrun~main.
            DATA:
            group_id   TYPE string VALUE '####',
            attachment TYPE /dmo/attachment,
            file_name  TYPE /dmo/filename,
            mime_type  TYPE /dmo/mime_type.

        *   clear data
            DELETE FROM zbooking_####.

            "insert travel demo data
            INSERT zbooking_0000  FROM (
                SELECT
                FROM /dmo/travel AS travel
                FIELDS
                    travel~travel_id        AS travel_id,
                    travel~agency_id        AS agency_id,
                    travel~customer_id      AS customer_id,
                    travel~begin_date       AS begin_date,
                    travel~end_date         AS end_date,
                    travel~booking_fee      AS booking_fee,
                    travel~total_price      AS total_price,
                    travel~currency_code    AS currency_code,
                    travel~description      AS description,
                    CASE travel~status    "[N(New) | P(Planned) | B(Booked) | X(Cancelled)]
                    WHEN 'N' THEN 'O'
                    WHEN 'P' THEN 'O'
                    WHEN 'B' THEN 'A'
                    ELSE 'X'
                    END                     AS overall_status,
                    @attachment             AS attachment,
                    @mime_type              AS mime_type,
                    @file_name              AS file_name,
                    travel~createdby        AS created_by,
                    travel~createdat        AS created_at,
                    travel~lastchangedby    AS last_changed_by,
                    travel~lastchangedat    AS last_changed_at,
                    travel~lastchangedat    AS local_last_changed_at
                    ORDER BY travel_id UP TO 10 ROWS
            ).
            COMMIT WORK.
            out->write( |Demo data generated for table zbooking_{ group_id }. | ).
        ENDMETHOD.
        ENDCLASS.
        
        ```

5. Save and activate your data model.

6. Run your console application. For that, select your ABAP `class zcl_booking_gen_data_####`, select the run button > **Run As** > **ABAP Application (Console) F9** or press **F9**. A message will be displayed.

    ![class](2-3.png)

    ![class](2-4.png)

7. Open your database table `ZBOOKING_####` and press **F8** to start the data preview and display the filled database entries, i.e. travel data.

    ![data](2-5.png)


### Generate the transactional UI services

Create your OData v4 based UI services with the built-in ADT generator.
The generated business service will be transactional, draft-enabled, and enriched with UI semantics for the generation of the Fiori elements app.

1. Right-click your database table `ZBOOKING_####` and select **Generate ABAP Repository Objects** from the context menu.

    ![app](3-1.png)

2. Maintain the required information and click **Next**:

    - Description: Booking App ###
    - Generator: ABAP RESTful Application Programming Model: UI Service

    ![app](3-2.png)


3. Maintain the required information on the **Configure Generator** dialog to provide the name of your data model and generate them.


     | RAP Layer | Artefacts | Artefact Name |
    | ------------- | ---------- | ---------- |
    | Business Object | | |
    | | Data Model | Data Definition Name: **ZEVENT_R_BOOKINGTP_####** |
    | | | Alias Name: **BOOKING** |
    | | Behavior | Implementation Class: **ZEVENT_BP_BOOKINGTP_####** |
    | | | Draft Table Name: **ZEVENT_BOOK0000** |
    | Service Projection (BO Projection) | | Name: **ZEVENT_C_BOOKINGTP_0000** |
    | Business Services | | |
    | | Service Definition | Name: **ZEVENT_UI_BOOKING_0000** |
    | | Service Binding | Name: **ZEVENT_UI_BOOKING_04_0000**  |
    | | | Binding Type: **OData V4 - UI** |



    ![app](3-3.png)


4. Verify the maintained entries and press **Next** to confirm. The needed artefacts will be generated.

    ![app](3-4.png)

5. Select a transport request and click **Finish** to confirm the dialog. 

    ![app](3-5.png)

6. Go to the **Project Explorer**, select your package, refresh it by pressing **F5**, and check all generated ABAP repository objects.

    ![app](3-6.png)

### Publish the Service and Preview the Booking App

1. Open your service binding `ZEVENT_UI_BOOKING_04_####` and click **Publish**.

    ![publish](4-1.png)

2. Double-click on the entity **BOOKING** in the Entity Set and Association section to open the Fiori elements App Preview.

    ![publish](4-2.png)

3. Click the **GO** Button to load the data and check your result.

    ![load](4-3.png)

    ![load](4-4.png)


### Create an Event in Behaviour Definition

  1. You need to create an event which is raised when a booking flight is cancelled. For this, open `ZEVENT_BP_BOOKINGTP_####` and add a new event to the business object. The event name is BookingCancelled.

    ```
      event BookingCancelled parameter ZEVENT_D_BOOKING_CANCEL_####;
      
    ```

    Your behavior definition will be looking like this:

    ![parameter](5-2.png)

  3. In addition,provide a cancellation reason and description. This can be done via a parameter define as a data definition (abstract entity) called `ZEVENT_D_BOOKING_CANCEL_####`.

     ![parameter](5-1.png)

  4. Copy the code below in this definition. Do not forget to save and activate.

    ```ZEVENT_D_BOOKING_CANCEL_####
     @EndUserText.label: 'booking cancelation reason'
     define abstract entity ZEVENT_D_BOOKING_CANCEL_####
     {
        REasonCode : abap.char(2);  
        Description : abap.char(64);
     }

    ```

    ![parameter](5-3.png)


### Creation of an Event Binding for the Business Event

Now create the event binding for your newly created business event. This event binding is needed to map an event to a RAP entity event.

  1. Right-click on your package and create an event binding

    - Name: `ZEVENT_CANCEL_FLIGHT_####`
    - Description: `cancel flight event`

    ![binding](6-1.png)

    ![binding](6-2.png)

  3. Here fill all fields out, to get errors gone. You can freely choose these names to specify your event with some considerations explained below

    - Namespace: `zevent####` (No camel case and no space)
    - Business Object: `booking` (No Space)
    - Business Object Operation: `Delete`

    ![error](6-5.png)

  4. Click **Add** to add items.

    - Root Entity Name: `ZEVENT_R_BOOKINGTP_####` (your behavior definition)
    - Entity Event Name: `BOOKINGCANCELLED` (Event name in your behavior definition)


    ![item](6-7.png)

    ![item](6-3.png)

  5. Save and activate your event binding.

  6. As you can see at the screenshot, **Type** (aka topic) is a concatenation of the three attributes (name space, business object, business object operation) and ends with the version of the event. The wildcard * points to the corresponding event e.g. created. This is relevant for addressing the events to the Event Mesh. Copy this address for later use `zevent####.booking.Delete.v*`.

    ![type](6-4.png)

### Edit behavior implementation class

1. Open your behavior definition `ZEVENT_R_BOOKINGTP_####` and add **with additional save** like following:

    ![add save](7-1.png)

2. Now open the implementation class `ZEVENT_BP_BOOKINGTP_####` and navigate to **Local Type**.

    ![local type](7-2.png)

3. Add the `lcl_event_handler` class and save modified method in there:


    ```
        CLASS lcl_event_handler DEFINITION INHERITING FROM cl_abap_behavior_saver.

        PROTECTED SECTION.
            METHODS save_modified REDEFINITION.

        ENDCLASS.

        CLASS lcl_event_handler IMPLEMENTATION.

        METHOD save_modified.
            IF delete-booking IS NOT INITIAL.
            RAISE ENTITY EVENT zevent_r_bookingtp_####~BookingCancelled
        FROM VALUE #( ( TravelID = '00000006'  %param = VALUE #( reasoncode ='02'  description = 'cancelled by customer in OP' ) ) ).
            ENDIF.
        ENDMETHOD.
        ENDCLASS.

    ```

    ![class](7-3.png)

4. Save and activate your class.

### Creating a channel using a service key

After an event is raised, it must be delivered to the SAP Event Mesh to be consumed later. The connection between the system and the SAP Event Mesh is achieved through a so-called channel. In an on premise system you can directly create the channel in the system.

A channel represents a single connection to a service instance of the SAP Event Mesh. As a prerequisite, you need to prepare an SAP Event Mesh instance in your SAP business technology platform system and download the service key of this instance.

1. Open **SAP Logon** and login in your on premise system and run the transaction `/n/IWXBE/CONFIG`

    ![logon](8-1.png)

2. Press **via Service Key** to create a new channel.

    ![key](8-2.png)

3. Enter a channel name and description and copy the service key of your SAP Event Mesh instance into the corresponding field and click Save.

    ![key](8-3.png)

4. Your newly created channel will now show up in the channel list. Activate the channel by pressing the **Activate** button. If the channel is active, it should appear with a green box under active channels.

    ![active](8-4.png)

    ![active](8-5.png)


### Configure outbound bindings

After creating a channel, you can decide which events should be listed on this channel. This explicit step of maintaining an outbound binding is necessary to publish events by an SAP S/4HANA system.

1. Run `/n/IWXBE/OUTBOUND_CFG` and select your channel.

    ![channel](9-1.png)

2. Click **Create new topic binding** icon and choose the topic which is generated during the event binding creation, `zevent####.booking.Delete.v*`.

    ![topic](9-2.png)

    ![topic](9-3.png)

    ![topic](9-4.png)

3. Check your topic is listed. 

    ![topic](9-5.png)


### Create queue

In the SAP Event Mesh system, create a queue for the selected instance and subscribe it to your topic. For more information on how to create a queue, check [ this ] (https://blogs.sap.com/2022/08/19/an-introduction-to-enterprise-event-enablement-for-sap-btp-abap-environment/) blog post. 

The structure of the topic should be like: your event mesh instance namespace/ce/the event type you are generated and are subscriebed to: `zevent####/booking/delete/*`.

The Topic will look like: `PmEvnt/PmEvnt.sap/Demo/ce/zevent####/booking/Delete/*`


### Create a Message in Application

After you created a queue for the selected instance in the SAP Event Mesh system and subscribe it to your topic,

1. Back to ADT and open service binding `ZEVENT_UI_BOOKING_04_####`, choose **BOOKING** and click **Preview**.

    ![preview](10-1.png)

2. Click **Go** to load all bookings. Select an entry with the chosen ID and delete it. An event will be raised then.

    ![preview](10-2.png)

    ![delete](10-3.png)

3. Back to your queue, hier you can see that a message is in your queue.

    ![message](10-4.png)

4. Click **Test** and choose your queue. Click **Consume message**.

    ![consume](10-5.png)

5. Check the **Message Data**

    ![data](10-6.png)

6. To monitor the generated event on SAP Logon, open **SAP Logon** and run the transaction `/n/IWXBE/EVENT_MONITOR`. Choose your channel and click **Execute**

    ![monitor](10-7.png)

    ![monitor](10-8.png)




### Test yourself
