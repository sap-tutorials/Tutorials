---
parser: v2
auto_validation: true
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, topic>abap-development, software-product>sap-business-technology-platform]
time: 15
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Enhance the Business Object Behavior With Dynamic Feature Control
<!-- description --> Enhance the business object behavior using dynamic feature control with SAP BTP ABAP environment.

## Prerequisites
- You need to have access to an SAP BTP, ABAP environment, or SAP S/4HANA Cloud, ABAP environment or SAP S/4HANA (release 2021 or higher) system.
For example, you can create [free trial user on SAP BTP, ABAP environment](abap-environment-trial-onboarding).
- You have downloaded and installed the [latest ABAP Development Tools (ADT)] (https://tools.hana.ondemand.com/#abap) on the latest Eclipse© platform.



## You will learn  
  - How to define dynamic instance feature control
  - How to implement dynamic instance feature control
  - How to preview and test enhanced travel app

  In the previous exercise, you've defined and implemented different instance-bound actions for the BO entity Travel.

  In the present exercise, you will implement the dynamic instance feature control for some of the standard and non-standard operations of the Travel entity.

---

## Intro
>Reminder: Do not forget to replace the suffix placeholder ### with your chosen or assigned group ID in the exercise steps below.
>**About: Dynamic Feature Control**  
>
> As an application developer you may want to determine based on certain attributes of your business object entity, which fields should be read-only or mandatory or which functionality like update or actions are allowed.  As this property is related to an instance of this business object it is called Dynamic Feature Control.
>
> **Further reading**: [Adding Static and Dynamic Feature Control](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/b6eb96dd784247a99cf8d70f77232ba4.html)

### Define dynamic instance feature control 

Define the dynamic instance feature control for the standard operations **`update`** and **`delete`**, the draft action **`Edit`**, and the instance action **`deductDiscount`**. If implemented in the previous exercise, you'll also define the dynamic instance feature control for the optional instance actions `acceptTravel` and `rejectTravel`.

 1. Open your behavior definition ![behaviordefinition](adt_bdef.png) **`ZRAP100_R_TRAVELTP_###`** and add the addition **`( features : instance )`** to the following operations as shown on following code snippet and the screenshot below:
    
    - Standard operations **`update`** and **`delete`**
    - Draft action **`Edit`**
    - Instance actions **`acceptTravel`**, **`rejectTravel`**, and **`deductDiscount`**

    ```ABAP
    ...
    create;
    update ( features : instance ) ;
    delete ( features : instance ) ;
    ...
    action ( features : instance ) deductDiscount parameter /dmo/a_travel_discount result [1] $self;         
    ...
    draft action ( features : instance ) Edit;
    ```

    In case you've defined and implemented the instance actions `acceptTravel` and `rejectTravel` in the previous exercise, then also add the code snippet provided below as shown on the screenshot.

    ```ABAP
    action ( features : instance ) acceptTravel result [1] $self;
    action ( features : instance ) rejectTravel result [1] $self;  
    ```

    ![Travel Behavior Definition](f.png)

 2. Save ![save icon](adt_save.png) and activate ![activate icon](adt_activate.png) the changes.

 3. At the top of the behavior definition, set the cursor on BO entity name ![bdef icon](adt_bdef.png) **`ZRAP100_R_TRAVELTP_###`** and press **CTRL+1** to open the **Quick Assist** view.

    Select the entry **Add method for operation `instance_features` of entity `zrap100_r_traveltp_###`** to add the required methods to the local handler class `lcl_handler` of your behavior pool ![class icon](adt_class.png) **`ZRAP100_BP_TRAVELTP_###`**.

    The result should look like this:

    ![Travel BO Behavior Pool](l.png)

 4. Check the interface of the method **`get_instance_features`** in the declaration part of the local handler class in the behavior pool ![class icon](adt_class.png) **`ZRAP100_BP_TRAVEL_###`**.  

    Set the cursor on one of the method name, press **F2** to open the **ABAP Element Info** view, and examine the full method interface.  
 
    ![Travel BO Behavior Pool](l2.png)

    **Short explanation**:  
    - The addition **FOR INSTANCE FEATURES** after the method name indicates that this method provides the implementation of an instance-based dynamic feature control.
    - Method signature of the instance method `get_instance_features`:

      - **IMPORTING** parameter **keys** - a table containing the keys of the instances on which the action must be executed

      -  Implicit **IMPORTING** parameter **`requested_features`** - structure reflecting which elements (fields, standard operations, and actions) of the entity are requested for dynamic feature control by the consumer.
      - Implicit **CHANGING** parameters (aka `_implicit response parameters_`):  

        - **result** - used to store the result of the performed  

        - **failed** - table with information for identifying the data set where an error occurred

        - **reported** - table with data for instance-specific messages

     Go ahead with the implementation.  

 
 ### Implement dynamic instance feature control 

 Implement the dynamic instance feature control for the standard operations update and delete, the draft action `Edit`, and the instance action `deductDiscount`. If defined in the previous exercise steps, you'll also implement the dynamic instance feature control for the the instance actions `acceptTravel` and `rejectTravel`.

 Following dynamic behavior will be implemented in the backend - and shown on the Fiori UI:
  - If a travel instance has the overall status Accepted (A), then the standard operations update and delete, and the actions Edit and `deductDiscount` must be disabled for the given instance.
  - In addition, following toggle behavior (enable/disable) should be implemented:
    - If the overall status is Accepted (A), then the action `acceptTravel` must be disabled.
    - If the overall status is Rejected (X), then the action `rejectTravel` must be disabled.
 
 1. Implement the instance feature control method **`get_instance_features`** in the implementation part of the local handler class.

    The logic consists of the following steps:  

    1. Read the relevant data of the transferred **travel** instances. Only the fields **`TravelID`** and **`OverallStatus`** are needed to determine the operation state in the present scenario.
    2. Evaluate the conditions and determine the state of the different operations. The `COND` operator is used inline in the present scenario for the purpose.

    3. Set the result set appropriately.   

    For that, replace the current method implementation with the code snippet provided below and replace all occurrences of the placeholder **`###`** with your group ID.
    You can make use of the **F1 Help** for more information about the EML statements and other ABAP constructs.

    ```ABAP
    **************************************************************************
    * Instance-based dynamic feature control
    **************************************************************************
      METHOD get_instance_features.
       " read relevant travel instance data
        READ ENTITIES OF ZRAP100_R_TravelTP_### IN LOCAL MODE
          ENTITY travel
            FIELDS ( TravelID OverallStatus )
            WITH CORRESPONDING #( keys )
          RESULT DATA(travels)
          FAILED failed.

        " evaluate the conditions, set the operation state, and set result parameter
        result = VALUE #( FOR travel IN travels
                          ( %tky                   = travel-%tky

                            %features-%update      = COND #( WHEN travel-OverallStatus = travel_status-accepted
                                                             THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                            %features-%delete      = COND #( WHEN travel-OverallStatus = travel_status-open
                                                             THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )
                            %action-Edit           = COND #( WHEN travel-OverallStatus = travel_status-accepted
                                                             THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
    *                        %action-acceptTravel   = COND #( WHEN travel-OverallStatus = travel_status-accepted
    *                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
    *                        %action-rejectTravel   = COND #( WHEN travel-OverallStatus = travel_status-rejected
    *                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                            %action-deductDiscount = COND #( WHEN travel-OverallStatus = travel_status-open
                                                              THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )
                         ) ).

      ENDMETHOD.
    ```   

    Your source code should look like this:

    ![Travel Behavior Pool](l3.png)

 2. Save ![save icon](adt_save.png) and activate ![activate icon](adt_activate.png) the changes.

    You're through with the implementation.



### Preview and test the enhanced travel app

Now the SAP Fiori elements app can be tested.

You can either refresh your application in the browser using **F5** if the browser is still open - or go to your service binding **`ZRAP100_UI_TRAVEL_O4_###`** and start the Fiori elements App preview for the **Travel** entity set.

You can go ahead and test the logic of the dynamic feature control implemented in the backend.

For example, select a **travel** instance has the overall status **Accepted**, and check the state of the **Accepted**, the **Edit**, and the **Delete** buttons. They all shall be disable.

Remember the implemented dynamic BO behavior expected on the UI:

  If a **travel** instance has the overall status **Accepted** **(A)**  or **Rejected** **(X)**, then the button **Edit** and **Delete** must be disabled for the given instance.

  In addition, following toggle behavior (enable/disable) should be displayed for both instance actions:
    - If the overall status **Accepted** **(A)**, then the action **Accept Travel** must be disabled.
    - If the overall status **Rejected** **(X)**, then the action **Reject Travel** must be disabled

   ![Travel App Preview](preview10.png)
 


### Test yourself




---
