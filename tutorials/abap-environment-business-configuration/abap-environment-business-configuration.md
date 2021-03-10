---
auto_validation: true
title: Execute Business Configuration Service and Implement Authorization Check for Modify Operations
description: Register your business configuration in SAP Fiori Launchpad. Provide authorizations for a business configuration and implement an authorization check for modify operations.
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-business-technology-platform, tutorial>license ]
time: 20
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
- You need a SAP BTP, ABAP environment license.

## Details
### You will learn  
- How to execute class to register business configuration
- How to define authorization check for read operations
- How to provide authorizations for a business configuration
- How to implement authorization check for modify operations

The **[Maintain Business Configurations](https://help.sap.com/viewer/DRAFT/60f1b283f0fd4d0aa7b3f8cea4d73d1d/Internal/en-US/76384d8e68e646d6ae5ce8977412cbb4.html)** app serves as an entry point to the configuration objects provided by different applications or partners. You can use the app to adjust these configuration objects to change and influence the system behavior.


>HINT: You don't need to create your own Fiori app for a Business Configuration service if you are using the **Maintain Business Configurations** app

---

[ACCORDION-BEGIN [Step 1: ](Execute class to register business configuration)]

You can now register your service with this app via the **[Maintain Business Configurations API](https://help.sap.com/viewer/DRAFT/60f1b283f0fd4d0aa7b3f8cea4d73d1d/Internal/en-US/508d406ac92043dba95f694144803c26.html)**.

  1. Right-click on **Classes** and select **New** > **ABAP Class**.

    ![userinterface](class.png)


  2. Create a new ABAP class:
     - Name: `ZCL_CAL_REGISTER_BC_XXX`
     - Description: `Class for business configuration`

     ![userinterface](class2.png)

      Click **Next >**.

  3.  Click **Finish**.

    ![userinterface](class3.png)


  4. Enter the correct data for `iv_namespace` and `iv_transport` in your **global class**. **`iv_transport`** is your **transport request**, which you can find in the transport organizer. You can use **`iv_namespace`** to set your **namespace**.

    ```ABAP
    CLASS zcl_cal_register_bc_xxx DEFINITION
      PUBLIC
      INHERITING FROM cl_xco_cp_adt_simple_classrun
      FINAL
      CREATE PUBLIC .

      PUBLIC SECTION.
      PROTECTED SECTION.
        METHODS: main REDEFINITION.
      PRIVATE SECTION.
    ENDCLASS.

    CLASS zcl_cal_register_bc_xxx IMPLEMENTATION.

      METHOD main.

        DATA(lo_business_configuration) = mbc_cp_api=>business_configuration(
          iv_identifier = 'ZCAL'
          iv_namespace  = '' ).

        TRY.
            lo_business_configuration->create(
              iv_name                  = 'Z_CAL_MANAGED_XXX'
              iv_description           = 'Public Holiday Calendar'
              iv_service_binding       = 'ZCAL_UI_HOLIDAY_XXX'
              iv_service_name          = 'ZCAL_UI_HOLIDAY_XXX'
              iv_service_version       = 0001
              iv_root_entity_set       = 'HolidayRoot'
              iv_transport             = ''
            ).
            out->write( 'done' ).
          CATCH cx_mbc_api_exception INTO DATA(lx_err).
            out->write( lx_err ).
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.
    ```

  5. Save and activate.

  6. Execute the class by pressing F9. Now you should see following in the ABAP console.

    ![userinterface](console.png)

  7. Log on to the ABAP system and start the **Maintain Business Configurations** app. Search for **`Z_CAL_MANAGED_XXX`** to see your business configuration.

    ![userinterface](configuration.png)

    When selecting the entry, an user interface identical to the preview app of the service is rendered.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Define authorization check for read operations)]

[Authorization control in RAP](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/375a8124b22948688ac1c55297868d06.html) protects your business object against unauthorized access to data.

To protect data from unauthorized read access, the ABAP CDS provides its own authorization concept based on a data control language (DCL). To restrict read access to RAP business objects, it is sufficient to model DCL for the CDS entities used in RAP business objects.

  1. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

  2. Create a new access control:
     - Name: `ZCAL_I_HOLIDAY_XXX`
     - Description: `Access control for cds view`

     ![userinterface](access2.png)

      Click **Next >**.

  3.  Click **Next >**.

    ![userinterface](access3.png)

  4. Select **`Define Role with PFCG Aspect`** as template and click **Finish**.

    ![userinterface](access4.png)

  5. Define the check on authorization object `S_TABU_NAM` with activity **03** (read) and **table** equal to the name of your root CDS View.   

    ```ABAP
    @EndUserText.label: 'Access control for cds view'
    @MappingRole: true
    define role ZCAL_I_HOLIDAY_XXX {
      grant
        select
          on
            ZCAL_I_HOLIDAY_XXX
              where
                ( ) = aspect pfcg_auth( S_TABU_NAM, TABLE = 'ZCAL_I_HOLIDAY_XXX', ACTVT = '03' );

    }
    ```

  6. Save and activate.

  7. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

  8. Create a new access control:
     - Name: `ZCAL_C_HOLIDAY_XXX`
     - Description: `Access control for projection view holiday`

     ![userinterface](accessc.png)

      Click **Next >**.

  9.  Click **Next >**.

    ![userinterface](accessc2.png)

  10. Select **`Define Role with inherited Conditions`** as template and click **Finish**.

    ![userinterface](accessc3.png)

  11. Choose **`ZCAL_I_HOLIDAY_XXX`** as the entity where the conditions are inherited.   

    ```ABAP
    @EndUserText.label: 'Access control for projection view holiday'
    @MappingRole: true
    define role ZCAL_C_HOLIDAY_XXX {
      grant
        select
          on
            ZCAL_C_HOLIDAY_XXX
              where
                inheriting conditions from entity ZCAL_I_HOLIDAY_XXX;

    }
    ```

  12. Save and activate.

  13. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

  14. Create a new access control:
     - Name: `ZCAL_I_HOLIDAYTXT_XXX`
     - Description: `Access control for CDS view holiday text`

     ![userinterface](access11.png)

      Click **Next >**.

  15.  Click **Next >**.

    ![userinterface](access12.png)

 16. Select **`Define Role with inherited Conditions`** as template and click **Finish**.

    ![userinterface](access13.png)

 17. Choose **`ZCAL_I_HOLIDAY_XXX`** as the entity where the conditions are inherited.   

    ```ABAP
    @EndUserText.label: 'Access control for cds view holiday text'
    @MappingRole: true
    define role ZCAL_I_HOLIDAYTXT_XXX {
      grant
        select
          on
            ZCAL_I_HOLIDAYTXT_XXX
              where
                inheriting conditions from entity ZCAL_I_HOLIDAY_XXX;

    }
    ```

 18. Save and activate.

 19. In the project explorer, right click on **Data Definitions** and select **New** > **Access Control**.

     ![userinterface](access.png)

 20. Create a new access control:
     - Name: `ZCAL_C_HOLIDAYTXT_XXX`
     - Description: `Access control for holiday text`

     ![userinterface](access8.png)

      Click **Next >**.

 21.  Click **Next >**.

    ![userinterface](access9.png)

 22. Select **`Define Role with inherited Conditions`** as template and click **Finish**.

    ![userinterface](access10.png)

 23. Replace your code with following.  

    ```ABAP
    @EndUserText.label: 'Access control for holiday text'
    @MappingRole: true
    define role ZCAL_C_HOLIDAYTXT_XXX {
      grant
        select
          on
            ZCAL_C_HOLIDAYTXT_XXX
              where
                inheriting conditions from entity ZCAL_I_HOLIDAYTXT_XXX;

    }
    ```

  24. Save and activate.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Provide authorizations for a business configuration)]

  1. Right-click on **`Z_CALENDAR_XXX`** and select **New** > **Other ABAP Repository Object**.

    ![authorizations](iam.png)

  2. Search for **IAM App**, select it and click **Next >**.

    ![authorizations](iam2.png)

  3. Create a new IAM app:
      - Name:`ZCAL_UI_HOLIDAY_O4_XXX`
      - Description: IAM App for factory calendar
      - Application Type: `MBC - Business Configuration App`

      ![authorizations](iam3.png)

      Click **Next >**.

  4. Click **Finish**.

      ![authorizations](iam4.png)

  5. Select **Services** and add a new service to the IAM App.

      ![authorizations](iam5.png)

  6. Add service to IAM app:
     - Service Type: `OData V4`
     - Service Name: `ZCAL_UI_HOLIDAY_XXX`

      ![authorizations](iam6.png)

      Click **OK**.

  7. Select **Authorization** and add an authorization object to the IAM App.

     ![authorizations](iam7.png)

  8. Search for **`S_TABU_NAM`** and click **OK** to add the authorization object.

     ![authorizations](iam8.png)

  9. Click **`S_TABU_NAM`** and add **Display** to `ACTVT` and `ZCAL_I_HOLIDAY_XXX` to `TABLE`.

    ![authorizations](iam9.png)

 10. Save, activate and publish the IAM app.

    ![authorizations](iam10.png)

 11. Right-click on **`Z_CALENDAR_XXX`** and select **New** > **Other ABAP Repository Object**.

    ![authorizations](iam.png)

 12. Search for **Business Catalog**, select it and click **Next >**.

    ![authorizations](catalog.png)

 13. Create a new business catalog:
      - Name: `Z_BC_HOLIDAY_XXX`
      - Description: Business catalog for holiday

      ![authorizations](catalog2.png)

      Click **Next >**.

 14. Click **Finish**.

      ![delete](catalog3.png)

 15. Select **Apps** and add a new app to the business catalog.

      ![authorizations](catalog4.png)

 16. Add app to business catalog:
     - IAM App: `ZCAL_UI_HOLIDAY_O4_XXX_MBC`
     - Name: `Z_BC_HOLIDAY_XXX_0001`
     - Description: Business Catalog to IAM App assignment

      ![authorizations](catalog5.png)

      Click **Next >**.

 17. Click **Finish**.

     ![authorizations](catalog6.png)

 18. Click **Publish Locally**.

     ![authorizations](catalog7.png)

     Once you have created the IAM app and business catalog, assign the catalog to a business role on the SAP Fiori Launchpad by following [this user guide](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/8980ad05330b4585ab96a8e09cef4688.html). Finally, follow [this user guide](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/e40e710321c74f28916affa9ae984bce.html) to assign the business role to business users to grant the rights to use your business configuration inside the maintain business configurations SAP Fiori app.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Implement authorization check for modify operations)]
In RAP business objects, modifying operations, such as standard operations and actions can be checked against unauthorized access during runtime.

  1. Open behavior definition **`ZCAL_I_HOLIDAY_XXX`** and add `authorization master( global )` to the root entity and `authorization dependent by _Public_Holiday` to the text entity.

    ```ABAP
    managed with additional save implementation in class zbp_cal_i_holiday_xxx unique;
    with draft;

    define behavior for ZCAL_I_HOLIDAY_XXX alias HolidayRoot
    persistent table zcal_holiday_xxx
    draft table zcal_d_holi_xxx
    lock master total etag last_changed_at
    authorization master( global )
    etag master local_last_changed_at
    {
      create;
      update;

      field ( readonly : update ) HolidayId;
      field ( readonly ) configdeprecationcode;

      association _HolidayTxt { create; with draft; }

      validation val_transport on save
      { create; update; delete; }

      action ( features : instance ) deprecate result [1] $self;
      delete ( features : global );

      mapping for zcal_holiday_xxx corresponding
      {
        HolidayId = holiday_id;
        MonthOfHoliday = month_of_holiday;
        DayOfHoliday = day_of_holiday;
      }
    }

    define behavior for ZCAL_I_HOLIDAYTXT_XXX alias HolidayText
    persistent table zcal_holitxt_xxx
    draft table zcal_d_holit_xxx
    authorization dependent by _Public_Holiday
    etag dependent by _Public_Holiday
    lock dependent by _Public_Holiday
    {
      update;
      delete;
      field ( readonly : update ) HolidayId;
      field ( readonly : update ) Language;

      association _Public_Holiday { with draft; }

      validation val_transport on save
      { create; update; delete; }

      mapping for zcal_holitxt_xxx corresponding
      {
        Language = spras;
        HolidayId = holiday_id;
        HolidayDescription = fcal_description;
      }
    }
    ```

  2. Place the cursor on **`ZCAL_I_HOLIDAY_XXX`** and use the quick assist CTRL+1 to create the missing method.

    ![authorizations](cursor.png)

  3. Your local types should look like this:

    ```ABAP
    CLASS lsc_zcal_i_holiday_xxx DEFINITION INHERITING FROM cl_abap_behavior_saver.

      PROTECTED SECTION.

        METHODS save_modified REDEFINITION.

    ENDCLASS.

    CLASS lsc_zcal_i_holiday_xxx IMPLEMENTATION.

      METHOD save_modified.
      zcl_bc_transport_api_f_xxx=>get_transport_api( use_table_scomp_transport = abap_false )->transport(
          table_entity_relations = VALUE #( ( table = 'ZCAL_HOLIDAY_XXX'    entity = 'HOLIDAYROOT' )
                                            ( table = 'ZCAL_HOLITXT_XXX'    entity = 'HOLIDAYTEXT' ) )
          create                 = REF #( create )
          update                 = REF #( update )
          delete                 = REF #( delete )
      ).
      ENDMETHOD.

    ENDCLASS.

    CLASS lhc_holidaytext DEFINITION INHERITING FROM cl_abap_behavior_handler.

      PRIVATE SECTION.

        METHODS val_transport FOR VALIDATE ON SAVE
          IMPORTING keys FOR HolidayText~val_transport.

    ENDCLASS.

    CLASS lhc_holidaytext IMPLEMENTATION.

      METHOD val_transport.
        DATA create TYPE TABLE FOR CREATE zcal_i_holidaytxt_xxx.
        zcl_bc_transport_api_f_xxx=>get_transport_api( use_table_scomp_transport = abap_false )->validate(
            table_entity_relation = VALUE #( table = 'ZCAL_HOLITXT_XXX' entity = 'HOLIDAYTEXT' )
            keys                  = REF #( keys )
            reported              = REF #( reported )
            failed                = REF #( failed )
            create                = REF #( create )
        ).
      ENDMETHOD.

    ENDCLASS.

    CLASS lhc_HolidayRoot DEFINITION INHERITING FROM cl_abap_behavior_handler.
      PRIVATE SECTION.

       CONSTANTS:
        BEGIN OF config_code,
          valid    TYPE config_deprecation_code VALUE '',
          obsolete TYPE config_deprecation_code VALUE 'W',
          not_valid TYPE config_deprecation_code VALUE 'E',
        END OF config_code.

        METHODS get_instance_features FOR INSTANCE FEATURES
          IMPORTING keys REQUEST requested_features FOR HolidayRoot RESULT result.
        METHODS val_transport FOR VALIDATE ON SAVE
          IMPORTING keys FOR holidayroot~val_transport.
        METHODS deprecate FOR MODIFY
          IMPORTING keys FOR ACTION holidayroot~deprecate RESULT result.
        METHODS get_global_features FOR GLOBAL FEATURES
          IMPORTING REQUEST requested_features FOR holidayroot RESULT result.
        METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
          IMPORTING REQUEST requested_authorizations FOR holidayroot RESULT result.

    ENDCLASS.

    CLASS lhc_HolidayRoot IMPLEMENTATION.

      METHOD get_instance_features.
      READ ENTITIES OF zcal_i_holiday_xxx IN LOCAL MODE
      ENTITY HolidayRoot
        FIELDS ( configdeprecationcode ) WITH CORRESPONDING #( keys )
      RESULT DATA(holidays)
      FAILED failed.

      result =
        VALUE #(
          FOR holiday IN holidays
            LET deprecate =  COND #( WHEN holiday-configdeprecationcode = config_code-valid
                                      THEN if_abap_behv=>fc-o-enabled
                                      ELSE if_abap_behv=>fc-o-disabled  )
            IN
              ( %tky                 = holiday-%tky
                %action-deprecate = deprecate
               ) ).
      ENDMETHOD.

      METHOD val_transport.
        DATA create TYPE TABLE FOR CREATE zcal_i_holiday_xxx.
        zcl_bc_transport_api_f_xxx=>get_transport_api( use_table_scomp_transport = abap_false )->validate(
            table_entity_relation = VALUE #( table = 'ZCAL_HOLIDAY_XXX' entity = 'HOLIDAYROOT' )
            keys                  = REF #( keys )
            reported              = REF #( reported )
            failed                = REF #( failed )
            create                = REF #( create )
        ).
      ENDMETHOD.

      METHOD deprecate.
       MODIFY ENTITIES OF zcal_i_holiday_xxx IN LOCAL MODE
      ENTITY HolidayRoot
         UPDATE
           FIELDS ( configdeprecationcode )
           WITH VALUE #( FOR key IN keys
                           ( %tky         = key-%tky
                             configdeprecationcode = config_code-obsolete ) )
      FAILED failed
      REPORTED reported.

      READ ENTITIES OF zcal_i_holiday_xxx IN LOCAL MODE
        ENTITY HolidayRoot
          ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(holidays).

      result = VALUE #( FOR holiday IN holidays
                          ( %tky   = holiday-%tky
                            %param = holiday ) ).
      ENDMETHOD.

      METHOD get_global_features.
         result = VALUE #( %delete = if_abap_behv=>fc-o-disabled ).
      ENDMETHOD.

      METHOD get_global_authorizations.
         zcl_bc_transport_api_f_xxx=>get_bc_authorization( )->get_global_authorization(
             cds_view_name = 'ZCAL_I_HOLIDAY_XXX'
             result        = REF #( result ) ).
      ENDMETHOD.

    ENDCLASS.
    ```

  4. Save. Activation not yet possible.

  5. In the project explorer, right click on **Interfaces** and select **New** > **ABAP Interface**.

      ![authorizations](interface.png)

  6. Create a new ABAP interface:
     - Name: `ZIF_BC_AUTHORIZATION_XXX`
     - Description: Authorization interface

      ![authorizations](interface2.png)

      Click **Next >**.

  7. Click **Finish**.

      ![authorizations](interface3.png)

  8. Replace your code with following:

    ```ABAP
    INTERFACE zif_bc_authorization_xxx
      PUBLIC .
      METHODS
        get_global_authorization
          IMPORTING
            cds_view_name TYPE tabname
            result       TYPE REF TO data.
    ENDINTERFACE.
    ```

  9. Save and activate the interface.

 10. Open class `ZCL_BC_TRANSPORT_API_F_XXX` and add new method `get_bc_authorization`.

    ```ABAP
    CLASS ZCL_BC_TRANSPORT_API_F_XXX DEFINITION PUBLIC FINAL CREATE PUBLIC
       GLOBAL FRIENDS zth_bc_injector_xxx.
      PUBLIC SECTION.
        CLASS-METHODS get_transport_api
          IMPORTING
            client_field    TYPE string DEFAULT 'CLIENT'
            use_table_scomp_transport type abap_bool DEFAULT abap_false
          RETURNING
            VALUE(result) TYPE REF TO zif_bc_transport_api_xxx.
        CLASS-METHODS get_bc_authorization
          RETURNING VALUE(result) TYPE REF TO zif_bc_authorization_xxx.
      PRIVATE SECTION.
        CLASS-DATA transport_api TYPE REF TO zif_bc_transport_api_xxx.
        CLASS-DATA bc_authorization TYPE REF TO zif_bc_authorization_xxx.
    ENDCLASS.

    CLASS ZCL_BC_TRANSPORT_API_F_XXX IMPLEMENTATION.

      METHOD GET_BC_AUTHORIZATION.
        IF bc_authorization IS BOUND.
          result = bc_authorization.
        ELSE.
          result = NEW zcl_bc_authorization_xxx( ).
        ENDIF.
      ENDMETHOD.

      METHOD get_transport_api.
        IF transport_api IS BOUND.
          result = transport_api.
        ELSE.
          result = NEW zcl_bc_transport_api_xxx(
            client_field        = client_field
            use_table_scomp_transport = use_table_scomp_transport
          ).
        ENDIF.
      ENDMETHOD.
    ENDCLASS.
    ```

 11. Save. Activation yet not possible.

 12. Right-click on **`Classes`** and select **New** > **ABAP Class**.

    ![authorizations](classnew.png)

 13. Create a new ABAP class:
     - Name: `ZCL_BC_AUTHORIZATION_XXX`
     - Description: Class for authorization

     ![authorizations](classnew2.png)

     Click **Next >**.

 14. Click **Finish**.

     ![authorizations](classnew3.png)

 15. Replace your code with following:

    ```ABAP
    CLASS zcl_bc_authorization_xxx DEFINITION
      PUBLIC
      FINAL
      CREATE PROTECTED GLOBAL FRIENDS zcl_bc_transport_api_f_xxx.

      PUBLIC SECTION.

        INTERFACES zif_bc_authorization_xxx .
      PROTECTED SECTION.
      PRIVATE SECTION.
    ENDCLASS.

    CLASS zcl_bc_authorization_xxx IMPLEMENTATION.

      METHOD zif_bc_authorization_xxx~get_global_authorization.
        AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD cds_view_name ID 'ACTVT' FIELD '02'.
        DATA(is_authorized) = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed
                                      ELSE if_abap_behv=>auth-unauthorized ).
        ASSIGN result->* TO FIELD-SYMBOL(<result_structure>).
        ASSIGN COMPONENT cl_abap_behv=>co_techfield_name-create OF STRUCTURE <result_structure> TO FIELD-SYMBOL(<comp>).
        IF sy-subrc = 0.
          <comp> = is_authorized.
        ENDIF.
        ASSIGN COMPONENT cl_abap_behv=>co_techfield_name-update OF STRUCTURE <result_structure> TO <comp>.
        IF sy-subrc = 0.
          <comp> = is_authorized.
        ENDIF.
        ASSIGN COMPONENT cl_abap_behv=>co_techfield_name-delete OF STRUCTURE <result_structure> TO <comp>.
        IF sy-subrc = 0.
          <comp> = is_authorized.
        ENDIF.
        ASSIGN COMPONENT cl_abap_behv=>co_techfield_name-action OF STRUCTURE <result_structure> TO FIELD-SYMBOL(<action_comp>).
        IF sy-subrc = 0.
          DATA comp TYPE i.
          DO.
            comp += 1.
            ASSIGN COMPONENT comp OF STRUCTURE <action_comp> TO <comp>.
            IF sy-subrc = 0.
              <comp> = is_authorized.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDMETHOD.
    ENDCLASS.
    ```

 16. Save and activate all inactive objects.

 17. Edit class `ZTH_BC_INJECTOR_XXX`.

    ```ABAP
    CLASS zth_bc_injector_xxx DEFINITION
      PUBLIC
      FINAL
      FOR TESTING
      CREATE PUBLIC .

      PUBLIC SECTION.
        CLASS-METHODS set_transport_api IMPORTING transport_api TYPE REF TO zif_bc_transport_api_xxx.
        CLASS-METHODS set_bc_auth      IMPORTING bc_auth      TYPE REF TO zif_bc_authorization_xxx.
      PROTECTED SECTION.
      PRIVATE SECTION.
    ENDCLASS.

    CLASS zth_bc_injector_xxx IMPLEMENTATION.
      METHOD set_bc_auth.
        zcl_bc_transport_api_f_xxx=>bc_authorization = bc_auth.
      ENDMETHOD.

      METHOD set_transport_api.
        zcl_bc_transport_api_f_xxx=>transport_api = transport_api.
      ENDMETHOD.
    ENDCLASS.
    ```

 18. Save and activate

 19. Restart your app, try to create a new configuration entry. You will get the error message **You are not authorized for this modification**.

 20. Edit your previously created IAM app. Add **Change** for **`ACTVT`** of Authorization Object `S_TABU_NAM` and save.

     ![authorizations](change.png)

 21. Logout from Fiori Launchpad if you are using the **Maintain Business Configurations** app or close & re-open your ADT project if you are using the preview of the service binding. Try to create a new configuration entry. You are once again authorized to create a new configuration entry.

     ![authorizations](accessagain.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
