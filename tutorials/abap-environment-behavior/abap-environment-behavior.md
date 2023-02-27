---
parser: v2
auto_validation: true
primary_tag: software-product>sap-btp--abap-environment
tags: [  tutorial>beginner, programming-tool>abap-development, software-product>sap-business-technology-platform ]
time: 15
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Create Behavior Definition for Managed Scenario
<!-- description --> Create behavior definition and implementation for managed scenario.

## Prerequisites  
- You need an SAP BTP, ABAP environment [trial user](abap-environment-trial-onboarding) or a license.
- You have downloaded and installed the [latest ABAP Development Tools (ADT)] (https://tools.hana.ondemand.com/#abap).

## You will learn  
  - How to create behavior definition
  - How to create behavior implementation
  - How to create behavior definition for projection view

## Intro
In this tutorial, wherever XXX appears, use a number (e.g. 000).

---

### Create behavior definition

  1. Right-click on your data definition `ZI_TRAVEL_M_XXX` and select **New Behavior Definition**. 

      ![Create behavior definition](definition.png)

  2. Check your behavior definition. Your **implementation type** is **managed**.

     Click **Next >**.

      ![Create behavior definition](definition2.png)

  3. Click **Finish** to use your transport request.

      ![Create behavior definition](definition3.png)

  4. Replace your code with following.

    ```ABAP
    managed implementation in class zbp_i_travel_m_xxx unique;

    define behavior for ZI_TRAVEL_M_XXX alias Travel
    persistent table ztravel_xxx
    etag master last_changed_at
    lock master
    {

      // semantic key is calculated in a determination
      field ( readonly ) travel_id;

      // administrative fields (read only)
      field ( readonly ) last_changed_at, last_changed_by, created_at, created_by;

      // mandatory fields that are required to create a travel
      field ( mandatory ) agency_id, overall_status, booking_fee, currency_code;

      // mandatory fields that are required to create a travel
      field ( mandatory ) Begin_Date, End_Date, Customer_ID;

      // standard operations for travel entity
      create;
      update;
      delete;
    }  
    ```

  5. Save and activate.

      ![save and activate](activate.png)

    A warning will appear first, but after the creation of the behavior implementation it will disappear.  

    Now the **behavior definition** is created and determines the create, update and delete functionality for travel booking.



### Create behavior definition for projection view

  1. Right-click on your data definition `ZC_TRAVEL_M_XXX` and select **New Behavior Definition**.

      ![Create behavior definition for projection view](projection.png)

  2. Check your behavior definition. Your implementation type is projection.

     Click **Next >**.

      ![Create behavior definition for projection view](projection2.png)

  3. Click **Finish** to use your transport request.

      ![Create behavior definition for projection view](projection3.png)

  4. Replace your code with following:

    ```ABAP
    projection;

    define behavior for ZC_TRAVEL_M_XXX alias TravelProcessor
    use etag
    {
      use create;
      use update;
      use delete;
    }
    ```

  5. Save and activate.

      ![save and activate](activate.png)

  6. Now switch to your service binding and activate your service binding again.

     ![Create behavior definition for projection view](activate2.png)

  7. Double-click on `TravelProcessor` to start your preview.

      ![Create behavior definition for projection view](projection4.png)

  7. **Refresh** your browser and check your result.

    >**HINT**: If you don't see the create and delete buttons, then please unpublish your service binding, activate and publish it again.
     The create and delete button appears on the UI because of the managed scenario.
     You can create and edit travel bookings or you' re able to delete existing ones.

     Please note that the semantic key Travel ID is not calculated yet. We will do this in the next tutorial.

      ![Create behavior definition for projection view](projection5.png)


### Test yourself



---
