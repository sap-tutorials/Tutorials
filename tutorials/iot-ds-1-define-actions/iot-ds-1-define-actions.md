---
title: Model the Needed Decision Support Based on IoT Data
description: Create actions, rules and configurations using SAP Leonardo IoT

auto_validation: true
time: 30
tags: [ tutorial>beginner, products>sap-leonardo-iot, topic>internet-of-things ]
primary_tag: products>sap-cloud-platform-internet-of-things
---

## Prerequisites
 - Complete Step 1 of [Get Your Hands on SAP Leonardo IoT Foundation Software and Hardware](iot-express-1-buy-sapstore).
 - Complete [Create a Simple IoT Device](iot-express-2-create-device-model).
 - Complete [Create a Thing Model and Bind to Device](iot-express-4-create-thing-model).

## Details
### You will learn
  - How to create a new Rule Context
  - How to create a new Streaming Rule
  - How to create an Action of type Decision Support
  - How to create an Action of type In-App Notification
  - How to create a Decision Support Configuration

---
In this tutorial, we'll create a streaming rule for high greenhouse temperature which triggers an action of type Decision Support.  This action will in turn trigger another action of type In-App Notification which creates Fiori Notifications for a set of recipients.  We'll also define a Decision Support configuration with two options.  One is a Quick Create application and the other one is a Fiori Navigation.

[ACCORDION-BEGIN [Step 1: ](Create rule context)]
  Please complete the prerequisites before starting this step.

  1. From Leonardo IoT Launchpad, select the **Rule Contexts** tile.

    ![New Rule Context 0](/images/launchpad_tile_rulecontexts.png)

  2. Create a new Rule Context. If you have already created a Rule Context for greenhouse from the tutorial [Define a Rule and Trigger an Action](iot-express-4a-define-rule-action), you can skip to the next Step **Create streaming rule**.

    ![New Rule Context 1](/images/rulectxt_new_1.png)

  3. Enter `greenhouserulecontext` in field **Name**.

  4. Enter `Greenhouse Rule Context` in the field **Short Text** and **Description**.

    ![New Rule Context 4](/images/rulectxt_new_4_1.png)

  5. Add a new **Property Set**.

    ![New Rule Context 5](/images/rulectxt_new_5.png)

  6. In the dialog, select `greenhouse` for **Package** and `greenhouseType` for **Thing Type**.  The entry `envData` should be displayed automatically in the **Property Sets** section.  Select `envData` and click **OK**.

    ![New Rule Context 2](/images/rulectxt_new_2_1.png)

  7. The new property set `envData` has been added.

    ![New Rule Context 3](/images/rulectxt_new_3_1.png)

  8. Click **Save** to save the changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create streaming rule)]
  This tutorial is based on the greenhouse device used in the Leonardo IoT Foundation tutorials.  You can find these tutorials under Prerequisites.  Please complete them before continuing with this tutorial.

  1. From Leonardo IoT Launchpad, select the **Rules** tile.

    ![New Rule 0](/images/launchpad_tile_rules.png)

  2. Create a new **Streaming Cloud** rule.

    ![New Rule 1](/images/rule_new_1_1.png)

  3. In General Information, enter `High Greenhouse Temperature` in the **Name** field.

  4. In the field **Description**, enter `Temperature in Greenhouse is above 25 degrees`.

    ![New Rule 2](/images/rule_new_2.png)

  5. In Definition, click on the field **Rule Context**.

    ![New Rule 3](/images/rule_new_3_1.png)

  6. Select the Rule Context for your Greenhouse. The name of your rule context might be different.

    ![New Rule 4](/images/rule_new_4.png)

  7. In the next dialog, select `envData`.

    ![New Rule 8](/images/rule_new_8.png)

  8. Confirm the selection.

    ![New Rule 5](/images/rule_new_5_1.png)

  9. Enter the following rule in the **If** field of Ruled Editor.

    `temperature of envData is greater than 25`

    ![New Rule 6](/images/rule_new_6.png)

    ![New Rule 7](/images/rule_new_7_1.png)

  10. **Activate** the Rule.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create decision support action)]

  1. From Leonardo IoT Launchpad, select the **Actions** tile.

    ![Actions Tile](/images/launchpad_tile_actions.png)

  2. On the Actions page, click **New** to create a new action.

    ![Actions New](/images/actions_new.png)

  3. Set **Name** to `Greenhouse Action`.

  4. In the **Triggered by** field, select `Event from Rule`.

  5. In the **Rule** dropdown, select `High Greenhouse Temperature`. The system populates **Thing Type** automatically.

  6. In the **Action Type** dropdown, select `Decision Support`.

  7. Enter `Greenhouse_CF` for **Alias** field.  You can enter a different value of your preference. This value will be used in a later tutorial step.

  8. If you want to display device event data and `Thing` master data in a Decision Support application, you can do so by providing a JSON payload. To include master data properties from the `Thing` model, please use the following notation:

    `${propertyname}`

    For example:

    ![Actions paylod thing data](/images/actions_payload_thing_data.png)

    For our scenario, we'll enter a sample JSON payload with a set of static values in the **Payload** text area.

    ```JSON
    "properties": {
       "device": {
          "exception":"EX001",
          "ServicePriorityCode":"2",
          "ProcessingTypeCode":"QOO",
          "CustomerID":"6789",
          "InstallationPointID":"47",
          "SerialID":"ABC-19-005-1",
          "DataOriginTypeCode":"13"
      }
    }
    ```	  

  9. Click **Save** to save your change.

    ![Actions Create](/images/actions_new_ds_details_1.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create in-app notification)]

  1. On the Actions page, click **New** to create a new action.

    ![Actions InApp New](/images/actions_new.png)

  2. Enter `Greenhouse In-App Notification Action` in the **Name** field.  

  3. In **Triggered By** dropdown, select `Event from Action`.

  4. In **Action** dropdown, select `Action - Greenhouse Action`. The system populates **Thing Type** automatically.  

  5. In the **Action Type** dropdown, select `In-App Notification`.  

  6. In **Recipients** field, enter your email address.  

  7. Enter `GreenhouseDSApp` as **Target Object** and `display` as **Target Action**.  These values are defined in Step 2 of [Building the Decision Support UI in the Web IDE](iot-ds-3-create-ui)

  8. In **Text** field, enter `High Greenhouse Temperature Alert`.

  9. Click **New** in the section **Target Parameters**.

    ![Actions InApp Target](/images/actions_new_inappnotif_details_1.png)

  10. Enter `Guid` for **Key** field.  

  11. Enter `${RecommendationServiceGuid}` for **Value** field.

  12.  Click **Save**.

    ![Actions InApp Target](/images/actions_new_inappnotif_details_2.png)

  13. Click **Save** to save your changes.  

    ![Actions Details](/images/actions_new_inappnotif_details.png)

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create decision support configuration)]

  1. From Leonardo IoT Launchpad, select the **Decision Support Definition** tile.

    ![DS Tile](/images/launchpad_tile_ds.png)

  2. On the Decision Support page, click **Create** to create a new Decision Support.

    ![DS New](/images/ds_new.png)

  3. On the next page, set **Name** to `High Greenhouse Temperature`.

  4. In the **Alias** field, enter `Greenhouse_CF`.

  5. Check the checkbox **Active**

  6. Click **Save** to save the changes.

  7. You will navigate back to the previous page.

    ![DS Config_1](/images/ds_new_config_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Define a quick create option)]

  1. Select the entry `High Greenhouse Temperature` on the list.

  2. In the Details page, click the **Create** button under the section "Possible Actions".

    ![DS Create Action_Button](/images/ds_create_action_button.png)

  3. In the **Alias** field, enter `Greenhouse_quickcreate`.

  4. In the **Title** field, enter `Service Ticket Quick Create`.

  5. In the **Descriptive Text** field, enter `Launch Quick Create app with mockdata`.

  6. In the **Sequence** field, enter `1`.

  7. Check the **Visible** checkbox.

  8. Select **Quick Create** in the `Action Mode` dropdown.

  9. In the field **Quick Create Intent**, enter `ServiceTicketQC-quickcreate`.

  10. In the field **Quick Create Binding**, enter the following:

    ```JSON
    {
      "Name":"{properties.device.exception}",
      "ServicePriorityCode":"{properties.device.ServicePriorityCode}",
      "ProcessingTypeCode":"{properties.device.ProcessingTypeCode}",
      "CustomerID":"{properties.device.CustomerID}",
      "InstallationPointID":"{properties.device.InstallationPointID}",
      "SerialID":"{properties.device.SerialID}",
      "DataOriginTypeCode":"{properties.device.DataOriginTypeCode}"
    }
    ```

  11. Click **Save** to save the changes.

    ![DS QC Def](/images/ds_quickcreate_def_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Define a fiori navigation option)]

  1. In the Details page, click the **Create** button under the section "Possible Actions".

    ![DS Create Action_Button](/images/ds_create_action_button.png)

  2. In the **Alias** field, enter `Greenhouse_fiori_nav`.

  3. In the **Title** field, enter `Fiori Navigation`.

  4. In the **Descriptive Text** field, enter `Navigate to Home screen`.

  5. In the **Sequence** field, enter `2`.

  6. Check the **Visible** checkbox.

  7. Select `Fiori Navigation` in the **Action Mode** dropdown.

  8. In the **Fiori Navigation Intent** field, enter `Shell-home`.

  9. Click **Save** to save the changes.

    ![DS FioriNav_Def](/images/ds_fiori_nav_def_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Define an action service option (Optional))]
In addition to defining Quick Create and Fiori Navigation options, you can also define an Action Service option.  We will not define an Action Service option in this tutorial but you can define one if you have an Action Service that you'd like to use as an additional option.

![DS ActionService_Def](/images/ds_action_service_def.png)

[DONE]
[ACCORDION-END]


---
