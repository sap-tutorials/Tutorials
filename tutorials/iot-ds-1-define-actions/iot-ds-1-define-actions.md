---
title: Model the Needed Decision Support Based on IoT Data
description: Create actions, rules and configurations using SAP IoT.
author_name: Jitendra Sharma
author_profile: https://github.com/JitendraSharma01
auto_validation: true
time: 30
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-internet-of-things, products>sap-business-technology-platform ]
---

## Details
### You will learn
  - How to create a new rule project and streaming rule
  - How to create an action of type decision support
  - How to create an action of type in-app notification
  - How to create a decision support configuration

---
In this tutorial, you'll create a streaming rule for high greenhouse temperature which triggers an action of type decision support.  This action will in turn trigger another action of type in-app notification which creates SAP Fiori notifications for a set of recipients.  You'll also define a decision support configuration with two options.  One is a quick create application and the other one is a SAP Fiori navigation.

---

[ACCORDION-BEGIN [Step 1: ](Create rule project)]
Please complete the prerequisites before starting this step.

  1. From SAP IoT launchpad, select the **Rule Projects** tile.

    ![Rule Projects tile on SAP Fiori launchpad](/images/launchpad_tile_ruleproject.png)

  2. On the **Rule Projects** page, click **Create** to create a new rule project.

    ![Rule Projects create](/images/ruleproject_create_1.png)

  3. Set **Name** to `Greenhouse Rule Project` and click **Create**.

    ![Rule Projects create dialog](/images/ruleproject_create_2.png)

  4. Under **Data Objects** section, click **+** for **Property Set-based**.

    ![Rule Projects create data object ](/images/ruleproject_create_dataobj_1.png)

  5. Set **Package** to `greenhouse`.

  6. Set **Thing Type** to `greenhouseType`.

  7. Select `envData`.

  8. Click **OK**.

    ![Rule Projects create data object dialog](/images/ruleproject_create_dataobj_2.png)

  9. In the next dialog, only select **Property Set** `Aggregates`. Click **OK**.

    ![Rule Projects create data object dialog 2](/images/ruleproject_create_dataobj_3.png)

  10. On the **Rule Projects** page, click **Save and Continue**.

    ![Rule Projects create data object finish](/images/ruleproject_create_3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create streaming rule)]

  1. Click on **Rules** section.  Select **Create Rule**, **Streaming** and **Event Creation**.

    ![Rule Projects create rule 1](/images/ruleproject_create_rule_1.png)

  2. Set **Name** to `High Greenhouse Temperature`.

  3. Set **Description** to `Temperature in Greenhouse is above 25 degrees`.

  4. Click icon on field **Input**.

    ![Rule Projects create rule 2](/images/ruleproject_create_rule_2.png)

  5. Select `envData` entry under **Property Set-based**.

    ![Rule Projects create rule 3](/images/ruleproject_create_rule_3.png)

  6. Click **OK** in the next dialog.

    ![Rule Projects create rule 4](/images/ruleproject_create_rule_4.png)

  7. Click **Confirm**.

    ![Rule Projects create rule 5](/images/ruleproject_create_rule_5.png)

  8. Enter the following rule in the **If** field of **Ruled Editor**:

    `envData.temperature > 25`

    ![Rule Projects create rule 6](/images/ruleproject_create_rule_6.png)

  9. Click **Activate**.

    ![Rule Projects create rule 7](/images/ruleproject_create_rule_7.png)

  10. Click **Save**.

    ![Rule Projects create rule 7](/images/ruleproject_create_rule_8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create decision support action)]

  1. From SAP IoT launchpad, select the **Actions** tile.

    ![Actions tile on SAP Fiori launchpad](/images/launchpad_tile_actions_1.png)

  2. On the **Actions** page, click **Create** to create a new action.

    ![Create new action](/images/actions_new_1.png)

  3. Set **Name** to `Greenhouse Action`.

  4. Set **Triggered by** field to `Event from Rule`.

  5. Click on the icon on **Rule** field.  

    ![Create new action 1](/images/actions_new_1a.png)

  6. Set **Rule Project** to `Greenhouse Rule Project`, **Data Object** to `envData` and select  `High Greenhouse Temperature`.

    ![Create new action 2](/images/actions_new_2.png)

  7. Click **OK**.

  8. In the **Action Type** dropdown, select `Decision Support`.

  9. Enter `Greenhouse_CF` for **Alias** field.

  10. If you want to display device event data and `Thing` master data in a decision support application, you can do so by providing a JSON payload. To include master data properties from the `Thing` model, please use the following notation:

    `${propertyname}`

    For example:

    ![Reference thing properties in payload](/images/actions_payload_thing_data.png)

    For this scenario, you'll enter a sample JSON payload with a set of static values in the **Payload** text area.

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

  11. Click **Save**.

    ![New action screen with entered data](/images/actions_new_ds_details_3.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create in-app notification)]

  1. On the **Actions** page, click **Create** to create a new action.

    ![Create new action](/images/actions_new_1.png)

  2. Enter `Greenhouse In-App Notification Action` in the **Name** field.  

  3. In **Triggered By** dropdown, select `Event from Action`.

  4. In **Action** dropdown, select `Greenhouse Action`. The system populates **Thing Type** automatically.  

  5. In the **Action Type** dropdown, select `In-App Notification`.  

  6. In **Recipients** field, enter your email address.  

  7. Enter `GreenhouseDSApp` as **Target Object** and `display` as **Target Action**.  These values are defined in step 3 of [Building the Decision Support UI in the Web IDE](iot-ds-3-create-ui)

  8. In **Text** field, enter `High Greenhouse Temperature Alert`.

  9. Click **New** in the section **Target Parameters**.

    ![Create new target parameter](/images/actions_new_inappnotif_details_1.png)

  10. Enter `Guid` for **Key** field.  

  11. Enter `${RecommendationServiceGuid}` for **Value** field.

  12.  Click **Save**.

    ![Target parameter values](/images/actions_new_inappnotif_details_2.png)

  13. Click **Save**.  

    ![New action screen with entered data](/images/actions_new_inappnotif_details_0.png)

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create decision support configuration)]

  1. From SAP IoT launchpad, select the **Decision Support Definition** tile.

    ![Select Decision Support tile in SAP Fiori launchpad](/images/launchpad_tile_ds_1.png)

  2. On the **Decision Support Definition** page, click **Create** to create a new decision support configuration.

    ![Create new Decision Support configuration](/images/ds_new.png)

  3. On the next page, set **Name** to `High Greenhouse Temperature`.

  4. In the **Alias** field, enter `Greenhouse_CF`.

  5. Check the checkbox **Active**

  6. Click **Save**.

  7. You will navigate back to the previous page.

    ![General information for new decision support configuration](/images/ds_new_config_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Define a quick create option)]

  1. Select the entry `High Greenhouse Temperature` on the list.

  2. On the next page, click the **Create** button under the section **Possible Actions**.

    ![Create a new possible action](/images/ds_create_action_button.png)

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

  11. Click **Save**.

    ![New possible action with data entered](/images/ds_quickcreate_def_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Define a SAP Fiori navigation option)]

  1. In the **Definition** page, click the **Create** button under the section **Possible Actions**.

    ![Create new possible action](/images/ds_create_action_button.png)

  2. In the **Alias** field, enter `Greenhouse_fiori_nav`.

  3. In the **Title** field, enter `SAP Fiori Navigation`.

  4. In the **Descriptive Text** field, enter `Navigate to Home screen`.

  5. In the **Sequence** field, enter `2`.

  6. Check the **Visible** checkbox.

  7. Select `Fiori Navigation` in the **Action Mode** dropdown.

  8. In the **Fiori Navigation Intent** field, enter `Shell-home`.

  9. Click **Save**.

    ![New possible action with data entered](/images/ds_fiori_nav_def_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Define an action service option (optional))]
In addition to defining quick create and SAP Fiori navigation options, you can also define an action service option.  You will not define an action service option in this tutorial but you can define one if you have an action service that you'd like to use as an additional option.

  ![Select action service as action mode](/images/ds_action_service_def.png)

>**Note**: Only action service of type triggered by "Decision Support" can be used as a decision support configured option. If you do not see your action service displayed in the **Action Service ID** dropdown, please check that your action service is defined correctly.

  ![Select action service as action mode](/images/ds_action_service_type_example.png)

  ![Select action service as action mode](/images/ds_action_service_type.png)


[DONE]
[ACCORDION-END]


---
