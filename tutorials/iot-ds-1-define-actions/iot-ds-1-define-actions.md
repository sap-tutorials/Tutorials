---
title: Model the Needed Decision Support Based on IoT Data
description: Create actions, rules and configurations using SAP IoT.
auto_validation: true
time: 30
tags: [ tutorial>beginner, products>sap-leonardo-iot, topic>internet-of-things ]
primary_tag: topic>internet-of-things
---

## Details
### You will learn
  - How to create a new rule context
  - How to create a new streaming rule
  - How to create an action of type decision support
  - How to create an action of type in-app notification
  - How to create a decision support configuration

---
In this tutorial, you'll create a streaming rule for high greenhouse temperature which triggers an action of type decision support.  This action will in turn trigger another action of type in-app notification which creates SAP Fiori notifications for a set of recipients.  You'll also define a decision support configuration with two options.  One is a quick create application and the other one is a SAP Fiori navigation.

---

[ACCORDION-BEGIN [Step 1: ](Create rule context)]
  Please complete the prerequisites before starting this step. If you already have created a rule context for `greenhouse`, you can skip to the next step **Create streaming rule**.

  1. From SAP IoT launchpad, select the **Rule Contexts** tile.

    ![Select rule contexts on SAP Fiori launchpad](/images/launchpad_tile_rulecontexts.png)

  2. Create a new rule context.

    ![Add new rule context](/images/rulectxt_new_1.png)

  3. Enter `greenhouserulecontext` in field **Name**.

  4. Enter `Greenhouse Rule Context` in the field **Short Text** and **Description**.

    ![Enter basic data](/images/rulectxt_new_4_1.png)

  5. Add a new **Property Set**.

    ![Add new property set](/images/rulectxt_new_5.png)

  6. In the dialog, select `greenhouse` for **Package** and `greenhouseType` for **Thing Type**.  The entry `envData` should be displayed automatically in the **Property Sets** section.  Select `envData` and click **OK**.

    ![Select property set](/images/rulectxt_new_2_1.png)

  7. The new property set `envData` has been added.

    ![Completed basic data and data object](/images/rulectxt_new_3_1.png)

  8. Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create streaming rule)]
This tutorial is based on the greenhouse device used in the SAP IoT Foundation tutorials.  You can find these tutorials under prerequisites.  Please complete them before continuing with this tutorial.

1. From SAP IoT launchpad, select the **Rules** tile.

    ![Select rule on SAP Fiori launchpad](/images/launchpad_tile_rules.png)

2. Create a new **Streaming Cloud** rule.

    ![Create new rule of type streaming cloud](/images/rule_new_1_1.png)

3. In **General Information**, enter `High Greenhouse Temperature` in the **Name** field.

4. In the field **Description**, enter `Temperature in Greenhouse is above 25 degrees`.

    ![Enter general information for new rule](/images/rule_new_2.png)

5. In **Definition**, click the field **Rule Context**.

    ![Select rule context](/images/rule_new_3_1.png)

6. Select the rule context for your greenhouse. The name of your rule context might be different.

    ![Select greenhouserulecontext](/images/rule_new_4.png)

7. In the next dialog, select `envData`.

    ![Select envdata](/images/rule_new_8.png)

8. Confirm the selection.

    ![Confirm rule context selection](/images/rule_new_5_1.png)

9. Enter the following rule in the **If** field of **Ruled Editor**:

    `temperature of envData is greater than 25`

    ![Enter if condition for new rule](/images/rule_new_6.png)

    ![Completed if condition](/images/rule_new_7_1.png)

10. **Activate** the rule.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create decision support action)]

  1. From SAP IoT launchpad, select the **Actions** tile.

    ![Actions tile on SAP Fiori launchpad](/images/launchpad_tile_actions.png)

  2. On the **Actions** page, click **New** to create a new action.

    ![Create new action](/images/actions_new.png)

  3. Set **Name** to `Greenhouse Action`.

  4. In the **Triggered by** field, select `Event from Rule`.

  5. In the **Rule** dropdown, select `High Greenhouse Temperature`. The system populates **Thing Type** automatically.

  6. In the **Action Type** dropdown, select `Decision Support`.

  7. Enter `Greenhouse_CF` for **Alias** field.  You can enter a different value of your preference. This value will be used in a later tutorial step.

  8. If you want to display device event data and `Thing` master data in a decision support application, you can do so by providing a JSON payload. To include master data properties from the `Thing` model, please use the following notation:

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

  9. Click **Save**.

    ![New action screen with entered data](/images/actions_new_ds_details_1.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create in-app notification)]

  1. On the **Actions** page, click **New** to create a new action.

    ![Create new action](/images/actions_new.png)

  2. Enter `Greenhouse In-App Notification Action` in the **Name** field.  

  3. In **Triggered By** dropdown, select `Event from Action`.

  4. In **Action** dropdown, select `Action - Greenhouse Action`. The system populates **Thing Type** automatically.  

  5. In the **Action Type** dropdown, select `In-App Notification`.  

  6. In **Recipients** field, enter your email address.  

  7. Enter `GreenhouseDSApp` as **Target Object** and `display` as **Target Action**.  These values are defined in step 2 of [Building the Decision Support UI in the Web IDE](iot-ds-3-create-ui)

  8. In **Text** field, enter `High Greenhouse Temperature Alert`.

  9. Click **New** in the section **Target Parameters**.

    ![Create new target parameter](/images/actions_new_inappnotif_details_1.png)

  10. Enter `Guid` for **Key** field.  

  11. Enter `${RecommendationServiceGuid}` for **Value** field.

  12.  Click **Save**.

    ![Target parameter values](/images/actions_new_inappnotif_details_2.png)

  13. Click **Save**.  

    ![New action screen with entered data](/images/actions_new_inappnotif_details.png)

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create decision support configuration)]

  1. From SAP IoT launchpad, select the **Decision Support Definition** tile.

    ![Select Decision Support tile in SAP Fiori launchpad](/images/launchpad_tile_ds.png)

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
In addition to defining quick create and SAP Fiori navigation options, you can also define an action service option.  We will not define an action service option in this tutorial but you can define one if you have an action service that you'd like to use as an additional option.

  ![Select action service as action mode](/images/ds_action_service_def.png)

>**Note**: Only action service of type "Decision Support" can be used as a decision support configured option. If you do not see your action service displayed in the **Action Service ID** dropdown, please check that your action service is defined with the type "Decision Support".

  ![Select action service as action mode](/images/ds_action_service_type_example.png)

  ![Select action service as action mode](/images/ds_action_service_type.png)


[DONE]
[ACCORDION-END]


---
