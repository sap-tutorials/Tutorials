---
title: Fetch Data from Public API to Your AppGyver Application
description: Configure your application to fetch records from a public API when a food item is scanned, using a Get Record command, which first needs to be configured.
auto_validation: true
time: 15
tags: [ tutorial>beginner, tutorial>license, topic>mobile, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-appgyver
author_name: Akseli Virtanen
author_profile: https://github.com/akseliv
---

## Prerequisites
- Access to an SAP BTP account in EU10 with Low-Code / No-Code entitlements
- Previously followed the steps provided in [Connect your AppGyver Application to a Public API](appgyver-connect-publicapi)
 - Access to the AppGyver Previewer App on a smart phone or tablet: [iOS](https://apps.apple.com/us/app/sap-appgyver-preview/id1585856868) / [macOS](https://downloads.appgyver.com/SAP_AppGyver_preview_v3.4.4.zip)/ [Android](https://play.google.com/store/apps/details?id=com.sap.appgyver.preview.release)

## Details
### You will learn
  - How to configure your application to fetch records from a public API
  - How to configure data variables.

In the previous tutorial, you learned how to connect your application to a public API. From here, you now need to configure the application to read specific information from that API once a barcode has been scanned. This again uses the Get Record HTTPS request, but this time that request is triggered with a logic flow.

---

[ACCORDION-BEGIN [Step 1: ](Remove alert component)]

Open your draft application in your AppGyver Composer account, displaying your barcode scanner app.

As you no longer need your application to send an alert, as this was just used as a test, you need to start by removing the alert component in your logic flow.

To do this, click your **Scan** button and then click **Show Logic for Button1**.

![Show logic for button 1](show_logic.png)

Remove the ***Alert*** component from your logic panel, as this is no longer needed.

![Remove alert component](alert_component.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add Get Record component)]

You now need to add your new logic flow for what should happen after the barcode has been scanned. For your application, you want the barcode scanner to fetch data from the data resource you configured in the previous tutorial.

To do this, using the core logic options, scroll down to **Data – Get Record** and then drag and drop this into the logic editor.

![Add Get Record component](get_record.png)

Add a connector from the top Scan QR/barcode option to the Get Record option, indicating the flow of logic in your application.

![Add connector](add_connector.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Edit binding)]

Once the logic flow is set, you need to bind the information to the output of the scanner node.

To do this, Select the **Get Record** element and, using the properties panel, click **Currently bound to: Static text**, opening the binding options screen.

![Currently bound options](currently_bound.png)

Click **Output value of another node**.

![Output value edit](output_value_node.png)

Configure the binding to the following:

- ***Select logic node*** – Scan QR/barcode
- ***Select node output*** – Scan QR/barcode / QR barcode content

Then click **Save**.

The Get Record flow function should now be able to fetch data for any food barcode you scan with your application.

![Edit binding](edit_binding.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add data variables)]

You now need to configure your application to store the data it receives. To do this, you need to add data variables.

To do this, switch to the **Variables** view.

![Switch to variables view](variables_view.png)

Click **Data Variables**.

![Data variables](data_variables.png)

Click **Add Data Variable**.

![Add data variables](add_data_variable.png)

Select your ***Open Food Facts*** variable.

![Select open food facts](open_food_facts.png)

As the barcode will be for a single product, select **Data variable type – Single data record**.

![Select single data record](single_data_record.png)

Data variables come with default logic that fetches new information every five seconds, however your app should only fetch information when a barcode is scanned. As a result, you need to remove the default logic. To do this, click **Show Logic for Empty Page**.

![Click show logic button](show_logic_empty.png)

Then delete the default logic by highlighting it and pressing the **delete button** on the keyboard.

![Delete default logic](delete_default_logic.png)

Click **Save**.

![Save logic](save_logic.png)

The data variables are now configured for your application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Set data variable for scan button)]

Now click **View** to switch back to your application interface view. From here, you will need to add the final piece to your logic flow.

![Change view](change_view.png)

To do this, click your **Scan** button to open the relevant logic panel.

![Scan button logic](scan_button_logic.png)

Using the core logic options, scroll down to **Variables – Set Data Variables** and then drag and drop this into the logic editor.

![Set Data Variable](set_data_variable.png)

Add a connector from the top **Get record** option to the **Set data variable** option.

![Add connector](add_connector_options.png)

Now, click the **Set data variable** element and click **Currently bound to: object with properties**.

![Currently bound](currently_bound_option.png)

Now you must store the data you just retrieved to the data variable.

>**IMPORTANT:** The following provides 2 ways to do this. The first way is the standard way, but for some people this may cause the AppGyver editor to hang (you can click to exit). So we have provided a second way to store the data using a formula.

-  Select **Output value of another node** and then choose the following:

    - ***Select logic node***: Get record
    - ***Select node output***: Record

    ![Link text e.g., Destination screen](select_get_record.png)

- Instead, you can do the same thing with a formula. Most, if not all, bindings can be done with the UI or manually with a formula.

    Select **Formula**, and then enter for the formula the following:

    ```
    outputs["Get record"].record
    ```

Then click **Save** to save this logic (no matter how you entered it).

![Save the logic](save_data_variable.png)

Click **Save** to save your draft application.

![Link text e.g., Destination screen](save_draft.png)

The logic has now been added to your draft application.

[VALIDATE_1]
[ACCORDION-END]
