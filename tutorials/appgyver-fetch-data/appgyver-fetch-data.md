---
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>mobile, software-product>sap-business-technology-platform, software-product>sap-build]
primary_tag: software-product>sap-build-apps
author_name: Daniel Wroblewski
author_profile: https://github.com/thecodester
parser: v2
---

# Fetch Data from Public API to Your App
<!-- description --> Configure your application to fetch records from a public API when a food item is scanned.

## You will learn
  - How to fetch records from a public API
  - How to configure data variables

## Intro
In the previous tutorial, you learned how to connect your application to a public API. From here, you now need to configure the application to read specific information from that API once a barcode has been scanned. 

This again uses the **Retrieve** request, as when you were testing the data resource, but this time the request is triggered with a logic flow.



### Remove alert component
As you no longer need your application to send an alert, as this was just used as a test, you need to remove this flow function.

1. Open your app, and go to **UI Canvas** tab.

2. Click the **Scan** button, then click **Add Logic for Button1**.

    ![Show logic for button 1](show_logic.png)

    >If the logic canvas is already open, you can just click the button.

3. Select the **Alert** flow function, and click the **X**.

    ![Remove alert component](alert_component.png)





### Add Get Record component

You now need to add your new logic flow for what should happen after the barcode has been scanned. For your application, you want the barcode scanner to fetch data from the data resource you configured in the previous tutorial.

1. In the **Core** logic tab, scroll down and drag a **Get Record** flow function. 

    ![Add Get Record component](get_record.png)

2. Connect the top output of the **Scan QR/Barcode** flow funcion to the input of the **Get Record flow function.

    ![Add connector](add_connector.png)






### Edit binding
After adding a **Get Record**, you need to indicate what backend you want to make a request to and what data you want to send (if any).


1. In the logic canvas, select **Get Record** element.
   
    The **Resource Name** is already set to the **OpenFoodFacts** data resource since this is the only data resource in the app.

    ![Currently bound options](currently_bound.png)

2. Next to the barcode input, click the **X**.

    ![Binding type](bind1.png)

    Since we want to send the barcode the user scanned (which the **Scan QR/Barcode** flow function will output), we select **Output value of another node**.

    ![Output value edit](output_value_node.png)

    Select **Scan QR/Barcode** as the logic node to get the data from.
    
    Select **QR barcode content** as the specific output.

    ![Output value](output_value_node2.png)

    Click **Save**.

The Get Record flow function should now be able to fetch data for any food barcode you scan with your application.








### Add data variables
You now need to configure your application to store the data it receives. To do this, you need to add data variables.

1. Switch to the **Variables** view.

    ![Switch to variables view](variables_view.png)

2. Click **Data Variables** (on the left).

    ![Data variables](data_variables.png)

    After reading, click the **Welcome to variables** message to hide it (if the message appears).

    ![Welcome message](data_variables2.png)

3. Click **Add Data Variable**.

    ![Add data variables](add_data_variable.png)

    Select your **Open Food Facts** variable.

    ![Select open food facts](open_food_facts.png)

    As the barcode will be for a single product, select **Data variable type – Single data record**.

    ![Select single data record](single_data_record.png)

4. Data variables come with default logic that fetches new information when the page loads. But you want to fetch data only when the user clicks the button and scans a barcode, so you will remove the default logic and later add logic on the button click.

    Select the **Get record** and **Set data variable** flow functions by drawing a rectangle around them.

    ![Delete default logic](delete_default_logic.png)

    Press the **Delete** key.
    
5. Click **Save** (upper right).








### Store API data in data variable
Finally, we want to store the data we retrieve when the user scans a barcode into the data variable.

1. Click **View** to switch back to the user interface view. 

    ![Change view](change_view.png)

2. Click your **Scan** button to open the relevant logic panel.

    ![Scan button logic](scan_button_logic.png)

    >This is where we added the logic to scan a barcode and retrieve the data for that product.

3. Drag a **Set Data Variable** flow function onto the logic canvas.

    ![Set Data Variable](set_data_variable.png)

    Connect the top **Get record** output to the **Set data variable** input.

    ![Add connector](add_connector_options.png)

5. Select the **Set data variable** flow function.

    Since you only have one data variable, it is already selected.
    
    ![Currently bound](currently_bound1.png)

6. Now you will select the data to store in the variable.

    >**IMPORTANT:** The following provides 2 ways to do this. The first way is the standard way, but for some people this may cause the SAP Build Apps editor to hang (if the app hangs, just click to exit). So we have provided a second way to store the data using a formula.

    -  Under **data**, select the binding type square.
        
        ![Select binding type](binddata1.png)

        Select **Output value of another node**. 
        
        Select **Get Record**.
        
        Select **Record**.

        >It may take 30 seconds for the browser to catch up. Just wait.

        ![Bind output](select_get_record.png)

    - Instead, you can do the same thing with a formula. Most, if not all, bindings can be done with the UI or manually with a formula.

        Under **data**, select the binding type square.
        
        Select **Formula** (instead of **Output value of another node**), click the existing formula and replace it with the following:

        ```JavaScript
        outputs["Get record"].record
        ```

        Click **Save** twice.

    Click **Save** to save this logic (no matter how you entered it).

    ![Save the logic](save_data_variable.png)

7. Click **Save** (upper right) to save your draft application.

    ![Link text e.g., Destination screen](save_draft.png)

The logic has now been added to your draft application.
