---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>sapui5, software-product>sap-business-application-studio]
primary_tag: software-product>sap-work-zone
author name: Boris Dafov
---

# Create a UI5 Integration Card that Displays Data from the Northwind Demo System
<!-- description --> Create a UI5 integration card in SAP Build Work Zone to display data from the Northwind backend.

## Prerequisites
- Please note that if you are following this tutorial as part of a workshop, you can skip these prerequisites.
- You have created a dev space. See [Create a Dev Space for SAP Fiori Apps](appstudio-devspace-fiori-create).
- To deploy a UI5 Integration card in the SAP Build Work Zone, you should have a subaccount in SAP BTP that includes a subscription to the SAP Build Work Zone service. Additionally, you have to configure a destination for SAP Build Work Zone instance. See [Development Tools for SAP Build Work Zone](https://help.sap.com/docs/build-work-zone-advanced-edition/sap-build-work-zone-advanced-edition/development-tools-for-sap-build-work-zone-advanced-edition).


>**IMPORTANT:** SAP Build Work Zone is not available in a trial account. If you only have a trial account and you want to learn more about the Integration cards you can follow this tutorial from steps 1 to 5.


## You will learn
- How to create a card for SAP Build Work Zone using SAP Business Application Studio (BAS)
- What the main elements of the Integration card are and understand their roles

## Intro
Integration cards are UI elements which display concise pieces of information in a limited-space container. Cards are small previews of application content, and each card represents a specific topic, task, or perspective. As a card developer, you only need to configure a descriptor (`manifest.json` file) and as a result you get fully functional and reusable card.


### Check for Development Tools for SAP Build Work Zone extension


>If you are following this tutorial as part of a workshop, please skip this step.

1. In SAP Business Application Studio, stop the dev space if it is not already stopped.

2. Edit the dev space by selecting the Edit button.

    <!-- border -->![Image depicting SAP Business Application Studio with configured dev spaces – click on Edit button](1-2.PNG)

3. Ensure that the **Development Tools for SAP Build Work Zone** extension is checked and save the changes.

    <!-- border -->![Image depicting SAP BAS with Development Tools for SAP Build Work Zone extension selected](1-3.PNG)



### Create application and run it


1. Open the dev space (If the dev space is not running, you first have to start it).

    <!-- border -->![Image depicting configured dev spaces – open dev space](2-1.PNG)

2. Select **Start from template**.

    <!-- border -->![Image - start from template](2-2.PNG)

3. Choose the **UI Integration Card template** and select **Start**.

    <!-- border -->![Image depicting UI Integration Card template option](2-3.PNG)

4. Fill-in the required project details. Use the **Highlight Card** template, which creates an Integration card of type List and select Finish.
>If you are following this tutorial as part of a workshop, please give your card a unique name. In this case your card name should be `wz<your unique identifier>_orders_by_shipper`.

    | Description                                        | Value   
    | :-------------                                     | :-------------
    | Project Name                                       | `orders_by_shipper` If you're taking part in a workshop, please add your unique identifier to the project name like this: `<your unique identifier>_orders_by_shipper`.
    | Name Space                                         | `ns`
    | Select a Card Sample (dropdown menu)               | `Highlight Card`
    | Title                                              | `Orders by Shipper`
    | Subtitle                                           | `UI5 Integration Card of Type List`
    | Compatible with SAP Mobile Cards (dropdown menu)   | `False`

    <!-- border -->![Image depicting required Project Details](2-4 NEW.PNG)

5. To see the card, right-click on `manifest.json` and select **UI Integration Card: Preview**.

    <!-- border -->![Image depicting UI Integration Card: Preview option](2-5 NEW.PNG)

6. Currently the card displays only static data:

    <!-- border -->![Image depicting the application showing only static data](2-6 NEW.PNG)

7. Open the `manifest.json` file. Everything needed to render the card is described in this file.

    The `manifest.json` is a simple JSON file and has the following structure (check the picture below to see where each part is located):

    - `sap.app` namespace declaration. The `type: card` defines that this is a manifest for a card.  Each card has a unique ID.

    - `sap.card` section:
    <ul><li>Card type (List): Defines how the card is displayed. It could be one of the available content types - Adaptive, Component, Analytical, List, etc.
      </li><li>Header: Displays general information about the card. Using its properties, you can configure the title, subtitle, status text, and icon.
      </li><li>Content: This is the main section of the displayed card.</li></ul>

    - `data` sections: Define how the card handles its data. It can provide static data (see the `json` object below) or define required parameters for a data request to a backend system. Can be set on different levels (card, header, filter-definition, or content). The inner level data sections take precedence. In the example below the data section is defined on content level.

    <!-- border -->![Image depicting manifest.json file structure](2-7 NEW.PNG)

In the next steps you edit the `manifest.json` file to configure the card.


### Add destination to connect to Gateway
 By connecting your card to the public Northwind demo service, you're enabling the card to display dynamic data. Card destinations are used for outbound communication to a remote resource and contain the required connection information.

1. To set a destination, add the following `configuration` section in the `sap.card` section after the `type` subsection.

    ```JSON
    "configuration": {
        "destinations": {
            "Northwind": {
                "name": "Northwind",
                "label": "Northwind V4 Service URL",
                "defaultUrl": "https://services.odata.org/V4/Northwind/Northwind.svc"
            }
        }
    },
    ```

      <!-- border -->![Image depicting manifest.json file – add configuration section](3-1 NEW.PNG)

2. To configure a data request pointing to the Northwind demo service, add a new `data` section after the `configuration`. In this way the `data` section will be defined on a card level. Note, that our destination is referred here using the double-bracket syntax `{{destinations.Northwind}}`.

    ```JSON
    "sap.card": {
        "data": {
    	    "request": {
                "url": "{{destinations.Northwind}}/Orders"
            },
            "path": "/value"
        }
    },
    ```

    <!-- border -->![Image depicting manifest.json file – add data section](3-2 NEW.PNG)

>**IMPORTANT:** Due to an issue with the **UI Integration Card: Preview** option, you may need to replace {{destinations.Northwind}} with "https://services.odata.org/V4/Northwind/Northwind.svc" !

Finally, to display the dynamically requested data, replace the static `content` section with the following one. The `title`, `description`, and `info` properties are now dynamically requested.

    ```JSON
    "content": {
        "item": {
            "title": "{ShipName}",
            "description": "{ShipAddress}",
            "info": {
                "value": "{ShipCountry}"
            }
        }
    }
    ```

      <!-- border -->![Image depicting manifest.json file – replace content section](3-3 NEW.PNG)

**Results after Step 3:**

The application displays dynamic data loaded from the Northwind demo service. Note, that the actual displayed products may differ depending on the current data provided by the Northwind demo service. You can also check the [manifest.json](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/appstudio-sapui5-integrationcard-create/manifest_after_step3.json) file at this step. To learn more, see the [Destinations](https://ui5.sap.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/configuration/destinations) and [Data](https://ui5.sap.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/features/data) sections in the Card Explorer.

<!-- border -->![Image depicting the application showing dynamic data](3-4 NEW.PNG)

If you would like to deploy the card and see how it looks on SAP Build Work Zone, you can skip to Step 6 and deploy it. In the next steps you add card capabilities that can make your card more interactive.


### Add manifest parameters
 Manifest parameters provide dynamic values for card attributes. They are replaced during manifest processing and can be used from the `parameters` model, for example: `{parameters>/city/value}`. As an example, in this step you will add parameters to set the header (`title`) property and the number (`maxItems`) of displayed items in the content.



>If you are following this tutorial as part of a workshop and run out of time, you can skip steps 4,5,6 and create a simpler card. You can later read the steps you missed.

1. To define parameters - add the following `parameters` subsection in the `manifest.json` in the `configuration` section (note the comma which divides the entries).

    ```JSON
    "parameters": {
        "title" : {
            "value": "Orders by Shipper"
        },
        "maxOrdersShown": {
            "value": "4",
            "type": "integer",
            "label": "Numbers of orders",
            "description": "How many orders to show in the list."
        }
    }
    ```

      <!-- border -->![Image depicting manifest.json file - add parameters](4-1 NEW.PNG)

2. To use the new `maxOrdersShown` parameter, add it as shown below:

    ```JSON
    "maxItems": "{parameters>/maxOrdersShown/value}"
    ```

      <!-- border -->![Image depicting manifest.json file – use maxOrdersShown parameter](4-2 NEW.PNG)

3. Update the data request as follows:

    ```JSON
    "data": {
        "request":{
            "url": "{{destinations.Northwind_V4}}/Orders",
            "parameters": {
                "$top": "{parameters>/maxOrdersShown/value}"	
            },
        "path": "/value/"
        }
    }
    ```
      <!-- border -->![Image depicting manifest.json file – use $top](4-3 DATA REQ.PNG)

>**IMPORTANT:** Due to an issue with the **UI Integration Card: Preview** option, you may need to replace {{destinations.Northwind}} with "https://services.odata.org/V4/Northwind/Northwind.svc" !

Finally, let's also use the new parameters in the `header` section. Use the `parameters` syntax and edit (or replace) the header, so it looks like this:

    ```JSON
    "header": {
        "title": "{parameters>/title/value}",
        "icon": {
            "src": "sap-icon://desktop-mobile"
        },
        "status": {
            "text": "{parameters>/maxOrdersShown/value}"
        }
    },
    ```

      <!-- border -->![Image depicting manifest.json file - edit header](4-3 NEW.PNG)

**Results after Step 4:**

In this step, you have learned how to declare configurable parameters and use them to achieve the desired dynamic behavior. The application now displays a list of 4 items according to the `parameters` property (`maxOrdersShown value: 4`).

<!-- border -->![Image depicting the application showing dynamic data using parameters](4-4 NEW.PNG)

To learn more, see the [Manifest Parameters](https://ui5.sap.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/configuration/manifestParameters) section in the Card Explorer.


### Add user interaction with filtering
 You can make the card even more dynamic when using filters. Filters appear as a dropdown under the card header, and users can interact to customize the data shown by the card. The value of each filter can be used inside a data request definition by using the `{filters>/myFilter/value}` placeholder. When the end user selects different value from the dropdown - a new data request is made with the updated value. As an example, in this step you will add a filter that enables users to filter the orders by a selected shipper.

1. Add a `filters` subsection in the `configuration` section. It defines a dropdown list with product categories, which are received by a data request.

    ```JSON
    "filters": {
        "shipper": {
            "value": "{parameters>/selectedShipperID/value}",
            "type": "Select",
            "label": "Shipper",
            "item": {
                "path": "/value",
                "template": {
                    "key": "{ShipperID}",
                    "title": "{CompanyName}"
                }
            },
            "data": {
                "request": {
                    "url": "{{destinations.Northwind}}/Shippers"
                }
            }
        }
    },
    ```

    <!-- border -->![Image depicting manifest.json file - add filters section](5-1 NEW.PNG)

2. Add `selectedShipperID` subsection in the `parameters` section. This is the shipper that is initially selected in the filter. Later, the user can change it from the dropdown list.

    ```JSON
    "selectedShipperID": {
        "value": 3,
        "label": "The default selected shipper"
    }   
    ```

    <!-- border -->![Image depicting manifest.json file – set the initially selected category](5-2 NEW.PNG)

3. Add `parameters` in the main `data` section > `request` subsection, after the `url` property as shown below. The `$filter` parameter will be used in a data request for the orders with `shipper` that is equal to the one selected by the user in the filter's dropdown list.

    ```JSON
    "request": {
		"url": "https://services.odata.org/V4/Northwind/Northwind.svc/Orders",
        "parameters": {
            "$top": "{parameters>/maxOrdersShown/value}",
            "$filter": "Shipper/ShipperID eq {filters>/shipper/value}"
		}
    }
    ```

    <!-- border -->![Image depicting manifest.json file - add filter parameter in the main data section](5-3 NEW.PNG)

4. Finally, replace the title in the `header` adding the `{filters>/shipper/selectedItem/title}` parameter, which will show the selected category:

    ```JSON
    "title": "Orders by Shipper {filters>/shipper/selectedItem/title}",
    ```

    <!-- border -->![Image depicting manifest.json file – use parameters in the header's title ](5-4 NEW.PNG)

**Results after Step 5:**

If you have any issues you can check the [manifest.json](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/appstudio-sapui5-integrationcard-create/manifest.json) file at this step. It is configured with destinations, parameters, and a filter. 

The application displays the products from the selected category:

<!-- border -->![Image depicting the application showing dynamic data, parameters, and a filter](5-5 NEW.PNG)

To learn more, see the [Card Filters](https://ui5.sap.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/filters) section in the Card Explorer.


### Configure card parameters that are displayed in SAP Build Work Zone


1. Select the `dt/configuration.js` file (in the Explorer view on the left).

    <!-- border -->![Image depicting the configuration.js file in the file menu](6-1 NEW.PNG)

2. Replace the content with the code below:

```JAVASCRIPT
sap.ui.define(["sap/ui/integration/Designtime"], function (
    Designtime
) {
    "use strict";
    return function () {
        return new Designtime({
        "form": {
          "items": {
            "maxItems": {
              "manifestpath": "/sap.card/configuration/parameters/maxOrdersShown/value",
              "type": "integer",
              "label": "Maximum Items",
              "translatable": false,
              "description": "Defines how many items will be displayed at most."
            }
          }
        },
        "preview": {
          "modes": "LiveAbstract"
        }
      });
    };
});
```

The `dt/configuration.js` now looks like:

<!-- border -->![Image depicting the configuration.js file content](6-2 NEW.PNG)



### Deploy card

>If you are using SAP Build Work Zone, advanced edition, please continue following this tutorial to deploy your card.

>If you are using SAP Build Work Zone, standard edition, you will first have to create a Content Package before you can deploy the card. Please skip this step and follow the tutorial that explains how to create the content package: [Create a Content Package with Your UI Integration Card](cp-portal-cloud-foundry-create-content-package)

1. Right-click on the `manifest.json` file (in the Explorer view on the left) and select the **UI Integration Card:Deploy to SAP Build Work Zone**  option from the dropdown menu.

    <!-- border -->![Image depicting UI Integration Card:Deploy to SAP Build Work Zone option ](7-1.PNG)

2. Select the target SAP Build Work Zone destination.

    <!-- border -->![Image depicting Select the target SAP Build Work Zone destination option](7-2.PNG)

3. In the right-bottom corner, confirm to **Continue** and wait to see the successful message.

    <!-- border -->![Image depicting the Continue button to proceed with card deployment](7-3.PNG)

Now the basic UI5 card deployment is done!




To learn more about the Integration cards and their functionalities, see the [Card Explorer](https://sapui5.hana.ondemand.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html) page.
