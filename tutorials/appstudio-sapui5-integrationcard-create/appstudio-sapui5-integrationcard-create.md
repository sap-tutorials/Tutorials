---
title: Create a UI5 Integration Card that Consumes Data from the SAP Gateway Demo System
description: Create a UI5 integration card in SAP Work Zone to display data from the backend SAP Gateway Demo System.
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>sapui5, software-product>sap-business-application-studio]
primary_tag: software-product>sap-work-zone
author name: Boris Dafov
---

## Prerequisites
- You have an account on the SAP Gateway Demo System. See [Create an Account on the SAP Gateway Demo System](gateway-demo-signup).
- You have connected the SAP BTP to your SAP Gateway Demo System Account. See [Connect SAP BTP to Your SAP Gateway Demo System Account (ES5)](cp-portal-cloud-foundry-gateway-connection).
- You have created a dev space. See [Create a Dev Space for SAP Fiori Apps](appstudio-devspace-fiori-create).
- To deploy a UI5 Integration card in the SAP Work Zone, you should have a subaccount in SAP BTP that includes a subscription to the SAP Work Zone service. Additionally, you have to configure a destination for SAP Work Zone instance See [Creating a Destination to the Content Repository](https://help.sap.com/viewer/7d3b9c7211ca4d7a9630b524205ee836/Cloud/en-US/b058b6314023480194b6dae513e4f035.html).
- **IMPORTANT:** SAP Work Zone is not available in a trial account. If you only have a trial account and you want to learn more about the Integration cards you can follow this tutorial from steps 1 to 5.


## Details
### You will learn
- How to create a card for SAP Work Zone using SAP Business Application Studio (BAS)
- What the main elements of the Integration card are and understand their roles

Integration cards are UI elements which display concise pieces of information in a limited-space container. Cards are small previews of application content, and each card represents a specific topic, task, or perspective. As a card developer, you only need to configure a descriptor (`manifest.json` file) and as a result you get fully functional and reusable card.

---

[ACCORDION-BEGIN [Step 1: ](Check for Development Tools for SAP Work Zone extension)]

1. In SAP Business Application Studio, stop the dev space if it is not already stopped.

2. Edit the dev space by selecting the Edit button.

    !![Image depicting SAP Business Application Studio with configured dev spaces – click on Edit button](1-2.PNG)

3. Ensure that the **Development Tools for SAP Work Zone** extension is checked and save the changes.

    !![Image depicting SAP BAS with Development Tools for SAP Work Zone extension selected](1-3.PNG)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create application and run it)]

1. Open the dev space (If the dev space is not running, you first have to start it).

    !![Image depicting configured dev spaces – open dev space](2-1.PNG)

2. Select **Start from template**.

    !![Image - start from template](2-2.PNG)

3. Choose the **UI Integration Card template** and select **Start**.

    !![Image depicting UI Integration Card template option](2-3.PNG)

4. Fill-in the required project details. Use the **Highlight Card** template, which creates an Integration card of type List and select Finish.

    | Description                                        | Value   
    | :-------------                                     | :-------------
    | Project Name                                       | `products_by_vendor_card` If you're taking part in a workshop, please add your unique identifier to the project name like this: `<your unique identifier>_products_by_vendor_card`.
    | Name Space                                         | `ns`
    | Select a Card Sample (dropdown menu)               | `Highlight Card`
    | Title                                              | `Products by Vendor Card`
    | Subtitle                                           | `UI5 Integration Card of Type List`
    | Compatible with SAP Mobile Cards (dropdown menu)   | `False`

    !![Image depicting required Project Details](2-4.PNG)

5. To see the card, right-click on `manifest.json` and select **UI Integration Card: Preview**.

    !![Image depicting UI Integration Card: Preview option](2-5.PNG)

6. Currently the card displays only static data:

    !![Image depicting the application showing only static data](2-6.PNG)

7. Open the `manifest.json` file. Everything needed to render the card is described in this file.

    The `manifest.json` is a simple JSON file and has the following structure (check the picture below to see where each part is located):

    - `sap.app` namespace declaration. The `type: card` defines that this is a manifest for a card.  Each card has a unique id.

    - `sap.card` section:
    <ul><li>Card type (List): Defines how the card is displayed. It could be one of the available content types - Adaptive, Component, Analytical, List, etc.
      </li><li>Header: Displays general information about the card. Using its properties, you can configure the title, subtitle, status text, and icon.
      </li><li>Content: This is the main section of the displayed card.</li></ul>

    - `data` sections: Define how the card handles its data. It can provide static data (see the `json` object below) or define required parameters for a data request to a backend system. Can be set on different levels (card, header, filter-definition, or content). The inner level data sections take precedence. In the example below the data section is defined on content level.

    !![Image depicting manifest.json file structure](2-7.PNG)

In the next steps you edit the `manifest.json` file to configure the card.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add destination to connect to Gateway)] By connecting your card to the SAP Gateway Demo System (ES5), you're enabling the card to consume dynamic data. Card destinations are used for outbound communication to a remote resource and contain the required connection information.

1. To set a destination, add the following `configuration` section in the `sap.card` section after the `type` subsection. Note, that the card destination is pointing to the same (ES5) destination that is set on the subaccount level.

    ```JSON
      "configuration": {
                            "destinations": {
                                "ES5": {
                                    "name": "ES5",
                                   "defaultUrl": "/sap/opu/odata/sap/EPM_REF_APPS_SHOP_SRV/"
                                }
                            }
                        },
    ```

      !![Image depicting manifest.json file – add configuration section](3-1.PNG)

2. To configure a data request pointing to the SAP Gateway Demo System, add a new `data` section after the `configuration`. In this way the `data` section will be defined on a card level. Note, that our destination is referred here using the double-bracket syntax `{{destinations.ES5}}`.

    ```JSON
    "data": {
    	"request": {
    "url": "{{destinations.ES5}}/sap/opu/odata/sap/EPM_REF_APPS_SHOP_SRV/Products",
    "withCredentials": true
                                },
                    "path": "/d/results"
            },
    ```

    !![Image depicting manifest.json file – add data section](3-2.PNG)

3. To display the dynamically requested data, replace the static `content` section with the following one. The `title`, `description`, `icon`, and `info` properties are now dynamically requested.

    ```JSON
    "content": {
                "item": {
                    "title": "{Name}",
                    "description": "{Description}",
                    "icon": {
                        "src": "{ImageUrl}"
                    },
                    "info": {
                        "value": "{AverageRating}",
                        "state": "{= ${AverageRating} > 3.5 ? 'Success' : 'Warning' }"
                    }
                },
                "maxItems": 5
            }
    ```

      !![Image depicting manifest.json file – replace content section](3-3.PNG)

**Results after Step 3:**

The application displays dynamic data loaded from the SAP Gateway Demo System (ES5). Note, that the actual displayed products may differ depending on the current data in the ES5 demo system.
To learn more, see the [Destinations](https://sapui5.hana.ondemand.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/features/destinations) and [Data](https://sapui5.hana.ondemand.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/features/data) sections in the Card Explorer.

!![Image depicting the application showing dynamic data](3-4.PNG)

If you would like to deploy the card and see how it looks on SAP Work Zone, you can skip to Step 6 and deploy it. In the next steps you add card capabilities that can make your card more interactive.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add manifest parameters)] Manifest parameters provide dynamic values for card attributes. They are replaced during manifest processing and can be used with the double-bracket syntax like: `{{parameters.city}}`. As an example, in this step you will add parameters to set the header (`title` and `subTitle`) properties and the number (`maxItems`) of displayed items in the content.

1. To define parameters - add the following `parameters` subsection in the `manifest.json` in the `configuration` section (note the comma which divides the entries).

    ```JSON
    ,
                                "parameters": {
                                    "title" : {
                                        "value": "List Card with Top {{parameters.maxItems}} Products"
                                    },
                                    "subTitle": {
                                        "value": "These are the top sellers this month"
                                    },
                                    "maxItems": {
                                        "value": 4
                                  		  }
               		 	             }
    ```

      !![Image depicting manifest.json file - add parameters](4-1.PNG)

2. To use the new `maxItems` parameter, replace the `maxItems: 5` static value in the `content` section with the (`maxItems`) parameter as shown below:

    ```JSON
    "maxItems": "{{parameters.maxItems}}"
    ```

      !![Image depicting manifest.json file – use maxItems parameter](4-2.PNG)

3. Let's also use the new parameters in the `header` section. Use the double-bracket syntax and edit (or replace) the header, so it looks like this:

    ```JSON
    "header": {
    			"title": "{{parameters.title}}",
    			"subTitle": "{{parameters.subTitle}}",
    			"icon": {
    				"src": "sap-icon://desktop-mobile"
    			},
    			"status": {
    				"text": "{{parameters.maxItems}} of 20"
    			}
    		},
    ```

      !![Image depicting manifest.json file - edit header](4-3.PNG)

**Results after Step 4:**

In this step you have learned how to declare configurable parameters and use them to achieve desired dynamic behavior. The application now displays a list of 4 items according to the `parameters` property (`maxItems value: 4`).

!![Image depicting the application showing dynamic data using parameters](4-4.PNG)

To learn more, see the [Manifest Parameters](https://sapui5.hana.ondemand.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/features/manifestParameters) section in the Card Explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add user interaction with filtering)] You can make the card even more dynamic when using filters. Filters appear as a dropdown under the card header, and users can interact to customize the data shown by the card. The value of each filter can be used inside a data request definition by using the `{filters>/myFilter/value}` placeholder. When the end user selects different value from the dropdown - a new data request is made with the updated value. As an example, in this step you will add a filter that enables users to filter products by a selected supplier.

1. Add a `filters` subsection in the `configuration` section. It defines a dropdown list with suppliers, which are received by a data request.

    ```JSON
    ,
                             "filters": {
                                 "supplier": {
                                     "value": "{{parameters.selectedSupplierID}}",
                                     "type": "string",
                                     "label": "Supplier",
                                     "description": "Filter the orders by supplier.",
                                        "item": {
                                            "path": "/d/results",
                                            "template": {
                                                "key": "{Id}",
                                                "title": "{Name}"
                                            }
                                        },
                                        "data": {
                                            "request": {
                                                "url": "{{destinations.ES5}}/sap/opu/odata/sap/EPM_REF_APPS_SHOP_SRV/Suppliers",
    "withCredentials": true
                                            }
                                        }
                                    }
                                }
    ```

    !![Image depicting manifest.json file - add filters section](5-1.PNG)

2. Add `selectedSupplierID` subsection in the `parameters` section. This is the supplier that is initially selected in the filter. Later the user can change it from the dropdown list. The `"value": 100000056` corresponds to the supplier ID (of Pear Computing Services) in the SAP Gateway Demo System (ES5).

    ```JSON
    ,
                                    "selectedSupplierID": {
                                        "value": 100000056,
                                        "type": "integer",
                                        "label": "The default selected supplier"
                                    }       
    ```

    !![Image depicting manifest.json file – set the initially selected supplier](5-2.PNG)

3. Add `parameters` in the main `data` section > `request` subsection, after the `url` property as shown below. The `$filter` parameter will be used in a data request for the supplier with ID that is equal to the one selected by the user in the filter's dropdown list.

    ```JSON
    "parameters": {
                                        "$filter": "Supplier/Id eq '{filters>/supplier/value}'"
                                    },
    ```

    !![Image depicting manifest.json file - add filter parameter in the main data section](5-3.PNG)

4. Finally replace the title in the `header` adding the `{filters>/supplier/selectedItem/title}` parameter, which will show the selected vendor:

    ```JSON
    "title": "Products provided by {filters>/supplier/selectedItem/title} vendor ",
    ```

    !![Image depicting manifest.json file – use parameters in the header's title ](5-4.PNG)

**Results after Step 5:**

The [`manifest.json`](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/appstudio-sapui5-integrationcard-create/manifest.json) file is configured with destinations, parameters, and a filter.
The application displays the products from the selected vendor:

!![Image depicting the application showing dynamic data, parameters, and a filter](5-5.PNG)

To learn more, see the [Filters](https://sapui5.hana.ondemand.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html#/learn/features/filters) section in the Card Explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure card parameters that are displayed in SAP Work Zone)]

1. Select the `dt/configuration.js` file (in the Explorer view on the left).

    !![Image depicting the configuration.js file in the file menu](7-1.PNG)

2. Replace the content with the code below:

```JSON
sap.ui.define(["sap/ui/integration/Designtime"], function (
    Designtime
) {
    "use strict";
    return function () {
        return new Designtime({
        "form": {
          "items": {
            "maxItems": {
              "manifestpath": "/sap.card/configuration/parameters/maxItems/value",
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

!![Image depicting the configuration.js file content](7-2.PNG)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Deploy card)]

1. Right-click on the `manifest.json` file (in the Explorer view on the left) and select the **UI Integration Card:Deploy to SAP Work Zone** option from the dropdown menu.

    !![Image depicting UI Integration Card:Deploy to SAP Work Zone option ](6-1.PNG)

2. Select the target SAP Work Zone destination.

    !![Image depicting Select the target Work Zone destination option](6-2.PNG)

3. In the right-bottom corner, confirm to **Continue** and wait to see the successful message.

    !![Image depicting the Continue button to proceed with card deployment](6-3.PNG)

Now the basic UI5 card deployment is done!

[VALIDATE_7]
[ACCORDION-END]


To learn more about the Integration cards and their functionalities, see the [Card Explorer](https://sapui5.hana.ondemand.com/test-resources/sap/ui/integration/demokit/cardExplorer/webapp/index.html) page.


---
