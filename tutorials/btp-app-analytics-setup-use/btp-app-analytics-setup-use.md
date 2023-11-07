---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Create an SAP Fiori Elements-Based Analytical UI for your CAP Application
description: This tutorial shows you how to create an analytical page using SAP Fiori elements app on top of your previously created CAP application.
keywords: cap
auto_validation: true
time: 25
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori-tools, software-product>sapui5]
primary_tag: software-product-function>sap-cloud-application-programming-model
---



## Prerequisites
 - [Prepare Your Development Environment for CAP](btp-app-prepare-dev-environment-cap)
 - Before you start with this tutorial, you have two options:
    - Follow the instructions in **Step 16: Start from an example branch** of [Prepare Your Development Environment for CAP](btp-app-prepare-dev-environment-cap) to checkout the [`cap-roles`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/cap-roles) branch.
      - Complete the group of tutorials [Create a CAP Application and SAP Fiori UI](group.btp-app-cap-create).


## Details
### You will learn
 - How to set up your application for analytics
 - Add new files to your project
 - Generate UI with an SAP Fiori elements template
 - Modify the UI with OData annotations
 - Check the annotation files
 - Visualize Risks in the analytics UI
 - Add your application to the launch page


---

[ACCORDION-BEGIN [Step 1: ](Overview)]
SAP Fiori elements is a framework that comprises the most commonly used floor plans and is designed to:

1. Speed up development by reducing the amount of front-end code needed to build SAP Fiori apps.
2. Drive UX consistency and compliance with the latest SAP Fiori design guidelines.

SAP Fiori elements for OData version 2 (V2) offers **Analytical list page (ALP)** as a separate floor plan for a unique way to analyze data step by step from different perspectives, to investigate a root cause through drill-down, and to act on transactional content. On the other hand, in SAP Fiori elements for OData V4, the **Analytical List Page (ALP)** is not a separate floor plan, but rather a "flavor" of the **List Report Object Page** floor plan. Hence, you can configure a **List Report Object Page** floor plan in such a way that it can serve as an "ALP flavor" report page.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Add new files to your project)]
Create a new service for Analytics as given in [Create a CAP-Based Application](btp-app-create-cap-application) using the following details.

1. Open the `tutorial` directory created in tutorial [Prepare Your Development Environment for CAP](btp-app-prepare-dev-environment-cap).

2. Open the folder `templates`.

3. Copy the file `risk-analysis-service.cds` from `templates/analytics/srv` to the `srv` folder of your app.

    The content of the file looks like this:

    ```JavaScript
    using { sap.ui.riskmanagement as my } from '../db/schema';
    using { RiskService } from './risk-service';

    extend service RiskService {
        @readonly
        entity RisksAnalysis @(restrict : [
                    {
                        grant : [ 'READ' ],
                        to : [ 'RiskManager' ]
                    }
                ]) as projection on my.Risks {
            *,
            substring(createdAt,1,4) as riskyear:String,
            cast (substring(createdAt,1,10) as Date) as createdAt
        };
    }

    // Fix ambiguity in Mitigations.risk association ensuring it points to Risks
    extend RiskService.Risks with @cds.redirection.target;
    ```

    It extends the `RiskService` with a new `RisksAnalysis` entity that has a couple of date fields for data analysis.

4. Run `cds watch` in the VS Code terminal and open the link [http://localhost:4004/](http://localhost:4004/) in your browser.

    You should be able to see the new `RisksAnalysis` entity among with the rest of the `risk` service entities under `Service Endpoints`.

    !![RisksAnalysis Entity](entity_riskAnalysis.png)

5. Copy the latest  `sap.ui.riskmanagement-Risks.csv` file from `templates/analytics/db/data` and replace the existing file in the `db/data` folder of your app.

    This `.csv` file has a large number of records for analytical purposes.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Generate the UI with an SAP Fiori template)]
1. In VS Code, invoke the Command Palette ( **View** &rarr; **Command Palette** or <kbd>Shift</kbd> + <kbd>Command</kbd> + <kbd>P</kbd> for macOS / <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>P</kbd> for Windows) and choose **Fiori: Open Application Generator**.

    > VS Code will automatically install `@sap/generator-fiori` if missing and open the **Template Wizard**.

    > In case you get an error launching the Application Generator, see the [SAP Fiori tools FAQ](https://help.sap.com/viewer/42532dbd1ebb434a80506113970f96e9/Latest/en-US) on SAP Help Portal to find a solution.

2. Choose application type **SAP Fiori** and floor plan **List Report Page**.

       !![V4 Template](analytics-template-selection.png)

    > Why not use the Analytical List Page floor plan?

    > Although you are using SAP Fiori elements for OData V4, you will notice that there is still a floor plan **Analytical List Page** in the previous screen. However, it is due to be removed from the Fiori Generator UI, so you are not going to use it for this tutorial.

3. Choose **Next**.

4. In the next dialog, choose **Use a Local CAP Project** and point to the folder of your current **`cpapp`** project.

    > In case you get the error: `Node module @sap/cds isn't found. Please install it and try again.`

    > You might get the error `Node module @sap/cds is not found. Please install it and try again.` after you have chosen your CAP project. This is an issue with the App Generator not finding the corresponding CAP modules, due to different repositories. This should be a temporary issue. For the meantime you can work around it by opening a command line and running the following command:

    > ```bash
    > npm install --global @sap/cds-dk --@sap:registry=https://npmjs.org/
    > ```

    > See the [CAP Troubleshooting guide](https://cap.cloud.sap/docs/advanced/troubleshooting#npm-installation) for more details.

5. Select the **`RiskService(Node.js)`** as the OData service and choose **Next**.

    !![CAPpro](analytics-data-source.png)

6. Select **Risks Analysis** as the main entity, choose the option **No** to avoid adding table columns automatically. Choose **Next**.

    !![entitySelection](analytics-entity-selection.png)

7. Enter `risk-analysis` as the module name and `Risk Analysis` as the application title.

8. Enter `ns` as the namespace and `Risk Analysis` as the description for the application.

9. Choose **Finish** to generate the application.

    !![Project Names](analytics-project-attributes.png)

The application is now generated and in a few seconds you can see it in the `app` folder of your project. It contains a `risk-analysis` and a `webapp` folder with a `Component.js` file that is characteristic for an SAPUI5 app.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Check your application)]
1. If it is not still running from the previous tutorial, execute `cds watch` in a VS Code terminal and switch to <http://localhost:4004> in your browser.

    You can now see that the CAP server has discovered an HTML page in your `app` folder:

       !![Index HTML Page](fea_app.png)

2. Choose the link [http://localhost:4004/risk-analysis/webapp/index.html](http://localhost:4004/risk-analysis/webapp/index.html) for the HTML page.

2. Choose **Go** and log in.

      - **Username**: `risk.manager@tester.sap.com`
      - **Password**: `initial`

    !![Login Empty App](login_empty_app.png)

    You can now see the application without any data. The table is empty because the application is currently missing UI annotations. You add them in the next step.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Modify the UI with OData annotations)]
1. Open the `app/risk-analysis/webapp/manifest.json` file.

2. Add the following lines to it:

    ```JSON[24-70]
    {
        "_version": "1.40.0",
        "sap.app": {
            ...
        },
        "sap.ui5": {
            "flexEnabled": true,
            "dependencies": {
            ...
            },
            "routing": {
                "config": {},
                "routes": [
                    ...
                ],
                "targets": {
                "RiskAnalysisList": {
                    ...
                    "options": {
                        "settings": {
                            ...
                            "navigation": {
                            ...
                            },
                            "initialLoad": true,
                            "views": {
                                "paths": [
                                    {
                                        "primary": [
                                            {
                                                "annotationPath": "@com.sap.vocabularies.UI.v1.Chart"
                                            }
                                        ],
                                        "secondary": [
                                            {
                                                "annotationPath": "@com.sap.vocabularies.UI.v1.LineItem"
                                            }
                                        ],
                                        "defaultPath": "both"
                                    }
                                ]
                            },
                            "controlConfiguration": {
                                "@com.sap.vocabularies.UI.v1.LineItem": {
                                    "tableSettings": {
                                        "type": "AnalyticalTable",
                                        "enableExport": true
                                    }
                                },
                                "@com.sap.vocabularies.UI.v1.SelectionFields": {
                                    "layout": "CompactVisual",
                                    "initialLayout": "Visual",
                                    "filterFields": {
                                        "prio": {
                                            "label": "Priority",
                                            "availability": "Default",
                                            "visualFilter": {
                                                "valueList": "com.sap.vocabularies.Common.v1.ValueList#vlPrio"
                                            }
                                        },
                                        "createdAt": {
                                            "label": "creation Date",
                                            "availability": "Default",
                                            "visualFilter": {
                                                "valueList": "com.sap.vocabularies.Common.v1.ValueList#vlcreatedAt"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                    },
                    "RiskAnalysisObjectPage": {
                        ...
                    }
                }
            }
        },
        ...
    }
    ```

3. Copy the file `risks-analysis-service-ui.cds` from `templates/analytics/srv` to the `srv` folder of your app.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 6: ](Check the annotations file)]
Let us have a look at the `risks-analysis-service-ui.cds`.

There are three major segments in the **List Report Object Page** (ALP flavor):

- Visual filters
- Smart chart
- Analytical table

The annotations file `risks-analysis-service-ui.cds` and the `app/risk-analysis/webapp/manifest.json` file are used to specify configurations for these artifacts.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 7: ](Visual filters)]
[OPTION BEGIN [Chart with data point]]

The visual filter bar offers a unique way of filtering large datasets through visualizations. Currently SAP Fiori elements V4 supports Line and Bar charts for visual filters.

!![Visual Filters](fea_visual_filters.png)

A visual filter can be defined by specifying `CommonValueList` in the annotations file and `SelectionFields` in the manifest file.

`Common Value List` provides the list of values from a specified property of the entity. It has two qualifiers underneath:

- Presentation variant defines the presentation / visualization of the chart (visual filter)
- Selection variant specifies the fields to be displayed for classical filtering (without chart and with dropdown boxes)

This is how you can define the **Cost Impact over Priority** visual filter:

You can define a chart (`#chartPrio`) with a data point (`#dpPrio`) in the following way:

```JavaScript
UI.Chart #chartPrio : {
    $Type : 'UI.ChartDefinitionType',
    ChartType : #Bar,
    Dimensions : [
        prio
    ],
    DimensionAttributes : [
        {
            $Type : 'UI.ChartDimensionAttributeType',
            Dimension : prio,
            Role : #Category
        }
    ],
    Measures : [
        sumImpact
    ],
    MeasureAttributes : [
        {
            $Type : 'UI.ChartMeasureAttributeType',
            Measure : sumImpact,
            Role : #Axis1,
            DataPoint : '@UI.DataPoint#dpPrio',
        }
    ]
},
UI.DataPoint #dpPrio              : {
    Value       : impact,
    Title       : 'Impact'
}
```

Similarly, other charts like **Line Chart for Visual Filters** can be added.
[OPTION END]
[OPTION BEGIN [Presentation variant]]

The visual filter bar offers a unique way of filtering large datasets through visualizations. Currently SAP Fiori elements V4 supports Line and Bar charts for visual filters.

!![Visual Filters](fea_visual_filters.png)

A visual filter can be defined by specifying `CommonValueList` in the annotations file and `SelectionFields` in the manifest file.

`Common Value List` provides the list of values from a specified property of the entity. It has two qualifiers underneath:

- Presentation variant defines the presentation / visualization of the chart (visual filter)
- Selection variant specifies the fields to be displayed for classical filtering (without chart and with dropdown boxes)

This is how you can define the **Cost Impact over Priority** visual filter:

You can also use the chart (`#chartPrio`) to define a presentation variant (`#pvPrio`):

```JavaScript
UI.PresentationVariant #pvPrio : {
    SortOrder : [
        {
            $Type : 'Common.SortOrderType',
            Property : impact,
            Descending : true
        },
    ],
    Visualizations : [
        '@UI.Chart#chartPrio'
    ]
},
```

Similarly, other charts like **Line Chart for Visual Filters** can be added.
[OPTION END]
[OPTION BEGIN [Selection variant]]

The visual filter bar offers a unique way of filtering large datasets through visualizations. Currently SAP Fiori elements V4 supports Line and Bar charts for visual filters.

!![Visual Filters](fea_visual_filters.png)

A visual filter can be defined by specifying `CommonValueList` in the annotations file and `SelectionFields` in the manifest file.

`Common Value List` provides the list of values from a specified property of the entity. It has two qualifiers underneath:

- Presentation variant defines the presentation / visualization of the chart (visual filter)
- Selection variant specifies the fields to be displayed for classical filtering (without chart and with dropdown boxes)

This is how you can define the **Cost Impact over Priority** visual filter:

You can define a selection variant (`#svPrio`) as defined in the following:

```JavaScript
UI.SelectionVariant #svPrio : {
    SelectOptions : [
        {
            $Type : 'UI.SelectOptionType',
            PropertyName : impact,
            Ranges : [
                {
                    $Type : 'UI.SelectionRangeType',
                    Sign : #I,
                    Option : #GE,
                    Low : 0,
                },
            ],
        },
    ],
},
```

Similarly, other charts like **Line Chart for Visual Filters** can be added.
[OPTION END]
[OPTION BEGIN [Common value list]]

The visual filter bar offers a unique way of filtering large datasets through visualizations. Currently SAP Fiori elements V4 supports Line and Bar charts for visual filters.

!![Visual Filters](fea_visual_filters.png)

A visual filter can be defined by specifying `CommonValueList` in the annotations file and `SelectionFields` in the manifest file.

`Common Value List` provides the list of values from a specified property of the entity. It has two qualifiers underneath:

- Presentation variant defines the presentation / visualization of the chart (visual filter)
- Selection variant specifies the fields to be displayed for classical filtering (without chart and with dropdown boxes)

This is how you can define the **Cost Impact over Priority** visual filter:

Common value list (`#vlPrio`) is created by providing these two qualifiers (`#pvPrio` & `#svPrio`) and the property for the value list:

```JavaScript
prio @(
        Common.ValueList #vlPrio: {
            Label : 'Priority',
            CollectionPath : 'Risks',
            SearchSupported : true,
            PresentationVariantQualifier : 'pvPrio',
            SelectionVariantQualifier : 'svPrio',
            Parameters : [
                {
                    $Type : 'Common.ValueListParameterInOut',
                    LocalDataProperty : prio,
                    ValueListProperty : 'prio'
                },
            ]
        }
    );

```

Finally the common value list configuration (`prio`) is added into selection fields in the manifest file:

```JSON
"@com.sap.vocabularies.UI.v1.SelectionFields": {
    "layout": "CompactVisual",
    "initialLayout": "Visual",
    "filterFields": {
        "prio": {
            "label": "Priority",
            "availability": "Default",
            "visualFilter": {
                "valueList": "com.sap.vocabularies.Common.v1.ValueList#vlPrio"
            }
        }
    }
}
```

Similarly, other charts like **Line Chart for Visual Filters** can be added.
[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 8: ](Visualize risks in the analytics UI)]
1. With `cds watch` running, open the app in your browser at <http://localhost:4004/risk-analysis/webapp/index.html>.

2. Log in if needed.

      - **Username**: `risk.manager@tester.sap.com`
      - **Password**: `initial`

3. You should be able to see the generated resultant visualization.

    !![Dashboard](fea_result_page.png)

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 9: ](Add your app to the launch page)]
Let us add the `Risk Analysis` application to the launch page where you already have the `Risks` and `Mitigations` apps.

1. Open the file `launchpage.html` in `app` folder of your application.

2. Add the following code after `mitigations-app`:

    ```HTML[20-27]
        <script>
                window['sap-ushell-config'] = {
                    defaultRenderer: 'fiori2',
                    applications: {
                        "risks-app": {
                            title: 'Risks',
                            description: 'Risks',
                            additionalInformation: 'SAPUI5.Component=ns.risks',
                            applicationType: 'URL',
                            url: "./risks/webapp",
                            navigationMode: 'embedded'
                        },
                        "mitigations-app": {
                            title: "Mitigations",
                            description: "Mitigations",
                            additionalInformation: "SAPUI5.Component=ns.mitigations",
                            applicationType: "URL",
                            url: "./mitigations/webapp",
                            navigationMode: "embedded"
                        },
                        "risk-analysis-app": {
                            title: "Risk Analysis",
                            description: "Risk Analysis",
                            additionalInformation: "SAPUI5.Component=ns.riskanalysis",
                            applicationType: "URL",
                            url: "./risk-analysis/webapp",
                            navigationMode: "embedded"
                        }
                    }
                };
            </script>
    ```

4. With `cds watch` running, open the app in your browser at [http://localhost:4004/launchpage.html#Shell-home](http://localhost:4004/launchpage.html#Shell-home).

5. You can now see the `Risk Analysis` app next to the `Mitigations` app.

    !![Launchpage](fea_Launchpage.png)


[VALIDATE_1]
The result of this tutorial can be found in the [`analytics`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/analytics) branch.


[ACCORDION-END]
---