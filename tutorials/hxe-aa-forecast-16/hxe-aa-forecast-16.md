---
title: Execute the PAL Seasonality Test algorithm (Forecast App)
description: Understand and implement the basics of an SAPUI5 application to generate your Forecast results using XSJS services and Machine Learning algorithm in SAP HANA, express edition
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning, topic>sapui5 ]
---

## Prerequisites
 - [Use Machine Learning to Build a Forecasting application using the XS advanced development model](https://developers.sap.com/group.hxe-aa-forecast.html)

## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn
- How to use inheritance in controller
- How use JSON models
- Create and use Formatters
- Use the SAPUI5 Router
- Configure the Application Manifest
- Create and use Fragments (including Pop Over)
- How to use an XS OData service (sorting and filtering) in a table and a `VizFrame`

### Time to Complete
**10 Min**

[ACCORDION-BEGIN [Step 1: ](Open the Web IDE)]

Open the Web IDE, and login using the **`XSA_DEV`** credentials.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

![Web IDE](01-01.png)

As a reminder the default URL for the Web IDE is:

 - `https://hxehost:53075`

A link to the Web IDE can also be found on the ***XSA Controller page*** at:

- `https://hxehost:39030`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create JSON Model)]

In order to drive some of the UI elements, you will define a JSON model. This model will hold the display list for the algorithm and dataset selection.

In the left side panel, expand the **`forecast/html/resources/webapp/model/algorithms/pal`** tree node.

Right click on the **`apl`** folder node from the tree, and select **New > File**.

Enter **`seasonality_test.json`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/html/resources/webapp/model/algorithms/pal/seasonality_test.json
```

Paste the following content:

```JavaScript
{
	"key": "seasonality_test",
	"library": "pal",
	"label": "PAL Seasonality Test",
	"service": {
		"url": "/xsjs/pal/seasonality_test.xsjs",
		"method": "POST",
		"params": {
			"ALPHA": {
				"label": "Autocorrelation coefficient",
				"default": "0.2",
				"step": 0.1,
				"min": 0,
				"max": 1,
				"description": "The criterion for the autocorrelation coefficient. The value range is (0, 1). A larger value indicates stricter requirement for seasonality."
			}
		}
	},
	"default_payload": {
		"DATASETNAME": null,
		"ALPHA":  0.2
	}
}
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 1: ](Edit the Application Descriptor)]

The descriptor file (`manifest.json`) for applications, components, and libraries is inspired by the Web Application Manifest concept introduced by the W3C.

The descriptor provides a central, machine-readable and easy-to-access location for storing metadata associated with an application, an application component, or a library.

It includes the definition of OData data sources and models used by SAPUI5 applications.

Open the **`manifest.json`** file located in the **`forecast/html/resources/webapp`** folder.

Extend the **`"models"`** section with the following element:

```JSON
"pal_seasonality_test": {
	"type": "sap.ui.model.json.JSONModel",
	"preload": true,
	"uri": "model/algorithms/pal/seasonality_test.json"
}
```

In the **`"routing"`** section, extend the **`"routes"`** array with the following element:

```JSON
{
	"name": "seasonality_test",
	"pattern": "seasonality_test",
	"target": "seasonality_test"
}
```

and extend the **`"targets"`** element and add the following element:

```JSON
"seasonality_test": {
	"clearAggregation": true,
	"viewName": "algorithms.pal.seasonality_test"
}
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the controller)]

Expand the **`forecast/html/resources/webapp/controller/algorithms/pal`** folder.

Create a new file **`seasonality_test.controller.js`**.

This is the full path of the created file:

```
forecast/html/resources/webapp/controller/algorithms/pal/seasonality_test.controller.js
```

Paste the following content:

```js
sap.ui.define([
	"forecast/html/base/algorithms/Controller"
], function(Controller) {
	"use strict";
	return Controller.extend("forecast.html.controller.algorithms.pal.seasonality_test", {
		forcedSelectedAlgorithm: "seasonality_test",
		onPressExecute: function(oEvent) {
			Controller.prototype.onPressExecute.apply(this, oEvent);
				this.setVizProperties("result_fit_viz_frame", "result_fit_popover");			
		}
	});
});
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the Fragments)]

Expand the **`forecast/html/resources/webapp/fragment/algorithms/pal`** folder.

Create a new file **`auto_arima_parameters.fragment.xml`**.

This is the full path of the created file:

```
forecast/html/resources/webapp/fragment/algorithms/pal/auto_arima_parameters.fragment.xml
```

Paste the following content:

```xml
<core:FragmentDefinition xmlns="sap.m" xmlns:core="sap.ui.core" xmlns:ui="sap.ui">
	<ui:layout.form.Form editable="true">
		<ui:layout>
			<ui:layout.form.ResponsiveGridLayout columnsL="1" columnsM="1"/>
		</ui:layout>
		<ui:formContainers>
			<ui:layout.form.FormContainer>
				<ui:formElements>
					<ui:layout.form.FormElement label="{pal_seasonality_test>/service/params/ALPHA/label}">
						<ui:fields>
							<Slider progress="true" value="{payload>/ALPHA}" step="{pal_seasonality_test>/service/params/ALPHA/step}"
								min="{pal_seasonality_test>/service/params/ALPHA/min}" max="{pal_seasonality_test>/service/params/ALPHA/max}" enableTickmarks="true"
								inputsAsTooltips="true" tooltip="{pal_seasonality_test>/service/params/ALPHA/description}"/>
							<Input type="Number" value="{payload>/ALPHA}" enabled="false"/>
						</ui:fields>
					</ui:layout.form.FormElement>
				</ui:formElements>
			</ui:layout.form.FormContainer>
		</ui:formContainers>
	</ui:layout.form.Form>
</core:FragmentDefinition>
```

Save the file using the ![save](00-save.png) icon from the menu.

Create a new file **`auto_arima_results.fragment.xml`**.

This is the full path of the created file:

```
forecast/html/resources/webapp/fragment/algorithms/pal/auto_arima_results.fragment.xml
```

Paste the following content:

```xml
<core:FragmentDefinition xmlns="sap.m" xmlns:core="sap.ui.core" xmlns:ui="sap.ui" xmlns:viz="sap.viz">
	<IconTabBar>
		<items>
			<IconTabFilter text="Results">
				<viz:ui5.controls.Popover id="result_popover"/>
				<viz:ui5.controls.VizFrame width="100%" id="result_viz_frame" uiConfig="{applicationSet:'fiori'}" vizType='timeseries_line'>
					<viz:dataset>
						<viz:ui5.data.FlattenedDataset data="{path : 'results>/tables/OUTPUT'}">
							<viz:dimensions>
								<viz:ui5.data.DimensionDefinition name="Date" value="{path : 'results>signal_time'}" dataType="date"/>
							</viz:dimensions>
							<viz:measures>
								<viz:ui5.data.MeasureDefinition name="Seasonal" value="{path : 'results>seasonal'}"/>
								<viz:ui5.data.MeasureDefinition name="Trend" value="{path : 'results>trend'}"/>
								<viz:ui5.data.MeasureDefinition name="Random" value="{path : 'results>random'}"/>
							</viz:measures>
						</viz:ui5.data.FlattenedDataset>
					</viz:dataset>
					<viz:feeds>
						<viz:ui5.controls.common.feeds.FeedItem uid="valueAxis" type="Measure" values="Seasonal"/>
						<viz:ui5.controls.common.feeds.FeedItem uid="valueAxis" type="Measure" values="Trend"/>
						<viz:ui5.controls.common.feeds.FeedItem uid="valueAxis" type="Measure" values="Random"/>
						<viz:ui5.controls.common.feeds.FeedItem uid="timeAxis" type="Dimension" values="Date"/>
					</viz:feeds>
				</viz:ui5.controls.VizFrame>
				<ui:table.Table enableBusyIndicator="true" selectionMode="None" width="100%"
					rows="{path : 'results>/tables/OUTPUT', sorter: { path: 'signal_time', descending: true} }">
					<ui:columns>
						<ui:table.Column sortProperty="signal_time" filterProperty="signal_time">
							<Label text="Date"/>
							<ui:template>
								<Text text="{path : 'results>signal_time'}"/>
							</ui:template>
						</ui:table.Column>
						<ui:table.Column sortProperty="seasonal" filterProperty="seasonal">
							<Label text="Seasonal"/>
							<ui:template>
								<Text text="{path : 'results>seasonal', formatter : '.formatter.formatNumber'}"/>
							</ui:template>
						</ui:table.Column>
						<ui:table.Column sortProperty="trend" filterProperty="trend">
							<Label text="Trend"/>
							<ui:template>
								<Text text="{path : 'results>trend', formatter : '.formatter.formatNumber'}"/>
							</ui:template>
						</ui:table.Column>
						<ui:table.Column sortProperty="random" filterProperty="random">
							<Label text="Random"/>
							<ui:template>
								<Text text="{path : 'results>random', formatter : '.formatter.formatNumber'}"/>
							</ui:template>
						</ui:table.Column>
					</ui:columns>
				</ui:table.Table>
			</IconTabFilter>
			<IconTabFilter text="Statistics">
				<ui:table.Table enableBusyIndicator="true" selectionMode="None" width="100%" height="100%" rows="{path : 'results>/tables/STATISTIC'}">
					<ui:columns>
						<ui:table.Column sortProperty="stat_name" filterProperty="stat_name">
							<Label text="Statistics name"/>
							<ui:template>
								<Text text="{path : 'results>stat_name'}"/>
							</ui:template>
						</ui:table.Column>
						<ui:table.Column sortProperty="stat_value" filterProperty="stat_value">
							<Label text="Statistics Value"/>
							<ui:template>
								<Text text="{path : 'results>stat_value'}"/>
							</ui:template>
						</ui:table.Column>
					</ui:columns>
				</ui:table.Table>
			</IconTabFilter>
		</items>
	</IconTabBar>
</core:FragmentDefinition>
```

Save the file using the ![save](00-save.png) icon from the menu.
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the View)]

Expand the **`forecast/html/resources/webapp/view/algorithms/pal`** folder.

Create a new file **`seasonality_test.view.xml`**.

This is the full path of the created file:

```
forecast/html/resources/webapp/view/algorithms/pal/auto_arima_results.view.xml
```

Paste the following content:

```xml
<mvc:View xmlns:html="http://www.w3.org/1999/xhtml" xmlns:mvc="sap.ui.core.mvc" xmlns="sap.m" xmlns:ui="sap.ui"
 controllerName="forecast.html.controller.algorithms.pal.seasonality_test"
	displayBlock="true">
	<App id="idAppControl">
		<Page showHeader="true" showNavButton="true" navButtonPress="onNavHome" title="Forecast - PAL Seasonality Test">
			<content>
				<ui:core.Fragment fragmentName="forecast.html.fragment.display_list" type="XML"/>
				<IconTabBar expandable="false" visible="{config>/enableSelectDataset}" id="tab">
					<items>
						<IconTabFilter key="params" text="Set The Algorithm Execution Parameters">
							<ui:core.Fragment fragmentName="forecast.html.fragment.algorithms.pal.seasonality_test_parameters" type="XML"/>
							<Button text="Execute" press="onPressExecute"/>
						</IconTabFilter>
						<IconTabFilter key="result" text="Results" visible="{= !!${results>/hasResult} }">
							<ui:core.Fragment fragmentName="forecast.html.fragment.algorithms.pal.seasonality_test_results" type="XML"/>
						</IconTabFilter>
					</items>
				</IconTabBar>
			</content>
			<footer>
				<Bar>
					<contentLeft>
						<Button text="Back" press="onNavHome"/>
					</contentLeft>
				</Bar>
			</footer>
		</Page>
	</App>
</mvc:View>
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Run the application)]

Select the **`html`** module, then click on the execute icon ![run](00-run.png) from the menu bar.

Once the application is started, the application will open in a new tab/window or you can click on the application URL:

![Web IDE](05-01.png)

This will open a web page with the following content:

![Web IDE](05-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Validate your results)]

Select **PAL Seasonality Test Algorithm**, then pick the **Trend And Cyclic** dataset.

Click on **Next**.

![Applications](06-01.png)

Leave the parameters with their default values and click on **Execute**.

![Applications](06-02.png)

The **Results** tab will be activated.

Et Voilà!

![Applications](06-03.png)

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Commit your changes)]

On the icon bar located on the right side of the Web IDE, click on the **Git Pane** icon ![Web IDE](00-webide-git.png).

Click on **Stage All**, enter a commit comment, then click on **Commit and Push > origin master**.

[DONE]
[ACCORDION-END]
