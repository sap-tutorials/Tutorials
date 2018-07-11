---
title: Execute the Recommendations algorithm
description: Understand and implement the basics of an SAPUI5 application to generate your `Movielens` recommendation results using XSJS services
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning, topic>sapui5 ]
---

## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn

- How to add a SAPUI5 controller and interact with an XSJS service
- How to add a SAPUI5 view and interact with an XSJS service

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

[ACCORDION-BEGIN [Step 2: ](Create the controller)]

As you will be building two view, you would usually create a controller per view. But here, in order to simplify to code, only one is needed.

A few details about the controller:

- it provides formatters for the number and date values to be displayed in a nicer format
- handles the entry suggestions list with model filters on all fields from the `ratings_movie` and `ratings_user` views

Expand the **`movielens/html/resources/webapp/controller/xsjs`** folder.

Create a new file **`execute.controller.js`**.

This is the full path of the created file:

```
movielens/html/resources/webapp/controller/xsjs/execute.controller.js
```

Paste the following content:

```js
sap.ui.define([
	"movielens/html/controller/demo.controller",
	"sap/m/MessageBox"
], function(Controller, MessageBox) {
	"use strict";

	return Controller.extend("movielens.html.controller.xsjs.execute", {
		onInit: function() {
			this.getView().setModel(new sap.ui.model.json.JSONModel(), "results");
		},
		ajaxCallCompleted: function(status, message) {
			MessageBox.show(message, {
				title: status
			});
			this.oBusyIndicator.close();
		},
		onPressExecute: function(oEvent) {
			var oController = this;
			oController.oBusyIndicator = new sap.m.BusyDialog();
			oController.oBusyIndicator.open();

			var library = oEvent.getSource().data("library");
			var algorithm = oEvent.getSource().data("algorithm");

			var configModel = oController.getView().getModel("config");
			var configData = configModel.getData();
			var serviceMethod = configData.services[library + "." + algorithm].execute_method;
			var serviceUrl = configData.services[library + "." + algorithm].execute_url;
			var serviceData = configData.default[library + "." + algorithm];

			var results = oController.getView().getModel("results");

			var ajaxSuccess = function(result, status) {
				oController.ajaxCallCompleted(status, result.message);
				results.setProperty("/results", result.results);
			};
			var ajaxError = function(xhr, status, error) {
				var msg = error;
				if(error.message) {
					msg = error.message;
				}
				oController.ajaxCallCompleted(status, msg);
			};
			$.ajax({
				method: serviceMethod,
				url: serviceUrl,
				async: true,
				timeout: 3000000,
				headers: {
					"content-type": "application/json",
					"accept": "application/json"
				},
				data: JSON.stringify(serviceData),
				success: ajaxSuccess,
				error: ajaxError
			});
		},
		onPressResults: function(oEvent) {
			var oController = this;
			oController.oBusyIndicator = new sap.m.BusyDialog();
			oController.oBusyIndicator.open();

			var library = oEvent.getSource().data("library");
			var algorithm = oEvent.getSource().data("algorithm");
			var resultType = oEvent.getSource().data("resultType");

			var configModel = oController.getView().getModel("config");
			var configData = configModel.getData();

			var serviceMethod = configData.services[library + "." + algorithm].results_method;
			var serviceUrl = configData.services[library + "." + algorithm].results_url;
			var serviceData = configData.default[library + "." + algorithm];
			serviceData.resultType = resultType;

			var results = oController.getView().getModel("results");

			var ajaxSuccess = function(result, status) {
				oController.ajaxCallCompleted(status, result.message);
				results.setProperty("/items", result.results);
				results.setProperty("/hasResult", true);
				results.setProperty("/resultType", resultType);
			};
			var ajaxError = function(xhr, status, error) {
				var msg = error;
				if(error.message) {
					msg = error.message;
				}
				oController.ajaxCallCompleted(status, msg);
			};
			$.ajax({
				method: serviceMethod,
				url: serviceUrl,
				async: true,
				timeout: 3000000,
				headers: {
					"content-type": "application/json",
					"accept": "application/json"
				},
				data: JSON.stringify(serviceData),
				success: ajaxSuccess,
				error: ajaxError
			});
		}
	});
});
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create the views)]

As you will be executing both the **collaborative filtering** and **content-based filtering** algorithms, you will need to create two views.

A few details about the content of the views:

- an series of input field that represents the algorithms parameters (not all algorithm parameters are actually exposed)

- a section to display the **collaborative filtering** results and the **collaborative filtering** results with the relevant parameters to customize the output.

Expand the **`movielens/html/resources/webapp/view/xsjs`** folder.

Create a new file **`apl_recommendation.view.xml`**.

This is the full path of the created file:

```
movielens/html/resources/webapp/view/xsjs/apl_recommendation.view.xml
```

Paste the following content:

```xml
<mvc:View xmlns:html="http://www.w3.org/2000/xhtml" xmlns:mvc="sap.ui.core.mvc" xmlns="sap.m" xmlns:form="sap.ui.layout.form"
	xmlns:table="sap.ui.table" xmlns:core="sap.ui.core" xmlns:layout="sap.ui.layout"
	xmlns:custom="http://schemas.sap.com/sapui5/extension/sap.ui.core.CustomData/1" controllerName="movielens.html.controller.xsjs.execute">
	<Page showHeader="false" press="onNavMenuItemPress" icon="sap-icon://menu" id="master">
		<content>
			<Bar>
				<contentMiddle>
					<Title text="Execute The APL Recommendation Algorithm"/>
				</contentMiddle>
				<contentLeft>
					<Button icon="sap-icon://menu" press="handlePressOpenMenu"/>
				</contentLeft>
			</Bar>
			<Panel expandable="true" expanded="true" headerText="Algorithm Execution Parameters">
				<form:Form editable="true">
					<form:layout>
						<form:ResponsiveGridLayout columnsL="1" columnsM="1"/>
					</form:layout>
					<form:formContainers>
						<form:FormContainer>
							<form:formElements>
								<form:FormElement label="Best Seller Threshold">
									<form:fields>
										<Input type="Number" value="{config>/default/apl.recommendation/BESTSELLERTHRESHOLD}"/>
									</form:fields>
								</form:FormElement>
								<form:FormElement label="Max Top Nodes">
									<form:fields>
										<Input type="Number" value="{config>/default/apl.recommendation/MAXTOPNODES}"/>
									</form:fields>
								</form:FormElement>
								<form:FormElement label="Minimum Confidence">
									<form:fields>
										<Slider progress="true" value="{config>/default/apl.recommendation/MINIMUMCONFIDENCE}" step="0.01" min="0" max="1" enableTickmarks="true"
											inputsAsTooltips="true"/>
										<Input type="Number" value="{config>/default/apl.recommendation/MINIMUMCONFIDENCE}" enabled="false"/>
									</form:fields>
								</form:FormElement>
								<form:FormElement label="Minimum Predictive Power">
									<form:fields>
										<Slider progress="true" value="{config>/default/apl.recommendation/MINIMUMPREDICTIVEPOWER}" step="0.01" min="0" max="1"
											enableTickmarks="true" inputsAsTooltips="true"/>
										<Input type="Number" value="{config>/default/apl.recommendation/MINIMUMPREDICTIVEPOWER}" enabled="false"/>
									</form:fields>
								</form:FormElement>
								<form:FormElement label="Minimum Support">
									<form:fields>
										<Input type="Number" value="{config>/default/apl.recommendation/MINIMUMSUPPORT}" placeholder="Enter an id" step="0.01" min="0" max="1"
											enableTickmarks="true" inputsAsTooltips="true"/>
									</form:fields>
								</form:FormElement>
							</form:formElements>
						</form:FormContainer>
					</form:formContainers>
				</form:Form>
				<Toolbar width="100%">
					<content>
						<Button text="Execute" press="onPressExecute" custom:library="apl" custom:algorithm="recommendation"/>
					</content>
				</Toolbar>
			</Panel>
			<Panel expandable="true" headerText="Collaborative Filtering Result">
				<ScrollContainer height="100%" width="100%" horizontal="true" vertical="true" focusable="true">
					<Panel expandable="false" expanded="true" headerText="Parameters">
						<form:Form editable="true">
							<form:layout>
								<form:ResponsiveGridLayout columnsL="1" columnsM="1"/>
							</form:layout>
							<form:formContainers>
								<form:FormContainer>
									<form:formElements>
										<form:FormElement label="User Id">
											<form:fields>
												<Input required="true" width="100%" showSuggestion="true" suggest="onSuggestionSuggest" value="{config>/default/apl.recommendation/USERID}"
													custom:type="user" id="user_input" placeholder="Enter a user identifier..."
													suggestionItems="{ path: 'odata>/ratings_user', filters: [ { path: 'USERID', operator: 'EQ', value1: '-1' } ]}">
													<suggestionItems>
														<core:ListItem key="{odata>USERID}" text="{odata>USERID}" additionalText="Rating count : {odata>RATING_COUNT}"/>
													</suggestionItems>
												</Input>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Include Best Seller">
											<form:fields>
												<Switch state="{config>/default/apl.recommendation/INCLUDEBESTSELLERS}">
													<layoutData>
														<FlexItemData growFactor="1"/>
													</layoutData>
												</Switch>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Best Seller Threshold">
											<form:fields>
												<Input type="Number" value="{config>/default/apl.recommendation/BESTSELLERTHRESHOLD}"/>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Skip already rated">
											<form:fields>
												<Switch state="{config>/default/apl.recommendation/SKIPALREADYOWNED}">
													<layoutData>
														<FlexItemData growFactor="1"/>
													</layoutData>
												</Switch>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Top N Results">
											<form:fields>
												<Slider progress="true" value="{config>/default/apl.recommendation/KEEPTOPN}" step="1" min="5" max="20" enableTickmarks="true"
													inputsAsTooltips="true"/>
												<Input type="Number" value="{config>/default/apl.recommendation/KEEPTOPN}" enabled="false"/>
											</form:fields>
										</form:FormElement>
									</form:formElements>
								</form:FormContainer>
							</form:formContainers>
						</form:Form>
						<Button text="Get Results" press="onPressResults" custom:library="apl" custom:algorithm="recommendation" custom:resultType="collaborative"/>
					</Panel>
					<Panel expandable="false" visible="{= !!${results>/hasResult} &amp;&amp; ${results>/resultType} === 'collaborative'}" headerText="Results">
						<table:Table selectionMode="None" visibleRowCount="5" enableBusyIndicator="true" refresh="true" rows="{ path: 'results>/items'}">
							<table:columns>
								<table:Column sortProperty="MOVIEID" width="10%">
									<Label text="Movie ID"/>
									<table:template>
										<Text text="{results>MOVIEID}"/>
									</table:template>
								</table:Column>
								<table:Column sortProperty="TITLE">
									<Label text="Title"/>
									<table:template>
										<Text text="{results>TITLE}"/>
									</table:template>
								</table:Column>
								<table:Column sortProperty="RANK" width="10%">
									<Label text="Rank"/>
									<table:template>
										<Text text="{results>RANK}"/>
									</table:template>
								</table:Column>
								<table:Column sortProperty="SCORE" width="10%">
									<Label text="Score"/>
									<table:template>
										<Text text="{path : 'results>SCORE', formatter : '.formatter.formatPercent'}"/>
									</table:template>
								</table:Column>
								<table:Column width="15%">
									<Label text="Links"/>
									<table:template>
										<HBox>
											<Link text="IMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="http://www.imdb.com/title/tt{results>IMDBID}"/>
											<Link text="TMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="https://www.themoviedb.org/movie/{results>TMDBID}"/>
										</HBox>
									</table:template>
								</table:Column>
							</table:columns>
						</table:Table>
					</Panel>
				</ScrollContainer>
			</Panel>
			<Panel expandable="true" headerText="Content-Based Filtering Result">
				<ScrollContainer height="100%" width="100%" horizontal="true" vertical="true" focusable="true">
					<Panel expandable="false" expanded="true" headerText="Parameters">
						<form:Form editable="true">
							<form:layout>
								<form:ResponsiveGridLayout columnsL="1" columnsM="1"/>
							</form:layout>
							<form:formContainers>
								<form:FormContainer>
									<form:formElements>
										<form:FormElement label="Movie Id">
											<form:fields>
												<Input required="true" width="100%" showSuggestion="true" suggest="onSuggestionSuggest" value="{config>/default/apl.recommendation/MOVIEID}"
													custom:type="movie" id="movie_input" placeholder="Enter a movie name or identifier ..."
													suggestionItems="{ path: 'odata>/ratings_movie', filters: [ { path: 'MOVIEID', operator: 'EQ', value1: '-1' } ]}">
													<suggestionItems>
														<core:ListItem key="{odata>MOVIEID}" text="{odata>MOVIEID}" additionalText="Title: {odata>TITLE} - Rating count : {odata>RATING_COUNT}"/>
													</suggestionItems>
												</Input>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Include Best Seller">
											<form:fields>
												<Switch state="{config>/default/apl.recommendation/INCLUDEBESTSELLERS}">
													<layoutData>
														<FlexItemData growFactor="1"/>
													</layoutData>
												</Switch>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Best Seller Threshold">
											<form:fields>
												<Input type="Number" value="{config>/default/apl.recommendation/BESTSELLERTHRESHOLD}"/>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Top N Results">
											<form:fields>
												<Slider progress="true" value="{config>/default/apl.recommendation/KEEPTOPN}" step="1" min="5" max="20" enableTickmarks="true"
													inputsAsTooltips="true"/>
												<Input type="Number" value="{config>/default/apl.recommendation/KEEPTOPN}" enabled="false"/>
											</form:fields>
										</form:FormElement>
									</form:formElements>
								</form:FormContainer>
							</form:formContainers>
						</form:Form>
						<Button text="Get Results" press="onPressResults" custom:library="apl" custom:algorithm="recommendation" custom:resultType="contentbased"/>
						<Panel expandable="false" visible="{= !!${results>/hasResult} &amp;&amp; ${results>/resultType} === 'contentbased'}" headerText="Results">
							<table:Table selectionMode="None" visibleRowCount="5" enableBusyIndicator="true" refresh="true" rows="{ path: 'results>/items'}">
								<table:columns>
									<table:Column sortProperty="SIMILAR_MOVIE" width="10%">
										<Label text="Similar Movie ID"/>
										<table:template>
											<Text text="{results>SIMILAR_MOVIE}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column sortProperty="TITLE">
										<Label text="Title"/>
										<table:template>
											<Text text="{results>TITLE}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column sortProperty="RANK" width="10%">
										<Label text="Rank"/>
										<table:template>
											<Text text="{results>RANK}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column sortProperty="results>SCORE" width="10%">
										<Label text="Score"/>
										<table:template>
											<Text text="{path : 'results>SCORE', formatter : '.formatter.formatPercent'}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column width="15%">
										<Label text="Links"/>
										<table:template>
											<HBox>
												<Link text="IMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="http://www.imdb.com/title/tt{results>IMDBID}"/>
												<Link text="TMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="https://www.themoviedb.org/movie/{results>TMDBID}"/>
											</HBox>
										</table:template>
									</table:Column>
								</table:columns>
							</table:Table>
						</Panel>
					</Panel>
				</ScrollContainer>
			</Panel>
		</content>
	</Page>
</mvc:View>
```

Save the file using the ![save](00-save.png) icon from the menu.

Create a new file **`pal_apriori.view.xml`**.

This is the full path of the created file:

```
movielens/html/resources/webapp/view/xsjs/pal_apriori.view.xml
```

Paste the following content:

```xml
<mvc:View xmlns:html="http://www.w3.org/2000/xhtml" xmlns:mvc="sap.ui.core.mvc" xmlns="sap.m" xmlns:form="sap.ui.layout.form"
	xmlns:table="sap.ui.table" xmlns:core="sap.ui.core" xmlns:layout="sap.ui.layout"
	xmlns:custom="http://schemas.sap.com/sapui5/extension/sap.ui.core.CustomData/1" controllerName="movielens.html.controller.xsjs.execute">
	<Page showHeader="false" press="onNavMenuItemPress" icon="sap-icon://menu" id="master">
		<content>
			<Bar>
				<contentMiddle>
					<Title text="Execute The PAL APRIORI Algorithm"/>
				</contentMiddle>
				<contentLeft>
					<Button icon="sap-icon://menu" press="handlePressOpenMenu"/>
				</contentLeft>
			</Bar>
			<Panel expandable="true" expanded="true" headerText="Algorithm Execution Parameters">
				<form:Form editable="true">
					<form:layout>
						<form:ResponsiveGridLayout columnsL="1" columnsM="1"/>
					</form:layout>
					<form:formContainers>
						<form:FormContainer>
							<form:formElements>
								<form:FormElement label="Minimum Support">
									<form:fields>
										<Slider progress="true" value="{config>/default/pal.apriori/MIN_SUPPORT}" step="0.01" min="0" max="1" enableTickmarks="true"
											inputsAsTooltips="true"/>
										<Input type="Number" value="{config>/default/pal.apriori/MIN_SUPPORT}" enabled="false"/>
									</form:fields>
								</form:FormElement>
								<form:FormElement label="Minimum Confidence">
									<form:fields>
										<Slider progress="true" value="{config>/default/pal.apriori/MIN_CONFIDENCE}" step="0.01" min="0" max="1" enableTickmarks="true"
											inputsAsTooltips="true"/>
										<Input type="Number" value="{config>/default/pal.apriori/MIN_CONFIDENCE}" enabled="false"/>
									</form:fields>
								</form:FormElement>
								<form:FormElement label="Minimum Lift">
									<form:fields>
										<Slider progress="true" value="{config>/default/pal.apriori/MIN_LIFT}" step="0.01" min="0" max="1" enableTickmarks="true"
											inputsAsTooltips="true"/>
										<Input type="Number" value="{config>/default/pal.apriori/MIN_LIFT}" enabled="false"/>
									</form:fields>
								</form:FormElement>
								<form:FormElement label="Ubiquitous">
									<form:fields>
										<Slider progress="true" value="{config>/default/pal.apriori/UBIQUITOUS}" step="0.01" min="0" max="1" enableTickmarks="true"
											inputsAsTooltips="true"/>
										<Input type="Number" value="{config>/default/pal.apriori/UBIQUITOUS}" enabled="false"/>
									</form:fields>
								</form:FormElement>
							</form:formElements>
						</form:FormContainer>
					</form:formContainers>
				</form:Form>
				<Button text="Execute" press="onPressExecute" custom:library="pal" custom:algorithm="apriori"/>
			</Panel>
			<Panel expandable="true" headerText="Collaborative Filtering Result">
				<ScrollContainer height="100%" width="100%" horizontal="true" vertical="true" focusable="true">
					<Panel expandable="false" expanded="true" headerText="Parameters">
						<form:Form editable="true">
							<form:layout>
								<form:ResponsiveGridLayout columnsL="1" columnsM="1"/>
							</form:layout>
							<form:formContainers>
								<form:FormContainer>
									<form:formElements>
										<form:FormElement label="User Id">
											<form:fields>
												<Input required="true" width="100%" showSuggestion="true" suggest="onSuggestionSuggest" value="{config>/default/pal.apriori/USERID}"
													custom:type="user" id="user_input" placeholder="Enter a user identifier..."
													suggestionItems="{ path: 'odata>/ratings_user', filters: [ { path: 'USERID', operator: 'EQ', value1: '-1' } ]}">
													<suggestionItems>
														<core:ListItem key="{odata>USERID}" text="{odata>USERID}" additionalText="Rating count : {odata>RATING_COUNT}"/>
													</suggestionItems>
												</Input>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Top N Results">
											<form:fields>
												<Slider progress="true" value="{config>/default/pal.apriori/KEEPTOPN}" step="1" min="5" max="20" enableTickmarks="true"
													inputsAsTooltips="true"/>
												<Input type="Number" value="{config>/default/pal.apriori/KEEPTOPN}" enabled="false"/>
											</form:fields>
										</form:FormElement>
									</form:formElements>
								</form:FormContainer>
							</form:formContainers>
						</form:Form>
						<Button text="Get Results" press="onPressResults" custom:library="pal" custom:algorithm="apriori" custom:resultType="collaborative"/>
					</Panel>
					<Panel expandable="false" visible="{= !!${results>/hasResult} &amp;&amp; ${results>/resultType} === 'collaborative'}" headerText="Results">
						<table:Table selectionMode="None" visibleRowCount="5" enableBusyIndicator="true" refresh="true" rows="{ path: 'results>/items'}">
							<table:columns>
								<table:Column sortProperty="MOVIEID" width="10%">
									<Label text="Movie ID"/>
									<table:template>
										<Text text="{results>MOVIEID}"/>
									</table:template>
								</table:Column>
								<table:Column sortProperty="TITLE">
									<Label text="Title"/>
									<table:template>
										<Text text="{results>TITLE}"/>
									</table:template>
								</table:Column>
								<table:Column sortProperty="RANK" width="10%">
									<Label text="Rank"/>
									<table:template>
										<Text text="{results>RANK}"/>
									</table:template>
								</table:Column>
								<table:Column sortProperty="SCORE" width="10%">
									<Label text="Score"/>
									<table:template>
										<Text text="{path : 'results>SCORE', formatter : '.formatter.formatPercent'}"/>
									</table:template>
								</table:Column>
								<table:Column width="15%">
									<Label text="Links"/>
									<table:template>
										<HBox>
											<Link text="IMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="http://www.imdb.com/title/tt{results>IMDBID}"/>
											<Link text="TMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="https://www.themoviedb.org/movie/{results>TMDBID}"/>
										</HBox>
									</table:template>
								</table:Column>
							</table:columns>
						</table:Table>
					</Panel>
				</ScrollContainer>
			</Panel>
			<Panel expandable="true" headerText="Content-Based Filtering Result">
				<ScrollContainer height="100%" width="100%" horizontal="true" vertical="true" focusable="true">
					<Panel expandable="false" expanded="true" headerText="Parameters">
						<form:Form editable="true">
							<form:layout>
								<form:ResponsiveGridLayout columnsL="1" columnsM="1"/>
							</form:layout>
							<form:formContainers>
								<form:FormContainer>
									<form:formElements>
										<form:FormElement label="Movie Id">
											<form:fields>
												<Input required="true" width="100%" showSuggestion="true" suggest="onSuggestionSuggest" value="{config>/default/pal.apriori/MOVIEID}"
													custom:type="movie" id="movie_input" placeholder="Enter a movie name or identifier ..."
													suggestionItems="{ path: 'odata>/ratings_movie', filters: [ { path: 'MOVIEID', operator: 'EQ', value1: '-1' } ]}">
													<suggestionItems>
														<core:ListItem key="{odata>MOVIEID}" text="{odata>MOVIEID}" additionalText="Title: {odata>TITLE} - Rating count : {odata>RATING_COUNT}"/>
													</suggestionItems>
												</Input>
											</form:fields>
										</form:FormElement>
										<form:FormElement label="Top N Results">
											<form:fields>
												<Slider progress="true" value="{config>/default/pal.apriori/KEEPTOPN}" step="1" min="5" max="20" enableTickmarks="true"
													inputsAsTooltips="true"/>
												<Input type="Number" value="{config>/default/pal.apriori/KEEPTOPN}" enabled="false"/>
											</form:fields>
										</form:FormElement>
									</form:formElements>
								</form:FormContainer>
							</form:formContainers>
						</form:Form>
						<Button text="Get Results" press="onPressResults" custom:library="pal" custom:algorithm="apriori" custom:resultType="contentbased"/>
						<Panel expandable="false" visible="{= !!${results>/hasResult} &amp;&amp; ${results>/resultType} === 'contentbased'}" headerText="Results">
							<table:Table selectionMode="None" visibleRowCount="5" enableBusyIndicator="true" refresh="true" rows="{ path: 'results>/items'}">
								<table:columns>
									<table:Column sortProperty="SIMILAR_MOVIE" width="10%">
										<Label text="Similar Movie ID"/>
										<table:template>
											<Text text="{results>SIMILAR_MOVIE}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column sortProperty="TITLE">
										<Label text="Title"/>
										<table:template>
											<Text text="{results>TITLE}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column sortProperty="RANK" width="10%">
										<Label text="Rank"/>
										<table:template>
											<Text text="{results>RANK}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column sortProperty="results>SCORE" width="10%">
										<Label text="Score"/>
										<table:template>
											<Text text="{path : 'results>SCORE', formatter : '.formatter.formatPercent'}" wrapping="false"/>
										</table:template>
									</table:Column>
									<table:Column width="15%">
										<Label text="Links"/>
										<table:template>
											<HBox>
												<Link text="IMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="http://www.imdb.com/title/tt{results>IMDBID}"/>
												<Link text="TMDb" class="sapUiTinyMarginBeginEnd" target="_blank" href="https://www.themoviedb.org/movie/{results>TMDBID}"/>
											</HBox>
										</table:template>
									</table:Column>
								</table:columns>
							</table:Table>
						</Panel>
					</Panel>
				</ScrollContainer>
			</Panel>
		</content>
	</Page>
</mvc:View>
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the application)]

Select the **`html`** module, then click on the execute icon ![run](00-run.png) from the menu bar.

Once the application is started, the application will open in a new tab/window or you can click on the application URL:

![Web IDE](04-01.png)

This will open a web page with the following content:

![Web IDE](04-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validate your APL Recommendation results)]

Using the menu icon on the top left corner of the panel, select **Execute Algorithms with XSJS > APL Recommendation**.

Set the ***Best Seller Threshold*** and the ***Max Top Nodes*** to 500.

Click on **Execute**. This will run the algorithm.

Expand the ***Collaborative Filtering Result*** panel, and enter ***123*** as ***User Id***.

Set the ***Top N Results*** to 20.

Click on **Get Results**.

Et voilà!

![Applications](05-01.png)

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Validate your PAL APRIORI results)]

Using the menu icon on the top left corner of the panel, select **Execute Algorithms with XSJS > PAL APRIORI**.

Leave the parameters as default.

Click on **Execute**. This will run the algorithm.

Expand the ***Content-based Filtering Result*** panel, and enter ***2*** as ***Movie Id*** (for ***`Jumanji`***).

Set the ***Top N Results*** to 20.

Click on **Get Results**.

Et voilà!

![Applications](06-01.png)

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Commit your changes)]

On the icon bar located on the right side of the Web IDE, click on the **Git Pane** icon ![Web IDE](00-webide-git.png).

Click on **Stage All**, enter a commit comment, then click on **Commit and Push > origin master**.

[DONE]
[ACCORDION-END]
