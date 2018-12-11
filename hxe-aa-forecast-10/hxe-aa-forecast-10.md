---
title: Expose procedures as XSJS and views as XS OData (Forecast App)
description: Understand and implement some of the options available with SAP HANA to expose your algorithm procedures and views
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 15
---

## Details
### You will learn
 - Create a Node.js Module
 - Add a dependency to the SAP HANA Database Module
 - Create and test a XS OData service
 - Create and test a XSJS service

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

[ACCORDION-BEGIN [Step 1: ](Create a Node.js Module)]

In the left panel, right click on the `forecast` project, then select **New > Node.js Module**.

![Web IDE](02-01.png)

Set the name to **`js`** and click on **Next**.

Check **Enable XSJS support**.

Click on **Finish**.

![Web IDE](02-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Add Module dependencies)]

In order to consume the data from the SAP HANA Database Module created previously, you will need to add a dependency from the Node.js Module to the SAP HANA Database Module.

Edit the **`mta.yaml`** file located in the `forecast` project folder.

![Web IDE](03-01.png)

Select the **`js`** module.

![Web IDE](03-02.png)

Under the **Requires** section, add your ***SAP HANA Database Module*** resource (most likely named **`hdi_db`**).

![Web IDE](03-03.png)

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the folder structure)]

Before creating the OData and XSJS services, you need to create the required directory structure.

Expand the **`forecast/js/lib`** folder.

Create the following directory structure:

```
|-- forecast/js/lib
    |-- xsodata
    |-- xsjs
        |-- apl
        |-- pal
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create SAP HANA XS OData services)]

Now, you can expose the results and summary views using XS OData services that will consumed by your SAPUI5 applications.

In the left side panel, expand the **`forecast/js/lib/xsodata`** tree node.

Right click on the **`xsodata`** folder node from the tree, and select **New > File**.

Enter **`data.xsodata`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/js/lib/xsodata/data.xsodata
```

Paste the following content in the console.

```JavaScript
service {
  "aa.forecast.db.data::CashFlows"                 as "apl_CashFlows";
  "aa.forecast.db.data::CashFlows_extrapredictors" as "apl_CashFlows_extrapredictors";
  "aa.forecast.db.data::Ozone"                     as "apl_Ozone";
  "aa.forecast.db.data::Lag1AndCycles"             as "apl_Lag1AndCycles";
  "aa.forecast.db.data::Lag1AndCyclesAndWn"        as "apl_Lag1AndCyclesAndWn";
  "aa.forecast.db.data::TrendAndCyclic"            as "apl_TrendAndCyclic";
  "aa.forecast.db.data::TrendAndCyclicAnd_4Wn"     as "apl_TrendAndCyclicAnd_4Wn";
  "aa.forecast.db.data::TrendAndCyclicAndWn"       as "apl_TrendAndCyclicAndWn";  

  "aa.forecast.db.algorithms.pal.views::CashFlows"              as "pal_CashFlows"              key ("signal_time");
  "aa.forecast.db.algorithms.pal.views::Ozone"                  as "pal_Ozone"                  key ("signal_time");
  "aa.forecast.db.algorithms.pal.views::Lag1AndCycles"          as "pal_Lag1AndCycles"          key ("signal_time");
  "aa.forecast.db.algorithms.pal.views::Lag1AndCyclesAndWn"     as "pal_Lag1AndCyclesAndWn"     key ("signal_time");
  "aa.forecast.db.algorithms.pal.views::TrendAndCyclic"         as "pal_TrendAndCyclic"         key ("signal_time");
  "aa.forecast.db.algorithms.pal.views::TrendAndCyclicAnd_4Wn"  as "pal_TrendAndCyclicAnd_4Wn"  key ("signal_time");
  "aa.forecast.db.algorithms.pal.views::TrendAndCyclicAndWn"    as "pal_TrendAndCyclicAndWn"    key ("signal_time");  
}
settings {
  support null;
}
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create SAP HANA XSJS services for APL)]

Now, you can expose the XSJS service that will let you run and retrieve the result for the APL Forecast algorithm using the procedure created previously.

In short, the service implements a POST method that accepts a JSON stream with a set of parameters.

Then the XSJS Connection API is used to execute the stored procedures previously created.

If successful, a HTTP OK return core is sent back with no body else the error message is returned.

In the left side panel, expand the **`forecast/js/lib/xsjs/apl`** tree node.

Right click on the **`apl`** folder node from the tree, and select **New > File**.

Enter **`forecast.xsjs`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/js/lib/xsjs/apl/forecast.xsjs
```

Paste the following content in the console.

```JavaScript
/*eslint no-console: 0, no-unused-vars: 0, dot-notation: 0*/
/*eslint-env node, es6 */
function close(o) {
    try {
        if (o) {
            o.close();
        }
    } catch (e) { /* do nothing */ }
}

function methodNotAllowed() {
    $.response.status = $.net.http.METHOD_NOT_ALLOWED;
    $.response.setBody(JSON.stringify({
        message: "Method Not Allowed"
    }));
}

function doPost() {
    var connection = null;
    var preparedStatement = null;
    try {
        // Build the SQL Query with the parameters
        var params = {};
        if (typeof $.request.body !== "undefined") {
            // Get the request body
            var requestBody = JSON.parse($.request.body.asString());
            if (typeof requestBody.DATASETNAME !== "undefined") {
                params.DATASETNAME = requestBody.DATASETNAME;
            }
            if (typeof requestBody.HORIZON !== "undefined") {
                params.HORIZON = requestBody.HORIZON;
            }
            if (typeof requestBody.LASTTRAININGTIMEPOINT !== "undefined") {
                params.LASTTRAININGTIMEPOINT = requestBody.LASTTRAININGTIMEPOINT;
            }
            if (typeof requestBody.CUTTINGSTRATEGY !== "undefined") {
                params.CUTTINGSTRATEGY = requestBody.CUTTINGSTRATEGY;
            }
            if (typeof requestBody.FORECASTMETHOD !== "undefined") {
                params.FORECASTMETHOD = requestBody.FORECASTMETHOD;
            }
            if (typeof requestBody.FORECASTFALLBACKMETHOD !== "undefined") {
                params.FORECASTFALLBACKMETHOD = requestBody.FORECASTFALLBACKMETHOD;
            }
            if (typeof requestBody.SMOOTHINGCYCLELENGTH !== "undefined") {
                params.SMOOTHINGCYCLELENGTH = requestBody.SMOOTHINGCYCLELENGTH;
            }
            if (typeof requestBody.FORCEPOSITIVEFORECAST !== "undefined") {
                params.FORCEPOSITIVEFORECAST = requestBody.FORCEPOSITIVEFORECAST;
            }
            if (typeof requestBody.FORECASTMAXCYCLICS !== "undefined") {
                params.FORECASTMAXCYCLICS = requestBody.FORECASTMAXCYCLICS;
            }
            if (typeof requestBody.FORECASTMAXLAGS !== "undefined") {
                params.FORECASTMAXLAGS = requestBody.FORECASTMAXLAGS;
            }
        }

        var start = Date.now();
        connection = $.hdb.getConnection();
        var algorithm = connection.loadProcedure(null, "aa.forecast.db.algorithms.apl.procedures::forecast");
        var results = algorithm(params);
        $.response.status = $.net.http.OK;
        $.response.setBody(JSON.stringify({
            results: results,
            message: "Process completed in : " + (Date.now() - start) + " ms"
        }));
    } catch (e) {
        $.response.setBody(JSON.stringify({
            message: e.message
        }));
        $.response.status = $.net.http.BAD_REQUEST;
    } finally {
        close(preparedStatement);
        close(connection);
    }
}
$.response.contentType = "application/json; charset=utf-16le";
switch ($.request.method) {
    case $.net.http.POST:
        doPost();
        break;
    default:
        methodNotAllowed();
        break;
}
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create SAP HANA XSJS services for PAL)]

Now, you can expose the XSJS service that will let you run and retrieve the result for the PAL APRIORI algorithm using the procedure created previously.

In the left side panel, expand the **`forecast/js/lib/xsjs/pal`** tree node.

#### Auto ARIMA

Right click on the **`pal`** folder node from the tree, and select **New > File**.

Enter **`auto_arima.xsjs`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/js/lib/xsjs/pal/auto_arima.xsjs
```

Paste the following content in the console.

```JavaScript
/*eslint no-console: 0, no-unused-vars: 0, dot-notation: 0*/
/*eslint-env node, es6 */
function close(o) {
	try {
		if (o) {
			o.close();
		}
	} catch (e) { /* do nothing */ }
}

function methodNotAllowed() {
	$.response.status = $.net.http.METHOD_NOT_ALLOWED;
	$.response.setBody(JSON.stringify({
		message: "Method Not Allowed"
	}));
}

function doPost() {
	var connection = null;
	var preparedStatement = null;
	try {
		// Build the SQL Query with the parameters
		var params = {};
		if (typeof $.request.body !== "undefined") {
			// Get the request body
			var requestBody = JSON.parse($.request.body.asString());
			if (typeof requestBody.DATASETNAME !== "undefined") {
				params.DATASETNAME = requestBody.DATASETNAME;
			}
			if (typeof requestBody.SEARCHSTRATEGY !== "undefined") {
				params.SEARCHSTRATEGY = requestBody.SEARCHSTRATEGY;
			}
			if (typeof requestBody.SEASONALPERIOD !== "undefined") {
				params.SEASONALPERIOD = requestBody.SEASONALPERIOD;
			}
			if (typeof requestBody.FORECASTLENGTH !== "undefined") {
				params.FORECASTLENGTH = requestBody.FORECASTLENGTH;
			}
		}

		var start = Date.now();
		connection = $.hdb.getConnection();
		var algorithm = connection.loadProcedure(null, "aa.forecast.db.algorithms.pal.procedures::auto_arima");
		var results = algorithm(params);
		$.response.status = $.net.http.OK;
		$.response.setBody(JSON.stringify({
			results: results,
			message: "Process completed in : " + (Date.now() - start) + " ms"
		}));
	} catch (e) {
		$.response.setBody(JSON.stringify({
			message: e.message
		}));
		$.response.status = $.net.http.BAD_REQUEST;
	} finally {
		close(preparedStatement);
		close(connection);
	}
}
$.response.contentType = "application/json; charset=utf-16le";
switch ($.request.method) {
	case $.net.http.POST:
		doPost();
		break;
	default:
		methodNotAllowed();
		break;
}
```

Save the file using the ![save](00-save.png) icon from the menu.

#### Auto Smoothing

Right click on the **`pal`** folder node from the tree, and select **New > File**.

Enter **`auto_smoothing.xsjs`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/js/lib/xsjs/pal/auto_smoothing.xsjs
```

Paste the following content in the console.

```JavaScript
/*eslint no-console: 0, no-unused-vars: 0, dot-notation: 0*/
/*eslint-env node, es6 */
function close(o) {
	try {
		if (o) {
			o.close();
		}
	} catch (e) { /* do nothing */ }
}

function methodNotAllowed() {
	$.response.status = $.net.http.METHOD_NOT_ALLOWED;
	$.response.setBody(JSON.stringify({
		message: "Method Not Allowed"
	}));
}

function doPost() {
	var connection = null;
	var preparedStatement = null;
	try {
		// Build the SQL Query with the parameters
		var params = {};
		if (typeof $.request.body !== "undefined") {
			// Get the request body
			var requestBody = JSON.parse($.request.body.asString());
			if (typeof requestBody.DATASETNAME !== "undefined") {
				params.DATASETNAME = requestBody.DATASETNAME;
			}
			if (typeof requestBody.FORECASTNUM !== "undefined") {
				params.FORECASTNUM = requestBody.FORECASTNUM;
			}
			if (typeof requestBody.ACCURACYMEASURE !== "undefined") {
				params.ACCURACYMEASURE = requestBody.ACCURACYMEASURE;
			}
			if (typeof requestBody.TRAININGRATIO !== "undefined") {
				params.TRAININGRATIO = requestBody.TRAININGRATIO;
			}
			if (typeof requestBody.SEASONALITYCRITERION !== "undefined") {
				params.SEASONALITYCRITERION = requestBody.SEASONALITYCRITERION;
			}
		}

		var start = Date.now();
		connection = $.hdb.getConnection();
		var algorithm = connection.loadProcedure(null, "aa.forecast.db.algorithms.pal.procedures::auto_smoothing");
		var results = algorithm(params);
		$.response.status = $.net.http.OK;
		$.response.setBody(JSON.stringify({
			results: results,
			message: "Process completed in : " + (Date.now() - start) + " ms"
		}));
	} catch (e) {
		$.response.setBody(JSON.stringify({
			message: e.message
		}));
		$.response.status = $.net.http.BAD_REQUEST;
	} finally {
		close(preparedStatement);
		close(connection);
	}
}
$.response.contentType = "application/json; charset=utf-16le";
switch ($.request.method) {
	case $.net.http.POST:
		doPost();
		break;
	default:
		methodNotAllowed();
		break;
}
```

Save the file using the ![save](00-save.png) icon from the menu.

#### Seasonality Test

Right click on the **`pal`** folder node from the tree, and select **New > File**.

Enter **`seasonality_test.xsjs`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/js/lib/xsjs/pal/seasonality_test.xsjs
```

Paste the following content in the console.

```JavaScript
/*eslint no-console: 0, no-unused-vars: 0, dot-notation: 0*/
/*eslint-env node, es6 */
function close(o) {
	try {
		if (o) {
			o.close();
		}
	} catch (e) { /* do nothing */ }
}

function methodNotAllowed() {
	$.response.status = $.net.http.METHOD_NOT_ALLOWED;
	$.response.setBody(JSON.stringify({
		message: "Method Not Allowed"
	}));
}

function doPost() {
	var connection = null;
	var preparedStatement = null;
	try {
		// Build the SQL Query with the parameters
		var params = {};
		if (typeof $.request.body !== "undefined") {
			// Get the request body
			var requestBody = JSON.parse($.request.body.asString());
			if (typeof requestBody.DATASETNAME !== "undefined") {
				params.DATASETNAME = requestBody.DATASETNAME;
			}
			if (typeof requestBody.ALPHA !== "undefined") {
				params.ALPHA = requestBody.ALPHA;
			}
		}

		var start = Date.now();
		connection = $.hdb.getConnection();
		var algorithm = connection.loadProcedure(null, "aa.forecast.db.algorithms.pal.procedures::seasonality_test");
		var results = algorithm(params);
		$.response.status = $.net.http.OK;
		$.response.setBody(JSON.stringify({
			results: results,
			message: "Process completed in : " + (Date.now() - start) + " ms"
		}));
	} catch (e) {
		$.response.setBody(JSON.stringify({
			message: e.message
		}));
		$.response.status = $.net.http.BAD_REQUEST;
	} finally {
		close(preparedStatement);
		close(connection);
	}
}
$.response.contentType = "application/json; charset=utf-16le";
switch ($.request.method) {
	case $.net.http.POST:
		doPost();
		break;
	default:
		methodNotAllowed();
		break;
}
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Build and Start the Node.js Module)]

Right click on the **`js`** folder and select **Build**.

![Web IDE](08-01.png)

The console should display at the end the following message:

```
(Builder) Build of /forecast/js completed successfully.
```

Select the **`js`** module,  then click on the execute icon ![run](00-run.png) from the menu bar.

Once the application is started, you can click on the application URL:

![Web IDE](08-02.png)

This should open the ***`index.xsjs`*** page.

![Web IDE](08-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Test your XS OData service)]

Now, let's test your **XS OData** service.

From the ***`index.xsjs`*** page, replace ***`index.xsjs`***  from the URL by:

```HTML
xsodata/data.xsodata?$format=json
```

You should now get the list of XS OData services available.

![Web IDE](09-01.png)

Replace ***`xsodata/data.xsodata?$format=json`***  from the URL by:

```url
apl_CashFlows_extrapredictors?$inlinecount=allpages&$filter=ReverseWorkingDaysIndices eq 21&$format=json
```

You should get the rows where the ***`ReverseWorkingDaysIndices`*** is equal to 21 including the row count.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Test your XSJS service )]

Open your preferred REST client, like **`cURL`**, and if don't have one yet you can [install and use Postman](https://www.sap.com/developer/tutorials/api-tools-postman-install.html).

#### Using Postman

Open a new tab, and set the following information:

Name           | Value
:------------- | :--------------
Request Method | POST
URL            | `https://hxehost:51xxx/xsjs/apl/forecast.xsjs`

Select the **Body** tab, enable the **raw** mode, select **`JSON (application/json)`** in the drop down (instead of ***Text***), then past the following content:

```JSON
{
  "DATASET" : "CashFlows",
  "HORIZON" : "21",
  "LASTTRAININGTIMEPOINT" : "2001-12-28"
}
```

Click on **Send**.

![Postman](10-01.png)

#### Using `cURL`

With `cURL`, you can use the following command:

```shell
curl --request POST \
  --url https://hxehost:51047/xsjs/apl/forecast.xsjs \
  --header 'cache-control: no-cache' \
  --header 'content-type: application/json' \
  --data '{DATASETNAME" : "CashFlows", "HORIZON" : "21", "LASTTRAININGTIMEPOINT" : "2001-12-28"}'
```

> ### **Note:** Make sure to adjust the host and port number used in the URL to your local environment.
&nbsp;
> You might also need to add the following parameters when using `cURL`  to ignore certificate signature origin and proxy:
> ```
--insecure --noproxy "*"
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 1: ](Commit your changes)]

On the icon bar located on the right side of the Web IDE, click on the **Git Pane** icon ![Web IDE](00-webide-git.png).

Click on **Stage All**, enter a commit comment, then click on **Commit and Push > origin master**.

[DONE]
[ACCORDION-END]
