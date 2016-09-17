---
title: SAP HCP predictive services, Test the "Forecast" HCP predictive service from a REST client
description: Using a REST client, you will test the "Forecast" HCP predictive service from a REST client
tags: [ tutorial>beginner, products>sap-hana, products>sap-hana-cloud-platform, products>sap-hana-cloud-platform-predictive-services, topic>predictive ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Test the "Data Set" HCP predictive services using a REST client](http://go.sap.com/developer/tutorials/hcpps-rest-ps-dataset.html)

## Next Steps
  - [Configure a SAPUI5 application to interact with the HCP predictive](http://go.sap.com/developer/tutorials/hcpps-sapui5-configure-application.html)

## Details
### You will learn
 - How to use the "Forecast" HCP predictive services from a REST Client in both synchronous and asynchronous mode.

### Time to Complete
  **10 minutes**

---

In order to ease the readability of this tutorial, we have used tokens to replace long URLs.
Therefore you can replace any occurrence of the token by the value listed above.
Make sure you update the URL with your HCP Account identifier.

Token                               | Value
----------------------------------- | -------------
<code><b>&lt;C4PA URL&gt;</b></code> | `http://aac4paservices<`<code><b>HCP Identifier</b></code>`>trial.hanatrial.ondemand.com/com.sap.aa.c4pa.services`
---

### Synchronous mode

---

With the synchronous mode, the HCP predictive service will be waiting until the operation completes to return the response.

1. Open a new tab in ***Postman***.

    Fill in the following information

    Field Name     | Value
    -------------- | --------------
    Request Type   | `POST`
    URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/forecast/sync`

    ![Postman URL](1.png)

    Select the **Authorization** tab

    Field Name     | Value
    :------------- | :-------------
    Type           | `Basic Auth`
    Username       | your ***HCP Account*** login (usually the email address used to register your ***HCP*** account)
    Password*      | your ***HCP Account*** password

    ![Postman URL](2.png)

    Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following entries

    ```JSON
    {
      "datasetID": 3,
      "targetColumn": "Cash",
      "dateColumn": "Date",
      "numberOfForecasts": 10,  
      "referenceDate" : "2001-12-01"
    }
    ```
    > Make sure the `datasetID` is correct. To get the list of valid identifier, you can run the last step of the previous tutorial.

    ![Postman URL](3.png)

    Click on **Send**

    Congratulations! You have just run a forecast of the `Cash` column and requested the next 10 values after the reference date (2001-12-01).

    Here is the result:

    ```JSON
    {
      "forecasts": [
        {
          "date": "2001-12-03",
          "errorBarHigherBound": 7176.2151730942105,
          "errorBarLowerBound": 511.95446775673827,
          "forecastValue": 3844.0848204254744
        },
        {
          "date": "2001-12-04",
          "errorBarHigherBound": 7678.334358158509,
          "errorBarLowerBound": 1014.0736528210364,
          "forecastValue": 4346.2040054897725
        },
        {
          "date": "2001-12-05",
          "errorBarHigherBound": 7678.734384482609,
          "errorBarLowerBound": 1014.4736791451369,
          "forecastValue": 4346.604031813873
        },
        {
          "date": "2001-12-06",
          "errorBarHigherBound": 7679.134410806742,
          "errorBarLowerBound": 1014.8737054692701,
          "forecastValue": 4347.004058138006
        },
        {
          "date": "2001-12-07",
          "errorBarHigherBound": 7679.534437130843,
          "errorBarLowerBound": 1015.2737317933706,
          "forecastValue": 4347.404084462107
        },
        {
          "date": "2001-12-10",
          "errorBarHigherBound": 7167.880957205314,
          "errorBarLowerBound": 503.62025186784194,
          "forecastValue": 3835.750604536578
        },
        {
          "date": "2001-12-11",
          "errorBarHigherBound": 7681.134542427244,
          "errorBarLowerBound": 1016.8738370897718,
          "forecastValue": 4349.004189758508
        },
        {
          "date": "2001-12-12",
          "errorBarHigherBound": 7681.534568751377,
          "errorBarLowerBound": 1017.273863413905,
          "forecastValue": 4349.404216082641
        },
        {
          "date": "2001-12-13",
          "errorBarHigherBound": 7681.934595075478,
          "errorBarLowerBound": 1017.6738897380055,
          "forecastValue": 4349.804242406742
        },
        {
          "date": "2001-12-14",
          "errorBarHigherBound": 7682.334621399578,
          "errorBarLowerBound": 1018.073916062106,
          "forecastValue": 4350.204268730842
        }
      ],
      "modelPerformance": {
        "mape": 0.20964290496118917,
        "qualityRating": 4
      },
      "parameters": {
        "datasetID": 3,
        "dateColumn": "Date",
        "numberOfForecasts": 10,
        "referenceDate": "2001-12-01",
        "targetColumn": "Cash"
      }
    }
    ```
---

### Asynchronous mode

---

With the asynchronous mode, the process is split across 3 service calls, where the first will post the request, the second will be used to check the status and the third will get the response.

1. Open a new tab in ***Postman***.

    Fill in the following information

    Field Name     | Value
    -------------- | --------------
    Request Type   | `POST`
    URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/forecast`

    Select the **Authorization** tab and fill in the same details as in the previous call.

    Select the **Body** tab, enable the **raw** mode and select `JSON (application/json)` in the drop down, then add the following entries

    ```JSON
    {
      "datasetID": 3,
      "targetColumn": "Cash",
      "dateColumn": "Date",
      "numberOfForecasts": 10,  
      "referenceDate" : "2001-12-01"
    }
    ```
    > Make sure the `datasetID` is correct. To get the list of valid identifier, you can run the last step of the previous tutorial.

    Click on **Send**

    Here is the result:

    ```JSON
    {
      "ID": 1,
      "status": "PROCESSING",
      "type": "forecasts",
      "input": "{
         \"datasetID\":3,
         \"dateColumn\":\"Date\",
         \"numberOfForecasts\":10,
         \"referenceDate\":\"2001-12-01\",
         \"targetColumn\":\"Cash\"
      }"
    }
    ```

    Now, you can use the `ID` value to reference the registered forecasting job with other HCP predictive services calls.

1. Open a new tab in ***Postman***.

    Fill in the following information

    Field Name     | Value
    -------------- | --------------
    Request Type   | `GET`
    URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/forecast/1/status`

    Select the **Authorization** tab and fill in the same details as in the previous call.

    Click on **Send**

    Here is the result:

    ```JSON
    {
      "ID": 1,
      "status": "SUCCESSFUL",
      "type": "forecasts"
    }
    ```

    The forecasting call was successful, now let's get the result:

1. Open a new tab in ***Postman***.

    Fill in the following information

    Field Name     | Value
    -------------- | --------------
    Request Type   | `GET`
    URL            | `<`<code><b>C4PA URL</b></code>`>/api/analytics/forecast/1`

    Select the **Authorization** tab and fill in the same details as in the previous call.

    Click on **Send**

    You should obtain the same output as with the Synchronous mode.

### Optional
For more details on the HCP predictive services, you can check the following URL:
  - `<`<code><b>C4PA URL</b></code>`>/raml/index.html?raml=../aa-cloud-services.raml`

## Next Steps
  - [Configure a SAPUI5 application to interact with the HCP predictive](http://go.sap.com/developer/tutorials/hcpps-sapui5-configure-application.html)

