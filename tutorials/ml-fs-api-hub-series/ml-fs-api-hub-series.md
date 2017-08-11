---
title: SAP Leonardo Machine Learning for series data on the SAP API Business Hub
description: Discover the SAP Leonardo Machine Learning Functional Service consuming series content on the SAP API Business Hub
primary_tag: topic>machine-learning
tags: [tutorial>beginner, topic>cloud, topic>machine-learning,  products>sap-api-management, products>sap-cloud-platform]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - [Sign up for an free trial account on the SAP Cloud Platform](http://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
 - Select a tutorial group from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)

## Details
### You will learn  
In this tutorial, you will learn where to find and test the **SAP Leonardo Machine Learning Functional Services** published on the SAP API Business Hub that consumes series data content.

The **Time Series Change Point Detection API** Machine Learning Functional Services will be used as an example to demonstrate how to consume series data content, but you will be able to transpose this tutorial to other services which also consume content like :

 - the **Time Series Forecast API**

### Time to Complete
**10 Min**

---

[ACCORDION-BEGIN [Step 1: ](Search the SAP API Business Hub)]

In order to consume the **Time Series Change Point Detection API** SAP Leonardo Machine Learning Functional Services, you will first need to get the service URI, request and response parameters.

Go to [https://api.sap.com/](https://api.sap.com) and click on the **Browse** tile.

![SAP API Business Hub](01.png)

Then you will be able to search for the **SAP Leonardo Machine Learning - Functional Services**, then click on the package found.

![SAP API Business Hub](02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Analyze the service)]

Click on **Artifacts**, then click on the **Time Series Change Point Detection API**.

![SAP API Business Hub](03.png)

As you can notice the **Time Series Change Point Detection API** has only one resource (or service): `/inference_sync`.

Now click on the `/inference_sync` link to expand the section.

> **Note**: the term *inference* refers to the application phase (scoring) an existing model (as opposed to the training or inception phase) and *sync* for synchronous.

![SAP API Business Hub](04.png)

As stated in the description, the service accepts either an archive file with a zip/tar extensions containing multiple text files, a single text file or a list of text files as input and returns a detected topic list with the associated keywords and scores (confidence).

The supported text file formats is plain text only.

The input text, file, files or archive file will be sent as a `FormData` query parameter in the service request.

A series of options are also required for the following parameters:

  - `separator`: Values separator (the default value is the comma: ",")
  - `series_separator`: Series separator for multivariate time series. (the default value is the colon: ":")

And the response model schema is the following:

```JSON
{
  "_id": "string",
  "error": "string",
  "request": "string",
  "response": "string",
  "responsetime": 0,
  "status": "QUEUED",
  "tenantName": "string",
  "error_description": "string"
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test the service)]

Now, scroll down a bit further and click on ![login-to-try-out](00-login-to-try-out.png). You will be prompted for your SAP Cloud Platform credentials.

Scroll up a little for the **Query Parameters** section.

In the **options** field enter the following:

```JSON
{"separator" : "|"}
```

In the **texts** field enter the following series data (a Cosine series):

```JSON
93969.262|76604.444|50000.000|17364.818|-17364.818|-50000.000|-76604.444|-93969.262|-100000.000|-93969.262|-76604.444|-50000.000|-17364.818|17364.818|50000.000|76604.444|93969.262|100000.000
```

![SAP API Business Hub](05.png)

Now scroll down a bit further and click on ![try-out](00-try-out.png).

Please take a note of the **Request URL** which we will use later:

```
https://sandbox.api.sap.com/ml/changepointdetection/inference_sync
```

In the **Response Body**, you will get the input data along with the probability of a change point for the corresponding value in the given time series.

You will notice that we provided 20 input values, and received only 19. This is because the probability is computed for each data point interval.

For example, the probability that a change point has been detected between the first and second element of the series (93969.26208 & 76604.44431) is 6.8%.

```JSON
{
  "_id": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  "processed_time": "Wed, 02 Aug 2017 08:41:04 GMT",
  "request": {
    "files": null,
    "options": {
      "separator": "|",
      "series_separator": "#"
    },
    "tenantName": "imgclassif-tech-user",
    "texts": [
      "93969.26208|76604.44431|50000|17364.81777|-17364.81777|-50000|-76604.44431|-93969.26208|-100000|-93969.26208|-76604.44431|-50000|-17364.81777|17364.81777|50000|76604.44431|93969.26208|100000"
    ]
  },
  "response": [
    "0.068,0.190,0.318,0.450,0.418,0.202,0.106,0.066,0.056,0.074,0.192,0.378,0.484,0.354,0.230,0.114,0.056,nan"
  ],
  "status": "DONE",
  "tenantName": "imgclassif-tech-user"
}
```

> **Note**: The result you will obtain might be slightly different form the one displayed above.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test the service for multivariate series)]

In the context of this service, the term "multivariate" represent the ability to execute the service for multiple series of data in one request.

In the **options** field enter the following:

```JSON
{"separator" : "|" , "series_separator" : "#"}
```

In the **texts** field enter the following series data (a Cosine series and a Sinus series):

```JSON
93969.262|76604.444|50000.000|17364.818|-17364.818|-50000.000|-76604.444|-93969.262|-100000.000|-93969.262|-76604.444|-50000.000|-17364.818|17364.818|50000.000|76604.444|93969.262|100000.000#34202.014|64278.761|86602.540|98480.775|98480.775|86602.540|64278.761|34202.014|0.000|-34202.014|-64278.761|-86602.540|-98480.775|-98480.775|-86602.540|-64278.761|-34202.014|0.000
```

> **Note**: each series must have the same length.


In the **Response Body**, you will get the input data along with the probability of a change point for the corresponding value in the given time series.

You will notice that we provided 2 series 20 input values, and received only 19. This is because the probability is computed between each series data point.

For example, the probability that a change point has been detected between the first element of both series (93969.26208 & 34202.014) is 5.8%.

```JSON
{
  "_id": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  "processed_time": "Wed, 02 Aug 2017 08:34:45 GMT",
  "request": {
    "files": null,
    "options": {
      "separator": "|",
      "series_separator": "#"
    },
    "tenantName": "imgclassif-tech-user",
    "texts": [
      "93969.262|76604.444|50000.000|17364.818|-17364.818|-50000.000|-76604.444|-93969.262|-100000.000|-93969.262|-76604.444|-50000.000|-17364.818|17364.818|50000.000|76604.444|93969.262|100000.000#34202.014|64278.761|86602.540|98480.775|98480.775|86602.540|64278.761|34202.014|0.000|-34202.014|-64278.761|-86602.540|-98480.775|-98480.775|-86602.540|-64278.761|-34202.014|0.000"
    ]
  },
  "response": [
    "0.058,0.220,0.316,0.396,0.242,0.274,0.386,0.266,0.278,0.346,0.326,0.312,0.244,0.312,0.462,0.190,0.034,nan"
  ],
  "status": "DONE",
  "tenantName": "imgclassif-tech-user"
}
```

[DONE]
[ACCORDION-END]

---

### Optional

[ACCORDION-BEGIN [Step 5: ](Investigate similar services)]

You can also try the following Machine Learning Functional Services consuming text content:

  - the **Time Series Forecast API**

For more information, you can also check the online [SAP Leonardo Machine Learning Foundation documentation](https://help.sap.com/viewer/product/SAP_LEONARDO_MACHINE_LEARNING_FOUNDATION/1.0/en-US)

[DONE]
[ACCORDION-END]

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
