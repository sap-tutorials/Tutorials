---
title: Get OAuth Access Token for Invoice Object Recommendation via Web Browser
description: Retrieve your OAuth access token, which will grant you access to the service APIs.
auto_validation: true
time: 5
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-business-technology-platform, products>sap-ai-business-services, products>invoice-object-recommendation]
primary_tag: topic>machine-learning
---

## Details
### You will learn
  - How to use your service key values to get an OAuth access token via any web browser

---

[ACCORDION-BEGIN [Step 1: ](Get OAuth access token details)]

To get your OAuth access token you will need the values listed below from the service key you created in the previous tutorial: [Create Service Instance for Invoice Object Recommendation](cp-aibus-ior-service-instance).

!![IOR Service Keys](png-files/service-key-details.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get OAuth access token)]

1. Add **`/oauth/token?grant_type=client_credentials`** to the `url` value (from inside the `uaa` section of the service key), paste it in any web browser and choose **Enter**.

2. Enter the **`clientid`** value from your service key as **Username**.

3. Enter the **`clientsecret`** value from your service key as **Password**.

4. Click **Sign in** or **OK** (depending on the web browser you work with).

!![IOR TOKEN](png-files/web-browser.png)

You should receive a response like the following:

```JSON
{
    "access_token": "<< your access token >>",
    "token_type": "bearer",
    "expires_in": 43199,
    "scope": "uaa.resource ior-trial!b51213.default",
    "jti": "feb1f4304de044a3831324089f1bc51a"
}
```

You have successfully generated your OAuth access token for the service and you are now all set to [Use Machine Learning to Recommend G/L Accounts for Incoming Invoices](cp-aibus-ior-swagger-ui).

[DONE]
[ACCORDION-END]
