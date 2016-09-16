---
title: Create an SAP HANA Cloud Platform destination pointing to an SAP Gateway
description: Create a new destination pointing to the SAP Gateway Demo System
tags: [  tutorial>beginner, topic>cloud, products>sap-gateway, products>sap-hana-cloud-platform ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Create an account on the Gateway Demo system](http://go.sap.com/developer/tutorials/gateway-demo-signup.html)

## Next Steps
 - [Build an SAP Fiori UX app with SAP Gateway data](http://go.sap.com/developer/tutorials/teched-2016-4.html)

## Details
### You will learn  
You will create a destination (a server end-point where HCP can send or receive data) that points to an SAP Gateway OData service.

### Time to Complete
**< 5 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Log into your HCP account)]
Log into [SAP HANA Cloud Platform](https://account.hanatrial.ondemand.com/) and click on your username to open the cockpit view.

![Click on user name](te-2016-3-01.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Go to Destinations)]

In the left-hand navigation bar, click on **Connectivity**, then **Destinations** and finally the **New Destination** link.

![open destinations](te-2016-3-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enter Destination details)]

Enter the field values AND the **Additional Properties** below then click **Save**. Leave the **Use default JDK truststore** checked.

Field Name     | Value
:------------- | :-------------
Name           | `SAP_Gateway`
Type           | `HTTP`
Description    | `SAP Gateway ES4`
URL            | `https://sapes4.sapdevcenter.com`
Proxy Type     | `Internet`
Authentication | `BasicAuthentication`
User           | `<Your ES4 Gateway user> (e.g. p123456789)`
Password       | `<Your ES4 Gateway password>`

Add three Additional Properties fields by clicking on the **New Property** button once for each property.


Field Name       | Value
:--------------- | :-------------
`WebIDEEnabled`  | `true`
`WebIDESystem`   | `gateway`
`WebIDEUsage`    | `odata_gen`

![creating destination](te-2016-3-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test the new Destinations)]

After the destination is saved, you can click the Check Connection button to test that SAP Gateway is reachable. 

![Check connection](te-2016-3-04.png)

[DONE]
[ACCORDION-END]


## Next Steps
 - [Build an SAP Fiori UX app with SAP Gateway data](http://go.sap.com/developer/tutorials/teched-2016-4.html)
