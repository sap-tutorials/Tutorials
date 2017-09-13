---
title: Enable the Multi Cloud Web IDE
description: Learn how to enable the multi cloud version of Web IDE
primary_tag: products>sap-web-ide
tags: [  tutorial>beginner, products>sap-cloud-platform, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Add API Business Hub API to a UI5 Application](https://www.sap.com/developer/tutorials/hcp-abh-api-ui5-app.html)


## Next Steps
 - [Using SAP API Business Hub with Web IDE](https://www.sap.com/developer/tutorials/webide-api-hub.html)

## Details
### You will learn  
Find all the latest features available from Web IDE in the latest, full-stack enabled version.

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Open SAP Cloud Platform services)]
Open your SAP Cloud Platform account. Make sure you are in a `Neo` Environment, not one of the new Cloud Foundry environments. `Neo` is the classic, proprietary SAP Cloud Platform.

Go to **Services**

![services location on SAP CP Home page](1.png)

![services page on SAP CP](2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Find the full-stack Web IDE)]
On the Service catalog, search for **Web IDE**.

![search on SCP services page](3.png)

Select the **SAP Web IDE for Full-Stack Development** service.

![sap web ide full-stack version](4.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Enable the service)]
Once the service page loads, **Enable** the service. This may take a few minutes.

![enable feature button for web ide](5.png)

Wait for the status to change to the green, _Enabled_ status.

![green enabled status icon](5b.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open the Web IDE)]
Once the service is enabled, click the **Go to Service** link. This will open the new, full-stack Web IDE.

![go to service link location for web ide](6.png)

SAP Web IDE for Full-Stack Development includes some new features. You will explore them in the following tutorials.

[DONE]
[ACCORDION-END]


### Optional

[ACCORDION-BEGIN [Step 5: ](Troubleshooting the Web IDE)]
Are you having trouble opening SAP Web IDE for Full-Stack Development? If you are seeing a 501/503 error, it may be that your principal propagation flag is not set properly.

To enable your principal propagation flag, in your SAP Cloud Platform Cockpit, click **Security** then **Trust**.

![trust link is CP Cockpit](7.png)

If the **Principal Propagation** flag is set to _Default_ or _Disabled_, you will need to update it. Click **Edit**.

![trust flags](8.png)

Change the value of **Principal Propagation** to _Enabled_ and **Save**.

![enabled trust flags](9.png)

Now, go back to step 4 and try to open SAP Web IDE for Full-Stack Development again.

[DONE]
[ACCORDION-END]

## Next Steps
- [Using SAP API Business Hub with Web IDE](https://www.sap.com/developer/tutorials/webide-api-hub.html)