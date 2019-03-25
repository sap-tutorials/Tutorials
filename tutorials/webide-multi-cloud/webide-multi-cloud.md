---
title: Enable SAP Web IDE Full-Stack
description: Locate SAP Web IDE Full-Stack, enable it, and access it, all from your browser.
primary_tag: products>sap-web-ide
auto_validation: true
tags: [  tutorial>beginner, products>sap-cloud-platform, products>sap-web-ide  ]
time: 5
---

## Details
### You will learn  
  - How to enable SAP Web IDE Full-Stack

You can use many different tools to build and deploy apps on the SAP Cloud Platform, but we recommend SAP Web IDE, a service of SAP Cloud Platform. In this tutorial, you'll locate it, enable it, and access it, all from your browser, without needing to install anything.

---

[ACCORDION-BEGIN [Step 1: ](Open SAP Cloud Platform services)]
Open your SAP Cloud Platform account. Make sure you are in a `Neo` Environment, not one of the new Cloud Foundry environments. `Neo` is the classic, proprietary SAP Cloud Platform.

Go to **Services**.

![services location on SAP CP Home page](1.png)

![services page on SAP CP](2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Find SAP Web IDE Full-Stack)]
In the service catalog, search for **Web IDE**.

![search on SCP services page](3.png)

Select the **SAP Web IDE Full-Stack** service.

![sap web ide multi-cloud version](4.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Enable the service)]
Once the service page loads, enable the service. This may take a few minutes.

![enable feature button for web ide](5.png)

Wait for the status to change to the green, **Enabled** status.

![green enabled status icon](5b.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open SAP Web IDE)]
Once the service is enabled, click **Go to Service**. This opens the new, full-stack SAP Web IDE.

![go to service link location for web ide](6.png)

The full-stack version of SAP Web IDE includes some new features. You will explore them in the following tutorials.

Copy and paste the URL from your SAP Web IDE Full-Stack window into the text box below.

[VALIDATE_4]
[ACCORDION-END]


### Optional

[ACCORDION-BEGIN [Step 5: ](Troubleshooting)]
Are you having trouble opening SAP Web IDE Full-Stack? If you are seeing a 501/503 error, it may be that your principal propagation flag is not set properly.

To enable your principal propagation flag, in your SAP Cloud Platform cockpit, click **Security** and then **Trust**.

![trust link is CP Cockpit](7.png)

If the **Principal Propagation** flag is set to _Default_ or _Disabled_, you will need to update it. Click **Edit**.

![trust flags](8.png)

Change the value of **Principal Propagation** to _Enabled_ and click **Save**.

![enabled trust flags](9.png)

Now, go back to step 4 and try again to open the SAP Web IDE Full-Stack.

[DONE]
[ACCORDION-END]
