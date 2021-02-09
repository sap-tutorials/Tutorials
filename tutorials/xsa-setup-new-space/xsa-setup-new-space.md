---
title: SAP HANA - Create a Space in XS Advanced
description: Create a new space for XS Advanced
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, products>sap-web-ide ]
time: 15
---

## Prerequisites
 - This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.

## Details
### You will learn  
Describe what the user will learn from your tutorial and what the outcome will be.


---

[ACCORDION-BEGIN [Step 1: ](Log in to the XS Advanced Administration site)]

If you are using SAP HANA, express edition, go to `https://hxehost:39030` to access the `xsa-cockpit` app.

![Cockpit log in](39030_cockpit.png)

>If you are not using SAP HANA, express edition, or you have changed the ports, you can find out the right URL using command `xs apps` on the Command Line Interface and look for the URL for application `xsa-cockpit`. You can access the CLI directly from an SSH console on the server or download using the Download Manager. You can get the Download Manager after registering at the [Download site](https://developers.sap.com/topics/hana.html)

&nbsp;

Log in using `xsa_admin` (or, if unavailable, a user with authorizations to create Spaces)

![Log in with XSA_ADMIN](1.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Space)]

Click on **Organizations** and on the organization to which you want to add a space

![Organization and space management](2.png)

Then on **New Space**

![Create a space](3.png)

Provide a name for the space and the roles you want to give to `XSA_ADMIN`

![Name space](4.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add users to the Space)]

Click the space

![Add users](5_1.png)

Click **Members** and then on **Add Member**

![Add users](5.png)

Select the users you want to add to the space and their roles:

![Add users](6.png)

Go back using the **Home** button

![Add users](8.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Setup the Space using the space enablement tool)]

Go back using the **home** button and go into the **SAP** space

![App monitor](9.png)

Look for the app `di-space-enablement-ui` and enable it if it is not running yet:

![App monitor](10.png)

Once it is running, click on the link to see the URL (by default, `https://hxehost:39030` )

![Space enablement](11.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the Space enablement tool)]

In the Space Enablement tool, click on **Enable**:

![Space enablement](12.png)

Wait until processing finishes and you get a successful message:

![Space enablement finished](13.png)

The space is now ready to use.

[DONE]

[ACCORDION-END]
