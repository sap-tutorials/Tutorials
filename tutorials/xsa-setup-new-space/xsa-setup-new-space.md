---
title: SAP HANA - Create a Space in XS Advanced
description: Create a new space for XS Advanced
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  
Describe what the user will learn from your tutorial and what the outcome will be.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Log in to the XS Advanced Administration site)]

If you are using SAP HANA, express edition, open `https://hxehost:51015`.

>If you are not using SAP HANA, express edition, or you have changed the ports, you can find out the right URL using command `xs apps` on the Command Line Interface and look for the URL for application `xsa-admin`. You can access the CLI directly from an SSH console on the server or download using the Download Manager. You can get the Download Manager after registering at the [Download site](https://www.sap.com/developer/topics/sap-hana-express.html)

&nbsp;

Log in using `xsa_admin` (or, if unavailable, a user with authorizations to create Spaces)

![Log in with XSA_ADMIN](1.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Space)]

Click on **Organization and Space Management**

![Organization and space management](2.png)

Then on **Create Space**

![Create a space](3.png)

Provide a name for the space

![Name space](4.png)


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add users to the Space)]

Click on the space

![Add users](5_1.png)

Click on Users and then on **Add User**

![Add users](5.png)

Select the users you want to add to the space:

![Add users](6.png)

Select the roles for each of the users, click on **Save** and go back to the organization

![Add users](7.png)

Go back using the **Home** button

![Add users](8.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Setup the Space using the space enablement tool)]

Click on the **Application Monitor** tile

![App monitor](9.png)

Look for the app `di-space-enablement-ui` and enable it if it is not running yet:

![App monitor](10.png)

Once it is running, click on the URL to access it:

![Space enablement](11.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the Space enablement tool)]

In the Space Enablement tool, click on **Enable**:

![Space enablement](12.png)

Wait until processing finishes and you get a successful message:

![Space enablement finished](13.png)

The space is now ready to use.

[ACCORDION-END]
