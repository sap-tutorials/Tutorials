---
title: Enable and configure the SAP Cloud Platform, gamification service
description: In SAP Cloud Platform, configure the user roles and connectivity destinations necessary to use the gamification service.
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, topic>cloud, products>sap-cloud-platform ]
---
## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Sign up for an account on SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
- [Create the gamified HelpDesk application](https://www.sap.com/developer/tutorials/hcp-gamification-sample-application.html)

## Details
### You will learn  
How to enable the gamification service in your SAP Cloud Platform account as well as what roles and destination configurations are necessary to develop gamified applications.

### Time to Complete
**5 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Open SAP Cloud Platform Cockpit)]

In your Web browser, open the cockpit of the [SAP Cloud Platform](https://account.hanatrial.ondemand.com/cockpit). Then select **Services** from the left-hand navigation.

![Selecting Services](1.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable gamification service)]

Select **Gamification Service** and click **Enable** to enable the service.

![Not Enabled Gamification Service](3.png)

A gamification service subscription is now assigned to your account.

![Enabled Gamification Service](2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure gamification service)]

Click **Configure Gamification Service**.

![Configure Gamification Service](4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify roles)]

Select **Roles** from the left-hand navigation.

![Configure Roles](5.png)

Verify that the following roles have been assigned to your user:
- **`AppStandard`**
- **`AppAdmin`**
- **`GamificationReviewer`**
- **`GamificationDesigner`**
- **`TenantOperator`**
- **`helpdesk`**

![Verify Roles](6.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Check destinations)]

Back in the Cockpit, select **Destinations** from the left-hand navigation. Verify that the **`gsdest`** and **`gswidgetdest`** destinations required by the gamification service are listed.

![Account-level Destinations](7.png)

> Do NOT click the **Destinations** button in the gamification service itself.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Edit destination)]

Click the **Edit** button to open the **`gsdest`** destination for editing.

![Verifying Destinations](8.png)

Verify that **User** is your SAP ID user and replace the default **Password** mask with your SAP ID user password. Click **Save**.

![Configure Destination](9.png)


[ACCORDION-END]


## Next Steps

- [Create the gamified HelpDesk application](https://www.sap.com/developer/tutorials/hcp-gamification-sample-application.html)
