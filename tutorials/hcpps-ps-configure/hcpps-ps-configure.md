---
title: SAP HCP predictive services, Enable, deploy and configure the SAP HCP predictive services
description: Enable, deploy and configure the SAP HCP predictive services
tags: [ tutorial>beginner, products>sap-hana, products>sap-hana-cloud-platform ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Expose your predictive demo dataset as an OData service](http://www.sap.com/developer/tutorials/hcpps-hana-dataset-odata.html)

## Next Steps
  - [Install a REST client to interact with the HCP predictive services](http://www.sap.com/developer/tutorials/hcpps-rest-client-install.html)

## Details
### You will learn
  - How to enable, deploy and configure the HCP predictive services for your HCP Developer Account

### Time to Complete
  **10 minutes**

---

1. Let's go back to the [***SAP HANA Cloud Platform Cockpit***](http://account.hanatrial.ondemand.com/cockpit) with your free trial account and access "Your Personal Developer Account".

    Click on your ***HCP Account*** identifier (which ends with *trial*) as highlighted on the below screenshot.

    ![SAP HANA Cloud Platform Cockpit](1.png)

1. On the left side bar, click on **Services**.

    Click on the **Predictive Services** tile

    ![Services](2.png)

1. By default, the ***Predictive Services*** are not enabled.

    Click on **Enable**

    ![Predictive Services](3.png)

1. Once enable, you will need to deploy the ***Cloud for Predictive Analytics*** application.

    Click on **Go To Service**

    ![Predictive Services](4.png)

1. You might receive the following information message that you may need to upgrade your ***Cloud for Predictive Analytics*** services application (***C4PA***) which we will do next.

    Click on **OK**

    ![Cloud for Predictive Analytics](5.png)

    Next, you can deploy the ***Cloud for Predictive Analytics*** services application.

    Click on the tile

    ![Cloud for Predictive Analytics](5-1.png)

1. In order to deploy the ***Cloud for Predictive Analytics*** services application in your ***Your Personal Developer Account***, you just need to provide your ***HCP Account Password*** as highlighted on the screenshot. The other settings, like ***HCP Account User Name***, will be filled with your current information.

    Click on **Deploy**

    ![Cloud for Predictive Analytics](6.png)

    Click on **OK** to confirm the deployment

    ![Cloud for Predictive Analytics](6-1.png)

1. Once deployed, you will be provided with the ***Application Dashboard*** URL.

    Click on the highlighted link

    ![Cloud for Predictive Analytics](7.png)

1. Now, you need to add you a data source binding to your trial HANA instance.

    On the left side bar, navigate in **Configuration**, then click on **Data Source Binding**

    ![Cloud for Predictive Analytics](8.png)

1. The data source binding will allow the ***Cloud for Predictive Analytics*** to locate the data to be consumed by the service and execute the ***SAP HANA Automated Predictive Library*** (`APL`) on it, but also to persist a set on internal tables.

    Click on **New Binding**

    ![New Binding](9.png)

    Enter your ***HANA User Account*** login (`HCPPSTRIAL`) and password (`Welcome16`).

    Click on **Save**

    ![New Binding](9-1.png)  

1. Now, you need to configure security settings for your ***HCP Account***.

    On the left side bar, navigate in **Security**, then click on **Roles**  

    ![Binding](10.png)

1. Assign to the `C4PA-User` and `C4PA-Admin` role your ***HCP Account User Name*** (which was displayed during the ***Deploy*** step, should start with a lower case letter like "p", "s", "i", "c" or "d" depending on the type of SAP account you have) using the **Assign** button as highlighted on the screenshot.
    
	Make sure you don't include the "trial" at the end.

    ![Roles](11.png)

1. Now, you need to configure the authentication schemes for the application.

    On the left side bar, navigate in **Security**, then click on **Authentication Configuration**

    Enable the **Custom** mode

    Check **User name and password** and **Client certificate** for the ***FORM*** authentication scheme

    Click on **Save**

    ![Authentication](12.png)

    You receive next an alert that the changes will be applied on the next restart, which is what we will do next.

    ![Authentication](12-1.png)

1. Go back to the ***Overview*** page (via the left menu bar).

    Click on **Start**

    ![Overview](13.png)

1. Once started, you will be provided with the **Application URLs** which will give us access to the online documentation and the administration panels.

    Click on the ***Application URLs*** link

    ![Overview](14.png)

    Later, you will be using the ***Application URLs***, so you should save the highlighted URL in a notepad as your ***C4PA Application URL***.

    Click on the **Administration** tile

    ![Application](14-1.png)

1. Status is green! Congrats, you have configured the HCP predictive services on your HCP trial account.

    ![Application](15.png)    

## Next Steps
  - [Install a REST client to interact with the HCP predictive services](http://www.sap.com/developer/tutorials/hcpps-rest-client-install.html)
