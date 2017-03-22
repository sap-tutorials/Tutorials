---
title: End-to-End Weather App Scenario Part 4
description: Convert your app to a Maven-based project
primary_tag: topic>java
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [End-to-End Weather App Scenario Part 3](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part3.html)

## Next Steps
- [End-to-End Weather App Scenario Part 5](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part5.html)

## Details
### You will learn  
In this tutorial you will learn how to convert your basic Java app into a Maven-based project which is useful to prepare for the complexities of dependency management in larger apps.

### Time to Complete
**10 min**

---


[ACCORDION-BEGIN [Step 1: ](Open configuration menu)]

Select the `weatherapp` node in the project explorer and open the context menu. Select the **Configure > Convert to Maven Project** option.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Change group ID)]

Change the group ID from `weatherapp` to `com.sap.hana.cloud.samples` and click on **Finish**. The most noticeable change will be that a `pom.xml` file will be created in the root folder of the`weatherapp` project.

![Customizing Maven POM - pom.xml](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part4/e2e_04-2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Replace existing content)]

Copy the entire content from the [`pom.xml`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/ebd8817f9842a6fc3cbae213d69b024762a7d30f/pom.xml) file from GitHub and use it to replace the existing content in your project.

![modifying the Maven pom.xml](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part4/e2e_04-3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Update project)]

Open the context menu on the `weatherapp` project in the Project Explorer and select the menu entry **Maven > Update Project…** (The first time you do this can take a bit longer, as Maven will download all the required build plugins and dependencies specified in the `pom.xml` file). Make sure your project is selected then click on **OK**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Build)]

Select the **Run as > Maven build…** context menu of the `weatherapp` project and enter the following in the **Goals** field: `clean package install`.

![Adding goals to the Maven Build settings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part4/e2e_04-5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Run the project)]

Click on **Run** and the project should build successfully.

[DONE]
[ACCORDION-END]



## Next Steps
- [End-to-End Weather App Scenario Part 5](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part5.html)
