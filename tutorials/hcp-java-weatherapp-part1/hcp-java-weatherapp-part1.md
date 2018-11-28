---
title: Create a basic Java app in SAP Cloud Platform
description: Create a basic Java app in SAP Cloud Platform
primary_tag: topic>java
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [Getting Started with the SAP Cloud Platform Tools for Java](https://www.sap.com/developer/tutorials/hcp-java-eclipse-setup.html)
- Note: JDK 1.6 or 1.7 are required. If you have a later version of Java installed, please install [JDK 1.7](http://www.oracle.com/technetwork/pt/java/javase/downloads/jdk7-downloads-1880260.html) and temporarily change your `JAVA_HOME` environment variable to point to it.

## Next Steps
- [End-to-End Weather App Scenario Part 2](https://www.sap.com/developer/tutorials/hcp-java-weatherapp-part2.html)

## Details
### You will learn  
In this tutorial series you will start from zero and develop a fully operational weather application. In total there are 10 parts to the series, each building on top of its predecessor. The entire source code of both the final and all intermediate parts are available on [GitHub](https://github.com/SAP/cloud-weatherapp).


The constituent parts of this tutorial series cover the following:

- How to create a simple web application on HCP
- How to apply authentication and authorization
- How to expose business functionality as an external RESTful API
- How to add JPA-based persistence to your web app
- How to leverage the multi-tenancy features of SAP Cloud Platform
- How to use the connectivity service to consume external services
- How to add a mobile-friendly UI5-based user interface to the web application

In Part 1, you will develop a basic Java app to ensure that both Eclipse IDE and the local SAP Cloud Platform (HCP) tooling have been properly installed and configured.

### Time to Complete
**10 min**

---

[ACCORDION-BEGIN [Step 1: ](Create a new dynamic web project)]

Create a new dynamic web project by selecting the **File > New > Dynamic Web Project** menu entry and enter the following information:

- **Name:** `weatherapp`
- **Target Runtime:** `Java Web`
- **Dynamic Web Module Version:** `2.5`

Click on **Next**

![Creating a new dynamic web project](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-1.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add new folder)]

Remove the standard `src` Source folder and add a new one called `src/main/java` to create a project that adheres to the standard Maven Directory Layout.

Change the default output folder to `target/classes`

Click on **Next**.

![Configuring java project](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Change the Content Directory)]

Change the Content Directory from `WebContent` to `src/main/webapp` (again, to adhere to Maven conventions)

Click on **Finish**.

![specifying output directory to comply with Maven conventions](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-3.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add details to weather app project)]

Make sure you have your `weatherapp` project folder selected, and then create a new Servlet by selecting the **File > New > Servlet** menu entry and enter the following information:

- **Package name:** `com.sap.hana.cloud.samples.weatherapp.web`
- **Class name:** `HelloWorldServlet`

Click on **Next**.

![Creating a new servlet](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Change the URL Mapping)]

Change the URL Mapping from `/HelloWorldServlet` to `/hello` to make it a bit easier to memorize.

Click on **Finish** and the `HelloWorldServlet.java` file will open in the editor.

![Changing the Servlet URL mapping](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-5.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Replace comment with code)]

Now we need to do our first bit of coding. Navigate to the servlet's `doGet()` method and replace the `TODO` comment with the following line of code and save your changes:

```javascript
response.getWriter().println("Hello World!");
```

![Modifying the doGet() method](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-6.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run on server)]

Deploy the application to your local server by using the **Run as > Run on Server** context menu of the `HelloWorldServlet` node in the Project Explorer view.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Manually define a new Server)]

Choose the **Manually define a new Server** option and select the **SAP / Java Web Server** option from the server selection. Make sure to select **Java Web** as the server runtime environment.

Click on **Finish**. The internal browser is now started and displays the traditional message marking the first step into a new programmer's journey.

![Creating a new local Java Server to run your app](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-8.png)



[ACCORDION-END]



