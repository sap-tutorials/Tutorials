---
title: End-to-End Weather App Scenario Part 1
description: Create a basic Java app in SAP HANA Cloud Platform
tags: [tutorial:interest/gettingstarted, tutorial:interest/cloud, tutorial:product/hcp, tutorial:technology/java]
---

## Prerequisites  
 - [Getting Started with the SAP HANA Cloud Platform Tools for Java](https://hcp.sap.com/developers/TutorialCatalog/jav100_01_java_setup_eclipse.html)
 - Note: JDK 1.6 or 1.7 are required. If you have a later version of Java installed, please install [JDK 1.7](http://www.oracle.com/technetwork/pt/java/javase/downloads/jdk7-downloads-1880260.html) and temporarily change your JAVA_HOME environment variable to point to it.

## Next Steps
 - [End-to-End Weather App Scenario Part 2](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part2.html)
 
## Details
### You will learn  
In this tutorial series you will start from zero and develop a fully operational weather application. In total there are 10 parts to the series, each building on top of its predecessor. The entire source code of both the final and all intermediate parts are available on [Github](https://github.com/SAP/cloud-weatherapp).


The constituent parts of this tutorial series cover the following:

 - How to create a simple web application on HCP
 - How to apply authentication and authorization
 - How to expose business functionality as an external RESTful API
 - How to add JPA-based persistence to your web app
 - How to leverage the multi-tenancy features of SAP HANA Cloud Platform
 - How to use the connectivity service to consume external services
 - How to add a mobile-friendly UI5-based user interface to the web application

In Part 1, you will develop a basic Java app to ensure that both Eclipse IDE and the local SAP HANA Cloud Platform (HCP) tooling have been properly installed and configured.

### Time to Complete
**10 min**

1. Create a new dynamic web project by selecting the “New > Dynamic Web Project” menu entry and enter the following information:

 - **Name:** `weatherapp`
 - **Target Runtime:** `Java Web`
 - **Dynamic Web Module Version:** `2.5`
 
 Click on **Next**
 
 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-1.png)

2. Remove the standard “src” Source folder and add a new one called `src/main/java` to create a project that adheres to the standard Maven Directory Layout.

 Change the default output folder to `target/classes` 

 Click on **Next**. 
 
  ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-2.png) 

3. Change the Content Directory from `WebContent` to `src/main/webapp` (again, to adhere to Maven conventions) 

 Click on **Finish**.

 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-3.png)

4. Create a new Servlet by selecting the “New > Servlet” menu entry and enter the following information:

 - **Package name:** `com.sap.hana.cloud.samples.weatherapp.web`
 - **Class name:** `HelloWorldServlet`

 Click on **Next**.
 
 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-4.png)
 

5. Change the URL Mapping from `/HelloWorldServlet` to `/hello` to make it a bit easier to memorize.

 Click on **Finish**.
 
  ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-5.png)

6. Now we need to do our first bit of coding. Navigate to the servlet’s **doGet()** method and replace the **TODO comment** with the following line of code and save your changes:

 ```javascript
 response.getWriter().println("Hello World!");
 ```

  ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-6.png)

7. Deploy the application to your local server by using the **Run as > Run on Server** context menu of the **HelloWorldServlet** node in the Project Explorer view.

8. Choose the **Manually define a new Server** option and select the **SAP / Java Web Server** option from the server selection. Make sure to select **Java Web** as the server runtime environment. 

 Click on **Finish**. The internal browser is now started and displays the traditional message marking the first step into a new programmer’s journey. 

 ![](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part1/e2e_01-8.png)


## Next Steps
 - [End-to-End Weather App Scenario Part 2](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part2.html)