---
title: Develop a Basic Java App on SAP Cloud Platform
description: Develop and deploy your first Java application using the SAP Cloud Platform Tools for Java.
primary_tag: topic>java
tags: [  tutorial>beginner, topic>cloud, topic>java, products>sap-cloud-platform ]
time: 15
---
## Prerequisites  
- [Configuring Eclipse with SAP Cloud Platform Tools for Java](https://developers.sap.com/tutorials/hcp-java-eclipse-setup.html)

## Details
### You will learn  
  - How to create a Dynamic Web project, a servlet to respond to a browser request
  - How to run the project in your local development environment
  - How to deploy the project to SAP Cloud Platform

> ### DEPRECATED
>This tutorial, which deals with writing Java in the Neo environment of SAP Cloud Platform, is deprecated.

>If you are interested in building Java applications using the SAP Cloud Platform's Cloud Foundry Environment, please see [Build a Business Application Using CAP for Java](mission.cap-java-app).

---

[ACCORDION-BEGIN [Step 1: ](Open your Eclipse IDE)]

The first step in building your application is to create a new Dynamic Web Project. Open your Eclipse IDE with the installed SAP Cloud Platform Tools. Make sure the **Java EE perspective** is open by choosing **Window > Open Perspective > Other**.

![open other perspective](jav100-2-open_other_perspective.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Choose Java EE (Default))]

Then choose the perspective **Java EE (Default)** and confirm by clicking **OK**.

![open J2EE perspective](jav100-2-open_java_ee_perspective.png)

The J2EE perspective is now open.

![J2EE perspective is open](jav100-2-java_ee_perspective_open.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open Dynamic Web Project )]

In the Eclipse main menu choose **File > New > Dynamic Web Project** to open the respective wizard.

![open wizard](jav100-2-new_dynamic_web_project.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set project name)]

In the **New Dynamic Web Project Wizard** define the **Project name** to be `helloworld`. Make sure the **Target Runtime** is set to `Java Web`, the Server Runtime Environment that has been created in the tutorial [Configuring Eclipse with SAP Cloud Platform Tools for Java](https://developers.sap.com/tutorials/hcp-java-eclipse-setup.html). Leave all other settings untouched and click **Finish** to create the project.

![project wizard](jav100-2-dynamic_web_project_wizard.png)

The `helloworld` project is now ready for your code.

![hello world project](jav100-2-helloworld_project_available.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create new servlet)]

In Java EE, web applications are implemented as `Servlets`. On the newly created `helloworld` project node, open the context menu with a right-click and choose **New > Servlet** to open the Create Servlet wizard.

![open servlet wizard](jav100-2-new_servlet.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Set class name)]

In the Create Servlet wizard enter `helloworld` as Java package and `HelloWorldServlet` as Class name. This will create Java classes with the respective package and name. Choose **Next**.

![create servlet wizard](jav100-2-create_servlet_wizard_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Set URL mapping)]

You will want this Servlet to be accessible via the URL `<servername>/helloworld`, for example `http://localhost:8080/helloworld`. For this we will set the URL mapping to `\`. For this select `/HelloWorldServlet` in the **URL mappings** field and choose **Edit**.

![create servlet wizard](jav100-2-create_servlet_wizard_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Change URL pattern)]

In the **Pattern** field, replace the current value with just `/`. Confirm with **OK**.

![URL mapping](jav100-2-url_mapping.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Generate servlet)]

Click **Finish** to generate the servlet.

![finish servlet wizard](jav100-2-create_servlet_wizard_3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Servlet opening)]

The Java Editor will open the corresponding `HelloWorldServlet` class in the editor pane. You will also find the `Servlet` under the **`helloworld` project node > Java Resources > `src` > `helloworld` > `HelloWorldServlet.java`**

![servlet in editor](jav100-2-helloworld_servlet_open.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Write hello world code)]

Next, you will edit the Servlet to output the classical "Hello World". For this you will modify the `doGet()` method and add the following code and save your changes.


```Java
/**
 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
 */
protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    response.getWriter().println("Hello World!");
}
```

![updated `doGet` method](jav100-2-change_and_save_doget.png)


The application is now ready to run.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Run on server)]

To test your application before deploying it to the SAP Cloud Platform you can run it on a local runtime. To do this, do the following:

Navigate to your `HelloWorldServlet.java` via the **`helloworld` project node > Java Resources > `src` > `helloworld` > `HelloWorldServlet.java`**. Open the context menu on the `Servlet` with a right-click and choose the **Run on Server** option.

![run on server](jav100-2-run_on_server.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Manually define server)]

Make sure that **Manually define a new server** is selected and choose **SAP > Java Web Server** as server type. Leave all other settings unchanged and click **Finish**.

![Create local server](jav100-2-create_local_server.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](View running server)]

A local server will start with your `helloworld` application deployed. After the server is ready your application will be opened in a browser within Eclipse and greet you with "Hello World!". In the **Servers** view you can also see the running server with your application deployed.

![App running](jav100-2-helloworld_local.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Run on the SAP Cloud Platform)]

To run your application on the SAP Cloud Platform you will choose a different server to run it. Again, navigate to your `HelloWorldServlet.java` via the **`helloworld` project node > Java Resources > `src` > `helloworld` > `HelloWorldServlet.java`**. Open the context menu on the Servlet with a right-click and choose the **Run on Server** option.

![run on server](jav100-2-run_on_server.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Select SAP Cloud Platform as server type)]

As before, make sure that **Manually define a new server** is selected. This time choose **SAP > SAP Cloud Platform** as server type. Make sure to set the **Landscape host** to `hanatrial.ondemand.com`. Leave all other settings unchanged and choose Next.

![run on server - cloud](jav100-2-create_cloud_server.png)

> Note: The used Landscape host `hanatrial.ondemand.com` is only valid if you are using a free Developer Account. Please change the landscape host if you want to use a productive account. The respective landscape hosts can be found in the [official documentation](https://help.hana.ondemand.com/help/frameset.htm?e4986153bb571014a2ddc2fdd682ee90.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Specify the Application name)]


On the next wizard page specify the Application name to be `helloworld`, provide the login information for your SAP Cloud Platform account and click **Finish**:

Field Name     | Value
:------------- | :-------------
Account Name   | Your SAP Cloud Platform account name, for example `p1234567890trial`
Username       | Your SAP Cloud Platform account name, for example `p1234567890` and your password

![app name](jav100-2-cloud_server_data.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 18: ](View running server)]

A Cloud server will start that has your `helloworld` application deployed. After the server is ready your application will be opened in a browser in Eclipse and greet you with Hello World!. In the **Servers** view you can also see the running server with your application deployed

![app running in HCP](jav100-2-helloworld_cloud.png)

Congratulations: You have your first application running on the SAP Cloud Platform!

[DONE]
[ACCORDION-END]




### Optional
Now that you are familiar with the basic routine of developing applications and deploying them locally and to the cloud, you may want to check out the [samples](https://help.hana.ondemand.com/help/frameset.htm?937ce0d172bb101490cf767db0e91070.html) provided as part of the SAP Cloud Platform SDK.

Related Information

- (Official documentation) [Deploying Applications](https://help.hana.ondemand.com/help/frameset.htm?e5dfbc6cbb5710149279f67fb43d4e5d.html)
- (Official documentation) [Deploying Locally from Eclipse IDE](https://help.hana.ondemand.com/help/frameset.htm?0f16c9db4a9c407abb1b4987c0afe714.html)
- (Official documentation) [Deploying on the Cloud from Eclipse IDE](https://help.hana.ondemand.com/help/frameset.htm?60ab35d9edde43a1b38cf48174a3dca2.html)
- (Official documentation) [SDK Samples](https://help.hana.ondemand.com/help/frameset.htm?937ce0d172bb101490cf767db0e91070.html)
