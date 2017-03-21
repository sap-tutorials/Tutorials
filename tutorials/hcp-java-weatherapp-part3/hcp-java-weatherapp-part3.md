---
title: End-to-End Weather App Scenario Part 3
description: Adding authentication and authorization to your Java app.
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
 - [End-to-End Weather App Scenario Part 2](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part2.html)

## Next Steps
 - [End-to-End Weather App Scenario Part 4](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part4.html)

## Details
### You will learn  
In this tutorial you will learn how to add authentication and authorization to your Java app.

### Time to Complete
**10 min**

 >Please note that SAP Cloud Platform adheres to Java standards to manage authentication and authorization.

---

1. In order to activate authentication and establish authorization we have to apply the respective security settings in the `web.xml` configuration file. The full `web.xml` contents are below:

    ```xml
    <?xml version="1.0" encoding="UTF-8"?>
    <web-app xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://java.sun.com/xml/ns/javaee" xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd" id="WebApp_ID" version="2.5">
    <display-name>cloud-weatherapp</display-name>
    <welcome-file-list>
    <welcome-file>index.html</welcome-file>
    </welcome-file-list>
    <servlet>
    <display-name>HelloWorldServlet</display-name>
    <servlet-name>HelloWorldServlet</servlet-name>
    <servlet-class>
        com.sap.hana.cloud.samples.weatherapp.web.HelloWorldServlet
    </servlet-class>
    </servlet>
    <servlet-mapping>
    <servlet-name>HelloWorldServlet</servlet-name>
    <url-pattern>/hello</url-pattern>
    </servlet-mapping>
    <login-config>
    <auth-method>FORM</auth-method>
    </login-config>
    <security-constraint>
    <web-resource-collection>
        <web-resource-name>Protected Area</web-resource-name>
        <url-pattern>/*</url-pattern>
    </web-resource-collection>
    <auth-constraint>
        <!-- Role Everyone will not be assignable -->
        <role-name>Everyone</role-name>
    </auth-constraint>
    </security-constraint>
    <security-role>
    <description>All SAP Cloud Platform users</description>
    <role-name>Everyone</role-name>
    </security-role>
    </web-app>
    ```

    ![Adding security settings to the web.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part3/e2e_03-1.png)

2. After successful authentication the application can access users' principal information using standard servlet APIs. To illustrate that, make the following changes to the `HelloWorldServlet`:

    ```java
    /**
    * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
    */protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
    	    String user = request.getRemoteUser();
    	    if (user != null)
    	    {
    	        response.getWriter().println("Hello, " + user);
    	    }
    	    else
    	    {
    	        LoginContext loginContext;
    		    try
    	        {
    	             loginContext = LoginContextFactory.createLoginContext("FORM");
    			 		loginContext.login();
    	             response.getWriter().println("Hello, " +  request.getRemoteUser());
    	        }
    	        catch (LoginException ex)
    	        {
    	             ex.printStackTrace();
    		    }
    	    }
    }


    /**
    * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
    */
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
    		doGet(request, response);
    }
    ```

    >Note: The reason we also had to implement the "doPost()" method is related to specifics of the SAML 2.0 authentication process  flow. For more information please refer to the [respective parts](https://help.hana.ondemand.com/help/frameset.htm?e637f62abb571014857cb0232adc43a7.html) of the SAP Cloud Platform online documentation.

    ![Modifying the doGet() and doPost() methods in the Java Servlet](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part3/e2e_03-2.png)


3. To remove the syntax errors, you need to organize import statements via the respective context menu **Source > Organize imports** of the main code editor window. Save your changes.

4. Deploy/publish the updated application (you should know the drill by now).

5. Since we are working with a local server so far we need to provide a local user repository to authenticate against. For this purpose, double-click on the local server node in the **Servers** view to open the configuration window.

    At the bottom of that window there are four tabs: **Overview**, **Connectivity**, **Users** and **Loggers**.

    Within the Users tab you can manage local users. Let's create a simple test user with the user id "test" and a password of your choice. Save your changes.

    ![Adding a user to the local Java Server](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part3/e2e_03-5.png)

6. Now, when you navigate to the `HelloWorldServlet` with the URL <http://localhost:8080/weatherapp/hello> you'll first be prompted to enter your user credentials before you are forwarded to the requested servlet. If the authentication was successful you should now see a personalized welcome message instead of the dull "Hello World!" we saw earlier.

    ![Running the Java app with authentication enabled](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part3/e2e_03-6.png)


## Next Steps
 - [End-to-End Weather App Scenario Part 4](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part4.html)
