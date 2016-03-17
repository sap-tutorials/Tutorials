---
title: End-to-End Weather App Scenario Part 8
description: Use the Connectivity Service to access external (backend) systems.
tags: [tutorial:interest/gettingstarted, tutorial:interest/cloud, tutorial:product/hcp, tutorial:technology/java]
---

## Prerequisites  
 - [End-to-End Weather App Scenario Part 7](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part7.html)
 - Register for an API Key from [openweathermap.org](http://openweathermap.org/appid)

## Next Steps
 - [End-to-End Weather App Scenario Part 9](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part9.html)
 
## Details
### You will learn  
In this tutorial you will learn how to use the [Connectivity Service](https://help.hana.ondemand.com/help/frameset.htm?e54cc8fbbb571014beb5caaf6aa31280.html) to connect to an external (e.g. backend) system. 

For this part of the series, you will consume a RESTful weather service that returns data in JSON format: <http://openweathermap.org/api>. 

 >Note: In order to consume the openweathermap API you need to apply for an API key as outlined here: <http://openweathermap.org/appid>. But don’t worry, that’s easy to do and won’t take longer than two minutes!


### Time to Complete
**10 min**

---

1. Once you have your APPID, create a new service class with the following properties:

    - **Package name:** `com.sap.hana.cloud.samples.weatherapp.api`
    - **Classname:** `WeatherService`

    ![Creating a new Java class](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part8/e2e_08-1.png)
 
2. Replace the contents of the **WeatherService.java** file with [this code from Github](https://raw.githubusercontent.com/SAP/cloud-weatherapp/0f16e22720cbc5032e9a63af4ee95e2ead6e0761/src/main/java/com/sap/hana/cloud/samples/weatherapp/api/WeatherService.java) and save your changes.


3. Include the full-qualified classname of the WeatherService class in the list of JAX-RS services specified in the **web.xml** configuration you did before. The corresponding <init-param> element should now look like this:

    ```xml
    <init-param>
    	<param-name>jaxrs.serviceClasses</param-name>
       	<param-value>
       		com.sap.hana.cloud.samples.weatherapp.api.AuthenticationService,
			com.sap.hana.cloud.samples.weatherapp.api.FavoriteCityService,
			com.sap.hana.cloud.samples.weatherapp.api.WeatherService
		</param-value>
    </init-param>
    ```

    ![Modifying the web.xml file to include the new service](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part8/e2e_08-3.png)

4. Within the code we are obtaining a reference to an HttpDestination with the logical name “openweathermap-destination” via JNDI, hence we need to create that destination. For that purpose, double-click on the local server in the Servers view.

    Switch to the **Connectivity** tab and click on the green **+** symbol to add a new destination.  Enter the following information (replacing `YOUR_APPID` with the code you received when you registered with the openweathermap service.

    - **Name:** `openweathermap-destination`
    - **Type:** `HTTP`
    - **URL:** `http://api.openweathermap.org/data/2.5/weather?APPID=YOUR_APPID`

    ![Adding a new destination to the local web server](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part8/e2e_08-4.png)
 
5. Similar to what we have done to register the DataSource in the **web.xml** file, we also need to specify the HTTP destination in the **web.xml** file. Open it and enter the following code snippet underneath the already existing tag. 

    ```xml
    <resource-ref>
	 	<res-ref-name>openweathermap-destination</res-ref-name>
	 	<res-type>com.sap.core.connectivity.api.http.HttpDestination</res-type>
    </resource-ref>
    ```

    ![Adding the HTTP destination to the web.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part8/e2e_08-5.png)
 

6. Save your changes and deploy/publish the application again. After successful authenticating yourself, navigate to the following URL: <http://localhost:8080/weatherapp/api/v1/weather?id=2158177>

    ![Testing the new weather destination](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part8/e2e_08-6.png)
 
7. One more thing: it would actually be nice to be able to traverse the path and query for weather information in a more RESTful manner via a URL pattern like: `/api/v1/cities/{id}/weather`. Let’s add a respective method to the **FavoriteCityService** class:

    ```java
    @GET
    @Path("/{id}/weather")
    @Produces({ MediaType.APPLICATION_JSON })
    public Response getWeatherInformation(@PathParam(value = "id") String id,    @Context SecurityContext ctx)
    {
        WeatherService weatherService = new WeatherService();
        return weatherService.getWeatherInformation(id, null);
    }
    ```

    ![Supporting a new URL by adding an @Path annotation](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part8/e2e_08-7.png)

    >Note: Since working behind corporate firewalls is a common cause of frustration I want to point out how to accomodate this. if your firewall requires a proxy for outbound communication you need to start your local server with proxy settings as follows: 

    - In the Servers view, double-click the added server to open the editor.
    - Click the Open Launch Configuration link.
    - Choose the (x)=Arguments tab page.
    - In the VM Arguments box, add the following row:
 
        - Dhttp.proxyHost=
        - Dhttp.proxyPort=
        - Dhttps.proxyHost=
        - Dhttps.proxyPort=
    - Choose OK

    ![Modifying launch configuration when running behind a firewall](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part8/e2e_08-Note.png)
 

## Next Steps
 - [End-to-End Weather App Scenario Part 9](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part9.html)