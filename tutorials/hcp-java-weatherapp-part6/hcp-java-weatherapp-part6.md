---
title: End-to-End Weather App Scenario Part 6
description: Adding persistence to your app using JPA (Java Persistence API)
tags: [ products>sap-hana-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [End-to-End Weather App Scenario Part 5](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part5.html)

## Next Steps
- [End-to-End Weather App Scenario Part 7](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part7.html)

## Details
### You will learn  
In this tutorial you will implement a simple domain model and implement the corresponding persistence layer using JPA (Java Persistence API). The domain model only features a single class: `FavoriteCity`, so that we can bookmark or favorite our favorite cities.

### Time to Complete
**20 min**

---

1. Let's create a base class for our domain model first. That's usually considered best practices as it provides us with a central place to add common functionality to be shared across all domain model objects on later. For that purpose, select New > Class from the context menu entry on the `weatherapp` project and provide the following details:

    - **Package name:** `com.sap.hana.cloud.samples.weatherapp.model`
    - **Classname:** `BaseObject`

    ![Adding a Java Class](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-1.png)

2. Replace the contents of the `BaseObject.java` file with [this code from GitHub](https://raw.githubusercontent.com/SAP/cloud-weatherapp/6b77dcac5a8de14ea2326fa770f941e08c5d8419/src/main/java/com/sap/hana/cloud/samples/weatherapp/model/BaseObject.java) and save your changes.

3. Next, create another Java class (`FavoriteCity.java`) using the same procedure:

    - **Package name:** `com.sap.hana.cloud.samples.weatherapp.model`
    - **Classname:** `FavoriteCity`

    ![Specifying the package and class name](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-3.png)

4. Replace the contents of the `FavoriteCity.java` file with [this code from GitHub](https://raw.githubusercontent.com/SAP/cloud-weatherapp/6b77dcac5a8de14ea2326fa770f941e08c5d8419/src/main/java/com/sap/hana/cloud/samples/weatherapp/model/FavoriteCity.java) and save your changes.

5. Next, we need to create a configuration file for our persistence layer. By Maven conventions, these non-source code artifacts should be located in a separate source code folder called: `src/main/resources`. Hence, let's create that source folder via the corresponding context menu entry on the **Java Resources** node in the Project Explorer: **New > Source Folder**. Provide the following information:

    - **Project name:** `weatherapp`
    - **Folder name:** `src/main/resources`

    ![Creating a Java resources directory](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-5.png)


6. Open the context menu of this newly created source folder and choose the **New > Other** option and then select the **Folder** option. Name the new folder `META-INF` (all capitals!) and click on **Finish**.

7. Open the context menu of the newly created `META-INF` folder and select **New > File**. Name the new file `persistence.xml` and click on **Finish**.

    ![Creating a configuration file for persistence](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-7.png)

8. Copy and paste the following XML content into the `persistence.xml` file:

    ```xml
    <?xml version="1.0" encoding="UTF-8"?>
    <persistence version="2.0"
    xmlns="http://java.sun.com/xml/ns/persistence"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://java.sun.com/xml/ns/persistence     http://java.sun.com/xml/ns/persistence/persistence_2_0.xsd">

    <persistence-unit name="application" transaction-type="RESOURCE_LOCAL">
    <provider>
        org.eclipse.persistence.jpa.PersistenceProvider
    </provider>
    <class>
        com.sap.hana.cloud.samples.weatherapp.model.BaseObject
    </class>
    <class>
        com.sap.hana.cloud.samples.weatherapp.model.FavoriteCity
    </class>
  	    <exclude-unlisted-classes>true</exclude-unlisted-classes>
  	    <properties>
	        <property name="eclipselink.ddl-generation" value="create-tables"/>
	    </properties>
    </persistence-unit>
    </persistence>
    ```

    ![Updating the persistence.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-8.png)


9. Next, we need to add some more dependencies to our `pom.xml` file. In this case, the most important dependency is on EclipseLink (our JPA implementation of choice). However, we also need to declare dependencies for the Derby DB and Jackson (a serialization framework needed to convert data into JSON and vice versa.)

    ``` xml
    <!-- EclipseLink (and JPA) -->
    <dependency>
	    <groupId>org.eclipse.persistence</groupId>
	    <artifactId>eclipselink</artifactId>
	    <version>2.5.0</version>
      </dependency>
    <dependency>
	     <groupId>org.eclipse.persistence</groupId>
	     <artifactId>javax.persistence</artifactId>
	     <version>2.1.0</version>
     </dependency>
     <!-- Derby -->
    <dependency>
    <groupId>org.apache.derby</groupId>
		  <artifactId>derbyclient</artifactId>
		  <version>10.9.1.0</version>
    </dependency>
    <dependency>
		<groupId>org.apache.derby</groupId>
		<artifactId>derby</artifactId>
		<version>10.9.1.0</version>
    </dependency>

    <dependency>
		<groupId>javax.ws.rs</groupId>
		<artifactId>javax.ws.rs-api</artifactId>
		<version>2.0</version>
    </dependency>

    <dependency>
    <groupId>org.codehaus.jackson</groupId>
		<artifactId>jackson-jaxrs</artifactId>
		<version>${org.codehaus.jackson-version}</version>
    </dependency>
    ```

    ![Adding dependencies to pom.xml for persistence](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-9.png)


10. We also need to add the Jackson version as a property in the properties section of the `pom.xml` file (as we have done in the previous section) and save your changes.

    ```xml
    <org.codehaus.jackson-version>1.9.9</org.codehaus.jackson-version>
    ```

    ![adding persistence property to pom.xml](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-10.png)

11. Next step is to create the respective CRUD service. For that purpose, create a new class with the following details:

    - **Package name:** `com.sap.hana.cloud.samples.weatherapp.api`
    - **Classname:** `FavoriteCityService`

    ![Adding a new CRUD service](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-11.png)

12. Replace the contents of the `FavoriteCityService.java` file with [this code from GitHub](https://raw.githubusercontent.com/SAP/cloud-weatherapp/0988620f000075011dd3eb29c7155fae523647d8/src/main/java/com/sap/hana/cloud/samples/weatherapp/api/FavoriteCityService.java) and save your changes.

13. To register our RESTful service implementation in the `web.xml` configuration file, add the fully qualified classname of our `FavoriteCityService` class to the *comma-separated* list of `jaxrs.serviceClasses`. See the snippet below for where to enter the fully qualified classname inside the `<param-value>` element (don't forget the comma at the end of the `AuthenticationService` line).

    ```xml
    <init-param>
    <param-name>jaxrs.serviceClasses</param-name>
	  <param-value>
    com.sap.hana.cloud.samples.weatherapp.api.AuthenticationService,
    com.sap.hana.cloud.samples.weatherapp.api.FavoriteCityService
    </param-value>
    </init-param>
    ```

    ![Updating web.xml for persistence](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-13.png)

14. Just below the aforementioned `<init-param>` tag we need to add another `<init-param>` for JSON de-serialization as follows:

    ```xml
    <init-param>
  		<param-name>jaxrs.providers</param-name>
  		<param-value>org.codehaus.jackson.jaxrs.JacksonJsonProvider</param-value>
    </init-param>
    ```
    ![Adding init-param for JSON de-serialization](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-14.png)

15. The last change to make is to define a DataSource within the `web.xml` in order to connect to the underlying database. To do this, copy and paste the following XML snippet after the closing `</welcome-file-list>` tag:


    ```xml
    <resource-ref>
    <res-ref-name>jdbc/DefaultDB</res-ref-name>
    <res-type>javax.sql.DataSource</res-type>
    </resource-ref>
    ```

    ![defining the DataSource in the web.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-15.png)


16. In order to properly test our RESTful service we need a REST tool (e.g. Postman) that allows you to execute HTTP calls in a convenient manner.

17. Within Postman, enter `http://localhost:8080/weatherapp/api/v1/cities` in the URL input field and make sure to provide your username/password as `Basic Auth` parameters in the **Authorization** tab.

    Afterwards, make sure to update the request by pressing the respective **Update request** button. That will then add the “Authorization” parameter as an HTTP header parameter to your request.

    ![Using a REST client to connect to the Java app](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-17.png)


18. Once you execute the call, you'll see two empty brackets “[]” (indicating an empty array) after successful authentication. Don't worry, we haven't saved any cities as favorites yet, so that's just what we would expect.

    ![Server response to REST client](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part6/e2e_06-18.png)


## Next Steps
- [End-to-End Weather App Scenario Part 7](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part7.html)
