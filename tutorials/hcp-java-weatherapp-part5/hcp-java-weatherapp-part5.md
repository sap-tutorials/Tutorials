---
title: Adding RESTful services to your app
description: Adding RESTful services to your app
primary_tag: topic>java
tags: [ products>sap-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
- [End-to-End Weather App Scenario Part 4](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part4.html)

## Next Steps
- [End-to-End Weather App Scenario Part 6](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part6.html)

## Details
### You will learn  
In this tutorial you will learn how to expose RESTful services using a library called [Apache CXF](http://cxf.apache.org/), which is one of the most often used implementations of the [JAX-RS](https://jcp.org/en/jsr/detail?id=339) standard.

### Time to Complete
**10 min**

---

[ACCORDION-BEGIN [Step 1: ](Add dependency references)]

First, we need to add the dependency references to Apache CXF in the `pom.xml` file. Insert the XML snippet below just below the **Servlet** dependency section.

```xml
<!-- Apache CXF -->
<dependency>
    <groupId>org.apache.cxf</groupId>
    <artifactId>cxf-rt-frontend-jaxws</artifactId>
    <version>${org.apache.cxf-version}</version>
</dependency>
<dependency>
    <groupId>org.apache.cxf</groupId>
    <artifactId>cxf-rt-frontend-jaxrs</artifactId>
    <version>${org.apache.cxf-version}</version>
</dependency>
<dependency>
    <groupId>javax.ws.rs</groupId>
    <artifactId>javax.ws.rs-api</artifactId>
    <version>2.0</version>
</dependency>
```

![Adding Apache CXF to the pom.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part5/e2e_05-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Specify CXF version property)]

We also need to specify the corresponding CXF version property at the end of the `<properties>` tag in `pom.xml`. See the image below for where to insert this snippet.

```xml
<org.apache.cxf-version>3.0.0</org.apache.cxf-version>
```

![Adding Apache CXF to pom.xml properties element](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part5/e2e_05-2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a new class)]

Next, create a new Class via the context menu entry **New > Class** of the `weatherapp` node in the Project Explorer. Enter the following information:

- **Package name:** `com.sap.hana.cloud.samples.weatherapp.api`
- **Classname:** `AuthenticationService`

Click on **Finish**.

![Adding a new Java class](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part5/e2e_05-3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Replace code)]

Replace the contents of `AuthenticationService.java` with the following and save your changes.

```java
package com.sap.hana.cloud.samples.weatherapp.api;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;

@Path("/auth")
@Produces({ MediaType.APPLICATION_JSON })
public class AuthenticationService
{
	@GET
	@Path("/")
	@Produces({ MediaType.TEXT_PLAIN })
	public String getRemoteUser(@Context SecurityContext ctx)
	{
		String retVal = "anonymous";
		try
		{
			retVal = ctx.getUserPrincipal().getName();
		}
		catch (Exception ex)
		{
			ex.printStackTrace(); // lazy
		}
		return retVal;
	}
}
```

![Building out the new Java class](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part5/e2e_05-4.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Edit configuration file)]

Open the `web.xml` configuration file and copy and paste the following lines of code in between the closing `</servlet-mapping>` and the opening `<login-config>` tags:

```xml
<servlet>
	<servlet-name>CXFServlet</servlet-name>
	<servlet-class>
		org.apache.cxf.jaxrs.servlet.CXFNonSpringJaxrsServlet
	</servlet-class>
	<init-param>
		<param-name>jaxrs.serviceClasses</param-name>
		<param-value> com.sap.hana.cloud.samples.weatherapp.api.AuthenticationService</param-value>
	</init-param>
	<load-on-startup>1</load-on-startup>
</servlet>
<servlet-mapping>
	<servlet-name>CXFServlet</servlet-name>
	<url-pattern>/api/v1/*</url-pattern>
</servlet-mapping>
```

![Modifying the web.xml file for REST services](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part5/e2e_05-5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Save and publish changes)]

With this, we have registered (Apache) CXF as a Servlet that listens to incoming requests using the URL-pattern: `/api/v1/*`. Furthermore, we registered our `AuthenticationService` class as one of the RESTful services. During start-up, CXF will introspect the class and use the provided JAX-RS annotations to properly configure our service.

Save your changes and publish/deploy your application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](View the app)]

Navigate to the following URL: <http://localhost:8080/weatherapp/api/v1/auth>. After successful authentication you should see your username.

![Running the Java app with RESTful API enabled](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part5/e2e_05-7.png)

[DONE]
[ACCORDION-END]




## Next Steps
- [End-to-End Weather App Scenario Part 6](http://www.sap.com/developer/tutorials/hcp-java-weatherapp-part6.html)
