---
title: Getting started with Spring Boot
description: Learn how-to develop a simple Spring Boot sample application that can be deployed to SAP Cloud Platform
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, topic>cloud, topic>java, products>sap-cloud-platform, tutorial>how-to ]
---
## Prerequisites  
  - **Proficiency:** Beginner
  - **Software Requirements:** Java, Maven
  - **Tutorials:** [Getting Started with the SAP Cloud Platform Tools for Java](https://hcp.sap.com/developers/TutorialCatalog/jav100_01_java_setup_eclipse.html)





## How-To Details
This document will teach you how-to develop a simple web application using [Spring Boot](http://projects.spring.io/spring-boot/). Given Spring Boot's approach of embedding a servlet container (such as Tomcat, Jetty or Undertow) directly into an executable `JAR` instead of creating a `WAR` file that needs to be explicitly deployed to a runtime environment, this aspect will be the main focus of this how-to exercise. For the sake of simplicity, we keep the rest of the coding fairly basic in order to not distract you from the important stuff.

### Time to Complete
**10 Min**.

---

1. Create a new `Maven Project` using the respective wizard.

    - Check the `Create a simple project (skip archetype selection)` option at the top
    - Select your `workspace location` of choice
    - (Optional) Add the to-be-created project to a new or existing working set
    - Choose **Next**

    During the course of this exercise we will use the following data:

    Field               | Value
    :------------------ | :------------------
    Group Id     		| `com.sap.hana.cloud.samples`
    Artifact Id     	| `cloud-spring-boot-sample`
    Versioning         	| `0.0.1-SNAPSHOT`
    Packaging				| `WAR`
    Name					| `cloud-spring-boot-sample`

    - Leave the rest of the fields empty and click on **Finish**

2. Now comes the _'magic'_ part of it. First, we will we will update the `pom.xml` file to add a reference to the Spring Boot `parent` project, from which our project will inherit sensible defaults and configurations. Furthermore, we will add code to include so-called `starters`, which basically define a set of dependencies.

    - Open the `pom.xml` file
    - Copy the `parent` information snippet below and paste them just underneath the `<name></name>` attribute

    ```xml
     <parent>
		  <groupId>org.springframework.boot</groupId>
		  <artifactId>spring-boot-starter-parent</artifactId>
		  <version>1.4.1.RELEASE</version>
		  <relativePath /> <!-- lookup parent from repository -->
	  </parent>
    ```

    - Add the following properties just below the `<parent></parent>` section we just added  

    ```xml
    <properties>
	   <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
	   <project.reporting.outputEncoding>UTF-8</project.reporting.outputEncoding>
	   <java.version>1.8</java.version>
	   <m2eclipse.wtp.contextRoot>/</m2eclipse.wtp.contextRoot>
	</properties>

    ```
    - Now adding the `starters` dependencies

    ```xml
    <dependencies>
        <!-- developer tools for hot code-replacement etc. -->
        <dependency>
			  <groupId>org.springframework.boot</groupId>
			  <artifactId>spring-boot-devtools</artifactId>
			  <optional>true</optional>
		 </dependency>

        <!--Embedded tomcat etc. -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-tomcat</artifactId>
            <scope>provided</scope>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-logging</artifactId>
            <scope>provided</scope>
        </dependency>
    </dependencies>
    ```
    > Note: The first dependency to `spring-boot-devtools` is optional and just something that comes in quite handy to accelerate development via hot-code replacement etc. Please refer to [Developer Tools](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#using-boot-devtools) for more details.

    > Please also note that we explicitly specified the last two `dependencies` as `provided`, hereby specifying that these `dependencies` will not be included in the resulting `WAR` file once a build is triggered. The reason we do this is that we don't need an embedded Tomcat (see [Tomcat 8](https://help.hana.ondemand.com/help/frameset.htm?fd6b72f17a11478e87fefe3f6ad2e30d.html)), nor the logging frameworks as the target runtime of SAP Cloud Platform already provides them out-of-the-box; hence including these libraries would only lead to strange classcloading issues. Please refer to the [online documentation](https://help.hana.ondemand.com/help/frameset.htm?e6e8ccd3bb571014b6afdc54744eef4d.html) for further details.

3. (Optional) If you intend to deploy the application to a Cloud Foundry environment, such as the SAP Cloud Platform, Starter Edition for Cloud Foundry Services (Beta), then we would need to include the logging frameworks. In order to facilitate this setup we have introduced Maven `profiles` so that we can simply trigger a `build` for the respective deployment platform.

    - Copy the `profiles` information snippet below and paste them just underneath the `<dependencies></dependecies>` section

    ```xml
    	<profiles>
		<!-- local/development profile -->
		<profile>
			<id>dev</id>
			<activation>
				<activeByDefault>true</activeByDefault>
			</activation>
		</profile>

		<!-- CF profile -->
		<profile>
			<id>cf</id>
			<activation>
				<activeByDefault>false</activeByDefault>
			</activation>
			<dependencies>
				<dependency>
					<groupId>org.springframework.boot</groupId>
					<artifactId>spring-boot-starter-logging</artifactId>
				</dependency>
			</dependencies>
		</profile>
	</profiles>
    ```
     > Note: You can trigger the `build` to create a `WAR` file that includes the `spring-boot-starter-logging` dependencies as needed for Cloud Foundry by running the following command: `mvn -P cf build package`.

     > Note: The complete `pom.xml` used in this how-to document can be obtained   as a reference from the [respective Github repository](https://github.com/SAP/cloud-spring-boot-sample/commit/e03faffb2e2b6fbea039ce5341dc90efa45b9755).

4. Create a main `Application` class as needed to bootstrap a Spring Boot web application:

    - Create a class called `Application` in package `com.sap.hana.cloud.samples.springboot`
    - Copy & paste the following content into the newly created class (hereby replacing the original content of the class)

    ```java
    package com.sap.hana.cloud.samples.springboot;

    import org.springframework.boot.SpringApplication;
    import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
    import org.springframework.boot.builder.SpringApplicationBuilder;
    import org.springframework.boot.web.support.SpringBootServletInitializer;
    import org.springframework.context.annotation.ComponentScan;
    import org.springframework.context.annotation.Configuration;

    @Configuration
    @ComponentScan
    @EnableAutoConfiguration
    public class Application extends SpringBootServletInitializer
    {

        @Override
        protected SpringApplicationBuilder configure(SpringApplicationBuilder application)
        {
            return application.sources(Application.class);
        }

	    public static void main(String[] args)
	    {
		     SpringApplication.run(Application.class, args);
	    }
    }

    ```


    > Note: Please refer to the official Spring Boot documentation for more details about the `SpringBootServletInitializer` interface implemented here as an alternative to the classic `web.xml` approach. See: [Traditional deployment](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#howto-traditional-deployment).


4. Next, we'll create a simple `Controller` class to render the classic _"Hello World!"_.

    - Create a class called `HomeController` in the package `com.sap.hana.cloud.samples.springboot.web`
    - Copy & paste the following content into the newly created class (hereby replacing the original content of the class)    

    ```java
    package com.sap.hana.cloud.samples.springboot.web;

    import org.springframework.web.bind.annotation.RequestMapping;
    import org.springframework.web.bind.annotation.RestController;

    @RestController
    public class HomeController
    {
	    @RequestMapping("/")
	    public String home()
	    {
		    return "Hello again!";
	    }
    }
    ```
5. That's it! You can now run / build your project. In order to just run it as a stand-alone application (as propagated by Spring Boot) simply execute the following command:

    `mvn spring-boot:run`

    In order to build a `WAR` file to be deployed to SAP Cloud Platform run the following command:

    `mvn clean package`

    > Note: Please refer to the [official online documentation](https://help.hana.ondemand.com/help/frameset.htm?e5dfbc6cbb5710149279f67fb43d4e5d.html) in case you need further instructions on how-to deploy an app to SAP Cloud Platform.


### Related Information
 - (Github) [https://github.com/SAP/cloud-spring-boot-sample](https://github.com/SAP/cloud-spring-boot-sample)
 - (Blog) [Getting started with Spring Boot – Part 1](https://blogs.sap.com/2016/10/14/getting-started-spring-boot-part-1/)


