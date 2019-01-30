---
title: Create a sample application on Cloud Foundry using SAP S/4HANA Cloud SDK
description: The following steps will explain how to create the very first Hello World example on Cloud Foundry using the SAP S/4HANA Cloud SDK.
tags: [ tutorial>intermediate, products>sap-s-4hana-cloud-sdk, products>sap-s-4hana, products>sap-cloud-platform, topic>cloud, topic>java ]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites  
 - **Proficiency:** intermediate

### You will learn  
This tutorial will cover your first steps when developing applications for SCP Cloud Foundry using SAP S/4HANA Cloud SDK. You will create an account for SCP Cloud Foundry and setup the Cloud Foundry command line interface for deploying and managing Cloud Foundry applications. Then you will generate your first project using the SAP S/4HANA Cloud SDK Maven archetype and deploy your first application to SCP Cloud Foundry.

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Setup for Cloud Foundry)]

In order to deploy applications to `SCP Cloud Foundry`, you need to create a free trial account. You can create your account by visiting <https://cloudplatform.sap.com/try.html>

After creating your account and activating it via email, you can log in to your personal `Cloud Cockpit`. For your first visit, it should look like this:

![SAP Cloud Platform Cockpit first view](cloudplatform-cockpit-first-view.png)

Now click the `Home` button in the upper navigation bar and then click `Start Cloud Foundry Trial`.

![SAP Cloud Platform Cockpit start trial view](cloudplatform-cockpit-start-cf-trial1.png)

After selecting your region, your account will be automatically set up for development with `Cloud Foundry`.

Now that your account is activated and configured, you will need the `Cloud Foundry` command line interface (`cf CLI`) to deploy and manage your `Cloud Foundry` applications.

To install `cf CLI`, you can either grab the latest release on the official release page or use your favorite package manager.

In order to deploy applications on `SAP Cloud Foundry` you need to provide `cf CLI` with an API endpoint. The API endpoint depends on the region you chose for your account:

  - for EU: `https://api.cf.eu10.hana.ondemand.com`
  - for US EAST: `https://api.cf.us10.hana.ondemand.com`
  - for US CENTRAL: `https://api.cf.us30.hana.ondemand.com`


Now enter the following commands (in this case for the EU region):

```
cf api https://api.cf.eu10.hana.ondemand.com
cf login
```

The `cf CLI` will ask you for your mail and your password. After entering these, you should be successfully logged in.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Generate project from archetype)]

To generate your first project from the Maven archetype, run the following command:

```
mvn archetype:generate -DarchetypeGroupId=com.sap.cloud.s4hana.archetypes -DarchetypeArtifactId=scp-cf-tomee -DarchetypeVersion=LATEST
```
During the generation process, Maven will require additional parameters to form your project:

**`groupId`** - An identifier representing your group, company or organization (e.g. `com.sap.cloud.sdk.tutorial`)
**`artifactId`** - An identifier for your application (e.g. `firstapp`)
**`version`** - The version of your application (e.g. `1.0-SNAPSHOT`)
The version of your application (e.g. `1.0-SNAPSHOT`)
**`package`** - The name of the top-level package your source code will reside in (typically equal to your **`groupId`**, e.g. `com.sap.cloud.sdk.tutorial`). Please pay attention to package and directory names in any upcoming source code when using a different package name than suggested here.
**`uniqueHostname`** - A unique identifier to determine your initial project URL on `Cloud Foundry`. This value must be unique across your `Cloud Foundry` region, but it can easily be changed later on. We recommend _Application Name + some random number_, e.g. `firstapp-D123456` (please, use a different number than D123456 - this is likely already taken by another developer trying out their sample application).

After providing these values, Maven will generate your project from the archetype.

![Maven generates project from archetype](maven-generates-project.png)

**Note**: Here you have created an application which is based on the `TomEE` runtime which is `Java EE 6` compliant open source runtime that is available in the `Cloud Foundry` platform if your goal is to create a `Java EE` application. You may also initialize the project with `SpringBoot` (`artifactId`: `scp-cf-spring`) or on a pure `Tomcat` container (`artifactId`: `scp-cf-tomcat`). Our tutorial series will be primarily based on the `TomEE` runtime. Nonetheless, the SAP S/4HANA Cloud SDK is compatible with these popular `runtimes` too.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Understand the project structure and its artifacts)]

Now you can open your favorite IDE and import the project as `Maven Project`. After importing the project into your IDE, the project structure should look like this:

![structure of project files](project-files.png)

The first thing you will notice is the different directories:

  - `application`
  - `cx-server`
  - `integration-tests`
  - `unit-tests`

These are Maven `submodules`, and they serve different aspects of your code application, test and deployment environment. The following separation of modules makes it possible to run dedicated unit tests and sensitive integration tests without deploying the application.

#### Multiple modules project

The advantage of operating a multiple modules project for your application becomes apparent as soon as the software complexity rises. Then it gets convenient to dedicate code distribution and responsibility to developers for either application or test environment. In terms of reliability and continuance, you will see that front-end testing and test automation are as important as classic back-end testing of your project. These fields of expertise require different programming paradigms, as well as different kinds of development life cycles. To ensure the overall software stability and reliability, a multiple modules setup is the best practice solution.
To get you started, we take a look into the conventional `application` project, as well as the classic `unit tests`. Then the `integration tests` follow, used for code tests with external servers and resources. Once software testing is covered, we briefly introduce the `Cx server` for continuous integration and delivery.

**`application`** contains the source code and configuration of your actual web application.

![project files in application folder](project-files-application-folder.png)

  - `src/main/java` - Here goes your production code, nothing else. As you can see, there's already the `HelloWorldServlet`, which we will look at in more detail soon.
  - `src/main/resources` - Anything that you require in your production code but is not compilable code goes here (typically things like API definition files for `RAML` or `OpenAPI`, `Database Migration Files` for `Flyway` or `Liquibase`)
  - `src/main/webapp` - contains the deployment descriptor for your web application `web.xml`
  - `src/test/java` - Additional test classes.
  - `src/test/resources` - Additional resources for attached test modules.
  - `pom.xml` - This is your project management file for Maven where you can maintain other open source dependencies or use plugins that simplify your build environment.

**`unit-tests`** contains the unit tests for your application. Its structure is similar to application but it exclusively holds test classes and resources. The purpose of this module is to test and validate single aspects of data flow and computational operations in the application project.

![project files in unit tests folder](project-files-unit-tests.png)

  - `src/test/java`	- This is the place for your automated tests.
  - `src/test/resources` - Tests may also require additional resources to work properly such as configuration files. This is their place.

**`integration-tests`** contains the integration tests for your application. Its structure is similar to **`application`**.

![project files in integration tests folder](project-files-integration-tests.png)

  - `src/test/java`	- Here you can put all your integration tests. As you can see, it already contains `HelloWorldServiceTest` corresponding to the `HelloWorldServlet`.
  - `src/test/resources` - Here are all the resources needed for the integration tests to run or validate.

**`cx-server`** contains the script and configuration file to manage your best practice continuous integration and delivery software environment (`Cx`). The including files allow `Linux` users to simply create your very own `Cx server` as part of a `Docker` deployment. `Jenkins` is the server which will be run. This automation server helps to manage all technical steps of a software development process.

![project files in cx server folder](project-files-cx-server.png)

  - `cx-server`	- This `Unix` bash script allows you to start and stop the `Jenkins` server on your local machine as part of a `Docker` container.
  - `server.cfg` - his is the configuration file for the server parameters.

Once a Jenkins server is configured for your personal needs, the files in the project root directory become useful:

**`Jenkinsfile`** - This text file contains the definition of a `Jenkins Pipeline` and stays part of your project source code. It defines what steps are run specifically for your application.
**`pipeline_config.yml`** - This is the configuration file for your specific application.
**`manifest.yml`** is the deployment descriptor for `Cloud Foundry`. We will take a closer look at this file once we deploy the application.

#### Unit tests and integration tests

This separation of test modules makes it possible to just run unit tests and integrations test without deploying, as well as deploying the application without running time consuming tests. Unit tests can either be kept publicly inside the application module, or in the separate `unit-tests` folder that is part of the archetype. For that topic we highly recommend the articles and educational videos by Martin Fowler. For a start we advise reading his post about [Unit Tests](https://martinfowler.com/bliki/UnitTest.html).

During development it becomes important to test the code newly implemented to external services, i.e. logic running in a distributed environment. This is where the integration tests are an important tool to ensure correctness and stability over the whole internal and external deployment. Since the integration tests may contain confidential information, like business logic and test access tokens, it can be helpful to maintain their operation inside a dedicated Maven `submodule`. That way the runnable application itself can be later shipped without tests and their dependency.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](HelloWorldServlet)]

Now that you understand the structure of the project, let's take a closer look at the `HelloWorldServlet`.

```java
package com.sap.cloud.sdk.tutorial;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@WebServlet("/hello")
public class HelloWorldServlet extends HttpServlet
{
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LoggerFactory.getLogger(HelloWorldServlet.class);

    @Override
    protected void doGet( final HttpServletRequest request, final HttpServletResponse response )
        throws ServletException, IOException
    {
        logger.info("I am running!");
        response.getWriter().write("Hello World!");
    }
}
```

The `HelloWorldServlet` extends `HttpServlet`, so this will be a HTTP endpoint that we can visit. We map this endpoint to the `/hello` route using `@WebServlet("/hello")`. By overriding the function `doGet`, we define what happens when a client performs an HTTP GET request on the `/hello` route. In this case we simply write a response containing **`Hello World!`**

**Note**: The application code runs seamlessly in `SAP Cloud Platform`, `Neo` as well as `SAP Cloud Platform`, `Cloud Foundry`. The `SAP S/4HANA Cloud SDK` is compatible with both versions and provides mechanisms to seamlessly transfer code between both environments.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deployment)]

In order to deploy the application, we first need to assemble our project into a deployable artifact – a `.war` file. Open your command line and change into the `firstapp` directory, the root directory of your project and run the following command:

```
cd /path/to/firstapp
mvn clean package
```

This tells Maven to remove any files from previous assemblies (clean) and to assemble our project (package). The project is set up so that Maven automatically runs your unit and integration tests for you. After running the command, there should be a directory target inside of the `application` directory, containing a file called `firstapp-application.war`. This is the file that we will deploy to `Cloud Foundry` (or locally).

**Deploy to Cloud Foundry**

Now the previously mentioned `manifest.yml` comes into play – it's the deployment descriptor used by `Cloud Foundry`.


```
  ---
  applications:

  - name: firstapp
    memory: 512M
    host: firstapp-D123456
    path: application/target/firstapp-application.war
    buildpack: sap_java_buildpack
    env:
      TARGET_RUNTIME: tomee
      ALLOW_MOCKED_AUTH_HEADER: true
```

Here we can provide additional application specific environment variables. For example, we specify that we want to use a `TomEE` container as our target runtime. Also we set a flag to mock authentication headers, since you have not yet implemented any [security measures](https://blogs.sap.com/2017/07/18/step-7-with-sap-s4hana-cloud-sdk-secure-your-application-on-sap-cloud-platform-cloudfoundry/).

The manifest contains a list of applications that will be deployed to `Cloud Foundry`. In this example we only have one application, `firstapp`, with the following parameters:

  - **`name`**	- This is the identifier of your application within your organization and your space in `SCP Cloud Foundry`.
  - **`memory`** -	The amount of memory allocated for your application.
  - **`host`** -	Determines the URL of your application after deploying it (this is where the `uniqueHostname` from the generation process is being used).
  - **`path`** -	The relative path to the artifact to be deployed.
  - **`buildpack`** -	A `buildpack` is what `Cloud Foundry` uses to build and deploy your application. Since this is a Java application, we use `sap_java_buildpack`.
  - **`env`**	- Here we can provide additional application specific environment variables. For example we specify that we want to use a `TomEE` container as our target runtime.

Now you can deploy the application by entering the following command:

```
cf push
```

`cf push` is the command used to deploy applications. The `-f` flag provides `cf CLI` with the deployment descriptor.

  _Hint: If you omit the `-f` flag,  `cf CLI` will check whether there is a file named `manifest.yml` in the current directory. If so, `cf CLI` will use this file as deployment descriptor. Else, it will fail._

After the deployment is finished, `cf CLI`'s output should look like this:

![cd push output](cf-push.png)

Now we can visit the application under its corresponding URL. Take the value from `cf CLI`'s `urls: ...` and append the `hello` path:

![deployed application look](deployed-application.png)

_Hello World!_

That's it.

**Run the Application on a Local Server**

[Since version 1.1.1 of the SAP S/4HANA Cloud SDK](https://sap.github.io/cloud-s4-sdk-examples/release-notes/), the generated projects can also be run locally out-of-the-box. To do so, first assemble the project using the `mvn clean package` command (see above).

Then, run the following commands to start the local server:

```
cd application
mvn tomee:run
```

Now you have a strong basis for developing your own cloud application for `SCP Cloud Foundry` using the `SAP S/4HANA Cloud SDK`. In the following tutorials you will learn about more advanced uses of the `SAP S/4HANA Cloud SDK`.

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Troubleshooting)]

`Windows PowerShell`

If you are using `PowerShell` on `Windows`, always put Maven arguments (supplied with `-D`) in quotes, for example:

```
mvn archetype:generate "-DarchetypeGroupId=com.sap.cloud.s4hana.archetypes" "-DarchetypeArtifactId=scp-cf-tomee" "-DarchetypeVersion=LATEST"
```

[ACCORDION-END]
