---
parser: v2
author_name: René Jeglinsky
author_profile: https://github.com/renejeglinsky
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, programming-tool>java, software-product>sap-business-application-studio]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

# Deploy CAP Java App to SAP Business Technology Platform
<!-- description --> Deploy the recently built bookstore application to SAP Business Technology Platform using the Cloud Foundry CLI.

## You will learn
  - How to create a Cloud Foundry application manifest
  - How to deploy an application to SAP Business Technology Platform Cloud Foundry (SAP BTP) environment

## Intro
In the previous tutorial you made your application ready to run with SAP HANA. In this tutorial, you will deploy the application to SAP BTP, Cloud Foundry environment and have it running fully in the cloud.

---

### Create a Cloud Foundry application manifest


When deploying an application to Cloud Foundry, you can use a manifest to describe attributes of the deployed application. The manifest can be used together with the Cloud Foundry CLI command `cf push` to deploy the application.

1. Go to the `~/projects/bookstore` folder and **create a new file** called **`manifest.yml`**.

2. Add the following code to the newly created file and make sure you **Save** the file.

    ```YAML
    ---
    applications:
    - name: bookstore
      path: srv/target/bookstore-exec.jar
      random-route: true
      buildpacks:
      - java_buildpack
      env:
        JBP_CONFIG_OPEN_JDK_JRE: '{ jre: { version: 17.+ }}'
        JBP_CONFIG_SPRING_AUTO_RECONFIGURATION: '{enabled: false}'
        SPRING_PROFILES_ACTIVE: cloud
      services:
      - bookstore-hana
    ```

The manifest describes the name of the application, the path where the application archive can be found and runtime type. In this tutorial the Java buildpack is used to deploy your Spring Boot application from a single JAR archive using Java 17.

The route of the application, meaning the HTTP endpoint where it will be available, will be randomized to prevent clashes with other application routes.

The name of SAP HANA service instance you created in the previous tutorial is used here under the services section (`bookstore-hana`).

When your application will be deployed to SAP BTP, it will use this database to store data instead of the in-memory database. In the previous tutorial you added the additional Java system property `-Dspring-boot.run.profiles=cloud` to your application to ensure that. When deploying the application to Cloud Foundry this is set with environment variable `SPRING_PROFILES_ACTIVE`.


### Enable application for Cloud Foundry


Cloud Foundry uses the Open Service Broker API to provide services to applications. When running your application on Cloud Foundry, an environment variable `VCAP_SERVICES` (similar to the content of the `default-env.json`) is available, which contains all required service credentials. CAP Java can automatically read this environment variable and configure your application to use the SAP HANA database. In addition you want to make sure that your application is secure by default, when you deploy it to the cloud. The required dependencies for these aspects are included in the `cds-starter-cloudfoundry` dependency bundle. It also includes the `cds-feature-hana` dependency you added earlier.

1. Edit the `pom.xml` in the `srv` directory (not the `pom.xml` file located in the root project folder) and under the `<dependencies>` tag replace the `cds-feature-hana` dependency you added in the previous tutorial with the `cds-starter-cloudfoundry` dependency. Make sure you **Save** the file.

    ```xml
    <dependency>
        <groupId>com.sap.cds</groupId>
        <artifactId>cds-starter-cloudfoundry</artifactId>
    </dependency>
    ```




### Push the application


You are now ready to push your application to the cloud by running the following commands from the terminal in SAP Business Application Studio:

1. Make sure that you are in the root of the bookstore project:

    ```Shell/Bash
    cd ~/projects/bookstore
    ```

2. Build your application once by running:

    ```Shell/Bash
    mvn clean install
    ```

    The log output should be similar to this one:

    <!-- border -->![screenshot build log output](expected-build-output.png)

3. Push the application to the cloud by running:

    ```Shell/Bash
    cf push
    ```

     The manifest will be [automatically picked up](https://cli.cloudfoundry.org/en-US/cf/push.html).


    > Provide the credentials you usually log in to SAP BTP if you are asked to log in.

4. To retrieve the application URL run the following command:

    ```Shell/Bash
    cf app bookstore
    ```

    <!-- border -->![console output of cf apps](cf-app-route.png)


5. Open the application in the browser. The according route can be found under `routes` of the previous step.

6. Observe that your application is now secured by requiring authentication on service and entity endpoints. In the following tutorials you will learn how to configure authentication and authorization locally and in the cloud.

---
