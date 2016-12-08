---

title: Continuous Integration (CI) Best Practices with SAP: Java Web on SAP HANA Cloud Platform with CI on Cloud
description: Part 5.2: Configuring cloud-based CI system for Maven-based Java Web on SAP HANA Cloud Platform project.
tags: [  tutorial>intermediate, tutorial:type/project ]

---

## Prerequisites

  - **Proficiency:** Intermediate
  - [Generic Project with CI on Cloud](http://www.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html)
  
## Next Steps


  - [Back to the Navigator](http://www.sap.com/developer/tutorials/ci-best-practices-intro.html)

---

This section continues what is discussed in [Generic Project (Pure Java) Using Cloud Services](http://www.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html), applying the approach using GitHub and Travis CI as cloud services to sample code that is delivered as part of the SDK installation for Java Web development. The sample discussed here contains some web applications that are built using Maven and share a common parent `pom.xml` file. Documentation and resources for Java web application development include the following:

> Documentation: https://help.hana.ondemand.com/help/frameset.htm?e66f3eecbb5710148397a19b46c4979b.html  
> Tutorial: https://hcp.sap.com/developers/TutorialCatalog/jav100_2_java_hello_world.html  
> SDK installation guide: https://help.hana.ondemand.com/help/frameset.htm?7613843c711e1014839a8273b0e91070.html  
> SDK Download: https://tools.hana.ondemand.com/#cloud

This section goes beyond the pure build and point out how to add a post-build step to deploy the application to SAP HANA Cloud Platform.

![Landscape using GitHub and Travis CI](java-hcp-cloud-1.png)

Figure 1: Landscape using GitHub and Travis CI


### Basic Setup


Follow the instructions for creating the GitHub project and a Travis CI build as described in [Generic Project (Pure Java) Using Cloud Services](http://www.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html). As sources, use the sample project from the `samples` folder of the SAP HANA Cloud Platform SDK installation zip that you can download from the link below.

> SDK installation guide: https://help.hana.ondemand.com/help/frameset.htm?7613843c711e1014839a8273b0e91070.html  
> Downloads: https://tools.hana.ondemand.com/#cloud

This example uses the project name `java_hcp_project`.

#### Procedure

1. In GitHub, create a new repository named `java_hcp_project` and clone it to your local machine as described in [Generic Project (Pure Java) Using Cloud Services](http://www.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html), steps 1-4.

2. The `samples` folder in the SAP HANA Cloud Platform SDK contains a Maven parent project that includes a couple of modules. Copy the sources of this Maven project into your cloned `java_hcp_project` repository root folder. For this example, we are going to use only the `explore-ui5` module; use comments in the parent `pom.xml` file to not include the other modules in the Gerrit project:

    ```
    <modules>
    <!--
        <module>...
           ...</module>
    -->
        <module>explore-ui5</module>
    <!--
        <module>...
           ...</module>
    -->
    </modules>
    ```

3. Continue setting up the GitHub project and Travis CI build as described in [Generic Project (Pure Java) Using Cloud Services](http://www.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html), steps 6-11. The result is a successful build of the `explore-ui5` application on Travis CI.


### Deployment to SAP HANA Cloud Platform

The tools that are required for deployment to SAP HANA Cloud Platform are already referenced in the project's `pom.xml` file. The next step is to configure the deployment parameters.


#### Procedure

##### Enhance the `pom.xml` file to install the SAP HANA Cloud Platform SDK automatically during the build step.

1. In the sources of the sample project, open the parent `pom.xml` file.

2. In the property definition, change the value of `${sap.cloud.sdk.path}` as follows:

    ```
    ...
      <properties>
        ...
        <sap.cloud.sdk.path>${project.build.directory}/sdk</sap.cloud.sdk.path>
        ...
      </properties>
    ...
    ```

3. In the section that defines the profile `cloud-integration-tests`, add the following lines to tell the Maven plugin to install the SDK and where:

    ```
    ...
      <profile>
        <id>cloud-integration-tests</id>
        <build>
          <plugins>
            <plugin>
              <groupId>com.sap.cloud</groupId>
              <artifactId>${sap.cloud.sdk.plugin}</artifactId>
              <executions>
                ...
                <execution>
                  <phase>initialize</phase>
                  <goals>
                    <goal>install-sdk</goal>
                  </goals>
                </execution>
                ...
              </executions>
              ...
            </plugin>
            ...
          </plugins>
        </build>
      </profile>
    ...
    ```
     
    > Documentation: https://help.hana.ondemand.com/mavenSite/usage.html


##### Configure the Travis build and make the `pom.xml` file configurable through the environment.

Travis CI enables you to customize the build, that is, to define environment variables, which control the deploy target. We will use those variables to provide the HCP account and credentials.

1. Open the `pom.xml` file and make the following changes in the `properties` section:
     
    ```
    ...
      <properties>
      ...
        <sap.cloud.password>${env.SAP_CLOUD_PASSWORD}</sap.cloud.password>
        <sap.cloud.host>${env.SAP_CLOUD_HOST}</sap.cloud.host>
        <sap.cloud.username>${env.SAP_CLOUD_USERNAME}</sap.cloud.username>
        <sap.cloud.account>${env.SAP_CLOUD_ACCOUNT}</sap.cloud.account>
      ...
      </properties>
    ...
    ```
    
    This ensures that the HCP access information is taken from the build environment, and we don't need to include them in the `pom.xml` file.
  
2. Add the following line, which forces Travis to call Maven using the correct profile, to the `.travis.yml` file:

    ```
    script: mvn install -P cloud-integration-tests
    ```
     
3. Open Travis CI and go to your project. Select **More options > Settings** and add the values below. You must switch off **Display value in build log** to avoid making your settings public.

    Field                | Value                               
    :------------------- |:------------------------------------
    `SAP_CLOUD_USERNAME` | {The user name of your HCP account}
    `SAP_CLOUD_ACCOUNT`  | {The name of your HCP account}
    `SAP_CLOUD_HOST`     | `hanatrial.ondemand.com`

    Although the environment variables are not visible, they are stored in clear text; the HCP password must not be handled like this, so the next step is to encrypt it properly.

4. On your local machine, install Ruby and the Travis command line client:

    > https://docs.travis-ci.com/user/encryption-keys/

5. In your local clone of the GitHub repository, execute the following command in the directory where the `.travis.yml` file is located:
  
    ```
    travis encrypt SAP_CLOUD_PASSWORD={your HCP password} --add env.global
    ```

    This command automatically adds an encrypted environment entry into the `.travis.yml` file. The content of `travis.yml` should look similar to the following:
     
    ```
    sudo: false
    jdk: oraclejdk7
    script: mvn install -P cloud-integration-tests
    env:
      global:
        secure: SdKbOJMBDLU4W6GJfK0n...
    ```
     
6. Open Travis CI and go to your project. Select **More options > Settings**, switch **Limit concurrent jobs** on and enter `1` as the value to prevent concurrent builds from colliding with the deployments on HCP.
  
    ![Build configuration in Travis](java-hcp-cloud-3.png)

    > Travis documentation: https://docs.travis-ci.com/user/customizing-the-build
    
7. Use Git to commit the changes, then push them to GitHub. Monitor the statuses of the build in Travis CI and of the application in the SAP HANA Cloud Platform Cockpit.


#### Further Refinements

1. If you have more than one parallel build:  
    In the example, the number of parallel builds is restricted to one to avoid conflicts in deployment. You can circumvent this restriction by providing dynamic names for the deployed application. In the property definition block of the `pom.xml` file, apply the following change:  

    ```
    <sap.cloud.application>${env.TRAVIS_BRANCH}${env.TRAVIS_BUILD_NUMBER}</sap.cloud.application>
    ```

    The application name is generated from the branch and the build number. Any build produces a unique application name that is also well-categorized by branch. Ensure that the resulting name adheres to the application naming rules for HCP.  
     
    > Rules for deployment: https://help.hana.ondemand.com/help/frameset.htm?937db4fa204c456f9b7820f83bc87118.html
     
2. Restricting builds for branches:  
    If only a few branches should be built automatically, add a build restriction into the `.travis.yml` file as shown here:

    ```
    branches:
      only:
        - master
    ```
     
    > Documentation: https://docs.travis-ci.com/user/customizing-the-build/#Building-Specific-Branches

More sophisticated control mechanisms are provided by Travis' build matrix:

> Documentation: https://docs.travis-ci.com/user/customizing-the-build/#Build-Matrix
  

## Next Steps

  - [Back to the Navigator](http://www.sap.com/developer/tutorials/ci-best-practices-intro.html)
  
