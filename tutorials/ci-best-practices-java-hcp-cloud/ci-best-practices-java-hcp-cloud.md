---

title: Continuous Integration (CI) Best Practices with SAP: Java Web on SAP HANA Cloud Platform with CI on Cloud
description: Part 5.2: Configuring cloud-based CI system for Maven-based Java Web on SAP HANA Cloud Platform project.
tags: [  tutorial>intermediate, tutorial:type/project ]

---

## Prerequisites

  - **Proficiency:** Intermediate
  - [Generic Project with CI on Cloud](http://go.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html)
  
## Next Steps


  - [Back to the Navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)

---

This section continues what was discussed in part [Generic Project (Pure Java) Using Cloud Services](http://go.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html).
We apply the approach using GitHub and Travis CI as cloud services to sample code that is delivered as part of the SDK installation for Java Web development.
This sample contains some web applications that are built using Maven and share a common parent `pom.xml` file. Documentation and resources for Java web application development:

> Documentation: https://help.hana.ondemand.com/help/frameset.htm?e66f3eecbb5710148397a19b46c4979b.html  
> Tutorial: https://hcp.sap.com/developers/TutorialCatalog/jav100_2_java_hello_world.html  
> SDK installation guide: https://help.hana.ondemand.com/help/frameset.htm?7613843c711e1014839a8273b0e91070.html  
> SDK Download: https://tools.hana.ondemand.com/#cloud

We go beyond the pure build and point out how to add a post-build step to deploy the application to SAP HANA Cloud Platform.

![Landscape using GitHub and Travis CI](java-hcp-cloud-1.png)

Figure 1: Landscape using GitHub and Travis CI


### Basic Setup


Follow the instructions for creating the GitHub project and a Travis CI build as described in the section [Generic Project (Pure Java) Using Cloud Services](http://go.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html). As sources, use the sample project from the `samples` folder of the SAP HANA Cloud Platform SDK installation zip that you can download from the link below.

> SDK installation guide: https://help.hana.ondemand.com/help/frameset.htm?7613843c711e1014839a8273b0e91070.html  
> Downloads: https://tools.hana.ondemand.com/#cloud

In this example, we use the name `java_hcp_project` for the project.

#### Procedure

1. Create a new repository named `java_hcp_project` in GitHub and clone it to your PC as described in [Generic Project (Pure Java) Using Cloud Services](http://go.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html), steps 1-4.

2. The `samples` folder in the SAP HANA Cloud Platform SDK contains a Maven parent project with a couple of modules. Copy the sources of this Maven project into your cloned `java_hcp_project` repository root folder. We do not want to build all the modules in this example but restrict ourselves on the `explore-ui5` module only. The others do not need to be included into the Gerrit project, and in the parent `pom.xml` file, place them into comments:

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

3. Continue setting up the GitHub project and Travis CI build as described in [Generic Project (Pure Java) Using Cloud Services](http://go.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html), steps 6-11. The result is a successful build of the `explore-ui5` application on Travis CI.


### Deployment to SAP HANA Cloud Platform

What is needed for deployment to SAP HANA Cloud Platform is already prepared in the project's `pom.xml` file. We configure now specific details of the Travis CI build.


#### Procedure

##### Enhance the `pom.xml` file to install the SAP HANA Cloud Platform SDK automatically during the build step.

1. In the sources of the sample project, open the parent `pom.xml` file.

2. In the property definition part, change the value of `${sap.cloud.sdk.path}` as follows:

    ```
    ...
      <properties>
        ...
        <sap.cloud.sdk.path>${project.build.directory}/sdk</sap.cloud.sdk.path>
        ...
      </properties>
    ...
    ```

3. In the section that defines the profile `cloud-integration-tests`, add the following lines to tell the maven plugin to install the SDK and where:

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

Travis CI enables you to customize the build, that is, to define environment variables, which control the deploy target. We will use that to provide the HCP account and credentials.

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
    
    This will make sure that the HCP access information is taken from the build environment and we don't have to put them into the `pom.xml` file directly.
  
2. In the `.travis.yml` file add the following line:

    ```
    script: mvn install -P cloud-integration-tests
    ```

    This forces Travis to call Maven with the right profile.
     
3. Open Travis CI and go to your project. Select **More options > Settings** and add the values below. You must switch off **Display value in build log** to avoid making your settings public.

    Field                | Value                               
    :------------------- |:------------------------------------
    `SAP_CLOUD_USERNAME` | {The user name of your HCP account}
    `SAP_CLOUD_ACCOUNT`  | {The name of your HCP account}
    `SAP_CLOUD_HOST`     | `hanatrial.ondemand.com`

    The environment variables are not visible but still stored in clear text on the central side. Thus the HCP password must not be handled like this. We describe now how to encrypt them properly.

4. Install Ruby and the Travis command line client on your local PC as described here:

    > https://docs.travis-ci.com/user/encryption-keys/

5. In your local clone of the GitHub repository, execute the following command in the directory where the `.travis.yml` file is located:
  
    ```
    travis encrypt SAP_CLOUD_PASSWORD={your HCP password} --add env.global
    ```

    This command automatically adds an encrypted environment entry into the `.travis.yml` file. The content of `travis.yml` should now look more or less like this:  
     
    ```
    sudo: false
    jdk: oraclejdk7
    script: mvn install -P cloud-integration-tests
    env:
      global:
        secure: SdKbOJMBDLU4W6GJfK0n...
    ```
     
6. Open Travis CI and go to your project. Select **More options > Settings**, switch **Limit concurrent jobs** on and enter `1` as value to prevent concurrent builds to collide with the deployments on HCP.
  
    ![Build configuration in Travis](java-hcp-cloud-3.png)

    > Travis documentation: https://docs.travis-ci.com/user/customizing-the-build
    
7. Commit the changes with Git and put them to GitHub. Monitor the statuses of the build in Travis CI and of the application in the SAP HANA Cloud Platform Cockpit.


#### Further Refinements

1. More than one parallel build.  
    In the example the number of parallel builds is restricted to one to avoid conflicts in deployment. This restriction can be circumvented by providing dynamic names for the application that is deployed. In the property definition block of the `pom.xml` file apply the following change:  

    ```
    <sap.cloud.application>${env.TRAVIS_BRANCH}${env.TRAVIS_BUILD_NUMBER}</sap.cloud.application>
    ```

    The application name is generated from the branch and the build number. Any build produces a unique application name, also well-categorized by branch. Please ensure that the resulting name still adheres to the application naming rules for HCP.  
     
    > Rules for deployment: https://help.hana.ondemand.com/help/frameset.htm?937db4fa204c456f9b7820f83bc87118.html
     
2. Restrict build for branches.  
    If only a few branches should be built automatically, you may add a build restriction into the `.travis.yml` file like this:

    ```
    branches:
      only:
        - master
    ```
     
    > Documentation: https://docs.travis-ci.com/user/customizing-the-build/#Building-Specific-Branches

More sophisticated control mechanisms are provided by Travis' build matrix:

> Documentation: https://docs.travis-ci.com/user/customizing-the-build/#Build-Matrix
  

## Next Steps

  - [Back to the Navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)
  
