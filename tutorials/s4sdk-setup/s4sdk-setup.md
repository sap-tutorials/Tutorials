---
title: Set Up Your Local Infrastructure to Develop with SAP Cloud SDK
description: Set up your system to create an SAP Cloud Platform application with the SAP Cloud SDK.
auto_validation: true
tags: [ tutorial>intermediate, products>sap-cloud-sdk, products>sap-s-4hana, products>sap-business-technology-platform, topic>cloud, topic>java ]
primary_tag: products>sap-cloud-sdk
time: 10
---

## Details
### You will learn  
In this tutorial, you will go through the steps required to install the [SDK](https://developers.sap.com/topics/cloud-sdk.html) and corresponding tools to use it for the development of your SAP Cloud Platform application.

For a complete overview, visit the [SAP Cloud SDK documentation](https://sap.github.io/cloud-sdk/).


---

[ACCORDION-BEGIN [Step 1: ](Prepare infrastructure)]

To develop with the [SAP Cloud SDK for Java](https://sap.github.io/cloud-sdk/docs/java/overview-cloud-sdk-for-java) you will need to have two things installed:

- Java 8 or 11
- Maven 3

If you have the required software installed already you can skip this step. You can check your the versions of your installations via the commands listed at the end of this step.

> Note: Java 11 is only available on SAP Cloud Platform: Cloud Foundry

[OPTION BEGIN [On Windows]]

For Windows 7+ or Windows Server 2003+ adhere to the following steps:

1. Install `Chocolatey`. `Chocolatey` is a package manager for Windows which will be useful for installing necessary components. Install it by opening a console and issuing the following command:

    ```shell
    @powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"
    ```

2. Install the Java Development Kit:

    ```shell
    choco install adoptopenjdk8
    ```

    > In case you want to develop with Java 11 please install [SapMachine](https://sap.github.io/SapMachine/).

3. Install Maven:

    ```shell
    choco install maven
    ```

[OPTION END]


[OPTION BEGIN [On Mac]]

For Mac OS adhere to the following steps:

1. Install `Homebrew`. `Homebrew` is a package manager for Mac which will be useful for installing necessary components. Install it by opening a terminal and issuing the following command:

    ```bash
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ```

2. Install Java Development Kit 8:

    ```bash
    brew update
    brew tap AdoptOpenJDK/openjdk
    brew cask install adoptopenjdk8
    ```

    > In case you want to develop with Java 11 please install [SapMachine](https://sap.github.io/SapMachine/).

3. Install Maven:

    ```bash
    brew update
    brew install maven
    ```

[OPTION END]


To validate that everything is installed correctly you can use the following commands:

```bash
javac -version
mvn -version
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install IDE)]

To develop your first 'Hello World' application with SAP Cloud SDK, you can just use your command line and a simple text editor. However, for larger development projects you can work with the IDE of your choice.

We recommend using [`Intellij IDEA`](https://www.jetbrains.com/idea/#chooseYourEdition) or [`Eclipse`](https://www.eclipse.org/users/). Follow the installation instructions of corresponding tools to prepare your IDE. In case you use Eclipse, make sure to install the [`Maven plugin for Eclipse`](http://www.eclipse.org/m2e/).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Troubleshoot with corporate proxies)]

Some of the steps in the upcoming tutorial blogs will fail if you sit behind a corporate proxy. If you cannot escape the proxy, you need to tell Maven where your proxy is located.
To do this, you need to cd to your `~/.m2 directory` (e.g. on Windows: `C:/Users/<username>/.m2"`") and create a file called `settings.xml`. Then you paste the following content:

```xml
<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0
                      http://maven.apache.org/xsd/settings-1.0.0.xsd">
  <proxies>
    <proxy>
      <id>my_corp_proxy</id>
      <active>true</active>
      <protocol>http</protocol>
      <host>proxy</host>
      <port>8080</port>
      <username></username>
      <password></password>
      <nonProxyHosts>localhost,127.0.0.1</nonProxyHosts>
    </proxy>
  </proxies>
</settings>
```

After finishing these steps, you are ready to start the development of your SAP Cloud Platform applications with SAP Cloud SDK. If you are interested to learn more, stay tuned for the upcoming development topics that we will cover in the following tutorials: available project templates in the SDK, setting up the communication with SAP S/4HANA, deployment on Cloud Foundry, etc.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]
---
