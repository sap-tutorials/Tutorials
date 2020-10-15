---
title: Test SAP HANA, express edition
description: Test your SAP HANA, express edition installation. Test your XSC, XSA, SAP Web IDE, and Cockpit installations.
author_name: Aaron Patkau
author_profile: https://github.com/aptk001
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 10
---

<!-- loioa00667372f1a44228ae039268e927ba6 -->

## Prerequisites

[ACCORDION-BEGIN [Step 1: ](Test your server installation)]

In a terminal, log in as the <sid>`adm` user.

Enter `HDB info`. The following services must be running:

-   `hdbnameserver`
-   `hdbcompileserver`
-   `hdbwebdispatcher`
-   `hdbdiserver`

If any services are not running, enter `HDB start`. When the prompt returns, the system is started.

Check that the XSEngine is running. Open a browser and enter:

```bash
http://<hostname>:80<instance-number>
```

A success page displays:

![XSEngine_Success_1](XSEngine_Success_1.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test XSA)]

As the <sid>`adm` user, log in to XSA services:

```bash
xs-admin-login
```

At the prompt for the `XSA_ADMIN` password, enter the master password you specified during installation.

View the list of XSA applications:

```bash
xs apps
```

> Note:
> When you run the `xs apps` command for the first time, it may take 1-2 minutes for the system to return the list of XSA applications.
>
>

Check that the application `cockpit-admin-web-app` shows `STARTED` with 1/1 instances in the list of XSA applications.

> Note:
> Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
>
>

Make a note of the URL for `cockpit-admin-web-app`.

![XSA_apps_cockpit-admin-web-app_entry_0](XSA_apps_cockpit-admin-web-app_entry_0.png)

Enter the URL for `cockpit-admin-web-app` in a browser. The address is the one that displays in your `xs apps` command output.

Example: `https://my.hostname:51043`

Log in using the `XSA_ADMIN` user.

If your site uses a proxy for connecting to HTTP and HTTPS servers, select *Cockpit Settings* > *Proxy*, then enable *Http(s) Proxy* and set the host, port, and non-proxy hosts.

> Note:
> To find your proxy server information, in a terminal, enter `env | grep PROXY`.
>
>

> Note:
> If you are using HANA Cockpit to register a resource, both HANA Cockpit and the SAP HANA, express edition server must be from the same release. SAP does not recommend using a newer HANA Cockpit to register an older version of SAP HANA, express edition.
>
>

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ]((Optional) Turn on XSA messaging)]

If you want the XSA messaging service, issue these commands to start the messaging service applications:

```bash
xs start messaging-service-hub
xs start messaging-service-node
xs start messaging-service-broker

```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test Web IDE)]

As the <sid>`adm` user, log in to XSA services:

```bash
xs-admin-login
```

At the prompt for the `XSA_ADMIN` password, enter the master password you specified during installation.

View the status of the `webide` application. Enter:

```bash
xs apps | grep webide
```

Check that the application `webide` shows `STARTED` with 1/1 instances in the list of XSA applications.

> Note:
> Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.
>
>

Make a note of the URL for `webide`.

Test your Web IDE connection. Enter the URL for `webide` in a browser. The address is the one that displays in your `xs apps` command output.

Example: `https://my.hostname:53075`

Log on to Web IDE using the `XSA_DEV` user.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ]((Optional) Test Your Installation Using the HANA Eclipse Plugin)]

Download `Eclipse IDE for Java EE Developers` from [http://www.eclipse.org/neon/](http://www.eclipse.org/neon/) to your local file system.

Follow the Eclipse installer prompts.

Launch when prompted, or go to the Eclipse folder (example: `C:\Users\<path>\eclipse\jee-neon`) and run the `eclipse` executable file.

[DONE]

[ACCORDION-END]
