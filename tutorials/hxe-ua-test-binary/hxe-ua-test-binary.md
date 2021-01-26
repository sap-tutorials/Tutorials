---
title: Test SAP HANA, express edition
description: Test your SAP HANA, express edition installation.
author_name: Aaron Patkau
author_profile: https://github.com/aptk001
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 10
---

<!-- loioa00667372f1a44228ae039268e927ba6 -->

## Prerequisites

## Details
### You will learn
You'll learn how to confirm that your SAP HANA, express edition installation is running.

---

[ACCORDION-BEGIN [Step 1: ](Test your server installation)]

In a terminal, log in as the <sid>`adm` user.

Enter `HDB info`. The following services must be running:

-   `hdbnameserver`
-   `hdbcompileserver`
-   `hdbwebdispatcher`

If any services are not running, enter `HDB start`. When the prompt returns, the system is started.

Check that the XSEngine is running. Open a browser and enter:

```bash
http://<hostname>:80<instance-number>
```

A success page displays:

![XSEngine_Success_0](XSEngine_Success_0.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ]((Optional) Test Your Installation Using the HANA Eclipse Plugin)]

Download `Eclipse IDE for Java EE Developers` from [http://www.eclipse.org/neon/](http://www.eclipse.org/neon/) to your local file system.

Follow the Eclipse installer prompts.

Launch when prompted, or go to the Eclipse folder (example: `C:\Users\<path>\eclipse\jee-neon`) and run the `eclipse` executable file.

[DONE]

[ACCORDION-END]
