---
title: Install SAP HANA 2.0, express edition
description: Extract the installation files and set up your SAP HANA 2.0, express edition installation– with XS Advanced, Web IDE, and SAP HANA Cockpit.
author_name: Aaron Patkau
author_profile: https://github.com/aptk001
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 60
---

<!-- loioe0727cd528264b0eade79b20cc9321d1 -->

## Prerequisites

## Details
### You will learn
You will learn how to extract and configure the binary image of SAP HANA 2.0, express edition on your Linux server. You downloaded the Server-only installer file and Applications file in the previous tutorial.

---

[ACCORDION-BEGIN [Step 1: ](Extract the installation files.)]

You downloaded the installation files in the previous tutorial.

Navigate to the directory where you wish to extract the installation files.

Extract the contents of the following files:

-   `hxe.tgz`
-   `hxexsa.tgz` (if you are installing the Applications package)
-   `eadesigner.tgz` (if you are installing SAP EA Designer)
-   `eml.tgz` (if you are installing the External Machine Learning Library)
-   `hsa.tgz` (if you are installing streaming analytics)
-   `sdi.tgz` (if you are installing smart data integration)
-   `apl.tgz` (if you are installing the Automated Predictive Library)
-   `shine.tgz` (if you are installing SHINE)

```bash
tar -xvzf <download_path>/hxe.tgz
```

```bash
tar -xvzf <download_path>/hxexsa.tgz
```

```bash
tar -xvzf <download_path>/eadesigner.tgz
```

```bash
tar -xvzf <download_path>/eml.tgz
```

```bash
tar -xvzf <download_path>/hsa.tgz
```

```bash
tar -xvzf <download_path>/sdi.tgz
```

```bash
tar -xvzf <download_path>/apl.tgz
```

```bash
tar -xvzf <download_path>/shine.tgz
```

> Note:
> Run the `tar` command from the command shell as shown, rather than using a GUI-based extraction tool.
> 
> 

> Note:
> You may have to give these files run permissions. Example:
> 
> ```bash
> chmod 444 <download_path>/hxe.tgz
> ```
> 
> 

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Start the installation.)]

Navigate to the directory where you extracted the files and run `./setup_hxe.sh` as the root user:

```bash
cd <extracted_path>
sudo ./setup_hxe.sh
```

Follow the prompts to configure your installation.

> Note:
> The master password you specify during installation is used for the <sid>`adm` and `sapadm` OS users, the telemetry technical user, and the SYSTEM user. The password is also used for the following users in additional components:
> 
> -   `XSA_ADMIN` and `XSA_DEV` (Applications package)
> -   `XSA_SHINE` (SHINE)
> -   `SYS_STREAMING` and `SYS_STREAMING_ADMIN` (streaming analytics)
> 
> SAP HANA, express edition requires a `very strong password` that complies with these rules:
> 
> -   At least 8 characters
> -   At least 1 uppercase letter
> -   At least 1 lowercase letter
> -   At least 1 number
> -   Can contain special characters, but not ``` ` ``` (backtick), `$` (dollar sign), `\` (backslash), `'` (single quote), or `"` (double quotes)
> -   Cannot contain simplistic or systematic values, like strings in ascending or descending numerical or alphabetical order
> 
> 

[DONE]

[ACCORDION-END]


