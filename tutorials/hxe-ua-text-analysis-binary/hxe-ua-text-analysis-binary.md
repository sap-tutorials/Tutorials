---
title: Install the Optional Text Analysis Files Package for SAP HANA, express edition
description: If you are using SAP HANA 2.0, express edition in a language other than English or German, you can download the `Text analysis files for additional languages` package in the Download Manager.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio604364b544704ac382b4782793852288 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  You have completed [Test the Installation](http://developers.sap.com/tutorials/hxe-ua-test-binary.html)  

## Details
### You will learn
You will learn how to download, install, and configure the `additional_lang.tgz` text analysis files package package.

### Time to Complete
15 min

---

The `Text analysis files for additional languages` package contains the text analysis files for the HANA Text Analysis feature (for languages other than English or German).

[ACCORDION-BEGIN [Step 1: ](Download additional_lang.tgz)]

Download `additional_lang.tgz` using the built-in Download Manager. From directory `/usr/sap/`SID`/home/bin` enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 installer additional_lang.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Update folder permissions)]

Update the folder permissions on the `lang` folder.

Navigate to `/hana/shared/`SID`/global/hdb/custom/config/lexicon/`

Enter this command:

```bash
chmod 755 lang
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract additional_lang.tgz)]

This step extracts `<download_path>/additional_lang.tgz` to `/hana/shared/`SID`/global/hdb/custom/config/lexicon`. Enter this command:

```bash
tar -xvzf /usr/sap/`SID`/home/Downloads/additional_lang.tgz -C /hana/shared/`SID`/global/hdb/custom/config/lexicon
```

> Note:
> If your tables do not use a full text index, or if your tables use a full text index but contain very little data, you can save about 120 MB of memory if you turn off the standalone text analysis preprocessor, and activate the embedded text analysis preprocessor.
> 
> Stop the standalone preprocessor:
> 
> ```bash
> alter system alter configuration ('daemon.ini','SYSTEM') set ('preprocessor','instances') = '0' with reconfigure;
> 
> ```
> 
> Start the embedded preprocessor:
> 
> ```bash
> alter system alter configuration ('preprocessor.ini','SYSTEM') set ('general','embedded') = 'true' with reconfigure;
> ```
> 
> 

[ACCORDION-END]


