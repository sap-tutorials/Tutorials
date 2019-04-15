---
title: Internationalize UI Strings
description: Add internationalization (i18n) strings.
time: 5
author_name: Marius Obert
author_profile: https://github.com/iobert
primary_tag: topic>sapui5
tags: [  tutorial>beginner, topic>html5, topic>sapui5, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment, products>sap-web-ide ]

---
## Details
### You will learn  
Set proper titles to `View1` and `Detail` page by updating the resource model (aka i18n model).  

---

[ACCORDION-BEGIN [Step : ](Add new i18n strings)]

Open the `mta_app/app/webapp/i18n/i18n.properties` file, and update the entire with the following lines.

```I18N
title=Product Overview
appTitle=App Title
appDescription=App Description

# Detail View
DetailTitle=Product Details
```

![Update the i18n properties file](1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Test the i18n)]

Re-run your application.  The title on both the list screen and the detail screen should match the i18n file.

![Update the i18n properties file](2a.png)

![Update the i18n properties file](2b.png)

[DONE]
[ACCORDION-END]

----

### Additional Information
- [Data-binding](http://help.sap.de/saphelp_uiaddon10/helpdata/en/91/f0f3cd6f4d1014b6dd926db0e91070/content.htm)
- [Internationalization (i18n)](https://sapui5.netweaver.ondemand.com/sdk/#docs/guide/91f217c46f4d1014b6dd926db0e91070.html)
