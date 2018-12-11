---
title: Localizing your SAPUI5 app
description: Learn how to support multiple languages in your app using the SAP Translation Hub, and how to manually add a language-locale combination.
primary_tag: products>sap-cloud-platform
tags: [ products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, tutorial>intermediate]
---

## Prerequisites
- **Proficiency:** Intermediate
- **Tutorials:** [Commit your project files to your SAP Cloud Platform Git repository](https://www.sap.com/developer/tutorials/hcp-webide-commit-git.html)

## Next Steps
- [An Open Data Protocol (OData) primer for developers](https://www.sap.com/developer/tutorials/hcp-webide-odata-primer.html)

## Details

### You will learn

In the fourth tutorial of Mobile group 2, you extracted strings from the application into the internationalization (i18n) bundle of UI5. In subsequent tutorials, you continued this practice by placing all strings in the `messageBundle.properties` file.

Using a single file not only makes it easy to update strings in the future, but it makes it trivial to localize the UI to different languages and locales. If you want to support multiple languages (English, Spanish, German, French, etc.) or multiple locale settings for a given language (e.g. UK vs US English, or German vs Swiss German) you can create separate files to store the localized strings.

In this tutorial, you will add implement language support for Dutch (`nl`) Spanish (`es`), German (`de`), with the base file covering English (`en`) via the SAP Cloud Platform Translation Hub. Additionally, strings are provided for Swiss German (`de_CH`) to demonstrate language and locale support. After running the app, you will test that the runtime loads the correct files by setting the language/locale in your browser.

**Background on i18n:**

When loading an app, SAPUI5 will check the container's (in this case your browser's) language/local settings, then attempt to load the most specific locale resource bundle file, with graceful degradation down to a "catch-all" case. For example, with a Swiss German-based browser, and no explicit language-related URL parameters, the SAPUI5 runtime will attempt to load the following files in this order:

1.	`messageBundle_de_CH.properties`
2.	`messageBundle_de.properties`
3.	`messageBundle.properties (catchall)`

Strings in more specific resource bundles (loaded earlier) will take precedence over strings for the same translation keys in less specific resource bundles (loaded later).

>An important point: Before you begin, make sure you have deployed the current version of your app to SAP Cloud Platform. This ensures that your latest code is checked into your Git Repository and is available to the Translation Hub service.

### Time to Complete
**20 min**

---


[ACCORDION-BEGIN [Step 1: ](Open Translation Hub page)]

Log into your [SAP Cloud Platform account](https://account.hanatrial.ondemand.com), click on the **Services** tab, scroll down until you see **SAP Translation Hub**, then click on the **SAP Translation Hub** tile.

![Open Translation Hub page](mob3-3_1.png)

Click the **Go To Service** link to open the **Translation Hub** service page.

![Open Translation Hub page](mob3-3_2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new translation project)]

Click the **+** icon to create a new translation project and enter the following information in the **Create Translation Project page**. You will see a toast notification at the bottom of the screen with the selected languages. Click **Save** after entering all information. A toast notification is a non-modal UI element used to display brief alerts to a user.


- **Field:** Value
- **Application Name:** `northwind` (See note below)
- **Branch:** `Master`
- **Path to Properties File:** `i18n/messageBundle.properties`
- **Target Languages:** Click on the menu and check: `Dutch, French, German, Spanish`
- **Domain:** `Basis`

> Note: The application name must be lower case, and match the HTML5 Application name in your git repository. To check your app name, go to your SAP Cloud Platform Cockpit and click on Git Repositories in the left navigation bar.)

![Translation project fields](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Get translations)]

The Translation Hub will display the Project Details. Click **Get Translations** in the lower right corner of the window, enter your password and click **Submit** when prompted.

The Translation Hub service will create the appropriate `messageBundle_xx.properties` files and add them to your Git repository (but they will not be visible in your project folder until you complete the next two steps).

![Get Translations](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_5a.png)

![git password](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_5b.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Review translation)]

To review and optionally edit the translated text, click the **Edit** icon, and then select the desired language to review from the pull down menu.

![Edit translations](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_6.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Change suggested text)]

To change the suggested text for any of the fields (for instance use the German word `Ort` instead of `Stadt` for the `label_City` string) simply edit the field under **Translated Text**. Once your review and your edits are complete, click **Save**. If you don't make any edits, you can simply click **Cancel**.

![changing strings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_7.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Get translations again)]

If you have made changes you will need to click the **Get Translations** button again, and enter your git password for them to be saved to your repository.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Review Git Pane)]

Switch to your Web IDE tab, select the **Git Pane** on the right hand side of the window. Notice that:
- The **northwind** repository and the **master** branch are selected
- The new `messageBundle_xx.properties` files are not yet in your project.

![Git pane showing only one messageBundle.properties file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_9.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Download new language files)]

To download the new language files, click on the **Pull** button.

![Git pane pull button](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_10a.png)

Log in with your SAP Cloud Platform credentials when prompted. You will see a progress indicator on the Pull button as the new `messageBundle_xx.properties` files are added to your project folder.

![Git pane](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_10b.png)

The green dot next to the files indicates that the version in your project is identical to the version in your Git repository.

The files added to your Web IDE **northwind > i18n** folder are:
* `messageBundle_de.properties`
* `messageBundle_es.properties`
* `messageBundle_fr.properties`
* `messageBundle_nl.properties`

![Files added to project](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_10c.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Review new files)]

Double-click on the new files and you will see each has the same 31 entries as `messageBundle.properties`.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Add a language/locale)]

Mobile web, hybrid and native applications have the ability to support both language and locale (for example `de_CH` for Swiss German).

The SAP Translation Hub does not yet support Swiss German, so to add that language to your app, you will create a file with the appropriate name and copy in the strings provided below.

Right-click on the **i18n** folder and select **New > File**. Name the file `messageBundle_de_CH.properties`.

![Create de_CH file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_12.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Add language-specific strings)]

Copy and paste the text below (these are Swiss German strings) into the new file and save your changes.

```xml
masterTitle=Produkt
detailTitle=Produktdetails
notFoundTitle=Nöd gfunde
notFoundText=Die agfordereti Ressource isch nöd gfunde worde
masterListNoDataText=Kei Iträg
masterSearchPlaceholder=Sueche nach Produktname
masterSearchTooltip=Produktname i geh
currencySymbol=€
label_ProductID=Produktschlüssel
label_UnitsInStock=Lagerbestand
label_UnitsOnOrder=Bstellti Stückazahl
label_CategoryID=Kategorieschlüssel
label_ReorderLevel=Mindestbestand
label_Discontinued=Uslaufstatus
label_SupplierID=Lieferanteschlüssel
label_CompanyName=Unternehmensname
label_ContactName=Kontaktname
label_ContactTitle=Kontakttitel
label_Address=Addresse
label_City=Stadt
label_PostalCode=Postleitzahl
label_Country=Land
label_Region=Region
label_Phone=Telefon
label_Fax=Fax
label_HomePage=Startsiite
tab_supplier_short_title=Lieferant
tab_supplier_sub_title=Lieferantedetails
tab_product_short_title=Produkt
tab_product_sub_title=Produktdetail
label_CurrentInventoryValue=Aktuelle Lagerbestand
```

Your new file should look like this. Note that the `messageBundle_de_CH.properties` file does not have a green dot because it hasn't been committed and pushed to your Git repository yet.

![de_CH strings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-localizing-app/mob3-3_13.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Commit your files)]

Commit your new file to Git (following the same procedure as in the previous [tutorial](https://www.sap.com/developer/tutorials/hcp-webide-commit-git.html).


[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Deploy your app)]

Deploy your app to SAP Cloud Platform (following the same procedure as in an earlier [tutorial](https://www.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html) and open the new, active version of the app.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Test your app)]

To test the language support in the deployed app, we can utilize the SAPUI5 `sap-ui-language` URL parameter. To do so, just append `?sap-ui-language=xx_YY` to your application URL

- If your standard application URL looks like this:
- `https://northwind-p12345678trial.dispatcher.hanatrial.ondemand.com/`
- You can view the German strings by specifying:
- `https://northwind-p12345678trial.dispatcher.hanatrial.ondemand.com/?sap-ui-language=de`
- Swiss German strings by specifying:
- `https://northwind-p12345678trial.dispatcher.hanatrial.ondemand.com/?sap-ui-language=de_CH`
- Spanish strings by specifying:
- `https://northwind-p12345678trial.dispatcher.hanatrial.ondemand.com/?sap-ui-language=es`

As described in the introduction for this tutorial, when a user opens your app URL, the app will check the language and locale settings on the device, then load the appropriate strings file.


[ACCORDION-END]




## Next Steps
- [An Open Data Protocol (OData) primer for developers](https://www.sap.com/developer/tutorials/hcp-webide-odata-primer.html)
