---
title: Translate a sample Fiori app
description: Use SAP Translation Hub to translate your Fiori app.
primary_tag: topic>sap-translation-hub
tags: [  tutorial>beginner, topic>sap-translation-hub, products>sap-cloud-platform, topic>sapui5 ]
---

## Prerequisites  
 - **Proficiency:** Beginner
  - **Tutorials:** [Prepare a sample Fiori app for translation](http://www.sap.com/developer/tutorials/sth-prepare-fiori-app-translation.html)
 - **Tutorials:** [Commit your project to Git and deploy to the cloud](http://www.sap.com/developer/tutorials/teched-2016-5.html) - This tutorial contains a Git repository called `te2016`. To make it easier to follow the steps in the translation tutorial, use the name of the project you created for the sample Fiori app in SAP Web IDE instead (`nwepmrefappsextshop`). Do not open the active version of the app, as described in the last step.

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)


## Details
### You will learn  
You will translate a sample Fiori app using SAP Translation Hub.

### Time to Complete
**10 Min**.

---
[ACCORDION-BEGIN [Step 1: ](Open SAP Translation Hub in the service catalog of the SAP Cloud Platform Cockpit)]

In the service catalog of the SAP Cloud Platform cockpit, locate the **SAP Translation Hub** service by searching for `Translation`. Click the tile and choose **Go to Service**.

![Open SAP Translation Hub](sth-translate-fiori-app-go-to-sth.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new Git translation project)]

First, you'll need to create a translation project; choose **+** and then **Git Project**.  

![Create git project](sth-translate-fiori-app-creategitproject.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enter translation project details)]

Now you need to enter the following details about your project:

Field Name | Value
:-------------  | :-------------
Application Name | `nwepmrefappsextshop`
Path to Properties File | `webapp/i18n/i18n.properties`
Domain | **Basis**
Target Languages   | Enter the files you defined in your project in SAP Web IDE: **Danish**, **Dutch**, **English**, **Finnish**, **French**, and **German**

Leave all other fields as they are and choose **Save**.

![Enter git translation project details](sth-translate-fiori-app-project-details.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Translate project texts in translation project)]

SAP Translation Hub shows an overview of your translation project. To start the translation process, choose **Get Translations** in the bottom right of the screen.

![Get translations](sth-translate-fiori-app-get-translations.png)

Enter your Git password and choose **Submit**.

![Enter Git password](sth-translate-fiori-app-enter-git-password.png)

If everything works as expected, you see a success message:

![Success message](sth-translate-fiori-app-success-message.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Check translations)]

The translation is now complete, but you might want to review the translations. To do that, choose **Translations**:

![Check translations](sth-translate-fiori-app-translations.png)

This is where you can switch between the target languages and, in the right columns, see the translation provider and a quality status. The higher the number on a scale from 0-100, the better the quality. Note that there is currently no quality index for translations provided by SAP Machine Translation (SAP MT).

![List of translations](sth-translate-fiori-app-list-of-translations.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Update translations (optional))]

If you want to change any of the translations, simply make your changes directly in the **Translated Text** column. When you're done, choose **Save** and then push your changes to the repository using the context menu at the right of the **Get Translations** button:

![Push translations to repository](sth-translate-fiori-push-changes-repo.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Pull translations from Git repository to SAP Web IDE)]

To see the translations in SAP Web IDE, you need to pull the translations from the Git repository. Open SAP Web IDE and select the `nw.epm.refapps.ext.shop` project:

![Select project in SAP Web IDE](sth-translate-fiori-app-select-project-ide.png)

In the **Git Pane**, choose **Pull**:

![Pull changes to SAP Web IDE](sth-translate-fiori-app-pull-to-ide.png)

A success message appears in the top right of the SAP Web IDE window.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Check i8n properties files in SAP Web IDE)]

The **i18n** folder in your project now contains properties files for the target languages that you selected in your project. To see the translations, open any of the properties files by double-clicking the files.

![i18n property files in target languages](sth-translate-fiori-app-i18n-lang-property-files.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Run sample app in additional languages)]

As a final step, you can run the app in different languages by choosing **Run**.

![run demo app](sth-translate-fiori-app-run-demo.png)

When the preview opens, change the language in the top right of the screen:

![switch languages](sth-translate-fiori-app-switch-languages.png)

Now open the **Shop** app:

![run demo app](sth-translate-fiori-app-Shop.png)

**Note: Some texts in the app come directly from the back end, and are therefore not part of the properties file that you translated.**

[DONE]
[ACCORDION-END]


## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
