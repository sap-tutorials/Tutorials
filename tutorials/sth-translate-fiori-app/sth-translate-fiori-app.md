---
title: Translate a sample Fiori app
description: Use SAP Translation Hub to translate your Fiori app.
primary_tag: products>sap-translation-hub
tags: [  tutorial>beginner, products>sap-translation-hub, products>sap-cloud-platform, topic>sapui5 ]
---

## Prerequisites  
 - **Proficiency:** Beginner
  - **Tutorials:** [Prepare a sample Fiori app for translation](https://www.sap.com/developer/tutorials/sth-prepare-fiori-app-translation.html)
 - **Tutorials:** [Deploy a sample Fiori app to SAP Cloud Platform]

## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)


## Details
### You will learn  
You'll learn how to translate a Fiori app using SAP Translation Hub.

### Time to Complete
**10 Min**.

---
[ACCORDION-BEGIN [Step 1: ](Open SAP Translation Hub)]

In the service catalog of the SAP Cloud Platform cockpit, locate the **SAP Translation Hub** service by searching for `Translation`. Click the tile and choose **Go to UI for Translation Workflow**.

![Open SAP Translation Hub](sth-translate-fiori-app-go-to-sth.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new Git translation project)]

First, you'll need to create a translation project. Choose **+** and then **Git Project**.  

![Create git project](sth-translate-fiori-app-creategitproject.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enter translation project details)]

Now you need to enter the following details about your project:

Field Name | Value
:-------------  | :-------------
Application Name | **`sampleapprovepurchaseorders`**
Path to Properties File | **`webapp/i18n/i18n.properties`**
Domain | **Sales**
Target Languages   | Enter the target languages that you specified in your project in SAP Web IDE: **Danish**, **Dutch**, **Finnish**, **French**, and **German**

To see a quality index for translations provided by machine translation (not all languages are supported - see on-screen help for details), select **Show Quality Index for MT**.
Leave all other fields and selection options as they are and choose **Save**.

![Enter git translation project details](sth-translate-fiori-app-project-details.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Translate texts in translation project)]

SAP Translation Hub shows an overview of your translation project. To start the translation process and push the translations to the Git repository, choose **Translate and Push** in the bottom right of the screen.

![Get translations](sth-translate-fiori-app-get-translations.png)

Enter your Git password and choose **Submit**.

![Enter Git password](sth-translate-fiori-app-enter-git-password.png)

If everything works as expected, you see the following status:

![Success message](sth-translate-fiori-app-success-status.png)


[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Check translations)]

The translation is now complete, but you might want to review the translations. To do that, choose **Translations**:

![Check translations](sth-translate-fiori-app-translations.png)

This is where you can switch between the target languages and, in the columns to the right, see the translation provider and a quality status. The higher the number on a scale from 0-100, the better the quality. Note: for translations provided by SAP Machine Translation (SAP MT), a quality index is not available for all languages.

![List of translations](sth-translate-fiori-app-list-of-translations.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Update translations (optional))]

If you want to change any of the translations, simply make your changes directly in the **Translated Text** column. After you make a change, the translation provider changes to `Manual`.
When you're done, choose **Save Project** and then push your changes to the Git repository by choosing **Push**:

![Push translations to repository](sth-translate-fiori-push-changes-repo.png)

Enter your Git password and choose **Submit**.

![Enter Git password](sth-translate-fiori-app-enter-git-password.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Pull translations from Git repository to SAP Web IDE)]

To see the translations in SAP Web IDE, you need to pull the translations from the Git repository. Open SAP Web IDE and select the `sampleapprovepurchaseorders` project:

In the **Git Pane**, choose **Pull**:

![Pull changes to SAP Web IDE](sth-translate-fiori-app-pull-to-ide.png)

A success message appears in the top right of the SAP Web IDE window.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Check i18n properties files in SAP Web IDE)]

The **i18n** folder in your project now contains properties files for the target languages that you selected in your project. To see the translations, open any of the properties files by double-clicking the files.

![i18n property files in target languages](sth-translate-fiori-app-i18n-lang-properties-files.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Run sample app in additional languages)]

As a final step, you can run the app in different languages by choosing the green button shown below:

![run demo app](sth-translate-fiori-app-run-demo.png)

When the preview opens, change the language in the top right of the screen:

![switch languages](sth-translate-fiori-app-switch-languages.png)

Now open the **Approve Purchase Orders** app:

![run demo app](sth-translate-fiori-app-man-prods.png)

> Note: Some texts in the app are pulled directly from the back end or are hard coded, and are therefore not part of the `i18n.properties` file that you translated in this tutorial.


[ACCORDION-END]


## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)
