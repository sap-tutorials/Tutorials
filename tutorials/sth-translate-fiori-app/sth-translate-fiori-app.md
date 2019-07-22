---
title: Translate an SAP Fiori App
description: Use SAP Translation Hub to translate your SAP Fiori app.
auto_validation: true
primary_tag: products>sap-translation-hub
tags: [  tutorial>beginner, products>sap-translation-hub, products>sap-cloud-platform, topic>sapui5 ]
time: 10
---

## Details
### You will learn  
  - How to translate an SAP Fiori app using SAP Translation Hub


---
[ACCORDION-BEGIN [Step 1: ](Open overview screen of cockpit)]
To go to the overview screen of the SAP Cloud Platform cockpit, choose your user ID at the top of the screen.

![Choose user ID](sth-translate-fiori-app-choose-user-ID.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Locate SAP Translation Hub in the cockpit)]

Choose **Services** and locate the **SAP Translation Hub** tile by searching for **`transl`**. Then choose the tile.
![Locate SAP Translation Hub](sth-prep-locate-STH.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open SAP Translation Hub)]

In the service description for SAP Translation Hub, choose **Go to UI for Translation Workflow**.

![Open SAP Translation Hub](sth-translate-fiori-app-go-to-sth.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a new Git translation project)]

First, you'll need to create a translation project. Choose **+** and then **Git Project**.  

![Create Git project](sth-translate-fiori-app-creategitproject.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Enter translation project details)]

Now you need to enter the following details about your project in the **Project Metadata**:

Field Name | Value
:-------------  | :-------------
Project Name | **`sampleshop`**
Domain | **Sales**
Target Languages   | Enter the target languages that you specified in your project in SAP Web IDE: **Chinese**, **French**, and **German**

![Enter translation project details](sth-translate-fiori-app-project-details.png)

Now you need to specify the Git details in the **Git Repository Details**:

Field Name | Value
:-------------  | :-------------
Git Server | **SAP Cloud Platform**
Application Name | **`sampleshop`**
Branch | **master**
Path to Properties File | **`webapp/i18n/i18n.properties`**

![Enter Git translation project details](sth-translate-fiori-app-git-details.png)

Finally you can specify in the **Project Configuration** section if you want to reuse existing translations, a company MLTR or a review process.
To simplify the tutorial only select **YES** for **Review Required**.
Leave all other fields and selection options as they are and choose **Save**.

![Enter project configuration details](sth-translate-fiori-app-project-configuration-details.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Translate texts in translation project)]

SAP Translation Hub shows an overview of your translation project. To start the translation process and push the translations to the Git repository, choose **Translate and Push** in the bottom right of the screen.

![Get translations](sth-translate-fiori-app-get-translations.png)

Enter your Git password and choose **Submit**.

![Enter Git password](sth-translate-fiori-app-enter-git-password.png)

If everything works as expected, you see the following status:

![Success message](sth-translate-fiori-app-success-status.png)

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Check translations)]

The translation is now complete, but you might want to review the translations. To do that, choose **Translations**:

![Check translations](sth-translate-fiori-app-translations.png)

This is where you can switch between the target languages and, in the columns to the right, see the translation provider and a quality status. The higher the number on a scale from 0-100, the better the quality.
> Note: For translations provided by SAP machine translation (SAP MT), the quality index is always 25.

![List of translations](sth-translate-fiori-app-list-of-translations.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Update translations (optional))]

If you want to change any of the translations, simply make your changes directly in the **Translated Text** column. After you make a change, the translation provider changes to `Manual`.
When you're done, choose **Save Project**.
As a final step, push your changes to the Git repository by choosing **Push**.

![Push translations to repository](sth-translate-fiori-push-changes-repo.png)

Enter your Git password and choose **Submit**.

![Enter Git password](sth-translate-fiori-app-enter-git-password.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Pull translations from the Git repository to SAP Web IDE)]

To see the translations in SAP Web IDE, you need to pull the translations from the Git repository.

In SAP Web IDE (see where you left off in [Deploy an SAP Fiori app to SAP Cloud Platform](https://developers.sap.com/tutorials/sth-deploy-fiori-app.html)), select the root folder (`sample.shop`) of your project.

Open the Git pane by choosing the icon under the magnifying glass at the right of the screen. Confirm that `sample.shop` appears under **Repository** and then choose **Pull**.

![Pull changes to SAP Web IDE](sth-translate-fiori-app-pull-to-ide.png)

A success message appears in the top right of the SAP Web IDE window.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Check the i18n properties files in SAP Web IDE)]

The `i18n` folder in your project now contains properties files for the target languages that you selected in your project. To see the translations, open any of the properties files by double-clicking them.

![i18n property files in target languages](sth-translate-fiori-app-i18n-lang-properties-files.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Run the app in additional languages)]

As a final step, you can view the app in different languages. To open the SAP Fiori launchpad, choose the green button shown below:
> Depending on your browser settings, you might have to disable the pop-up blocker to proceed.

![Run demo app](sth-translate-fiori-app-run-demo.png)

When the preview opens, change the language in the top right of the screen to German.

![Switch languages](sth-translate-fiori-app-switch-languages.png)

Now open the **Shop** app. The example below shows the app in German.

![Run demo app in German](sth-translate-fiori-app-man-prods.png)

> Note: Some texts in the app come from the back end or are hard coded, and are therefore not part of the `i18n.properties` file that you translated in this tutorial.

[VALIDATE_2]

[ACCORDION-END]
