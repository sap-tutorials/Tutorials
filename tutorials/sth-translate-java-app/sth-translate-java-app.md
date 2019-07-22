---
title: Translate a Java App
description: Use SAP Translation Hub to translate the UI texts for a Java app that you develop in Eclipse.
auto_validation: true
primary_tag: products>sap-translation-hub
tags: [  tutorial>beginner, products>sap-translation-hub, products>sap-cloud-platform, topic>java ]
time: 15
---

## Prerequisites  
  - [Install and configure Eclipse environment](https://github.com/SAP/cloud-espm-v2#1-quick-start-guide) Complete the **1. Quick start guide** section.

## Details
### You will learn  
  - How to translate the properties file of a Java app using the file upload scenario in SAP Translation Hub


---
[ACCORDION-BEGIN [Step 1: ](Locate properties file)]

In your Eclipse window, locate the `i18n.properties` file that contains your app texts.

![Locate i18n.properties file](sth-locate-i18n-prop-file.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](View texts in properties file)]

Before you get started with the translation process, open the `i18n.properties` file to look at the texts that you're going to translate.

![Open i18n.properties file](sth-view-texts-i18n-prop-file.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Choose tile for trial account in Neo)]

In the [SAP Cloud Platform cockpit](https://account.hanatrial.ondemand.com), choose the tile for a trial account in the Neo environment.

![access menu to open service catalog](sth-choose-Neo.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open the service catalog)]

Choose **Services** in the navigation area on the left.

![open service catalog](sth-open-services.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Locate the SAP Translation Hub service)]

Search for the SAP Translation Hub service by entering **transl** and then choose the **SAP Translation Hub** tile.

![Find Translation Hub service](sth-search-tran.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Open SAP Translation Hub UI)]

To access the SAP Translation Hub, UI, choose **Go to UI for Translation Workflow**.

![Go to SAP Translation Hub service](sth-tile-go-to-service.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create a translation project)]

Create a translation project for the .properties file that you're going to upload.

![Create translation project](sth-create-file-proj.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Enter translation project details)]

Enter the following details about your translation project:

Field Name | Value
:-------------  | :-------------
Project Name | `espmcloud`
File Type | **Java Properties File**
Domain | **Sales**
Source Language | **English**
Target Languages | Enter the languages of your choice, such as **French** and **Italian**

Choose **Save**.

![Create project](sth-create-file-proj-details.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Upload properties file)]

You can upload the properties file in two ways: there's the drag & drop option, which automatically starts the translation process, and there's the more traditional file selection option, which requires you to select the file and start the translation process. The end result is the same, it's just a matter of how you prefer to work.

#### Drag & Drop Option
Simply drag the file from your Eclipse project to the translation project screen. SAP Translation Hub uploads your properties file to the translation project and translates the texts in one go.

#### File Selection Option
 <ol> <li> On the translation project screen, choose **Upload File**:
![Upload properties file](sth-choose-upload-file.png)</li>
 <li> Navigate to the folder on your local PC that contains the `i18n.properties` file and open it:
![Select properties file](sth-file-upload-explorer.png) </li>
<li> Choose **Upload and Translate**:
![Select properties file](sth-file-upload-translate.png)</li>
</ol>

Regardless of the option you choose, when SAP Translation Hub has translated the texts, you see the following status:

![Success status](sth-translate-fiori-app-success-status.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Review translations)]

To review the translated texts, choose **Translations**.

![Choose translations tab](sth-translations-tab.png)

On this screen, you can switch between target languages and view information about the translation provider and quality rating:

![Review translations, translation provider, and quality index](sth-review-translations.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Filter by quality index (optional))]

To better plan the language review process, you can filter translations by the quality rating. For example, this enables you to see only those translations that have a quality rating below a certain threshold, such as `50`.
Choose the filter icon above the **Quality Index** column:

![Choose filter button](sth-filter-step1.png)

Choose **Quality Index**:

![Choose quality index button](sth-filter-step2.png)

Enter `50` as the upper limit or move the right slider bubble to the center of the scale:

![Adjust quality index slider](sth-filter-step3.png)

The following success message confirms your filter setting:

![Confirm quality index filter](sth-filter-confirm.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Update translations (optional))]

If you want to update any translations, make the changes in the **Translated Text** column and choose **Save Project**:

![Save translation project](sth-save-trans-proj.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Download translations)]

Now it's time to download your translations so that you see your Java app in other languages. Choose **Download Translations**:

![Download translations](sth-download-translations.png)

SAP Translation Hub prepares a `zip` file that contains properties files for each of the target languages in your translation project; save the `zip` file on your local PC and make a note of its location. You'll need to navigate to this file later in the tutorial.

![Confirm zip download](sth-zip-downloaded.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Import translations in Eclipse)]

Now it's time to head back to your Eclipse window and import the translations into the project for your Java app.

Ensure that you're looking at the **Java** perspective:

![Choose Java perspective](sth-Java-perspective-import.png)

In the **Package Explorer**, navigate to your `i18n` folder, by choosing `espm-cloud-web > src > main > webapp > webshop`.
Right-click the `i18n` folder and choose **Import...**.  

![Choose import in Eclipse](sth-eclipse-i18n-import.png)

Choose **Archive File**:

![Choose archive file for import](sth-eclipse-import-archive-file.png)

At the top of the **Import** window, browse to the `zip` file that you downloaded from the SAP Translation Hub UI and choose **Finish**:

![Select zip file to import](sth-eclipse-import-archive-file-zip.png)

The properties files per target language appear in the `i18n` folder:

![Show target language properties files](sth-target-prop-files-imported.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Restart Java server (optional))]

To see the translations in your Java app, you need to restart the Java server. In Eclipse, switch to the **Git** perspective:  

![Choose Git perspective](sth-eclipse-choose-git-persp.png)

Choose the **Servers** tab:  

![Choose Servers tab](sth-eclipse-servers-tab.png)

Right-click **Java Web Server** and choose **Clean...**:  

![Choose Servers tab](sth-eclipse-choose-clean.png)

Confirm the clean process by choosing **OK**:

![Confirm clean process](sth-eclipse-clean-confirm.png)

Depending on system performance, it can take several minutes for the Java server to be cleaned and restarted. When the process is complete, the status of the Java Web server changes to **Started, Synchronized**:

![Server started](sth-eclipse-server-started-synchro.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Display app in browser)]

To see your app in the translated languages, enter the following address in a browser; the last two letters denote the language of your app: `http://localhost:8080/espm-cloud-web/webshop/?sap-language=fr`

![Enter local host address in browser](sth-espm-app-browser-lang-select.png)

The French version of the app appears:

![Display French version of app](sth-espm-app-browser-fr-version.png)

[VALIDATE_2]

[ACCORDION-END]
