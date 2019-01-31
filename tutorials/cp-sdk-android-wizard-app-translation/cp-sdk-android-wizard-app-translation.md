---
title: Add New Languages to Your Android Application
description: See how the SAP Translation Hub can be used to quickly add support for a new language to your app.
primary_tag: products>sap-cloud-platform-sdk-for-android
auto_validation: true
tags: [  tutorial>beginner, operating-system>android, topic>mobile, topic>odata, products>sap-cloud-platform-sdk-for-android, products>sap-cloud-platform ]
time: 20
---

## Details
### You will learn
- An overview of how the generated app supports different languages
- How to sign up for the SAP Translation Hub
- How to use the SAP Translation Hub from within an Android Studio Project
---

[ACCORDION-BEGIN [Step 1: ](Language Support in the Generated App)]

The wizard generates an Android app that contains translated strings for multiple languages as shown below.

![Languages Localized](localized-strings.png)

The language codes (ar, cs, da, de, en, es etc.) can be looked up at <a target="_blank" href="http://www.loc.gov/standards/iso639-2/php/code_list.php">Codes for the Representation of Names of Languages</a>.


When a string is used within the app, it reads the string from the appropriate version of the `strings_localized` or `strings.xml` file.  The `strings.xml` file contains strings that are provided at design time such as the screen names from the `metadata.xml` or the app name.  Android Studio provides a Translations Editor that can be used to view the values from across the localized strings files.  Right-click on the `strings.xml` file and choose **`Open Translations Editor`**.

>Notice in the screenshot below that the resources that are from the `strings.xml` file such as Suppliers are not translated.  These will get translated in Step 3.

![Translations Editor](translations-editor.png)


The below code in the class `LogonActivity`, in the method `startLaunchScreen`, is an example of the code used to read from a strings file.

```Java
launchScreenSettings.setLaunchScreenHeadline(getString(R.string.welcome_screen_headline_label));
```

The `getString` method determines what the current local is and then reads from the matching strings file.

The language can be set on an Android device or emulator via Settings > System > Languages & input > Language preferences.  A second language such as French can be added.

![Additional Languages](additional-languages.png)

 The default language can be selected by long pressing on a language and dragging it to the top of the list.

 ![Default Language](default-language.png)

When the app is restarted, it now shows strings in the preferred language if that language is supported by the app.

![French Welcome Screen](welcome-french-screen.png)

See also

<a target="_blank" href="https://developer.android.com/guide/topics/resources/localization">Localize your app</a>   

<a target="_blank" href="https://developer.android.com/training/basics/supporting-devices/languages">Support different languages and cultures</a>  

<a target="_blank" href="https://developer.android.com/studio/write/translations-editor">Localize the UI with Translations Editor</a>  


[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Sign Up for the SAP Translation Hub)]

>Note that the SAP Translation Hub wizard in the SAP Cloud Platform SDK for Android is currently not compatible with the SAP Translation Hub in the SAP Cloud Platform Trial.

The <a target="_blank" href="https://cloudplatform.sap.com/capabilities/devops/translation-hub.html">SAP Translation Hub</a> enables translation of resources to specified languages.  

In the SAP Cloud Platform management cockpit, enable **SAP Translation Hub**.

![Enable SAP Translation Hub](enable-translation-hub.png)

Click on the tile and choose **Go to UI for Translation Workflow**.

![Go To Translation Hub UI](go-to-translation-hub-ui.png)

Notice below that there are no existing translation projects.  In the next section, a wizard integrated into Android Studio will create a translation project and request new language files for Italian which is not one of the languages the generated app already contains.

![Translation Hub UI](translation-hub-ui.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add a new Language to your Project)]
Right-click on a file in the Project Explorer and choose **`Translate Resources with SAP Translation Hub`**.

![Translation Wizard Page 1](wiz-page1.png)

Choose **Create new SAP Translation Hub project**.

![Translation Wizard Page 2](wiz-page2.png)

Choose the Domain of the application, specify the source language as English and the target language to add.

![Translation Wizard Page 2](wiz-page3.png)

Select the two source files to be used for the translation.

![Translation Wizard Page 2](wiz-page3b.png)

After pressing finish and waiting for a few moments, the Event Log should print Success: Project translated successfully.  

There should now be two additional files, `strings_localized_it.xml` and `strings_it.xml`.

![Translation Result](translation-result.png)

In the emulator or device, set the preferred language to be Italian, then run the app and notice that the app now displays Italian strings.

![Translation Result in App](translation-result-in-app.png)

>Some of the strings in the app are part of the foundation or Fiori library and their strings are not easily accessible to be localized.  A couple of examples are the basic authentication screen and the passcode screen.

For more information on the translation wizard, see [TODO Add link](https://help.sap.com/doc/c2d571df73104f72b9f1b73e06c5609a/Latest/en-US/docs/user-guide/foundation/Usage.html).


[DONE]
[ACCORDION-END]

---
