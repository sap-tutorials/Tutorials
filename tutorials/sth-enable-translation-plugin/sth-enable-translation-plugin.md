---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-web-ide]
primary_tag: products>sap-translation-hub
---

# Enable Translation Plugin in SAP WEB IDE (Neo environment)
<!-- description --> You will learn in this tutorial how to enable and use the translation plugin in SAP WEB IDE

## Prerequisites
- You have created and deployed a project in SAP WEB IDE as described in this tutorial group: [Create and Translate an SAP Fiori App with SAP Translation Hub](group.sth-fiori-app)
- Or you have created a project in SAP WEB IDE as described in this tutorial for GitHub: [Collaborate with GitHub and SAP Web IDE](group.webide-github)

## You will learn
  - How to enable and use the translation plugin in SAP WEB IDE
  - How to translate a project in SAP WEB IDE
---
### Open SAP Cloud Platform neo account

Open your SAP Cloud Platform Neo account e.g. [https://account.eu1.hana.ondemand.com/](https://account.eu1.hana.ondemand.com/)




### Maintain destination

  ![destination](10_webide_destination.png)


### Open SAP WEB IDE


Choose Services and locate the SAP Web IDE tile by searching for `web`. Then choose the tile.
    ![Global account Servicecatalog](web_ide_service_catalog.png)


### Select SAP Web IDE service

In the service description for SAP Web IDE, choose `Go to Service`
    ![SAP Web IDE](web_ide_service_tile.png)



### Select Extensions

Select on the left side in SAP Web IDE the `Preference` icon and select `Extensions`
    ![Extensions](web_ide_service_extension.png)


### Enable SAP Translation Hub Plugin

To enable the `SAP Translation Hub Plugin`, search for `transla` and enable it by clicking on the off/on button.
    ![Plugin](web_ide_service_translation_plugin.png)


### Open your project

Select the Development area via the menu on the left and open your project in SAP WEB IDE.
    ![project](web_ide_service_project.png)


### Start Translation

Now you can start the translation with a right click on the source properties file in your project with `Generate Translation Files (plugin)`
    ![project](web_ide_service_translation_step.png)


### Select domain and target languages

If not yet done in your project settings – select the domain and target languages and start the translation with a click on the `Generate` button.
    ![project](web_ide_service_translation_step_domain.png)


### Git Password

Enter your Git password and click on the `Submit` button.
    ![project](web_ide_service_translation_step_password.png)


### Pull translations from git

The translation starts in the background and a new translation project is created automatically in SAP Translation Hub. After the translation is finished you will get a notification in right side `Pull files from git`.
    ![project](web_ide_service_translation_step_finish.png)


### Pull translations from git

Open the `Git Pane and` select `Pull`.
    ![project](web_ide_service_translation_step_pull.png)


### Check the translation

You can find the new i18n.properties files for the selected target languages in your project
    ![project](web_ide_service_translation_step_target_file.png)



