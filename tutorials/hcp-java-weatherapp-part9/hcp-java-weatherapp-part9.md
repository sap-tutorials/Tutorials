---
title: End-to-End Weather App Scenario Part 9
description: Implement a responsive UI using OpenUI5
tags: [ products>sap-hana-cloud-platform, topic>cloud, topic>java, tutorial>intermediate]
---

## Prerequisites  
 - [End-to-End Weather App Scenario Part 8](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part8.html)

## Next Steps
 - [End-to-End Weather App Scenario Part 10](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part10.html)

## Details
### You will learn  
In this tutorial you will implement a simple responsive, mobile-compatible user interface using OpenUI5.

### Time to Complete
**15 min**

---

1. Create a `weather_app` folder underneath the `webapp` folder.

2. In that `weather_app` folder create another sub-folder called `view`.

3. Next create the following JavaScript and XML files:

    - `/weather_app/view/App.view.xml`
    - `/weather_app/view/Details.controller.js`
    - `/weather_app/view/Details.view.xml`
    - `/weather_app/view/List.controller.js`
    - `/weather_app/view/List.view.xml`
    - `/weather_app/Component.js`
    - `/weather_app/util/formatter.js`

    >Note that `Component.js` is in the `weather_app` directory, not the `weather_app/view` directory, and you must create the `util` directory for the `formatter.js` file.

    ![Project explorer for UI5 view](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part9/e2e_09-3.png)

4. Copy and paste the content from GitHub into the respective files and familiarize yourself with the content of the individual files.

    - `/weather_app/view/`[`App.view.xml`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/App.view.xml)
    - `/weather_app/view/`[`Details.controller.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/Details.controller.js)
    - `/weather_app/view/`[`Details.view.xml`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/Details.view.xml)
    - `/weather_app/view/`[`List.controller.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/List.controller.js)
    - `/weather_app/view/`[`List.view.xml`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/view/List.view.xml)
    - `/weather_app/`[`Component.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/weather_app/Component.js)
    - `/weather_app`[`/util/formatter.js`](https://raw.githubusercontent.com/SAP/cloud-weatherapp/master/src/main/webapp/weather_app/util/formatter.js)


5. Replace the content of the `index.html` file to match this [version in Github](https://raw.githubusercontent.com/SAP/cloud-weatherapp/4c6c4ca78a680042fda82ab1d413e520608b581c/src/main/webapp/index.html).

6. Publish/deploy your updated app and navigate your browser to the root URL: <http://localhost:8080/weatherapp>. After successful authentication you should see a fully operational UI.

    ![App example showing weather info for a specific city](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-java-weatherapp-part9/e2e_09-6.png)

    >Note: Feel free beautify the UI by applying formatters and respective icons etc.


## Next Steps
 - [End-to-End Weather App Scenario Part 10](http://go.sap.com/developer/tutorials/hcp-java-weatherapp-part10.html)
