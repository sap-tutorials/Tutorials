---
title: Building translytical applications with Hibernate on SAP HANA - Setup
description: This tutorial will guide you through the steps required for setting up the code, the Google Cloud Platform project, and the data.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>intermediate, topic>java, products>sap-hana\,-express-edition ]
time: 30
---

## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** [SAP HANA, express edition, in Google Cloud Platform Launcher (Database Services)](https://www.sap.com/developer/tutorials/hxe-gcp-getting-started-launcher.html)

## Details
### You will learn  
  - How to set up the Google Cloud Platform project, the application code, and the required external data.

---

[ACCORDION-BEGIN [Step 1: ](Prerequisites)]
You'll need the following software installed before setting up the project:

- A Git client. If you don't have on installed you can download one at [https://git-scm.com/downloads](https://git-scm.com/downloads)
- Apache Maven, available at [http://maven.apache.org/download.cgi](http://maven.apache.org/download.cgi)
- A Google Cloud account
- A code editor. For example, [Eclipse](http://www.eclipse.org/downloads/packages/release/photon/r/eclipse-ide-java-ee-developers)
- A zip tool capable of creating split zip archives

Please also complete the tutorial [SAP HANA, express edition, in Google Cloud Platform Launcher (Database Services)](https://www.sap.com/developer/tutorials/hxe-gcp-getting-started-launcher.html) including all optional steps.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Check out the application code)]
The application code is available on [GitHub](https://github.com/SAP/hxe-hibernate-google-app-engine).

Clone it to your local machine using a Git client and switch to the branch `tutorial`.

```
git clone --branch tutorial https://github.com/SAP/hxe-hibernate-google-app-engine.git
```

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Setup the code editor)]
You can import the project as a Maven project into Eclipse.

Open the ***Eclipse IDE***.

Import the project by selecting ***File -> Import...***, then choosing ***Maven -> Existing Maven Projects***

![Import Maven project](import-maven-project.png)

Browse to the location into which you cloned the code and afterwards click on the ***Finish*** button.

![Select Maven project location](select-maven-project-location.png)

You should now see the project in the Eclipse project explorer.

![Eclipse project explorer](eclipse-project-explorer.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Setup the Google Maps Javascript API)]

In the [Google Cloud Platform Console](https://console.cloud.google.com) browse to ***APIs & Services -> Library***

![API library](api-library.png)

Search for `Maps Javascript API`

![API library search](api-library-search.png)

Click on the tile labeled `Maps JavaScript API` and click on the ***Enable*** button

![Google Maps Javascript API](maps-api-enable.png)

To be able to use the Maps JavaScript API you also need to create an API key. Navigate to ***APIs & Services -> Credentials***

![API credentials](api-credentials.png)

Click on ***Create credentials -> API key*** to create a new API key.

![Create new API key](create-api-key.png)

You'll see a popup window displaying the API key. It is strongly recommended to restrict the API key to your applications to prevent unauthorized use. To restrict the key click on the ***Restrict key*** button

![Restrict the API key](restrict-api-key-dialog.png)

You can choose any restriction type that fits your use case.

For example, to restrict the key to certain domains you can choose the restriction type ***HTTP referrers (web sites)*** and enter the URLs of the sites that are allowed to use the API key. Good choices are `http://localhost:8080/*` for local testing and `https://<your project ID>.appspot.com/*` for running the application on the Google App Engine (make sure to replace `<your project ID>` with your actual Google Cloud project ID).

When you're finished entering the referrers click on ***Save***

![Restrict the API key](restrict-api-key.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Setup the Google App Engine)]
If you haven't done so yet, you need to set up the Google App Engine for you project. To do this, navigate to ***App Engine -> Dashboard*** in the [Google Cloud Platform Console](https://console.cloud.google.com).

![App Engine dashboard](app-engine-dashboard.png)

In the tile labeled ***Your first app*** click on ***Select a language*** and choose ***Java***

![App Engine setup](app-engine-setup.png)

Choose the same region that you already chose for your SAP HANA, express edition instance to minimize the latency between the App Engine and the Compute Engine.

Then click on ***Next*** and wait for the setup to complete.

![App Engine location](app-engine-location.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Prepare the data)]
Download the [CSV version](https://data.sfgov.org/api/views/tmnf-yvry/rows.csv?accessType=DOWNLOAD) of the [San Francisco Police Department Incidents data set](https://data.sfgov.org/api/views/tmnf-yvry/) and the [CSV version](https://data.sfgov.org/api/views/sr5d-tnui/rows.csv?accessType=DOWNLOAD) of the [San Francisco Addresses data set](https://data.sfgov.org/Geographic-Locations-and-Boundaries/Addresses-Enterprise-Addressing-System/sr5d-tnui) and place them into the `src/main/resources/csv` directory as `incidents.csv` and `addresses.csv`, respectively.

Create two ZIP archives named `incidents.zip` and `addresses.zip`, respectively, each containing the corresponding CSV file and make sure to have the zip tool split the archives into parts no larger than 30 megabytes. Otherwise the resulting files can't be uploaded to the Google App Engine.

Once the ZIP files have been created delete the two CSV files.

Your  `src/main/resources/csv` directory should now contain 6 files.

```
src/main/resources/csv
|- addresses.zip
|- incidents.zip
|- incidents.z01
|- incidents.z02
|- incidents.z03
\- PLACE_CSV_FILES_HERE
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Update the application build descriptor)]
Open the file `pom.xml` in the project's root directory and update the build properties to match your settings

```xml
<properties>
  <spring.version>5.0.6.RELEASE</spring.version>
  <spring-data.version>Kay-SR7</spring-data.version>
  <apt.version>1.1.3</apt.version>
  <hibernate.version>5.3.1.Final</hibernate.version>
  <jackson.version>2.9.4</jackson.version>
  <flyway.version>5.1.3</flyway.version>

  <jdbc.username>DEMO_USER</jdbc.username>
  <jdbc.password>D3m0-U$eR</jdbc.password>
  <jdbc.host>hxehost</jdbc.host>
  <jdbc.port>39015</jdbc.port>
  <jdbc.url>jdbc:sap://${jdbc.host}:${jdbc.port}/</jdbc.url>

  <google.project.id>REPLACE_WITH_YOUR_PROJECT_ID</google.project.id>
  <google.api.key>REPLACE_WITH_YOUR_API_KEY</google.api.key>
  <maps.api.key.parameter>key=${google.api.key}</maps.api.key.parameter>
</properties>
```

You'll have to update the properties `jdbc.username`, `jdbc.password`, `jdbc.host`, and `jdbc.port` to match the actual connection data of your database instance.

You'll also have to update the properties `google.project.id` to match your Google Cloud Platform project ID and `google.api.key` to match the API key created earlier.

After the adjustment the properties should look something like this:

```xml
<properties>
  <spring.version>5.0.6.RELEASE</spring.version>
  <spring-data.version>Kay-SR7</spring-data.version>
  <apt.version>1.1.3</apt.version>
  <hibernate.version>5.3.1.Final</hibernate.version>
  <jackson.version>2.9.4</jackson.version>
  <flyway.version>5.1.3</flyway.version>

  <jdbc.username>WORKSHOP_01</jdbc.username>
  <jdbc.password>P4ssword</jdbc.password>
  <jdbc.host>132.154.10.5</jdbc.host>
  <jdbc.port>39015</jdbc.port>
  <jdbc.url>jdbc:sap://${jdbc.host}:${jdbc.port}/</jdbc.url>

  <google.project.id>hxe-hibernate-tutorial</google.project.id>
  <google.api.key>AIzaSyAVCKWHyKRc8NR4RdeFm_JWfFStrRVQYpw</google.api.key>
  <maps.api.key.parameter>key=${google.api.key}</maps.api.key.parameter>
</properties>
```

Save the `pom.xml` file.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Update the application configuration)]

Open the file `application.properties` in the directory `src/main/resources`.

Update the properties `csv.incidents.file.location` and `csv.addresses.file.location` to point to the ZIP archives created earlier.

```
spring.datasource.url=@jdbc.url@
spring.jpa.database-platform=org.hibernate.spatial.dialect.hana.HANASpatialDialect
spring.datasource.username=@jdbc.username@
spring.datasource.password=@jdbc.password@
spring.datasource.driver-class-name=com.sap.db.jdbc.Driver

spring.jpa.hibernate.ddl-auto=validate
spring.jpa.show-sql=false
spring.jpa.properties.hibernate.format_sql=true

spring.jpa.properties.hibernate.jdbc.batch_size=50

logging.level.org.hibernate.type.descriptor.sql.BasicBinder=warning
logging.level.org.hibernate.type.descriptor.sql.BasicExtractor=warning

translation.enabled=false
translation.api.key=@google.api.key@

csv.incidents.file.location=/csv/incidents.zip
csv.addresses.file.location=/csv/addresses.zip
```

Save the `application.properties` file.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Deploy the application)]
Now the application is ready to be deployed to the Google App Engine. To do this you can use the Google App Engine Maven plugin.

In a console run the following command from the root directory of the project

```
mvn clean appengine:update
```

The application will be deployed to the Google App Engine.

The initial deployment will take some time because the data files need to be uploaded and the data needs to be loaded into the database.

After the deployment has succeeded you can navigate to `https://<your project ID>.appspot.com`. If the deployment was successful, you should see the web UI.

![Web UI](web-ui.png)

[ACCORDION-END]

---

## Next Steps
- [Building translytical applications with Hibernate on SAP HANA - Geospatial queries](https://www.sap.com/developer/tutorials/hxe-gcp-hibernate-spatial.html)
