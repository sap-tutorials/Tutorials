---
title: Create an application processing geospatial data using Hibernate on SAP HANA
description: This tutorial shows how to create an application for processing geospatial data using Hibernate on SAP HANA leveraging the SAP HANA Spatial engine.
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>java, products>sap-hana, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Intermediate

## Details
### You will learn  
In this tutorial you will learn how to create an application that process geospatial data using Hibernate on SAP HANA to create the data model, process the data, and interact with the database.

### Time to Complete
**15 Min**


[ACCORDION-BEGIN [Step 1: ](The GDELT data)]

The data set you will use for the tutorial is the one provided by the Global Database of Events, Language, and Tone data (GDELT) version 1.0 from the [GDELT project](http://gdeltproject.org/).

The GDELT 1.0 event dataset comprises over 3.5 billion mentions of over 364 million distinct events from almost every corner of the earth spanning January 1979 to present and updated daily.

The [file format documentation](http://data.gdeltproject.org/documentation/GDELT-Data_Format_Codebook.pdf) describes the various fields and their structure.

In addition, tab-delimited lookup files are available that contain the human-friendly textual labels for each of those codes to make it easier to work with the data for those who have not previously worked with CAMEO.

Lookups are available for both [Event Codes](http://gdeltproject.org/data/lookups/CAMEO.eventcodes.txt) and the [Goldstein Scale](http://gdeltproject.org/data/lookups/CAMEO.goldsteinscale.txt).

In addition, detailed recording characteristics of the actors involved in each event are stored as a sequence of 3 character codes.

Lookups are available for:

 - [Country Codes](http://gdeltproject.org/data/lookups/CAMEO.country.txt)
 - [Type Codes](http://gdeltproject.org/data/lookups/CAMEO.type.txt)
 - [Known Group Codes](http://gdeltproject.org/data/lookups/CAMEO.knowngroup.txt)
 - [Ethnic Codes](http://gdeltproject.org/data/lookups/CAMEO.ethnic.txt)
 - [Religion Codes](http://gdeltproject.org/data/lookups/CAMEO.religion.txt).

And you can download the complete collection as a series of Event CSV files here:

- <http://data.gdeltproject.org/events/index.html>

You can find more details about GDELT version 1 here:

- <http://gdeltproject.org/data.html#documentation>

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Update the project configuration)]

In order to be able to work with geospatial data in Hibernate, the Hibernate Spatial artefact must be added as a dependency to the Maven project descriptor `pom.xml`.

Open the **`pom.xml`** file and switch to the **Dependencies** tab.

![pom](pom.png)

Click on **Add** in the **Dependencies** area (not the **Dependency Management**).

Enter the following details:

- Group Id: `org.hibernate`
- Artifact Id: `hibernate-spatial`
- Version: `5.2.12.Final`

![Add Hibernate Maven dependency](add-hibernate-dependency.png)

Click on **OK**

Save the `pom.xml` file.

After the Hibernate Spatial dependency has been added, the persistence configuration must be adapted to use the SAP HANA spatial dialect by replacing
  - ***`org.hibernate.dialect.HANAColumnStoreDialect`***
by:
  - ***`org.hibernate.spatial.dialect.hana.HANASpatialDialect`***

Open the **`persistence.xml`** file located at **`src/main/resources/META-INF`** and change the configuration to resemble the following:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<persistence xmlns="http://java.sun.com/xml/ns/persistence" version="1.0">
    <persistence-unit name="Tutorial">
        <provider>org.hibernate.jpa.HibernatePersistenceProvider</provider>
        <properties>
          <!-- switch to spatial dialect-->
          <property name="hibernate.dialect" value="org.hibernate.spatial.dialect.hana.HANASpatialDialect"/>
          <property name="hibernate.connection.driver_class" value="com.sap.db.jdbc.Driver"/>
          <!-- update the <server host> and <port>-->
          <property name="hibernate.connection.url" value="jdbc:sap://<server host>:<port>"/>
          <!-- update the <username>-->
          <property name="hibernate.connection.username" value="<username>"/>
          <!-- update the <password>-->
          <property name="hibernate.connection.password" value="<password>"/>
          <!-- -->
          <property name="hibernate.connection.pool_size" value="5"/>
          <property name="hibernate.show_sql" value="false"/>
          <property name="hibernate.format_sql" value="false"/>
          <property name="hibernate.hbm2ddl.auto" value="create-drop"/>
          <property name="hibernate.bytecode.provider" value="javassist" />
          <property name="hibernate.bytecode.use_reflection_optimizer" value="true" />
          <property name="hibernate.jdbc.batch_size" value="10000" />
        </properties>
    </persistence-unit>
</persistence>
```

Don't forget to update the following property values to match your target SAP HANA instance:

 - `hibernate.connection.url`
 - `hibernate.connection.username`
 - `hibernate.connection.password`

Save the `persistence.xml` file.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Download a GDELT data file)]

Download one of the GDELT zipped CSV file, for example `20171008.export.CSV.zip`, from <http://data.gdeltproject.org/events/index.html>.

Save the file locally, then extract the contents of the zip file into the `src/main/resources` project folder.

> ***Hint:*** Copy-Paste works from Windows Explorer.

&nbsp;

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Define the model)]

The GDELT data can be modelled using four entities:

 - Event
 - Actor
 - Action
 - Geography

The entities need to define the attributes listed in the [GDELT file format documentation](http://data.gdeltproject.org/documentation/GDELT-Data_Format_Codebook.pdf).

Therefore, we will need to create the Java classes representing the entities during the following steps.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create the Actor Entity)]

This entity defines the attributes listed in the documentation and uses the code as the identifier.

Create a Java class named **`Actor`** in a package named **`com.sap.hana.hibernate.tutorial.spatial`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.spatial;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
public class Actor {

	@Id
	private String code;
	private String name;
	private String countryCode;
	private String knownGroupCode;
	private String ethnicCode;
	private String religion1Code;
	private String religion2Code;
	private String type1Code;
	private String type2Code;
	private String type3Code;
}
```

Now, generate the ***Getters*** and ***Setters*** for all the attributes using the ***Source -> Generate Getters and Setters...*** menu bar or using a right click in the code, click on ***Select All***, and then click on ***Finish***.

Save the class file.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create the Action Entity)]

Like the actor class this class defines the attributes listed in the documentation and uses the code as the identifier.

Create a Java class named **`Action`** in a package named **`com.sap.hana.hibernate.tutorial.spatial`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.spatial;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
public class Action {

	private boolean rootEvent;
	@Id
	private String code;
	private String baseCode;
	private String rootCode;
	private int quadClass;
	private double goldsteinScale;
	private int numMentions;
	private int numSources;
	private int numArticles;
	private double avgTone;
}
```

Now, generate the ***Getters*** and ***Setters*** for all the attributes using the ***Source -> Generate Getters and Setters...*** menu bar or using a right click in the code, click on ***Select All***, and then click on ***Finish***.

Save the class file.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create the Geography Entity)]

The geography entity is marked as an embeddable entity via the `@Embeddable` annotation. Because of this the geography entities will not be stored in a separate table. Instead they will be stored in the table of a parent entity, in this case the event entity.

Create a Java class named **`Geography`** in a package named **`com.sap.hana.hibernate.tutorial.spatial`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.spatial;

import javax.persistence.Embeddable;

import org.geolatte.geom.C2D;
import org.geolatte.geom.Point;

@Embeddable
public class Geography {

	private int type;
	private String fullName;
	private String countryCode;
	private String adm1Code;
	private Point<C2D> position;
	private int featureId;
}
```
Now, generate the ***Getters*** and ***Setters*** for all the attributes using the ***Source -> Generate Getters and Setters...*** menu bar or using a right click in the code, click on ***Select All***, and then click on ***Finish***.

Save the class file.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create the Event Entity)]

This class defines the event-level attributes listed in the documentation and contains associations to the other entities. It uses the global event ID as the identifier.

The event-level attributes combined with the referenced entities describe one complete record of the GDELT data set.

Create a Java class named **`Event`** in a package named **`com.sap.hana.hibernate.tutorial.spatial`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.spatial;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

@Entity
public class Event {

	private static final SimpleDateFormat DAY_DATE_FORMAT = new SimpleDateFormat("yyyyMMdd");
	private static final SimpleDateFormat MONTH_DATE_FORMAT = new SimpleDateFormat("yyyyMM");

	@Id
	private int id;
	private Date day;
	private Date month;
	private int year;
	private double fractionDate;
	@ManyToOne(optional = true)
	private Actor actor1;
	@ManyToOne(optional = true)
	private Actor actor2;
	@ManyToOne
	private Action action;
  @Embedded
	@AttributeOverrides(value = { @AttributeOverride(name = "type", column = @Column(name = "actor1Geo_type")),
			@AttributeOverride(name = "fullName", column = @Column(name = "actor1Geo_fullName")),
			@AttributeOverride(name = "countryCode", column = @Column(name = "actor1Geo_countryCode")),
			@AttributeOverride(name = "adm1Code", column = @Column(name = "actor1Geo_adm1Code")),
			@AttributeOverride(name = "position", column = @Column(name = "actor1Geo_position")),
			@AttributeOverride(name = "featureId", column = @Column(name = "actor1Geo_featureId")) })
	private Geography actor1Geo;
	@Embedded
	@AttributeOverrides(value = { @AttributeOverride(name = "type", column = @Column(name = "actor2Geo_type")),
			@AttributeOverride(name = "fullName", column = @Column(name = "actor2Geo_fullName")),
			@AttributeOverride(name = "countryCode", column = @Column(name = "actor2Geo_countryCode")),
			@AttributeOverride(name = "adm1Code", column = @Column(name = "actor2Geo_adm1Code")),
			@AttributeOverride(name = "position", column = @Column(name = "actor2Geo_position")),
			@AttributeOverride(name = "featureId", column = @Column(name = "actor2Geo_featureId")) })
	private Geography actor2Geo;
	@Embedded
	@AttributeOverrides(value = { @AttributeOverride(name = "type", column = @Column(name = "actionGeo_type")),
			@AttributeOverride(name = "fullName", column = @Column(name = "actionGeo_fullName")),
			@AttributeOverride(name = "countryCode", column = @Column(name = "actionGeo_countryCode")),
			@AttributeOverride(name = "adm1Code", column = @Column(name = "actionGeo_adm1Code")),
			@AttributeOverride(name = "position", column = @Column(name = "actionGeo_position")),
			@AttributeOverride(name = "featureId", column = @Column(name = "actionGeo_featureId")) })
	private Geography actionGeo;
	private Date dateAdded;
	private String sourceURL;

	public void setDay(String day) {
		try {
			this.day = DAY_DATE_FORMAT.parse(day);
		} catch (ParseException e) {
			throw new IllegalArgumentException("Invalid day date: " + day, e);
		}
	}

	public void setMonth(String month) {
		try {
			this.month = MONTH_DATE_FORMAT.parse(month);
		} catch (ParseException e) {
			throw new IllegalArgumentException("Invalid month date: " + month, e);
		}
	}
}
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Test The Spatial Data Import)]

In this application we will be parsing the event data from the downloaded CSV file and then storing it in the database

Parsing the data consists of opening the CSV file, reading the data line by line, splitting each line into fields, setting the value of the appropriate entity, and finally persisting the entities in the database.

Create a Java class named **`TestSpatialDataImport`** in a package named **`com.sap.hana.hibernate.tutorial.spatial`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

> **Note:** Before running the application make sure to adjust the path of the CSV file to the actual file location (***`FILE_NAME`***).

```Java
package com.sap.hana.hibernate.tutorial.spatial;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import org.geolatte.geom.C2D;
import org.geolatte.geom.Point;
import org.geolatte.geom.crs.CoordinateReferenceSystem;
import org.geolatte.geom.crs.CoordinateReferenceSystems;
import org.geolatte.geom.crs.Unit;

public class TestSpatialDataImport {

	// The GDELT file name
	private static final String FILE_NAME = "20171105.export.CSV";

	// The flush size
	private static final int FLUSH_SIZE = 10000;

	// The default HANA coordinate reference system with ID 0
	private static final CoordinateReferenceSystem<C2D> DEFAULT_CRS = CoordinateReferenceSystems.mkProjected(0,
			Unit.METER);

	// SimpleDateFormat instance for parsing the date when an event was added
	private static final SimpleDateFormat DAY_DATE_FORMAT = new SimpleDateFormat("yyyyMMdd");

	public static void main(String[] args) {
		try {
			EntityManagerFactory entityManagerFactory = Persistence.createEntityManagerFactory("Tutorial");
			EntityManager entityManager = entityManagerFactory.createEntityManager();

			Path csvFilePath = Paths.get(Thread.currentThread().getContextClassLoader().getResource(FILE_NAME).toURI());
			parseAndStoreCSV(csvFilePath, entityManager);

			entityManager.clear();
			entityManager.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.exit(0);
	}

	public static void parseAndStoreCSV(Path filePath, EntityManager entityManager)
			throws IOException, ParseException {
		// Open the CSV file containing the data
		try (BufferedReader reader = Files.newBufferedReader(filePath)) {
			String line = null;
			int numberOfRecords = 0;

			// Start a database transaction
			entityManager.getTransaction().begin();

			// Read the CSV file line by line
			while ((line = reader.readLine()) != null) {
				Event event = parseEvent(line, entityManager);
				entityManager.persist(event);

				numberOfRecords++;

				// Clear the entity manager cache every 1000 records to avoid
				// excessive memory usage
				if (numberOfRecords % FLUSH_SIZE == 0) {
					System.out.println("Flushing after " + numberOfRecords + " records");

					entityManager.flush();
					entityManager.clear();
				}
			}
			System.out.println("Imported " + numberOfRecords + " records");

			// Commit the database transaction
			entityManager.getTransaction().commit();
		}
	}

	private static Event parseEvent(String line, EntityManager entityManager) throws IOException, ParseException {
		// Split the line by tab separator
		String[] fields = line.split("\t");
		assert fields.length == 58;

		Event event = new Event();
		event.setId(parseInt(fields[0]));
		event.setDay(fields[1]);
		event.setMonth(fields[2]);
		event.setYear(parseInt(fields[3]));
		event.setFractionDate(parseDouble(fields[4]));

		String actor1Code = fields[5];
		if (actor1Code != null && !actor1Code.trim().isEmpty()) {
			Actor actor1 = entityManager.find(Actor.class, actor1Code);
			if (actor1 == null) {
				actor1 = new Actor();
				actor1.setCode(actor1Code);
				actor1.setName(fields[6]);
				actor1.setCountryCode(fields[7]);
				actor1.setKnownGroupCode(fields[8]);
				actor1.setEthnicCode(fields[9]);
				actor1.setReligion1Code(fields[10]);
				actor1.setReligion2Code(fields[11]);
				actor1.setType1Code(fields[12]);
				actor1.setType2Code(fields[13]);
				actor1.setType3Code(fields[14]);

				entityManager.persist(actor1);
			}
			event.setActor1(actor1);
		}

		String actor2Code = fields[15];
		if (actor2Code != null && !actor2Code.trim().isEmpty()) {
			Actor actor2 = entityManager.find(Actor.class, actor2Code);
			if (actor2 == null) {
				actor2 = new Actor();

				actor2.setCode(actor2Code);
				actor2.setName(fields[16]);
				actor2.setCountryCode(fields[17]);
				actor2.setKnownGroupCode(fields[18]);
				actor2.setEthnicCode(fields[19]);
				actor2.setReligion1Code(fields[20]);
				actor2.setReligion2Code(fields[21]);
				actor2.setType1Code(fields[22]);
				actor2.setType2Code(fields[23]);
				actor2.setType3Code(fields[24]);

				entityManager.persist(actor2);
			}
			event.setActor2(actor2);
		}

		String actionCode = fields[26];
		Action action = entityManager.find(Action.class, actionCode);
		if (action == null) {
			action = new Action();
			action.setRootEvent("1".equals(fields[25]));
			action.setCode(actionCode);
			action.setBaseCode(fields[27]);
			action.setRootCode(fields[28]);
			action.setQuadClass(parseInt(fields[29]));
			action.setGoldsteinScale(parseDouble(fields[30]));
			action.setNumMentions(parseInt(fields[31]));
			action.setNumSources(parseInt(fields[32]));
			action.setNumArticles(parseInt(fields[33]));
			action.setAvgTone(parseDouble(fields[34]));

			entityManager.persist(action);
		}
		event.setAction(action);

		Geography actor1Geo = new Geography();
		int actor1Geo_Type = parseInt(fields[35]);
		if (actor1Geo_Type > 0) {
			actor1Geo.setType(actor1Geo_Type);
			actor1Geo.setFullName(fields[36]);
			actor1Geo.setCountryCode(fields[37]);
			actor1Geo.setAdm1Code(fields[38]);
			setPosition(actor1Geo, fields[39], fields[40]);
			setFeatureId(actor1Geo, actor1Geo_Type, fields[41]);
		}
		event.setActor1Geo(actor1Geo);

		Geography actor2Geo = new Geography();
		int actor2Geo_Type = parseInt(fields[42]);
		if (actor2Geo_Type > 0) {
			actor2Geo.setType(actor2Geo_Type);
			actor2Geo.setFullName(fields[43]);
			actor2Geo.setCountryCode(fields[44]);
			actor2Geo.setAdm1Code(fields[45]);
			setPosition(actor2Geo, fields[46], fields[47]);
			setFeatureId(actor2Geo, actor2Geo_Type, fields[48]);
		}
		event.setActor2Geo(actor2Geo);

		Geography actionGeo = new Geography();
		int actionGeo_Type = parseInt(fields[49]);
		if (actionGeo_Type > 0) {
			actionGeo.setType(actionGeo_Type);
			actionGeo.setFullName(fields[50]);
			actionGeo.setCountryCode(fields[51]);
			actionGeo.setAdm1Code(fields[52]);
			setPosition(actionGeo, fields[53], fields[54]);
			setFeatureId(actionGeo, actionGeo_Type, fields[55]);
		}
		event.setActionGeo(actionGeo);

		event.setDateAdded(parseDate(fields[57]));

		return event;
	}

	private static void setFeatureId(Geography geography, int type, String featureId) {
		if (type > 2) {
			geography.setFeatureId(parseInt(featureId));
		}
	}

	private static void setPosition(Geography geography, String latitude, String longitude) {
		if (latitude.isEmpty() || longitude.isEmpty()) {
			return;
		}
		C2D location = new C2D(parseDouble(latitude), parseDouble(longitude));
		Point<C2D> position = new Point<C2D>(location, DEFAULT_CRS);
		geography.setPosition(position);
	}

	private static double parseDouble(String s) {
		if (s == null || s.isEmpty()) {
			return 0.d;
		}

		return Double.parseDouble(s);
	}

	private static int parseInt(String s) {
		if (s == null || s.isEmpty()) {
			return 0;
		}

		return Integer.parseInt(s);
	}

	private static Date parseDate(String s) {
		if (s == null || s.isEmpty()) {
			return null;
		}

		try {
			return DAY_DATE_FORMAT.parse(s);
		} catch (ParseException e) {
			return null;
		}
	}
}
```

> **Note:** Before running the application make sure to adjust the path of the CSV file to the actual file location (***`FILE_NAME`***).

Save the class file.

Run the application by right-clicking the class file and choosing ***Run As -> Java Application*** or click on the ![Run](run.png) icon.

You should see the following output log in your console:

```
...
INFO: HHH000204: Processing PersistenceUnitInfo [
	name: Tutorial
	...]
INFO: HHH000412: Hibernate Core {5.2.12.Final}
INFO: HHH000206: hibernate.properties not found
INFO: HHH80000001: hibernate-spatial integration enabled : true
INFO: HCANN000001: Hibernate Commons Annotations {5.0.1.Final}
WARN: HHH10001002: Using Hibernate built-in connection pool (not for production use!)
INFO: HHH10001005: using driver [com.sap.db.jdbc.Driver] at URL [jdbc:sap://rhhxehost:39015]
INFO: HHH10001001: Connection properties: {user=SYSTEM, password=****}
INFO: HHH10001003: Autocommit mode: false
INFO: HHH000115: Hibernate connection pool size: 5 (min=1)
INFO: HHH000400: Using dialect: org.hibernate.spatial.dialect.hana.HANASpatialDialect
INFO: HHH000424: Disabling contextual LOB creation as createClob() method threw error : java.lang.reflect.InvocationTargetException
INFO: HHH10001501: Connection obtained from JdbcConnectionAccess [org.hibernate.engine.jdbc.env.internal.JdbcEnvironmentInitiator$ConnectionProviderJdbcConnectionAccess@6d24ffa1] for (non-JTA) DDL execution was not in auto-commit mode; the Connection 'local transaction' will be committed and the Connection will be set into auto-commit mode.
INFO: HHH10001501: Connection obtained from JdbcConnectionAccess [org.hibernate.engine.jdbc.env.internal.JdbcEnvironmentInitiator$ConnectionProviderJdbcConnectionAccess@773cbf4f] for (non-JTA) DDL execution was not in auto-commit mode; the Connection 'local transaction' will be committed and the Connection will be set into auto-commit mode.
INFO: HHH000476: Executing import script 'org.hibernate.tool.schema.internal.exec.ScriptSourceInputNonExistentImpl@2756c0a7'
Flushing after 10000 records
Flushing after 20000 records
Flushing after 30000 records
Flushing after 40000 records
Flushing after 50000 records
Flushing after 60000 records
Flushing after 70000 records
Flushing after 80000 records
Flushing after 90000 records
Flushing after 100000 records
Flushing after 110000 records
Imported 112207 records
```

> **Note:** Depending on which data set file you will be using, the number of records and the number of events in Europe might be different.
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Test The Spatial Data Querying)]

Now that the data is loaded in our database, we run spatial queries.

As an example for a query, the code bellow retrieves and counts all events whose action is located in Europe (at least approximately).

Create a Java class named **`TestSpatialDataQuery`** in a package named **`com.sap.hana.hibernate.tutorial.spatial`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.spatial;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.TypedQuery;

import org.geolatte.geom.Polygon;
import org.geolatte.geom.codec.Wkt;
import org.geolatte.geom.codec.WktDecoder;

public class TestSpatialDataQuery {

	// The GDELT file name
	private static final String FILE_NAME = "20171105.export.CSV";

	public static void main(String[] args) {
		try {
			// TestSpatialDataImport.main(args);
			EntityManagerFactory entityManagerFactory = Persistence.createEntityManagerFactory("Tutorial");
			EntityManager entityManager = entityManagerFactory.createEntityManager();

			Path csvFilePath = Paths.get(Thread.currentThread().getContextClassLoader().getResource(FILE_NAME).toURI());
			TestSpatialDataImport.parseAndStoreCSV(csvFilePath, entityManager);

			// Select all events within a region
			TypedQuery<Event> eventQuery = entityManager
					.createQuery("select e from Event e where within(actionGeo.position, :area) = true", Event.class);

			// Create a polygon describing a box around Europe
			WktDecoder decoder = Wkt.newDecoder(Wkt.Dialect.HANA_EWKT);
			Polygon<?> europeBox = (Polygon<?>) decoder.decode("POLYGON((35 -10, 35 30, 71 30, 71 -10, 35 -10))");
			eventQuery.setParameter("area", europeBox);

			// Return all events within the Europe box
			List<Event> eventsInEurope = eventQuery.getResultList();

			System.out.println("Found " + eventsInEurope.size() + " events in Europe");

			entityManager.clear();
			entityManager.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.exit(0);
	}
}
```

Save the class file.

Run the application by right-clicking the class file and choosing ***Run As -> Java Application*** or click on the ![Run](run.png) icon.

You should see the following output log in your console:

```
...
INFO: HHH000204: Processing PersistenceUnitInfo [
	name: Tutorial
	...]
INFO: HHH000412: Hibernate Core {5.2.12.Final}
INFO: HHH000206: hibernate.properties not found
INFO: HHH80000001: hibernate-spatial integration enabled : true
INFO: HCANN000001: Hibernate Commons Annotations {5.0.1.Final}
WARN: HHH10001002: Using Hibernate built-in connection pool (not for production use!)
INFO: HHH10001005: using driver [com.sap.db.jdbc.Driver] at URL [jdbc:sap://rhhxehost:39015]
INFO: HHH10001001: Connection properties: {user=SYSTEM, password=****}
INFO: HHH10001003: Autocommit mode: false
INFO: HHH000115: Hibernate connection pool size: 5 (min=1)
INFO: HHH000400: Using dialect: org.hibernate.spatial.dialect.hana.HANASpatialDialect
INFO: HHH000424: Disabling contextual LOB creation as createClob() method threw error : java.lang.reflect.InvocationTargetException
INFO: HHH10001501: Connection obtained from JdbcConnectionAccess [org.hibernate.engine.jdbc.env.internal.JdbcEnvironmentInitiator$ConnectionProviderJdbcConnectionAccess@6d24ffa1] for (non-JTA) DDL execution was not in auto-commit mode; the Connection 'local transaction' will be committed and the Connection will be set into auto-commit mode.
INFO: HHH10001501: Connection obtained from JdbcConnectionAccess [org.hibernate.engine.jdbc.env.internal.JdbcEnvironmentInitiator$ConnectionProviderJdbcConnectionAccess@773cbf4f] for (non-JTA) DDL execution was not in auto-commit mode; the Connection 'local transaction' will be committed and the Connection will be set into auto-commit mode.
INFO: HHH000476: Executing import script 'org.hibernate.tool.schema.internal.exec.ScriptSourceInputNonExistentImpl@2756c0a7'
Flushing after 10000 records
Flushing after 20000 records
Flushing after 30000 records
Flushing after 40000 records
Flushing after 50000 records
Flushing after 60000 records
Flushing after 70000 records
Flushing after 80000 records
Flushing after 90000 records
Flushing after 100000 records
Flushing after 110000 records
Imported 112207 records
INFO: HHH000397: Using ASTQueryTranslatorFactory
Found 14412 events in Europe
```

[ACCORDION-END]
