---
title: Create an app consuming calculation view using Hibernate on SAP HANA
description: Create an application that uses Hibernate on SAP HANA to read data from a calculation view.
primary_tag: products>sap-hana
auto_validation: true
tags: [  tutorial>intermediate, topic>java, products>sap-hana, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** [Get started with Hibernate on SAP HANA, express edition](https://developers.sap.com/group.hana-hibernate-getting-started.html) & [SAP HANA XS Advanced, Creating a Graphical Calculation View](https://developers.sap.com/tutorials/xsa-graphical-view.html)

## Details
### You will learn  
In this tutorial you will learn how to create an application that reads data from a calculation view using Hibernate on SAP HANA for the mapping of the results into Java objects.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Prerequisites: ](Tutorials)]

If you haven't done so, please complete the tutorial [SAP HANA XS Advanced, Creating a Graphical Calculation View](https://developers.sap.com/tutorials/xsa-graphical-view.html). This tutorial builds on top of the calculation view created in that tutorial.

This tutorial builds upon the other tutorials in the tutorial group [Get started with Hibernate on SAP HANA, express edition](https://developers.sap.com/group.hana-hibernate-getting-started.html). Please complete these tutorials before starting this one.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Grant SQL access to the calculation view)]

To be able to use the calculation view from Hibernate, the Hibernate connection user must be given the `SELECT` privilege on the calculation view's database schema.

To do this you must first determine the physical database schema name of the calculation view. You can do this by checking the SAP Web IDE build log.

![SAP Web IDE Build Log](webide-build-output.png)

The container name from the build log corresponds to the physical schema name.

Each HDI container comes with an access role which can be granted to other database users to get access to the container.

To grant the container's access role to the Hibernate connection user, log in to the database with a user having the `ROLE ADMIN` system privilege, e.g. the SAP HANA `SYSTEM` user, and execute the following SQL statement:

```sql
GRANT "HANA_2_SPS1_HDI_DB_1::access_role" TO "<Hibernate user>"
```

Replace **`<Hibernate user>`** with your actual Hibernate connection user. If your physical schema name is not **`HANA_2_SPS1_HDI_DB_1`** replace this with your actual schema name as well.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Update project configuration)]

Since the calculation view isn't a table that can be managed by Hibernate, Hibernate must be configured to not create the schema on startup. This can be done by setting the value of the property `hibernate.hbm2ddl.auto` to `none`.

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
          <property name="hibernate.hbm2ddl.auto" value="none"/>
          <property name="hibernate.bytecode.provider" value="javassist" />
          <property name="hibernate.bytecode.use_reflection_optimizer" value="true" />
        </properties>
    </persistence-unit>
</persistence>
```

Don't forget to update the following property values to match your target SAP HANA instance:

 - `hibernate.connection.url`
 - `hibernate.connection.username`
 - `hibernate.connection.password`

Save the `persistence.xml` file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create entity for calculation view)]

In order for Hibernate to be able to run queries against the calculation view and to map the data from the calculation view to Java objects, a Java Persistence API (JPA) entity for the calculation view has to be created. The calculation view's output columns will be mapped as the fields of the entity.

Create a Java class named **`CdProduct`** in a package named **`com.sap.hana.hibernate.tutorial.cv`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.cv;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import org.geolatte.geom.C2D;
import org.geolatte.geom.Point;

@Entity
@Table(name = "HANA_2_SPS1.db.src.data.models::CD_PRODUCT", schema = "HANA_2_SPS1_DB_1")
public class CdProduct {

	private String category;
	@Id
	@Column(name = "NAMEID")
	private String nameId;
	@Column(name = "DESCID")
	private String descId;
	private String currency;
	private double price;
	@Column(name = "EMAILADDRESS")
	private String emailAddress;
	@Column(name = "ADDRESSES_ADDRESSID")
	private String addressesAddressId;
	@Column(name = "COMPANYNAME")
	private String companyName;
	@Column(name = "ADDRESSID")
	private String addressId;
	private String city;
	@Column(name = "POSTALCODE")
	private String postalCode;
	private String street;
	private String building;
	private String country;
	private String region;
	@Column(name = "ADDRESSTYPE")
	private String addressType;
	@Column(name = "VALIDITY_STARTDATE")
	private Date validityStartDate;
	@Column(name = "VALIDITY_ENDDATE")
	private Date validityEndDate;
	private double latitude;
	private double longitude;
	private Point<C2D> point;
	private String text;
}
```

If the physical schema name of your calculation view is not `HANA_2_SPS1_HDI_DB_1` replace the value of the `@Table` annotation's `schema` attribute with your actual schema name.

Now, generate the ***Getters*** and ***Setters*** for all the attributes using the ***Source -> Generate Getters and Setters...*** menu bar or using a right click in the code, click on ***Select All***, and then click on ***Finish***.

Save the class file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run simple query on calculation view)]

Using the entity created in the previous step, we can now run queries against the calculation view.

The example query in the code below retrieves and counts all products.

Create a Java class named **`TestCalculationViewDataQuery`** in a package named **`com.sap.hana.hibernate.tutorial.cv`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.cv;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.TypedQuery;

public class TestCalculationViewDataQuery {

	public static void main(String[] args) {
		try {
			EntityManagerFactory entityManagerFactory = Persistence.createEntityManagerFactory("Tutorial");
			EntityManager entityManager = entityManagerFactory.createEntityManager();

			// Select all products
			TypedQuery<CdProduct> allProductsQuery = entityManager.createQuery("select c from CdProduct c", CdProduct.class);

			List<CdProduct> products = allProductsQuery.getResultList();

			System.out.println("Found " + products.size() + " products");

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
Found 22472 products
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run Geo Spatial query on the calculation view)]

It's also possible to run more complex queries against a calculation view, for example, geospatial queries leveraging the SAP HANA geospatial engine.

The example query in the code below retrieves all products being supplied by a supplier located in Europe (at least approximately).

Create a Java class named **`TestCalculationViewSpatialQuery`** in a package named **`com.sap.hana.hibernate.tutorial.cv`** (either using a right-click on the project and choose ***New -> Class*** or use the ***File -> New -> Class*** menu bar), then paste the following content:

```java
package com.sap.hana.hibernate.tutorial.cv;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.TypedQuery;

import org.geolatte.geom.Polygon;
import org.geolatte.geom.codec.Wkt;
import org.geolatte.geom.codec.WktDecoder;

public class TestCalculationViewSpatialQuery {

	public static void main(String[] args) {
		try {
			EntityManagerFactory entityManagerFactory = Persistence.createEntityManagerFactory("Tutorial");
			EntityManager entityManager = entityManagerFactory.createEntityManager();

			// Select all products provided by suppliers within a region
			TypedQuery<CdProduct> regionalProductsQuery = entityManager.createQuery("select c from CdProduct c where within(c.point, :area) = true", CdProduct.class);

			// Create a polygon describing a box around Europe
			WktDecoder decoder = Wkt.newDecoder(Wkt.Dialect.HANA_EWKT);
			Polygon<?> europeBox = (Polygon<?>) decoder.decode("POLYGON((35 -10, 35 30, 71 30, 71 -10, 35 -10))");
			regionalProductsQuery.setParameter("area", europeBox);

			// Return all products provided by suppliers within the Europe box
			List<CdProduct> productsFromEurope = regionalProductsQuery.getResultList();

			System.out.println("Found " + productsFromEurope.size() + " products from Europe");

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
Found 10176 products from Europe
```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6 (optional): ](Set calculation view schema as default)]

For flexibility or maintainability reasons it might be preferable to not have the calculation view schema hard-coded on each entity.

```java
...
@Entity
@Table(name = "HANA_2_SPS1.db.src.data.models::CD_PRODUCT", schema = "HANA_2_SPS1_DB_1")
public class CdProduct {
	...
}
```

If you don't want to specify the calculation view schema on the JPA entity, or if you want to run an entire application against a schema generated by the SAP Web IDE, you can specify the default schema in the Hibernate configuration via the property `hibernate.default_schema`:

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
          <property name="hibernate.hbm2ddl.auto" value="none"/>
          <property name="hibernate.bytecode.provider" value="javassist" />
          <property name="hibernate.bytecode.use_reflection_optimizer" value="true" />

          <property name="hibernate.default_schema" value="HANA_2_SPS1_HDI_DB_1" />
        </properties>
    </persistence-unit>
</persistence>
```

Make sure to replace the value of the property `hibernate.default_schema` with the actual name of your schema.

Now you can remove the `schema` attribute from the entity's `@Table` annotation.

```java
...
@Entity
@Table(name = "HANA_2_SPS1.db.src.data.models::CD_PRODUCT")
public class CdProduct {
	...
}
```

The application will now run all queries including the queries against the calculation view with the container schema as the default schema.

[DONE]
[ACCORDION-END]

---
