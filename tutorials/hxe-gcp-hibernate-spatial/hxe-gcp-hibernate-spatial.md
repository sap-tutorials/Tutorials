---
title: Add Advanced Geospatial Processing with Hibernate
description: Make use of the SAP HANA geospatial engine via Hibernate Spatial.
auto_validation: false
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>intermediate, topic>java, products>sap-hana\,-express-edition ]
time: 20
---

## Prerequisites  
- **Tutorials:** [Prepare to build a translytical application with Hibernate](https://developers.sap.com/tutorials/hxe-gcp-hibernate-setup.html)

## Details
### You will learn  
  - How to make use of the SAP HANA geospatial engine via Hibernate Spatial

### Motivation
The initial implementation of the application not leveraging geospatial processing capabilities has some severe disadvantages.

Here is a query taken from the file `IncidentRepository.java` in the directory `src/main/java/com/sap/hana/hibernate/sample/repositories` that loads the police incidents within a certain distance around a given location:

```java
query = this.em.createQuery(
    "select i from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and i.x between (x(:location) - (cast(:distance as double) / 111319)) "
        + "    and (x(:location) + (cast(:distance as double) / 111319)) "
        + "  and i.y between (y(:location) - (cast(:distance as double) / 111319)) "
        + "    and (y(:location) + (cast(:distance as double) / 111319)) "
        + "order by i.date desc",
    Incident.class );
```

Some of the disadvantages of this query are:

- The query is somewhat long and hard to understand.
- The query uses a constant for converting a distance in meters to degrees which is less accurate the farther the given location is from the earth's equator.
- For simplicity, the query retrieves the incidents whose x and y coordinates are within +/- the distance which means that the area of the incidents is a rectangle rather than a circle.

All of these disadvantages can be addressed by using SAP HANA's built-in geospatial engine.

---

[ACCORDION-BEGIN [Step 1: ](Use geospatial functions for the incident list)]
Open the file `IncidentRepository.java` from the directory `src/main/java/com/sap/hana/hibernate/sample/repositories`.

In all queries, replace the part starting with

```
and i.y between
```

until the last

```
(cast(:distance as double) / 111319))
```

with a geospatial function. The function to be used is `dwithin` which calculates whether a location is within a given distance of another location.

**Before**
```java
query = this.em.createQuery(
    "select i from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and i.x between (x(:location) - (cast(:distance as double) / 111319)) "
        + "    and (x(:location) + (cast(:distance as double) / 111319)) "
        + "  and i.y between (y(:location) - (cast(:distance as double) / 111319)) "
        + "    and (y(:location) + (cast(:distance as double) / 111319)) "
        + "order by i.date desc",
    Incident.class );
```

**After**
```java
query = this.em.createQuery(
    "select i from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and dwithin(i.location, :location, :distance) = true "
        + "order by i.date desc",
    Incident.class );
```

As you can immediately see, the query is now much easier to read, doesn't contain any magic constants and performs the correct calculations taking into account the earth's geometry.

Do this adjustment for all 4 queries in the file.

```java
public Page<Incident> findByLocationNear(Point<G2D> location, Distance distance, Date dateFrom, Date dateTo,
    List<String> category, Pageable pageable) {
  TypedQuery<Incident> query;
  if ( category == null || category.isEmpty() ) {
    query = this.em.createQuery(
        "select i from Incident i "
            + "where i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.location, :location, :distance) = true "
            + "order by i.date desc",
        Incident.class );
  }
  else {
    query = this.em.createQuery(
        "select i from Incident i "
            + "where i.category in :category "
            + "  and i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.location, :location, :distance) = true "
            + "order by i.date desc",
        Incident.class );
    query.setParameter( "category", category );
  }

  query.setParameter( "dateFrom", dateFrom );
  query.setParameter( "dateTo", dateTo );
  query.setParameter( "location", location );
  query.setParameter( "distance", distance );

  query.setFirstResult( (int) pageable.getOffset() );
  query.setMaxResults( pageable.getPageSize() );

  Query countQuery;
  if ( category == null || category.isEmpty() ) {
    countQuery = this.em.createQuery(
        "select count(i) from Incident i "
            + "where i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.location, :location, :distance) = true " );

  }
  else {
    countQuery = this.em.createQuery(
        "select count(i) from Incident i "
            + "where i.category in :category "
            + "  and i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.location, :location, :distance) = true " );
    countQuery.setParameter( "category", category );
  }

  countQuery.setParameter( "dateFrom", dateFrom );
  countQuery.setParameter( "dateTo", dateTo );
  countQuery.setParameter( "location", location );
  countQuery.setParameter( "distance", distance );

  long count = ( (Long) countQuery.getSingleResult() ).longValue();

  return new PageImpl<>( query.getResultList(), pageable, count );
}
```

Save the `IncidentRepository.java` file.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Use geospatial functions for the incident heat map)]
Open the file `IncidentLocationAndCountRepository.java` from the directory `src/main/java/com/sap/hana/hibernate/sample/repositories`.

Like in the previous step, replace the part starting with

```
and i.y between
```
until the last

```
(cast(:distance as double) / 111319))
```

with a geospatial function in all queries.

In addition, we can now group the result by the geospatial location directly instead of the individual X and Y coordinates and use the location as the input of the `com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount` data transfer object (DTO).

**Before**
```java
query = this.em.createQuery(
    "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(i.x, i.y, count(*)) "
        + "from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and i.x between (x(:location) - (cast(:distance as double) / 111319)) "
        + "    and (x(:location) + (cast(:distance as double) / 111319)) "
        + "  and i.y between (y(:location) - (cast(:distance as double) / 111319)) "
        + "    and (y(:location) + (cast(:distance as double) / 111319)) "
        + "group by i.x, i.y",
    IncidentLocationAndCount.class );
```

**After**
```java
query = this.em.createQuery(
    "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(i.location, count(*)) "
        + "from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and dwithin(i.location, :location, :distance) = true "
        + "group by i.location",
    IncidentLocationAndCount.class );
```

Do these adjustments for both queries in the file.

```java
public List<IncidentLocationAndCount> findByLocationAndCategory(Point<G2D> location, Distance distance,
    Date dateFrom, Date dateTo, List<String> category) {
  TypedQuery<IncidentLocationAndCount> query;
  if ( category == null || category.isEmpty() ) {
    query = this.em.createQuery(
        "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(i.location, count(*)) "
            + "from Incident i "
            + "where i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.location, :location, :distance) = true "
            + "group by i.location",
        IncidentLocationAndCount.class );
  }
  else {
    query = this.em.createQuery(
        "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(i.location, count(*)) "
            + "from Incident i "
            + "where i.category in :category "
            + "  and i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.location, :location, :distance) = true "
            + "group by i.location",
        IncidentLocationAndCount.class );
    query.setParameter( "category", category );
  }

  query.setParameter( "dateFrom", dateFrom );
  query.setParameter( "dateTo", dateTo );
  query.setParameter( "location", location );
  query.setParameter( "distance", distance );

  return query.getResultList();
}
```

Save the `IncidentLocationAndCountRepository.java` file.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Deploy the application)]
With the geospatial changes in place we can now deploy the application again to the cloud.

In a console run the following command from the root directory of the project

```
mvn clean appengine:update
```

The application will be deployed to the Google App Engine.

This deployment should be much faster than the initial deployment since the large data files don't need to be transferred again and the database has already been loaded.

After the deployment has succeeded you can navigate to `https://<your project ID>.appspot.com` to see the changes.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Learn about Spatial Reference Systems (SRS))]
While the application now processes the incidents correctly, it does so rather slowly. The reason for this is the way that the geospatial data is stored inside the database. In the next steps you'll implement improvements for dealing with the geospatial data.

For an introduction of what spatial reference systems (SRS) are, check out the tutorial [Spatial Reference Systems](hana-spatial-intro6-srs).

The default spatial reference system used by the application is [WGS 84](http://epsg.io/4326) (SRID 4326). It is a round-earth SRS which means that the calculations are reasonably accurate, but computationally expensive.

Since the data the application uses is restricted to a relatively small area (the city limits of San Francisco), we can use a planar SRS specifically for that region. During deployment the application has already installed the SRS [NAD83(2011) / San Francisco CS13](http://epsg.io/7131) with SRID 7131 into the database. This SRS is a planar SRS for the San Francisco area which should give reasonable accuracy while being computationally much cheaper.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Enable the SRS transformation)]
Open the file `DatabaseInitializer.java` from the directory `src/main/java/com/sap/hana/hibernate/sample/app`.

Find the method `transformIncidentLocations` and change

```
// @EventListener
```

to

```
@EventListener
```

to make the method a Spring event listener.

```java
  /**
   * Trigger the transformation of incident locations if necessary
   *
   * @param event The context event
   */
  @EventListener
  @Transactional
  @Order(10)
  public void transformIncidentLocations(ContextRefreshedEvent event) {
  ...
  }
```

During application startup the method will now be called and transform the locations from the current SRS 4326 to the SRS 7131.

Save the `DatabaseInitializer.java` file.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Change the SRS used for the incident list)]
Now that the locations are available in an optimized representation, you'll need to update the application to make use of the new data.

Open the file `IncidentRepository.java` from the directory `src/main/java/com/sap/hana/hibernate/sample/repositories`.

In all queries, replace the column `i.location` (the location in SRS 4326) with `i.mapLocation` (the location in SRS 7131).

In addition, the input location which is still represented in SRS 4326 must be converted to SRS 7131 before doing the geospatial computations. For this, Hibernate Spatial provides a `transform` function which can be used to transform a geometry from one SRS to another. To make use of the function, replace all occurrences of `:location` with `transform(:location, 7131)`.

**Before**
```java
query = this.em.createQuery(
    "select i from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and dwithin(i.location, :location, :distance) = true "
        + "order by i.date desc",
    Incident.class );
```

**After**
```java
query = this.em.createQuery(
    "select i from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true "
        + "order by i.date desc",
    Incident.class );
```

Do these adjustments for all 4 queries in the file.

```java
public Page<Incident> findByLocationNear(Point<G2D> location, Distance distance, Date dateFrom, Date dateTo,
    List<String> category, Pageable pageable) {
  TypedQuery<Incident> query;
  if ( category == null || category.isEmpty() ) {
    query = this.em.createQuery(
        "select i from Incident i "
            + "where i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true "
            + "order by i.date desc",
        Incident.class );
  }
  else {
    query = this.em.createQuery(
        "select i from Incident i "
            + "where i.category in :category "
            + "  and i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true "
            + "order by i.date desc",
        Incident.class );
    query.setParameter( "category", category );
  }

  query.setParameter( "dateFrom", dateFrom );
  query.setParameter( "dateTo", dateTo );
  query.setParameter( "location", location );
  query.setParameter( "distance", distance );

  query.setFirstResult( (int) pageable.getOffset() );
  query.setMaxResults( pageable.getPageSize() );

  Query countQuery;
  if ( category == null || category.isEmpty() ) {
    countQuery = this.em.createQuery(
        "select count(i) from Incident i "
            + "where i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true " );

  }
  else {
    countQuery = this.em.createQuery(
        "select count(i) from Incident i "
            + "where i.category in :category "
            + "  and i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true " );
    countQuery.setParameter( "category", category );
  }

  countQuery.setParameter( "dateFrom", dateFrom );
  countQuery.setParameter( "dateTo", dateTo );
  countQuery.setParameter( "location", location );
  countQuery.setParameter( "distance", distance );

  long count = ( (Long) countQuery.getSingleResult() ).longValue();

  return new PageImpl<>( query.getResultList(), pageable, count );
}
```

Save the `IncidentRepository.java` file.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Change the SRS used for the incident heat map)]
Open the file `IncidentLocationAndCountRepository.java` from the directory `src/main/java/com/sap/hana/hibernate/sample/repositories`.

Like in the previous step, replace `i.location` with `i.mapLocation` and `:location` with `transform(:location, 7131)` in all queries.

In addition, we need to convert the location back to SRS 4326 before storing it in the `com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount` data transfer object (DTO) so it can be displayed in the UI. To do this, replace

```
select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(i.mapLocation, count(*))
```

with

```
select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(transform(i.mapLocation, 4326), count(*))
```

**Before**
```java
query = this.em.createQuery(
    "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(i.location, count(*)) "
        + "from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and dwithin(i.location, :location, :distance) = true "
        + "group by i.location",
    IncidentLocationAndCount.class );
```

**After**
```java
query = this.em.createQuery(
    "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(transform(i.mapLocation, 4326), count(*)) "
        + "from Incident i "
        + "where i.category in :category "
        + "  and i.date between :dateFrom and :dateTo "
        + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true "
        + "group by i.mapLocation",
    IncidentLocationAndCount.class );
```

Do these adjustments for both queries in the file.

```java
public List<IncidentLocationAndCount> findByLocationAndCategory(Point<G2D> location, Distance distance,
    Date dateFrom, Date dateTo, List<String> category) {
  TypedQuery<IncidentLocationAndCount> query;
  if ( category == null || category.isEmpty() ) {
    query = this.em.createQuery(
        "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(transform(i.mapLocation, 4326), count(*)) "
            + "from Incident i "
            + "where i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true "
            + "group by i.mapLocation",
        IncidentLocationAndCount.class );
  }
  else {
    query = this.em.createQuery(
        "select new com.sap.hana.hibernate.sample.entities.IncidentLocationAndCount(transform(i.mapLocation, 4326), count(*)) "
            + "from Incident i "
            + "where i.category in :category "
            + "  and i.date between :dateFrom and :dateTo "
            + "  and dwithin(i.mapLocation, transform(:location, 7131), :distance) = true "
            + "group by i.mapLocation",
        IncidentLocationAndCount.class );
    query.setParameter( "category", category );
  }

  query.setParameter( "dateFrom", dateFrom );
  query.setParameter( "dateTo", dateTo );
  query.setParameter( "location", location );
  query.setParameter( "distance", distance );

  return query.getResultList();
}
```

Save the `IncidentLocationAndCountRepository.java` file.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Deploy the application)]
With the changes in place we can now deploy the application again to the cloud.

In a console run the following command from the root directory of the project

```
mvn clean appengine:update
```

The application will be deployed to the Google App Engine.

After the deployment has succeeded you can navigate to `https://<your project ID>.appspot.com` to see the changes.

The retrieval of the incidents should be much faster now.

In the text area below enter the name of the Hibernate Spatial function that is used to compute the set of locations within a given distance of a given location.

[VALIDATE_2]

[ACCORDION-END]
