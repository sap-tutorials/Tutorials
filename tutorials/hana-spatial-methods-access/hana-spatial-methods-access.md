---
title: Access methods
description: Spatial access methods help you to retrieve properties of geometries
primary_tag: products>sap-hana
tags: [  tutorial>beginner, topic>big-data, topic>sql, products>sap-hana, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Introduction to SAP HANA Spatial data types](https://developers.sap.com/group.hana-aa-spatial-get-started.html)

## Next Steps
 - [Computation methods](https://developers.sap.com/tutorials/hana-spatial-methods-compute.html)

## Details
### You will learn  
You will learn about a number of access methods and how to apply them to different geometries.

>You must have table `"TESTSGEO"."SPATIALSHAPES"` from previous tutorials already created and loaded in your system to be able to run examples from this tutorial.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Access methods)]
If you have done previous tutorials, then you must have seen some of the spatial access methods already.

E.g. `ST_Dimension()` returned the dimension of a geometry object and could be applied to a geometry of any type - be it a point or a polygon. `ST_X()` returns the X coordinate of an `ST_POINT` and works with points only.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access methods for all geometries)]
Some other methods that can be applied to all types of geometries are presented below.

`ST_XMin()`, `ST_XMax()`, `ST_YMin()`, `ST_YMax` all retrieve corresponding minimum or maximum values of geometry's coordinates `X` and `Y`. There are similar methods available for coordinates `Z` and `M`.

In case of a point both minimum and maximum values will be the same and equal to the same value as point-only methods retrieving coordinate value, like `ST_X()`.

```sql
select
"SHAPE".ST_asWKT(),
"SHAPE".ST_X(),
"SHAPE".ST_XMin(),
"SHAPE".ST_XMax()
from "TESTSGEO"."SPATIALSHAPES"
where "SHAPE".ST_GeometryType()='ST_Point';
```

![access methods](access10.png)

After running the SQL statement above you should notice is that these methods return `null` when applied to an empty geometry.

The other access method you used here was `ST_GeometryType()` to check and filter the type of geometry.
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access methods for line strings)]
You used `ST_GeometryType()` in the previous step as a predicate to select only records for points. Otherwise trying to apply `ST_X()` to a string or polygon would return run-time error from SQL.

There are some access methods that can be used only with line strings. For example:
 - `ST_StartPoint()` and `ST_EndPoint()` to retrieve an `ST_Point` value of the starting and the ending points,
 - `ST_NumPoints()` to get the total number of points that can be used then to retrieve Nth point of using `ST_PointN()`. Numbering starts from `1` and `ST_PointN()` returns `null` in Nth point does not exist.

```sql
select
"SHAPE".ST_asWKT(),
"SHAPE".ST_StartPoint().st_asWKT(),
"SHAPE".ST_EndPoint().st_asWKT(),
"SHAPE".ST_NumPoints(),
"SHAPE".ST_PointN(2).st_asWKT()
from "TESTSGEO"."SPATIALSHAPES"
where "SHAPE".ST_GeometryType()='ST_LineString';
```

![Line strings](access20.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Access methods for collections)]
Similarly to points in strings you can get a number of individual geometries in collections (types `ST_MultiPoint`, `ST_MultiLineString`, `ST_MultiPolygon`) using `ST_NumGeometries()`. And then access each of them using `ST_GeometryN()` method.

```sql
select ST_UnionAggr("SHAPE").ST_GeometryType(), ST_UnionAggr("SHAPE").ST_NumGeometries()
from "TESTSGEO"."SPATIALSHAPES";
```

![Number of geometries](access30.png)

The query above aggregates all different geometries from the `SPATIALSHAPES` table. The result is `ST_GeometryCollection` with 5 different geometries in it. Some of overlapping polygons got aggregated into new single polygons, so the total number of geometries in the collection is smaller than a number of single geometries in the table.

```sql
select "SHAPE".ST_GeometryType(), ST_UnionAggr("SHAPE").ST_GeometryType(),
ST_UnionAggr("SHAPE").ST_NumGeometries(), ST_UnionAggr("SHAPE").ST_GeometryN(1).ST_asWKT()
from "TESTSGEO"."SPATIALSHAPES"
group by "SHAPE".ST_GeometryType();
```

![Geometry 1](access40.png)

This query now produced 3 different collections. Each of them contains only the same geometry types thanks to `GROUP BY` statement.

You will learn more about different kinds of spatial aggregations in a separate tutorial.

[ACCORDION-END]

### Optional
- You can find all available methods in [SAP HANA Spatial Reference](https://help.sap.com/viewer/cbbbfc20871e4559abfd45a78ad58c02/latest/en-US/7a13f280787c10148dc893063dfed1c4.html). Make sure you review documentation for the version of SAP HANA you run.

