---
title: Transformation methods
description: Transformation methods transform a geometry into some other geometry
auto_validation: true
time: 15
primary_tag: products>sap-hana
tags: [  tutorial>beginner, topic>big-data, topic>sql, products>sap-hana, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorial:** [Computation methods](https://developers.sap.com/tutorials/hana-spatial-methods-compute.html)

## Next Steps
 - Spatial predicates (coming soon)

## Details
### You will learn  
You will learn about a number of methods transforming one geometries into another.

---

[ACCORDION-BEGIN [Step 1: ](Boundary)]
`ST_Boundary` method returns the boundary of the geometry value. Boundary depends on the geometry type and its characteristics.

A point has no boundary and returns the empty geometry. An empty geometry returns `null`.

```sql
select
"SHAPEID", "SHAPE".ST_asWKT(),
"SHAPE".ST_Boundary().ST_asWKT()
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPE".st_GeometryType() = 'ST_Point'
 order by 1 asc;
```

![Points boundaries](trans10.png)

The boundary for line strings is a collection of their end points.

```sql
select
"SHAPEID", "SHAPE".ST_asWKT(),
"SHAPE".ST_Boundary().ST_asWKT()
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPE".st_GeometryType() = 'ST_LineString'
 order by 1 asc;
```

![Lines boundaries](trans20.png)

The boundary for multi line strings is a collection of all their end points of individual line strings in the collection.

```sql
select ST_UnionAggr("SHAPE").ST_asWKT(),
ST_UnionAggr("SHAPE").ST_Boundary().ST_asWKT()
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPE".st_GeometryType() = 'ST_LineString';
```

![Multi lines boundaries](trans30.png)

A ring - a special case of a string where the start point is the same as the end point and there are no self-intersections - has no boundary.

```sql
select new ST_LineString('LINESTRING (6 7,10 3,10 10,6 7)')
.ST_Boundary().ST_asWKT() from dummy;
```

![Ring boundary](trans40.png)

The boundary for a polygon is its outer ring and any inner rings.

```sql
select
"SHAPEID", "SHAPE".ST_asWKT(),
"SHAPE".ST_Boundary().ST_asWKT()
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPE".st_GeometryType() = 'ST_Polygon'
 order by 1 asc;
```

![Polygon boundary](trans50.png)

And the example for a polygon with the inner ring to illustrate the output containing both the outer and inner rings as a `MULTILINESTRING`.

```sql
select
NEW ST_Polygon('Polygon ((-5 -5, 5 -5, 0 5, -5 -5), (-2 -2, -2 0, 2 0, 2 -2, -2 -2))').ST_Boundary().ST_asWKT()
from dummy;
```

![Polygon with inner ring boundary](trans60.png)

Obviously the length of a boundary is equal to the perimeter of a polygon.

```sql
select
"SHAPEID", "SHAPE".ST_asWKT(),
"SHAPE".ST_Boundary().ST_Length(),
"SHAPE".ST_Perimeter()
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPE".st_GeometryType() = 'ST_Polygon'
 order by 1 asc;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Envelope)]

`ST_Envelope()` method returns the bounding rectangle for the geometry value. This method cannot be used with geometries in a round-Earth spatial reference system.

```sql
select
"SHAPEID", "SHAPE".ST_asWKT(),
"SHAPE".ST_Envelope().ST_asWKT()
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPEID" in (3, 7, 12)
 order by 1 asc;
```

![Boundaries for different geometry types](trans70.png)

Below are visualizations of boundaries for two geometry types: string (for the shape with `id = 7` from `"TESTSGEO"."SPATIALSHAPES"`) and polygon (`id = 11`).

```sql
select ST_asSVGAggr("SHAPE"), ST_asSVGAggr("ENVELOPE") from
(
select
"SHAPE",
"SHAPE".ST_Envelope() as "ENVELOPE"
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPEID" in (7, 11)
);
```

Combine returned SVG outputs into one and modify drawing parameters to display initial geometries in red, and envelopes with thicker lines (`stroke-width='0.2%'`) to get SVG like below.

```XML
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="3.9 -10.1 6.2 7.2">
<path fill="none"  stroke='red' stroke-width='0.1%' d="M 4,-4 l 2,-1 1,1 "/>
<path fill='red' stroke='red' stroke-width='0.1%' d="M 6,-7 l 4,4 0,-7 Z"/>

<path fill='none' stroke='black' stroke-width='0.2%' d="M 4,-4 l 3,0 0,-1 -3,0 Z"/>
<path fill='none' stroke='black' stroke-width='0.2%' d="M 6,-3 l 4,0 0,-7 -4,0 Z"/>
</svg>
```

![Boundaries visualizations](trans80.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Convex hull)]

`ST_ConvexHull` method returns the convex hull of the geometry value.

```sql
select "SHAPE".ST_GeometryType(),
ST_asSVGAggr("SHAPE"), ST_UnionAggr("SHAPE").ST_ConvexHull().ST_asSVG()
from "TESTSGEO"."SPATIALSHAPES"
group by "SHAPE".ST_GeometryType();
```

Below is slightly modified SVG to better display original geometries (in red) and the convex hull (in blue).

For points:
```xml
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="2.4 -6.1 1.6 3.2">
<rect width="0.5%" height="0.5%" fill='grey' stroke='red' stroke-width='0.1%' x="2.5" y="-3"/>
<rect width="0.5%" height="0.5%" fill='grey' stroke='red' stroke-width='0.1%' x="3" y="-4.5"/>
<rect width="0.5%" height="0.5%" fill='grey' stroke='red' stroke-width='0.1%' x="3" y="-6"/>
<rect width="0.5%" height="0.5%" fill='grey' stroke='red' stroke-width='0.1%' x="4" y="-6"/>

<path fill="none" stroke="blue" stroke-width="0.3%" d="M 2.5,-3 l 1.5,-3 -1,0 Z"/>
</svg>
```

![Convex hull for points](trans90.png)

For lines:
```xml
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="2.9 -7.1 6.2 4.2">
<path fill="none"  stroke='red' stroke-width='0.5%' d="M 3,-3 l 2,-1 1,1 "/>
<path fill="none"  stroke='red' stroke-width='0.5%' d="M 4,-4 l 2,-1 1,1 "/>
<path fill="none"  stroke='red' stroke-width='0.5%' d="M 7,-5 l 2,-2 "/>
<path fill="none"  stroke='red' stroke-width='0.5%' d="M 7,-3 l 1,-2 "/>

<path fill="none" stroke="blue" stroke-width="0.1%" d="M 7,-3 l 2,-4 -5,3 -1,1 Z"/>
</svg>
```

![Convex hull for lines](trans100.png)

For polygons:
```xml
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox=".9 -10.1 9.1 9.2">
<path fill='grey' stroke='red' stroke-width='0.5%' opacity="0.75" d="M 6,-7 l 4,4 0,-7 Z"/>
<path fill='grey' stroke='red' stroke-width='0.5%' opacity="0.75" d="M 4,-5 l 1,2 1,-2 Z"/>
<path fill='grey' stroke='red' stroke-width='0.5%' opacity="0.75" d="M 1,-1 l 5,0 0,-5 -5,0 Z"/>
<path fill='grey' stroke='red' stroke-width='0.5%' opacity="0.75" d="M 1,-3 l 4,0 0,-1 -4,0 Z"/>

<path fill="none" stroke="blue" stroke-width="0.1%" d="M 6,-1 l 4,-2 0,-7 -9,4 0,5 Z"/>
</svg>
```

![Convex hull for polygons](trans110.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Buffer)]
`ST_Buffer` method returns the geometry that represents all points whose distance from any point of an input geometry is less than or equal to a specified distance.

```sql
select
"SHAPEID", "SHAPE".ST_asWKT(),
"SHAPE".ST_Buffer(0.5).ST_asWKT()
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPEID" in (3, 7, 11)
 order by 1 asc;
```

![Buffer WKT for different geometry types](trans120.png)

Visualize using the following SQL...

```sql
select ST_asSVGAggr("SHAPE"), ST_asSVGAggr("BUFFER") from
(
select
"SHAPE",
"SHAPE".ST_Buffer(0.5) as "BUFFER"
 from "TESTSGEO"."SPATIALSHAPES"
 where "SHAPEID" in (3, 7, 11)
);
```

...and modify SVG output for better visualization.

```xml
<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="2.4 -10.6 8.1 8.2">
<rect width="0.1%" height="0.1%" fill='grey' stroke='red' stroke-width='0.1%' x="3" y="-6"/>
<path fill="none"  stroke='red' stroke-width='0.1%' d="M 4,-4 l 2,-1 1,1 "/>
<path fill='red' stroke='red' stroke-width='0.1%' d="M 6,-7 l 4,4 0,-7 Z"/>

<path fill='grey' stroke='blue' stroke-width='0.1%' opacity="0.25" d="M 3.5,-6 l -.00599,-.07717 -.01783,-.07532 -.02924,-.07167 -.03995,-.06629 -.04971,-.05933 -.05826,-.05095 -.06543,-.04135 -.07103,-.03076 -.07493,-.01942 -.07702,-.00763 -.01061,-.00011 -.07717,.00599 -.07532,.01783 -.07167,.02924 -.06629,.03995 -.05933,.04971 -.05095,.05826 -.04135,.06543 -.03076,.07103 -.01942,.07493 -.00763,.07702 -.00011,.01061 .00599,.07717 .01783,.07532 .02924,.07167 .03995,.06629 .04971,.05933 .05826,.05095 .06543,.04135 .07103,.03076 .07493,.01942 .07702,.00763 .01061,.00011 .07717,-.00599 .07532,-.01783 .07167,-.02924 .06629,-.03995 .05933,-.04971 .05095,-.05826 .04135,-.06543 .03076,-.07103 .01942,-.07493 .00763,-.07702 Z"/>
<path fill='grey' stroke='blue' stroke-width='0.1%' opacity="0.25" d="M 5.77639,-5.44721 l -2,1 -.06634,.03987 -.0594,.04963 -.05102,.0582 -.04143,.06538 -.03084,.07099 -.01951,.07491 -.00772,.07701 -.00013,.01122 .00599,.07717 .01783,.07532 .02897,.07112 .03987,.06634 .04963,.0594 .0582,.05102 .06538,.04143 .07099,.03084 .07491,.01951 .07701,.00772 .01122,.00013 .07717,-.00599 .07532,-.01783 .07112,-.02897 1.67766,-.83883 .74518,.74517 .0588,.05033 .06587,.04066 .07135,.03 .07512,.01862 .07711,.00681 .0053,.00003 .07717,-.00599 .07532,-.01783 .07167,-.02924 .06629,-.03995 .05933,-.04971 .05096,-.05826 .04134,-.06543 .03076,-.07103 .01942,-.07493 .00763,-.07702 .00011,-.01061 -.00599,-.07717 -.01783,-.07532 -.02924,-.07167 -.03995,-.06629 -.04971,-.05933 -1.00373,-1.00377 -.0588,-.05033 -.06587,-.04066 -.07135,-.03 -.07512,-.01862 -.0771,-.00681 -.00531,-.00003 -.07717,.00599 -.07532,.01783 Z"/>
<path fill='grey' stroke='blue' stroke-width='0.1%' opacity="0.25" d="M 9.7,-10.4 l -4,3 -.05814,.0511 -.04956,.05945 -.03979,.0664 -.02906,.07173 -.01765,.07537 -.0058,.07595 .00599,.07717 .01783,.07532 .02924,.07167 .03995,.06629 .04971,.05934 4.00373,4.00376 .0588,.05033 .06587,.04066 .07135,.02999 .07512,.01863 .07711,.00681 .0053,.00003 .07717,-.00599 .07532,-.01783 .07167,-.02924 .06629,-.03995 .05933,-.04971 .05095,-.05826 .04135,-.06543 .03076,-.07103 .01942,-.07493 .00763,-.07702 .00011,-.01061 0,-7 -.00599,-.07717 -.01783,-.07532 -.02924,-.07167 -.03995,-.06629 -.04971,-.05933 -.05826,-.05095 -.06543,-.04135 -.07103,-.03076 -.07493,-.01942 -.07702,-.00763 -.01061,-.00011 -.07717,.00599 -.07532,.01783 -.07166,.02924 -.0663,.03995 Z"/>
</svg>
```

![Buffer SVG for different geometry types](trans130.png)

[VALIDATE_1]
[ACCORDION-END]

### Optional
- You can find all available methods in [SAP HANA Spatial Reference](https://help.sap.com/viewer/cbbbfc20871e4559abfd45a78ad58c02/latest/en-US/7a13f280787c10148dc893063dfed1c4.html). Make sure you review documentation for the version of SAP HANA you run.

---
