---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud--sap-hana-database, software-product>sap-hana, software-product>sap-hana--express-edition, software-product-function>sap-hana-multi-model-processing, software-product-function>sap-hana-spatial, software-product-function>sap-hana-graph, tutorial>license]
primary_tag: software-product>sap-hana-cloud
---

# Try Out Multi-Model Functionality with the SAP HANA Database Explorer and Database Objects App
<!-- description --> Explore knowledge graph, property graph, JSON document store, and spatial capabilities in the SAP HANA database explorer.

## Prerequisites
- A productive SAP HANA Cloud database
- You have completed the first 3 tutorials in this group.

## You will learn
  - How to create a knowledge graph, a property graph, a document store, and import spatial data.
  - How the SAP HANA database explorer and the database objects app can be used with multi-model data.

## Intro
A [knowledge graph](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-knowledge-graph-guide/sap-hana-cloud-sap-hana-database-knowledge-graph-engine-guide) can be used to store facts in triples providing additional meaning and relationships. 

A [property graph](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-property-graph-engine-reference/sap-hana-cloud-sap-hana-database-property-graph-engine-reference) can be used to show the connections between items such as the connections between airports or between people or groups in a social network.

SAP HANA Cloud provides the ability to store and perform queries on spatial data such as a point, a line segment, or a polygon.  Additional details can be found at [SAP HANA Cloud, SAP HANA Database Spatial Reference](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/sap-hana-cloud-sap-hana-database-spatial-reference).

This tutorial is meant to be an introduction to this topic.  For additional content see the tutorial groups [Introduction to SAP HANA Spatial Data Types](group.hana-aa-spatial-get-started),  [Smart Multi-Model Data Processing with SAP HANA Cloud](group.hana-cloud-smart-multi-model-data), and the multi-model chapters in [Basic Trial - Introduction to SAP HANA Cloud](https://www.sap.com/products/technology-platform/hana/trial.html) that is available once you sign up for the basic trial.

---


### Enable the triple store and Create a knowledge graph
The following steps will create a knowledge graph that provides information on additional hotel amenities, explore the created knowledge graph using the database objects app, and then will perform a query on the knowledge graph.

Before you can create a knowledge graph, please ensure your HANA Instance is version 2025.2 or above, and your instance has triple store activated. Here are the steps to doing this:

1. Go to Manage Configuration on your SAP HANA Cloud Database.

    ![Manage Configuration](manage_config.png)

2. Under the General Tab, change the version of your instance to 2025.2 or above

    ![Change Instance Version](change_version.png)

3. Under the Advanced Settings Tab, check off the Triple Store option if it's not already on.

    ![Add Triple Store](add_triple_store.png)

    *The knowledge graph feature is not available for trial or free tier users.*
    
    To learn more about knowledge graphs see [Connecting the Facts: SAP HANA Cloud’s Knowledge Graph Engine for Business Context](https://community.sap.com/t5/technology-blogs-by-sap/connecting-the-facts-sap-hana-cloud-s-knowledge-graph-engine-for-business/ba-p/13888597) and [Choosing Between Knowledge Graphs and Property Graphs in SAP HANA Cloud and Why Both Matter](https://community.sap.com/t5/technology-blogs-by-sap/choosing-between-knowledge-graphs-and-property-graphs-in-sap-hana-cloud-and/ba-p/14074575).


4. Execute the following in the SQL Console. This query creates a KG with several hotels and amenities such as an indoor pool, hot tub, fitness center, etc.

    ```SQL
    --CALL SPARQL_EXECUTE('DROP GRAPH <kg_hotels>', '', ?, ?);
    CALL SPARQL_EXECUTE('
        INSERT DATA { GRAPH <kg_hotels> { 
            <http://example.org/hotels/Delta> a <Hotel>;
            <name> "Delta";
            <city> "Waterloo";
            <yearBuilt> 2012 .
        
            <http://example.org/hotels/hotelamenities/IndoorPool1> a <IndoorPool>;
            <included_at> <http://example.org/hotels/Delta>;
            <length> 25;
            <width> 10.
        
            <http://example.org/hotels/hotelamenities/Hottub1> a <Hottub>;
            <included_at> <http://example.org/hotels/Delta>;
            <length> 6;
            <width> 6.
        
            <http://example.org/hotels/hotelamenities/FitnessCenter1> a <FitnessCenter>;
            <included_at> <http://example.org/hotels/Delta>.
        
            <http://example.org/hotels/hotelamenities/Keurig> a <CoffeeMaker>;
            <included_at> <http://example.org/hotels/Delta>. 

            <http://example.org/hotels/Sunshine> a <Hotel>;
            <name> "Sunshine";
            <city> "Clearwater";
            <yearBuilt> 1975 .
        
            <http://example.org/hotels/hotelamenities/Restaurant1> a <Restaurant>;
            <included_at> <http://example.org/hotels/Sunshine>;
            <seating> 95.
        
            <http://example.org/hotels/hotelamenities/FitnessCenter2> a <FitnessCenter>;
            <included_at> <http://example.org/hotels/Sunshine>.
        
            <http://example.org/hotels/hotelamenities/Laundry1> a <DryCleaning>;
            <included_at> <http://example.org/hotels/Sunshine>. 
            
            <http://example.org/hotels/Congress> a <Hotel>;
            <name> "Congress";
            <city> "Seattle";
            <yearBuilt> 2012 .
        
            <http://example.org/hotels/hotelamenities/IndoorPool2> a <IndoorPool>;
            <included_at> <http://example.org/hotels/Congress>;
            <length> 30;
            <width> 15.
        
            <http://example.org/hotels/hotelamenities/Restaurant2> a <Restaurant>;
            <included_at> <http://example.org/hotels/Congress>;
            <seating> 122.
        
            <http://example.org/hotels/hotelamenities/FitnessCenter3> a <FitnessCenter>;
            <included_at> <http://example.org/hotels/Congress>.
        
            <http://example.org/hotels/hotelamenities/Spa1> a <Spa>;
            <included_at> <http://example.org/hotels/Congress>. 

            <http://example.org/hotels/OceanStar> a <Hotel>;
            <name> "Ocean Star";
            <city> "Atlantic City";
            <yearBuilt> 2012 .
        
            <http://example.org/hotels/hotelamenities/IndoorPool3> a <IndoorPool>;
            <included_at> <http://example.org/hotels/OceanStar>;
            <length> 50;
            <width> 30.
        
            <http://example.org/hotels/hotelamenities/Restaurant2> a <Restaurant>;
            <included_at> <http://example.org/hotels/OceanStar>;
            <seating> 205.
        
            <http://example.org/hotels/hotelamenities/Cleaning1> a <CleaningServices>;
            <included_at> <http://example.org/hotels/OceanStar>.
        
            <http://example.org/hotels/hotelamenities/Spa2> a <Spa>;
            <included_at> <http://example.org/hotels/OceanStar>. 

            <http://example.org/hotels/LongIsland> a <Hotel>;
            <name> "LongIsland";
            <city> "Long Island";
            <yearBuilt> 2021 .
        
            <http://example.org/hotels/hotelamenities/Entertainment1> a <Entertainment>;
            <included_at> <http://example.org/hotels/LongIsland>.
        
            <http://example.org/hotels/hotelamenities/Restaurant3> a <Restaurant>;
            <included_at> <http://example.org/hotels/LongIsland>;
            <seating> 122.
        
            <http://example.org/hotels/hotelamenities/Cleaning2> a <CleaningServices>;
            <included_at> <http://example.org/hotels/LongIsland>.
        
            <http://example.org/hotels/hotelamenities/Spa3> a <Spa>;
            <included_at> <http://example.org/hotels/LongIsland>. 
            
            <http://example.org/hotels/hotelamenities/Hottub2> a <Hottub>;
            <included_at> <http://example.org/hotels/LongIsland>. 
        } 
        }
        ', '', ?, ?
    );
    ```
    
    Additional examples can be found at [SAP HANA Cloud, SAP HANA Database SPARQL Reference Guide](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sparql-reference-guide/sap-hana-cloud-sap-hana-database-sparql-reference-guide)


5. To visualize and examine the knowledge graph database objects, you must first enable the feature in your preferences.  Click on your account

    ![Account Settings](go_preferences_RDF.png)

    Go to the Database Objects tab

    ![Database Objects Tab](go_database_objects.png)

    Enable RDF Named Graphs
    
    ![Enable RDF Graphs](enable_RDF_graphs.png)

    You will now have the RDF Named Graphs view available. Click on it to view the graphs you have created
        
    ![RDF Named Graphs View](RDF_view.png)

    The DEFAULT graph is where content goes when it doesn't have a name. For more information, review the [DEFAULT_GRAPHS and Named Graphs help page](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-knowledge-graph-guide/default-graph-and-named-graphs).

    Click on the kg_hotels graph, and open the graph ontology to see the nodes.
    
    ![Graph Ontology View](view_graph.png)


6. Run a query to find hotels that have an indoor pool.

    ```SQL
    SELECT *
    FROM SPARQL_TABLE('
    PREFIX ex: <http://example.org/hotels/>
    SELECT ?hotel ?hotelName ?city ?yearBuilt ?poolLength ?poolWidth
    FROM <kg_hotels>
    WHERE {
        ?hotel a <Hotel> ;
    <name> ?hotelName ;
    <city> ?city ;
    <yearBuilt> ?yearBuilt .
        ?pool a <IndoorPool> ;
    <included_at> ?hotel ;
    <length> ?poolLength ;
    <width> ?poolWidth .
    }
    ');
    ```

    Further examples of creating SPARQL statements can be found at [SPARQL SELECT Queries Using SPARQL_TABLE](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-knowledge-graph-guide/sparql-select-queries-using-sparql-table).


### Create a property graph workspace
The following steps will create a property graph workspace that can display the distance between hotels in a state.

In SAP HANA Cloud, a property graph is made up of a set of vertices and a set of edges. Vertices are stored in vertex tables, while edges are stored in edge tables. Vertex and edge tables are collectively denoted as graph tables.

1. Create a vertex table that represents distances between hotels by executing the following in the SQL console.

    ```SQL
    CREATE COLUMN TABLE DISTANCES(
      DKEY INTEGER UNIQUE NOT NULL,
      HSOURCE INTEGER NOT NULL
        REFERENCES HOTEL(HNO),
      HTARGET INTEGER NOT NULL
        REFERENCES HOTEL(HNO),
      DIST_KM DOUBLE
    );
    ```

2. Populate the vertex table with distances between hotels that are in the same state.

    ```SQL
    --Washington
    INSERT INTO DISTANCES VALUES (1, 10, 11, 11.8);
    INSERT INTO DISTANCES VALUES (2, 11, 10, 11.8);

    --New York
    INSERT INTO DISTANCES VALUES (3, 12, 13, 217.3);
    INSERT INTO DISTANCES VALUES (4, 13, 12, 217.3);
    INSERT INTO DISTANCES VALUES (5, 12, 14, 71.9);
    INSERT INTO DISTANCES VALUES (6, 14, 12, 71.9);
    INSERT INTO DISTANCES VALUES (7, 12, 15, 71.5);
    INSERT INTO DISTANCES VALUES (8, 15, 12, 71.5);
    INSERT INTO DISTANCES VALUES (9, 13, 14, 212.2);
    INSERT INTO DISTANCES VALUES (10, 14, 13, 212.2);
    INSERT INTO DISTANCES VALUES (11, 13, 15, 212.1);
    INSERT INTO DISTANCES VALUES (12, 15, 13, 212.1);
    INSERT INTO DISTANCES VALUES (13, 14, 15, 0.4);
    INSERT INTO DISTANCES VALUES (14, 15, 14, 0.4);

    --Illinois
    INSERT INTO DISTANCES VALUES (15, 16, 17, 23.5);
    INSERT INTO DISTANCES VALUES (16, 17, 16, 23.5);

    --Florida
    INSERT INTO DISTANCES VALUES (17, 18, 19, 219.8);
    INSERT INTO DISTANCES VALUES (18, 19, 18, 219.8);
    INSERT INTO DISTANCES VALUES (19, 18, 20, 323.4);
    INSERT INTO DISTANCES VALUES (20, 20, 18, 323.4);
    INSERT INTO DISTANCES VALUES (21, 19, 20, 333.8);
    INSERT INTO DISTANCES VALUES (22, 20, 19, 333.8);

    --California
    INSERT INTO DISTANCES VALUES (23, 21, 22, 149.5);
    INSERT INTO DISTANCES VALUES (24, 22, 21, 149.5);
    INSERT INTO DISTANCES VALUES (25, 21, 23, 35.7);
    INSERT INTO DISTANCES VALUES (26, 23, 21, 35.7);
    INSERT INTO DISTANCES VALUES (27, 22, 23, 163.2);
    INSERT INTO DISTANCES VALUES (28, 23, 22, 163.2);
    ```

3. Create a graph workspace.

    ```SQL
    CREATE GRAPH WORKSPACE DISTANCEGRAPH
    EDGE TABLE DISTANCES
        SOURCE COLUMN HSOURCE
        TARGET COLUMN HTARGET
        KEY COLUMN DKEY
    VERTEX TABLE HOTEL
        KEY COLUMN HNO;
    ```

4. Navigate to graph workspaces, select the previously created graph workspace, and open it to view its properties.

    ![DISTANCEGRAPH properties](distance-graph-workspace.png)

For additional information, see [SAP HANA Cloud, SAP HANA Database Graph Reference](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-graph-reference/sap-hana-cloud-sap-hana-database-graph-reference).


### Explore a property graph using the viewer
1. Open the property graph viewer.

    ![DISTANCEGRAPH graph viewer](distance-graph-workspace-viewer.png)

    The property graph viewer will open in a new tab as shown below.

    ![DISTANCEGRAPH graph viewer](distance-graph-viewer.png)

2. Set the vertex and edge names using the property graph viewer settings.

    Set the vertex label to `NAME`.

    ![DISTANCEGRAPH vertex labels](vertex-label.png)

    Set the edge label to `DIST_KM`.

    ![DISTANCEGRAPH edge labels](edge-label.png)

3. Optionally, adjust a few of the property graph vertices to accommodate viewing by dragging and dropping property graph vertices.

    ![DISTANCEGRAPH better viewing](graph-viewer-wider.png)

4. Apply a filter to vertices where `STATE` is NY.  After specifying the filter, press the Add button and then apply it by pressing the Apply button.

    ![DISTANCEGRAPH vertex filter](vertex-filter.png)

     Apply a filter to edges where `DIST_KM` is less than 100.

    ![DISTANCEGRAPH edge filter](edge-filter.png)

5. Highlight the Long Island vertex using the property graph viewer settings. You may do so by selecting a color. 

    ![DISTANCEGRAPH highlighted Long Island](long-island.png)

Additional property graph examples include the [Greek Mythology Graph Example](https://help.sap.com/viewer/f381aa9c4b99457fb3c6b53a2fd29c02/2.0.04/en-US/071d7b7349f04e419507387c271dce8f.html) and [Open Flights](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-graph-reference/appendix-open-flights-and-company-graph-examples).  The company graph example does not currently display in the SAP HANA database explorer property graph viewer as it does not currently support the display of homogeneous graphs.  Property graph workspaces may also be viewed using the [SAP HANA plug-in for Cytoscape](https://github.com/SAP/sap-hana-plugin-for-cytoscape).


### Use property graph algorithms in the SAP HANA database explorer
The shortest path algorithm can be used to provide the optimal route between two vertices. The nearest neighbor algorithm can be used to show only the vertices that are connected to a specified vertex.

The following steps will walk through using the shortest path algorithm to determine the optimal route from Airport Hotel in Rosemont, IL to Regency Hotel in Seattle, WA.

1. Execute the following in SQL to add a few connections between hotels in different states.

    ```SQL
    --Midtown New York to Lake Michigan Chicago
    INSERT INTO DISTANCES VALUES (29,14,16,1227);
    INSERT INTO DISTANCES VALUES (30,16,14,1227);

    --Long Island New York to Lake Michigan Chicago
    INSERT INTO DISTANCES VALUES (31,12,16,1357);
    INSERT INTO DISTANCES VALUES (32,16,12,1357);

    --Long Island New York to Beach Florida
    INSERT INTO DISTANCES VALUES (33,12,19,1738);
    INSERT INTO DISTANCES VALUES (34,19,12,1738);

    --Congress Seattle to Star California
    INSERT INTO DISTANCES VALUES (35,10,23,1817);
    INSERT INTO DISTANCES VALUES (36,23,10,1817);

    --Indian Horse California to Beach Florida
    INSERT INTO DISTANCES VALUES (37,22,19,3861);
    INSERT INTO DISTANCES VALUES (38,19,22,3861);

    --Atlantic Florida to Long Beach California
    INSERT INTO DISTANCES VALUES (39,20,21,4348);
    INSERT INTO DISTANCES VALUES (40,21,20,4348);
    ```

2. After removing the previously applied filters, navigate to the property graph viewer, and select the algorithms tab. Update the Algorithm field to "Shortest Path", specify the values shown below, and click Apply.

    ![Graph Algorithms](graph-algorithms.png)


### Create, populate, and query a JSON collection (optional)
SAP HANA provides the ability to store and query JSON data.  This can be useful if the schema of the data is often changed or if you wish to join data in SQL queries that comes from both SQL tables and JSON data.

The following steps will demonstrate how to create a JSON collection that can be used to collect notes about customers staying at a hotel.

>The creation of a JSON collection is not supported in the SAP HANA Cloud free tier or trial.

1. Enable the JSON document store.  

    For an SAP HANA Cloud database, in the creation wizard, or for an existing instance, the Manage Configuration dialog, enable the document store.  

    ![enable the document store](hc-document-store.png)

    For an on-premise server, add the document service as described at [Enable the SAP HANA JSON Document Store](https://help.sap.com/docs/SAP_HANA_PLATFORM/3e48dd3ad36e41efbdf534a89fdf278f/28334f8e631c447598d2591305b28660.html).

2. Create a collection named `GUEST_NOTES`.

    ```SQL
    CREATE COLLECTION GUEST_NOTES;
    ```

3. Insert some data.

    ```SQL
    INSERT INTO GUEST_NOTES VALUES ('{"FIRST_NAME": "Jenny", "LAST_NAME": "Porter", "REQUEST": "requested a courtesy call at 7:00 am"}');
    INSERT INTO GUEST_NOTES VALUES ('{"FIRST_NAME": "Jenny", "LAST_NAME": "Porter", "REQUEST": "requested an extra blanket"}');
    INSERT INTO GUEST_NOTES VALUES ('{"title": "Mr.", "FIRST_NAME": "Peter", "LAST_NAME": "Brown", "REQUEST": "requested an earl grey at 2:00"}');
    ```

    Notice that the structure of the Guest Notes does not need to be defined in advance.

4. (Optional) You can use the [Support for JSON Schema](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-json-document-store-guide/support-for-json-schema) feature to define rules for your documents to adhere to. The example below restricts the "title" property to only accept strings.
   
    ```SQL
    CREATE COLLECTION "GUEST_NOTES_WITH_SCHEMA" JSON SCHEMA '{
        "$schema": "http://json-schema.org/draft-07/schema#",
            "properties": {
                "title": {
                    "type": "string"
                }
            }
        }';

    INSERT INTO "GUEST_NOTES_WITH_SCHEMA" VALUES ('{"title": 1, "FIRST_NAME": "Peter", "LAST_NAME": "Brown", "REQUEST": "requested an earl grey at 2:00"}'); -- throws error 
    ```

    ![Schema Validation Error](schema_validation_error.png)
    

5. The JSON data can be returned as a JSON document, in a tabular result, or can be joined with data from a table.

    ```SQL
    SELECT * FROM GUEST_NOTES;  --returns JSON

    SELECT FIRST_NAME, LAST_NAME, REQUEST FROM GUEST_NOTES; --returns tabular result

    WITH myJSON AS (SELECT GUEST_NOTES FROM GUEST_NOTES)
        SELECT '[' || STRING_AGG(TO_NVARCHAR(GUEST_NOTES), ',') || ']' FROM myJSON;  --returns all the results as one JSON document

    WITH GN_VIEW AS (SELECT FIRST_NAME, LAST_NAME, REQUEST FROM GUEST_NOTES) --joins a collection with a table
        SELECT DISTINCT GN_VIEW.REQUEST, C.FIRSTNAME, GN_VIEW.LAST_NAME, C.ADDRESS
        FROM GN_VIEW INNER JOIN CUSTOMER AS C ON GN_VIEW.LAST_NAME = C.NAME;
    ```

    ![collection queries](join-query.png)

    For additional details see the [SELECT Statement](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-json-document-store-guide/select-statement) in the JSON Document Store guide.

6. The properties of the JSON collection can also be viewed.  

    ![Collection properties](collection-properties.png)

    Click on the glasses icon to view the JSON text.

    ![JSON text](json-text.png)

Further examples can be found in the [The Small JSON Document Store Cookbook](https://community.sap.com/t5/technology-blogs-by-sap/the-small-json-document-store-cookbook/ba-p/13516348).

### Import a JSON collection using the hana_ml.docstore package (optional)
JSON collections can be imported using the import data wizard but are required to be in a specified format as mentioned at [Import and Export](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-json-document-store-guide/import-and-export).  An alternative method of performing an import is shown in this step.

1. Find a JSON collection to be imported.  The example below uses the Parks dataset from [City of Waterloo Open Data](https://opendata-city-of-waterloo.opendata.arcgis.com/) downloaded as a GeoJSON file. 

2. Create a Python application and update the host variable below. 

    ```Shell (Microsoft Windows)
    notepad loadJSON.py
    ```
    
    ```Python
    from hana_ml import docstore
    from hana_ml.dataframe import ConnectionContext

    import json

    host = 'xxxxx-ee5a-4782-bc7e-297099099b59.hana.prod-ca10.hanacloud.ondemand.com'
    port = 443
    user = 'USER1'
    passwd = 'Password1'

    try :
        conn = ConnectionContext(address=host, port=port, user=user, password=passwd)
    except ConnectionContext.Error as er:
        print('Connect failed, exiting')
        print(er)
        exit()

    #If no errors, print connected
    print('connected')

    with open("ParksInWaterloo.geojson") as json_file:
        data = json.load(json_file)

    docstore.create_collection_from_elements(conn, 
        collection_name="WATERLOO_PARKS", elements=[data], drop_exist_coll=True, schema="HOTELS")
    ```

3. Add the required packages.

    ```Shell
    pip install hana_ml
    pip install shapely
    ```

4. Run the application.

    ```Shell
    python loadJSON.py 
    ```

    ![load the collection](load-collection.png)

5. Query the imported collection.

    ```SQL
    SELECT CARDINALITY("features")  FROM "HOTELS"."WATERLOO_PARKS";
    SELECT "features" FROM "HOTELS"."WATERLOO_PARKS";
    SELECT "features" FROM "HOTELS"."WATERLOO_PARKS" UNNEST "features" as "f";
    SELECT "f"."properties"."NAME", "f"."properties"."TENNIS_COURT" FROM "HOTELS"."WATERLOO_PARKS" UNNEST "features" as "f" ORDER BY "f"."properties"."TENNIS_COURT" DESC;
    ```
    ![parks with tennis courts](parks-with-tennis-courts.png)

Additional details can be found at [hana_ml.docstore package](https://help.sap.com/doc/cd94b08fe2e041c2ba778374572ddba9/latest/en-US/hana_ml.docstore.html#hana_ml.docstore.create_collection_from_elements).


### Import and view spatial data
This step will import an [`ESRI shapefile`](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/support-for-esri-shapefiles) or optionally a `GeoJSON` file containing points of interest in the city of Waterloo Ontario.  The `ESRI shapefile` import will result in a table while the JSON import will result in a JSON Collection.  In the following step, a search will be performed to return the closest points of interest to the Delta hotel located in Waterloo.

1. At the [ARCGIS Hub](https://hub.arcgis.com/search), search for **`Points of Interest Waterloo`**.  Scroll through the results and choose the selection below.

    ![Search](search.png)

2. Choose to download the data as a `shapefile`.  The final sub-steps provide some details on how a `GeoJSON` file can be imported.

    ![download shapefile](download-shapefile.png)


3. To import a `shapefile`, start the import data wizard.

    ![Open import data wizard](open-import-data-wizard.png)


    Choose **Import ESRI Shapefiles** and select the `Points_of_Interest.zip` file.

    ![Import ESRI Shapefile](importESRI.png)

    >If importing an `ESRI Shapefile` from a cloud storage provider, the file must be unzipped, and the object name would be `folder_name/shapefle_name_minus_the_shp_extension`.

4. Choose to import the `ESRI shapefile` into the schema **HOTELS**.  

    Within the downloaded `ESRI shapefile`, there is a file named `Points_of_Interest.prj`.  This file mentions the spatial reference system used by this `ESRI shapefile`.  Specify **WGS 84** as the spatial reference system.

    ![Choose schema and reference system](importESRI2.png)

    >By default, the database server adds the following [spatial reference systems](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/spatial-reference-systems-srs-and-spatial-reference-identifiers-srid) to a new database. Additionally, the [`ST_SPATIAL_REFERENCE_SYSTEMS`](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/st-spatial-reference-systems-system-view?q=ST_SPATIAL_REFERENCE_SYSTEMS) System View can be queried for available spatial reference systems.

5. Rename the imported table as it was created using mixed case.

    ```SQL
    RENAME TABLE "Points_of_Interest" TO POI_WATERLOO;
    ```

6. View the table.  Notice that the points of interest locations are stored in a column of type `ST_GEOMETRY`.

    ![view table editor](view-table-metadata.png)

7. Select **Open Data** to view the raw data, and select **View Spatial Data** on the SHAPE column of a point of interest. You may choose to select one or multiple points of interest.

    ![view data](multiple-spatial-data.png)

    The selected location(s) is shown on a map. You can view more details about the point of interest by clicking on a map marker.

    ![view data on a map](spatial-data-map.png)

    >The Leaflet map is not shown in on-premise installs of the SAP HANA database explorer.

8. Perform the below query.

    ```SQL
    SELECT FACILITY, SHAPE.ST_AsWKT(), SHAPE FROM POI_WATERLOO;
    ```

    Notice that the location data can be formatted in a more readable format using the methods `ST_AsWKT` or `ST_AsEWT` which in addition shows the SRID.

    ![view the spatial data](long-point-data.png)

    Additional details on spatial reference systems can be found at [SAP HANA Spatial Reference for SAP HANA Cloud](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/spatial-reference-systems-srs-and-spatial-reference-identifiers-srid).

9. Optionally follow the rest of the sub-steps to import a `GeoJSON` file as a JSON Collection.

    After downloading the file, rename the file to have a `.json` file extension, instead of `.geojson`.

10. Open the file in n Microsoft VS Code.

    Remove the extra metadata at the beginning and end so that the file only contains a JSON document on each line.

    Remove the commas at the end of each line.  Press Ctrl F and use a regular expression search and replace to search for `\},\n` and replace it with `}\n`.

    ![find and replace](find-replace.png)

    To see the required formatting for importing JSON documents, see [SAP HANA Database JSON Document Store Guide](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-json-document-store-guide/import-and-export).

11. Upload the file to a cloud storage provider.

    See [Export and Import Data and Schema with SAP HANA Database Explorer](hana-dbx-export-import) for more information.

12. Start the import data wizard.

    ![Open import data wizard](open-import-data-wizard.png)

    Choose **Import Data** and provide the credentials to the storage bucket containing the JSON file.

13. Choose to import the JSON file into the schema **HOTELS** and name the collection **`POI_WATERLOO_JSON`**.

    ![import JSON](import-JSON.png)

    Alternatively, a text-based import of the JSON file may be used:

    ```SQL
    CREATE COLLECTION POI_WATERLOO_JSON;
    IMPORT FROM JSON FILE 's3-us-east-1://dansawsbucket1/Points_of_Interest.json' INTO POI_WATERLOO_JSON WITH FAIL ON INVALID DATA CREDENTIAL 'AWS';
    ```

    After the import completes and the catalog is refreshed, a new JSON collection will appear.

    ![imported collection](import-json-success.png)

    Click on the `glasses` icon to open the JSON document viewer.

    ![view json](json-viewer.png)

14. The following SQL queries show a few examples of querying the imported JSON data including the last query which can be shown on the map viewer.

    ```SQL
    SELECT * FROM POI_WATERLOO_JSON;  --JSON data format. Can use the built-in viewer.

    SELECT "properties".FACILITY as name, "properties".TYPE as type, "geometry" as geometry FROM POI_WATERLOO_JSON;  --Can return data in column format.

    SELECT "properties".FACILITY as name, "properties".TYPE as type, JSON_VALUE("geometry", '$.coordinates[0]') as long, JSON_VALUE("geometry", '$.coordinates[1]') as lat FROM POI_WATERLOO_JSON; --Can return longitude and latitude

    SELECT "properties".FACILITY as name, "properties".TYPE as type, ST_GeomFromGeoJSON("geometry", 4326) as shape FROM POI_WATERLOO_JSON; --Can now be shown in the map viewer
    ```

    Additional details can be found at [JSON_VALUE Function](https://help.sap.com/docs/HANA_CLOUD_DATABASE/c1d3f60099654ecfb3fe36ac93c121bb/9355cb9e45a149c1a6ddb2bd2392d864.html) and [ST_GeomFromGeoJSON](https://help.sap.com/docs/HANA_CLOUD_DATABASE/bc9e455fe75541b8a248b4c09b086cf5/40771e89ed1641e88674b45adb2ef6a1.html).


### Use spatial functions in a query
1. The following statement shows the list of points of interest within 3 kilometers of the `Delta` hotel.

    ```SQL
    SELECT
        P.FACILITY,
        P.TYPE,
        P.ADDRESS,
        ROUND(H.LOCATION.ST_Distance(P.SHAPE, 'kilometer'), 2) as DISTANCE,
        P.SHAPE
    FROM HOTEL H, POI_WATERLOO P
    WHERE
        H.HNO=26 /*Delta*/ AND
        TO_BOOLEAN(H.LOCATION.ST_WithinDistance(NEW ST_Point(SHAPE.ST_AsWKT(), 4326), 3, 'kilometer')) = TRUE
    ORDER BY DISTANCE ASC;
    ```

    Each point can be viewed as shown below.

    ![Within 3 km](within-distance.png)

    ![Rail Station](rail-station.png)

    Alternatively, all the points can be shown together.

    ```SQL
    SELECT
        ST_UnionAggr(P.SHAPE)
    FROM HOTEL H, POI_WATERLOO P
    WHERE
        H.HNO=26 /*Delta*/ AND
        TO_BOOLEAN(H.LOCATION.ST_WithinDistance(NEW ST_Point(SHAPE.ST_AsWKT(), 4326), 3, 'kilometer')) = TRUE;
    ```

    ![Points of interest close by](poi-close-to-hotel.png)

    >The map may not display if the byte limit is exceeded. The byte limit can be changed in the SQL console settings.

    For additional details, see [ST_Point Type](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/st-point-type), [ST_Distance](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/st-distance-method), [ST_WithinDistance](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/st-withindistance-method), and [ST_UnionAggr Method](https://help.sap.com/docs/HANA_CLOUD_DATABASE/bc9e455fe75541b8a248b4c09b086cf5/601aa9fb93e241af96faafcb8f01b12e.html).

    To view all access methods for spatial data, see [SAP HANA Spatial Reference for SAP HANA Cloud - Accessing and Manipulating Spatial Data](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-spatial-reference/accessing-and-manipulating-spatial-data).

    >The latitude and longitude for a location in Google Maps can be obtained for a given address via the marker's context menu.  

    >![google maps](google.png)

    >It should also be noted that when a used in a ST_POINT, x is longitude and y is latitude so the above point would be represented as `NEW ST_Point('POINT (-94.71832 32.50459)', 4326)`.

### Knowledge check

Congratulations! You have explored a few of the multi-model features in SAP HANA and are now familiar with graph workspaces, JSON collections, and spatial data when using the SAP HANA database explorer.


---
