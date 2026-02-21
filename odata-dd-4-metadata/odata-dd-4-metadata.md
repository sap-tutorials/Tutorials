---
parser: v2
author_name: DJ Adams
author_profile: https://github.com/qmacro
auto_validation: false
primary_tag: software-product>sap-business-technology-platform
tags: [ software-product>sap-business-technology-platform, topic>cloud, programming-tool>odata, tutorial>beginner ]
time: 30
---

# Learn how to read OData metadata documents

<!-- description --> Take a tour of a simple metadata document and get to know its key sections.

## You will learn

- What the metadata document looks like
- What it describes
- How to navigate it

## Intro

A key resource in any OData service is its metadata document. In this tutorial you'll take a tour of a simple metadata document (the one for the Northbreeze service introduced in the [previous tutorial in this mission](https://developers.sap.com/tutorials/odata-dd-3-northbreeze.html)).

Throughout this tutorial you should endeavor to use your own instance of the Northbreeze service (see the previous [Northbreeze](https://developers.sap.com/tutorials/odata-dd-3-northbreeze.html) tutorial); for illustration purposes, URLs for the publicly available instance will be used here.

---

<!-- 1 -->
### Retrieve the Northbreeze metadata document

Head over to your Northbreeze service and request the metadata document resource. The URL is formed from the OData service root:

<https://odd.cfapps.eu10.hana.ondemand.com/northbreeze>

with `$metadata` added as a further path segment:

<https://odd.cfapps.eu10.hana.ondemand.com/northbreeze/$metadata>

<!-- 2 -->
### Take a first look at the content

Initially the content of this resource can be a little overwhelming. Here's what the first part looks like:

![Part of the metadata document](metadata-document.png)

But if we [stare at it](https://qmacro.org/blog/posts/2017/02/19/the-beauty-of-recursion-and-list-machinery/#initial-recognition) for long enough, it becomes less overwhelming and we start to see the structure.

<!-- 3 -->
### Consider the high level XML structure

Regard this drastically reduced version of the entire metadata document XML structure:

```xml
<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:Reference Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Capabilities.V1.xml"></edmx:Reference>
  <edmx:Reference Uri="https://sap.github.io/odata-vocabularies/vocabularies/Common.xml"></edmx:Reference>
  <edmx:Reference Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Core.V1.xml"></edmx:Reference>
  <edmx:DataServices>
    <Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <Annotation Term="Core.Links"></Annotation>
      <EntityContainer Name="EntityContainer">
        <EntitySet Name="Products" EntityType="Main.Products"></EntitySet>
        <EntitySet Name="Categories" EntityType="Main.Categories"></EntitySet>
        <EntitySet Name="Suppliers" EntityType="Main.Suppliers"></EntitySet>
      </EntityContainer>
      <EntityType Name="Products"></EntityType>
      <EntityType Name="Categories"></EntityType>
      <EntityType Name="Suppliers"></EntityType>
      <Annotations Target="Main.EntityContainer/Categories">
        <Annotation Term="Capabilities.DeleteRestrictions"></Annotation>
      </Annotations>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

It allows us to see the overall structure of this resource, and start to feel a bit more comfortable navigating it. Being XML, the first thing we see is the XML declaration (`<?xml ...?>` - see the [Key terminology](https://en.wikipedia.org/wiki/XML#Key_terminology) section of the XML page on Wikipedia), and then we have the document itself.

The outermost (or "root") element is `Edmx`, which has:

- a `Version` attribute which reflects the OData version
- a namespace prefix
- a namespace declaration

It also contains, as children:

- a number of references to vocabularies
- a single `DataServices` element

The primary area of interest to us in any metadata document is the content within the `DataServices` element, as that's [where the rubber meets the road](https://en.wiktionary.org/wiki/the_rubber_meets_the_road) with respect to what the OData service represents for us as architects or developers. But it helps if we are comfortable with the rest of the document, the "context" for the content of the `DataServices` element so to speak, if only to be able to mentally put it aside, move past it and get to what we're looking for.

So we will look briefly at namespaces in the next step. We'll look at OData vocabularies & annotations in the next couple of tutorials.

<!-- 4 -->
### Understand the XML namespaces

While not critical to getting to the heart of what the metadata document conveys, its worth dwelling for a moment on all those element name prefixes (such as the `edmx` part of `<edmx:Edmx>`, `<edmx:Reference>` and so on).

These are artifacts relating to the use of XML namespaces.

> There are actually two different types of namespaces at play in these OData metadata document resources. There are the XML namespaces, which are the subject of this step. There are also OData namespaces. These are found in `Namespace` attributes of the `<edmx:Include>` and `<Schema>` elements. We'll look at the schema namespace in a later step in this tutorial, and at the namespaces in the `<edmx:Include>` elements in the next tutorial.

For the usual reasons, namespaces are used in XML to compartmentalize element and attribute names, which allow the use of various XML vocabularies (not to be confused with the OData vocabularies which we'll look at next) together in a single document, without element and attribute name collisions.

These XML namespaces are declared with `xmlns` attributes, which are either in the pure `xmlns` form, or in a `xmlns:prefix` form. The first form is how a default namespace is declared, the second is how non-default (named) namespaces are declared. Any element can be specified with a namespace prefix (such as `edmx:Reference`) or without (such as `<Schema>`). Elements without a specific namespace prefix are considered to belong to the default namespace.

So, if we look again at the entire XML structure, differently compacted this time:

```xml
<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:Reference Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Capabilities.V1.xml">
    <edmx:Include Alias="Capabilities" Namespace="Org.OData.Capabilities.V1"/>
  </edmx:Reference>
  <edmx:DataServices>
    <Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <Annotation Term="Core.Links"> ... </Annotation>
      <EntityContainer Name="EntityContainer">
        <EntitySet Name="Products" EntityType="Main.Products"> ... </EntitySet>
      </EntityContainer>
      <EntityType Name="Products"> ... </EntityType>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

we see that there are two XML namespaces at play, a named one (i.e. using a prefix) and a default one:

Namespace|Prefix|Covers
-|-|-
`http://docs.oasis-open.org/odata/ns/edmx`|`edmx`|`Edmx`, `Reference`, `Include`, `DataServices`
`http://docs.oasis-open.org/odata/ns/edm`|(default)|`Schema`, `EntityContainer`, `EntitySet`, `EntityType` etc

As the primary area of interest in such resources is what's in the `DataServices` section (the entity type definitions, the entitysets, annotations and so on) it makes sense to specify the namespace that encompasses the elements that are used for such definitions ... as the the default, affording clarity in such declarations (i.e. less "busy", as the element names aren't prefixed).

<!-- 5 -->
### Learn about the DataServices context

To understand the context of the `DataServices` element, let's use what we learned in the [Standards](https://developers.sap.com/tutorials/odata-dd-2-standards.html) tutorial in this mission on navigating OData standards documents.

We should refer to the OData standards document "OData Version 4.0. Part 3: Common Schema Definition Language (CSDL)", the latest version being available at the canonical URL <https://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html>, which brings us specifically to the "Oasis Standard Plus Errata 03" version which has its own URL <https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html>.

The document's "Abstract" section tells us that we're on the right track:

> "OData services are described by an Entity Data Model (EDM). The Common Schema Definition Language (CSDL) defines an XML representation of the entity data model exposed by an OData service."

In this document, [section 3 Entity Model Wrapper](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752500) tells us all about this context:

- the root `edmx:Edmx` element (a) is mandatory and (b) must contain a single `edmx:DataServices` element
- that single `edmx:DataServices` element must contain one or more `edm:Schema` elements
- it is in these `edm:Schema` elements that our OData service schemas (service and entity detail) are defined

In our case, there's one schema, and therefore a single `edm:Schema` element.

<!-- 6 -->
### Get acquainted with the schema element

> The `edm` prefix to the `Schema` element name here is from the documentation; in our particular metadata document the namespace represented by this prefix, `http://docs.oasis-open.org/odata/ns/edm`, is defined as the default (see the previous step). From now on, element names in the standards document that are prefixed with `edm` will be written here without the prefix, to stay close to our specific metadata document.

To become acquainted with the `Schema` element, we can now jump to [section 5 Schema](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752520) to know what to expect inside it. The section tells us to expect one or more of the following elements:

- `Action`
- `Annotations`
- `Annotation`
- `ComplexType`
- `EntityContainer`
- `EntityType`
- `EnumType`
- `Function`
- `Term`
- `TypeDefinition`

If we inspect what's in our `Schema`, we see these elements at the next level:

- `Annotation` (and `Annotations`)
- `EntityContainer`
- `EntityType`

Visualizing our path through this metadata document, we've now found our way to what we really want to know:

```text
+------+
| Edmx |--+
+------+  |
          |
   +--------------+
   | DataServices |--+
   +--------------+  |
                     |
                 +--------+
                 | Schema |
                 +--------+
                     |
           +---------+-------+------------------+
           |                 |                  |
 +-----------------+   +------------+     +------------+
 | EntityContainer |   | EntityType |-+   | Annotation |-+
 +-----------------+   +------------+ |   +------------+ |
                         +------------+     +------------+
```

Note that there is only a single entity container (see [section 13 Entity Container](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752596)), but multiple entity types and annotations.

Annotations are covered in a subsequent tutorial, so that leaves the `EntityContainer` and `EntityType` elements. Let's take these one at a time to round out our brief look at the metadata document.

<!-- 7 -->
### Take a brief look at the OData namespace

Before we do, we should make a note of one more thing at this level, and that's the `Namespace` attribute in the `<Schema>` element:

```xml
<Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
   ...
</Schema>
```

Both attributes of this element relate to namespaces:

- `xmlns` gives us the XML namespace (as discussed previously)
- `Namespace` is an OData mechanism

That OData namespace mechanism is described in the aforementioned CSDL standards document, in [section 5.1.1 Attribute namespace](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752522), thus:

> "A schema is identified by a namespace. All `edm:Schema` elements MUST have a namespace defined through a `Namespace` attribute which MUST be unique within the document, and SHOULD be globally unique ... It is combined with the name of elements in the entity model to create unique qualified names ..."

The value of this `Namespace` attribute is `Main` (this OData service is served from a CAP server, which generates the value from the service name).

This is certainly unique within the metadata document itself, but the value is certainly not globally unique.

This is fine according to the "MUST" and "SHOULD" terms, which are defined according to [RFC2119](https://www.ietf.org/rfc/rfc2119.txt) (see [section 1.1 Terminology](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752492)):

- "MUST" means that the definition is an absolute requirement (which is fulfilled, here)
- "SHOULD" means that the definition is a recommendation

Incidentally, here are some values for this OData namespace definition from similar services curated and maintained by OASIS:

- [Northwind](https://services.odata.org/V4/Northwind/Northwind.svc/$metadata):

  ```xml
  <Schema
    xmlns="http://docs.oasis-open.org/odata/ns/edm"
    Namespace="NorthwindModel">
  ```

- [TripPin](https://services.odata.org/V4/TripPinServiceRW/$metadata):

  ```xml
  <Schema
    xmlns="http://docs.oasis-open.org/odata/ns/edm"
    Namespace="Microsoft.OData.SampleService.Models.TripPin">
  ```

Let's have a brief look at where this `Main` OData namespace is used. Here's another drastically reduced version of the entire XML document, showing where `Main` is found:

```xml
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EntityContainer Name="EntityContainer">
        <EntitySet Name="Products" EntityType="Main.Products"> ... </EntitySet>
        <EntitySet Name="Categories" EntityType="Main.Categories"> ... </EntitySet>
        <EntitySet Name="Suppliers" EntityType="Main.Suppliers"> ... </EntitySet>
      </EntityContainer>
      <EntityType Name="Products">
        ...
        <NavigationProperty Name="Category" Type="Main.Categories" Partner="Products"> ... </NavigationProperty>
        <NavigationProperty Name="Supplier" Type="Main.Suppliers" Partner="Products"> ... </NavigationProperty>
      </EntityType>
      <EntityType Name="Categories">
        ...
        <NavigationProperty Name="Products" Type="Collection(Main.Products)" Partner="Category"/>
      </EntityType>
      ...
      <Annotations Target="Main.EntityContainer/Categories">
        <Annotation Term="Capabilities.DeleteRestrictions"> ... </Annotation>
        ...
      </Annotations>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

The `Main` namespace is used to prefix the "variable building blocks" of the schema when referencing them. So:

- the `EntitySet` "Products" refers to `Main.Products` as the type of the entity contained
- within the corresponding `EntityType` definition we see that the `NavigationProperty` "Category" is of type `Main.Categories`

Following that to the "Categories" `EntityType` we see that:

- there's another `NavigationProperty` "Products" that leads back to the `Main.Products` type, this time in a `Collection( ... )` expression (denoting a zero-or-more relationship)

Finally:

- annotations have targets, which are expressed as containees (such as the "Products" `EntitySet`) of the `Main.EntityContainer` element

<!-- 8 -->
### Consider the entity container

The `EntityContainer` element within the `Schema` represents the "shop front" of the OData service. It's here that the entitysets are declared. If there are any actions or functions defined in the service, they would be found listed here too, via `<ActionImport>` and `<FunctionImport>` elements respectively.

If there are any relationships emanating from the `EntityType` upon which an `EntitySet` is based, these are declared in that definition. For example, the "Products" `EntityType` has relationships with "Categories" and "Suppliers", as declared with the `NavigationPropertyBinding` elements within the "Products" `EntitySet`:

```xml
<EntitySet Name="Products" EntityType="Main.Products">
  <NavigationPropertyBinding Path="Category" Target="Categories"/>
  <NavigationPropertyBinding Path="Supplier" Target="Suppliers"/>
</EntitySet>
```

Thus the `EntityContainer` is a great machine-readable overview of the entire service. At a high level (from an entityset perspective), it corresponds with the content of the service document, which looks like this:

```json
{
  "@odata.context": "$metadata",
  "@odata.metadataEtag": "...",
  "value": [
    {
      "name": "Products",
      "url": "Products"
    },
    {
      "name": "Categories",
      "url": "Categories"
    },
    {
      "name": "Suppliers",
      "url": "Suppliers"
    }
  ]
}
```

<!-- 9 -->
### Take a look at the entity type definitions

Last but certainly not least, we come to the `EntityType` elements within the `Schema`. These correspond directly to the business objects, or entities, in our data model, and as such, are described using elements that reflect such detail. Let's take one of the types as an example:

```xml
<EntityType Name="Products">
  <Key>
    <PropertyRef Name="ProductID"/>
  </Key>
  <Property Name="ProductID" Type="Edm.Int32" Nullable="false"/>
  <Property Name="ProductName" Type="Edm.String"/>
  <Property Name="QuantityPerUnit" Type="Edm.String"/>
  <Property Name="UnitPrice" Type="Edm.Decimal" Scale="variable"/>
  <NavigationProperty Name="Category" Type="Main.Categories" Partner="Products">
    <ReferentialConstraint Property="Category_CategoryID" ReferencedProperty="CategoryID"/>
  </NavigationProperty>
  <Property Name="Category_CategoryID" Type="Edm.Int32"/>
  <NavigationProperty Name="Supplier" Type="Main.Suppliers" Partner="Products">
    <ReferentialConstraint Property="Supplier_SupplierID" ReferencedProperty="SupplierID"/>
  </NavigationProperty>
  <Property Name="Supplier_SupplierID" Type="Edm.Int32"/>
  <Property Name="UnitsInStock" Type="Edm.Int32"/>
  <Property Name="UnitsOnOrder" Type="Edm.Int32"/>
  <Property Name="ReorderLevel" Type="Edm.Int32"/>
  <Property Name="Discontinued" Type="Edm.Boolean"/>
</EntityType>
```

The `EntityType` clearly defines:

- the individual properties and their [types](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752517)
- which property or properties make up the entity's key structure
- the navigation properties, their relationship conditions, and the type of the navigation (relation)'s target

> Some of the `Property` elements have more information in further attributes (such as `Nullable="false"` for key properties, and `Scale="variable"` for how long the decimal place value can be).

Mostly these definitions are self-explanatory, but it's worth digging in a little to the detail of navigation properties. Let's take the "Category" one here, and also look at the relevant part of the "Category" `EntityType`:

```xml
<Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
  ...
  <EntityType Name="Products">
    ...
    <NavigationProperty Name="Category" Type="Main.Categories" Partner="Products">
      <ReferentialConstraint Property="Category_CategoryID" ReferencedProperty="CategoryID"/>
    </NavigationProperty>
    <Property Name="Category_CategoryID" Type="Edm.Int32"/>
    ...
  </EntityType>
  <EntityType Name="Categories">
    <Key>
      <PropertyRef Name="CategoryID"/>
    </Key>
    <Property Name="CategoryID" Type="Edm.Int32" Nullable="false"/>
    <NavigationProperty Name="Products" Type="Collection(Main.Products)" Partner="Category"/>
  </EntityType>
  ...
</Schema>
```

If we stare at this for a moment, this relation is expressed beautifully:

```text
+----------+  N:1 +------------+
| Products |------| Categories |
+----------+      +------------+
```

Looking at the "Products" `EntityType`, we see that:

- there is a `Property` "Category_CategoryID" to hold the actual category foreign key
- there is also a `NavigationProperty` "Category" [along which we can traverse the relationship](https://odd.cfapps.eu10.hana.ondemand.com/northbreeze/Products?$top=3&$expand=Category) with, say, the OData system query option `$expand`
- the `ReferentialConstraint` that is contained within (and therefore qualifies) the `NavigationProperty` says that the value of "Category_CategoryID" must equal the value of the property "CategoryID" in the target ("Main.Categories")

Looking at the "Categories" `EntityType`, we see that:

- the "CategoryID" property is indeed the key property
- there's a reverse `NavigationProperty` "Products" but the type is a _collection_ of `Main.Products` signifying a to-many relationship (as in "N" in the relation diagram above)

### Further info

- [Serving OData APIs](https://cap.cloud.sap/docs/guides/protocols/odata) in Capire
- [ABAP RESTful Application Programming Model](https://help.sap.com/docs/abap-cloud/abap-rap/abap-restful-application-programming-model)
