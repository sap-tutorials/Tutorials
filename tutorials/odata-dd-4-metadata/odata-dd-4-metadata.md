---
parser: v2
author_name: DJ Adams
author_profile: https://github.com/qmacro
auto_validation: false
primary_tag: software-product>sap-business-technology-platform
tags: [ software-product>sap-business-technology-platform, topic>cloud, programming-tool>odata, tutorial>beginner ]
time: 20
---

# Learn how to read OData metadata

<!-- description --> Take a tour of a simple metadata document and get to know its key sections.

## You will learn

- What the metadata document looks like
- What it describes
- How to navigate it

## Intro

A key resource in any OData service is its metadata document. In this tutorial you'll take a tour of a simple metadata document (the one for the Northbreeze service introduced in the [previous tutorial in this mission](https://developers.sap.com/tutorials/odata-dd-3-northbreeze.html)).

Throughout this tutorial you should endeavor to use your own instance of the Northbreeze service (see the [previous tutorial](https://developers.sap.com/tutorials/odata-dd-3-northbreeze.html)); for illustration purposes, URLs for the publicly available instance will be used here.

---

### Retrieve the Northbreeze metadata document

Head over to your Northbreeze service and request the metadata document resource. The URL is formed from the OData service root (where the service document is):

<https://odd.cfapps.eu10.hana.ondemand.com/northbreeze>

with `$metadata` added as a further path segment:

<https://odd.cfapps.eu10.hana.ondemand.com/northbreeze/$metadata>

### Take a first look at the content

Initially the content of this resource can be a little overwhelming. Here's what the first part looks like:

![Part of the metadata document](metadata-document.png)

But if we [stare at it](https://qmacro.org/blog/posts/2017/02/19/the-beauty-of-recursion-and-list-machinery/#initial-recognition) for long enough, it becomes less overwhelming and we start to see the structure.

### Consider the high level XML structure 

Regard this drastically reduced version of the metadata document XML:

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
      <EntityType Name="Products">
        <Key>
          <PropertyRef Name="ProductID"/>
        </Key>
        <Property Name="ProductID" Type="Edm.Int32" Nullable="false"/>
        <Property Name="ProductName" Type="Edm.String"/>
        <Property Name="QuantityPerUnit" Type="Edm.String"/>
      </EntityType>
      <Annotations Target="Main.EntityContainer/Products">
        <Annotation Term="Capabilities.DeleteRestrictions"></Annotation>
        <Annotation Term="Capabilities.InsertRestrictions"></Annotation>
        <Annotation Term="Capabilities.UpdateRestrictions"></Annotation>
      </Annotations>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

It allows us to see the overall structure of this resource, and start to feel a bit more comfortable navigating it. Being XML, the first thing we see is the XML declaration (see the [Key terminology](https://en.wikipedia.org/wiki/XML#Key_terminology) section of the XML page on Wikipedia), and then we have the document itself.

The outermost (or "root") element is `Edmx`, which has:

- a version attribute which reflects the OData version
- a namespace declaration

It also contains, as children:

- a number of vocabulary references
- a single `DataServices` element

(we will look briefly at namespaces and vocabularies in subsequent steps).

The primary content of interest to us in any metadata document is the content within the `DataServices` element, as that's [where the rubber meets the road](https://en.wiktionary.org/wiki/the_rubber_meets_the_road) with respect to what the OData service represents for us as architects or developers.

But it helps if we are comfortable with the rest of the document, the "context" for the content of the `DataServices` element so to speak, if only to be able to mentally ignore it, to move past it and get to what we're looking for.

### Understand the DataServices context

As described in the OData standards document [OData Version 4.0. Part 3: Common Schema Definition Language (CSDL) Plus Errata 03](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html), specifically in [section 3.1 Element edmx:Edmx](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752501), the root `Edmx` element (a) is mandatory and (b) must contain a single `DataServices` element, and optionally some `Reference` elements.


