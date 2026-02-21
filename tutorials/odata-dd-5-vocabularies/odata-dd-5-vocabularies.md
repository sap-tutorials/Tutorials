---
parser: v2
author_name: DJ Adams
author_profile: https://github.com/qmacro
auto_validation: false
primary_tag: software-product>sap-business-technology-platform
tags: [ software-product>sap-business-technology-platform, topic>cloud, programming-tool>odata, tutorial>beginner ]
time: 20
---

# Understand how vocabularies are used in OData metadata documents

<!-- description --> Metadata can be extended, using content organized into vocabularies.

## You will learn

- How annotations are organized into vocabularies
- How they're included within an OData metadata document

## Intro

The members of the OData Technical Committee have worked hard on OData as a robust, well-defined and extensible standard. In this tutorial we'll look at a key extensibility mechanism in the form of vocabularies, and how they're used to organize annotations.

---

### Examine the rest of the entity model wrapper

In the previous [Metadata](https://developers.sap.com/tutorials/odata-dd-4-metadata.html) tutorial we saw how the schema was presented within a context. That context is called the [entity model wrapper](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752500). We left the examination of part of that wrapper - the references to vocabularies - to this tutorial. Let's start by digging into those now.

Here's what the relevant section of the wrapper in [the Northbreeze OData service's metadata document](https://odd.cfapps.eu10.hana.ondemand.com/northbreeze/$metadata) looks like:

```xml
<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:Reference
    Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Capabilities.V1.xml">
    <edmx:Include Alias="Capabilities" Namespace="Org.OData.Capabilities.V1"/>
  </edmx:Reference>
  <edmx:Reference
    Uri="https://sap.github.io/odata-vocabularies/vocabularies/Common.xml">
    <edmx:Include Alias="Common" Namespace="com.sap.vocabularies.Common.v1"/>
  </edmx:Reference>
  <edmx:Reference
    Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Core.V1.xml">
    <edmx:Include Alias="Core" Namespace="Org.OData.Core.V1"/>
  </edmx:Reference>
  ...
</edmx:Edmx>
```

What are these references? Well, section [3.3 Element edmx:Reference](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752504) of the CSDL standards document is helpful here (as well as section [3.1 Element edmx:Edmx](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752504) telling us that there can be zero or more of them within an `<edmx:Edmx>` element).

Basically, `edmx:Reference` elements point to external CSDL documents, specific content from which (indicated by the `edmx:Include` elements within) is then added to the overall scope of the referring (OData metadata) document.

Think of it like an "include" or "import" as found in various programming languages; these references might look like this in pseudo-JavaScript, for example:

```javascript
import { "Org.OData.Capabilities.V1" as "Capabilities" } from "https://oasis-tcs.github.io/.../Org.OData.Capabilities.V1.xml";
import { "com.sap.vocabularies.Common.v1" as "Common" } from "https://sap.github.io/.../Common.xml";
import { "Org.OData.Core.V1" as "Core" } from "https://oasis-tcs.github.io/.../Org.OData.Core.V1.xml";
```

### Meet the OData vocabularies

Note that each of the referenced external documents in our OData metadata document are resources in GitHub repositories:

- two belonging to OASIS in <https://oasis-tcs.github.io/odata-vocabularies/>
- one belonging to SAP in <https://sap.github.io/odata-vocabularies/>

Taking the first of the three `<edmx:Reference>` elements here:

```xml
<edmx:Reference
  Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Capabilities.V1.xml">
  <edmx:Include Alias="Capabilities" Namespace="Org.OData.Capabilities.V1"/>
</edmx:Reference>
```

we see that:

- the reference points to an [XML representation](https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Capabilities.V1.xml) of a CSDL document "Org.OData.Capabilities.V1"
- moving one level up from that CSDL document resource's location, there is a [vocabularies overview page](https://oasis-tcs.github.io/odata-vocabularies/vocabularies/) listing each of the OASIS Technical Committee vocabularies, including this "Capabilities" one.
- for each of these vocabulary resources there are HTML, XML and JSON representations
- the HTML representation is especially useful for us as it describes the vocabulary's purpose in general, and gives details for each of the terms and types contained within it

![OASIS OData TC - Vocabularies](oasis-vocabularies-toc.png)

If we look specifically at the [XML source](https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Capabilities.V1.xml) representation of the "Capabilities" vocabulary in CSDL form, we see this:

```xml
<?xml version="1.0" encoding="utf-8"?>
  ...

  Abstract:
  This document contains terms describing capabilities of an OData service.

-->
<edmx:Edmx xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx" Version="4.0">
  <edmx:Reference Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Authorization.V1.xml">
    <edmx:Include Alias="Authorization" Namespace="Org.OData.Authorization.V1" />
  </edmx:Reference>
  <edmx:Reference Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Core.V1.xml">
    <edmx:Include Alias="Core" Namespace="Org.OData.Core.V1" />
  </edmx:Reference>
  <edmx:Reference Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Validation.V1.xml">
    <edmx:Include Alias="Validation" Namespace="Org.OData.Validation.V1" />
  </edmx:Reference>
  <edmx:DataServices>
    <Schema xmlns="http://docs.oasis-open.org/odata/ns/edm" Namespace="Org.OData.Capabilities.V1" Alias="Capabilities">
    ...
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

It's another EDMX document! There is definitely a certain beauty to the OData specifications that is special.

Anyway, there are two thing of note here:

- this document _also_ has references to vocabularies
- there is a single `<Schema>` element, with the OData namespace "Org.Data.Capabilities.V1"

And that specific schema is exactly the one that's referenced in the `<edmx:Include>` element within the `<edmx:Reference>` element:

```xml
<edmx:Include Alias="Capabilities" Namespace="Org.OData.Capabilities.V1"/>
```

It just so happens that in this referenced CSDL document, there _is_ only one schema, but there could be more.

So an `<edmx:Include>` element forms an important part of the `<edmx:Reference>`.

> Indeed, [section 3.3 Element edmx:Reference](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752504) states that at least one `<edmx:Include>` or `<edmx:IncludeAnnotation>` is mandatory.

The `<edmx:Include>` element serves to identify a particular schema to be included, from the referenced vocabulary. It also does one more thing here - it specifies a short name, in the form of a value for an `Alias` attribute, which can be used to refer to that imported vocabulary schema. Just like the `as` aliasing in our imaginary JavaScript equivalent example earlier. So instead of the full name "Org.OData.Capabilities.V1" being needed to prefix vocabulary terms, the short form "Capabilities" can be used, as we'll see in subsequent steps.

### Take a first look at the annotations

Now we understand how vocabulary resources are referenced to be included into an OData metadata document, let's now take a first look at what annotations are used in this particular OData metadata document, and how.

To set the scene, if we take brief look at the CSDL specification, we learn that:

- "Vocabulary annotations can be specified as a child of the model element being annotated or as a child of an `edm:Annotations` element that targets the model element." (from [section 4.6 Annotations](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Toc453752519))
- "An annotation applies a term to a model element and defines how to calculate a value for the applied term." (from [section 14 Vocabulary and Annotation](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Vocabulary_and_Annotation))

Additionally:

- "A service SHOULD NOT require a client to interpret annotations. Clients SHOULD ignore unknown terms and silently treat unexpected or invalid values (including invalid type, invalid literal expression, etc.) as an unknown value for the term." ( also from [section 14 Vocabulary and Annotation](https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part3-csdl/odata-v4.0-errata03-os-part3-csdl-complete.html#_Vocabulary_and_Annotation))

With the information from the first two points here, we can see in our OData metadata document that both approaches to annotation are used.

First, we see the "annotation as child element" approach where the annotation "Core.Links" is applied directly to the `<Schema>` element via the `<Annotation>` element which appears a direct child of `<Schema>`:

```xml
<Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
  <Annotation Term="Core.Links">
    <Collection>
      <Record>
        <PropertyValue Property="rel" String="author"/>
        <PropertyValue Property="href" String="https://cap.cloud.sap"/>
      </Record>
    </Collection>
  </Annotation>
  ...
</Schema>
```

Further on in the metadata document we see an example of the other approach, where `<Annotation>` elements appear as direct children of a containing `<Annotations>` element, and the schema elements to which the annotations are applied are specified in `Target` attributes):

> Again, as explained in the previous [Metadata](https://developers.sap.com/tutorials/odata-dd-4-metadata.html) tutorial, in the "Get acquainted with the schema element" step, we'll leave out the XML namespace prefix `edm` her when writing elements that belong to that namespace.


```xml
<Schema Namespace="Main" xmlns="http://docs.oasis-open.org/odata/ns/edm">
  ...
  <EntityContainer Name="EntityContainer">
    <EntitySet Name="Categories" EntityType="Main.Categories">
      <NavigationPropertyBinding Path="Products" Target="Products"/>
    </EntitySet>
  </EntityContainer>
  ...
  <Annotations Target="Main.EntityContainer/Categories">
    <Annotation Term="Capabilities.DeleteRestrictions">
      <Record Type="Capabilities.DeleteRestrictionsType">
        <PropertyValue Property="Deletable" Bool="false"/>
      </Record>
    </Annotation>
    <Annotation Term="Capabilities.InsertRestrictions">
      <Record Type="Capabilities.InsertRestrictionsType">
        <PropertyValue Property="Insertable" Bool="false"/>
      </Record>
    </Annotation>
    <Annotation Term="Capabilities.UpdateRestrictions">
      <Record Type="Capabilities.UpdateRestrictionsType">
        <PropertyValue Property="Updatable" Bool="false"/>
      </Record>
    </Annotation>
  </Annotations>
</Schema>
```

Here, three different annotations terms:

- Capabilities.DeleteRestrictions
- Capabilities.InsertRestrictions
- Capabilities.UpdateRestrictions

are being applied via `Target="Main.EntityContainer/Categories"` to the "Categories" entityset in the entity container named "EntityContainer" in the "Main" OData namespace.

In the next tutorial we'll dig in to the detail of the annotations used in this OData metadata document.
