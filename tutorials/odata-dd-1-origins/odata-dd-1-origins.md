---
parser: v2
author_name: DJ Adams
author_profile: https://github.com/qmacro
auto_validation: false
primary_tag: software-product>sap-business-technology-platform
tags: [ software-product>sap-business-technology-platform, topic>cloud, programming-tool>odata, tutorial>beginner ]
time: 20
---

# Learn about OData's origins

<!-- description --> Discover OData's origins in RSS and Atom.

## You will learn

- Where OData came from
- Why OData looks and acts the way it does

## Intro

*This series of tutorials on OData is to replace the original "Take a Deep Dive into OData" which you may already know, and which we're now retiring. We're building out the content of this new series in the open, as a work in progress, and would love to hear your feedback, what you would like to see and what you think of the content as we create it. As you can see, it's very early days. Please let us know your thoughts on this new mission [via this issue link](https://github.com/sap-tutorials/Tutorials/issues/new?title=Early%20feedback%20on%20the%20new%20OData%20mission&body=Let%20us%20know%20your%20thoughts%20on%20this%20new%20series%20-%20what%20you%20like,%20dislike,%20would%20like%20to%20see.%20Thank%20you!).*

OData is an open standard that is both a data format and a protocol for consuming and manipulating data in a uniform way. It's ISO/IEC approved and managed by the [OASIS organization](https://www.oasis-open.org/).

OData has its origins in the world of weblogs and syndication, but now serves to power a great deal of the API and integration activities in typical SAP enterprise environments. This tutorial will help you understand OData's origins.

---

### Examine RSS, an ancestor of OData

You can understand OData as being the combination of two essential parts. The first is the format, the second is the protocol. The format defines how data is structured, described and serialized. The protocol defines how that data is retrieved, manipulated, and maintained.

The origin of OData's format comes from the world of weblogs: blogging and syndication. The Rich Site Summary (RSS) format was defined to describe a blog and the posts available in it, typically with the newest posts first, but in XML format for machine consumption.

> RSS is also known as "RDF Site Summary" or "Really Simple Syndication".

Let's look at an example of RSS. The British Broadcasting Corporation (BBC) maintains a number of [news feeds](https://www.bbc.co.uk/news/10628494), one of which is for [World News](https://feeds.bbci.co.uk/news/world/rss.xml).

The content of this feed looks something like this (reduced to just a few items for brevity):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<rss
  xmlns:dc="http://purl.org/dc/elements/1.1/"
  xmlns:content="http://purl.org/rss/1.0/modules/content/"
  xmlns:atom="http://www.w3.org/2005/Atom"
  xmlns:media="http://search.yahoo.com/mrss/"
  version="2.0">
    <channel>
        <title><![CDATA[BBC News]]></title>
        <description><![CDATA[BBC News - World]]></description>
        <link>https://www.bbc.co.uk/news/world</link>
        <image>
            <url>https://news.bbcimg.co.uk/nol/shared/img/bbc_news_120x60.gif</url>
            <title>BBC News</title>
            <link>https://www.bbc.co.uk/news/world</link>
        </image>
        <generator>RSS for Node</generator>
        <lastBuildDate>Mon, 19 Jan 2026 15:14:13 GMT</lastBuildDate>
        <atom:link href="https://feeds.bbci.co.uk/news/world/rss.xml" rel="self" type="application/rss+xml"/>
        <copyright><![CDATA[Copyright: (C) British Broadcasting Corporation, see https://www.bbc.co.uk/usingthebbc/terms-of-use/#15metadataandrssfeeds for terms and conditions of reuse.]]></copyright>
        <language><![CDATA[en-gb]]></language>
        <ttl>15</ttl>
        <item>
            <title><![CDATA[IMF warns of trade tension risk to global growth]]></title>
            <description><![CDATA[Trade tensions and a reversal in the AI boom are among the main risks to global economic growth, the IMF says.]]></description>
            <link>https://www.bbc.com/news/articles/c0r47ey0d1vo?at_medium=RSS&amp;at_campaign=rss</link>
            <guid isPermaLink="false">https://www.bbc.com/news/articles/c0r47ey0d1vo#0</guid>
            <pubDate>Mon, 19 Jan 2026 11:04:56 GMT</pubDate>
            <media:thumbnail width="240" height="134" url="https://ichef.bbci.co.uk/ace/standard/240/cpsprodpb/a2c4/live/b15aa5d0-f51d-11f0-b011-ad48785e60d2.jpg"/>
        </item>
        <item>
            <title><![CDATA[Japan PM Takaichi calls snap election three months after taking office]]></title>
            <description><![CDATA[Sanae Takaichi, who took office last October, hopes the vote will give her a stronger mandate. ]]></description>
            <link>https://www.bbc.com/news/articles/c1dk0x0v6pdo?at_medium=RSS&amp;at_campaign=rss</link>
            <guid isPermaLink="false">https://www.bbc.com/news/articles/c1dk0x0v6pdo#0</guid>
            <pubDate>Mon, 19 Jan 2026 12:00:48 GMT</pubDate>
            <media:thumbnail width="240" height="135" url="https://ichef.bbci.co.uk/ace/standard/240/cpsprodpb/b2dd/live/55618d90-f52f-11f0-b5f7-49f0357294ff.jpg"/>
        </item>
        <item>
            <title><![CDATA[South African team helps search for politician swept away by Mozambique floodwaters]]></title>
            <description><![CDATA[Mozambique President Daniel Chapo cancels a trip to Davos due to severe floods in the country]]></description>
            <link>https://www.bbc.com/news/articles/c62nen4n971o?at_medium=RSS&amp;at_campaign=rss</link>
            <guid isPermaLink="false">https://www.bbc.com/news/articles/c62nen4n971o#1</guid>
            <pubDate>Mon, 19 Jan 2026 14:22:15 GMT</pubDate>
            <media:thumbnail width="240" height="134" url="https://ichef.bbci.co.uk/ace/standard/240/cpsprodpb/116d/live/3a1890e0-f52c-11f0-90ad-db9d8738e245.jpg"/>
        </item>
        <item>
            <title><![CDATA[China's birth rate hits record low as population continues to shrink]]></title>
            <description><![CDATA[Beijing has been trying hard to encourage more young people to marry and have children.]]></description>
            <link>https://www.bbc.com/news/articles/c79r7v7qr53o?at_medium=RSS&amp;at_campaign=rss</link>
            <guid isPermaLink="false">https://www.bbc.com/news/articles/c79r7v7qr53o#1</guid>
            <pubDate>Mon, 19 Jan 2026 07:12:01 GMT</pubDate>
            <media:thumbnail width="240" height="135" url="https://ichef.bbci.co.uk/ace/standard/240/cpsprodpb/cc3b/live/a4e454e0-f50e-11f0-b385-5f48925de19a.jpg"/>
        </item>
    </channel>
</rss>
```

Observe the structure of the XML document.

- Following the XML declaration (first line), `<rss>` is the outermost enclosing element, and in the opening tag, comes with a number of namespace (`xmlns`) declarations and a version. Think of this as the "envelope".
- Nested a level within the `<rss>` element is the `<channel>` element, which is the encoded representation of this feed resource, with elements that one would expect here, conveying the feed's title, image link, last updated date, and so on. Think of this as the "header".
- Also within the `<channel>` element are multiple `<item>` elements, each representing an individual news item in the context of the channel, i.e. world news items. Each item has elements conveying the details, such as news item title, short description, publication date, and so on.

Here's the structure in simple terms:

```text
rss
  |
  +-- channel
        |
        +-- item
        |
        +-- item
        |
        +-- ...
```

Within the RSS envelope, this is indeed much like a document (in the ERP sense), with a header and items.

Beyond news feeds like this, the archetypal use case for RSS (and indeed Atom) is for weblogs, where a channel represents a weblog and the items represent the individual posts.

### Examine the Atom Syndication Format

Atom is a format very similar to RSS, serving the same purpose, that came about for reasons that are not relevant for this tutorial (but see the Further Info section for a link to more background details if you're curious). It's known as the [Atom Syndication Format](https://tools.ietf.org/html/rfc4287).

Some may call Atom a successor to RSS. Unlike RSS, which is just a _format_ specification, Atom also has a related _protocol_ which we'll look at shortly.

Like RSS, the vast majority of Atom use is as machine readable representations of weblogs.

Here's an example of the Atom format, representing the weblog at <https://qmacro.org/blog/> (the Atom feed itself is at <https://qmacro.org/feed/feed.xml>); just two entries are shown, for brevity:

```xml
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
  <title>DJ Adams</title>
  <subtitle>Reserving the right to be wrong</subtitle>
  <link href="https://qmacro.org/feed/feed.xml" rel="self" />
  <link href="https://qmacro.org/blog/" />
  <updated>2026-01-14T00:00:00Z</updated>
  <id>https://qmacro.org/blog/</id>
  <author>
    <name>DJ Adams</name>
  </author>
  <entry>
    <title>Modules, modularity &amp; reuse in CDS models - part 3 - publishing the simple reuse package</title>
    <link href="https://qmacro.org/blog/posts/2026/01/14/modules-modularity-and-reuse-in-cds-models-part-3-publishing-the-simple-reuse-package/" />
    <updated>2026-01-14T00:00:00Z</updated>
    <id>https://qmacro.org/blog/posts/2026/01/14/modules-modularity-and-reuse-in-cds-models-part-3-publishing-the-simple-reuse-package/</id>
    <content type="html">&lt;p&gt;(Get to all the parts in this series via the &lt;a href=&quot;https://qmacro.org/blog/posts/2026/01/01/modules-modularity-and-reuse-in-cds-models/&quot;&gt;series
      post&lt;/a&gt;.)&lt;/p&gt;&lt;p&gt;What we have from &lt;a href=&quot;https://qmacro.org/blog/posts/2026/01/07/modules-modularity-and-reuse-in-cds-models-part-2-creating-a-simple-reuse-package/&quot;&gt;
      ... (the rest of the blog post content)
    </content>
  </entry>
  <entry>
    <title>Modules, modularity &amp; reuse in CDS models - part 2 - creating a simple reuse package</title>
    <link href="https://qmacro.org/blog/posts/2026/01/07/modules-modularity-and-reuse-in-cds-models-part-2-creating-a-simple-reuse-package/" />
    <updated>2026-01-07T00:00:00Z</updated>
    <id>https://qmacro.org/blog/posts/2026/01/07/modules-modularity-and-reuse-in-cds-models-part-2-creating-a-simple-reuse-package/</id>
    <content type="html">&lt;p&gt;(Get to all the parts in this series via the &lt;a href=&quot;https://qmacro.org/blog/posts/2026/01/01/modules-modularity-and-reuse-in-cds-models/&quot;&gt;series post&lt;/a&gt;.)&lt;/p&gt;
      &lt;p&gt;In &lt;a href=&quot;https://qmacro.org/blog/posts/2026/01/01/modules-modularity-and-reuse-in-cds-models-part-1-an-introduction/#wrapping-up&quot;&gt;
      ... (the rest of the blog post content)
1&lt;/a&gt;
    </content>
  </entry>
</feed>
```

Notice that in both RSS and Atom feeds, the item can contain just a short summary, as in the BBC World News RSS example, or the entire content, as in this weblog Atom example - although for brevity, again, the content has been elided.

The Atom structure is very similar to that of RSS; apart from the element names themselves, the main difference is that there's no "envelope" element enclosing everything - the outermost element here is `<feed>`:

```text
feed
  |
  +-- entry
  |
  +-- entry
  |
  +-- ...
```

While the XML representation of RSS is not namespaced, Atom's is - notice the non-qualified (i.e. default) `xmlns` attribute in the opening `<feed>` tag:

```xml
<feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
```

This, formally, is very much an Atom feed. Indeed, if you're curious, you may have opened that URL to see what it is. If you haven't yet, go ahead now :-) <https://www.w3.org/2005/Atom>.

### Examine the Atom Publishing Protocol

Complementing the Atom Syndication Format is a protocol, designed to enable the manipulation of data stored in Atom-formatted resources. This was originally designed for weblog authoring - using tools that spoke the protocol one could create, edit and push posts to remote blogging systems for publication.

Officially called the [Atom Publishing Protocol](https://tools.ietf.org/html/rfc5023), this protocol is alternatively known as AtomPub, or simply APP for short.

AtomPub's Request For Comments (RFC) document ([RFC5023](https://tools.ietf.org/html/rfc5023)) describes a series of standard operations that can be performed on entries in an Atom feed - in other words, operations on XML representations of blog posts that are in the form of `entry` elements.

These operations are for listing (querying) multiple entries, and creating, editing, retrieving & deleting individual entries, and they correspond to the standard HTTP methods (GET, POST, PUT and DELETE).

> And if you're thinking these operations sound familiar, you'd be on exactly the right track - the same five operations are defined in OData.

The Atom Publishing Protocol specification also includes the concept of a service document that describes what collections of entries that are available. Here's an example of an Atom service document, from that same RFC document:

```xml
<?xml version="1.0" encoding="utf-8"?>
<service
  xmlns="http://www.w3.org/2007/app"
  xmlns:atom="http://www.w3.org/2005/Atom">
  <workspace>
    <atom:title>Main Site</atom:title>
    <collection href="http://example.org/blog/main">
      <atom:title>My Blog Entries</atom:title>
      <categories href="http://example.com/cats/forMain.cats" />
    </collection>
    <collection href="http://example.org/blog/pic">
      <atom:title>Pictures</atom:title>
      <accept>image/png</accept>
      <accept>image/jpeg</accept>
      <accept>image/gif</accept>
    </collection>
  </workspace>
</service>
```

You will see that these fundamental building blocks of Atom are alive and well in the OData protocol today. In fact, this example Atom Publishing Protocol service document ... was taken from an [OData Specification Document](https://docs.oasis-open.org/odata/odata-atom-format/v4.0/cs01/odata-atom-format-v4.0-cs01.html#_Toc365464533).

### Consider the basics of OData

The ideas in Atom formed the foundation of OData. The [Wikipedia page](https://en.wikipedia.org/wiki/Open_Data_Protocol) has a good overview, but at a simple level, there is:

- a service document describing the data available in a given OData service
- the concept of entity sets and entities, which are direct parallels of feeds and entries, respectively, in Atom
- a basic set of operations: Create, Read, Update, Delete and Query (commonly referred to as CRUD+Q)

Since [OData has been under the stewardship of OASIS](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata), it has [moved through a few iterations](https://github.com/qmacro/odata-specs/blob/master/overview.md), the most current major version of which is 4.

### Look at the public Northwind service

There is a [publicly available set of OData services](https://services.odata.org) maintained by the OASIS organisation. They are known as the **Northwind** services because they offer a data set based on a business scenario that revolves around a company called **Northwind Traders**. This data set contains entities such as customers, products and suppliers.

### Retrieve the Northwind service's service document

Head to the OData V4 flavor of the Northwind service at <https://services.odata.org/V4/Northwind/Northwind.svc/>.

As is standard, the resource at an OData service's root URL is the service document, and it looks like this (some collections have been left out, for brevity):

```xml
<?xml version="1.0" encoding="utf-8"?>
<service
  xmlns="http://www.w3.org/2007/app"
  xmlns:atom="http://www.w3.org/2005/Atom"
  xmlns:m="http://docs.oasis-open.org/odata/ns/metadata"
  xml:base="https://services.odata.org/V4/Northwind/Northwind.svc/"
  m:context="https://services.odata.org/V4/Northwind/Northwind.svc/$metadata">
  <workspace>
    <atom:title type="text">Default</atom:title>
    <collection href="Categories">
      <atom:title type="text">Categories</atom:title>
    </collection>
    <collection href="Products">
      <atom:title type="text">Products</atom:title>
    </collection>
    <collection href="Suppliers">
      <atom:title type="text">Suppliers</atom:title>
    </collection>
    <collection href="Territories">
      <atom:title type="text">Territories</atom:title>
    </collection>
    <collection href="Alphabetical_list_of_products">
      <atom:title type="text">Alphabetical_list_of_products</atom:title>
    </collection>
  </workspace>
</service>
```

Compare this to the Atom service document earlier - from a structural point of view it's almost identical. From a content perspective, while the previous Atom service document example described blog content, this OData service document describes business entities such as products and suppliers.

### Retrieve entity data

The business entities are available from a resource address perspective by appending the name, such as `Suppliers`, onto the service's root URL. Go ahead and do this, to retrieve data for a handful of suppliers, by following this link: <https://services.odata.org/V4/Northwind/Northwind.svc/Suppliers?$top=3>.

Technically speaking in OData terms, by following this link you are performing an OData query operation on the `Suppliers` entity set, limiting the results to 3 via the `$top` system query option. We'll dig into these terms and much more in subsequent tutorials, but for now, take a look at what is returned, which is something like this:

```json
{
  "@odata.context": "https://services.odata.org/V4/Northwind/Northwind.svc/$metadata#Suppliers",
  "value": [
    {
      "SupplierID": 1,
      "CompanyName": "Exotic Liquids",
      "ContactName": "Charlotte Cooper",
      "ContactTitle": "Purchasing Manager",
      "Address": "49 Gilbert St.",
      "City": "London",
      "Region": null,
      "PostalCode": "EC1 4SD",
      "Country": "UK",
      "Phone": "(171) 555-2222",
      "Fax": null,
      "HomePage": null
    },
    {
      "SupplierID": 2,
      "CompanyName": "New Orleans Cajun Delights",
      "ContactName": "Shelley Burke",
      "ContactTitle": "Order Administrator",
      "Address": "P.O. Box 78934",
      "City": "New Orleans",
      "Region": "LA",
      "PostalCode": "70117",
      "Country": "USA",
      "Phone": "(100) 555-4822",
      "Fax": null,
      "HomePage": "#CAJUN.HTM#"
    },
    {
      "SupplierID": 3,
      "CompanyName": "Grandma Kelly's Homestead",
      "ContactName": "Regina Murphy",
      "ContactTitle": "Sales Representative",
      "Address": "707 Oxford Rd.",
      "City": "Ann Arbor",
      "Region": "MI",
      "PostalCode": "48104",
      "Country": "USA",
      "Phone": "(313) 555-5735",
      "Fax": "(313) 555-3349",
      "HomePage": null
    }
  ]
}
```

These days, particularly with OData V4, the default representation of entity set resources is JSON. With earlier versions of OData that default was XML ... specifically Atom flavored XML! In fact, some OData servers still support the Atom flavored XML, which we can explicitly ask for using HTTP's `Accept` header in the request.

Try this now, if you have access to an HTTP client, such as `curl` on the command line, or [Postman](https://web.postman.co/) on the Web.

If you want to use `curl`, try this:

```shell
curl \
  --header 'Accept: application/atom+xml' \
  --url 'https://services.odata.org/V4/Northwind/Northwind.svc/Suppliers?$top=3'
```

If you're using Postman or similar, be sure to add the `Accept` header with the value `application/atom+xml`.

This should return the same supplier data resource, but in a different representation - Atom XML:

```xml
<?xml version="1.0" encoding="utf-8"?>
<feed
  xmlns="http://www.w3.org/2005/Atom"
  xmlns:d="http://docs.oasis-open.org/odata/ns/data"
  xmlns:m="http://docs.oasis-open.org/odata/ns/metadata"
  xmlns:georss="http://www.georss.org/georss"
  xmlns:gml="http://www.opengis.net/gml"
  xml:base="https://services.odata.org/V4/Northwind/Northwind.svc/"
  m:context="https://services.odata.org/V4/Northwind/Northwind.svc/$metadata#Suppliers">
  <id>https://services.odata.org/V4/Northwind/Northwind.svc/Suppliers</id>
  <title type="text">Suppliers</title>
  <updated>2026-01-20T15:32:43Z</updated>
  <link rel="self" title="Suppliers" href="Suppliers"/>
  <entry>
    <id>https://services.odata.org/V4/Northwind/Northwind.svc/Suppliers(1)</id>
    <category term="#NorthwindModel.Supplier" scheme="http://docs.oasis-open.org/odata/ns/scheme"/>
    <link rel="edit" title="Supplier" href="Suppliers(1)"/>
    <link rel="http://docs.oasis-open.org/odata/ns/related/Products" type="application/atom+xml;type=feed" title="Products" href="Suppliers(1)/Products"/>
    <title/>
    <updated>2026-01-20T15:32:43Z</updated>
    <author>
      <name/>
    </author>
    <content type="application/xml">
      <m:properties>
        <d:SupplierID m:type="Int32">1</d:SupplierID>
        <d:CompanyName>Exotic Liquids</d:CompanyName>
        <d:ContactName>Charlotte Cooper</d:ContactName>
        <d:ContactTitle>Purchasing Manager</d:ContactTitle>
        <d:Address>49 Gilbert St.</d:Address>
        <d:City>London</d:City>
        <d:Region m:null="true"/>
        <d:PostalCode>EC1 4SD</d:PostalCode>
        <d:Country>UK</d:Country>
        <d:Phone>(171) 555-2222</d:Phone>
        <d:Fax m:null="true"/>
        <d:HomePage m:null="true"/>
      </m:properties>
    </content>
  </entry>
  <entry>
    <id>https://services.odata.org/V4/Northwind/Northwind.svc/Suppliers(2)</id>
    <category term="#NorthwindModel.Supplier" scheme="http://docs.oasis-open.org/odata/ns/scheme"/>
    <link rel="edit" title="Supplier" href="Suppliers(2)"/>
    <link rel="http://docs.oasis-open.org/odata/ns/related/Products" type="application/atom+xml;type=feed" title="Products" href="Suppliers(2)/Products"/>
    <title/>
    <updated>2026-01-20T15:32:43Z</updated>
    <author>
      <name/>
    </author>
    <content type="application/xml">
      <m:properties>
        <d:SupplierID m:type="Int32">2</d:SupplierID>
        <d:CompanyName>New Orleans Cajun Delights</d:CompanyName>
        <d:ContactName>Shelley Burke</d:ContactName>
        <d:ContactTitle>Order Administrator</d:ContactTitle>
        <d:Address>P.O. Box 78934</d:Address>
        <d:City>New Orleans</d:City>
        <d:Region>LA</d:Region>
        <d:PostalCode>70117</d:PostalCode>
        <d:Country>USA</d:Country>
        <d:Phone>(100) 555-4822</d:Phone>
        <d:Fax m:null="true"/>
        <d:HomePage>#CAJUN.HTM#</d:HomePage>
      </m:properties>
    </content>
  </entry>
  <entry>
    <id>https://services.odata.org/V4/Northwind/Northwind.svc/Suppliers(3)</id>
    <category term="#NorthwindModel.Supplier" scheme="http://docs.oasis-open.org/odata/ns/scheme"/>
    <link rel="edit" title="Supplier" href="Suppliers(3)"/>
    <link rel="http://docs.oasis-open.org/odata/ns/related/Products" type="application/atom+xml;type=feed" title="Products" href="Suppliers(3)/Products"/>
    <title/>
    <updated>2026-01-20T15:32:43Z</updated>
    <author>
      <name/>
    </author>
    <content type="application/xml">
      <m:properties>
        <d:SupplierID m:type="Int32">3</d:SupplierID>
        <d:CompanyName>Grandma Kelly's Homestead</d:CompanyName>
        <d:ContactName>Regina Murphy</d:ContactName>
        <d:ContactTitle>Sales Representative</d:ContactTitle>
        <d:Address>707 Oxford Rd.</d:Address>
        <d:City>Ann Arbor</d:City>
        <d:Region>MI</d:Region>
        <d:PostalCode>48104</d:PostalCode>
        <d:Country>USA</d:Country>
        <d:Phone>(313) 555-5735</d:Phone>
        <d:Fax>(313) 555-3349</d:Fax>
        <d:HomePage m:null="true"/>
      </m:properties>
    </content>
  </entry>
</feed>
```

Does this format and structure look familiar? Yes, of course it does - it's the same as the Atom Syndication Format example from earlier - a `<feed>` element containing `<entry>` elements, each with a `<content>` element where the actual item's data is to be found.

Indeed, the set of formal documents that form the complete OData specification includes the [OData Atom Format Version 4.0](https://docs.oasis-open.org/odata/odata-atom-format/v4.0/odata-atom-format-v4.0.html), which is currently at Committee Specification level 02, the introduction to which reads:

> "The OData protocol is comprised of a set of specifications for representing and interacting with structured content. The core specification for the protocol is in OData-Protocol. The OData Atom Format specification extends the former by defining representations for OData requests and responses using an Atom format."

The use of "OData-Protocol" is a reference to a related document in the OData V4 specification set, specifically the [OData Version 4.0 Part 1: Protocol](https://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html) specification. In the next tutorial you'll learn more about the protocol specification and how to navigate it.

### Further info

- [Accuracy and precision in language](https://qmacro.org/blog/posts/2024/01/22/accuracy-and-precision-in-language/) on the distinction between "blog" and "post".
- [Monday morning thoughts: OData](https://qmacro.org/blog/posts/2018/08/20/monday-morning-thoughts-odata/) has further details on the journey from RSS, through Atom and AtomPub, to OData.

