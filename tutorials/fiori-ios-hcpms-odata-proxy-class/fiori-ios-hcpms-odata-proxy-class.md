---
title: Connecting to OData service (Proxy class)
description: Connecting to OData service using a generated / hand-built proxy class
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, topic>mobile, operating_system>ios, products>sap-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Development machine:** Access to a Mac computer

## Next Steps
 - [Logging and Tracing](#)

## Details
### You will learn  
In this tutorial, you will learn how the OData proxy classes are structured.

> **NOTE: Although the proxy classes will be generated using the *SDK Assistant*, and you will *not create these by hand*, this tutorial actually works counter-productively by creating one from scratch.**

### Time to Complete
**15 Min**.

---

**_REMINDER: The contents of this part may change, since it does not involve creating the proxy class using the "SDK Assistant". For now, it is created by hand using the most simple, one-dimensional OData entity possible_.**

The OData proxy class is used to map OData Entity types, Entity sets and Association to Swift objects, object arrays and object-relations, respectively.

> **IMPORTANT:** The SDK Assistant generates the proxy class based on the OData EDMX file. In this tutorial, we go into the details on how the Proxy class is structured, as if we were to build it by hand.

Let's imagine you have an OData service which only returns a single entity `User` which has a single property `Name`. We also assume `Name` is unique, and therefore can be used as the entity key. The Schema namespace is `SAP_DEMO`. The Entity would thus look like this:

| `SAP_DEMO.User` |
|----|
| `Name` (primary key) |

A proxy class for the OData service would have the following three main parts:

1. One or more Entity values, which are mapped to simple object classes:

    ```swift
	open class User: EntityValue {
	    public static let name: Property = DemoServiceMetadata.EntityTypes.user.property(withName: "Name")

	    public init() {
	        super.init(type: DemoServiceMetadata.EntityTypes.user)
	    }

	    open func copy() -> User {
	        return CastRequired<User>.from(self.copyEntity())
	    }

	    open var name: String {
	        get {
	            return StringValue.unwrap(self.dataValue(for: User.name))
	        }
	        set(value) {
	            self.setDataValue(for: User.name, to: StringValue.of(value))
	        }
	    }

	    open class func key(id param_name: String) -> EntityKey {
	        return EntityKey().with(name: "Name", value: StringValue.of(param_name))
	    }
	}
    ```

    The Entity value `User` is a standard Swift object class. The class' properties match the `User` Entity type properties.

    The object class consists of getter and setter for the properties. In addition, it should also contain an `init` event hook, `copy` function for convenience, and `key` function to map the (unique) `Name` to the EntityKey.

2. A service class, which, based on the provided `DataProviderService`, returns a reference to your data:

    ```swift
	open class DemoService<Provider: DataServiceProvider>: DataService<Provider> {
	    public override init(provider: Provider) {
	        super.init(provider: provider)
	        self.provider.metadata = DemoServiceMetadata.document
	    }

	    open func users(query: DataQuery = DataQuery()) throws -> Array<User> {
	        return try ArrayFunction.convert(self.executeQuery(query.from(DemoServiceMetadata.EntitySets.users)).entityList().toArray(), Array<User>())
	    }
	}

    ```

3. A metadata class, which contains the EDMX metadata file as a String, and the definitions of the Entity types and Entity sets available in your service:

    ```swift
	public class DemoServiceMetadata {
	    public static let source: String = "<?xml version=\"1.0\" encoding=\"utf-8\"?><edmx:Edmx ...etc... ></edmx:Edmx>"

	    internal static let parsed: CSDLDocument = DemoServiceMetadata.parse()

	    public static let document: CSDLDocument = DemoServiceMetadata.resolve()

	    static func parse() -> CSDLDocument {
	        let parser: CSDLParser = CSDLParser()
	        parser.logWarnings = false
	        parser.csdlOptions = (CSDLOption.processMixedVersions | CSDLOption.retainOriginalText | CSDLOption.ignoreAllReferences | CSDLOption.ignoreUndefinedTerms)
	        return parser.parseInProxy(csdl: DemoServiceMetadata.source, url: "SAP_DEMO")
	    }

	    static func resolve() -> CSDLDocument {
	        DemoServiceMetadata.EntityTypes.user.registerFactory(ObjectFactory.with(create: { User() }))

	        return DemoServiceMetadata.parsed
	    }

	    public class EntityTypes {
	        public static let user: EntityType = DemoServiceMetadata.parsed.entityType(withName: "SAP_DEMO.User")
	    }

	    public class EntitySets {
	        public static let users: EntitySet = DemoServiceMetadata.parsed.entitySet(withName: "Users")
	    }
	}
    ```

In its entirety, for your most basic OData service with the single User entity, the Proxy file will be like this:

```swift
import HCPOData
import Foundation

open class User: EntityValue {
    public static let name: Property = DemoServiceMetadata.EntityTypes.user.property(withName: "Name")

    public init() {
        super.init(type: DemoServiceMetadata.EntityTypes.user)
    }

    open func copy() -> User {
        return CastRequired<User>.from(self.copyEntity())
    }

    open var name: String {
        get {
            return StringValue.unwrap(self.dataValue(for: User.name))
        }
        set(value) {
            self.setDataValue(for: User.name, to: StringValue.of(value))
        }
    }

    open class func key(id param_name: String) -> EntityKey {
        return EntityKey().with(name: "Name", value: StringValue.of(param_name))
    }
}

open class DemoService<Provider: DataServiceProvider>: DataService<Provider> {
    public override init(provider: Provider) {
        super.init(provider: provider)
        self.provider.metadata = DemoServiceMetadata.document
    }

    open func users(query: DataQuery = DataQuery()) throws -> Array<User> {
        return try ArrayFunction.convert(self.executeQuery(query.from(DemoServiceMetadata.EntitySets.users)).entityList().toArray(), Array<User>())
    }
}

public class DemoServiceMetadata {
    public static let source: String = "<?xml version=\"1.0\" encoding=\"utf-8\"?><edmx:Edmx ...etc... ></edmx:Edmx>"

    internal static let parsed: CSDLDocument = DemoServiceMetadata.parse()

    public static let document: CSDLDocument = DemoServiceMetadata.resolve()

    static func parse() -> CSDLDocument {
        let parser: CSDLParser = CSDLParser()
        parser.logWarnings = false
        parser.csdlOptions = (CSDLOption.processMixedVersions | CSDLOption.retainOriginalText | CSDLOption.ignoreAllReferences | CSDLOption.ignoreUndefinedTerms)
        return parser.parseInProxy(csdl: DemoServiceMetadata.source, url: "SAP_DEMO")
    }

    static func resolve() -> CSDLDocument {
        DemoServiceMetadata.EntityTypes.user.registerFactory(ObjectFactory.with(create: { User() }))

        return DemoServiceMetadata.parsed
    }

    public class EntityTypes {
        public static let user: EntityType = DemoServiceMetadata.parsed.entityType(withName: "SAP_DEMO.User")
    }

    public class EntitySets {
        public static let users: EntitySet = DemoServiceMetadata.parsed.entitySet(withName: "Users")
    }
}
```

Building upon the code you have added in the [Sample OData services](#) tutorial, in the `init` event hook, add the following code below the `let onlineODataProvider = ..etc..` line:

```swift
onlineODataProvider.prettyTracing = true
onlineODataProvider.traceRequests = true
onlineODataProvider.traceWithData = true

self.oDataService = DemoService(provider: onlineODataProvider)
```

This will output additional tracing, as well as reference the `DemoService` class defined in the Proxy class file.

In order to retrieve the `User` Entity set and its  Entity values, simply create a `DataQuery` with the properties to display, and call the `users` function from the reference to the `DemoService` :

```swift
// Select properties
var query = DataQuery().select(User.name)

// Sorting
query = query.orderBy(User.name)

let users = try self.oDataService.users(query: query)
```

## Next Steps
 - [Logging and Tracing](#)
