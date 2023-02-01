---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>javascript]
primary_tag: topic>user-interface
---

# Add Localization to Luigi React Application
<!-- description --> Enable your main application to be displayed in multiple languages using the Luigi localization features.

## You will learn
  - How to add localization to the part of your application developed with React

---


### Configure localization in Luigi navigation


 In this step, you will update the Luigi configuration with the labels for the German language localization that will be implemented in later steps.

1. Open `react-core-mf/public/luigi-config.js`.

2. Change the `label` attribute of the `products` and `order` node as shown below:

    ```JavaScript
    children: [
            {
                pathSegment: "products",
                //HERE
                label: "PRODUCTS",
                icon: "product",
                viewUrl: "/sampleapp.html#/products",
                keepSelectedForChildren: true,
                children: [{
                    pathSegment: ':id',
                    viewUrl: '/sampleapp.html#/productDetail/:id',
                    context: { id: ':id' }
                }]
            },
            {
                pathSegment: 'order',
                //AND HERE
                label: 'ORDERHISTORY',
                icon: 'history',
                viewUrl: 'http://localhost:8080/index.html'
            }
        ],
    ```

3. Add following at around line 111:

    ```JavaScript
    var dict = {
      "en-US": { PRODUCTS: "Products", ORDERHISTORY: "Order History" },
      //THIS LINE HERE HAS TO BE ADDED
      "de-DE": { PRODUCTS: "Produkte", ORDERHISTORY: "Bestellungen" },
    };
    ```


### Add second language in `language.js`


In this step, you will add text for your React app in multiple languages by creating a "dictionary" file.

In the previously created `language.js` file in `react-core-mf/src`, replace the content with:

```JavaScript
export const dict = {
            'de-DE': {
                ITEMS: 'Produkte',
                STOCKS: 'Bestand',
                SELECTLANGUAGE: 'Bitte wählen Sie eine Sprache',
                PRICE: 'Preis',
                WELCOME_LUIGI: 'Willkommen bei Luigi - einem Micro-Frontend Framework',
                DESCRIPTION: 'Beschreibung',
                PRODUCTADDED: 'Das Produkt wurde hinzugefügt',
                AVAILABLE: 'Verfügbar',
                AVAILABLEQUANT: 'Verfügbare Anzahl: ',
                ADDTOCART: 'Zu Einkaufswagen hinzufügen',
                BACK: 'Zurück',
                OUTOFSTOCK: 'Nicht auf Lager'
            },
            "en-US": {
                ITEMS: "Products",
                STOCKS: "Stocks",
                SELECTLANGUAGE: "Please select a language",
                PRICE: "Price",
                WELCOME_LUIGI: "Welcome to Luigi - a micro-frontend framework",
                DESCRIPTION: "Description",
                PRODUCTADDED: "Product has been added to cart",
                AVAILABLE: "Available",
                AVAILABLEQUANT: "Available quantity: ",
                ADDTOCART: "Add to cart",
                BACK: "Back",
                OUTOFSTOCK: "Out of stock",
            },
        };
```


### Configure localization in React app


1. In the file `react-core-mf/src/views/home.js`, around line 20, replace the `this.options` variable with the following code:

    ```JavaScript
    this.options = [{ key: 'en-US', text: 'en-US' }, { key: 'de-DE', text: 'de-DE' }];
    ```






---
