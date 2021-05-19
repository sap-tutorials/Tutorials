---
title: Add Localization to Luigi React Application
description: Enable your main application to be displayed in multiple languages using the Luigi localization features.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>javascript]
primary_tag: topic>user-interface
---

## Details
### You will learn
  - How to add localization to the part of your application developed with React

---


[ACCORDION-BEGIN [Step 1: ](Configure localization in Luigi navigation)]

 In this step, you will create a translation provider function in order to be able to display the Luigi left-side navigation in different languages. You will also use Luigi [lifecycle hooks](https://docs.luigi-project.io/docs/lifecycle-hooks) to set the default language, and [custom messages](https://docs.luigi-project.io/docs/communication) to communicate between Luigi Core and the micro-frontends.

1. Open the `luigi-config.js` file. Change the label `products` to `PRODUCTS`, and `Order History` to `ORDERHISTORY`.

2. At line 56, add a translation provider function:

    ```JavaScript
    var defaultLocale = 'en-US';
    function myTranslationProvider() {
     var dict = {
       'de-DE': { PRODUCTS: 'Produkte', 'ORDERHISTORY': 'Bestellungen' },
       'en-US': { PRODUCTS: 'Products', 'ORDERHISTORY': 'Order History' }
     };
     return {
       getTranslation: function (label, interpolation, locale) {
         return dict[locale || Luigi.i18n().getCurrentLocale() || defaultLocale][label] || label;
       }
     }
    };
    ```

3. Update the `settings:` section and use Luigi lifecycle hooks and a custom message to set the language:

    ```JavaScript
    settings: {
      header: {
        title: 'Luigi Application',
        logo: '/logo.png'
      },
      responsiveNavigation: 'simpleMobileOnly',
      customTranslationImplementation: myTranslationProvider
    },
    lifecycleHooks: {
      luigiAfterInit: () => {
        Luigi.i18n().setCurrentLocale(defaultLocale);
      }
    },
    communication: {
      customMessagesListeners: {
        'set-language': (msg) => {
          Luigi.i18n().setCurrentLocale(msg.locale);
        }
      }
    }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add text in multiple languages)]

In this step, you will add text for your React app in multiple languages by creating a "dictionary" file.

Create a new file called `language.js` in `react-core-mf/src` with the following content:

```JavaScript
export const dict = {
  'de-DE': {
        ITEMS: 'Produkte',
        STOCKS: 'Bestand',
        PRICE: 'Preis',
        SELECT_LANGUAGE: 'Sprache auswählen',
        WELCOME_LUIGI: 'Willkommen bei Luigi - einem Micro-Frontend Framework',
        NO_MATCH_PRODUCT: 'Das Produkt ist nicht verfügbar. bitte überprüfen Sie es erneut.',
        NO_AVAILABLE_PRODUCT: 'Derzeit ist leider kein Produkt verfügbar.'
  },
  'en-US': {
        ITEMS: 'Products',
        STOCKS: 'Stocks',
        PRICE: 'Price',
        SELECT_LANGUAGE: 'Select Language',
        WELCOME_LUIGI: 'Welcome to Luigi - a micro-frontend framework',
        NO_MATCH_PRODUCT: 'This product is not available. Please check again.',
        NO_AVAILABLE_PRODUCT: 'Unfortunately, there is no available product at this moment.'
  }
};
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure localization in React app)]

In this step, you will configure the entry point `ìndex.js` of the React file to work with Luigi localization. You will use [Luigi Client API](https://docs.luigi-project.io/docs/luigi-client-api) functions in order to change the language.

1. Go to `react-core-mf/src/index.js` and make the edits indicated below.

2. Import functions from Luigi Client and the dictionary from the file created in the previous step:

    ```JavaScript
    import { addInitListener, addContextUpdateListener, uxManager } from '@luigi-project/client';
    ```

    and:

    ```JavaScript
    import { dict } from './language.js';
    ```

3. Use Luigi [UX Manager](https://docs.luigi-project.io/docs/luigi-client-api?section=uxmanager) functions and update the router:

    ```JavaScript
    class App extends Component {
     constructor(props) {
       super(props);
       this.state = { currentLocale : 'en-US' };

       const updateCurrentLanguage = () =>{
          this.setState({
            currentLocale: uxManager().getCurrentLocale()
          });
       };
       addInitListener(() => {
         console.log('Luigi Client initialized.');
         updateCurrentLanguage();
       });
       addContextUpdateListener(()=>{
          updateCurrentLanguage();
       });
     }
     render() {
       return (
         <Router basename={`/app.html#`}>
           <Route path="/home" render={(props) => <Home {...props} localeDict={dict[this.state.currentLocale]} currentLocale={this.state.currentLocale} />} />
           <Route path="/products" render={(props) => <Products {...props} localeDict={dict[this.state.currentLocale]} />} />
           <Route path="/productDetail/:id" render={(props) => <ProductDetail {...props} localeDict={dict[this.state.currentLocale]} />} />
         </Router>
       );
     }
    }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add language selection dropdown to the homepage)]

In this step, you will add a dropdown to the homepage from which the language can be changed. Then, you will send a custom message to Luigi when a language change occurs.

Go to `react-core-mf/src/views/Home.jsx` and replace the content with:

```JavaScript
import React, { useState } from 'react';
import { LayoutPanel, Select } from 'fundamental-react';
import LuigiClient from '@luigi-project/client';

export const Home = ({ localeDict, currentLocale }) => {
  const [locale, setLocale] = useState(currentLocale);
  const options = [{ key:'en-US', text: 'en-US'}, { key:'de-DE', text: 'de-DE'}];

  const onChangeValue = (event) => {
    setLocale(event.target.textContent);
    LuigiClient.sendCustomMessage({id: 'set-language', 'locale': event.target.textContent})
  }

  return (
    <LayoutPanel>
      <LayoutPanel.Body>
        <h2>{ localeDict.WELCOME_LUIGI }</h2>
        <br/>
        <span style={{marginRight: "12px"}}>{ localeDict.SELECT_LANGUAGE }</span>
        <Select
          options={options}
          placeholder={ localeDict.SELECT_LANGUAGE }
          onSelect={(e) => onChangeValue(e)}
          selectedKey={locale}
        />
      </LayoutPanel.Body>
    </LayoutPanel>
  );
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure localization inside React micro-frontends)]

This is a development step strictly related to React, and it involves passing the "dictionary" object to all the React views.

1. Open `react-core-mf/src/views/List.jsx` and make the edits indicated below.

2. Bring the `localeDict` object into the `List` component:

    ```JavaScript
    export const List = ({ items, localeDict }) => (
       (items.length === 0) ? <MessageStrip type='error'>{ localeDict.NO_AVAILABLE_PRODUCT }</MessageStrip>
    ```

3. Replace `Price` with `{ localeDict.PRICE }` and `Stocks` with `{ localeDict.STOCKS }`.

4. Open `react-core-mf/src/views/Products.jsx` and make the edits indicated below.

5. Bring the `localeDict` object into the `Products` component:

    ```JavaScript
    export​ ​const​ ​Products​ = ({ ​localeDict​ }) ​=>​ (
    ```

6. Replace `items` with `{ localeDict.ITEMS}`.

7. Pass `localeDict` to the List child Component:

    ```JavaScript
    <List items={ProductCollection} localeDict={localeDict} />
    ```

8. Open `react-core-mf/src/views/ProductDetail.jsx` and bring the `localeDict` object into the `ProductDetail` component:

    ```JavaScript
    ​const​ ​renderContent​ = () ​=>​ {
      if​ (​item​ === ​undefined​) {
    ​    return​ ​<​MessageStrip​ t​ ype​=​'error'>​ ​{​ ​localeDict​.​NO_MATCH_PRODUCT }​</​MessageStrip​>​;
      }
    ​   return​ ​item​ ? ​renderDetails​() : ​null​;
    }
    ​return​ (
      ​<​section​ ​className​=​"fd-section"​>
    ​   {​ ​renderContent​() ​} ​
      </​section​>
    );
    ```

[VALIDATE_1]
[ACCORDION-END]




---
