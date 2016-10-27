---
title: React JS - Create children components in React
description: Step #8: Break apart the large main componnets, moving functionality to child components
tags: [  tutorial>beginner, topic>html5, topic>mobile, topic>odata, products>sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner 
 - **Tutorial:** [Step 7 - Add a header and a detail modal dialog](http://go.sap.com/developer/tutorials/react-add-header-detail-dialog.html)

## Next Steps
 - **Tutorial:** [Step 9 - Examine child to parent communication](http://go.sap.com/developer/tutorials/react-add-filter.html)


## Details
### You will learn  
In this tutorial series, we will explore another technology for Single Page Application (SPA) development - React.  React is another popular web framework, and is used by many companies for both internal and client-facing systems.  These tutorials will parallel our SAPUI5 tutorials, building a visual interface using Angular, and connecting it to an OData back end service.

### Time to Complete
**30 Min**.

---

#### REACT tutorial series
**Step 8**: Up to this point, the tutorial has focused on adding functionality to the application.  The components on the page, however, have grown very large and have many moving parts.  REACT is designed to segment the components in to smaller "child" components.  This is a very object oriented concept, where each component is compartmentalized.  This makes maintaining each component easier and debugging faster.

This tutorial will focus on creating smaller child components.  The focus is on components that do not render straight text, instead, they do some sort of translation on the data.  

Here are the detailed steps:

1.  Create a currency child component
2.  Create an Available or Discontinued status component
3.  Add a Category display component
 
---

### Create a currency child component

The currency in both the list and the detail is still the raw data coming from the server.  Correctly formatted, it would be both locale specific, and also rounded to two decimal places.

1.  Start by adding a new currency component to the JavaScript file.  

    Open the `main.js` file, and add the following code at the top, directly underneath the variable declarations:
    
    ```javascript
    var CurrencyDisplay = React.createClass({
    	render: function() {
    		var computedPrice = new Number( this.props.value );
    		computedPrice = Math.round( computedPrice * 100 )/100;
    		computedPrice = computedPrice.toFixed(2).toLocaleString();
    		
    		return(
    			<span>
    				{computedPrice}
    				<small className="text-muted"> EUR</small>
    			</span>
    		)
    	}
    })
    ```


2.  Add the new `CurrencyDisplay` item to the list.  Scroll down to the `ListBox` element, and replace the section for currency with this new JavaScript:

    ```javascript
    <CurrencyDisplay value={this.props.row.UnitPrice} />
    ```



3.  The currency is also displayed in the modal box.  Scroll up to the `ModalProductDetail`, and in the `render:` function replace the `<FormGroup>` for the unit price with this new code:

    ```javascript
    <Static className="col-sm-9">
    	<CurrencyDisplay value={this.props.row.UnitPrice} />
    </Static>
    ``` 

4.  Run your application.  The currency will now be displayed with two decimal points in both the  list and the dialog box:



---

### Create an Available or Discontinued status component

The *Discontinued* property is just a boolean, but on screen it should be displayed as a word.  The original display also had formatting (using a CSS style) which can be added back as well.

1.  Add the new component to the top of your JavaScript file.  Scroll to the top, and add this code just under the variable declarations:

    ```javascript
    var AvailableDisplay = React.createClass({
    	render: function() {
    		return(
    			<span className={this.props.value ? "discontinued" : "available"}>
    				{this.props.value ? "Discontinued" : "Available"}
    			</span>
    		)
    	}
    });
    ```

2.  Change the display in the list box.  Replace the code for the *Discontinued* value with the following new JMX:

    ```javascript
    <AvailableDisplay value={this.props.row.Discontinued} />
    ```


3.  Change the display in the modal box, as well.  Replace the code inside the *Discontinued* `<FormGroup>` with the following new JMX:

    ```javascript
    <Static className="col-sm-9">
    	<AvailableDisplay value={this.props.row.Discontinued} />
    </Static>
    ```


4.  Run the application.  The list will show the Available/Discontinued tag with the CSS formatting.



---

### Add a Category display component

The category display, in the modal detail, only displays a number.  This number is the primary key to the actual Category object, which is a separate OData object.  Using encapsulation, the category primary key can be resolved in to a name, using an OData call.

In this case, we will use the properties of the component to store the Categories that have already been loaded, so that there is only one single call for each primary key.

1.  Create the new `CategoryDisplay` component by adding the following component to the top of your JavaScript file (under the variable declarations):

    ```javascript
    var CategoryDisplay = React.createClass({
    	getInitialState: function() {
    		return {
    			categoryCache: [],
    			loaded: false,
    		};
    	},
    	componentDidMount: function() {
    		var odataUrl = "/Northwind/V3/Northwind/Northwind.svc/";
    		
    		$.ajax({
    			url: odataUrl + "Categories" ,
    			dataType: 'json',
    			cache: false
    		})
    		.done( function( data, textStatus, jqXHR ) {
    				this.setState( {
    					categoryCache: data.value,
    					loaded: true,
    				} );
    			}.bind(this)
    		)
    		.fail( function( jqXHR, textStatus, errorThrown ) {
    			console.log("Error", jqXHR, textStatus, errorThrown );
    			alert( "An error occurred while retrieving the category list from the server: " + textStatus );
    		});
    	},
    	findRow: function( id ) {
    		var result = $.grep(this.state.categoryCache, function(e){ return e.CategoryID == id; });
    		if ( result.length > 0 ) return result[0].CategoryName;
    		else return null;
    	},
    	render: function() {
    		return(
    			<span>
    				{ this.state.loaded ? this.findRow( this.props.id ) : "...loading..." }
    			</span>
    		)
    	}
    })
    ```

2.  In the `ModalProductDetail` component, replace the `<Static>` label for the *Category ID* with this new JMX code:

    ```javascript
    <CategoryDisplay id={this.props.row.CategoryID} />    
    ```


3.  Run your application.  When you click on a row, and the detail modal opens, you will see the Category display refresh.   (It starts by showing "...loading...", and then changes when the data is loaded.)



##Final Code

The `main.js` file is the only one modified.  It has become quite complex.  Here is the final copy of that file:

```javascript
var FormGroup = ReactBootstrap.FormGroup;
var Label = ReactBootstrap.ControlLabel;
var Input = ReactBootstrap.FormControl;
var Static = ReactBootstrap.FormControl.Static;
var Button = ReactBootstrap.Button;

var CategoryDisplay = React.createClass({
	getInitialState: function() {
		return {
			categoryCache: [],
			loaded: false,
		};
	},
	componentDidMount: function() {
		var odataUrl = "/Northwind/V3/Northwind/Northwind.svc/";
		
		$.ajax({
			url: odataUrl + "Categories" ,
			dataType: 'json',
			cache: false
		})
		.done( function( data, textStatus, jqXHR ) {
				this.setState( {
					categoryCache: data.value,
					loaded: true,
				} );
			}.bind(this)
		)
		.fail( function( jqXHR, textStatus, errorThrown ) {
			console.log("Error", jqXHR, textStatus, errorThrown );
			alert( "An error occurred while retrieving the category list from the server: " + textStatus );
		});
	},
	findRow: function( id ) {
		var result = $.grep(this.state.categoryCache, function(e){ return e.CategoryID == id; });
		if ( result.length > 0 ) return result[0].CategoryName;
		else return null;
	},
	render: function() {
		return(
			<span>
				{ this.state.loaded ? this.findRow( this.props.id ) : "...loading..." }
			</span>
		)
	}
})

var AvailableDisplay = React.createClass({
	render: function() {
		return(
			<span className={this.props.value ? "discontinued" : "available"}>
				{this.props.value ? "Discontinued" : "Available"}
			</span>
		)
	}
});

var CurrencyDisplay = React.createClass({
	render: function() {
		var computedPrice = new Number( this.props.value );
		computedPrice = Math.round( computedPrice * 100 )/100;
		computedPrice = computedPrice.toFixed(2).toLocaleString();
		
		return(
			<span>
				{computedPrice}
				<small className="text-muted"> EUR</small>
			</span>
		)
	}
});

var ModalProductDetail = React.createClass({
	getInitialState: function() {
		return {
			OrderCount: "",
			orderSubmitted: false,
		}	
	},
	onOrderChange: function(event) {
		this.setState({OrderCount: event.target.value});
	},
	onSaveClick: function() {
		this.setState({
			orderSubmitted: true,
		});
		
		setTimeout( this.closeAndReset, 2000 );
	},
	closeAndReset: function() {
		$('#product-detail').modal('hide');
		this.setState({
			orderSubmitted: false,
			OrderCount: "",
		});
	},
	render: function() {
		return (
			<div className="modal fade" tabIndex="-1" role="dialog" id="product-detail">
				<div className="modal-dialog modal-lg" role="document">
					<div className="modal-content">
						<div className="modal-header">
							<h4 className="modal-title">Product Details - {this.props.row.ProductName}</h4>
						</div>
						<div className="modal-body">
							{ this.state.orderSubmitted ? <div className="alert alert-success" role="alert">
								<strong>Success!</strong>&nbsp;
								{this.state.OrderCount} units of {this.props.row.ProductName} have been ordered.
							</div> : null }
							<form className="form-horizontal">
								<FormGroup>
									<Label className="col-sm-3">Product Name</Label>
									<Static className="col-sm-9">{this.props.row.ProductName}</Static>
								</FormGroup>
								<FormGroup>
									<Label className="col-sm-3">Unit Price</Label>
									<Static className="col-sm-9">
										<CurrencyDisplay value={this.props.row.UnitPrice} />
									</Static>
								</FormGroup>
								<FormGroup>
									<Label className="col-sm-3">Status</Label>
									<Static className="col-sm-9">
										<AvailableDisplay value={this.props.row.Discontinued} />
									</Static>
								</FormGroup>
								<FormGroup>
									<Label className="col-sm-3">Category</Label>
									<Static className="col-sm-9">
										<CategoryDisplay id={this.props.row.CategoryID} />
									</Static>
								</FormGroup>
								<FormGroup>
									<Label className="col-sm-3">Units In Stock</Label>
									<Static className="col-sm-9">{this.props.row.UnitsInStock}</Static>
								</FormGroup>
								<FormGroup>
									<Label className="col-sm-3">Order Amount</Label>
									<div className="col-sm-6">
										<Input type="number"
												value={this.state.OrderCount}
												placeholder="Number of units to order"
												onChange={this.onOrderChange}
												disabled={this.state.orderSubmitted}/>
									</div>
								</FormGroup>
							</form>
						</div>
						<div className="modal-footer">
							<Button bsStyle="primary" onClick={this.onSaveClick} 
								disabled={this.state.OrderCount == "" || this.state.orderSubmitted}>Save</Button>
							<Button onClick={this.closeAndReset}>Close</Button>
						</div>
					</div>
				</div>
			</div>
		)
	}
});

var TitleBar = React.createClass({
	render: function() {
		return (
			<nav className="navbar navbar-default navbar-fixed-top">
				<div className="container">
					<div className="navbar-header">
						<div className="navbar-brand">Product Overview</div>
					</div>
					
					<div className="nav navbar-nav navbar-right">
						<p className="navbar-text">Items: {this.props.count}</p>
					</div>
				</div>
			</nav>	
		)	
	}
});

var ListBox = React.createClass({
    render: function() {
        return (
			<button type="button" className="list-group-item" id="product-list" 
					onClick={this.props.clickEvent} >
				<div className="row vertical-align">
					<div className="col-sm-8 top">
						<h4>{this.props.row.ProductName}</h4>
						<p> {this.props.row.QuantityPerUnit}</p>
					</div>
					<div className="col-sm-3 text-right top">
						<h4>
							<CurrencyDisplay value={this.props.row.UnitPrice} />
						</h4>
						<AvailableDisplay value={this.props.row.Discontinued} />
					</div>
					<div className="col-sm-1 center">
						<span className="glyphicon glyphicon-chevron-right pull-right" aria-hidden="true"></span>
					</div>
				</div>
			</button>
		);
    }
});

var ProductList = React.createClass({
	getInitialState: function() {
		return {
			products: [],
			selectedRow: null,
		};
	},

	onListBoxClick: function( row ) {
		this.setState({selectedRow: row});
		
		setTimeout( function() {
			$('#product-detail').modal("show");	
		});
	},
	
	componentDidMount: function() {
		var odataUrl = "/Northwind/V3/Northwind/Northwind.svc/";
		
		$.ajax({
			url: odataUrl + "Products" ,
			dataType: 'json',
			cache: false
		})
		.done( function( data, textStatus, jqXHR ) {
				this.setState( {products: data.value } )
			}.bind(this)
		)
		.fail( function( jqXHR, textStatus, errorThrown ) {
			console.log("Error", jqXHR, textStatus, errorThrown );
			alert( "An error occurred while retrieving data from the server: " + textStatus );
		});
	},
  
	render: function() {
		var productListBoxes = this.state.products.map( function(row) {
			return(
				<ListBox row={row} key={row.ProductName} 
					clickEvent={this.onListBoxClick.bind(this, row)}/>		
			);
		}.bind(this) )

		return (
			<div>
				<TitleBar count={this.state.products.length}/>
				<div className="list-group">
					{productListBoxes}
				</div>
				{this.state.selectedRow == null ? null : 
					<ModalProductDetail row={this.state.selectedRow} /> }
			</div>
		)
	}	
});

ReactDOM.render(
    <ProductList />,
    document.getElementById('product-list')
);
```

## Next Steps
 - **Tutorial:** [Step 9 - Examine child to parent communication](http://go.sap.com/developer/tutorials/react-add-filter.html)
