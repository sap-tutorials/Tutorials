---
title: Finalize your new UI5 Control by specifying the control logic
description: Part 4 of 4. In this tutorial you will write logic and extend the created control in your application.
primary_tag: topic>sapui5
tags: [  tutorial>beginner, topic>html5 ]
time: 15
---
## Prerequisites
  - **Tutorials:** [SAPUI5 Creating Control](https://developers.sap.com/tutorials/sapui5-creating-control.html)
  - You should have basic understanding of HTML, CSS and JavaScript.

## Details
### You will learn
  - How to extend controls in your application. In the below tutorial you will define controllers and it's logic to use it for generating and validating text-based captcha


---

1.   From the previous Tutorial you have created the control template, now you will define the metadata and it's content.

    ```javascript
    metadata : {
      properties: {
        title: {type: "string"},
      },

      aggregations : {
        _gButton : {type: "sap.ui.commons.Button", multiple:false,visibility:"hidden"},
        _vButton : {type: "sap.ui.commons.Button", multiple:false,visibility:"hidden"}
      },

      events : {
        generateCaptcha : {},
        validateCaptcha : {}
      }
    },
    ```
    ![metadata](step-8.png)

    Here properties has just one attribute which is title and it's type is string, aggregations are two buttons with type as button, multiple tells whether it is a 0...1 aggregation or 0...n aggregation and set visibility as hidden. You will require two events, one for generating captcha and other for validating the generated captcha and your input.

2. Now the `init` function. As described in the previous tutorial, this is where your initial control is born.
   You will do the initialization here.

    ```javascript
    init: function(){
     var oControl = this, gButton,vButton;

     gButton = new sap.ui.commons.Button({
       text : "Generate Captcha",
       press : function (evt){
         oControl.fireGenerateCaptcha();
       }
     });

     vButton = new sap.ui.commons.Button({
       text : "Validate Captcha",
       press : function (evt){
         oControl.fireValidateCaptcha();
       }
     });

     this.setAggregation("_gButton",gButton);
     this.setAggregation("_vButton",vButton);
   },
    ```

    ![initialization](step-9.png)

    Here you declare the control object and initialize the buttons the actions to perform whenever and event is triggered. Then you set Aggregations and associate it with the button variables.

3. Then you have to define the renderer which will add the elements to your application.

    ```javascript
    renderer : function(oRm,oControl){

              oRm.write("<div");
              oRm.writeControlData(oControl);
              oRm.write(">");

              oRm.renderControl(oControl.getAggregation("_gButton"));
              oRm.renderControl(oControl.getAggregation("_vButton"));
              oRm.write("</div>");

    }
    ```

    ![renderer](step-10.png)

4. In this step you will define the logic for your functions(events). Firstly you need to edit the Object you created in the previous tutorial by defining the logic.

    ```javascript
      var oo = new extendController({
  		title : "Title",
  		generateCaptcha : function (evt){
  			var alpha = new Array('A','B','C','D','E','F','G',
  			'H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
  			'1','2','3','4','5','6','7','8','9','0','!','@','#','$','%','^','&','*');
		 for(var i=0;i<9;i++)
				 {
				    var a = alpha[Math.floor(Math.random() * alpha.length)];
				    var b = alpha[Math.floor(Math.random() * alpha.length)];
				    var c = alpha[Math.floor(Math.random() * alpha.length)];
				    var d = alpha[Math.floor(Math.random() * alpha.length)];
				 }
		console.log("Captcha Generated");
		var captcha = a + b + c + d;
		document.getElementById("mainCaptcha").value = captcha;
  			},

	      validateCaptcha : function(evt){
  			var string1 = document.getElementById('mainCaptcha').value;
  			var string2 = document.getElementById('textInput').value;
  			if (string1.length != 0 && string1 == string2)
  				{
  				    alert("Matched");
  				}
  			else
  				{        
  				    alert("Try again");
  				}
  			}
  		});

    ```
    ![logic](step-11.png)

    Logic for both buttons i.e `Generate Captcha` and `Validate captcha` are defined above. See to it that the name of **id's** of the elements of body and one's defined in the function are the same.

5. To run the code, right-click on the project file and select run as **Web App Preview**.You should get such output when you run your application.

    ![sapui5_project](step-12.png)

6. Now try out the application you created  by generating and validating captcha.

    ![sapui5_project](step-13.png)

    ![sapui5_project](step-14.png)

---

