#  Appendix C 
#  CLOS Operators Not Documented in This Book

This appendix briefly mentions the CLOS operators that are not covered in this book.

### Generic Functions with Local Names or No Names

CLOS enables you to define generic functions whose names are local in much the same way that Common Lisp enables you to define ordinary functions whose names are local. You can also define a generic function with no name; this is analogous to defining an ordinary Lisp function with no name.

**generic-flet special form**

		Defines new generic functions and methods; the scoping is like flet.

**generic-labels special form** 

		Defines new generic functions and methods; the scoping is like labels.

**with-added-methods special form** 

		Defines new generic functions and methods; the names are scoped within the lexical context
		of the body. This is an extension to generic-labels.

**generic-function macro** 

		Defines an anonymous generic function and methods for it.

###   Generic Functions Called in Error Situations 

These generic functions are not intended to be called by users; they are called when errors are encountered. These generic functions are exception handlers. The default method signals an error, but you can specialize the generic function to do something different.

**slot-unbound generic function** 

		Called when an attempt is made to read an unbound slot.

**slot-missing generic function** 

		Called when an attempt is made to access a slot of an instance, but there is no slot by
		that name accessible to the instance.

**no-applicable-method generic function** 

		Called when a generic function is called and there is no applicable method for it.

**no-next-method generic function**

		Called when call-next-method is used and there is no "next method."

###  Tools for Defining Method Combination Types

The long form of define-method-combination offers a rich syntax for defining new method combination types. The other operators mentioned here are used within the body of define-method-combination.

**define-method-combination macro** 

		Defines a new method combination type. 

**call-method macro** 

		In the framework of a method combination type, indicates that a method should be called.

**method-qualifiers generic function** 

		Returns a list of the qualifiers of a method. 

**method-combination-error function** 

		Signals an error encountered in the method combination process.

**invalid-method-error function** 

		Signals an error when an applicable method has method qualifiers that are not recognized
		by the method combination type.

### Miscellaneous Operators

**add-method generic function** 

		Adds a method object to a generic function; this function level operator implements
		defmethod and other macros that create methods for generic functions.

**documentation generic function** 

		Retrieves the documentation string of various kinds of Lisp objects.

**ensure-generic-function function** 

		Defines a generic function object; this function level operator implements defgeneric and
		other macros that create generic functions.

**function-keywords** 

		Returns the keyword parameters of a given method.

**make-instances-obsolete generic function** 

		Called by the system when a class is redefined to trigger the updating process. Users can
		call make-instances-obsolete to cause the update-instance-for-redefined-class generic
		function to be called for instances of a given class (and for instances of subclasses).

**slot-makunbound function** 

		Makes a slot of an instance unbound.

**slot-exists-p function** 

		Tests whether an instance has a slot of a given name. 

**symbol-macrolet macro** 

		Associates forms with variables within its body; using such a variable causes the form to
		be executed. This macro implements the with-accessors and with-slots macros.