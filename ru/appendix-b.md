# Appendix B 
# Syntax of CLOS Operators

This appendix is intended to be used as a reference. It briefly describes the purpose, syntax, and return values of the operators covered by this book, and refers to the sections in the book where the operator is presented in context. The operators are listed alphabetically.

The descriptions of the CLOS operators are adapted from the "Common Lisp Object System Specification" with permission from the authors. This book does not cover every operator in the CLOS programmer interface, and the descriptions given here are not complete reference documentation. Refer to the "Common Lisp Object System Specification" for the complete definition of CLOS.

In Appendix C, we list the operators in the programmer interface that are not covered by this book.

**call-next-method** &rest arguments                                     Function

		Used within a method to call the "next method," which is defined by the method combination
		type in use by the generic function, call-next-method returns the values of the next method.
		If there is no next method, an error is signaled. (The default behavior of signaling an error
		is supported by the no-next-met hod generic function, which is called whenever this error
		is detected.) You can use next-method-p in the body of a method to test whether there is a
		next method. 
		The standard method combination type supports call-next-method in around-methods and primary
		methods. The operator method combination types support call-next-method in around-methods
		only.

		arguments  	     Arguments to be passed to the next method.

		Usually, you call call-next-met hod with no arguments, and the original arguments given to
		the generic function are passed to the next method. However, you can pass different arguments
		to the next method as long as the new arguments would cause the same set of applicable
		methods to be selected, as did the original arguments.

		For related information, see 

		    "Around-Methods," page 102 
		    "Calling a Shadowed Primary Method," page 105 
		    "Summary of the Standard Method Combination Type," page 113 
		    "Controlling the Generic Dispatch," page 101

**change-class** instance new-class                                         Generic Function

		Changes the class of an instance to a new class and calls the generic function
		update-instance-for-different-class. The change-class function returns the instance.

		instance                 An object. 

		new-class                A class object or the name of a class.

		CLOS guarantees that change-class is supported when both the original class of the instance
		and the new class are of the metaclass standard-class. An individual CLOS implementation
		might support change-class in other circumstances as well.

		For related information, see

		     "Changing the Class of an Instance," page 151 
		     "A Procedural Definition: Initialization," page 165

**class-name** class                                                         Generic Function 

		Returns the name of the class object. 

		class A class object. 

		You can use setf with class-name to change the name of the class object.

		For related information, see 

		     "Mapping Between Names and Objects," page 134

**class-of** object                                                          Function

		Returns the class of the object. Note that every object is of some class, class-of
		returns a class object. 

		object              Any object.

**defclass** name ({superclass}`*`)                                             Macro
		   ([slot-spec]`*`) [class-option]`*`

		Defines a new class or redefines an existing one. The name of the class and the class
		object are made valid type specifiers, defclass returns the class object that represents
		the new class.

		name               A symbol naming this class. 

		superclass         A symbol naming a direct superclass of this class.

		slot-spec          Defines a slot of the new class. Can be given as  symbol (the name of
		                   the slot), or as a list containing the name of the slot followed by
				   one or more slot-options pertaining to the slot:

				       slot-name 
				        (slot-name slot-options...) 

				   The slot-options are as follows: 

		       :accessor reader-name 

		                   Defines methods for a reader and a writer generic function. You can
				   then use the reader named reader-name to read the value of this slot,
				   and use the writer named (setf reader-name) to write the value of this
				   slot. 
		       :reader reader-name 

		                   Defines a method for the reader generic function named reader-name for
				   reading the value of this slot. 

		       :writer function-spec

		                   Defines a method for the writer generic function named function-spec
				   for writing the value of this slot. If function-spec is a symbol, you
				   call the writer with the normal Lisp syntax: (symbol new-value instance).
				   If function-spec is a list such as (setf symbol), you call the writer
				   with the setf syntax, which is (setf (symbol instance) new-value).

		       :documentation string

		                   Specifies documentation for the slot.

		       :allocation allocation-type

                                   States whether this is a shared or local slot. The default allocation-type
				   is :instance, which indicates a local slot; :class indicates a shared
				   slot.

		       :initform form

		                   Gives a default initial value form for the slot. form is evaluated each
				   time it is used, in the lexical environment in which the defclass was
				   evaluated.

		       :initarg name

		                   Specifies an initarg for the slot. You can then initialize the value of
				   the slot when making an instance, by providing this initarg name and a
				   value in the call to make-instance.

		       :type type-specifier

		                   States that the value of this slot is expected to be of the type named
				   by type-specifier. This can result in compiler optimizations, but CLOS
				   does not guarantee error checking when the value is stored in the slot. 

*class-option*           An option pertaining to the class as a whole.

		The class-options are as follows: 

		       (: documentation string) 
		                  Specifies documentation for the class.

		       (:default-initargs {initarg-name form}*)
		                  Specifies default values for initargs. Each form is treated as a default
				  initial value form for the initarg of initarg-name. The :default-initargs
				  class option is the only class option inherited by subclasses; see
				  "Initialization Arguments," page 160.

		       (:metaclass class-name)

		                  States the class of the newly defined class; this is known as the
				  metaclass. The default metaclass is standard-class.

For related information, see

		"Defining the Kinds of Objects—Classes," page 19 
		"Implementation Choices: Methods versus Slots," page 66 
		"Class Inheritance," page 117 
		"Redefining Classes," page 140 
		"Creating and Initializing Instances," page 155 

**defgeneric** name lambda-list {option}*                                                        Macro 

		Defines a new generic function or redefines an existing one. Enables you to specify aspects
		of the generic function, such as the lambda-list, documentation, method combination type,
		argument precedence order, and declarations. You can also define methods within the
		defgeneric form, defgeneric returns the generic function object.

		name               Names the generic function; it is either a symbol or a list such as
		                   (setf symbol). 

		lambda-list        Describes the parameters of this generic function. It cannot contain
		                   any taux variables. Optional and keyword arguments may not have default
				   initial value forms or use supplied-p parameters. No parameter in this
				   lambda-list may be specialized. 

		options            The options are as follows: 

		     (:argument-precedence-order {parameter-name}*) 

		                   This affects the ranking of methods by precedence order. Instead of the
				   default left-to-right order, the arguments are considered in the order
				   of the parameter names given here. Each required parameter must appear
				   in this list.
				   For more information, see "Summary of Method Inheritance," page 98. 

		     (declare {declaration}*)

		                   Specifies declarations for the generic function, optimize can be given
				   to specify whether the generic dispatch procedure should be optimized
				   for speed or space. The following declarations are not allowed:
				   special, ftype, function, inline, notline, and declaration.

		     (documentation string)

		                   Specifies documentation for the generic function. 

		     (:method-combination symbol {arg}*) 

		                   Specifies that this generic function uses the method combination type
				   whose name is symbol, args are any arguments used by the method
				   combination type. For example, all method combination types defined by
				   the short form of define-method-combination accept an optional order
				   argument, which can be :most-specific-last to reverse the order of the
				   primary methods. :most-specific-first is the default.
				   For more information, see "Defining a New Method Combination Type,"
				   page 109.

		    (:method {qualifier}* specialized-lambda-list {decl \ doc}* 
		                  {form}*)

		                   Defines a method for this generic function. The method's qualifier,
				   specialized lambda-list, declarations, documentation, and forms are
				   the same as for defmethod. 

		    (:generic-function-class class-name)

		                   Specifies the class of the generic function object; the default is
				   standard-generic-function. 

		    (:method-class class-name)

		                   Specifies the class of the methods for this generic function; the
				   default is standard-method.

**defgeneric** is used to define a named generic function. You can use generic-function to define an anonymous generic function, generic-function has the same syntax as defgeneric, except the name argument is omitted.

For related information, see

		"Defining the Interface—Generic Functions," page 27 
		"Congruent Lambda-Lists," page 132 
		"Redefining Methods and Generic Functions," page 143 
		"Removing Generic Functions and Methods," page 136

**define-method-combination** name [option]*                                                Macro 

		Defines a new method combination type. Provides a convenient short-form syntax, which
		defines an operator method combination type. The syntax given here is for the short form. 

		name                A symbol naming this method combination type. 

		option              These are the options for the short form:

		      :documentation string 

		                    Specifies documentation for the method combination type. 
		      :identity-with-one-argument boolean

		                    Requests the compiler to optimize for cases when there is only one
				    method; this indicates that the value of that method should be returned
				    as the value of the generic function, rather than the operator being
				    called. This makes sense for operators such as progn, and, +, and max.

		      :operator operator

		                    Specifies the operator that receives the values of the methods.

The alternate long form provides a more flexible syntax that allows for defining more complex method combination types. This book does not cover the syntax of the long form.

For related information, see 

		"Defining a New Method Combination Type," page 109

**defmethod** name [qualifier]* specialized-lambda-list                                        Macro
		   {decl | doc }* {form}* 

		Defines a new method for a generic function or redefines an existing one. defmethod returns
		the method object.

		name                 The name of the generic function that this method is implementing. This
		                     is either a symbol or a list such as (setf symbol). 

		qualifier            A non-null atom used to identify the role of this method, according to
		                     the method combination type of the generic function. When standard
				     method combination type is used, the lack of any qualifier indicates a
				     primary method. The standard method combination also recognizes the
				     method qualifiers :before, :after, and :around. 

		specialized-lambda-list

		                     An ordinary function lambda-list except that the name of any of the
				     required parameters can be replaced by a specialized parameter. That is,
				     a required parameter is either var or (var parameter-specializer-name).
				     The optional parameters have exactly the same syntax as they do in an
				     ordinary lambda-lists, and they may not be specialized.

		parameter-specializer-name

		                     Can be a list such as (eql form) or a symbol naming a class. The class
				     can be a user-defined class, a built-in class, or a structure defined by
				     defstruct if the :type option was not used. 

		decl                   A declaration pertaining to this method. 

		doc                    A documentation string for this method. 

		form                   The body of this method. This is Lisp code to be executed when the
		                       generic dispatch calls this method.

		For related information, see 

		     "Methods for Null Locks," page 32 
		     "Methods for Simple Locks," page 35 
		     "Programming with Methods," page 65 
		     "Summary of Method Inheritance," page 98 
		     "Congruent Lambda-Lists," page 132 
		     "Redefining Methods and Generic Functions," page 143 
		     "Removing Generic Functions and Methods," page 136 

**describe** object                                                                  Generic Function 

		Prints a description of an object on the standard output stream. This is a generic function
		for which you can write methods, to specialize its behavior for a given class, describe
		returns no values. 

		object                  Any Lisp object.

		CLOS provides a default primary method for describe, describe uses the standard method
		combination type. 

		For related information, see

		    "Specializing describe for Locks," page 39 
		    "An After-Method for Describing Simple Locks," page 40 
		    "Specializing describe for Ordered Locks," page 49 
		    "Specializing describe for Print-Request Queues," page 56 

**find-class** symbol soptional (errorp t) environment                                  Function

		If the symbol is the name of a class, find-class returns the class object.

		symbol                   The name of a class.

		errorp                   States what to do if there is no class by this name: If errorp is
		                         true, an error is signaled; otherwise, nil is returned.

		You can use setf with find-class to change the class associated with this symbol.

		For related information, see 

		      "Mapping Between Names and Objects," page 134 

**find-method** generic-function qualifiers                                            Generic Function
		specializers &optional errorp 

		Returns the method object identified by the generic function it implements, the method's
		qualifiers, and the parameter specializers.

		generic-function           A generic function object, which can be obtained by using
		                           symbol-function.

		qualifiers                 A list of the method's qualifiers.

		parameter-specializers 
		                           A list of the method's parameter specializer objects. This list
					   must contain one element corresponding to each required parameter.
					   For any unspecialized parameters, the class named t should be
					   given. 

		errorp                     If errorp is t, CLOS signals an error if there is no such method.
		                           If errorp is nil, CLOS returns nil if there is no such method.
					   The default is t.

		For related information, see

		     "Mapping Between Names and Objects," page 134 
		     "Removing Generic Functions and Methods," page 136 

**initialize-instance** instance &rest initargs                                           Generic Function

		Invoked automatically by the system when make-instance is called; initialize-instance
		should not be called by users. You can specialize initialize-instance to control how new
		instances are initialized. This generic function returns the instance.

		instance                   The newly created instance. 

		initargs                   Alternating initarg names and values. The valid initarg names
		                           include the slot-filling initarg names for the class (defined by
					   the :initarg option to defclass) and the names of keyword
					   parameters specified in methods for initialize-instance or
					   shared-initialize.

		A system-supplied default primary method performs slot initialization by calling
		shared-initialize with the instance, t (indicating that all slots should be filled with the
		values of their initforms), and the initargs. In most cases, you should supply after-methods
		to allow the default primary method to run. This generic function uses the standard method
		combination type.

		For related information, see 

		     "Creating and Initializing Instances," page 155 
		     "Controlling Initialization with Methods," page 159 
		     "A Procedural Definition: Initialization," page 165

**make-instance** class &rest initargs                                                     Generic Function 

		Creates a new instance of the specified class and initializes the slots of the new instance
		by calling the generic function initialize-instance with the newly created instance and
		initargs. make-instance returns the initialized instance.

		class                       The name of a class or a class object. 

		initargs                    Alternating initarg names and values. The valid initarg names
		                            include the slot-filling initarg names for the class (defined by
					    the :initarg option to defclass) and the names of keyword
					    parameters specified in methods for make-instance,
					    initialize-instance, and shared-initialize. 

		For related information, see

		     "Creating and Initializing Instances," page 155 
		     "Summary of What make-instance Does," page 156 
		     "Controlling Initialization with defclass Options," page 157 
		     "Controlling Initialization with Methods," page 159 
		     "Initialization Arguments," page 160 
		     "Defining Tape Streams," page 186 
		     "Defining Disk Streams," page 192

**next-method-p**                                                                           Function

		Can be called within a method to find out whether there is a "next method." This function
		is useful in methods where you expect to use call-next-method, and you want to ensure that
		there is a next method to call. This function takes no arguments. It returns true if there
		is a next method, and nil if there is not.

		The method combination type defines what the "next method" is. The standard method
		combination type defines the next method as follows:

*  In an around-method, the "next method" is the next most specific around-method if there is one. Otherwise, the "next method" consists of the before-methods, the most specific primary method, and the after-methods.

*   In a primary method, the "next method" is the next most specific primary method.

		For related information, see

		      "Around-Methods," page 102 
		      "Calling a Shadowed Primary Method," page 105 

**print-object** object stream Generic                                                        Function

		Writes the printed representation of an object to a stream. The purpose of print-object is
		to allow you to control the printing behavior of objects of a given class, by writing methods
		that specialize print-object. CLOS provides a default primary method for print-object,
		print-object uses the standard method combination type.

		print-object is called by the print system and should not be called by users. All Common
		Lisp printing functions call print-object, including write, prinl, format -A and -S, and
		others.

		print-object returns the object, its first argument.

		object                    Any object. 

		stream                    This must be a real stream, and cannot be t or nil.

		The generic function print-object has a protocol that all methods should follow. Methods
		should obey the print control special variables described in Steele's Common LISP: The
		Language. For more details on print-object, see the CLOS specification.

		For related information, see

		    "Controlling How Locks Print," page 37 
		    "Specializing print-object for Locks," page 38 

**reinitialize-instance** instance &rest initargs                                           Generic Function

		Reinitializes an instance according to the initargs. You can specialize reinitialize-instance
		to control how instances are reinitialized. This generic function is rarely used in
		application programs, but is used within the implementation of CLOS itself, in the metaobject
		protocol. This generic function returns the instance.

		instance                   The instance to reinitialize.

		initargs                   Alternating initarg names and values. The valid initarg names
		                           include the slot-filling initarg names for the class (defined by
					   the :initarg option to defclass) and the names of keyword
					   parameters specified in methods for reinitialize-instance or
					   shared-initialize. A system-supplied default primary method
					   performs slot initialization by calling shared-initialize with
					   the instance, nil (indicating that no slots should be filled with
					   the values of their initforms), and the initargs. In most cases,
					   you should supply after-methods to allow the default primary
					   method to run. This generic function uses the standard method
					   combination type. 

		For related information, see 

		     "A Procedural Definition: Initialization," page 165 

**remove-method** generic-function method                                                    Generic Function 

		Removes a method from a generic function and returns the modified generic function object.

		generic-function           A generic function object. 

		method                     A method object.

		CLOS signals an error if the method is not one of the methods for the generic function.

		For related information, see

		      "Removing Generic Functions and Methods," page 136

**shared-initialize** instance slots-for- initform                                           Generic Function 
		&rest initargs 

		Called in four contexts to initialize an instance: to initialize a new instance
		(initialize-instance), to reinitialize an instance (reinitialize-instance), to update an
		instance to a new class redefinition (update-instance-for-redefined-class), and to update
		an instance to a different class (update-instance-for-different-class). The shared-initialize
		generic function should not be called by users. You can specialize shared-initialize to
		control how instances are initialized in these four contexts. This generic function returns
		the instance.

		instance                   The instance to initialize.

		slots-for-initform         Indicates which slots should be filled with the values of their
		                           initforms (if they are still unbound). Either a list of slot
					   names, or t to indicate all slots, or nil to indicate no slots. 

		initargs                   Alternating initarg names and values. The valid initarg names
		                           include the slot-filling initarg names for the class (defined by
					   the :initarg option to defclass) and the names of keyword
					   parameters specified in methods for shared-initialize.

		A system-supplied default primary method first initializes all slots for which a slot-filling
		initarg is given. Then, for any slots indicated by the slots-for-initform argument that are
		still unbound, the method fills those slots with the values of their initforms. In most
		cases, you should supply after-methods for shared-initialize, to allow the default primary
		method to run. This generic function uses the standard method combination type.

		For related information, see

		     "A Procedural Definition: Initialization," page 165 
		     "Creating and Initializing Instances," page 155 

**slot-boundp** instance slot-name                                                              Function

		Returns true if the indicated slot of the instance is bound; otherwise, returns false. 

		instance                   An instance.

		slot-name                  A symbol naming a slot of the instance.

		This generic function is useful in methods for print-object or describe, if you want to
		ensure that the methods do not signal errors if slots are unbound. It can also be useful
		in methods that initialize instances.

		For related information, see

		      "Specializing print-object for Locks," page 38

**slot-value** object slot-name                                                                 Function 

		Returns the value of the specified slot of the object. If there is no slot of that name,
		an error is signaled. You can use setf with slot-value to write a new value into the slot,
		slot-value is the primitive used to implement accessor methods.

		object                      A form evaluating to an object that has slots. Usually this is
		                            an instance of a user-defined class, since the structure of
					    these classes is in the form of slots. 

		slot-name                   A symbol naming a slot. 

		For related information, see

		       "Programming with Accessors," page 70 

**update-instanee-for-different-elass**                                                    Generic Function 
		previous new &rest initargs 

Invoked automatically by the system when change-class is called; update-instance-for-different-class should
not be called by users. You can specialize update-instance-for-different-class to control how instances are
updated to the target class. Any value returned is ignored by the caller, change-class.

		previous                    A copy of the previous version of the instance.

		new                         The new version of the instance.

		initargs                    Alternating initarg names and values. The valid initarg names
		                            include the slot-filling initarg names for the class (defined
					    by the :initarg option to defclass) and the names of keyword
					    parameters specified in methods for
					    update-instance-for-different-class or shared-initialize.

A system-supplied default primary method performs slot initialization by calling shared-initialize with the instance, a list of the names of the added local slots (indicating that they should be filled with the values of their initforms), and the initargs. In most cases, you should supply after-methods to allow the default primary method to run. This generic function uses the standard method combination type. The caller of change-class arranges the arguments such that a copy of the previous version is accessible, as well as the new version of the instance. This allows methods to access information stored in the previous version and to use that information to update the new version of the instance. Any value returned is ignored by its caller.

		For related information, see

		     "Changing the Class of an Instance," page 151 
		     "A Procedural Definition: Initialization," page 165 

**update-instance-for-redeflned-class** instance                                        Generic Function 
             added-slots discarded-slots plist 
             &rest initargs 

Invoked automatically by the system when a class is redefined; update-instance-for-redefined-class should not be called by users. You can specialize update-instance-for-redefined-class to control how instances are updated to the new version. Any value returned is ignored by the caller.

The caller of update-instance-for-redef ined-class provides the arguments added-slots, discarded-slots, and plist to be used by methods. These arguments allow methods for update-instance-for-redefined-class to access information stored in the previous version and to use that information to update the new version of the instance.

		instance                    The instance after its structure has been updated.

		added-slots                 A list of slots that were added to the instance.

		discarded-slots             A list of slots whose values are being discarded. This includes
		                            any slots specified in the old class definition but not in the
					    new one, and any slots specified as local in the old definition
					    and shared in the new one.

		plist                       A list of alternating slot names and values. Each discarded slot
		                            with a value appears in the plist. No unbound slots appear in the
					    plist.

		initargs                    Alternating initarg names and values. The valid initarg names
		                            include the slot-filling initarg names for the class (defined by
					    the :initarg option to defclass) and the names of keyword
					    parameters specified in methods for
					    update-instance-for-redefined-class or shared-initialize.

A system-supplied default primary method performs slot initialization by calling shared-initialize with the instance, added-slots (indicating that all added local slots should be filled with the values oftheir initforms), and the initargs. In most cases, you should supply after-methods to allow the default primary method to run. This generic function uses the standard method combination type.

		For related information, see 

		      "Redefining Classes," page 140 
		      "Example of Redefining CLOS Elements," page 144 
		      "A Procedural Definition: Initialization," page 165

**with-accessors** ({accessor-entry)*) instance-form sbody body                                   Macro 

		Creates a lexical context for referring to accessors by variables. This is a convenient
		shorthand for calling reader or writer generic functions, with-accessors returns the values
		of the last form in the body. 

		instance-form               A form that evaluates to an instance. 

		accessor-entry              A list of the form (variable-name accessor-name). 

		Within the body of with-accessors you can use setf or setq with the variable to call the
		writer generic function. 

		For related information, see 

		      "Programming with Accessors," page 70 
		      "Using with-accessors and with-slots," page 73

**with-slots** ([slot-entry]*) instance-form sbody body                                            Macro 

		Creates a lexical context for referring to slots by variables. This is a convenient
		shorthand for calling slot-value, with-slots returns the values of the last form in the body.

		instance-form              A form that evaluates to an instance. 

		slot-entry                 Either a slot name alone or a list (variable-name slot-name).
		                           If the slot name is given alone, you can access the slot by a
					   variable with the same name as the slot. The alternate syntax
					   allows you to specify a different variable name for accessing
					   the slot.

		Within the body of with-slots you can use setf or setq with the variable to write a value
		into the slot.

		For related information, see 

		      "Programming with Accessors," page 70 
		      "Using with-accessors and with-slots," page 73