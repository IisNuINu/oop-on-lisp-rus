# Appendix A 
# Glossary of CLOS Terminology

**Accessor**

		A generic function for reading or writing the value of a slot. The term "accessor" includes
		both readers and writers. The :accessor slot option to defclass causes methods for a reader
		and a writer for that slot to be generated automatically. 

**After-method**

		A method whose role is to be called after the primary method, usually to do some sort of
		cleanup work. The standard method combination recognizes an after-method by the method
		qualifier :after in the defmethod form. 

**Aggregate class**

		A descriptive term for a class composed of several building block classes. An aggregate
		class usually derives all its structure and behavior from its superclasses and does not
		provide further customizations. 

**Applicable method**

		A method whose required parameters are all satisfied by the corresponding arguments to
		the generic function. When a generic function is called, CLOS locates the set of applicable
		methods.

**Argument**

		An object given as input to a function. 

**Around-method**

		A method whose role is to surround all other kinds of methods. An around-method usually
		performs some computation and uses call-next-method to invoke the before-methods, primary
		method, and after-methods. An around-method can set up an environment to be in effect
		during the execution of the other methods, such as setting up a catch, binding a special
		variable, or owning a lock. The standard method combination recognizes an around-method by
		the method qualifier :around in the defmethod form. 

**Basic class**

		A descriptive term for a class that is the root, or foundation, of a set of classes. A
		basic class provides characteristics that all its subclasses have in common, such as their
		type (for example, all locks are of type lock), and default methods. 

**Before-method**

		A method whose role is to be called before the primary method, usually to do some sort of
		set-up work in advance of the primary method. The standard method combination recognizes
		a before-method by the method qualifier :before in the defmethod form. 

**Built-in class**

		A predefined class that is implemented in a special system-dependent way; in other words,
		it is not implemented as a user-defined class. Many of the classes corresponding to Common
		Lisp types (such as array, list, number, and t) are implemented as built-in classes. 

**Built-in method combination type** 

		A predefined method combination type provided by CLOS. The default method combination type
		is called standard. The others are operator method combination types, including: +, and,
		append, list, max, min, nconc, or, and progn. 

**Class**

		A Common Lisp type that defines the structure and behavior of a set of objects, which are
		called instances of the class. The structure of the class lies in its slots. The behavior
		is implemented by methods. Classes can be "built on" other classes, to inherit structure
		and behavior from them. 

**Class precedence list**

		A list of classes containing the class itself and all its superclasses, ordered from most
		to least specific. CLOS computes a class precedence list for each class, based on the
		defclass forms of the class and all of its superclasses. The class precedence list governs
		how methods, slots, and other characteristics are inherited. When one class is more specific
		than another, it has precedence (or dominance) over the other class. Thus, if the two
		classes offer competing traits, the more specific class takes precedence over the less
		specific class. 

**Client**

		A Lisp program that uses a CLOS program; the client calls generic functions defined by the
		CLOS program. 

**CLOS implementation**

		A body of code that supports CLOS as defined by the CLOS specification and runs on a
		particular operating system. This term is useful for discussing portability issues (issues
		of writing programs with the intention of running them on different operating systems). 

**Constructor** 

		A function used to create new instances. Constructors are ordinary Lisp functions that
		call make-instance. Constructors provide a more abstract interface than does make-instance,
		and they can use the full power of Lisp argument processing. 

**Default method**

		A descriptive term for a method whose purpose is to be inherited by a family of classes.
		CLOS provides several default methods, which we call "system-supplied default methods" to
		distinguish them from methods that users define.

**Direct subclass** 

		A direct subclass is the inverse of a direct superclass. If the class shape is a direct
		superclass of the class triangle, then triangle is a direct subclass of shape. 

**Direct superclass** 

		A class that is included in the defclass form of another class. The relationship between
		a class and its direct superclass is like that between a child and its parent, in that
		there is no intervening  ancestor. A class inherits structure and behavior from its direct
		superclasses. Class inheritance is transitive, so a class inherits from each of its direct
		superclasses, their direct superclasses, and so on. 

**Effective method** 

		The Lisp code that comprises the implementation of a generic function for a given set
		of arguments. An effective method combines the applicable methods according to the method
		combination type. 

**Generic dispatch** 

		The CLOS mechanism that occurs when a generic function is called. The generic dispatch
		chooses the implementation appropriate for the arguments. This entails selecting the set
		of applicable methods, ranking the applicable methods in precedence order, combining the
		applicable methods into an effective method, calling the effective method, and returning
		the values of the effective method. 

**Generic function**

		A Lisp function whose implementation is distributed across one or more methods. To the
		caller, a generic function looks like an ordinary Lisp function. It accepts arguments,
		performs some operation, and returns values. Invisibly to the caller, an internal and
		automatic procedure (the generic dispatch) occurs when a generic function is called;
		this entails choosing the method or methods appropriate to the arguments. 

**Implementation**

		The inner workings of a program or function. This information is usually known to the
		developer of the program but is concealed from callers. The implementation of an ordinary
		function consists of the body of the defun, whereas the implementation of a generic
		function is distributed across a set of methods. See also "CLOS implementation" in this
		glossary. 

**Individual method** 

		A method that specializes one of its parameters on an individual Lisp object. The
		lambda-list of an individual method contains a parameter specializer name such as
		(eql form). This method is applicable if the corresponding argument is eql to the
		object that is the value of form (and if all other specialized parameters are satisfied). 

**Inheritance** 

		The sharing of characteristics or behavior among related classes. CLOS supports inheriting
		methods, slots, most slot options, and one class option. 

**Initarg** 

		An argument given to make-instance to control the initialization of instances. An initarg
		can be used to fill a slot with a value, or by an initialization method, or both. Initargs
		can be used in related initialization tasks, such as updating an instance when a class is
		redefined, changing the class of an instance, and reinitializing an instance. Initarg is
		shorthand for "initialization argument." 

**Initform** 

		A default value for a slot. The :initform slot option to defclass is used to provide a
		default value for a slot.

**Instance** 

		A Lisp object. With the advent of CLOS, every Lisp object is an instance of a class. Objects
		of the Common Lisp types, such as numbers, arrays, and lists, are instances of classes whose
		name is the same as the name of the type specifier. Other objects are instances of
		user-defined classes. All instances of a given class have the same type, the same structure,
		and the same behavior. (Note that individual methods can be used to cause one particular
		instance to behave differently from the other instances of its class.) 

**Interface** 

		The information about a function (whether it is ordinary or generic) that callers need to
		know, including: its expected arguments, the job it does, and its returned values. 

**Lambda-list** 

		A list that specifies the names of parameters of a function. Methods and generic functions
		have lambda-lists, as do ordinary Lisp functions. 

**Local slot** 

		A slot that stores information about the state of an instance. A local slot is defined when
		the :allocation :instance slot option to defclass is provided, or when the :allocation slot
		option is omitted. 

**Metaobject** 

		An object that represents a CLOS element, such as a class object, method object, or generic
		function object.

**Metaclass** 

		A class whose instances are class objects, such as standard-class, built-in-class, and
		structure-class.

**Method**

		Lisp code that implements a portion of (or the entire) implementation of a generic function
		for a set of arguments. Like functions, methods take arguments, perform some computation,
		possibly produce side effects, and return values. Unlike functions, methods are never called
		directly; they are called by the generic dispatch procedure. Each method has a role, which
		states its purpose in the generic function and controls how it interacts with other methods. 

**Method combination type** 

		A mechanism that specifies, for a generic function, what method roles are allowed, how the
		applicable methods are combined into an effective method, and how the values of the generic
		function are computed. A method combination type is a Lisp object named by a symbol.

**Method qualifier** 

		A symbol appearing in the defmethod form that indicates the method's role. The symbols
		:after, :before, and :around are three examples. A method whose qualifier is :after is an
		after-method.

**Method role** 

		The way this method interacts with the other applicable methods. The method combination
		type uses the method's role when combining it with the other methods into the effective
		method.

**Mixin class** 

		A descriptive term for a class intended to be a building block for other classes. It
		usually supports some aspect of behavior orthogonal to the behavior supported by other
		classes in the program; typically, this customization is supported in before- and after-
		methods. A mixin class is not intended to interfere with other behavior, so it usually
		does not override primary methods supplied by other classes.

**Multi-method** 

		A method that specializes more than one parameter. The technique of using multi-methods
		is intended for operations whose implementation truly depends on the type of more than
		one argument.

**Multiple inheritance** 

		A system in which a class can share the characteristics and behavior of more than one
		direct superclass. CLOS supports multipie inheritance, in that a class can have any number
		of direct superclasses. This flexibility makes all sorts of class organizations possible,
		whereas single inheritance is limited to strictly hierachical organizations. CLOS controls
		the multiple inheritance by using a class precedence list, which unambiguously states the
		precedence of each class with respect to the others.

**Operator method combination types** 

		A method combination type that defines a framework that combines all applicable primary
		methods inside a Lisp function, macro, or special form. CLOS offers a set of built-in
		operator method combination types, and you can define new ones with the short from of
		define-method-combination.

**Parameter**

		A specification of the expected input of a function, generic function, method, or other
		kind of Lisp operator. Each parameter specifies a variable name, which is bound to the
		corresponding argument when the function is called. Methods can have specialized parameters,
		which indicate the method's applicability, as well as variable names.

**Parameter specializer** 

		The object indicated by a parameter specializer name. If a parameter specializer name is
		a class name, the parameter specializer is the class object named by that name. If a
		parameter specializer name is a list (eql form), the parameter specializer is the list
		(eql object), where object is the result of evaluating form at the time the method is
		defined.

**Parameter specializer name** 

		The portion of a specialized parameter appearing in a method's lambda-list that indicates
		the applicability of the method. A parameter specializer name can be a class name or a
		list (eql form).

**Primary method**

		A method whose role is to perform the bulk of the work of a generic function. In the
		standard method combination, only the most specific applicable primary method is called;
		however, a primary method can use call-next-method to cause the next most specific
		applicable primary method to be called. The standard method combination recognizes a
		primary method by the absence of any method qualifier in the defmethod form. 

**Procedural definition**

		A technique in which a high-level task is broken down into separate generic functions,
		each of which is responsible for a clearly defined portion of the task. Usually there
		is a default behavior for the generic functions. Programmers use these generic functions
		as entry points; they can control portions of the task by specializing one or more of
		the generic functions.

**Protocol**

		A definition of the behavior of a set of objects. Some protocols are intended for
		programmers who are developing client programs, whereas other protocols are intended
		for programmers who wish to extend a program.

**Reader**

		A generic function for reading the value of a slot. Reader methods can be generated
		automatically, through use of the :accessor or :reader slot options to defclass.

**Shared slot**

		A slot that stores information about the state of a class (or of all instances of the
		class). A shared slot is defined when the :allocation :class slot option to defclass is
		provided.

**Single inheritance**

		A system in which a class can be built on no more than one other class, which in turn
		can be built on no more than one other class, and so on. Single inheritance results in
		a strictly hierarchical organization. Common Lisp defstruct supports single inheritance.

**Slot**

		A place where state information is stored. A slot has a name and a value. The :allocation
		slot option to defclass controls whether a slot is local or shared. A local slot stores
		information about the state of an instance, and a shared slot stores information about the
		state of a class (or of all instances of the class). The value of a slot can be read and
		written by accessors.

**Specialized parameter** 

		A parameter expressed as a list whose first element is a variable, and whose second
		element is a parameter specializer name. Any required parameter in a method's lambda-list
		may be specialized.

**standard-object**

		A class that is implicitly included in the class precedence list of every user-defined
		class. Several default methods are attached to the class standard-object. 

**standard method combination type** 

		The default method combination type: It supports around- methods, before-methods, primary
		methods, and after-methods.

**Subclass**

		The inverse of superclass. If the class t is a superclass of the class triangle, then
		triangle is a subclass oft.

**Superclass** 

		A class from which another class inherits. The superclasses of a class include all of its
		direct superclasses, all of their direct superclasses, and so on.

**Unbound slot** 

		A slot that has no value. A slot that was neither initialized nor written to is unbound.
		CLOS signals an error if an attempt is made to read the value of an unbound slot.

**Writer** 

		A generic function for writing the value of a slot. Writer methods can be generated
		automatically, through use of the :accessor or :writer slot options to defclass.
		Usually, the name of a writer is a list such as (setf reader); such a writer is
		called with the setf syntax, which is (setf [reader object) new-value).