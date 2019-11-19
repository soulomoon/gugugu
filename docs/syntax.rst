Writing Definition File
=======================

The definition syntax is a strict subset of Haskell.

.. literalinclude:: ./examples/Hello.pg
   :language: haskell


Module
------

Gugugu definitions are grouped into several modules.
The first line is the module header.
The ``module`` and ``where`` are keywords.
The module name is required to has its first letter capitalized,
and the file name of that module must be ``ModuleName.pg``.
It is possible to import types from other modules with ``import ModuleName``.

Modules cannot be nested yet.


Types
-----

Primitive Types
~~~~~~~~~~~~~~~

Gugugu has several primitive types,
which are impossible to be defined with the language itself,
or defined as primitive for efficiency reasons.

Scalar Types
^^^^^^^^^^^^

Scalar types are the most basic types,
they cannot be decomposed to other types.

+------------+------------------------------------------------------------+
| ``Unit``   | a type with only single value, usually used as placeholder |
+------------+------------------------------------------------------------+
| ``Bool``   | boolean value                                              |
+------------+------------------------------------------------------------+
| ``Int32``  | 32-bit signed integer                                      |
+------------+------------------------------------------------------------+
| ``Double`` | double precision floating point number                     |
+------------+------------------------------------------------------------+
| ``String`` | string of characters, not bytes                            |
+------------+------------------------------------------------------------+

Container Types
^^^^^^^^^^^^^^^

Container types contain several values of other types,
they are ``Maybe`` and ``List``.

``Maybe A`` contains either a value of type ``A`` or nothing.

``List A`` contains :math:`n` values of type ``A``, where :math:`n >= 0`.

Use parentheses if necessary, like ``List (Maybe Int32)``.

Function Types
^^^^^^^^^^^^^^

They are ``->`` and ``IO``, which are only used as type of a function.

Record Type
~~~~~~~~~~~

A record type consists of several values of other types.
They are defined with

.. sourcecode:: haskell

   data Book
     = Book
       { id   :: Int32
       , name :: String
       }

The ``data`` is a keyword.
The name before the equal sign is the type constructor,
and the name after the equal sign is the value constructor.
They have to be the same for record types.

A record type can has as many fields as you want,
though there *is* a hard limit for most targets,
which is usually :math:`2^{31} - 1`, and usually not a problem.
The type of the field comes after the field name,
with a double-colon ``::``.

Gugugu recommends ``camelCase`` for both field names and consturctors,
but you can use any name you want.

Enum Type
~~~~~~~~~

An enum type is a set name values of the type.
They do not hold other values.
They are defined with

.. sourcecode:: haskell

   data Color
     = Red
     | Green
     | Blue

The name before the equal sign is the type constructor,
and the name after the equal sign is the value constructor like the record type.
But they do not have the same name.

Foreign Type
~~~~~~~~~~~~

A foreign type is like a primitive type.
You have to tell Gugugu how to encode/decode it.

.. sourcecode:: haskell

   data DateTime
     {-# FOREIGN target1 target1SpecificContent #-}
     {-# FOREIGN target2 target2SpecificContent #-}

The definition of foreign type depends on the target you are compiling to,
consult the documentation of the target for more information.

Functions
---------

A function, or remote call is defined with

.. sourcecode:: haskell

   aFunction :: A -> IO B

   fold :: FoldRequest -> IO Int32

A function definition always has the type ``A -> IO B``,
where ``A`` is the parameter of the function,
and ``B`` is the return value of the function.
A function can have exactly one parameter and one return value.
Create a new type for the function,
if you need functions with multiple parameters.
