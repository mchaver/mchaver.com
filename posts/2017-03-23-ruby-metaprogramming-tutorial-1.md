---
title: Ruby Metaprogramming Tutorial Part 1 - Introduction and Background
---

## Introduction

Ruby provides a convenient syntax for metaprogramming but it is often unclear
when other libraries are using metaprogramming

Ruby is reflective, you can take a look at the properties of all objects. 

Ruby's facilities for DSL authoring don't change the nature of the language. Ruby's metaprogramming facilities are inherently tied to Ruby syntax and semantics, and whatever you write has to be shunted into Ruby's object model.

The often repeated Ruby mantra is "Don't Repeat Yourself" (DRY). In Ruby this is 
mainly achieved through the use of Metaprogramming. Metaprogramming is a 
technique that allows us to write code that returns code that we can use. We 
will start with a simple example.

```ruby
class Simple
  def hello
    puts "Hello"
  end
  
  def method_missing(name, *args, &block)
    puts "tried to handle unknown method %s" % name
    unless args.empty?
      puts "it had arguments: %p" % [args]
    end
  end  
end
```

We can try loading that class in `irb` and running a few methods on it.

```ruby
irb(main):013:0> Simple.new.hello
Hello
=> nil
irb(main):014:0> Simple.new.goodbye
tried to handle unknown method goodbye
=> nil
irb(main):015:0> Simple.new.goodmorning("Julio")
tried to handle unknown method goodmorning
it had arguments: ["Julio"]
=> nil
```

The `method_missing` method catches any `method` that we attempt to 
call in an instance of the `Simple` that is not previously declared. If the 
method does not exist in `Simple` then `method_missing` will be called with all 
of the details of the non-existent method and print its values to the stdout. 

This provides us a way to handle methods we have not defined within in the class. 
We can use particular details about the method name, arguments or code block to 
decide what to do next.

## Background 

Before exploring Metaprogramming further, we need to understand the Ruby 
object system and introspection. In Ruby, we do everything in the context of 
an object. Even when we run `irb` and run code at the top level, we are actually 
working in `main` of class `Object`. At anytime you add methods to a ruby class 
or override existing methods.

#### class and instance variables

Class variables are available directly from a class to a class or an instance. 
Instance variables require making an instance with new and then the instance 
have access to the variables. Users only have direct access to exposed class and 
instance variables. However, you can still access private variables through 
`Object` methods. Class variables only have one value per class and subclass 
hierarchy.

```ruby
class Animal
  @@legs = 2
  def self.legs
    @@legs # class variable getter
  end
end

puts Animal.legs # => 4
```

Even when you inherit a class with class variables and change the value in the 
subclass, the value update will be reflected in the parent class as well. The 
class variable is shared between the parent and the subclasses.


```ruby
class Dog << Animal
  @@legs = 4
end

puts Animal.legs # => 4
puts Dog.legs # => 4
```

We will remake the dog class with its own class and instance variables. We 
expose the `@name` instance variable via `attr_accessor`.

```ruby
class Dog
  @@legs = 4
  @name 
  attr_accessor :name # create getter and setter for Dog  
end

d = Dog.new
d.name = "Fido"
puts d.instance_variables # => [:@name]
```

#### class and instance methods

Class methods are available without making an instance and require the `self`
prefix. They cannot access instance variables. Instance variables are available
after creating an instance with `new`. They can access class and instance 
variables.

```ruby
class Foo
  def self.bar
    puts 'class method'
  end
  
  def baz
    puts 'instance method'
  end
end
```


#### include 

The `include` statement mixes in a module into a class. It is a simple form of 
multiple inheritance and forms a "is-a" relationship. It makes its methods 
available to an instance of the class that includes it.

```ruby
module Foo 
  def foo 
    puts "Hello from Foo"
  end
end

class Bar
  include Foo
end

Bar.new.foo
Hello from Foo
```

#### extend

The `extend` statement makes a module's methods to class. We can call the 
methods directly from the class without making a new instance.

```ruby
class Baz
  extend Foo
end

Baz.foo
Hello from Foo
```

#### self

`self` is a contextual pointer. It points to the current object the program is 
located in. When we are in the top level of the file, we are located in the 
`main` object.

```ruby
> puts self
main

> puts self.class
Object
```

When we declare a method at the top level, it becomes part of the `main` object. 
We can see that `:m` is a method of `main`.

```ruby
def m
end

puts self.method(:m)
=> #<Method: Object#m>

self.methods
=> [:m ...]
```

In an object or module `self` is the current object.

```ruby
class S
  def put_self
    puts self
  end
end

> S.new.m
#<S:0x007fad47294db0>

module Library
  puts self
end
Library
```


#### BasicObject

`BasicObject`[^1] is the parent object of all objects in Ruby. It can be used 
to create alternative object hierarchies in Ruby. The main hierarchy in Ruby is
`Object` that includes `Kernel`. It provides a small number of methods. 

There are four callback methods that are useful for metaprogramming.

- `method_missing`
- `singleton_method_added`
- `singleton_method_removed`
- `singleton_method_undefined`

#### Kernel

`Kernel`[^2] provides a number of useful methods such as `Array`, `Hash`, 
`Integer`, `String`, `open`, `puts`, etc. `Kernel` is mixed into `Object` and 
`main` is of class type `Object` so all of these methods act as global methods 
in Ruby. 

These methods are useful for metaprogramming.

- `eval`
- `exec`
- `lambda`
- `local_variables`

#### Object

`Object`[^3] is the main object in the Ruby object hierarchy. It `include`s `Kernel`
and is a child of `BasicObject`. `Object` has a large number of useful methods 
for metaprogramming.

```
define_singleton_method     extend                    instance_of? 
instance_variable_defined?  instance_variable_get     instance_variable_set
instance_variables          is_a?                     kind_of?
method                      methods                   private_methods
protected_methods           public_method             public_methods
public_send                 remove_instance_variable  respond_to?
respond_to_missing?         send
```

[^1]: [ruby-doc :: BasicObject](https://ruby-doc.org/core-2.4.1/BasicObject.html)
[^2]: [ruby-doc :: Kernel](https://ruby-doc.org/core-2.4.1/Kernel.html)
[^3]: [ruby-doc :: Object](https://ruby-doc.org/core-2.4.1/Object.html)
