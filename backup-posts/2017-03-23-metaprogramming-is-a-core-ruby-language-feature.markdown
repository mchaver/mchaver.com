---
title: Metaprogramming is a Core Ruby Language Feature
---

class 
instance
activesupport
attributes
instance variable
class variable
http://www.railstips.org/blog/archives/2009/05/11/class-and-instance-methods-in-ruby/
http://www.railstips.org/blog/archives/2006/11/18/class-and-instance-variables-in-ruby/


Before using Ruby professionally, I thought the language was pretty 
similar to Python. They are both high-level, object-oriented programming 
languages with dynamic typing systems. They do have different communities and 
available packages and while being central to the growth and use of programming languages,
I would not consider that part of the core language features. After spending time with various
Ruby gems, I have come to understand the importance the role metaprogramming 
plays in Ruby. I think the ease of metaprogramming is a distinguishing feature
of Ruby. Python also supports metaprogramming, but it does not seem as widely 
used (this claim is unquantified).

Don't Repeat Yourself (DRY) is an often repeated Ruby mantra. Metaprogramming plays 
a central role in avoiding code duplication in Ruby. To have a strong understanding 
of the Ruby language and commonly used gems, one must understand metaprogramming.

Ruby is reflective, you can take a look at the properties of all objects. 

It is also dangerous, you can easily break the expected behavior of Ruby.

Values are objects, but not all things in Ruby are values. There is also control flow
and bodies. Procs are Ruby bodies as objects. blocks are not objects. Lambdas are a different 
type of proc. 

Ruby provides a convenient syntax for metaprogramming. 

Lisp creation of new abstractions. 

Both are difficult to reason about if you do not have a strong understanding of 
the package.

[What is the difference between a Block, a Proc, and a Lambda in Ruby?](http://awaxman11.github.io/blog/2013/08/05/what-is-the-difference-between-a-block/)

[Why Ruby is an acceptable LISP](http://www.randomhacks.net/2005/12/03/why-ruby-is-an-acceptable-lisp/)

[In what area is LISP's macro better than Ruby's “ability” to create DSL](http://softwareengineering.stackexchange.com/questions/81202/in-what-area-is-lisps-macro-better-than-rubys-ability-to-create-dsl)

[A Rubyist's Impressions of Common Lisp](https://blog.jacius.info/2012/04/04/a-rubyists-impressions-of-common-lisp/)

Some good places to start are the ruby documentation. 

[BasicObject](https://ruby-doc.org/core-2.4.1/BasicObject.html)
[Kernel](https://ruby-doc.org/core-2.4.1/Kernel.html)
[Object](https://ruby-doc.org/core-2.4.1/Object.html)

irb operates in object
For all Objects deriving Object, you can use kernel features as if they were 
language features. It is a clever trick. You can easily extend objects in ruby 
so whenever you extend Kernel, the methods will be available in all objects.

object at the top level is main. Everything in ruby occurs in the context of an object.
an instance of Object with the special property that any methods defined there are added as instance methods of Object
it is a class of object

irb 
```
irb(main):001:0> self
=> main
irb(main):002:0> self.class
=> Object
```

(activesupport/concern)[http://api.rubyonrails.org/classes/ActiveSupport/Concern.html]

hooks

[About Ruby](https://www.ruby-lang.org/en/about/)
