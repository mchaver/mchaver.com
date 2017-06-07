---
title: Ruby Metaprogramming Hooks
---

inherited

inherited(subclass)
> Callback invoked whenever a subclass of the current class is created.

```ruby
class Foo
  def inherited(subclass)
    puts "Foo has been inherited by #{subclass}"
  end
end

class Bar < Foo
end
```

included

included(othermod)

> Callback invoked whenever the receiver is included in another module or class. 
This should be used in preference to Module.append_features if your code wants 
to perform some action when a module is included in another.

```ruby
module A
  def A.included(mod)
    puts "#{self} included #{mod}"
  end
end

module B
  include A
end
```

extended

extended(othermod)
> The equivalent of included, but for extended modules.

```ruby
module A 
  def self.extended(mod)
    puts "#{self} extended in #{mod}"
  end
end

module B
  extend A
end
```

extend_object
const_missing

[What Hooks Does Ruby Have for Metaprogramming?](http://codefol.io/posts/What-Hooks-does-Ruby-have-for-Metaprogramming)
[Ruby Meat Programming](https://gist.github.com/Integralist/a29212a8eb10bc8154b7)


https://ruby-doc.org/core-2.2.0/Class.html
https://ruby-doc.org/core-2.2.0/Module.html
