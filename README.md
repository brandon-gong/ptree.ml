# ptree.ml

_For documentation, see [here]()_.

ptree.ml is a "universal adaptor" between the extraordinarily popular data
serialization formats INI, JSON, and XML for OCaml. It provides support for
parsing any of these formats into a unified data structure, making edits /
adding new data, and then exporting data back out to any of those file formats.

As such, it can be used as
- a single, familiar tool to parse multiple file formats (no need to learn a new
  API every time);
- a tool to convert between different file formats (e.g. read in an INI file,
	write it out as JSON);
- a way to easily serialize any data you have (not just modifying pre-existing
	data from another file – you can easily build up your own tree from scratch
	and write it to whatever format you like).

(The name is a nod to C++'s
[Boost.PropertyTree](https://www.boost.org/doc/libs/1_65_1/doc/html/property_tree.html),
which inspired this project.)

## Some more background
The problem of _serializing_ data – that is, converting some abstract object in
a program's memory into a format that can be easily saved in a file or
transmitted – is almost as old as computers themselves. As such, over the
decades many different formats have emerged (INI being a particularly early
format, XML bringing more flexibility at the cost of more complexity and
verbosity, JSON being kind of a middle ground between the two). These formats
still remain pervasive today: INI is used by many Windows programs, and a
similar format is used in `.conf` files on Unix systems; XML is hugely popular
and used by projects such as React Native and Maven; JSON is almost ubiquitous
as a communication format between servers and browsers, as well as being a
general-purpose storage format just as flexible as XML.

Here's a small sample of the same data shown in INI, then XML, then JSON.
```ini
[person]
name = Brandon Gong
is_cool = true
favorite_number = 3.1415
```
```xml
<person>
	<name>Brandon Gong</name>
	<is_cool>true</is_cool>
	<favorite_number>3.1415</favorite_number>
</person>
```
```json
{
	"person": {
		"name": "Brandon Gong",
		"is_cool": true,
		"favorite_number": 3.1415
	}
}
```
Although the three formats appear very different, at heart what they are all
doing is the same: structuring different properties about a certain thing into
a tree, with different categories and subcategories storing various information.

Thus, it makes sense to be able to parse (or _deserialize_) all three of these
file formats into ultimately the same data structure: a _property tree_. It also
makes sense that, given a property tree, we can always turn it back into XML or
JSON; the information is still the same, just some syntactic details have
changed.

This is the central idea behind this library.

ptree.ml views the property tree as a file system, i.e. a set of different
folders (the categories) that can contain files (the properties) or other
folders nested within them. If you're familiar with the Unix/Linux terminal, the
API will be very intuitive to use; much like working in the terminal, you'll be
able to navigate throughout the tree with `cd`, delete properties with `rm`,
rename properties with `mv`, and more.

This intuitive, context-aware editing flow is accomplished all in a pure
functional manner using a Zipper data structure. Thus, editing ptrees is fast,
efficient, and thread-safe.

## Examples

## Caveats