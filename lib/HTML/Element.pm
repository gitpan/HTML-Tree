
require 5;
# Time-stamp: "2000-05-18 23:45:57 MDT"
package HTML::Element;

# TODO: add 'are_element_identical' method ?
# TODO: add 'are_content_identical' method ?
# TODO: maybe alias ->destroy to ->delete ?
# TODO: have use a HTML::Tagset or something (to write)
# TODO: have address() take relative address. (".3.1")

=head1 NAME

HTML::Element - Class for objects that represent HTML elements

=head1 SYNOPSIS

  use HTML::Element;
  $a = HTML::Element->new('a', href => 'http://www.perl.com/');
  $a->push_content("The Perl Homepage");

  $tag = $a->tag;
  print "$tag starts out as:",  $a->starttag, "\n";
  print "$tag ends as:",  $a->endtag, "\n";
  print "$tag\'s href attribute is: ", $a->attr('href'), "\n";

  $links_r = $a->extract_links();
  print "Hey, I found ", scalar(@$links_r), " links.\n";
  
  print "And that, as HTML, is: ", $a->as_HTML, "\n";
  $a = $a->delete;

=head1 DESCRIPTION

Objects of the HTML::Element class can be used to represent elements
of HTML.  These objects have attributes, notably attributes that
designates the elements's parent and content.  The content is an array
of text segments and other HTML::Element objects.  A tree with HTML::Element
objects as nodes can represent the syntax tree for a HTML document.

=head1 HOW WE REPRESENT TREES

It may occur to you to wonder what exactly a "tree" is, and how
it's represented in memory.  Consider this HTML document:

  <html lang='en-US'>
    <head>
      <title>Stuff</title>
      <meta name='author' content='Jojo'>
    </head>
    <body>
     <h1>I like potatoes!</h1>
    </body>
  </html>

Building a syntax tree out of it makes a tree-structure in memory
that could be diagrammed as:

                     html (lang='en-US')
                      / \
                    /     \
                  /         \
                head        body
               /\               \
             /    \               \
           /        \               \
         title     meta              h1
          |       (name='author',     |
       "Stuff"    content='Jojo')    "I like potatoes"

This is the traditional way to diagram a tree, with the "root" at the
top, and it's this kind of diagram that people have in mind when they
say, for example, that "the meta element is under the head element
instead of under the body element".  (The same is also said with
"inside" instead of "under" -- the use of "inside" makes more sense
when you're looking at the HTML source.)

Another way to represent the above tree is with indenting:

  html (attributes: lang='en-US')
    head
      title
        "Stuff"
      meta (attributes: name='author' content='Jojo')
    body
      h1
        "I like potatoes"

Incidentally, diagramming with indenting works much better for very
large trees, and is easier for a program to generate.  The $tree->dump
method uses indentation just that way.

However you diagram the tree, it's stored the same in memory -- it's a
network of objects, each of which has attributes like so:

  element #1:  _tag: 'html'
               _parent: none
               _content: [element #2, element #5]
               lang: 'en-US'

  element #2:  _tag: 'head'
               _parent: element #1
               _content: [element #3, element #4]

  element #3:  _tag: 'title'
               _parent: element #2
               _content: [text segment "Stuff"]

  element #4   _tag: 'meta'
               _parent: element #2
               _content: none
               name: author
               content: Jojo

  element #5   _tag: 'body'
               _parent: element #1
               _content: [element #6]

  element #6   _tag: 'h1'
               _parent: element #5
               _content: [text segment "I like potatoes"]

The "treeness" of the tree-structure that these elements comprise is
not an aspect of any particular object, but is emergent from the
relatedness attributes (_parent and _content) of these element-objects
and from how you use them to get from element to element.

While you could access the content of a tree by writing code that says
"access the 'src' attribute of the root's I<first> child's I<seventh>
child's I<third> child", you're more likely to have to scan the contents
of a tree, looking for whatever nodes, or kinds of nodes, you want to
do something with.  The most straightforward way to look over a tree
is to "traverse" it; an HTML::Element method ($h->traverse) is
provided for this purpose; and several other HTML::Element methods are
based on it.

(For everything you ever wanted to know about trees, and then some,
see Niklaus Wirth's I<Algorithms + Data Structures = Programs> or
Donald Knuth's I<The Art of Computer Programming, Volume 1>.)

=cut


use strict;
use Carp ();
use HTML::Entities ();
use integer; # vroom vroom!

use vars qw($VERSION
            $html_uc
            %emptyElement %optionalEndTag %linkElements %boolean_attr
           );

$VERSION = '1.54';
sub Version { $VERSION; }

# Constants for signalling back to the traverser:
my $travsignal_package = __PACKAGE__ . '::_travsignal';
my(
  $ABORT, $PRUNE, $PRUNE_SOFTLY, $OK, $PRUNE_UP
) =
  map
   {my $x = $_ ; bless \$x, $travsignal_package;}
   qw(
     ABORT  PRUNE   PRUNE_SOFTLY   OK   PRUNE_UP
   )
;
sub ABORT           () {$ABORT}
sub PRUNE           () {$PRUNE}
sub PRUNE_SOFTLY    () {$PRUNE_SOFTLY}
sub OK              () {$OK}
sub PRUNE_UP        () {$PRUNE_UP}

$html_uc = 0;
# set to 1 if you want tag and attribute names from starttag and endtag
#  to be uc'd

# Elements that does not have corresponding end tags (i.e. are empty)
%emptyElement   = map { $_ => 1 } qw(base link meta isindex
                                     img br hr wbr
                                     input area param
                                     embed bgsound spacer
                                     basefont col frame
                                     ~comment ~literal
                                     ~declaration ~pi
                                    );
%optionalEndTag = map { $_ => 1 } qw(p li dt dd); # option th tr td);

# Elements that might contain links and the name of the link attribute
%linkElements =
(
 body   => 'background',
 base   => 'href',
 a      => 'href',
 img    => [qw(src lowsrc usemap)],   # lowsrc is a Netscape invention
 form   => 'action',
 input  => 'src',
'link'  => 'href',          # need quoting since link is a perl builtin
 frame  => 'src',
 applet => 'codebase',
 area   => 'href',
);
      #TODO : update the above from Extor


# These attributes are normally printed without showing the "='value'".
# If there's just one such attribute for a given tag, just represent with
# "tag => attribute"; otherwise, use "tag => {attr1 => 1, attr2 => 1}"
%boolean_attr = (
 area   => 'nohref',
 dir    => 'compact',
 dl     => 'compact',
 hr     => 'noshade',
 img    => 'ismap',
 input  => { checked => 1, readonly => 1, disabled => 1 },
 menu   => 'compact',
 ol     => 'compact',
 option => 'selected',
'select'=> 'multiple',
 td     => 'nowrap',
 th     => 'nowrap',
 ul     => 'compact',
);

#==========================================================================


=head1 BASIC METHODS

=over 4

=item $h = HTML::Element->new('tag', 'attrname' => 'value', ... )

This constructor method returns a new HTML::Element object.  The tag
name is a required argument; it will be forced to lowercase.
Optionally, you can specify other initial attributes at object
creation time.

=cut

#
# An HTML::Element is represented by blessed hash reference, much like
# Tree::DAG_Node objects.  Key-names not starting with '_' are reserved
# for the SGML attributes of the element.
# The following special keys are used:
#
#    '_tag':    The tag name (i.e., the generic identifier)
#    '_parent': A reference to the HTML::Element above (when forming a tree)
#    '_pos':    The current position (a reference to a HTML::Element) is
#               where inserts will be placed (look at the insert_element
#               method)  If not set, the implicit value is the object itself.
#    '_content': A ref to an array of nodes under this.
#                It might not be set.
#
# Example: <img src="gisle.jpg" alt="Gisle's photo"> is represented like this:
#
#  bless {
#     _tag => 'img',
#     src  => 'gisle.jpg',
#     alt  => "Gisle's photo",
#  }, 'HTML::Element';
#

sub new
{
    my $class = shift;
    $class = ref($class) || $class;

    my $tag   = shift;
    Carp::croak("No tagname") unless defined $tag and length $tag;
    my $self  = bless { _tag => lc $tag }, $class;
    my($attr, $val);
    while (($attr, $val) = splice(@_, 0, 2)) {
        $val = $attr unless defined $val;
        $self->{lc $attr} = $val;
    }
    if ($tag eq 'html') {
        $self->{'_pos'} = undef;
    }
    $self;
}


=item $h->attr('attr') or $h->attr('attr', 'value')

Returns (optionally sets) the value of the given attribute of $h.  The
attribute name (but not the value, if provided) is forced to
lowercase.  If trying to read the value of an attribute not present
for this element, the return value is undef.
If setting a new value, the old value of that attribute is
returned.

If methods are provided for accessing an attribute (like $h->tag for
"_tag", $h->content_list, etc. below), use those instead of calling
attr $h->attr, whether for reading or setting.

Note that setting an attribute to undef (as opposed to "", the empty
string) actually deletes the attribute.

=cut

sub attr
{
    my $self = shift;
    my $attr = lc shift;
    if (@_) {  # set
	if(defined $_[0]) {
	    my $old = $self->{$attr};
	    $self->{$attr} = $_[0];
	    return $old;
	} else {  # delete, actually
	    return delete $self->{$attr};
	}
    } else {   # get
        return $self->{$attr};
    }
}


=item $h->tag() or $h->tag('tagname')

Returns (optionally sets) the tag name (also known as the generic
identifier) for the element $h.  In setting, the tag name is always
converted to lower case.

=cut

sub tag
{
    my $self = shift;
    if (@_) { # set
    #print "SET\n";
        $self->{'_tag'} = lc $_[0];
    } else { # get
    #print "GET\n";
        $self->{'_tag'};
    }
}


=item $h->parent() or $h->parent($new_parent)

Returns (optionally sets) the parent for this element.
The parent should either be undef, or should be another element.

You B<should not> use this to directly set the parent of an element.
Instead use any of the other methods under "Structure-Modifying
Methods", below.

Note that not($h->parent) is a simple test for whether $h is the
root of its subtree.

=cut

sub parent
{
    my $self = shift;
    if (@_) { # set
        Carp::croak "an element can't be made its own parent"
         if defined $_[0] and ref $_[0] and $self eq $_[0]; # sanity
        $self->{'_parent'} = $_[0];
    } else {
        $self->{'_parent'}; # get
    }
}


=item $h->content_list()

Returns a list representing the content of this element -- i.e., what
nodes (elements or text segments) are inside/under this element. (Note
that this may be an empty list.)

In a scalar context, this returns the count of the items,
as you may expect.

=cut

sub content_list
{
    return
      wantarray ?        @{shift->{'_content'} || return()}
                : scalar @{shift->{'_content'} || return 0};
}



=item $h->content()

This somewhat deprecated method returns the content of this element;
but unlike content_list, this returns either undef (which you should
understand to mean no content), or a I<reference to the array> of
content items, each of which is either a text segment (a string, i.e.,
a defined non-reference scalar value), or an HTML::Element object.
Note that even if an arrayref is returned, it may be a reference to an
empty array.

While older code should feel free to continue to use $h->content,
new code should use $h->content_list in almost all conceivable
cases.  It is my experience that in most cases this leads to simpler
code anyway, since it means one can say:

  @children = $h->content_list;

instead of the inelegant:

  @children = @{$h->content || []};

If you do use $h->content, you should not use the reference returned
by it (assuming it returned a reference, and not undef) to directly
set or change the content of an element!  Instead use any of the other
methods under "Structure-Modifying Methods", below.

=cut

# a read-only method!  can't say $h->content( [] )!
sub content
{
    shift->{'_content'};
}


=item $h->implicit() or $h->implicit($bool)

Returns (optionally sets) the "_implicit" attribute.  This attribute is
a flag that's used for indicating that the element was not originally
present in the source, but was added to the parse tree (by
HTML::TreeBuilder, for example) in order to conform to the rules of
HTML structure.

=cut

sub implicit
{
    shift->attr('_implicit', @_);
}



=item $h->pos() or $h->pos($element)

Returns (and optionally sets) the "_pos" (for "current I<pos>ition")
pointer of $h.
This attribute is a pointer used during some parsing operations,
whose value is whatever HTML::Element element at or under $h is
currently "open", where $h->insert_element(NEW) will actually insert a
new element.

(This has nothing to do with the Perl function called "pos", for
controlling where regular expression matching starts.)

If you set $h->pos($element), be sure that $element is either $h, or
an element under $h.

If you've been modifying the tree under $h and are
no longer sure $h->pos is valid, you can enforce validity with:

    $h->pos(undef) unless $h->pos->is_inside($h);

=cut

sub pos
{
    my $self = shift;
    my $pos = $self->{'_pos'};
    if (@_) {  # set
        if(defined $_[0] and $_[0] ne $self) {
          $self->{'_pos'} = $_[0]; # means that element
        } else {
          $self->{'_pos'} = undef; # means $self
        }
    }
    return $pos if defined($pos);
    $self;
}


=item $h->all_attr()

Returns all this element's attributes and values, as key-value pairs.
This will include some "internal" attributes (i.e., ones not present
in the original element, and which will not be represented if/when you
call $h->as_HTML).  Internal attributes are distinguished by the fact
that the first character of their key (not value, key!) is an
underscore ("_").

=cut

sub all_attr {
  return %{$_[0]};
  # Yes, trivial.  But no other way for the user to do the same
  #  without breaking encapsulation.
  # And if our object representation changes, this method's behavior
  #  should stay the same.
}


=item $h->all_external_attr()

Like C<all_attr>, except that internal attributes are not present.

=cut

sub all_external_attr {
  my $self = $_[0];
  return
    map(
	(length($_) && substr($_,0,1) ne '_') ? ($self->{$_}) : (),
	keys %$self
       );
}

#==========================================================================

=back

=head1 STRUCTURE-MODIFYING METHODS

These methods are provided for modifying the content of trees
by adding or changing nodes as parents or children of other nodes.

=over 4

=item $h->push_content($element_or_text, ...)

Adds the specified items to the I<end> of the content list of the
element $h.  The items of content to be added should each be either a
text segment (a string) or an HTML::Element object.

The push_content method will try to consolidate adjacent text segments
while adding to the content list.  That's to say, if $h's content_list is

  ('foo bar ', $some_node, 'baz!')

and you call

   $h->push_content('quack?');

then the resulting content list will be this:

  ('foo bar ', $some_node, 'baz!quack?')

and not this:

  ('foo bar ', $some_node, 'baz!', 'quack?')

If that latter is what you want, you'll have to override the
feature of consolidating text by using splice_content,
as in:

  $h->splice_content(scalar($h->content_list),0,'quack?');

Similarly, if you wanted to add 'Skronk' to the beginning of
the content list, calling this:

   $h->push_content('Skronk');

then the resulting content list will be this:

  ('Skronkfoo bar ', $some_node, 'baz!')

and not this:

  ('Skronk', 'foo bar ', $some_node, 'baz!')

What you'd to do get the latter is:

  $h->splice_content(0,0,'Skronk');

=cut

sub push_content
{
    my $self = shift;
    return $self unless @_;

    my $content = ($self->{'_content'} ||= []);
    for (@_) {
        if (ref $_) {  # insert an element
            $_->detach if $_->{'_parent'};
            $_->{'_parent'} = $self;
            push(@$content, $_);
        } else {  # insert text segment
            if (@$content && !ref $content->[-1]) {
                # last content element is also text segment -- append
                $content->[-1] .= $_;
            } else {
                push(@$content, $_);
            }
        }
    }
    $self;
}


=item $h->unshift_content($element_or_text, ...)

Adds the specified items to the I<beginning> of the content list of
the element $h.  The items of content to be added should each be
either a text segment (a string) or an HTML::Element object.

The unshift_content method will try to consolidate adjacent text segments
while adding to the content list.  See above for a discussion of this.

=cut

sub unshift_content
{
    my $self = shift;
    return $self unless @_;

    my $content = ($self->{'_content'} ||= []);
    for (reverse @_) { # so they get added in the order specified
        if (ref $_) {  # insert an element
            $_->detach if $_->{'_parent'};
            $_->{'_parent'} = $self;
            unshift(@$content, $_);
        } else {  # insert text segment
            if (@$content && !ref $content->[0]) {
                # last content element is also text segment -- prepend
                $content->[0]  = $_ . $content->[0];
            } else {
                unshift(@$content, $_);
            }
        }
    }
    $self;
}

#  splice ARRAY,OFFSET,LENGTH,LIST

=item $h->splice_content($offset, $length, $element_or_text, ...)

Detaches the elements from $h's list of content-nodes, starting at
$offset and continuing for $length items, replacing them with the
elements of the following list, if any.  Returns the elements (if any)
removed from the content-list.  If $offset is negative, then it starts
that far from the end of the array, just like Perl's normal C<splice>
function.  If $length and the following list is omitted, removes
everything from $offset onward.

The items of content to be added (if any) should each be either a text
segment (a string), or an HTML::Element object that's not already
a child of $h.

=cut

sub splice_content {
    my($self, $offset, $length, @to_add) = @_;
    Carp::croak
      "splice_content requires at least one argument"
      if @_ < 2;  # at least $h->splice_content($offset);
    return $self unless @_;

    my $content = ($self->{'_content'} ||= []);
    # prep the list

    my @out;
    if(@_ > 2) {  # self, offset, length, ...
      foreach my $n (@to_add) {
        if(ref($n)) {
          $n->detach;
          $n->{'_parent'} = $self;
        }
      }
      @out = splice @$content, $offset, $length, @to_add;
    } else {  #  self, offset
      @out = splice @$content, $offset;
    }
    foreach my $n (@out) {
      $n->{'_parent'} = undef if ref $n;
    }
    return @out;
}


=item $h->detach()

This unlinks $h from its parent, by setting its 'parent' attribute to
undef, and by removing it from the content list of its parent (if it
had one).  The return value is the parent that was detached from (or
undef, if $h had no parent to start with).  Note that neither $h nor
its parent are explicitly destroyed.

=cut

sub detach {
  my $self = $_[0];
  return undef unless(my $parent = $self->{'_parent'});
  $self->{'_parent'} = undef;
  my $cohort = $parent->{'_content'} || return $parent;
  @$cohort = grep { not( ref($_) and $_ eq $self) } @$cohort;
    # filter $self out, if parent has any evident content
  
  return $parent;
}


=item $h->detach_content()

This unlinks $h all of $h's children from $h, and returns them.
Note that these are not explicitly destroyed; for that, you
can just use $h->delete_content.

=cut

sub detach_content {
  my $c = $_[0]->{'_content'} || return(); # in case of no content
  for (@$c) { $_->{'_parent'} = undef if ref $_; }
  return splice @$c;
}


=item $h->replace_with( $element_or_text, ... ) 

This replaces $h in its parent's content list with the nodes specified.
The element $h (which by then may have no parent) is
returned.  This causes a fatal error if $h has no parent.  
The list of nodes to insert may contain $h, but at most once.
Aside from that possible exception, the nodes to insert should not
already be children of $h's parent.

Also, note that this method does not destroy $h -- use
$h->replace_with(...)->delete if you need that.

=cut

sub replace_with {
  my($self, @replacers) = @_;
  Carp::croak "the target node has no parent"
    unless my($parent) = $self->{'_parent'};

  my $parent_content = $parent->{'_content'};
  Carp::croak "the target node's parent has no content!?" 
   unless $parent_content and @$parent_content;
  
  my $replacers_contains_self;
  for(@replacers) {
    if(!ref $_) {
      # noop
    } elsif($_ eq $self) {
      # noop, but check that it's there just once.
      Carp::croak 
        "Replacement list contains several copies of target!"
       if $replacers_contains_self++;
    } elsif($_ eq $parent) {
      Carp::croak "Can't replace an item with its parent!";
    } else {
      $_->detach;
      $_->{'_parent'} = $parent;
      # each of these are necessary
    }
  }
  
  #my $content_r = $self->{'_content'} || [];
  @$parent_content 
   = map { ( ref($_) and $_ eq $self) ? @replacers : $_ }
         @$parent_content
  ;
  
  $self->{'_parent'} = undef unless $replacers_contains_self;
   # if replacers does contain self, then the parent attribute is fine as-is
  
  return $self;
}

=item $h->preinsert($element_or_text...)

Inserts the given nodes right BEFORE $h in $h's parent's content list.
This causes a fatal error if $h has no parent.  None of the
given nodes should be $h or other children of $h.  Returns $h.

=cut

sub preinsert {
  my $self = shift;
  return $self unless @_;
  return $self->replace_with(@_, $self);
}

=item $h->postinsert($element_or_text...)

Inserts the given nodes right AFTER $h in $h's parent's content list.
This causes a fatal error if $h has no parent.  None of the
given nodes should be $h or other children of $h.  Returns $h.

=cut

sub postinsert {
  my $self = shift;
  return $self unless @_;
  return $self->replace_with($self, @_);
}


=item $h->replace_with_content()

This replaces $h in its parent's content list with its own content.
The element $h (which by then has no parent or content of its own) is
returned.  This causes a fatal error if $h has no parent.  Also, note
that this does not destroy $h -- use $h->replace_with_content->delete
if you need that.

=cut

sub replace_with_content {
  my $self = $_[0];
  Carp::croak "the target node has no parent"
    unless my($parent) = $self->{'_parent'};

  my $parent_content = $parent->{'_content'};
  Carp::croak "the target node's parent has no content!?" 
   unless $parent_content and @$parent_content;

  my $content_r = $self->{'_content'} || [];
  @$parent_content 
   = map { ( ref($_) and $_ eq $self) ? @$content_r : $_ }
         @$parent_content
  ;

  $self->{'_parent'} = undef; # detach $self from its parent

  # Update parentage link, removing from $self's content list
  for (splice @$content_r) {  $_->{'_parent'} = $parent if ref $_ }

  return $self;  # note: doesn't destroy it.
}



=item $h->delete_content()

Clears the content of $h, calling $i->delete for each content element.
Compare with $h->detach_content.

Returns $h.

=cut

sub delete_content
{
    for (splice @{ delete($_[0]->{'_content'})
              # Deleting it here (while holding its value, for the moment)
              #  will keep calls to detach() from trying to uselessly filter
              #  the list (as they won't be able to see it once it's been
              #  deleted)
            || return($_[0]) # in case of no content
          },
          0
           # the splice is so we can null the array too, just in case
           # something somewhere holds a ref to it
        )
    {
        $_->delete if ref $_;
    }
    $_[0];
}



=item $h->delete()

Detaches this element from its parent (if it has one) and explicitly
destroys the element and all its descendants.  The return value is
undef.

Perl uses garbage collection based on reference counting; when no
references to a data structure exist, it's implicitly destroyed --
i.e., when no value anywhere points to a given object anymore, Perl
knows it can free up the memory that the now-unused object occupies.

But this fails with HTML::Element trees, because a parent element
always holds references to its children, and its children elements
hold references to the parent, so no element ever looks like it's
I<not> in use.  So, to destroy those elements, you need to call
$h->delete on the parent.

=cut
#'

sub delete
{
    my $self = $_[0];
    $self->delete_content   # recurse down
     if $self->{'_content'} && @{$self->{'_content'}};
    
    $self->detach if $self->{'_parent'} and $self->{'_parent'}{'_content'};

    %$self = (); # null out the whole object on the way out
    return undef;
}



=item $h->clone()

Returns a copy of the element (whose children are clones (recursively)
of the original's children, if any).

The returned element is parentless.  Any '_pos' attributes present in the
source element/tree will be absent in the copy.  For that and other reasons,
the clone of an HTML::TreeBuilder object that's in mid-parse (i.e, the head
of a tree that HTML::TreeBuilder is elaborating) cannot (currently) be used
to continue the parse.

You are free to clone HTML::TreeBuilder trees, just as long as:
1) they're done being parsed, or 2) you don't expect to resume parsing
into the clone.  (You can continue parsing into the original; it is
never affected.)

=cut

sub clone {
  #print "Cloning $_[0]\n";
  my $it = shift;
  Carp::croak "clone() can be called only as an object method" unless ref $it;
  Carp::croak "clone() takes no arguments" if @_;

  my $new = bless { %$it }, ref($it);     # COPY!!! HOOBOY!
  delete @$new{'_content', '_parent', '_pos', '_head', '_body'};
  
  # clone any contents
  $new->{'_content'} = [  ref($it)->clone_list( @{$it->{'_content'}} )  ]
   if $it->{'_content'} and @{$it->{'_content'}};

  return $new;
}

=item HTML::Element->clone_list(...nodes...)

=item or: ref($h)->clone_list(...nodes...)

Returns a list consisting of a copy of each node given.
Text segments are simply copied; elements are cloned by
calling $it->clone on each of them.

=cut

sub clone_list {
  Carp::croak "I can be called only as a class method" if ref shift @_;
  
   # all that does is get me here
  return
    map
      {
        ref($_)
          ? $_->clone   # copy by method
          : $_  # copy by evaluation
      }
      @_
  ;
}


=item $h->normalize_content

Normalizes the content of $h -- i.e., concatenates any adjacent text nodes.
(Any undefined text segments are turned into empty-strings.)
Note that this does not recurse into $h's descendants.

=cut

sub normalize_content {
  my $start = $_[0];
  my $c;
  return unless $c = $start->{'_content'} and ref $c and @$c; # nothing to do
  # TODO: if we start having text elements, deal with catenating those too?
  my @stretches = (undef); # start with a barrier

  # I suppose this could be rewritten to treat stretches as it goes, instead
  #  of at the end.  But feh.

  # Scan:
  for(my $i = 0; $i < @$c; ++$i) {
    if(defined $c->[$i] and ref $c->[$i]) { # not a text segment
      if($stretches[0]) {
	# put in a barrier
	if($stretches[0][1] == 1) {
	  #print "Nixing stretch at ", $i-1, "\n";
	  undef $stretches[0]; # nix the previous one-node "stretch"
	} else {
	  #print "End of stretch at ", $i-1, "\n";
	  unshift @stretches, undef
	}
      }
      # else no need for a barrier
    } else { # text segment
      $c->[$i] = '' unless defined $c->[$i];
      if($stretches[0]) {
	++$stretches[0][1]; # increase length
      } else {
	#print "New stretch at $i\n";
	unshift @stretches, [$i,1]; # start and length
      }
    }
  }

  # Now combine.  Note that @stretches is in reverse order, so the indexes
  # still make sense as we work our way thru (i.e., backwards thru $c).
  foreach my $s (@stretches) {
    if($s and $s->[1] > 1) {
      #print "Stretch at ", $s->[0], " for ", $s->[1], "\n";
      $c->[$s->[0]] .= join('', splice(@$c, $s->[0] + 1, $s->[1] - 1))
	# append the subsequent ones onto the first one.
    }
  }
  return;
}


=item $h->insert_element($element, $implicit)

Inserts (via push_content) a new element under the element at
$h->pos().  Then updates $h->pos() to point to the inserted element,
unless $element is a prototypically empty element like "br", "hr",
"img", etc.  The new $h->pos() is returned.  This method is useful
only if your particular tree task involves setting $h->pos.

=cut

sub insert_element
{
    my($self, $tag, $implicit) = @_;
    return $self->pos() unless $tag; # noop if nothing to insert

    my $e;
    if (ref $tag) {
        $e = $tag;
        $tag = $e->tag;
    } else { # just a tag name -- so make the element
        $e = HTML::Element->new($tag)
    }

    $e->{'_implicit'} = 1 if $implicit;

    my $pos = $self->{'_pos'};
    $pos = $self unless defined $pos;

    $pos->push_content($e);

    $self->{'_pos'} = $pos = $e
      unless $emptyElement{$tag} || $e->{'_empty_element'};

    $pos;
}

#==========================================================================

=back

=head1 DUMPING METHODS

=over 4

=item $h->dump()

Prints the element and all its children to STDOUT, in a format useful
only for debugging.  The structure of the document is shown by
indentation (no end tags).

=cut

sub dump
{
    my $self = shift;
    my $depth = shift || 0;
    print STDOUT
      "  " x $depth,   $self->starttag,   " \@", $self->address,
      $self->{'_implicit'} ? " (IMPLICIT)\n" : "\n";
    for (@{$self->{'_content'}}) {
        if (ref $_) {  # element
            $_->dump($depth+1);  # recurse
        } else {  # text node
            print STDOUT "  " x ($depth + 1);
            if(length($_) > 65 or m<[\x00-\x1F]>) {
              # it needs prettyin' up somehow or other
              my $x = (length($_) <= 65) ? $_ : (substr($_,0,65) . '...');
              $x =~ s<([\x00-\x1F])>
                     <'\\x'.(unpack("H2",$1))>eg;
              print STDOUT qq{"$x"\n};
            } else {
              print STDOUT qq{"$_"\n};
            }
        }
    }
}


=item $h->as_HTML() or $h->as_HTML($entities)

=item or $h->as_HTML($entities, $indent_char)

Returns a string representing in HTML the element and its
children.  The optional argument C<$entities> specifies a string of
the entities to encode.  For compatibility with previous versions,
specify C<'E<lt>E<gt>&'> here.  If omitted or undef, I<all> unsafe
characters are encoded as HTML entities.  See L<HTML::Entities> for
details.

If $indent_char is specified and defined, the HTML to be output is
intented, using the string you specify (which you probably should
set to "\t", or some number of spaces, if you specify it).  This
feature is currently somewhat experimental.  But try it, and feel
free to email me any bug reports.  (Note that output, although
indented, is not wrapped.  Patches welcome.)

=cut

sub as_HTML
{
  my($self, $entities, $indent) = @_;
  #my $indent_on = defined($indent) && length($indent);
  my @html = ();
  
  my $last_tag_tightenable = 0;
  my $this_tag_tightenable = 0;
  
  my $nonindentable_ancestors = 0;  # count of nonindentible tags over us.
  
  my($tag, $node, $start, $depth); # per-iteration scratch
  
  if(defined($indent) && length($indent)) {
    unless(defined $HTML::TreeBuilder::VERSION) {
      require HTML::TreeBuilder or die "Can't require HTML::TreeBuilder";
    }
    #require Text::Wrap;
    $self->traverse(
      sub {
        ($node, $start, $depth) = @_;
        if(ref $node) { # it's an element
           
           $tag = $node->tag;
           
           if($start) { # on the way in
             if(
                ($this_tag_tightenable = $HTML::TreeBuilder::canTighten{$tag})
                and !$nonindentable_ancestors
                and $last_tag_tightenable
             ) {
               push
                 @html,
                 "\n",
                 $indent x $depth,
                 $node->starttag($entities),
               ;
             } else {
               push(@html, $node->starttag($entities));
             }
             $last_tag_tightenable = $this_tag_tightenable;
             
             ++$nonindentable_ancestors
               if $tag eq 'pre' or $tag eq 'xmp' or
                $tag eq 'listing' or $tag eq 'plaintext' or 
                $tag eq 'script'
             ;
             
           } elsif (not($emptyElement{$tag} or $optionalEndTag{$tag})) {
             # on the way out
             if($tag eq 'pre' or $tag eq 'xmp' or
                $tag eq 'listing' or $tag eq 'plaintext' or
                $tag eq 'script'
             ) {
               --$nonindentable_ancestors;
               $last_tag_tightenable = $HTML::TreeBuilder::canTighten{$tag};
               push @html, $node->endtag;
               
             } else { # general case
               if(
                  ($this_tag_tightenable = $HTML::TreeBuilder::canTighten{$tag})
                  and !$nonindentable_ancestors
                  and $last_tag_tightenable
               ) {
                 push
                   @html,
                   "\n",
                   $indent x $depth,
                   $node->endtag,
                 ;
               } else {
                 push @html, $node->endtag;
               }
               $last_tag_tightenable = $this_tag_tightenable;
               #print "$tag tightenable: $this_tag_tightenable\n";
             }
           }
        } else {  # it's a text segment
        
          $last_tag_tightenable = 0;  # I guess this is right
          HTML::Entities::encode_entities($node, $entities);
            # that does magic things if $entities is undef
          if($nonindentable_ancestors) {
            push @html, $node; # say no go
          } else {
            if($last_tag_tightenable) {
              $node =~ s<\s+>< >s;
              #$node =~ s< $><>s;
              $node =~ s<^ ><>s;
              push
                @html,
                "\n",
                $indent x $depth,
                $node,
                #Text::Wrap::wrap($indent x $depth, $indent x $depth, "\n" . $node)
              ;
            } else {
              push
                @html,
                $node,
                #Text::Wrap::wrap('', $indent x $depth, $node)
              ;
            }
          }
        }
        1; # keep traversing
      }
    );
    
  } else { # no indenting -- much simpler code
    $self->traverse(
      sub {
          ($node, $start, $depth) = @_;
          if(ref $node) {
            $tag = $node->tag;
            if($start) { # on the way in
              push(@html, $node->starttag($entities));
            } elsif (not($emptyElement{$tag} or $optionalEndTag{$tag})) {
              # on the way out
              push(@html, $node->endtag);
            }
          } else {
            # simple text content
            HTML::Entities::encode_entities($node, $entities);
              # that does magic things if $entities is undef
            push(@html, $node);
          }
         1; # keep traversing
        }
    );
  }
  
  join('', @html, "\n");
}



=item $h->as_text()

=item $h->as_text(skip_dels => 1)

Returns a string consisting of only the text parts of the element's
descendants.  Entities are decoded to corresponding ISO-8859-1
(Latin-1) characters.  See L<HTML::Entities> for more information.

If C<skip_dels> is true, then text content under "del" nodes is not
included in what's returned.

=cut

sub as_text {
    my($self,%options) = @_;
    
    my @text = ();
    my $skip_dels = $options{'skip_dels'} || 0;
    #print "Skip dels: $skip_dels\n";
    $self->traverse(
      [
         sub { # work only in pre-order
            if(ref $_[0]) {
                #print "Tag: $_[0]{'_tag'}\n";
                return 0 if $skip_dels and $_[1] and $_[0]{'_tag'} eq 'del';
                # else just fall thru to returning 1.
            } else {
                # simple text content
                push(@text, $_[0]); # copies it
                scalar HTML::Entities::decode_entities($text[-1]);
                 # decode in place
            }
            1;
         },
         undef # no post-order call
       ]
       # and don't ignore text nodes
    );
    join('', @text);
}



sub format
{
    my($self, $formatter) = @_;
    unless (defined $formatter) {
        require HTML::FormatText;
        $formatter = HTML::FormatText->new();
    }
    $formatter->format($self);
}



=item $h->starttag() or $h->starttag($entities)

Returns a string representing the complete start tag for the element.
I.e., leading "<", tag name, attributes, and trailing ">".  Attributes
values that don't consist entirely of digits are surrounded with
double-quotes, and appropriate characters are encoded.  If C<$entities>
is omitted or undef, I<all> unsafe characters are encoded as HTML
entities.  See L<HTML::Entities> for details.  If you specify some
value for C<$entities>, remember to include the double-quote character in
it.  (Previous versions of this module would basically behave as if
C<'&"E<gt>'> were specified for C<$entities>.)

=cut

sub starttag
{
    my($self, $entities) = @_;
    
    my $name = $self->{'_tag'};
    
    # TODO: document these...
    return        $self->{'text'}        if $name eq '~literal';
    
    return "<!" . $self->{'text'} . ">"  if $name eq '~declaration';
    
    return "<?" . $self->{'text'} . "?>" if $name eq '~pi';
    
    if($name eq '~comment') {
      if(ref($self->{'text'} || '') eq 'ARRAY') {
        return 
          "<!" .
          join(' ', map("--$_--", @{$self->{'text'}}))
          .  ">"
       ;
      } else {
        return "<!--" . $self->{'text'} . "-->"
      }
    }
    
    my $tag = $html_uc ? "<\U$name" : "<\L$name";
    my $val;
    for (sort keys %$self) { # predictable ordering
        next if m/^_/s;
        $val = $self->{$_};
        # Hm -- what to do if val is undef?
        # I suppose that shouldn't ever happen.
        if ($_ eq $val &&   # if attribute is boolean, for this element
            exists($boolean_attr{$name}) &&
            (ref($boolean_attr{$name}) ? $boolean_attr{$name}{$_} : 
                                         $boolean_attr{$name} eq $_)
        ) {
            $tag .= $html_uc ? " \U$_" : " \L$_";
        } else { # non-boolean attribute
            if ($val !~ m/^[0-9]+$/s) { # quote anything not purely numeric
              # Might as well double-quote everything, for simplicity's sake
              HTML::Entities::encode_entities($val, $entities);
              $val = qq{"$val"};
            }
            $tag .= $html_uc ? qq{ \U$_\E=$val} : qq{ \L$_\E=$val};
        }
    }
    "$tag>";
}



=item $h->endtag()

Returns a string representing the complete end tag for this element.
I.e., "</", tag name, and ">".

=cut

sub endtag
{
    $html_uc ? "</\U$_[0]->{'_tag'}>" : "</\L$_[0]->{'_tag'}>";
}


#==========================================================================

=back

=head1 THE TRAVERSER METHOD

The C<traverse()> method is the most important general method for accessing
the information in a tree.  It accepts the following syntaxes:

=over

=item $h->traverse(\&callback)

=item or $h->traverse(\&callback, $ignore_text)

=item or $h->traverse([\&pre_callback,\&post_callback], $ignore_text)

=back

These all mean to traverse the element and all of its children.  That
is, this method starts at node $h, "pre-order visits" $h, traverses its
children, and then will "post-order visit" $h.  "Visiting" means that
the callback routine is called, with these arguments:

    $_[0] : the node (element or text segment),
    $_[1] : a startflag, and
    $_[2] : the depth

If the $ignore_text parameter is given and true, then the pre-order
call I<will not> be happen for text content.

The startflag is 1 when we enter a node (i.e., in pre-order calls) and
0 when we leave the node (in post-order calls).

Note, however, that post-order calls don't happen for nodes that are
text segments or are elements that are prototypically empty (like "br",
"hr", etc.).

If we visit text nodes (i.e., unless $ignore_text is given and true),
then when text nodes are visited, we will also pass two extra
arguments to the callback:

    $_[3] : the element that's the parent
             of this text node
    $_[4] : the index of this text node
             in its parent's content list

Note that you can specify that the pre-order routine can
be a different routine from the post-order one:

    $h->traverse([\&pre_callback,\&post_callback], ...);

You can also specify that no post-order calls are to be made,
by providing a false value as the post-order routine:

    $h->traverse([ \&pre_callback,0 ], ...);

And similarly for suppressing pre-order callbacks:

    $h->traverse([ 0,\&post_callback ], ...);

Note that these two syntaxes specify the same operation:

    $h->traverse([\&foo,\&foo], ...);
    $h->traverse( \&foo       , ...);

The return values from calls to your pre- or post-order 
routines are significant, and are used to control recursion
into the tree.

These are the values you can return, listed in descending order
of my estimation of their usefulness:

=over

=item HTML::Element::OK, 1, or any other true value

...to keep on traversing.

Note that C<HTML::Element::OK> et
al are constants.  So if you're running under C<use strict>
(as I hope you are), and you say:
C<return HTML::Element::PRUEN>
the compiler will flag this as an error (an unallowable
bareword, specifically), whereas if you spell PRUNE correctly,
the compiler will not complain.

=item undef, 0, '0', '', or HTML::Element::PRUNE

...to block traversing under the current element's content.
(This is ignored if received from a post-order callback,
since by then the recursion has already happened.)
If this is returned by a pre-order callback, no
post-order callback for the current node will happen.

=item HTML::Element::ABORT

...to abort the whole traversal immediately.
This is often useful when you're looking for just the first
node in the tree that meets some criterion of yours.

=item HTML::Element::PRUNE_UP

...to abort continued traversal into this node and its parent
node.  No post-order callback for the current or parent
node will happen.

=item HTML::Element::PRUNE_SOFTLY

Like PRUNE, except that the post-order call for the current
node is not blocked.

=back

Almost every task to do with extracting information from a tree can be
expressed in terms of traverse operations (usually in only one pass,
and usually paying attention to only pre-order, or to only
post-order), or operations based on traversing. (In fact, many of the
other methods in this class are basically calls to traverse() with
particular arguments.)

The source code for HTML::Element and HTML::TreeBuilder contain
many examples of the use of the "traverse" method to gather
information about the content of trees and subtrees.

(Note: you should not change the structure of a tree I<while> you are
traversing it.)

=cut
#'

# This, ladies and germs, is an iterative implementation of a
# recursive algorithm.  DON'T TRY THIS AT HOME.
# Basically, the algorithm says:
#
# To traverse:
#   1: pre-order visit this node
#   2: traverse any children of this node
#   3: post-order visit this node, unless it's a text segment,
#       or a prototypically empty node (like "br", etc.)
# Add to that the consideration of the callbacks' return values,
# so you can block visitation of the children, or siblings, or
# abort the whole excursion, etc.
#
# So, why all this hassle with making the code iterative?
# It makes for real speed, because it eliminates the whole
# hassle of Perl having to allocate scratch space for each
# instance of the recursive sub.  Since the algorithm
# is basically simple (and not all recursive ones are!) and
# has few necessary lexicals (basically just the current node's
# content list, and the current position in it), it was relatively
# straightforward to store that information not as the frame
# of a sub, but as a stack, i.e., a simple Perl array (well, two
# of them, actually: one for content-listrefs, one for indexes of
# current position in each of those).

my $NIL = [];
sub traverse {
  my($start, $callback, $ignore_text) = @_;
  
  Carp::croak('must provide a callback for traverse()!')
   unless defined $callback and ref $callback;
  
  # Elementary type-checking:
  my($c_pre, $c_post);
  if(UNIVERSAL::isa($callback, 'CODE')) {
    $c_pre = $c_post = $callback;
  } elsif(UNIVERSAL::isa($callback,'ARRAY')) {
    ($c_pre, $c_post) = @$callback;
    Carp::croak("pre-order callback \"$c_pre\" is true but not a coderef!")
     if $c_pre and not UNIVERSAL::isa($c_pre, 'CODE');
    Carp::croak("pre-order callback \"$c_post\" is true but not a coderef!")
     if $c_post and not UNIVERSAL::isa($c_post, 'CODE');
    return $start unless $c_pre or $c_post;
     # otherwise there'd be nothing to actually do!
  } else {
    Carp::croak("$callback is not a known kind of reference")
     unless ref($callback);
  }
  
  my(@C) = [$start]; # a stack containing lists of children
  my(@I) = (-1); # initial value must be -1 for each list
    # a stack of indexes to current position in corresponding lists in @C
  # In each of these, 0 is the active point
  
  # scratch:
  my(
    $rv,   # return value of callback
    $this, # current node
    $content_r, # child list of $this
  );
  
  
  # THE BIG LOOP
  while(@C) {
    # Move to next item in this frame
    #print "Loop: \@C has ", scalar(@C), " frames: @C\n";
    if(!defined($I[0]) or ++$I[0] >= @{$C[0]}) {
      # We either went off the end of this list, or aborted the list
      # So call the post-order callback:
      if($c_post
         and defined $I[0]
         and @C > 1
          # to keep the next line from autovivifying
         and defined($this = $C[1][ $I[1] ]) # sanity, and
          # suppress callbacks on exiting the fictional top frame
         and ref($this) # sanity
         and not(
                 $this->{'_empty_element'}
                 || $emptyElement{$this->{'_tag'} || ''}
                ) # things that don't get post-order callbacks
      ) {
        shift @I;
        shift @C;
        #print "Post! at depth", scalar(@I), "\n";
        $rv = $c_post->(
           #map $_, # copy to avoid any messiness
           $this,           # 0: this
           0,               # 1: startflag (0 for post-order call)
           @I - 1,          # 2: depth
        );
        
        if(defined($rv) and ref($rv) eq $travsignal_package) {
          $rv = $$rv; #deref
          if($rv eq 'ABORT') {
            last; # end of this excursion!
          } elsif($rv eq 'PRUNE') {
            # NOOP on post!!
          } elsif($rv eq 'PRUNE_SOFTLY') {
            # NOOP on post!!
          } elsif($rv eq 'OK') {
            # noop
          } elsif($rv eq 'PRUNE_UP') {
            $I[0] = undef;
          } else {
            die "Unknown travsignal $rv\n";
            # should never happen
          }
        }
        
      } else {
        #print "Oomph.  Callback suppressed\n";
        shift @I;
        shift @C;
      }
      next;
    }
    
    $this = $C[0][ $I[0] ];
    
    if($c_pre) {
      if(defined $this and ref $this) { # element
        $rv = $c_pre->(
           #map $_, # copy to avoid any messiness
           $this,           # 0: this
           1,               # 1: startflag (1 for pre-order call)
           @I - 1,          # 2: depth
        );
      } else { # text segment
        next if $ignore_text;
        $rv = $c_pre->(
           #map $_, # copy to avoid any messiness
           $this,           # 0: this
           1,               # 1: startflag (1 for pre-order call)
           @I - 1,          # 2: depth
           $C[1][ $I[1] ],  # 3: parent
               # And there will always be a $C[1], since
               #  we can't start traversing at a text node
           $I[0]            # 4: index of self in parent's content list
        );
      }
      if(not $rv) { # returned false.  Same as PRUNE.
        next; # prune
      } elsif(ref($rv) eq $travsignal_package) {
        $rv = $$rv; # deref
        if($rv eq 'ABORT') {
          last; # end of this excursion!
        } elsif($rv eq 'PRUNE') {
          next;
        } elsif($rv eq 'PRUNE_SOFTLY') {
          if(ref($this)
             and
             not($this->{'_empty_element'}
                 || $emptyElement{$this->{'_tag'} || ''})
          ) {
            # push a dummy empty content list just to trigger a post callback
            unshift @I, -1;
            unshift @C, $NIL;
          }
          next;
        } elsif($rv eq 'OK') {
          # noop
        } elsif($rv eq 'PRUNE_UP') {
          $I[0] = undef;
          next;
          
          # equivalent of last'ing out of the current child list.
          
        # Used to have PRUNE_UP_SOFTLY and ABORT_SOFTLY here, but the code
        # for these was seriously upsetting, served no particularly clear
        # purpose, and could not, I think, be easily implemented with a
        # recursive routine.  All bad things!
        } else {
          die "Unknown travsignal $rv\n";
          # should never happen
        }
      }
      # else fall thru to meaning same as \'OK'.
    }
    # end of pre-order calling
    
    # Now queue up content list for the current element...
    if(ref $this
       and
       not( # ...except for those which...
         not($content_r = $this->{'_content'} and @$content_r)
            # ...have empty content lists...
         and $this->{'_empty_element'} || $emptyElement{$this->{'_tag'} || ''}
            # ...and that don't get post-order callbacks
       )
    ) {
      unshift @I, -1;
      unshift @C, $content_r || $NIL;
      #print $this->{'_tag'}, " ($this) adds content_r ", $C[0], "\n";
    }
  }
  return $start;
}

=head1 SECONDARY STRUCTURAL METHODS

These methods all involve some structural aspect of the tree;
either they report some aspect of the tree's structure, or they involve
traversal down the tree, or walking up the tree.

=over 4

=item $h->is_inside('tag', ...) or $h->is_inside($element, ...)

Returns true if the $h element is, or is contained anywhere inside an
element that is any of the ones listed, or whose tag name is any of
the tag names listed.

=cut

sub is_inside {
  my $self = shift;
  return undef unless @_; # if no items specified, I guess this is right.

  my $current = $self;
      # the loop starts by looking at the given element
  while (defined $current and ref $current) {
    for (@_) {
      if(ref) { # element
        return 1 if $_ eq $current;
      } else { # tag name
        return 1 if $_ eq $current->{'_tag'};
      }
    }
    $current = $current->{'_parent'};
  }
  0;
}

=item $h->is_empty()

Returns true if $h has no content, i.e., has no elements or text
segments under it.  In other words, this returns true if $h is a leaf
node, AKA a terminal node.  Do not confuse this sense of "empty" with
another sense that it can have in SGML/HTML/XML terminology, which
means that the element in question is of the type (like HTML's "hr",
"br", "img", etc.) that I<can't> have any content.

That is, a particular "p" element may happen to have no content, so
$that_p_element->is_empty will be true -- even though the prototypical
"p" element isn't "empty" (not in the way that the prototypical "hr"
element is).

If you think this might make for potentially confusing code, consider
simply using the clearer exact equivalent:  not($h->content_list)

=cut

sub is_empty
{
  my $self = shift;
  !$self->{'_content'} || !@{$self->{'_content'}};
}


=item $h->pindex()

Return the index of the element in its parent's contents array, such
that $h would equal

  $h->parent->content->[$h->pindex]
  or
  ($h->parent->content_list)[$h->pindex]

assuming $h isn't root.  If the element $h is root, then
$h->pindex returns undef.

=cut

#'
sub pindex {
  my $self = shift;

  my $parent = $self->{'_parent'} || return undef;
  my $pc =  $parent->{'_content'} || return undef;
  my $i = 0;
  for(my $i = 0; $i < @$pc; ++$i) {
    return $i  if  ref $pc->[$i] and $pc->[$i] eq $self;
  }
  return undef; # we shouldn't ever get here
}


=item $h->address()

Returns a string representing the location of this node in the tree.
The address consists of numbers joined by a '.', starting with '0',
and followed by the pindexes of the nodes in the tree that are
ancestors of $h, starting from the top.

So if the way to get to a node starting at the root is to go to child
2 of the root, then child 10 of that, and then child 0 of that, and
then you're there -- then that node's address is "0.2.10.0".

As a bit of a special case, the address of the root is simply "0".

I forsee this being used mainly for debugging, but you may
find your own uses for it.

=item $h->address($address)

This returns the node (whether element or text-segment) at
the given address in the tree that $h is a part of.  (That is,
the address is resolved starting from $h->root.)

If there is no node at the given address, this returns undef.

=cut

sub address {
  if(@_ == 1) { # report-address form
    return
      join('.',
        reverse( # so it starts at the top
          map($_->pindex() || '0', # so that root's undef -> '0'
            $_[0], # self and...
            $_[0]->lineage
          )
        )
      )
    ;
  } else { # get-node-at-address
    my $here = $_[0]->root;
    my @stack = split(/\./, $_[1]);
    return undef unless 0 == shift @stack; # to pop the initial 0-for-root
    while(@stack) {
      return undef
       unless
         $here->{'_content'}
         and @{$here->{'_content'}} > $stack[0];
            # make sure the index isn't too high
      $here = $here->{'_content'}[ shift @stack ];
      return undef if @stack and not ref $here;
        # we hit a text node when we expected a non-terminal element node
    }
    
    return $here;
  }
}


=item $h->depth()

Returns a number expressing $h's depth within its tree, i.e., how many
steps away it is from the root.  If $h has no parent (i.e., is root),
its depth is 0.

=cut

#'
sub depth {
  my $here = $_[0];
  my $depth = 0;
  while(defined($here = $here->{'_parent'}) and ref($here)) {
    ++$depth;
  }
  return $depth;
}



=item $h->root()

Returns the element that's the top of $h's tree.  If $h is root, this
just returns $h.  (If you want to test whether $h I<is> the root,
instead of asking what its root is, just test not($h->parent).)

=cut

#'
sub root {
  my $here = my $root = shift;
  while(defined($here = $here->{'_parent'}) and ref($here)) {
    $root = $here;
  }
  return $root;
}


=item $h->lineage()

Returns the list of $h's ancestors, starting with its parent, and then
that parent's parent, and so on, up to the root.  If $h is root, this
returns an empty list.

If you simply want a count of the number of elements in $h's lineage,
use $h->depth.

=cut

#'
sub lineage {
  my $here = shift;
  my @lineage;
  while(defined($here = $here->{'_parent'}) and ref($here)) {
    push @lineage, $here;
  }
  return @lineage;
}


=item $h->lineage_tag_names()

Returns the list of the tag names of $h's ancestors, starting
with its parent, and that parent's parent, and so on, up to the
root.  If $h is root, this returns an empty list.
Example output: ('html', 'body', 'table', 'tr', 'td', 'em')

=cut

#'
sub lineage_tag_names {
  my $here = my $start = shift;
  my @lineage_names;
  while(defined($here = $here->{'_parent'}) and ref($here)) {
    push @lineage_names, $here->{'_tag'};
  }
  return @lineage_names;
}


=item $h->descendants()

In list context, returns the list of all $h's descendant elements,
listed in pre-order (i.e., an element appears before its
content-elements).  Text segments DO NOT appear in the list.
In scalar context, returns a count of all such elements.

=cut

#'
sub descendants {
  my $start = shift;
  if(wantarray) {
    my @descendants;
    $start->traverse(
      [ # pre-order sub only
        sub {
          push(@descendants, $_[0]);
          return 1;
        },
        undef # no post
      ],
      1, # ignore text
    );
    shift @descendants; # so $self doesn't appear in the list
    return @descendants;
  } else { # just returns a scalar
    my $descendants = -1; # to offset $self being counted
    $start->traverse(
      [ # pre-order sub only
        sub {
          ++$descendants;
          return 1;
        },
        undef # no post
      ],
      1, # ignore text
    );
    return $descendants;
  }
}


=item $h->find_by_tag_name('tag', ...)

In list context, returns a list of elements at or under $h that have
any of the specified tag names.  In scalar context, returns the first
(in pre-order traversal of the tree) such element found, or undef if
none.

=cut


sub find_by_tag_name {
  my($self) = shift;
  return() unless @_;
  my @tags = map lc($_), @_;
  my $wantarray = wantarray;
  
  my @matching;
  $self->traverse(
    [ # pre-order only
      sub {
        foreach my $t (@tags) {
          if($t eq $_[0]{'_tag'}) {
            push @matching, $_[0];
            return HTML::Element::ABORT unless $wantarray; # only take the first
            last; # found it.
          }
        }
        1; # keep traversing
      },
      undef  # no post
    ],
    1, # yes, ignore text nodes.
  );

  if($wantarray) {
    return @matching;
  } else {
    return undef unless @matching;
    return $matching[0];
  }
}



=item $h->find_by_attribute('attribute', 'value')

In a list context, returns a list of elements at or under $h that have
the specified attribute, and have the given value for that attribute.
In a scalar context, returns the first (in pre-order traversal of the
tree) such element found, or undef if none.

=cut

#TODO: make it take multiple attributes, and AND and OR them.

sub find_by_attribute {
  # We could limit this to non-internal attributes, but hey.
  my($self, $attribute, $value) = @_;
  Carp::croak "Attribute must be a defined value!" unless defined $attribute;
  $attribute = lc $attribute;
  
  my @matching;
  my $wantarray = wantarray;
  my $quit;
  $self->traverse(
    [ # pre-order only
      sub {
        if( exists $_[0]{$attribute}
             and $_[0]{$attribute} eq $value
        ) {
          push @matching, $_[0];
          return HTML::Element::ABORT unless $wantarray; # only take the first
        }
        1; # keep traversing
      },
      undef # no post
    ],
    1, # yes, ignore text nodes.
  );

  if($wantarray) {
    return @matching;
  } else {
    return undef unless @matching;
    return $matching[0];
  }
}



=item $h->attr_get_i('attribute')

In list context, returns a list consisting of the values of the given
attribute for $self and for all its ancestors starting from $self and
working its way up.  Nodes with no such attribute are skipped.
("attr_get_i" stands for "attribute get, with inheritance".)
In scalar context, returns the first such value, or undef if none.

Consider a document consisting of:

   <html lang='i-klingon'>
     <head><title>Pati Pata</title></head>
     <body>
       <h1 lang='la'>Stuff</h1>
       <p lang='es-MX' align='center'>
         Foo bar baz <cite>Quux</cite>.
       </p>
       <p>Hooboy.</p>
     </body>
   </html>

If $h is the "cite" element, $h->attr_get_i("lang") in list context
will return the list ('es-MX', 'i-klingon').  In scalar context, it
will return the value 'es-MX'.

If you call with multiple attribute names...

=item $h->attr_get_i('a1', 'a2', 'a3')

...in list context, this will return a list consisting of
the values of these attributes which exist in $self and its ancestors.
In scalar context, this returns the first value (i.e., the value of
the first existing attribute from the first element that has
any of the attributes listed).  So, in the above example,

  $h->attr_get_i('lang', 'align');

will return:

   ('es-MX', 'center', 'i-klingon') # in list context
  or
   'es-MX' # in scalar context.

But note that this:

 $h->attr_get_i('align', 'lang');

will return:

   ('center', 'es-MX', 'i-klingon') # in list context
  or
   'center' # in scalar context.

=cut

sub attr_get_i {
  if(@_ > 2) {
    my $self = shift;
    Carp::croak "No attribute names can be undef!"
     if grep !defined($_), @_;
    my @attributes = map lc($_), @_;
    if(wantarray) {
      my @out;
      foreach my $x ($self, $self->lineage) {
        push @out, map { exists($x->{$_}) ? $x->{$_} : () } @attributes;
      }
      return @out;
    } else {
      foreach my $x ($self, $self->lineage) {
        foreach my $attribute (@attributes) {
          return $x->{$attribute} if exists $x->{$attribute}; # found
        }
      }
      return undef; # never found
    }
  } else {
    # Single-attribute search.  Simpler, most common, so optimize
    #  for the most common case
    Carp::croak "Attribute name must be a defined value!" unless defined $_[1];
    my $self = $_[0];
    my $attribute = lc $_[1];
    if(wantarray) { # list context
      return
        map {
          exists($_->{$attribute}) ? $_->{$attribute} : ()
        } $self, $self->lineage;
      ;
    } else { # scalar context
      foreach my $x ($self, $self->lineage) {
        return $x->{$attribute} if exists $x->{$attribute}; # found
      }
      return undef; # never found
    }
  }
}


=item $h->extract_links() or $h->extract_links(@wantedTypes)

Returns links found by traversing the element and all of its children
and looking for attributes (like "href" in an "a" element, or "src" in
an "img" element) whose values represent links.  The return value is a
I<reference> to an array.  Each element of the array is reference to
an array with two items: the link-value and a the element that has the
attribute with that link-value.  You may or may not end up using the
element itself -- for some purposes, you may use only the link value.

You might specify that you want to extract links from just some kinds
of elements (instead of the default, which is to extract links from
I<all> the kinds of elements known to have attributes whose values
represent links).  For instance, if you want to extract links from
only "a" and "img" elements, you could code it like this:

  for (@{  $e->extract_links('a', 'img')  }) {
      my($link, $element) = @$_;
      print
        "Hey, there's a ", $element->tag,
        " that links to $link\n";
  }

=cut


sub extract_links
{
    my $start = shift;

    my %wantType;
    @wantType{map { lc $_ } @_} = (1) x @_; # if there were any
    my $wantType = scalar(@_);

    my @links;

    my($link_attrs, $tag, $self, $val); # scratch for each iteration
    $start->traverse(
      [
        sub { # pre-order call only
          $self = $_[0];
  
          $tag = $self->{'_tag'};
          return 1 if $wantType && !$wantType{$tag};  # if we're selective
  
          if(defined(  $link_attrs = $linkElements{$tag}  )) {
            # If this is a tag that has any link attributes,
            #  look over possibly present link attributes,
            #  saving the value, if found.
            for (ref($link_attrs) ? @$link_attrs : $link_attrs) {
              if(defined(  $val = $self->attr($_)  )) {
                push(@links, [$val, $self])
              }
            }
          }
  
          1; # return true, so we keep recursing
        },
        undef
      ],
      1, # ignore text nodes
    );
    \@links;
}

#--------------------------------------------------------------------------

=item $h->same_as($i)

Returns true if $h and $i are both elements representing the same tree
of elements, each with the same tag name, with the same explicit
attributes (i.e., not counting attributes whose names start with "_"),
and with the same content (textual, comments, etc.).

Sameness of descendant elements is tested, recursively, with
C<$child1-E<gt>same_as($child_2)>, and sameness of text segments is tested
with C<$segment1 eq $segment2>.

=cut

sub same_as {
  die "same_as() takes only one argument: \$h->same_as(\$i)" unless @_ == 2;
  my($h,$i) = @_[0,1];
  die "same_as() can be called only as an object method" unless ref $h;

  return 0 unless defined $i and ref $i;
   # An element can't be same_as anything but another element!
   # They needn't be of the same class, tho.

  return 1 if $h eq $i;
   # special (if rare) case: anything is the same as... itself!
  
  # assumes that no content lists in/under $h or $i contain subsequent
  #  text segments, like: ['foo', ' bar']
  
  # compare attributes now.
  #print "Comparing tags of $h and $i...\n";

  return 0 unless $h->{'_tag'} eq $i->{'_tag'};
    # only significant attribute whose name starts with "_"
  
  #print "Comparing attributes of $h and $i...\n";
  # Compare attributes, but only the real ones.
  {
    # Bear in mind that the average element has very few attributes,
    #  and that element names are rather short.
    # (Values are a different story.)
    
    my @keys_h = sort grep {length $_ and substr($_,0,1) ne '_'} keys %$h;
    my @keys_i = sort grep {length $_ and substr($_,0,1) ne '_'} keys %$i;
    
    #print '<', join(',', @keys_h), '> =?= <', join(',', @keys_i), ">\n";
    
    return 0 unless @keys_h == @keys_i;
     # different number of real attributes?  they're different.
    for(my $x = 0; $x < @keys_h; ++$x) {
      return 0 unless
       $keys_h[$x] eq $keys_i[$x] and  # same key name
       $h->{$keys_h[$x]} eq $i->{$keys_h[$x]}; # same value
       # Should this test for definedness on values?
       # People shouldn't be putting undef in attribute values, I think.
    }
  }
  
  #print "Comparing children of $h and $i...\n";
  my $hcl = $h->{'_content'} || [];
  my $icl = $i->{'_content'} || [];
  
  return 0 unless @$hcl == @$icl;
   # different numbers of children?  they're different.
  
  if(@$hcl) {
    # compare each of the children:
    for(my $x = 0; $x < @$hcl; ++$x) {
      if(ref $hcl->[$x]) {
        return 0 unless ref($icl->[$x]);
         # an element can't be the same as a text segment
        # Both elements:
        return 0 unless $hcl->[$x]->same_as($icl->[$x]);  # RECURSE!
      } else {
        return 0 if ref($icl->[$x]);
         # a text segment can't be the same as an element
        # Both text segments:
        return 0 unless $hcl->[$x] eq $icl->[$x];
      }
    }
  }
  
  return 1; # passed all the tests!
}


#--------------------------------------------------------------------------

=item $h = HTML::Element->new_from_lol(ARRAYREF)

Resursively constructs a tree of nodes, based on the (non-cyclic)
data structure represented by ARRAYREF, where that is a reference
to an array of arrays (of arrays (of arrays (etc.))).
In each arrayref in that structure:  arrayrefs are considered to
designate a sub-tree representing children for the node constructed
from the current arrayref; hashrefs are considered to contain
attribute-value pairs to add to the element to be constructed from
the current arrayref; text segments at the start of any arrayref
will be considered to specify the name of the element to be
constructed from the current araryref; all other text segments will
be considered to specify text segments as children for the current
arrayref.

An example will hopefully make this more obvious:

  my $h = HTML::Element->new_from_lol(
    ['html',
      ['head',
        [ 'title', 'I like stuff!' ],
      ],
      ['body',
        {'lang', 'en-JP', _implicit => 1},
        'stuff',
        ['p', 'um, p < 4!', {'class' => 'par123'}],
        ['div', {foo => 'bar'}, '123'],
      ]
    ]
  );
  $h->dump;

Will print this:

  <html> @0
    <head> @0.0
      <title> @0.0.0
        "I like stuff!"
    <body lang="en-JP"> @0.1 (IMPLICIT)
      "stuff"
      <p class="par123"> @0.1.1
        "um, p < 4!"
      <div foo="bar"> @0.1.2
        "123"

And printing $h->as_HTML will give something like:

  <html><head><title>I like stuff!</title></head>
  <body lang="en-JP">stuff<p class="par123">um, p &lt; 4!
  <div foo="bar">123</div></body></html>

=cut

sub new_from_lol {
  my $class = ref($_[0]) || $_[0]; # I /should/ be called as a class method!
  my $lol = $_[1];

  Carp::croak "first argument to new_from_lol mustn't be undef!"
    unless defined $lol;
  
  Carp::croak
   "first argument to new_from_lol must be an arrayref, not \"$lol\"!"
    unless ref($lol) eq 'ARRAY';

  my @ancestor_lols;
   # So we can make sure there's no cyclicities in this lol.
   # That would be perverse, but one never knows.
  my($sub, $k, $v, $node); # last three are scratch values
  $sub = sub {
    #print "Building for $_[0]\n";
    my $lol = $_[0];
    return unless @$lol;
    my(@attributes, @children);
    Carp::croak "Cyclicity detected in source LOL tree, around $lol?!?"
     if grep($_ eq $lol, @ancestor_lols);
    push @ancestor_lols, $lol;

    my $tag_name = 'null';

    # Recursion in in here:
    for(my $i = 0; $i < @$lol; ++$i) { # Iterate over children
      if(ref($lol->[$i]) eq 'ARRAY') { # subtree: most common thing in loltree
        push @children, $sub->($lol->[$i]);
      } elsif(! ref($lol->[$i])) {
        if($i == 0) { # name
          $tag_name = $lol->[$i];
        } else { # text segment child
          push @children, $lol->[$i];
        }
      } elsif(ref($lol->[$i]) eq 'HASH') { # attribute hashref
        keys %{$lol->[$i]}; # reset the each-counter, just in case
        while(($k,$v) = each %{$lol->[$i]}) {
          push @attributes, lc($k), $v
            unless $k eq '_name' or $k eq '_content' or $k eq '_parent';
          # enforce /some/ sanity!
        }
      }
      # else...?
    }

    pop @ancestor_lols;

    #print "Children: @children\n";

    $node = $class->new($tag_name); # finally construct

    if($class eq __PACKAGE__) {  # Special-case it, for speed:
      #print "Special cased\n";
      %$node = (%$node, @attributes) if @attributes;
      if(@children) {
        $node->{'_content'} = \@children;
        foreach my $c (@children) { $c->{'_parent'} = $node if ref $c }
      }
    } else {  # Do it the clean way...
      #print "Done neatly\n";
      while(@attributes) { $node->attr(splice @attributes,0,2) }
      $node->push_content(@children) if @children;
    }

    return $node;
  };

  $node = $sub->($lol);
  undef $sub; # so it won't be in its own frame, so its refcount can hit 0
  return $node;
}

#--------------------------------------------------------------------------

=item $h->has_insane_linkage

This method is for testing whether this element or the elements
under it have linkage attributes (_parent and _content) whose values
are deeply aberrant: if there are undefs in a content list; if an
element appears in the content lists of more than one element;
if the _parent attribute of an element doesn't match its actual
parent; or if an element appears as its own descendant (i.e.,
if there is a cyclicity in the tree).

This returns empty list (or false, in scalar context) if the subtree's
linkage methods are sane; otherwise it returns two items (or true, in
scalar context): the element where the error occurred, and a string
describing the error.

This method is provided is mainly for debugging and troubleshooting --
it should be I<quite impossible> for any document constructed via
HTML::TreeBuilder to parse into a non-sane tree (since it's not
the content of the tree per se that's in question, but whether
the tree in memory was properly constructed); and it I<should> be
impossible for you to produce an insane tree just thru reasonable
use of normal documented structure-modifying methods.  But if you're
constructing your own trees, and your program is going into infinite
loops as during calls to traverse() or any of the secondary
structural methods, as part of debugging, consider calling is_insane
on the tree.

=cut

sub has_insane_linkage {
  my @todo = ($_[0]);
  my($c, $i, $p, $this); # scratch
  
  # Another iterative traverser; this time much simpler because
  #  only in pre-order:
  my %parent_of = ($_[0], 'TOP-OF-SCAN');
  while(@todo) {
    $this = shift @todo;
    $c = $this->{'_content'} || next;
    return($this, "_content attribute is true but nonref.")
     unless ref($c) eq 'ARRAY';
    next unless @$c;
    for($i = 0; $i < @$c; ++$i) {
      return($this, "Child $i is undef")
       unless defined $c->[$i];
      if(ref($c->[$i])) {
        return($c->[$i], "appears in its own content list")
         if $c->[$i] eq $this;
        return($c->[$i],
          "appears twice in the tree: once under $this, once under $parent_of{$c->[$i]}"
        )
         if exists $parent_of{$c->[$i]};
        $parent_of{$c->[$i]} = ''.$this;
          # might as well just use the stringification of it.
        
        return($c->[$i], "_parent attribute is wrong (not defined)")
         unless defined($p = $c->[$i]{'_parent'});
        return($c->[$i], "_parent attribute is wrong (nonref)")
         unless ref($p);
        return($c->[$i],
          "_parent attribute is wrong (is $p; should be $this)"
        )
         unless $p eq $this;
      }
    }
    unshift @todo, grep ref($_), @$c;
     # queue up more things on the todo stack
  }
  return; #okay
}

#==========================================================================
1;

__END__

=back

=head1 BUGS

* If you want to free the memory associated with a tree built of
HTML::Element nodes, then you will have to delete it explicitly.
See the $h->delete method, above.

* There's almost nothing to stop you from making a "tree" with
cyclicities (loops) in it, which could, for example, make the
traverse method go into an infinite loop.  So don't make
cyclicities!  (If all you're doing is parsing HTML files,
and looking at the resulting trees, this will never be a problem
for you.)

* There's no way to represent comments or processing directives
in a tree with HTML::Elements.  Not yet, at least.

* There's (currently) nothing to stop you from using an undefined
value as a text segment.  If you're running under C<perl -w>, however,
this may make HTML::Element's code produce a slew of warnings.

=head1 NOTES ON SUBCLASSING

You are welcome to derive subclasses from HTML::Element, but you
should be aware that the code in HTML::Element makes certain
assumptions about elements (and I'm using "element" to mean ONLY an
object of class HTML::Element, or of a subclass of HTML::Element):

* The value of an element's _parent attribute must either be undef or
otherwise false, or must be an element.

* The value of an element's _content attribute must either be undef or
otherwise false, or a reference to an (unblessed) array.  The array
may be empty; but if it has items, they must ALL be either mere
strings (text segments), or elements.

* The value of an element's _tag attribute should, at least, be a 
string of printable characters.

Moreover, bear these rules in mind:

* Do not break encapsulation on objects.  That is, access their
contents only thru $obj->attr or more specific methods.

* You should think twice before completely overriding any of the
methods that HTML::Element provides.  (Overriding with a method that
calls the superclass method is not so bad, tho.)

=head1 SEE ALSO

L<HTML::AsSubs>, L<HTML::TreeBuilder>

=head1 COPYRIGHT

Copyright 1995-1998 Gisle Aas, 1999-2000 Sean M. Burke.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 AUTHOR

Original author Gisle Aas E<lt>gisle@aas.noE<gt>; current maintainer
Sean M. Burke, E<lt>sburke@cpan.orgE<gt>

=cut

If you've read the code this far, you need some hummus:

EASY HUMMUS
(Adapted from a recipe by Ralph Baccash (1937-2000))

INGREDIENTS:

  - The juice of two smallish lemons
     (adjust to taste, and depending on how juicy the lemons are)
  - 6 tablespoons of tahini
  - 4 tablespoons of olive oil
  - 5 big cloves of garlic, chopped fine
  - salt to taste
  - pepper to taste
  - onion powder to taste
  - pinch of coriander powder  (optional)
  - big pinch of cumin
Then:
  - 2 16oz cans of garbanzo beans
  - parsley, or Italian parsley
  - a bit more olive oil

PREPARATION:

Drain one of the cans of garbanzos, discarding the juice.  Drain the
other, reserving the juice.

Peel the garbanzos (just pressing on each a bit until the skin slides
off).  It will take time to peel all the garbanzos.  It's optional, but
it makes for a smoother hummus.  Incidentally, peeling seems much
faster and easier if done underwater -- i.e., if the beans are in a
bowl under an inch or so of water.

Now, in a blender, combine everything in the above list, starting at the
top, stopping at (but including) the cumin.  Add one-third of the can's
worth of the juice that you reserved.  Blend very well.  (For lack of a
blender, I've done okay using a Braun hand-mixer.)

Start adding the beans little by little, and keep blending, and
increasing speeds until very smooth.  If you want to make the mix less
viscous, add more of the reserved juice.  Adjust the seasoning as
needed.

Cover with chopped parsley, and a thin layer of olive oil.  The parsley
is more or less optional, but the olive oil is necessary, to keep the
hummus from discoloring.  Possibly sprinkle with paprika or red chile
flakes.

Serve at about room temperature, with warm pitas.  Possible garnishes
include olives, peperoncini, tomato wedges.

Variations on this recipe consist of adding or substituting other
spices.  The garbanzos, tahini, lemon juice, and oil are the only really
core ingredients, and note that their quantities are approximate.

# End
