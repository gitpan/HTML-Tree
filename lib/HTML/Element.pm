 
# Time-stamp: "2000-03-08 01:32:06 MST"
package HTML::Element;

# TODO: add 'are_element_identical' method ?
# TODO: add 'are_content_identical' method ?
# TODO: maybe alias ->destroy to ->delete ?
# TODO: add a new, better traverser?
# TODO: have use a HTML::Tagset or something (to write)
# TODO: provide 

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
"access the 'src' attribute of the root's first child's seventh
child's third child", you're more likely to have to scan the contents
of a tree, looking for whatever nodes, or kinds of nodes, you want to
do something with.  The most straightforward way to look over a tree
is to "traverse" it; an HTML::Element method ($h->traverse) is
provided for this purpose; and several other HTML::Element methods are
based on it.

(For everything you ever wanted to know about trees, and then some,
see Donald Knuth's I<The Art of Computer Programming, Volume 1>.)

=cut


use strict;
use Carp ();
use HTML::Entities ();

use vars qw($VERSION
            $html_uc
            %emptyElement %optionalEndTag %linkElements %boolean_attr
           );

$VERSION = '1.48';
sub Version { $VERSION; }

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


=item $h->tag() or $h->tag('tagname')

Returns (optionally sets) the tag name (also known as the generic
identifier) for the element $h.  The tag name is always converted to
lower case.

=cut

sub tag
{
    my $self = shift;
    if (@_) { # set
        $self->{'_tag'} = lc $_[0];
    } else { # get
        $self->{'_tag'};
    }
}


=item $h->content()

Returns the content of this element -- i.e., what is inside/under this
element.  The return value is either undef (which you should
understand to mean no content), or a I<reference> to the array of
content items, each of which is either a text segment, or an
HTML::Element object.

=cut

sub content
{
    shift->{'_content'};
}



=item $h->parent() or $h->parent($new_parent)

Returns (optionally sets) the parent for this element.  (If you're
thinking about using this to attach or detach nodes, instead consider
$new_parent->push_content($h), $new_parent->unshift_content($h),
or $h->detach.)

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



=item $h->implicit() or $h->implicit($bool)

Returns (optionally sets) the implicit attribute.  This attribute is
used to indicate that the element was not originally present in the
source, but was added to the parse tree (by HTML::TreeBuilder, for
example) in order to conform to the rules of HTML structure.

=cut

sub implicit
{
    shift->attr('_implicit', @_);
}



=item $h->pos() or $h->pos($element)

Returns (and optionally sets) the "current position" pointer of $h.
This "pos" attribute is a pointer used during some parsing operations,
whose value is whatever HTML::Element element at or under $h is
currently "open", where $h->insert_element(NEW) will actually insert a
new element.

(This has nothing to do with the Perl function called "pos", for
controlling where regular expression matching starts.)

If you set $h->pos($element), be sure that $element is either $h, or
an element under $h.

If you've been modifying the tree under $h and are
no longer sure $h->pos is valid, you can enforce validity with:

    $h->pos(undef) unless $h->pos->is_under($h);

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



=item $h->attr('attr') or $h->attr('attr', 'value')

Returns (optionally sets) the value of the given attribute of $h.  The
attribute name (but not the value, if provided) is forced to
lowercase.  If setting a new value, the old value of that attribute is
returned.

=cut

sub attr
{
    my $self = shift;
    my $attr = lc shift;
    if (@_) {  # set
        my $old = $self->{$attr};
        $self->{$attr} = $_[0];
        return $old
    } else {   # get
        return $self->{$attr};
    }
}


=item $h->all_attr()

Returns all this element's attributes and values, as attribute-value pairs.

=cut

sub all_attr {
  return %{$_[0]};
  # Yes, trivial.  But no other way for the user to do the same
  #  without breaking encapsulation.
  # And if our object representation changes, this method's behavior
  #  should stay the same.
}

#==========================================================================

=back

=head1 STRUCTURE-MODIFYING METHODS

While you theoretically could modify a tree by directly manipulating
objects' parent and content attributes, it's much simpler (and less
error-prone), to use these methods:

=over 4


=item $h->insert_element($element, $implicit)

Inserts a new element under the element at $h->pos().  Then updates
$h->pos() to point to the inserted element, unless $element is a
prototypically empty element like "br", "hr", "img", etc.  The new
$h->pos() is returned.

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



=item $h->push_content($element_or_text, ...)

Adds the specified items to the I<end> of the content list of the
element $h.  The items of content to be added should each be either a
text segment (a string) or an HTML::Element object.

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

Removes the elements designated by $offset and $length from the
content-list of element $h, and replaces them with the elements of the
following list, if any.  Returns the elements removed from the array.
If $offset is negative, then it starts that far from the end of the
array.  If $length and the following list are omitted, removes
everything from $offset onward.

The items of content to be added should each be either a text segment
(a string) or an HTML::Element object, and should not already be
children of $h.

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
  my $parent;
  return undef unless($parent = $self->{'_parent'});

  @{$parent->{'_content'}}
   = grep { not( ref($_) and $_ eq $self) }  # filter $self out
  @{$parent->{'_content'}}
   if $parent->{'_content'};

  $self->{'_parent'} = undef;
  return $parent;
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
  Carp::croak "the target node has no parent" unless $self->{'_parent'};

  my $parent = $self->{'_parent'};
  my $parent_content = $parent->{'_content'};
  Carp::croak "the target node's parent has no content!?" 
   unless $parent_content;

  my $content_r = $self->{'_content'} || [];
  @$parent_content 
   = map { ( ref($_) and $_ eq $self) ? @$content_r : $_ }
         @$parent_content
  ;

  for (@$content_r) {  $_->{'_parent'} = $parent  }

  @$content_r = (); # unattach from old parent
  $self->{'_parent'} = undef; # detach old parent from its parent

  return $self;  # note, doesn't destroy it.
}



=item $h->delete_content()

Clears the content of $h, calling $i->delete for each content element.

Returns $h.

=cut

sub delete_content
{
    for (@{ delete($_[0]->{'_content'})
              # Deleting it here (while holding its value, for the moment)
              #  will keep calls to detach from trying to uselessly filter
              #  the list (as they won't be able to see it once it's been
              #  deleted)
            || return($_[0]) # in case of no content
          }
        )
    {
        $_->delete if ref $_;
    }
    $_[0];
}



=item $h->delete()

Removes this element from its parent (if it has one) and explicitly
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

The returned element is parentless.  Any pos attributes present in the
source element/tree will be absent in the copy.

=cut

sub clone {
  #print "Cloning $_[0]\n";
  my $it = shift;
  Carp::croak "clone() can be called only as an object method" unless ref $it;
  Carp::croak "clone() takes no arguments" if @_;

  my $new = bless { %$it }, ref($it);     # COPY!!! HOOBOY!
  delete @$new{'_content', '_parent', '_pos'};
  
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
  my $indent_on = defined($indent) && length($indent);
  my @html = ();
  
  unless(defined $HTML::TreeBuilder::VERSION) {
    require HTML::TreeBuilder or die "Can't require HTML::TreeBuilder";
  }
  
  my $last_tag_tightenable = 0;
  my $this_tag_tightenable = 0;
  
  my $nonindentable_ancestors = 0;  # count of nonindentible tags over us.
  
  my($tag, $node, $start, $depth); # per-iteration scratch
  
  if($indent_on) {
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

Returns a string that represents only the text parts of the element's
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
        sub {
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
        }
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
double-quotes, and appropriate characters are encoded.  If $entities
is omitted or undef, I<all> unsafe characters are encoded as HTML
entities.  See L<HTML::Entities> for details.  If you specify some
value for $entities, remember to include the double-quote character in
it.  (Previous versions of this module would basically behave as if
C<'&"E<gt>'> were specified for $entities.)

=cut

sub starttag
{
    my($self, $entities) = @_;
    
    my $name = $self->{'_tag'};
    
    # TODO: document these...
    return        $self->{'text'}        if $name eq '~literal';
    
    return "<!" . $self->{'text'} . ">" if $name eq '~declaration';
    
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
  my $current_tag;
  while (defined $current) {
    $current_tag = $current->{'_tag'};
    for (@_) {
      if(ref) { # element
        return 1 if $_ eq $current;
      } else { # tag name
        return 1 if $_ eq $current_tag;
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
"p" element isn't "empty" (in the way that the prototypical "hr"
element is).

=cut

sub is_empty
{
  my $self = shift;
  !$self->{'_content'} || !@{$self->{'_content'}};
}


=item $h->pindex()

Return the index of the element in its parent's contents array, such
that $h would equal $h->parent->content->[$h->pindex], assuming $h
isn't root.  If the element $h is root, then $h->pindex returns undef.

=cut

#'
sub pindex {
  my $self = shift;

  my $parent = $self->{'_parent'} || return undef;
  my $pc =  $parent->{'_content'} || return undef;
  my $i = 0;
  for(my $i = 0; $i < @$pc; ++$i) {
    return $i  if  ref $pc->[$i] and $pc->[$i] == $self;
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

I forsee this being used mainly for debugging.

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
  while(defined($here = $here->{'_parent'})) {
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
  my $here = shift;
  my $root = $here;
  while(defined($here = $here->{'_parent'})) {
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
  while(defined($here = $here->{'_parent'})) {
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
  my $here = shift;
  my @lineage_names;
  while(defined($here = $here->{'_parent'})) {
    push @lineage_names, $here->{'_tag'};
  }
  return @lineage_names;
}


=item $h->descendants()

In list context, returns the list of all $h's descendant elements,
listed in pre-order (i.e., an element appears before its
content-elements).  Text segments do not appear in the list.
In scalar context, returns a count of all such elements.

=cut

#'
sub descendants {
  my $start = shift;
  if(wantarray) {
    my @descendants;
    $start->traverse(
      sub {
        return unless $_[1]; # work in pre-order
        push(@descendants, $_[0]);
        return 1;
      },
      1, # ignore text
    );
    shift @descendants; # so $self doesn't appear in the list
    return @descendants;
  } else { # just returns a scalar
    my $descendants = -1; # to offset $self being counted
    $start->traverse(
      sub {
        return unless $_[1]; # work in pre-order
        ++$descendants;
        return 1;
      },
      1, # ignore text
    );
    return $descendants;
  }
}



=item $h->traverse(\&callback)

=item or $h->traverse(\&callback, $ignore_text)

Traverse the element and all of its children.  For each node visited, the
callback routine is called with these arguments:

    $_[0] : the node (element or text segment),
    $_[1] : a startflag, and
    $_[2] : the depth

If the $ignore_text parameter is given and true, then the callback
will not be called for text content.

The startflag is 1 when we enter a node (i.e., in pre-order calls) and
0 when we leave the node (in post-order calls).  Note, however, that
post-order calls don't happen for nodes that are text segments or
elements that are prototypically empty (like "br", "hr", etc.).

If the returned value is false from the pre-order call to the
callback, then the children will not be traversed, nor will the
callback be called in post-order for that node.

If $ignore_text is given and false (so we I<do> visit text nodes,
instead of ignoring them), then when text nodes are visited, we will
also pass two extra arguments to the callback:

    $_[3] : the element that's the parent
             of this text node
    $_[4] : the index of this text node
             in its parent's content list

The source code for HTML::Element and HTML::TreeBuilder contain
several examples of the use of the "traverse" method.

(Note: you should not change the structure of a tree I<while> you are
traversing it.)

=cut

sub traverse {
  my($start, $callback, $ignore_text) = @_;
  my $depth = 0;

  my $sub;
  
  $sub = sub { # Make a sub to handle the work of recursion
    my $self = $_[0]; # all other parameters are under closure
    
    # Visit this node in pre-order.
    $callback->($self, 1, $depth) || return; # give up here if false.
    
    # "Won't somebody PLEASE think of the children!?"
    if($self->{'_content'} and @{$self->{'_content'}}) {
      ++$depth;
      my $children_r = $self->{'_content'};
      for(my $i = 0; $i < @$children_r; ++$i) {
        if(ref $children_r->[$i]) { # a real node
          $sub->( $children_r->[$i] ); # recurse.
        } else {
          # a text node (scalar segment)
          $callback->( $children_r->[$i], 1, $depth, $self, $i )
           # yup, extra arguments
            unless $ignore_text;
        }
      }
      --$depth;
    }
    
    # Now, if applicable, visit in post-order
    $callback->($self, 0, $depth)
      unless $self->{'_empty_element'} # allow exceptional emptiness
             || $emptyElement{$self->{'_tag'}};
    return;
  };
  
  $sub->($start); # GO!
  undef $sub; # break cyclicity.
  
  return $start;
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
  my $quit;
  $self->traverse(
    sub {
      return 0 if $quit;
      return unless $_[1]; # in pre-order, not that it matters
      foreach my $t (@tags) {
        if($t eq $_[0]{'_tag'}) {
          push @matching, $_[0];
          $quit = 1 unless $wantarray; # only take the first
          last; # found it.
        }
      }
      1; # keep traversing
    },
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


sub find_by_attribute {
  # We could limit this to non-internal attributes, but hey.
  my($self, $attribute, $value) = @_;
  Carp::croak "Attribute must be a defined value!" unless defined $attribute;
  $attribute = lc $attribute;
  
  my @matching;
  my $wantarray = wantarray;
  my $quit;
  $self->traverse(
    sub {
      return 0 if $quit;
      return unless $_[1]; # in pre-order, not that it matters
      if( exists $_[0]{$attribute}
           and $_[0]{$attribute} eq $value
      ) {
        push @matching, $_[0];
        $quit = 1 unless $wantarray;
      }
      1; # keep traversing
    },
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

=cut

# TODO: make a syntax supporting multiple attributes at once,
#  so that one could do: $x->attr_get_i('lang', 'xml:lang') ?
#  ditto for attr_get

sub attr_get_i {
  my $self = $_[0];
  Carp::croak "Attribute name must be a defined value!" unless defined $_[1];
  my $attribute = lc $_[1];
  if(wantarray) { # list context
    return
      map {
        exists($_->{$attribute}) ? $_->{$attribute} : ()
      } $self, $self->lineage;
    ;
  } else { # scalar context
    # otherwise, we're in scalar context
    foreach my $x ($self, $self->lineage) {
      return $x->{$attribute} if exists $x->{$attribute}; # found
    }
    return undef; # never found
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
      sub {
        return 1 unless $_[1]; # work only in pre-order
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

=head1 SEE ALSO

L<HTML::AsSubs>, L<HTML::TreeBuilder>

=head1 COPYRIGHT

Copyright 1995-1998 Gisle Aas, 1999-2000 Sean M. Burke.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 AUTHOR

Original author Gisle Aas E<lt>gisle@aas.noE<gt>; current maintainer
Sean M. Burke, E<lt>sburke@netadventure.netE<gt>

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
