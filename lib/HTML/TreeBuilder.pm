
require 5;
# Time-stamp: "2000-09-04 14:56:53 MDT"
package HTML::TreeBuilder;
#TODO: maybe have it recognize higher versions of
# Parser, and register the methods as subs?
# Hm, but TreeBuilder wouldn't be subclassable, then.

# TODO: document tweaks?
# TODO: deprecate subclassing TreeBuilder?

use strict;
use integer; # vroom vroom!
use vars qw(@ISA $VERSION $Debug);
$VERSION = '3.04';
$Debug = 0 unless defined $Debug;

use HTML::Entities ();
use HTML::Tagset 3.02 ();

use HTML::Element ();
use HTML::Parser ();
@ISA = qw(HTML::Element HTML::Parser);
 # This looks schizoid, I know.
 # It's not that we ARE an element AND a parser.
 # We ARE an element, but one that knows how to handle signals
 #  (method calls) from Parser in order to elaborate its subtree.

# Legacy aliases:
*HTML::TreeBuilder::isKnown = \%HTML::Tagset::isKnown;
*HTML::TreeBuilder::canTighten = \%HTML::Tagset::canTighten;
*HTML::TreeBuilder::isHeadElement = \%HTML::Tagset::isHeadElement;
*HTML::TreeBuilder::isBodyElement = \%HTML::Tagset::isBodyElement;
*HTML::TreeBuilder::isPhraseMarkup = \%HTML::Tagset::isPhraseMarkup;
*HTML::TreeBuilder::isHeadOrBodyElement = \%HTML::Tagset::isHeadOrBodyElement;
*HTML::TreeBuilder::isList = \%HTML::Tagset::isList;
*HTML::TreeBuilder::isTableElement = \%HTML::Tagset::isTableElement;
*HTML::TreeBuilder::isFormElement = \%HTML::Tagset::isFormElement;
*HTML::TreeBuilder::p_closure_barriers = \@HTML::Tagset::p_closure_barriers;

=head1 NAME

HTML::TreeBuilder - Parser that builds a HTML syntax tree

=head1 SYNOPSIS

  foreach my $file_name (@ARGV) {
    my $tree = HTML::TreeBuilder->new; # empty tree
    $tree->parse_file($file_name);
    print "Hey, here's a dump of the parse tree of $file_name:\n";
    $tree->dump; # a method we inherit from HTML::Element
    print "And here it is, bizarrely rerendered as HTML:\n",
      $tree->as_HTML, "\n";
    
    # Now that we're done with it, we must destroy it.
    $tree = $tree->delete;
  }

=head1 DESCRIPTION

This class is for HTML syntax trees that get built out of HTML
source.  The way to use it is to:

1. start a new (empty) HTML::TreeBuilder object,

2. then use one of the methods from HTML::Parser (presumably with
$tree->parse_file($filename) for files, or with
$tree->parse($document_content) and $tree->eof if you've got
the content in a string) to parse the HTML
document into the tree $tree.

3. do whatever you need to do with the syntax tree, presumably
involving traversing it looking for some bit of information in it,

4. and finally, when you're done with the tree, call $tree->delete to
erase the contents of the tree from memory.  This kind of thing
usually isn't necessary with most Perl objects, but it's necessary for
TreeBuilder objects.  See L<HTML::Element> for a more verbose
explanation of why this is the case.

=head1 METHODS AND ATTRIBUTES

Objects of this class inherit the methods of both HTML::Parser and
HTML::Element.  The methods inherited from HTML::Parser are used for
building the HTML tree, and the methods inherited from HTML::Element
are what you use to scrutinize the tree.  Besides this
(HTML::TreeBuilder) documentation, you must also carefully read the
HTML::Element documentation, and also skim the HTML::Parser
documentation -- probably only its parse and parse_file methods are of
interest.

The following methods native to HTML::TreeBuilder all control how
parsing takes place; they should be set I<before> you try parsing into
the given object.  You can set the attributes by passing a TRUE or
FALSE value as argument.  E.g., $root->implicit_tags returns the current
setting for the implicit_tags option, $root->implicit_tags(1) turns that
option on, and $root->implicit_tags(0) turns it off.

=over 4

=item $root->implicit_tags(value)

Setting this attribute to true will instruct the parser to try to
deduce implicit elements and implicit end tags.  If it is false you
get a parse tree that just reflects the text as it stands, which is
unlikely to be useful for anything but quick and dirty parsing.  (And, in current versions, $root->
Default is true.

Implicit elements have the implicit() attribute set.

=item $root->implicit_body_p_tag(value)

This controls an aspect of implicit element behavior, if implicit_tags
is on:  If a text element (PCDATA) or a phrasal element (such as
"E<lt>emE<gt>") is to be inserted under "E<lt>bodyE<gt>", two things
can happen: if implicit_body_p_tag is true, it's placed under a new,
implicit "E<lt>pE<gt>" tag.  (Past DTDs suggested this was the only
correct behavior, and this is how past versions of this module
behaved.)  But if implicit_body_p_tag is false, nothing is implicated
-- the PCDATA or phrasal element is simply placed under
"E<lt>bodyE<gt>".  Default is false.

=item $root->ignore_unknown(value)

This attribute controls whether unknown tags should be represented as
elements in the parse tree, or whether they should be ignored. 
Default is true (to ignore unknown tags.)

=item $root->ignore_text(value)

Do not represent the text content of elements.  This saves space if
all you want is to examine the structure of the document.  Default is
false.

=item $root->ignore_ignorable_whitespace(value)

If set to true, TreeBuilder will try to avoid
creating ignorable whitespace text nodes in the tree.  Default is
true.  (In fact, I'd be interested in hearing if there's ever a case
where you need this off, or where leaving it on leads to incorrect
behavior.)

=item $root->p_strict(value)

If set to true (and it defaults to false), TreeBuilder will take a
narrower than normal view of what can be under a "p" element; if it sees
a non-phrasal element about to be inserted under a "p", it will close that
"p".  Otherwise it will close p elements only for other "p"'s, headings,
and "form" (altho the latter may be removed in future versions).

For example, when going thru this snippet of code,

  <p>stuff
  <ul>

TreeBuilder will normally (with C<p_strict> false) put the "ul" element
under the "p" element.  However, with C<p_strict> set to true, it will
close the "p" first.

In theory, there should be strictness options like this for other/all
elements besides just "p"; but I treat this as a specal case simply
because of the fact that "p" occurs so frequently and its end-tag is
omitted so often; and also because application of strictness rules
at parse-time across all elements often makes tiny errors in HTML
coding produce drastically bad parse-trees, in my experience.

If you find that you wish you had an option like this to enforce
content-models on all elements, then I suggest that what you want is
content-model checking as a stage after TreeBuilder has finished
parsing.

=item $root->store_comments(value)

This determines whether TreeBuilder will normally store comments found
while parsing content into C<$root>.  Currently, this is off by default.

=item $root->store_declarations(value)

This determines whether TreeBuilder will normally store markup
declarations found while parsing content into C<$root>.  Currently,
this is off by default.

It is somewhat of a known bug (to be fixed one of these days, if
anyone needs it?) that declarations in the preamble (before the "html"
start-tag) end up actually I<under> the "html" element.

=item $root->store_pis(value)

This determines whether TreeBuilder will normally store processing
instructions found while parsing content into C<$root> -- assuming a
recent version of HTML::Parser (old versions won't parse PIs
correctly).  Currently, this is off (false) by default.

It is somewhat of a known bug (to be fixed one of these days, if
anyone needs it?) that PIs in the preamble (before the "html"
start-tag) end up actually I<under> the "html" element.

=item $root->warn(value)

This determines whether syntax errors during parsing should generate
warnings, emitted via Perl's C<warn> function.

This is off (false) by default.

=back

=head1 HTML AND ITS DISCONTENTS

HTML is rather harder to parse than people who write it generally
suspect.

Here's the problem: HTML is a kind of SGML that permits "minimization"
and "implication".  In short, this means that you don't have to close
every tag you open (because the opening of a subsequent tag may
implicitly close it), and if you use a tag that can't occur in the
context you seem to using it in, under certain conditions the parser
will be able to realize you mean to leave the current context and
enter the new one, that being the only one that your code could
correctly be interpreted in.

Now, this would all work flawlessly and unproblematically if: 1) all
the rules that both prescribe and describe HTML were (and had been)
clearly set out, and 2) everyone was aware of these rules and wrote
their code in compliance to them.

However, it didn't happen that way, and so most HTML pages are
difficult if not impossible to correctly parse with nearly any set of
straightforward SGML rules.  That's why the internals of
HTML::TreeBuilder consist of lots and lots of special cases -- instead
of being just a generic SGML parser with HTML DTD rules plugged in.

=head1 BUGS

* Hopefully framesets behave correctly now.  Email me if you find a
strange parse of documents with framesets.

* Bad HTML code will, often as not, make for a bad parse tree. 
Regrettable, but unavoidably true.

* If you're running with implicit_tags off (God help you!), consider
that $tree->content_list probably contains the tree or grove from the
parse, and not $tree itself (which will, oddly enough, be an implicit
'html' element).  This seems counter-intuitive and problematic; but
seeing as how almost no HTML ever parses correctly with implicit_tags
off, this interface oddity seems the least of your problems.

=head1 BUG REPORTS

When a document parses in a way different from how you think it
should, I ask that you report this to me as a bug.  The first thing
you should do is copy the document, trim out as much of it as you can
while still producing the bug in question, and I<then> email me that
mini-document I<and> the code you're using to parse it, at C<sburke@cpan.org>.
Include a note as to how it 
parses (presumably including its $tree->dump output), and then a
I<careful and clear> explanation of where you think the parser is
going astray, and how you would prefer that it work instead.

=head1 SEE ALSO

L<HTML::Parser>, L<HTML::Element>, L<HTML::Tagset>

=head1 COPYRIGHT

Copyright 1995-1998 Gisle Aas; copyright 1999, 2000 Sean M. Burke.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 AUTHOR

Original author Gisle Aas E<lt>gisle@aas.noE<gt>; current maintainer
Sean M. Burke, E<lt>sburke@cpan.orgE<gt>

=cut

#==========================================================================

sub new { # constructor!
  my $class = shift;
  $class = ref($class) || $class;

  my $self = HTML::Element->new('html');  # Initialize HTML::Element part

  {
    # A hack for certain strange versions of Parser:
    my $other_self = HTML::Parser->new();
    %$self = (%$self, %$other_self);              # copy fields
      # Yes, multiple inheritance is messy.  Kids, don't try this at home.
    bless $other_self, "HTML::TreeBuilder::_hideyhole";
      # whack it out of the HTML::Parser class, to avoid the destructor
  }

  # The root of the tree is special, as it has these funny attributes,
  # and gets reblessed into this class.

  # Initialize parser settings
  $self->{'_implicit_tags'}  = 1;
  $self->{'_implicit_body_p_tag'} = 0;
    # If true, trying to insert text, or any of %isPhraseMarkup right
    #  under 'body' will implicate a 'p'.  If false, will just go there.

  $self->{'_tighten'} = 1;
    # whether ignorable WS in this tree should be deleted

  $self->{'_element_class'}      = 'HTML::Element';
  $self->{'_ignore_unknown'}     = 1;
  $self->{'_ignore_text'}        = 0;
  $self->{'_warn'}               = 0;
  $self->{'_store_comments'}     = 0;
  $self->{'_store_pis'}          = 0;
  $self->{'_store_declarations'} = 0;
  $self->{'_p_strict'} = 0;
  
  # Parse attributes passed in as arguments
  if(@_) {
    my %attr = @_;
    for (keys %attr) {
      $self->{"_$_"} = $attr{$_};
    }
  }

  # rebless to our class
  bless $self, $class;

  $self->{'_head'} = $self->insert_element('head',1);
  $self->{'_pos'} = undef; # pull it back up
  $self->{'_body'} = $self->insert_element('body',1);
  $self->{'_pos'} = undef; # pull it back up again
  $self->{'_implicit'} = 1;

  return $self;
}

#==========================================================================

sub _elem # universal accessor...
{
  my($self, $elem, $val) = @_;
  my $old = $self->{$elem};
  $self->{$elem} = $val if defined $val;
  return $old;
}

# accessors....
sub implicit_tags  { shift->_elem('_implicit_tags',  @_); }
sub implicit_body_p_tag  { shift->_elem('_implicit_body_p_tag',  @_); }
sub p_strict       { shift->_elem('_p_strict',  @_); }
sub ignore_unknown { shift->_elem('_ignore_unknown', @_); }
sub ignore_text    { shift->_elem('_ignore_text',    @_); }
sub ignore_ignorable_whitespace  { shift->_elem('_tighten',    @_); }
sub store_comments { shift->_elem('_store_comments', @_); }
sub store_declarations { shift->_elem('_store_declarations', @_); }
sub store_pis      { shift->_elem('_store_pis', @_); }
sub warn           { shift->_elem('_warn',           @_); }


#==========================================================================

sub warning {
    my $self = shift;
    CORE::warn("HTML::Parse: $_[0]\n") if $self->{'_warn'};
     # should maybe say HTML::TreeBuilder instead
}

#==========================================================================

sub start {
    return if $_[0]{'_stunted'};
    
  # Accept a signal from HTML::Parser for start-tags.
    my($self, $tag, $attr) = @_;
    # Parser passes more, actually:
    #   $self->start($tag, $attr, $attrseq, $origtext)
    # But we can merrily ignore $attrseq and $origtext.

    if($tag eq 'x-html') {
      print "Ignoring open-x-html tag.\n" if $Debug;
      # inserted by some lame code-generators.
      return;    # bypass tweaking.
    }

    my $ptag = (
                my $pos  = $self->{'_pos'} || $self
               )->{'_tag'};
    my $already_inserted;
    my($indent);
    if($Debug) {
      # optimization -- don't figure out indenting unless we're in debug mode
      my @lineage = $pos->lineage;
      $indent = '  ' x (1 + @lineage);
      print
        $indent, "Proposing a new \U$tag\E under ",
        join('/', map $_->{'_tag'}, reverse($pos, @lineage)) || 'Root',
        ".\n";
    #} else {
    #  $indent = ' ';
    }
    
    #print $indent, "POS: $pos ($ptag)\n" if $Debug > 2;
    
    my $e =
     ($self->{'_element_class'} || 'HTML::Element')->new($tag, %$attr);
     # Make a new element object.
     # (Only rarely do we end up just throwing it away later in this call.)
     
    # Some prep -- custom messiness for those damned tables, and strict P's.
    if($self->{'_implicit_tags'}) {
      
      unless($HTML::TreeBuilder::isTableElement{$tag}) {
        if ($ptag eq 'table') {
          print $indent,
            " * Phrasal \U$tag\E right under TABLE makes implicit TR and TD\n"
           if $Debug > 1;
          $self->insert_element('tr', 1);
          $pos = $self->insert_element('td', 1); # yes, needs updating
        } elsif ($ptag eq 'tr') {
          print $indent,
            " * Phrasal \U$tag\E right under TR makes an implicit TD\n"
           if $Debug > 1;
          $pos = $self->insert_element('td', 1); # yes, needs updating
        }
        $ptag = $pos->{'_tag'}; # yes, needs updating
      }
       # end of table-implication block.
      
      
      # Now maybe do a little dance to enforce P-strictness.
      # This seems like it should be integrated with the big
      # "ALL HOPE..." block, further below, but that doesn't
      # seem feasable.
      if(
        $self->{'_p_strict'}
        and $HTML::TreeBuilder::isKnown{$tag}
        and not $HTML::Tagset::is_Possible_Strict_P_Content{$tag}
      ) {
        my $here = $pos;
        my $here_tag = $ptag;
        while(1) {
          if($here_tag eq 'p') {
            print $indent,
              " * Inserting $tag closes strict P.\n" if $Debug > 1;
            $self->end(\'p'); # '
            last;
          }
          
          #print("Lasting from $here_tag\n"),
          last if
            $HTML::TreeBuilder::isKnown{$here_tag}
            and not $HTML::Tagset::is_Possible_Strict_P_Content{$here_tag};
           # Don't keep looking up the tree if we see something that can't
           #  be strict-P content.
          
          $here_tag = ($here = $here->{'_parent'} || last)->{'_tag'};
        }
      }
       # end of strict-p block.
      
    }
    
    # And now, get busy...
    #----------------------------------------------------------------------
    if (!$self->{'_implicit_tags'}) {
        # do nothing
        print $indent, " * _implicit_tags is off.  doing nothing\n"
         if $Debug > 1;

    #----------------------------------------------------------------------
    } elsif ($HTML::TreeBuilder::isHeadOrBodyElement{$tag}) {
        if ($pos->is_inside('body')) { # all is well
          print $indent,
            " * ambilocal element \U$tag\E is fine under BODY.\n"
           if $Debug > 1;
        } elsif ($pos->is_inside('head')) {
          print $indent,
            " * ambilocal element \U$tag\E is fine under HEAD.\n"
           if $Debug > 1;
        } else {
          # In neither head nor body!  mmmmm... put under head?
          
          if ($ptag eq 'html') { # expected case
            # TODO?? : would there ever be a case where _head would be
            #  absent from a tree that would ever be accessed at this
            #  point?
            die "Where'd my head go?" unless ref $self->{'_head'};
            if ($self->{'_head'}{'_implicit'}) {
              print $indent,
                " * ambilocal element \U$tag\E makes an implicit HEAD.\n"
               if $Debug > 1;
              # or rather, points us at it.
              $self->{'_pos'} = $self->{'_head'}; # to insert under...
            } else {
              $self->warning(
                "Ambilocal element <$tag> not under HEAD or BODY!?");
              # Put it under HEAD by default, I guess
              $self->{'_pos'} = $self->{'_head'}; # to insert under...
            }
            
          } else { 
            # Neither under head nor body, nor right under html... pass thru?
            $self->warning(
             "Ambilocal element <$tag> neither under head nor body, nor right under html!?");
          }
        }

    #----------------------------------------------------------------------
    } elsif ($HTML::TreeBuilder::isBodyElement{$tag}) {
        
        # Ensure that we are within <body>
        if($ptag eq 'body') {
            # We're good.
        } elsif($HTML::TreeBuilder::isBodyElement{$ptag}
          and not $HTML::TreeBuilder::isHeadOrBodyElement{$ptag}
        ) {
            # Special case: Save ourselves a call to is_inside further down.
            # If our $ptag is an isBodyElement element (but not an
            # isHeadOrBodyElement element), then we must be under body!
            print $indent, " * Inferring that $ptag is under BODY.\n",
             if $Debug > 3;
            # I think this and the test for 'body' trap everything
            # bodyworthy, except the case where the parent element is
            # under an unknown element that's a descendant of body.
        } elsif ($pos->is_inside('head')) {
            print $indent,
              " * body-element \U$tag\E minimizes HEAD, makes implicit BODY.\n"
             if $Debug > 1;
            $ptag = (
              $pos = $self->{'_pos'} = $self->{'_body'} # yes, needs updating
                || die "Where'd my body go?"
            )->{'_tag'}; # yes, needs updating
        } elsif (! $pos->is_inside('body')) {
            print $indent,
              " * body-element \U$tag\E makes implicit BODY.\n"
             if $Debug > 1;
            $ptag = (
              $pos = $self->{'_pos'} = $self->{'_body'} # yes, needs updating
                || die "Where'd my body go?"
            )->{'_tag'}; # yes, needs updating
        }
         # else we ARE under body, so okay.
        
        
        # Handle implicit endings and insert based on <tag> and position
        # ... ALL HOPE ABANDON ALL YE WHO ENTER HERE ...
        if ($tag eq 'p'  or
            $tag eq 'h1' or $tag eq 'h2' or $tag eq 'h3' or 
            $tag eq 'h4' or $tag eq 'h5' or $tag eq 'h6' or
            $tag eq 'form'
            # Hm, should <form> really be here?!
        ) {
            # Can't have <p>, <h#> or <form> inside these
            $self->end([qw(p h1 h2 h3 h4 h5 h6 pre textarea)],
                       @HTML::TreeBuilder::p_closure_barriers # used to be just li!
                      );
            
        } elsif ($tag eq 'ol' or $tag eq 'ul' or $tag eq 'dl') {
            # Can't have lists inside <h#> -- in the unlikely
            #  event anyone tries to put them there!
            if (
                $ptag eq 'h1' or $ptag eq 'h2' or $ptag eq 'h3' or 
                $ptag eq 'h4' or $ptag eq 'h5' or $ptag eq 'h6'
            ) {
                $self->end(\$ptag);
            }
        } elsif ($tag eq 'li') { # list item
            # Get under a list tag, one way or another
            unless(
              exists $HTML::TreeBuilder::isList{$ptag} or
              $self->end(\'*', keys %HTML::TreeBuilder::isList) #'
            ) { 
              print $indent,
                " * inserting implicit UL for lack of containing ",
                  join('|', keys %HTML::TreeBuilder::isList), ".\n"
               if $Debug > 1;
              $self->insert_element('ul', 1); 
            }
            
        } elsif ($tag eq 'dt' or $tag eq 'dd') {
            # Get under a DL, one way or another
            unless($ptag eq 'dl' or $self->end(\'*', 'dl')) { #'
              print $indent,
                " * inserting implicit DL for lack of containing DL.\n"
               if $Debug > 1;
              $self->insert_element('dl', 1);
            }
            
        } elsif ($HTML::TreeBuilder::isFormElement{$tag}) {
            if($self->{'_ignore_formies_outside_form'}  # TODO: document this
               and not $pos->is_inside('form')
            ) {
                print $indent,
                  " * ignoring \U$tag\E because not in a FORM.\n"
                  if $Debug > 1;
                return;    # bypass tweaking.
            }
            if($tag eq 'option') {
                # return unless $ptag eq 'select';
                $self->end(\'option'); #'
                $ptag = ($self->{'_pos'} || $self)->{'_tag'};
                unless($ptag eq 'select' or $ptag eq 'optgroup') {
                    print $indent, " * \U$tag\E makes an implicit SELECT.\n"
                       if $Debug > 1;
                    $pos = $self->insert_element('select', 1);
                    # but not a very useful select -- has no 'name' attribute!
                     # is $pos's value used after this?
                }
            }
        } elsif ($HTML::TreeBuilder::isTableElement{$tag}) {
            
            $self->end(\$tag, 'table'); #'
            # Hmm, I guess this is right.  To work it out:
            #   tr closes any open tr (limited at a table)
            #   td closes any open td (limited at a table)
            #   th closes any open th (limited at a table)
            #   thead closes any open thead (limited at a table)
            #   tbody closes any open tbody (limited at a table)
            #   tfoot closes any open tfoot (limited at a table)
            #   colgroup closes any open colgroup (limited at a table)
            #   col can try, but will always fail, at the enclosing table,
            #     as col is empty, and therefore never open!

            if(!$pos->is_inside('table')) {
                print $indent, " * \U$tag\E makes an implicit TABLE\n"
                  if $Debug > 1;
                $pos = $self->insert_element('table', 1);
                 # is $pos's value used after this?
            }
        } elsif ($HTML::TreeBuilder::isPhraseMarkup{$tag}) {
            if($ptag eq 'body' and $self->{'_implicit_body_p_tag'}) {
                print
                  " * Phrasal \U$tag\E right under BODY makes an implicit P\n"
                 if $Debug > 1;
                $pos = $self->insert_element('p', 1);
                 # is $pos's value used after this?
            }
        }
        # End of implicit endings logic
        
    # End of "elsif ($HTML::TreeBuilder::isBodyElement{$tag}"
    #----------------------------------------------------------------------
    
    } elsif ($HTML::TreeBuilder::isHeadElement{$tag}) {
        if ($pos->is_inside('body')) {
            print $indent, " * head element \U$tag\E found inside BODY!\n"
             if $Debug;
            $self->warning("Header element <$tag> in body");  # [sic]
        } elsif (!$pos->is_inside('head')) {
            print $indent, " * head element \U$tag\E makes an implicit HEAD.\n"
             if $Debug > 1;
        } else {
            print $indent,
              " * head element \U$tag\E goes inside existing HEAD.\n"
             if $Debug > 1;
        }
        $self->{'_pos'} = $self->{'_head'} || die "Where'd my head go?";

    #----------------------------------------------------------------------
    } elsif ($tag eq 'html') {
        if(delete $self->{'_implicit'}) { # first time here
            print $indent, " * good! found the real HTML element!\n"
             if $Debug > 1;
        } else {
            print $indent, " * Found a second HTML element\n"
             if $Debug;
            $self->warning("Found a nested <html> element");
        }

        # in either case, migrate attributes to the real element
        for (keys %$attr) {
            $self->attr($_, $attr->{$_});
        }
        $self->{'_pos'} = undef;
        return $self;    # bypass tweaking.

    #----------------------------------------------------------------------
    } elsif ($tag eq 'head') {
        my $head = $self->{'_head'} || die "Where'd my head go?";
        if(delete $head->{'_implicit'}) { # first time here
            print $indent, " * good! found the real HEAD element!\n"
             if $Debug > 1;
        } else { # been here before
            print $indent, " * Found a second HEAD element\n"
             if $Debug;
            $self->warning("Found a second <head> element");
        }

        # in either case, migrate attributes to the real element
        for (keys %$attr) {
            $head->attr($_, $attr->{$_});
        }
        return $self->{'_pos'} = $head;    # bypass tweaking.

    #----------------------------------------------------------------------
    } elsif ($tag eq 'body') {
        my $body = $self->{'_body'} || die "Where'd my body go?";
        if(delete $body->{'_implicit'}) { # first time here
            print $indent, " * good! found the real BODY element!\n"
             if $Debug > 1;
        } else { # been here before
            print $indent, " * Found a second BODY element\n"
             if $Debug;
            $self->warning("Found a second <body> element");
        }

        # in either case, migrate attributes to the real element
        for (keys %$attr) {
            $body->attr($_, $attr->{$_});
        }
        return $self->{'_pos'} = $body;    # bypass tweaking.

    #----------------------------------------------------------------------
    } elsif ($tag eq 'frameset') {
      if(
        !($self->{'_frameset_seen'}++)   # first frameset seen
        and !$self->{'_noframes_seen'}
          # otherwise it'll be under the noframes already
        and !$self->is_inside('body')
      ) {
        my $c = $self->{'_content'} || die "Contentless root?";
        my $body = $self->{'_body'} || die "Where'd my BODY go?";
        for(my $i = 0; $i < @$c; ++$i) {
          if($c->[$i] eq $body) {
            splice(@$c, $i, 0, $self->{'_pos'} = $pos = $e);
            $already_inserted = 1;
            print $indent, " * inserting 'frameset' right before BODY.\n"
             if $Debug > 1;
            last;
          }
        }
        die "BODY not found in children of root?" unless $already_inserted;
      }
 
    } elsif ($tag eq 'frame') {
        # Okay, fine, pass thru.
        # Should probably enforce that these should be under a frameset.
        # But hey.  Ditto for enforcing that 'noframes' should be under
        # a 'frameset', as the DTDs say.

    } elsif ($tag eq 'noframes') {
        # This basically assumes there'll be exactly one 'noframes' element
        #  per document.  At least, only the first one gets to have the
        #  body under it.  And if there are no noframes elements, then
        #  the body pretty much stays where it is.  Is that ever a problem?
        if($self->{'_noframes_seen'}++) {
          print $indent, " * ANOTHER noframes element?\n" if $Debug;
        } else {
          if($pos->is_inside('body')) {
            print $indent, " * 'noframes' inside 'body'.  Odd!\n" if $Debug;
            # In that odd case, we /can't/ make body a child of 'noframes',
            # because it's an ancestor of the 'noframes'!
          } else {
            $e->push_content( $self->{'_body'} || die "Where'd my body go?" );
            print $indent, " * Moving body to be under noframes.\n" if $Debug;
          }
        }

    #----------------------------------------------------------------------
    } else {
        # unknown tag
        if ($self->{'_ignore_unknown'}) {
            print $indent, " * Ignoring unknown tag \U$tag\E\n" if $Debug;
            $self->warning("Skipping unknown tag $tag");
            return;
        } else {
            print $indent, " * Accepting unknown tag \U$tag\E\n"
              if $Debug;
        }
    }
    #----------------------------------------------------------------------
     # End of mumbo-jumbo
    
    
    print
      $indent, "(Attaching ", $e->{'_tag'}, " under ",
      ($self->{'_pos'} || $self)->{'_tag'}, ")\n"
        # because if _pos isn't defined, it goes under self
     if $Debug;
    
    
    # The following if-clause is to delete /some/ ignorable whitespace
    #  nodes, as we're making the tree.
    # This'd be a node we'd catch later anyway, but we might as well
    #  nip it in the bud now.
    # This doesn't catch /all/ deletable WS-nodes, so we do have to call
    #  the tightener later to catch the rest.

    if($self->{'_tighten'} and !$self->{'_ignore_text'}) {  # if tightenable
      my($sibs, $par);
      if(
         ($sibs = ( $par = $self->{'_pos'} || $self )->{'_content'})
         and @$sibs  # parent already has content
         and !ref($sibs->[-1])  # and the last one there is a text node
         and $sibs->[-1] !~ m<\S>s  # and it's all whitespace

         and (  # one of these has to be eligible...
               $HTML::TreeBuilder::canTighten{$tag}
               or
               (
                 (@$sibs == 1)
                   ? # WS is leftmost -- so parent matters
                     $HTML::TreeBuilder::canTighten{$par->{'_tag'}}
                   : # WS is after another node -- it matters
                     (ref $sibs->[-2]
                      and $HTML::TreeBuilder::canTighten{$sibs->[-2]{'_tag'}}
                     )
               )
             )

         and !$par->is_inside('pre', 'xmp', 'textarea', 'plaintext')
                # we're clear
      ) {
        pop @$sibs;
        print $indent, "Popping a preceding all-WS node\n" if $Debug;
      }
    }
    
    $self->insert_element($e) unless $already_inserted;

    if($Debug) {
      if($self->{'_pos'}) {
        print
          $indent, "(Current lineage of pos:  \U$tag\E under ",
          join('/',
            reverse(
              # $self->{'_pos'}{'_tag'},  # don't list myself!
              $self->{'_pos'}->lineage_tag_names
            )
          ),
          ".)\n";
      } else {
        print $indent, "(Pos points nowhere!?)\n";
      }
    }

    unless(($self->{'_pos'} || '') eq $e) {
      # if it's an empty element -- i.e., if it didn't change the _pos
      &{  $self->{"_tweak_$tag"}
          ||  $self->{'_tweak_*'}
          || return $e
      }(map $_,   $e, $tag, $self); # make a list so the user can't clobber
    }

    return $e;
}

#==========================================================================

sub end {
    return if $_[0]{'_stunted'};
    
  # Either: Acccept an end-tag signal from HTML::Parser
  # Or: Method for closing currently open elements in some fairly complex
  #  way, as used by other methods in this class.
    my($self, $tag, @stop) = @_;
    if($tag eq 'x-html') {
      print "Ignoring close-x-html tag.\n" if $Debug;
      # inserted by some lame code-generators.
      return;
    }

    # This method accepts two calling formats:
    #  1) from Parser:  $self->end('tag_name', 'origtext')
    #        in which case we shouldn't mistake origtext as a blocker tag
    #  2) from myself:  $self->end(\'tagname1', 'blk1', ... )
    #     from myself:  $self->end(['tagname1', 'tagname2'], 'blk1',  ... )
    
    # End the specified tag, but don't move above any of the blocker tags.
    # The tag can also be a reference to an array.  Terminate the first
    # tag found.
    
    my $ptag = ( my $p = $self->{'_pos'} || $self )->{'_tag'};
     # $p and $ptag are sort-of stratch
    
    if(ref($tag)) {
      # First param is a ref of one sort or another --
      #  THE CALL IS COMING FROM INSIDE THE HOUSE!
      $tag = $$tag if ref($tag) eq 'SCALAR';
       # otherwise it's an arrayref.
    } else {
      # the call came from Parser -- just ignore origtext
      @stop = ();
    }
    
    my($indent);
    if($Debug) {
      # optimization -- don't figure out depth unless we're in debug mode
      my @lineage_tags = $p->lineage_tag_names;
      $indent = '  ' x (1 + @lineage_tags);
      
      # now announce ourselves
      print $indent, "Ending ",
        ref($tag) ? ('[', join(' ', @$tag ), ']') : "\U$tag\E",
        scalar(@stop) ? (" no higher than [", join(' ', @stop), "]" )
          : (), ".\n"
      ;
      
      print $indent, " (Current lineage: ", join('/', @lineage_tags), ".)\n"
       if $Debug > 1;
       
      if($Debug > 3) {
        #my(
        # $package, $filename, $line, $subroutine,
        # $hasargs, $wantarray, $evaltext, $is_require) = caller;
        print $indent,
          " (Called from ", (caller(1))[3], ' line ', (caller(1))[2],
          ")\n";
      }
      
    #} else {
    #  $indent = ' ';
    }
    # End of if $Debug
    
    # Now actually do it
    my @to_close;
    if($tag eq '*') {
      # Special -- close everything up to (but not including) the first
      #  limiting tag, or return if none found.  Somewhat of a special case.
     PARENT:
      while (defined $p) {
        $ptag = $p->{'_tag'};
        print $indent, " (Looking at $ptag.)\n" if $Debug > 2;
        for (@stop) {
          if($ptag eq $_) {
            print $indent, " (Hit a $_; closing everything up to here.)\n"
             if $Debug > 2;
            last PARENT;
          }
        }
        push @to_close, $p;
        $p = $p->{'_parent'}; # no match so far? keep moving up
        print
          $indent, 
          " (Moving on up to ", $p ? $p->{'_tag'} : 'nil', ")\n"
         if $Debug > 1;
        ;
      }
      unless(defined $p) { # We never found what we were looking for.
        print $indent, " (We never found a limit.)\n" if $Debug > 1;
        return;
      }
      #print
      #   $indent,
      #   " (To close: ", join('/', map $_->tag, @to_close), ".)\n"
      #  if $Debug > 4;
      
      # Otherwise update pos and fall thru.
      $self->{'_pos'} = $p;
    } elsif (ref $tag) {
      # Close the first of any of the matching tags, giving up if you hit
      #  any of the stop-tags.
     PARENT:
      while (defined $p) {
        $ptag = $p->{'_tag'};
        print $indent, " (Looking at $ptag.)\n" if $Debug > 2;
        for (@$tag) {
          if($ptag eq $_) {
            print $indent, " (Closing $_.)\n" if $Debug > 2;
            last PARENT;
          }
        }
        for (@stop) {
          if($ptag eq $_) {
            print $indent, " (Hit a limiting $_ -- bailing out.)\n"
             if $Debug > 1;
            return; # so it was all for naught
          }
        }
        push @to_close, $p;
        $p = $p->{'_parent'};
      }
      return unless defined $p; # We went off the top of the tree.
      # Otherwise specified element was found; set pos to its parent.
      push @to_close, $p;
      $self->{'_pos'} = $p->{'_parent'};
    } else {
      # Close the first of the specified tag, giving up if you hit
      #  any of the stop-tags.
      while (defined $p) {
        $ptag = $p->{'_tag'};
        print $indent, " (Looking at $ptag.)\n" if $Debug > 2;
        if($ptag eq $tag) {
          print $indent, " (Closing $tag.)\n" if $Debug > 2;
          last;
        }
        for (@stop) {
          if($ptag eq $_) {
            print $indent, " (Hit a limiting $_ -- bailing out.)\n"
             if $Debug > 1;
            return; # so it was all for naught
          }
        }
        push @to_close, $p;
        $p = $p->{'_parent'};
      }
      return unless defined $p; # We went off the top of the tree.
      # Otherwise specified element was found; set pos to its parent.
      push @to_close, $p;
      $self->{'_pos'} = $p->{'_parent'};
    }
    
    $self->{'_pos'} = undef if $self eq ($self->{'_pos'} || '');
    print $indent, "(Pos now points to ",
      $self->{'_pos'} ? $self->{'_pos'}{'_tag'} : '???', ".)\n"
     if $Debug > 1;
    
    ### EXPENSIVE, because has to check that it's not under a pre
    ### or a CDATA-parent.  That's one more method call per end()!
    ### Might as well just do this at the end of the tree-parse, I guess,
    ### at which point we'd be parsing top-down, and just not traversing
    ### under pre's or CDATA-parents.
    ##
    ## Take this opportunity to nix any terminal whitespace nodes.
    ## TODO: consider whether this (plus the logic in start(), above)
    ## would ever leave any WS nodes in the tree.
    ## If not, then there's no reason to have eof() call
    ## delete_ignorable_whitespace on the tree, is there?
    ##
    #if(@to_close and $self->{'_tighten'} and !$self->{'_ignore_text'} and
    #  ! $to_close[-1]->is_inside('pre', keys %HTML::Tagset::isCDATA_Parent)
    #) {  # if tightenable
    #  my($children, $e_tag);
    #  foreach my $e (reverse @to_close) { # going top-down
    #    last if 'pre' eq ($e_tag = $e->{'_tag'}) or
    #     $HTML::Tagset::isCDATA_Parent{$e_tag};
    #    
    #    if(
    #      $children = $e->{'_content'}
    #      and @$children      # has children
    #      and !ref($children->[-1])
    #      and $children->[-1] =~ m<^\s+$>s # last node is all-WS
    #      and
    #        (
    #         # has a tightable parent:
    #         $HTML::TreeBuilder::canTighten{ $e_tag }
    #         or
    #          ( # has a tightenable left sibling:
    #            @$children > 1 and 
    #            ref($children->[-2])
    #            and $HTML::TreeBuilder::canTighten{ $children->[-2]{'_tag'} }
    #          )
    #        )
    #    ) {
    #      pop @$children;
    #      #print $indent, "Popping a terminal WS node from ", $e->{'_tag'},
    #      #  " (", $e->address, ") while exiting.\n" if $Debug;
    #    }
    #  }
    #}
    
    
    foreach my $e (@to_close) {
      # Call the applicable callback, if any
      $ptag = $e->{'_tag'};
      &{  $self->{"_tweak_$ptag"}
          ||  $self->{'_tweak_*'}
          || next
      }(map $_,   $e, $ptag, $self);
      print $indent, "Back from tweaking.\n" if $Debug;
      last if $self->{'_stunted'}; # in case one of the handlers called stunt
    }
    return @to_close;
}

#==========================================================================

sub text {
    return if $_[0]{'_stunted'};
    
  # Accept a "here's a text token" signal from HTML::Parser.
    my($self, $text, $is_cdata) = @_;
      # the >3.0 versions of Parser may pass a cdata node.
      # Thanks to Gisle Aas for pointing this out.
    
    return unless length $text; # I guess that's always right
    
    my $ignore_text = $self->{'_ignore_text'};
    
    my $pos = $self->{'_pos'} || $self;
    
    HTML::Entities::decode($text)
     unless $ignore_text || $is_cdata
      || $HTML::Tagset::isCDATA_Parent{$pos->{'_tag'}};
    
    my($indent, $nugget);
    if($Debug) {
      # optimization -- don't figure out depth unless we're in debug mode
      my @lineage_tags = $pos->lineage_tag_names;
      $indent = '  ' x (1 + @lineage_tags);
      
      $nugget = (length($text) <= 25) ? $text : (substr($text,0,25) . '...');
      $nugget =~ s<([\x00-\x1F])>
                 <'\\x'.(unpack("H2",$1))>eg;
      print
        $indent, "Proposing a new text node ($nugget) under ",
        join('/', reverse($pos->{'_tag'}, @lineage_tags)) || 'Root',
        ".\n";
      
    #} else {
    #  $indent = ' ';
    }
    
    
    my $ptag;
    if ($HTML::Tagset::isCDATA_Parent{$ptag = $pos->{'_tag'}}
        or $pos->is_inside('pre')
    ) {
        return if $ignore_text;
        $pos->push_content($text);
    } else {
        # return unless $text =~ /\S/;  # This is sometimes wrong
        
        if (!$self->{'_implicit_tags'} || $text !~ /\S/) {
            # don't change anything
        } elsif ($ptag eq 'head' or $ptag eq 'noframes') {
            if($self->{'_implicit_body_p_tag'}) {
              print $indent,
                " * Text node under \U$ptag\E closes \U$ptag\E, implicates BODY and P.\n"
               if $Debug > 1;
              $self->end(\$ptag);
              $pos =
                $self->{'_body'}
                ? ($self->{'_pos'} = $self->{'_body'}) # expected case
                : $self->insert_element('body', 1);
              $pos = $self->insert_element('p', 1);
            } else {
              print $indent,
                " * Text node under \U$ptag\E closes, implicates BODY.\n"
               if $Debug > 1;
              $self->end(\$ptag);
              $pos =
                $self->{'_body'}
                ? ($self->{'_pos'} = $self->{'_body'}) # expected case
                : $self->insert_element('body', 1);
            }
        } elsif ($ptag eq 'html') {
            if($self->{'_implicit_body_p_tag'}) {
              print $indent,
                " * Text node under HTML implicates BODY and P.\n"
               if $Debug > 1;
              $pos =
                $self->{'_body'}
                ? ($self->{'_pos'} = $self->{'_body'}) # expected case
                : $self->insert_element('body', 1);
              $pos = $self->insert_element('p', 1);
            } else {
              print $indent,
                " * Text node under HTML implicates BODY.\n"
               if $Debug > 1;
              $pos =
                $self->{'_body'}
                ? ($self->{'_pos'} = $self->{'_body'}) # expected case
                : $self->insert_element('body', 1);
              #print "POS is $pos, ", $pos->{'_tag'}, "\n";
            }
        } elsif ($ptag eq 'body') {
            if($self->{'_implicit_body_p_tag'}) {
              print $indent,
                " * Text node under BODY implicates P.\n"
               if $Debug > 1;
              $pos = $self->insert_element('p', 1);
            }
        } elsif ($ptag eq 'table') {
            print $indent,
              " * Text node under TABLE implicates TR and TD.\n"
             if $Debug > 1;
            $self->insert_element('tr', 1);
            $pos = $self->insert_element('td', 1);
             # double whammy!
        } elsif ($ptag eq 'tr') {
            print $indent,
              " * Text node under TR implicates TD.\n"
             if $Debug > 1;
            $pos = $self->insert_element('td', 1);
        }
        # elsif (
        #       # $ptag eq 'li'   ||
        #       # $ptag eq 'dd'   ||
        #         $ptag eq 'form') {
        #    $pos = $self->insert_element('p', 1);
        #}
        
        
        # Whatever we've done above should have had the side
        # effect of updating $self->{'_pos'}
        
                
        #print "POS is now $pos, ", $pos->{'_tag'}, "\n";
        
        return if $ignore_text;
        $text =~ s/\s+/ /g;  # canonical space
        
        print
          $indent, " (Attaching text node ($nugget) under ",
          # was: $self->{'_pos'} ? $self->{'_pos'}{'_tag'} : $self->{'_tag'},
          $pos->{'_tag'},
          ").\n"
         if $Debug > 1;
        
        $pos->push_content($text);
    }
    
    &{ $self->{'_tweak_~text'} || return }($text, $pos, $pos->{'_tag'} . '');
     # Note that this is very exceptional -- it doesn't fall back to
     #  _tweak_*, and it gives its tweak different arguments.
    return;
}

#==========================================================================

# TODO: test whether comment(), declaration(), and process(), do the right
#  thing as far as tightening and whatnot.
# Also, currently, doctypes and comments that appear before head or body
#  show up in the tree in the wrong place.  Something should be done about
#  this.  Tricky.  Maybe this whole business of pre-making the body and
#  whatnot is wrong.

sub comment {
  return if $_[0]{'_stunted'};
  # Accept a "here's a comment" signal from HTML::Parser.

  my($self, $text) = @_;
  my $pos = $self->{'_pos'} || $self;
  return unless $self->{'_store_comments'}
     || $HTML::Tagset::isCDATA_Parent{ $pos->{'_tag'} };
  
  if($Debug) {
    my @lineage_tags = $pos->lineage_tag_names;
    my $indent = '  ' x (1 + @lineage_tags);
    
    my $nugget = (length($text) <= 25) ? $text : (substr($text,0,25) . '...');
    $nugget =~ s<([\x00-\x1F])>
                 <'\\x'.(unpack("H2",$1))>eg;
    print
      $indent, "Proposing a Comment ($nugget) under ",
      join('/', reverse($pos->{'_tag'}, @lineage_tags)) || 'Root',
      ".\n";
  }

  (my $e = (
    $self->{'_element_class'} || 'HTML::Element'
   )->new('~comment'))->{'text'} = $text;
  $pos->push_content($e);

  &{  $self->{'_tweak_~comment'}
      || $self->{'_tweak_*'}
      || return $e
   }(map $_,   $e, '~comment', $self);
  
  return $e;
}

#==========================================================================
# TODO: currently this puts declarations in just the wrong place.
#  How to correct? look at pos->_content, and go to insert at end, 
#  but back up before any head elements?  Do that just if implicit
#  mode is on?

sub declaration {
  return if $_[0]{'_stunted'};
  # Accept a "here's a markup declaration" signal from HTML::Parser.

  return unless $_[0]->{'_store_declarations'};
  my($self, $text) = @_;
  my $pos = $self->{'_pos'} || $self;
  
  if($Debug) {
    my @lineage_tags = $pos->lineage_tag_names;
    my $indent = '  ' x (1 + @lineage_tags);
    
    my $nugget = (length($text) <= 25) ? $text : (substr($text,0,25) . '...');
    $nugget =~ s<([\x00-\x1F])>
                 <'\\x'.(unpack("H2",$1))>eg;
    print
      $indent, "Proposing a Declaration ($nugget) under ",
      join('/', reverse($pos->{'_tag'}, @lineage_tags)) || 'Root',
      ".\n";
  }
  (my $e = (
    $self->{'_element_class'} || 'HTML::Element'
   )->new('~declaration'))->{'text'} = $text;
  $pos->push_content($e);

  &{  $self->{'_tweak_~declaration'}
      || $self->{'_tweak_*'}
      || return $e
   }(map $_, $e,   '~declaration', $self);
  
  return $e;
}

#==========================================================================

sub process {
  return if $_[0]{'_stunted'};
  # Accept a "here's a PI" signal from HTML::Parser.

  return unless $_[0]->{'_store_pis'};
  my($self, $text) = @_;
  my $pos = $self->{'_pos'} || $self;
  
  if($Debug) {
    my @lineage_tags = $pos->lineage_tag_names;
    my $indent = '  ' x (1 + @lineage_tags);
    
    my $nugget = (length($text) <= 25) ? $text : (substr($text,0,25) . '...');
    $nugget =~ s<([\x00-\x1F])>
                 <'\\x'.(unpack("H2",$1))>eg;
    print
      $indent, "Proposing a PI ($nugget) under ",
      join('/', reverse($pos->{'_tag'}, @lineage_tags)) || 'Root',
      ".\n";
  }
  (my $e = (
    $self->{'_element_class'} || 'HTML::Element'
   )->new('~pi'))->{'text'} = $text;
  $pos->push_content($e);

  &{  $self->{'_tweak_~pi'}
      || $self->{'_tweak_*'}
      || return $e
   }(map $_,   $e, '~pi', $self);
  
  return $e;
}


#==========================================================================

#When you call $tree->parse_file($filename), and the
#tree's ignore_ignorable_whitespace attribute is on (as it is
#by default), HTML::TreeBuilder's logic will manage to avoid
#creating some, but not all, nodes that represent ignorable
#whitespace.  However, at the end of its parse, it traverses the
#tree and deletes any that it missed.  (It does this with an
#around-method around HTML::Parser's eof method.)
#
#However, with $tree->parse($content), the cleanup-traversal step
#doesn't happen automatically -- so when you're done parsing all
#content for a document (regardless of whether $content is the only
#bit, or whether it's just another chunk of content you're parsing into
#the tree), call $tree->eof() to signal that you're at the end of the
#text you're inputting to the tree.  Besides properly cleaning any bits
#of ignorable whitespace from the tree, this will also ensure that
#HTML::Parser's internal buffer is flushed.

sub eof {
  # Accept an "end-of-file" signal from HTML::Parser, or thrown by the user.
  return $_[0]->SUPER::eof() if $_[0]->{'_stunted'};
  
  my $x = $_[0];
  print "EOF received.\n" if $Debug;
  my $rv = $x->SUPER::eof(); # assumes a scalar return value
  
  $x->end('html') unless $x eq ($x->{'_pos'} || $x);
   # That SHOULD close everything, and will run the appropriate tweaks.
   # We /could/ be running under some insane mode such that there's more
   #  than one HTML element, but really, that's just insane to do anyhow.

  unless($x->{'_implicit_tags'}) {
    # delete those silly implicit head and body in case we put
    # them there in implicit tags mode
    foreach my $node ($x->{'_head'}, $x->{'_body'}) {
      $node->replace_with_content
       if defined $node and ref $node
          and $node->{'_implicit'} and $node->{'_parent'};
       # I think they should be empty anyhow, since the only
       # logic that'd insert under them can apply only, I think,
       # in the case where _implicit_tags is on
    }
    # this may still leave an implicit 'html' at the top, but there's
    # nothing we can do about that, is there?
  }
  
  $x->delete_ignorable_whitespace()
   # this's why we trap this -- an after-method
   if $x->{'_tighten'} and ! $x->{'_ignore_text'};
  return $rv;
}

#==========================================================================

# TODO: document

sub stunt {
  my $self = $_[0];
  print "Stunting the tree.\n" if $Debug;
  
  if($HTML::Parser::VERSION < 3) {
    #This is a MEAN MEAN HACK.  And it works most of the time!
    $self->{'_buf'} = '';
    my $fh = *HTML::Parser::F{IO};
    # the local'd FH used by parse_file loop
    if(defined $fh) {
      print "Closing Parser's filehandle $fh\n" if $Debug;
      close($fh);
    }
    
    # But if they called $tree->parse_file($filehandle)
    #  or $tree->parse_file(*IO), then there will be no *HTML::Parser::F{IO}
    #  to close.  Ahwell.  Not a problem for most users these days.
    
  } else {
    $self->SUPER::eof();
     # Under 3+ versions, calling eof from inside a parse will abort the
     #  parse / parse_file
  }
  
  # In the off chance that the above didn't work, we'll throw
  #  this flag to make any future events be no-ops.
  $self->stunted(1);
  return;
}

# TODO: document
sub stunted  { shift->_elem('_stunted',  @_); }

#==========================================================================

sub delete {
  # Override Element's delete method.
  # This does most, if not all, of what Element's delete does anyway.
  # Deletes content, including content in some special attributes.
  # But doesn't empty out the hash.

  delete @{$_[0]}{'_body', '_head', '_pos'};
  for (@{ delete($_[0]->{'_content'})
          || []
        }, # all/any content
#       delete @{$_[0]}{'_body', '_head', '_pos'}
         # ...and these, in case these elements don't appear in the
         #   content, which is possible.  If they did appear (as they
         #   usually do), then calling $_->delete on them again is harmless.
#  I don't think that's such a hot idea now.  Thru creative reattachment,
#  those could actually now point to elements in OTHER trees (which we do
#  NOT want to delete!).
## Reasoned out:
#  If these point to elements not in the content list of any element in this
#   tree, but not in the content list of any element in any OTHER tree, then
#   just deleting these will make their refcounts hit zero.
#  If these point to elements in the content lists of elements in THIS tree,
#   then we'll get to deleting them when we delete from the top.
#  If these point to elements in the content lists of elements in SOME OTHER
#   tree, then they're not to be deleted.
      )
  {
    $_->delete
     if defined $_ and ref $_   #  Make sure it's an object.
        and $_ ne $_[0];   #  And avoid hitting myself, just in case!
  }

  return undef;
}

sub tighten_up { # legacy
  shift->delete_ignorable_whitespace(@_);
}

#--------------------------------------------------------------------------
1;

__END__

CUCUMBER AND ORANGE SALAD

Adapted from:

Holzner, Yupa.  /Great Thai Cooking for My American Friends: Creative
Thai Dishes Made Easy/.  Royal House 1989.  ISBN:0930440277

Makes about four servings.

 1 small greenhouse cucumber, sliced thin.
 2 navel oranges, peeled, separated into segments (seeded if
   necessary), and probably chopped into chunks.

Dressing:
 3 tablespoons rice vinegar
 .5 teaspoon salt
 1 tablespoon sugar
 1 teaspoon roasted sesame seeds
 .5 tablespoons mirin

Mix all ingredients in the dressing.  Pour the dressing over the
oranges and cucumbers, and toss.  Serve.


All ingredient quantities are approximate.  Adjust to taste.  Multipy
quantities for more servings.

Note: the sesame seeds you get in an American supermarket are almost
certain to be unroasted, suspiciously waxy-looking, and astronomically
overpriced.  Good cheap sesame seeds are available at most Asian
markets -- they usually come roasted; if they're unroasted, roast them
in a skillet for a few short minutes as needed.

[End Recipe]
