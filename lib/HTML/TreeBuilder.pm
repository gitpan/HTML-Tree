
# Time-stamp: "1999-12-21 12:56:35 MST"
package HTML::TreeBuilder;

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
    $tree = $tree->destroy;
  }

=head1 DESCRIPTION

This class is for HTML syntax trees.  The way to use it is to:

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
FALSE value as argument.  E.g., $p->implicit_tags returns the current
setting for the implicit_tags option, $p->implicit_tags(1) turns that
option on, and $p->implicit_tags(0) turns it off.

=over 4

=item $p->implicit_tags(value)

Setting this attribute to true will instruct the parser to try to
deduce implicit elements and implicit end tags.  If it is false you
get a parse tree that just reflects the text as it stands, which is
unlikely to be useful for anything but quick and dirty parsing.
Default is true.

Implicit elements have the implicit() attribute set.

=item $p->implicit_body_p_tag(value)

This controls an aspect of implicit element behavior, if implicit_tags
is on:  If a text element (PCDATA) or a phrasal element (such as
"E<lt>emE<gt>") is to be inserted under "E<lt>bodyE<gt>", two things
can happen: if implicit_body_p_tag is true, it's placed under a new,
implicit "E<lt>pE<gt>" tag.  (Past DTDs suggested this was the only
correct behavior, and this is how past versions of this module
behaved.)  But if implicit_body_p_tag is false, nothing is implicated
-- the PCDATA or phrasal element is simply placed under
"E<lt>bodyE<gt>".  Default is false.

=item $p->ignore_unknown(value)

This attribute controls whether unknown tags should be represented as
elements in the parse tree, or whether they should be ignored. 
Default is true (to ignore unknown tags.)

=item $p->ignore_text(value)

Do not represent the text content of elements.  This saves space if
all you want is to examine the structure of the document.  Default is
false.

=item $p->ignore_ignorable_whitespace(value)

If set to true, TreeBuilder will try to delete (and/or to avoid
creating) ignorable whitespace text nodes in the tree.  Default is
true.  (In fact, I'd be interested in hearing if there's ever a case
where you need this off, or where leaving it on leads to incorrect
behavior.)

=item $p->warn(value)

This determines whether syntax errors during parsing should generate
warnings, emitted via Perl's C<warn> function.

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

* Currently, it's assumed that "HTML" is the top node in the tree, and
that "HEAD" and "BODY" must be right under "HTML".  Framesets are
therefore coerced into being under "BODY", even if the document in
question has the "BODY" I<inside> a "NOFRAMES" element. This may
change in a future version, I<particularly> if anyone points out a
case where this is troublesome for them.

* Bad HTML code will, often as not, make for a bad parse tree. 
Regrettable, but unavoidably true.

=head1 BUG REPORTS

When a document parses in a way different from how you think it
should, I ask that you report this to me as a bug.  The first thing
you should do is copy the document, trim out as much of it as you can
while still producing the bug in question, and I<then> email me that
mini-document at C<sburke@netadventure.net>, with a note as to how it
parses (presumably including its $tree->dump output), and then a
I<careful and clear> explanation of where you think the parser is
going astray, and how you would prefer that it work instead.

=head1 SEE ALSO

L<HTML::Parser>, L<HTML::Element>

=head1 COPYRIGHT

Copyright 1995-1998 Gisle Aas, copyright 1999 Sean M. Burke.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=head1 AUTHOR

Original author Gisle Aas E<lt>gisle@aas.noE<gt>; current maintainer
Sean M. Burke, E<lt>sburke@netadventure.netE<gt>

=cut

use HTML::Entities ();

use strict;
use vars qw(@ISA $VERSION $Debug
            %isHeadElement %isBodyElement %isPhraseMarkup
            %isHeadOrBodyElement
            %isList %isTableElement %isFormElement
            %isKnown %canTighten
            @p_closure_barriers
           );

$Debug = 0 unless defined $Debug;

require HTML::Element;
require HTML::Parser;
@ISA = qw(HTML::Element HTML::Parser);
$VERSION = '2.92';

#==========================================================================
# List of all elements from Extensible HTML version 1.0 Transitional DTD:
#
#   a abbr acronym address applet area b base basefont bdo big
#   blockquote body br button caption center cite code col colgroup
#   dd del dfn dir div dl dt em fieldset font form h1 h2 h3 h4 h5 h6
#   head hr html i iframe img input ins isindex kbd label legend li
#   link map menu meta noframes noscript object ol optgroup option p
#   param pre q s samp script select small span strike strong style
#   sub sup table tbody td textarea tfoot th thead title tr tt u ul
#   var
#
# Varia from Mozilla source internal table of tags:
#   Implemented:
#     xmp listing wbr nobr frame frameset noframes ilayer
#     layer nolayer spacer embed multicol
#   But these are unimplemented:
#     sound??  keygen??  server??
# Also seen here and there:
#     marquee??  app??  (both unimplemented)
#==========================================================================

%isPhraseMarkup = map { $_ => 1 } qw(
  span abbr acronym q sub sup
  cite code em kbd samp strong var dfn strike
  b i u s tt small big 
  a img br
  wbr nobr blink
  font basefont bdo
  spacer embed noembed
);  # had: center, hr, table

# Elements that should be present only in the head
%isHeadElement = map { $_ => 1 }
 qw(title base link meta isindex script style object bgsound);

%isList         = map { $_ => 1 } qw(ul ol dir menu);
%isTableElement = map { $_ => 1 }
 qw(tr td th thead tbody tfoot caption col colgroup);

%isFormElement  = map { $_ => 1 }
 qw(input select option optgroup textarea button label);

# Elements that should be present only in/under the body
%isBodyElement = map { $_ => 1 } qw(
  h1 h2 h3 h4 h5 h6
  p div pre plaintext address blockquote
  xmp listing
  center

  multicol
  frame frameset noframes
  iframe ilayer nolayer
  bgsound

  hr
  ol ul dir menu li
  dl dt dd
  ins del
  
  fieldset legend
  
  map area
  applet param object
  isindex script noscript
  table
  center
  form
 ),
 keys %isFormElement,
 keys %isPhraseMarkup,   # And everything phrasal
 keys %isTableElement,
;

# Elements that can be present in the head or the body.
%isHeadOrBodyElement = map { $_ => 1 }
  qw(script isindex style object map area param noscript bgsound);
  # i.e., if we find 'script' in the 'body' or the 'head', don't freak out.


%isKnown = (%isHeadElement, %isBodyElement, map{$_=>1} qw(head body html));
 # that should be all known tags ever ever
#print join(' ',%isKnown), "\n";


%canTighten = %isKnown;
delete @canTighten{keys(%isPhraseMarkup), 'input', 'select'};
  # xmp, listing, plaintext, and pre  are untightenable, but
  #   in a /really/ special way that we hardcode in, later.
@canTighten{'hr','br'} = (1,1);
 # exceptional 'phrasal' things that ARE subject to tightening.

# The one case where I can think of my tightening rules failing is:
#  <p>foo bar<center> <em>baz quux</em> ...
#                    ^-- that would get deleted.
# But that's pretty gruesome code anyhow.  You gets what you pays for.

#==========================================================================

# When we see a <p> token, we go lookup up the lineage for a <p> we might
# have to minimize.  At first sight, we might say that if there's a <p>
# anywhere in the lineage of this new <p>, it should be closed.  But
# that's wrong.  Consider this document:
#
# <html>
#   <head>
#     <title>foo</title>
#   </head>
#   <body>
#     <p>foo
#       <table>
#         <tr>
#           <td>
#              foo
#              <p>bar
#           </td>
#         </tr>
#       </table>
#     </p>
#   </body>
# </html>
#
# The second p is quite legally inside a much higher p.
#
# My formalization of the reason why this is legal, but <p>foo<p>bar</p></p>
# isn't, is that something about the table constitutes a "barrier" to the
# application of the rule about what p must minimize.
# 
# And here is the list of all such barrier-tags:
@p_closure_barriers = qw(
  li blockquote
  ul ol menu dir
  dl dt dd
  td th tr table caption
 );

# In an ideal world (i.e., XHTML) we wouldn't have to bother with any of this
# monkey business of barriers to minimization!

#==========================================================================

sub new {
  my $class = shift;
  $class = ref($class) || $class;

  my $self = HTML::Element->new('html');  # Initialize HTML::Element part

  {
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

  $self->{'_ignore_unknown'} = 1;
  $self->{'_ignore_text'}    = 0;
  $self->{'_warn'}           = 0;

  # Parse attributes passed in as arguments
  my %attr = @_;
  for (keys %attr) {
    $self->{"_$_"} = $attr{$_};
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
sub ignore_unknown { shift->_elem('_ignore_unknown', @_); }
sub ignore_text    { shift->_elem('_ignore_text',    @_); }
sub ignore_ignorable_whitespace  { shift->_elem('_tighten',    @_); }
sub warn           { shift->_elem('_warn',           @_); }


#==========================================================================

sub warning {
    my $self = shift;
    CORE::warn("HTML::Parse: $_[0]\n") if $self->{'_warn'};
     # should maybe say HTML::TreeBuilder instead
}

#==========================================================================

sub start {
    my($self, $tag, $attr) = @_;

    my $pos  = $self->{'_pos'};
    $pos = $self unless defined $pos;

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
    
    my $ptag = $pos->{'_tag'};
    #print $indent, "POS: $pos ($ptag)\n" if $Debug > 2;
    
    my $e = HTML::Element->new($tag, %$attr); # in that class, not this class!
    
    
    # Some prep -- custom messiness for those damned tables...
    if($self->{'_implicit_tags'} and !$isTableElement{$tag}) {
      if ($ptag eq 'table') {
        print $indent,
          " * Phrasal \U$tag\E right under TABLE makes an implicit TR and TD\n"
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
    
    # And now, get busy...
    #----------------------------------------------------------------------
    if (!$self->{'_implicit_tags'}) {
        # do nothing
        print $indent, " * _implicit_tags is off.  doing nothing\n"
         if $Debug > 1;

    #----------------------------------------------------------------------
    } elsif ($isHeadOrBodyElement{$tag}) {
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
    } elsif ($isBodyElement{$tag}) {

        # Ensure that we are within <body>
        if ($pos->is_inside('head')) {
            print $indent,
              " * body-element \U$tag\E minimizes HEAD, makes implicit BODY.\n"
             if $Debug > 1;
            $pos = $self->{'_pos'} = $self->{'_body'}; # yes, needs updating
            $ptag = $pos->tag; # yes, needs updating
        } elsif (!$pos->is_inside('body')) {
            print $indent,
              " * body-element \U$tag\E makes implicit BODY.\n"
             if $Debug > 1;
            $pos = $self->{'_pos'} = $self->{'_body'}; # yes, needs updating
            $ptag = $pos->tag; # yes, needs updating
        }
         # else OK.

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
                       @p_closure_barriers    # used to be just 'li'!
                      );
            
        } elsif ($tag eq 'ol' or $tag eq 'ul' or $tag eq 'dl') {
            # Can't have lists inside <h#> -- in the unlikely
            #  event anyone tries to put them there!
            if (
                $ptag eq 'h1' or $ptag eq 'h2' or $ptag eq 'h3' or 
                $ptag eq 'h4' or $ptag eq 'h5' or $ptag eq 'h6'
            ) {
                $self->end($ptag);
            }
        } elsif ($tag eq 'li') { # list item
            # Get under a list tag, one way or another
            $self->end('*', keys %isList) || $self->insert_element('ul', 1);
            
        } elsif ($tag eq 'dt' || $tag eq 'dd') {
            # Get under a DL, one way or another
            $self->end('*', 'dl') || $self->insert_element('dl', 1);
            
        } elsif ($isFormElement{$tag}) {
            unless($pos->is_inside('form')) {
                print $indent,
                  " * ignoring \U$tag\E because not in a FORM.\n"
                  if $Debug > 1;
                return;
            }
            if ($tag eq 'option') {
                # return unless $ptag eq 'select';
                $self->end('option');
                $ptag = $self->pos->tag;
                unless($ptag eq 'select' or $ptag eq 'optgroup') {
                    print $indent, " * \U$tag\E makes an implicit SELECT.\n"
                       if $Debug > 1;
                    $pos = $self->insert_element('select', 1);
                    # but not a very useful select -- has no 'name' attribute!
                     # is $pos's value used after this?
                }
            }
        } elsif ($isTableElement{$tag}) {
            
            $self->end($tag, 'table');
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
        } elsif ($isPhraseMarkup{$tag}) {
            if ($self->{'_implicit_body_p_tag'} and $ptag eq 'body') {
                print
                  " * Phrasal \U$tag\E right under BODY makes an implicit P\n"
                 if $Debug > 1;
                $pos = $self->insert_element('p', 1);
                 # is $pos's value used after this?
            }
        }
        # End of implicit endings logic
        
    # End of "elsif ($isBodyElement{$tag}"
    #----------------------------------------------------------------------
    
    } elsif ($isHeadElement{$tag}) {
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
        $self->{'_pos'} = $self->{'_head'};

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
        return;

    #----------------------------------------------------------------------
    } elsif ($tag eq 'head') {
        my $head = $self->{'_head'};
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
        $self->{'_pos'} = $head;
        return;

    #----------------------------------------------------------------------
    } elsif ($tag eq 'body') {
        my $body = $self->{'_body'};
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
        $self->{'_pos'} = $body;
        return;

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

    print
      $indent, "(Attaching ", $e->{'_tag'}, " under ",
      $self->{'_pos'} ? $self->{'_pos'}{'_tag'} : $self->{'_tag'},
        # because if _pos isn't defined, it goes under self
      ")\n"
     if $Debug;
    
    # The following if-clause is to delete /some/ ignorable whitespace
    #  nodes, as we're making the tree.
    # This'd be a node we'd catch later anyway, but we might as well
    #  nip it in the bud now.
    # This doesn't catch /all/ deletable WS-nodes, so we do have to call
    #  the tightener later to catch the rest.

    if($self->{'_tighten'} and !$self->{'_ignore_text'}) {  # if tightenable
      my $par = $self->{'_pos'} || $self;
      my $sibs = $par->{'_content'};
      if(
         $sibs and @$sibs  # parent already has content
         and !ref($sibs->[-1])  # and the last one there is a text node
         and $sibs->[-1] !~ m<\S>s  # and it's all whitespace

         and (  # one of these has to be eligible...
               $canTighten{$tag}
               or
               (
                 (@$sibs == 1)
                   ? # WS is leftmost -- so parent matters
                     $canTighten{$par->{'_tag'}}
                   : # WS is after another node -- it matters
                     (ref $sibs->[-2]
                      and $canTighten{$sibs->[-2]{'_tag'}}
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
    
    $self->insert_element($e);
    
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

    return;
}

#==========================================================================

sub end {
    my($self, $tag, @stop) = @_;

    # End the specified tag, but don't move above any of the @stop tags.
    # The tag can also be a reference to an array.  Terminate the first
    # tag found.

    my $p = $self->{'_pos'};
    $p = $self unless defined($p);

    my($indent);
    if($Debug) {
      # optimization -- don't figure out depth unless we're in debug mode
      my @lineage_tags = $p->lineage_tag_names;
      $indent = '  ' x (1 + @lineage_tags);
      
      shift @stop
       if
        @stop == 1
        and !ref($stop[0])
        and substr($stop[0],0,2) eq '</'
        # HTML::Parser will call us with the original text of the end tag
        # as the 2nd argument.  We shouldn't (mis)interpret it as
        # a "no higher than" limitation.
      ;
      
      # now announce ourselves
      print $indent, "Ending ",
        ref($tag)
          ? ('[', join(' ', @$tag ), ']')
          : "\U$tag\E",
        scalar(@stop)
          ? (" no higher than [",
             join(' ', @stop) ,
             "]"
            )
          : (),
        ".\n"
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
    
    if($tag eq '*') {
      # Special -- close everything up to the first limiting tag, or return
      #  if none found.  Somewhat of a special case.
      # (Yes, I know it shares so little code with the rest of the
      #  code in this method that it's almost pointless having it here.
      #  But at least it's semantically related.)

      my $ptag; # no point in reallocating in every loop
     PARENT:
      while (defined $p) {
        $ptag = $p->{'_tag'};
        print $indent, " (Looking at $ptag.)\n" if $Debug > 2;
        for (@stop) {
          if($ptag eq $_) {
            print
              $indent,
              " (Hit a $_; closing everything up to here.)\n"
             if $Debug > 2;
             
             # And return now -- we have to opt out of the normal returner
             #  code later on...
             # Move position, since the specified tag was found
             $self->{'_pos'} = $p;
             print $indent, "(Pos now points at ",
               $p ? $p->{'_tag'} : '???', ".)\n"
              if $Debug > 1;
             return $p;
          }
        }
        $p = $p->{'_parent'}; # no match so far? keep moving up
        print
          $indent, 
          " (Movin on up to ", $p ? $p->{'_tag'} : 'nil', ")\n"
         if $Debug > 1
        ;
      }
      return undef; # went off the top of the tree -- fail
    }
    
    # Otherwise...
    if (ref $tag) { # list of potential tags to close
        my $ptag; # no point in reallocating in every loop
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
                    return;
                }
            }
            $p = $p->{'_parent'};
        }
    } else { # a single tag to close
        my $ptag; # no point in reallocating in every loop
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
                    return;
                }
            }
            $p = $p->{'_parent'};
        }
    }
    
    # Move position if the specified tag was found
    if(defined $p) {
      $self->{'_pos'} = $p->{'_parent'};
      print $indent, "(Pos now points to ",
        $p->{'_parent'} ? $p->{'_parent'}{'_tag'} : '???', ".)\n"
       if $Debug > 1;
    }
    
    return $p;
}

#==========================================================================

sub text {
    my($self, $text, $is_cdata) = @_;
      # the >3.0 versions of Parser may pass a cdata node.
      # Thanks to Gisle Aas for pointing this out.
    
    return unless length $text; # I guess that's always right
    
    my $ignore_text = $self->{'_ignore_text'};
    
    my $pos = $self->{'_pos'};
    $pos = $self unless defined($pos);
    
    HTML::Entities::decode($text) unless $ignore_text || $is_cdata;
    
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
    
    if ($pos->is_inside('pre', 'xmp', 'listing', 'plaintext')) {
        return if $ignore_text;
        $pos->push_content($text);
    } else {
        # return unless $text =~ /\S/;  # This is sometimes wrong
        
        my $ptag = $pos->{'_tag'};
        if (!$self->{'_implicit_tags'} || $text !~ /\S/) {
            # don't change anything
        } elsif ($ptag eq 'head') {
            if($self->{'_implicit_body_p_tag'}) {
              print $indent,
                " * Text node under HEAD closes HEAD, implicates BODY and P.\n"
               if $Debug > 1;
              $self->end('head');
              $pos =
                $self->{'_body'}
                ? ($self->{'_pos'} = $self->{'_body'}) # expected case
                : $self->insert_element('body', 1);
              $pos = $self->insert_element('p', 1);
            } else {
              print $indent,
                " * Text node under HEAD closes, implicates BODY.\n"
               if $Debug > 1;
              $self->end('head');
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
#HTML::Parser's internal buffers is flushed.

sub eof {
  my $x = $_[0];
  print "EOF received.\n" if $Debug;
  my $rv = $x->SUPER::eof(); # assumes a scalar return value
  $x->tighten_up()  # this's why we trap this -- an after-method
   if $x->{'_tighten'} and ! $x->{'_ignore_text'};
  return $rv;
}

#==========================================================================

sub delete {
  # Override Element's delete method.
  # This does most, if not all, of what Element's delete does anyway.
  # Deletes content, including content in some special attributes.
  # But doesn't empty out the hash.

  for (@{ delete($_[0]->{'_content'})
          || []
        }, # all/any content
       delete @{$_[0]}{'_body', '_head', '_pos'}
         # ...and these, in case these elements don't appear in the
         #   content, which is possible.  If they did appear (as they
         #   usually do), then calling $_->delete on them again is harmless.
      )
  {
    $_->delete
     if defined $_ and ref $_   #  Make sure it's an object.
        and $_ ne $_[0];   #  And avoid hitting myself, just in case!
  }

  return undef;
}

#==========================================================================

sub tighten_up { # delete text nodes that are "ignorable whitespace"
  
  # This doesn't delete all sorts of whitespace that won't actually
  #  be used in rendering, tho -- that's up to the rendering application.
  # For example:
  #   <input type='text' name='foo'>
  #     [some whitespace]
  #   <input type='text' name='bar'>
  # The WS between the two elements /will/ get used by the renderer.
  # But here:
  #   <input type='hidden' name='foo' value='1'>
  #     [some whitespace]
  #   <input type='text' name='bar' value='2'>
  # the WS between them won't be rendered in any way, presumably.

  my $tree = $_[0];
  my @delenda;
  print "About to tighten up...\n" if $Debug > 2;
  
  {
  my($i, $sibs); # scratch for the lambda...
  $tree->traverse( # define a big lambda...
    sub {
      
      # For starters, we can apply only to text nodes in pre-order...
      return 0 unless $_[1]; # work in pre-order...
      if(ref $_[0]) {
        if(
              # A few special nodes to NEVER clean up under...
              $_[0]{'_tag'} eq 'pre'
           or $_[0]{'_tag'} eq 'xmp'
           or $_[0]{'_tag'} eq 'textarea'
           or $_[0]{'_tag'} eq 'plaintext'
        ) {
          # block the traversal under those
          print "Aborting tightener's traversal under $_[0]{'_tag'}\n"
           if $Debug;
          return 0;
        } else {
          return 1; # keep traversing
        }
      }
      
      return unless $_[0] =~ m<^\s+$>s; # it's /all/ whitespace
      
      print "At $_[0] at depth $_[2] under $_[3]{'_tag'} whose canTighten ",
          "value is ", 0 + $canTighten{$_[3]{'_tag'}}, ".\n" if $Debug > 3;
      $sibs = $_[3]{'_content'}; # my sibling list
      $i = $_[4]; # my index in my sibling list
      
      # It's all whitespace...
      
      if($i == 0) {
        if(@$sibs == 1) { # I'm an only child
          return unless $canTighten{$_[3]{'_tag'}}; # parent
        } else { # I'm leftmost of many
          # if either my parent or sister are eligible, I'm good.
          return unless
             $canTighten{$_[3]{'_tag'}} # parent
             or
              (ref $sibs->[1]
               and $canTighten{$sibs->[1]{'_tag'}} # right sister
              )
          ;
        }
      } elsif ($i == $#$sibs) { # I'm rightmost of many
        # if either my parent or sister are eligible, I'm good.
        return unless
           $canTighten{$_[3]{'_tag'}} # parent
           or
            (ref $sibs->[$i - 1]
             and $canTighten{$sibs->[$i - 1]{'_tag'}} # left sister
            )
      } else { # I'm the piggy in the middle
        # My parent doesn't matter -- it all depends on my sisters
        return
          unless
            ref $sibs->[$i - 1] or ref $sibs->[$i + 1];
        # if NEITHER sister is a node, quit
        
        return if
          # bailout condition: if BOTH are INeligible nodes
          #  (as opposed to being text, or being eligible nodes)
            ref $sibs->[$i - 1]
            and ref $sibs->[$i + 1]
            and !$canTighten{$sibs->[$i - 1]{'_tag'}} # left sister
            and !$canTighten{$sibs->[$i + 1]{'_tag'}} # right sister
        ;
      }
      # Unknown tags aren't in canTighten and so AREN'T subject to tightening
      
      # DELENDUM!
      print "  delendum: child $i of ", $_[3]{'_tag'}, " at depth $_[1]\n"
       if $Debug > 3;
      push @delenda, [$sibs, $i];
      
      return;
    }, # End of the big traverser callback sub
    0, # Don't ignore text nodes.
  );
  }
  
  # Now do things with delenda, in REVERSE of PRE-order!
  foreach my $d (reverse @delenda) {
    ## Sanity checking:
    # die "WHAAAT!?  it's a ref now?!"
    #  if ref $d->[0][ $d->[1] ]; # sanity
    # die "WHAAAT!?  it's not whitespace anymore?!"
    #  if $d->[0][ $d->[1] ] =~ m<\S>; # sanity
    splice @{$d->[0]},
           $d->[1],
           1; # slice it out now
  }
  print scalar(@delenda), " ignorable whitespace nodes deleted.\n"
    if $Debug > 2;

  return;
}

#--------------------------------------------------------------------------
1;

