
require 5; # Time-stamp: "2001-01-20 13:39:01 MST"
package HTML::Tree;
$VERSION = $VERSION = 3.09;
  # This is where the dist gets its version from.

# Basically a happy alias to HTML::TreeBuilder
use HTML::TreeBuilder ();
sub new { shift; HTML::TreeBuilder->new(@_); }

1;  

__END__

=head1 NAME

HTML-Tree - overview of HTML::TreeBuilder et al

=head1 SYNOPSIS

  use HTML::TreeBuilder;
  my $tree = HTML::TreeBuilder->new();
  $tree->parse_file($filename);
   #
   # Then do something with the tree, using HTML::Element
   # methods -- for example $tree->dump
   #
   # Then:
  $tree->delete;

=head1 DESCRIPTION

HTML-Tree is a suite of Perl modules for making parse trees out of
HTML source.  It consists of mainly two modules, whose documentation
you should refer to: L<HTML::TreeBuilder> and L<HTML::Element>.

HTML::TreeBuilder is the module builds the parse trees.  (It uses
HTML::Parser to do the work of breaking the HTML up into tokens.)

The tree that TreeBuilder builds for you is made up of objects of the
class HTML::Element.

If you find that you do not properly understand the documentation
for HTML::TreeBuilder and HTML::Element, it may be because you are
unfamiliar with tree-shaped data structures, or with object-oriented
modules in general.  I have written some articles for I<The Perl
Journal> (C<www.tpj.com>) that seek to provide that background:
my article "Scanning HTML" in TPJ19; my article "Trees" in TPJ18, and
my article "A User's View of Object-Oriented Modules" in TPJ17.
The full text of those articles will likely appear in a later version
of this HTML-Tree module distribution.

=head1 SEE ALSO

L<HTML::TreeBuilder>, L<HTML::Element>, L<HTML::Tagset>,
L<HTML::Parser>

L<HTML::DOMbo>

=head1 COPYRIGHT

Copyright 1995-1998 Gisle Aas; copyright 1999-2001 Sean M. Burke.

The whole HTML-Tree distribution, of which this file is a part, is
free software; you can redistribute it and/or modify it under the same
terms as Perl itself.

=head1 AUTHOR

Original HTML-Tree author Gisle Aas E<lt>gisle@aas.noE<gt>; current
maintainer Sean M. Burke, E<lt>sburke@cpan.orgE<gt>

=cut

