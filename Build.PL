use 5.008;
use strict;
use warnings;
use inc::My_Build;

my $builder = inc::My_Build->new(
    module_name        => 'HTML::Tree',
    license            => 'perl',
    dist_author        => 'Christopher J. Madsen <perl@cjmweb.net>',
    dist_version_from  => 'lib/HTML/Element.pm',
  'build_requires' => {
    'Module::Build' => '0.2808',
    'Test::Fatal' => '0',
    'Test::More' => '0.88',
    'base' => '0'
  },
  'configure_requires' => {
    'Module::Build' => '0.2808'
  },
  'recommends' => {
    'HTML::FormatText' => '0',
    'LWP::UserAgent' => '5.815'
  },
  'requires' => {
    'Carp' => '0',
    'Encode' => '0',
    'Exporter' => '0',
    'HTML::Entities' => '0',
    'HTML::Parser' => '3.46',
    'HTML::Tagset' => '3.02',
    'IO::HTML' => '0',
    'Scalar::Util' => '0',
    'perl' => '5.008'
  },
    add_to_cleanup =>
        [ 'HTML-Tree-*', 'tmp', 'blib', 'lwp-test-*', 'MANIFEST.bak' ],
    script_files => 'bin',
);

$builder->create_build_script();
