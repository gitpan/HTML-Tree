require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2001-01-20 12:43:06 MST"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '3.08',
    'VERSION_FROM' => 'lib/HTML/Tree.pm', # finds $VERSION
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
	                 'HTML::Tagset' => 3.02,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
