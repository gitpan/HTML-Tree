require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-11-03 16:29:32 MST"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '3.08',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
	                 'HTML::Tagset' => 3.02,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
