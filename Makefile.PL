require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-03-25 23:24:12 MST"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '0.65',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
