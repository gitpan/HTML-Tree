require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-05-18 23:58:37 MDT"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '0.66',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
