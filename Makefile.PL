require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-06-12 02:09:02 MDT"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '0.67',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
