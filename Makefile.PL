require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "1999-12-21 22:07:53 MST"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '0.62',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
