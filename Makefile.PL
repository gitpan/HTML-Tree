require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-03-08 00:53:31 MST"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '0.63',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
