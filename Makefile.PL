require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-10-15 22:14:07 MDT"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '3.06',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
	                 'HTML::Tagset' => 3.02,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
