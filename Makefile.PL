require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-08-26 15:56:03 MDT"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '3.03',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
	                 'HTML::Tagset' => 3.01,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
