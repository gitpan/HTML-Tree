require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "2000-10-02 15:12:47 MDT"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '3.05',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,   # at LEAST!
	                 'HTML::Tagset' => 3.02,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
