require 5.004;
use ExtUtils::MakeMaker;
# Time-stamp: "1999-12-15 19:14:31 MST"

WriteMakefile(
    'NAME'	   => 'HTML-Tree',
    'VERSION'      => '0.61',
    'PREREQ_PM'    => {	
	                 'HTML::Parser' => 2.19,
		         # 'Font::AFM'    => 1.17,
		      },
    dist           => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
