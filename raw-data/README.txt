This directory contains data from a variety of sources. The structure of the
sub-directories is:

	    /publisher/topic/vintage

Example: decc/regional/2012-10 contains regional energy and emissions data
published by DECC and republished here on 2012-10.

Within each such directory there are typically three subdirectories:

raw/   	    Contains the raw data as downloaded from the publisher, typically an Excel
	    file. 

modified/   Contains restructured versions of the raw data to make it easier to
	    work with. Modifications typically include recoding of nonstandard
	    fields and reorganising the data to "flat table" format. The format
	    is usually that of the originally download.

csv/	    Contains the data as comma-separated-value text files, suitable for
	    loading into a statistical package such as R. 

R/	    R code for creating .Rdata files, and the .Rdata files themselves.

