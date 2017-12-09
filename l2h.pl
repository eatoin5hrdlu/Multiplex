#!C:/Perl/bin/perl -w
while(<>)
{
    if (/\\begin\{itemize\}/)
    {
	print "<ul>\n";
    }
    elsif (/\\item\{\}/)
    {
	if ($initem) {
	    print "</li>\n"; $initem = 0;
	}
	print "<li>\n";  $initem++;
    }
    elsif (/\\end\{itemize\}/)
    {
	if ($initem) { print "</li>\n"; $initem = 0; }
	print "</ul>\n";
    }
    elsif (/\\begin\{verbatim\}/)
    {
	print "<pre>\n";
    }
    elsif (/\\end\{verbatim\}/)
    {
	print "</pre>\n";
    }
    elsif (/\\chapter\{([^}]+)\}/)
    {
	print "<h1>$1</h1>\n";
    }
    elsif (/\\section\{([^}]+)\}/)
    {
	print "<h2>$1</h2>\n";
    }
    elsif (/\\subsection\{([^}]+)\}/)
    {
	print "<h3>$1</h3>\n";
    }
    elsif ( /\\[A-Za-z]+\{[^}]*\}/ )
    {

    } else
    {
        print $_;
    }
}
