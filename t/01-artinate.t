#!perl

use Test::More tests => 3;
use Acme::AsciiArtinator;
use strict;
use warnings;

my $art = '
XXXXXXXXXXXXXXX
 XXXXXXXXXXXXX
  XXXXXXXXXXX
   XXXXXXXXX
    XXXXXXX
     XXXXX
      XXX
       X';

my $code = '$_="rst";print"Hello",", ","world!\n" if /st/;';

my @output = asciiartinate( code => $code, art => $art);
my $output = join '', @output;

print STDERR @output;

ok($output =~ /print/);
ok($output =~ /Hello/);
ok($output =~ /;;/ || $output =~ /\{\w+\}/);

