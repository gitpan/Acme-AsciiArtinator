package Acme::AsciiArtinator;
use Carp;
use base 'Exporter';
use strict;
use warnings;
our $VERSION = '0.01';
our @EXPORT = qw(asciiartinate);

#############################################################################

#
# run ASCII Artinization on a picture and a code string.
#
sub asciiartinate {
  my %opts = @_;
  if (@_ == 1 && ref $_[0] eq "HASH") {
    %opts = @{$_[0]};
  }

  my ($PIC, $CODE, $OUTPUT);

  if (defined $opts{"art_file"}) {
    my $fh;
    local $/ = undef;
    open($fh, "<", $opts{"art_file"}) || croak "Invalid  art_file  specification: $!\n";
    $PIC = <$fh>;
    close $fh;
  } elsif (defined $opts{"art_string"}) {
    $PIC = $opts{"art_string"};
  } elsif (defined $opts{"art"}) {
    $PIC = $opts{"art"};
  } else {
    croak "Invalid spec. Must specify  art, art_file, or  art_string \n";
  }
  $Acme::AsciiArtinator::PIC = $PIC;

  if (defined $opts{"code_file"}) {
    my $fh;
    local $/ = undef;
    open($fh, "<", $opts{"code_file"}) || croak "Invalid  code_file  specification: $!\n";
    $CODE = <$fh>;
    close $fh;
  } elsif ($opts{"code_string"}) {
    $CODE = $opts{"code_string"};
  } elsif ($opts{"code"}) {
    $CODE = $opts{"code"};
  } else {
    croak "Invalid spec. Must specify  code, code_file,  or  code_string \n";
  }

  if (defined $opts{"output"}) {
    $OUTPUT = $opts{"output"};
  } else {
    print STDERR "Output will go to \"ascii-art.pl\"\n";
    $OUTPUT = "ascii-art.pl";
  }

  if (defined $opts{"compile-check"}) {
    my $fh;
    open($fh, ">", "ascii-art.$$.pl");
    print $fh $CODE;
    close $fh;

    my $c1 = &compile_check("ascii-art.$$.pl");
    unlink "ascii-art.$$.pl";
    if ($c1 > 0) {
      croak "Initial code in ",$opts{"code"},$opts{"code_string"},
	    $opts{"code_file"}," does not compile!\n";
    }
  }

  my $max_tries = $opts{"retry"} || 100;


  my @tokens = &asciiindex_code($CODE);
  my @contexts = @asciiartinate::contexts;
  my @blocks = &asciiindex_art($PIC);

  my $ipad;
  for ($ipad = 0; $ipad < $max_tries; $ipad++) {
    print "\n\n\n\nPad try # $ipad\n\n\n\n"; 

    my ($newt,$newc) = &pad(\@tokens, \@contexts, \@blocks);
    if (defined $newc) {
      print "Code with filler: @$newt\n";
      @tokens = @$newt;

      print_code_to_pic($PIC, @tokens);
      my $fh;
      open($fh, ">", $OUTPUT);
      select $fh;
      print_code_to_pic($PIC, @tokens);
      select STDOUT;
      close $fh;

      my $c1 = &compile_check($OUTPUT);
      if ($c1 > 0) {
	croak "Artinated code does not compile! Darn.\n";
      }
      exit $c1 >> 8 if $c1;

      open($fh,"<", $OUTPUT);
      my @output = <$fh>;
      close $fh;
      return @output;
    }
  }

  if ($ipad >= $max_tries) {
    croak "The ASCII Artinator was unable to embed your code in the picture ",
      "after $max_tries tries.\n";
  }
}


sub compile_check {
  my ($file) = @_;
  print "\n";
  print "-----------------------------------\n";
  print "Compile check for $file:\n";
  print "-----------------------------------\n";
  print `$^X -cw "$file"`;
  print "-----------------------------------\n";
  return $?;
}

#############################################################################
#
# Perl code tokenization 
#
#

# You know that this [decompiling Perl code] is impossible, right ?
# http://www.perlmonks.org/index.pl?node_id=44722

my @token_keywords = qw(&&= ||= <<= >>= <=> ... **= //=
   && || ++ -- == != <= >= -> ** =~ !~ 
   <= >= => .. += -= *= /= %= |= &= ^= << >>
   .= <> // );    

# //= is an operator in perl 5.10, I believe
# //  is usually a regular expression, or a perl 5.10 operator

my %sigil = qw($ 1 @ 2 % 3 & 4 & 0);


#
# does the current string begin with an "operator keyword"?
# if so, return it
#
sub find_token_keyword {
  my ($q) = @_;
  foreach my $k (@token_keywords) {
    if (substr($q,0,length($k)) eq $k) {
      return $k;
    }
  }
  return;
}

#
# find position of a scalar in an array.
#
sub STRPOS {
  my ($word, @array) = @_;
  my $pos = -1;
  for (my $i=0; $i<@array; $i++) {
    $pos = $i if $array[$i] =~ /$word/;
  }
  return $pos;
}

#
# this is a hard game to play.
# see http://www.perlmonks.org/index.pl?node_id=44722
#
sub regex_or_divide {
  my ($tokenref, $contextref) = @_;
  my @tokens = @$tokenref;
  my @contexts = @$contextref;

  # regex is expected following an operator,
  #       at the beginning of a statement
  # divide is expected following a scalar,
  #       or any token that could complete an expression

  my $c = $#contexts;
  $c-- while $contexts[$c] eq "whitespace";
  return "regex" if $contexts[$c] eq "operator";
  return "regex" if $tokens[$c] eq ";" && $tokens[$c-1] ne "SIGIL";

  return "divide";
}

sub tokenize_code {
  my ($INPUT) = @_;
  local $" = '';
  my @INPUT = grep { /[^\n]/ } split //, $INPUT;

  # tokens are:
  #   quotes strings
  #   numeric literals
  #   regular expression specifications
  #       except with //x and s///x
  #   alphanumeric strings
  #   punctuation strings from @token_keywords
  #

  my ($i, $j, $Q, @tokens, $token, $sigil, @contexts, @blocks);

  $sigil = 0;
  for ($i = 0; $i < @INPUT; $i++) {
    $_ = $INPUT[$i];
    $Q = "@INPUT[$i..$#INPUT]";

    print STDERR "\$Q = ", substr($Q,0,8), "... SIGIL=$sigil\n" if $_ eq "q";

    # $#  could be "the output format of printed numbers"
    # or it could be the start of an expression like  $#X  or  $#{@$X}
    # in the latter case we need $# + one more token to be contiguous
    if ($Q =~ /^\$\#\{/ || $Q =~ /^\$\#\w+/) {
      $token = $&;
      push @tokens, $token;
      push @contexts, "\$# operator";
      $i = $i - 1 + length $token;
      $sigil = 0;
      next;
    }


    if ($sigil{$_} && $Q !~ /^\$\#/) {
      $sigil = $sigil{$_};
      push @tokens, $_;
      push @contexts, "SIGIL";
      next;
    }

    if (!$sigil && ($_ eq "'" || $_ eq '"' ||
		    $_ eq "/" && regex_or_divide(\@tokens,\@contexts) eq "regex")) {
      # walk through @INPUT looking for the end of the string
      # manage a boolean $escaped variable handy to allow
      # escaped strings inside strings.

      my $escaped = 0;
      my $terminator = $_;
      for($j = $i + 1; $j <= $#INPUT; $j++) {
	if ($INPUT[$j] eq "\\") {
	  $escaped = !$escaped;
	  next;
	}
	last if $INPUT[$j] eq $terminator && !$escaped;
	$escaped = 0;
      }
      my $token = "@INPUT[$i..$j]";

      if ($_ eq "/" && (length $token > 30 || $j >= $#INPUT)) {
	# this regex is pretty long. Maybe we made a mistake.
	my $toke2 = find_token_keyword($Q) || "/";
	$token = $toke2;
	$_ = "/!";
      }


      push @tokens, $token;
      if ($_ eq "/!") {
	push @contexts, "misanalyzed regex or operator";
      } elsif ($_ eq "/") {
	push @contexts, "regular expression C";
      } else {
	push @contexts, "quoted string";
      }
      $i = $j;

    } elsif (!$sigil && $Q =~ /^[-+]?[0-9]*\.{0,1}[0-9]+([eE][-+]?[0-9]+)?/) {

      # if first char starts a numeric literal, include all characters
      # from the number in the token

      

      $token = $&;
      push @tokens, $token;
      push @contexts, "numeric literal A";
      $i = $i - 1 + length $token;

    } elsif (!$sigil && $Q =~ /^[-+]?[0-9]+\.{0,1}[0-9]*([eE][-+]?[0-9]+)?/) {

      $token = $&;
      push @tokens, $token;
      push @contexts, "numeric literal B";
      $i += length $token;

    } elsif (!$sigil && ($Q =~ /^m\W/ || $Q =~ /^qr\W/ || $Q =~ /^q[^\w\s]/ || $Q =~ /^qq\W/)) {
      $j = $Q =~ /^q[rq]\W/ ? $i + 3 : $i + 2;

      my $terminator = $INPUT[$j - 1];
      $terminator =~ tr!{}<>[]{}()!}{><][}{)(!;


      my $escaped = 0;
      for(; $j <= $#INPUT; $j++) {
	if ($INPUT[$j] eq "\\") {
	  $escaped = !$escaped;
	  next;
	}
	last if $INPUT[$j] eq $terminator && !$escaped;
	# XXX - if regex has 'x' modifier,
	# then 
	$escaped = 0;
      }
      push @tokens, "@INPUT[$i..$j]";
      push @contexts, "regular expression A";
      $i = $j;

    } elsif (!$sigil && ($Q =~ /^s\W/ || $Q =~ /^y\W/ || $Q =~ /^tr\W/)) {
      $j = $_ eq "t" ? $i + 3 : $i + 2;
      my $terminator = $INPUT[$j-1];
      $terminator =~ tr!{}<>[]{}()!}{><][}{)(!;
      my $escaped = 0;
      my $terminators_found = 0;
      for (; $j <= $#INPUT; $j++) {
	if ($INPUT[$j] eq "\\") {
	  $escaped = !$escaped;
	  next;
	}
	if ($INPUT[$j] eq $terminator && !$escaped) {
	  if ($terminators_found++) {
	    last;
	  }
	}
	$escaped = 0;
      }
      push @tokens, "@INPUT[$i..$j]";
      push @contexts, "regular expression B";
      $i = $j;

    } elsif ($Q =~ /^[a-zA-Z_]\w*/) {


      $token = $&;

      # "T"x90 should be ["T",x,90] not ["T",x90]
      #  x90 should be x,90 when previous token is a scalar
      if ($token =~ /^x\d+$/) {
	if ($tokens[-1] =~ /^[\'\"]/ || $tokens[-1] eq ")") {
	  $token = "x";
	}
      }

      push @tokens, $token;
      if ($sigil) {
	push @contexts, "name";
      } elsif ($contexts[-1] =~ /regular expression/) {
	push @contexts, "regular expression modifier";
	# XXX if $token =~ /x/ then the previous token need not be contiguous ...
      } else {
	push @contexts, "alphanumeric literal";   # bareword? name? label? keyword?
      }
      $i = $i -1 + length $token;

    } elsif (($token = find_token_keyword($Q)) && !$sigil) {

      push @tokens, $token;
      push @contexts, "operator";
      $i = $i - 1 + length $token;

    } else {

      push @tokens, $_;

      if ($sigil) {
	push @contexts, "name";
      } elsif (/\s/) {
	push @contexts, "whitespace";
      } elsif (/;/ && !$sigil) {
	push @contexts, "end of statement";
      } elsif (/\//) {
	push @contexts, "operator or misanalyzed regex";
      } elsif (/[\+\-\*\/\%\^\|\&\!\~\?\:\.]/) {
	push @contexts, "operator";

      } elsif (/\{/ && $sigil) {
	push @contexts, "name container";
      } elsif (/\}/ && STRPOS("name contained",@contexts) > STRPOS("name decontainer",@contexts)) {
	push @contexts, "name decontainer";

      } else {
	push @contexts, "unknown";
      }
    }

    $sigil = 0;
  }

  print "----------------------------\n";
  my @c = @contexts;
  foreach $token (@tokens) {
    my $cc = shift @c;
    print $token,"\t",$cc,"\n";
  }
  print "----------------------------\n";
  print "Total token count: ", scalar @tokens, "\n";

  @asciiartinate::contexts = @contexts;
  @asciiartinate::tokens = @tokens;

  @tokens;
}

sub asciiindex_code {
  my ($X) = @_;
  my $endpos = index($X,"\n__END__\n");
  if ($endpos >= 0) {
    substr($X,$endpos) = "\n";
  }
  $X =~ s/\n\s*#[^\n]*\n/\n/g;
  $X =~ s/\n\s*#[^\n]*\n/\n/g;
  &tokenize_code($X);
}

#############################################################################

sub tokenize_art {
  my ($INPUT) = @_;
  my @INPUT = split //, $INPUT;

  my $white = 1;
  my $block_size = 0;
  my @tokens = ();
  foreach my $char (@INPUT) {
    if ($char eq " " || $char eq "\n" || $char eq "\t") {
      if ($block_size > 0) {
	push @tokens, $block_size;
	$block_size = 0;
      }
    } else {
      ++$block_size;
    }
  }
  if ($block_size > 0) {
    push @tokens, $block_size;
  }
  return @tokens;
}

sub asciiindex_art {
  my ($X) = @_;
  &tokenize_art($X);
}

sub print_code_to_pic {
  my ($pic, @tokens) = @_;
  local $" = '';
  my $code = "@tokens";
  my @code = split //, $code;

  $pic =~ s/(\S)/@code==0?"#":shift @code/ge;

  print $pic;
}



sub padding_needed {
  my @tokens = @{$_[0]};
  my @blocks = @{$_[1]};
  my $ib = 0;
  my $tc = 0;
  my $bc = $blocks[$ib++];
  foreach my $t (@tokens) {
    my $tt = length $t;
    if ($tt > $bc) {
      print "\rNeed to pad by ",$bc," spaces at or before position $tc            ";
      return [$tc, $bc];
    }
    $bc -= $tt;
    if ($bc == 0) {
      $bc = $blocks[$ib++];
      if ($ib > @blocks) {
	print "Error: picture is not large enough to contain code!\n";

	print map {(" ",length $_)} @tokens;
	print "\n\n@blocks\n";

	return [-1,-1];
	$bc = 100;
      }
    }
    $tc += length $t;
  }
  return;
}

sub hi_weighted_rand {
  my $n = shift;
  my (@p, $r, $p);
  for ($r = 1; $r <= $n; $r++) {
    push @p, $p += $r * $r * $r;
  }
  $p = int(rand() * $p);
  for ($r = 1; $r <= @p; $r++) {
    return $r if $p[$r-1] >= $p;
  }
  return $n;
}

sub try_to_pad {
  my ($pos, $npad, $tref, $cref) = @_;

    #     padding techniques:
    #         SIGIL name --->   SIGIL { name }
    #         XXX       --->    ( XXX )
    #              for XXX in (numeric literal,quoted string)
    #         XXX ;     --->    XXX ;;  
    #              for XXX in (quoted string,numeric literal,regular expression,
    #                          <> operator, ")"
    #         }         --->   ; }  for } that ends a code BLOCK
    #         ; }       --->   ; ; }
    #         inserting strings in void context after semi-colons (for howmuch > 2)
    #         = expr    --->   = 0|| expr  (if expr does not have ops with lower prec than ||)
    #         = expr    --->   = 1&& expr  (if expr does not have ops with lower prec than &&)
    #         = expr    --->   = 0 or expr , = 0 xor expr

  my $t = 0;
  my $it = 0;
  for ($it = 0; $it < @$tref; $it++) {
    last if $t + length $tref->[$it] > $pos;
    $t += length $tref->[$it];
  }

#  print STDERR "Trying to pad at ", join " :: ", @{$tref}[$it-1 .. $it+1], "\n";
#  print STDERR "Contexts: ", join " :: ", @{$cref}[$it-1 .. $it+1], "\n\n";

  my $z = rand() * 0.5;
  $z = 0.45 if $it == 0;
  if ($z < 0.45 && $npad > 1) {

    # convert  SIGIL name  -->  SIGIL { name }

    if ($cref->[$it] eq "name" && $cref->[$it-1] eq "SIGIL") {
      # print STDERR "Padding name $tref->[$it] at pos $t\n";

      splice @$tref, $it+1, 0, "}";
      splice @$tref, $it, 0, "{";
      splice @$cref, $it+1, 0, "filler";
      splice @$cref, $it, 0, "filler";
    }

  } elsif ($z < 0.05) {

    # try to pad the beginning of a statement with filler

    if ($it == 0 || ($tref->[$it-1] eq ";" && $cref->[$it-1] eq "end of statement")
	|| ($tref->[$it] eq ";" && $cref->[$it] eq "end of statement")
	|| $cref->[$it] eq "flexible filler"
	|| $cref->[$it-1] eq "flexible filler") {

      # print STDERR "Padding with flexible filler x $npad at pos $t\n";
      while ($npad-- > 0) {
	splice @$tref, $it, 0, ";";
	splice @$cref, $it, 0, "flexible filler";
      }
    }
  } elsif ($z < 0.5 && $npad > 1) {

  } elsif ($z < 0.75) {

  }

}

sub pad {
  my @tokens = @{$_[0]};
  my @contexts = @{$_[1]};
  my @blocks = @{$_[2]};

  my $nblocks = 0;
  map { $nblocks += $_ } @blocks;

  my ($needed, $where, $howmuch);
  while ($needed = padding_needed(\@tokens,\@blocks)) {
    ($where,$howmuch) = @$needed;
    if ($where < 0 && $howmuch < 0) {
      return;
    }

    # select position to attempt padding.
    #     position must be prior to where
    #     choose position randomly, put random distribution
    #         should be (heavily) biased towards
    #         where.
    my $pos = $where < 5 || rand() < 0.1 
      ? hi_weighted_rand($where) : $where - 5 + hi_weighted_rand(5);
    my $npad = $howmuch > 1 ? $howmuch - hi_weighted_rand($howmuch-1) : $howmuch;

    try_to_pad($pos, $npad, \@tokens, \@contexts);

    my $tlength = 0;
    map { $tlength += length $_ } @tokens;
    if ($tlength > $nblocks) {
      print "Padded length exceeds space length.\n";

      print_code_to_pic($Acme::AsciiArtinator::PIC, @tokens);
      print "\n\n";

      return;
    }

  }
  return ([ @tokens ],[ @contexts ]);
}

#
# can run from command line:
#
#   perl Acme/AsciiArtinator.pm art-file code-file [output-file]
#
if ($0 =~ /AsciiArtinator.pm/) {
  asciiartinate( art_file => $ARGV[0] ,
	         code_file => $ARGV[1] ,
                 output => $ARGV[2] || "ascii-art.pl" );
}

1;

__END__
=head1 NAME

Acme::AsciiArtinator - Embed Perl code in ASCII artwork

=head1 VERSION

0.01

=head1 SYNOPSIS

    use Acme::AsciiArtinator;
    asciiartinate( { art_file  => "ascii.file",
                     code_file => "code.pl",
                     output    => "output.pl" } );

=head1 DESCRIPTION

Embeds Perl code (or at least gives it a good
college try) into a piece of ASCII artwork by 
replacing the non-whitespace
(we'll refer to C<non-whitespace> a lot in this
document, so let's just call it
C<darkspace> for convenience) characters of an 
ASCII file with the characters of a Perl script.
If necessary, the code is modified (padded) so
that blocks of contiguous characters (keywords,
quoted strings, alphanumeric literals, etc.)
in the code are aligned with at least the
minimum number of contiguous darkspace
characters in the artwork.

=head1 EXAMPLE

Suppose we have a file called C<spider.pl> with
the following code:

    &I();$N=<>;@o=(map{$z=${U}x($x=1+$N-$_);
    ' 'x$x.($".$F)x$_.($B.$z.$P.$z.$F).($B.$")x$_.$/}
    0..$N);@o=(@o,($U.$F)x++$N.($"x3).($B.$U)x$N.$/);
    print@o;
    sub I{($B,$F,$P,$U)=qw(\\ / | _);}
    while($_=pop@o){y'/\\'\/';@o||y#_# #;$t++||y#_ # _#;print}

What this code does is read one value from standard input
and draws a spider web of the given size:

    $ echo 5 | perl spiders.pl
          \______|______/
          /\_____|_____/\
         / /\____|____/\ \
        / / /\___|___/\ \ \
       / / / /\__|__/\ \ \ \
      / / / / /\_|_/\ \ \ \ \
    _/_/_/_/_/_/   \_\_\_\_\_\_
     \ \ \ \ \ \___/ / / / / /
      \ \ \ \ \/_|_\/ / / / /
       \ \ \ \/__|__\/ / / /
        \ \ \/___|___\/ / /
         \ \/____|____\/ /
          \/_____|_____\/
          /      |      \

Suppose we also have a file called C<spider.ascii>
that looks like:

               ;               ,
             ,;                 '.
            ;:                   :;
           ::                     ::
           ::                     ::
           ':                     :
            :.                    :
         ;' ::                   ::  '
        .'  ';                   ;'  '.
       ::    :;                 ;:    ::
       ;      :;.             ,;:     ::
       :;      :;:           ,;"      ::
       ::.      ':;  ..,.;  ;:'     ,.;:
        "'"...   '::,::::: ;:   .;.;""'
            '"""....;:::::;,;.;"""
        .:::.....'"':::::::'",...;::::;.
       ;:'.'""'"";.,;:::::;.'"""""". ':;
      ::'         ;::;:::;::..         :;
     ::.        ,;:::::::::::;:..       ::
     ;'     ,;;:;::::::::::::::;";..    ':.
    ::     ;:"  ::::::"__'::::::  ":     ::
     :.    ::   ::::::;__:::::::  ::    .;
      ;    ::   :::::::__:::::::   :    ;
       '   ::   ::::::....:::::'  ,:   '
        '  ::    :::::::::::::"   ::
           ::     ':::::::::"'    ::
           ':       """""""'      ::
            ::                   ;:
            ':;                 ;:"
              ';              ,;'
                "'           '"            

And B<now> suppose that we think it would be
pretty cool if the code that draws spider
webs on the screen actually looked like a
spider. Well, this is a job for the Acme::AsciiArtinator.

Let's code up a quick script that just says:

    use Acme::AsciiArtinator;
    asciiartinate( art_file => "spiders.ascii",
                   code_file => "spiders.pl",
                   output => "spider-art.pl" );

and run it. 

If this works (and it might not, for a variety of
reasons), we will get a new file called C<spider-art.pl>
that looks something like:

               &               I
             ()                 ;$
            N=                   <>
           ;;                     ;;
           ;;                     ;;
           ;;                     ;
            ;;                    ;
         ;; ;;                   ;;  ;
        ;;  ;;                   ;;  ;;
       ;;    ;;                 ;@    o=
       (      map             {$z     =$
       {U      }x(           $x=      1+
       $N-      $_)  ;' 'x  $x.     ($".
        $F)x$_   .($B.$z.$ P.   $z.$F).
            ($B.$")x$_.$/}0..$N);@
        o=(@o,($U.$F)x++$N.($"x3).($B.$U
       )x$N.$/);;;;print@o;;;sub I{( $B,
      $F,         $P,$U)=qw(\\          /
      |         _);;}while($_=pop       @o
     ){     y'/\\'\/';;;@o||y#_# #;;    ;;;
    ;$     t++  ||y#_ # _#;print  }#     ##
     ##    ##   ################  ##    ##
      #    ##   ################   #    #
       #   ##   ################  ##   #
        #  ##    ##############   ##
           ##     ############    ##
           ##       ########      ##
            ##                   ##
            ###                 ###
              ##              ###
                ##           ##

Hey, that was pretty cool! Let's see if it works.

    $ echo 6 | perl spider-art.pl
           \_______|_______/
           /\______|______/\
          / /\_____|_____/\ \
         / / /\____|____/\ \ \
        / / / /\___|___/\ \ \ \
       / / / / /\__|__/\ \ \ \ \
      / / / / / /\_|_/\ \ \ \ \ \
    _/_/_/_/_/_/_/   \_\_\_\_\_\_\_
     \ \ \ \ \ \ \___/ / / / / / /
      \ \ \ \ \ \/_|_\/ / / / / /
       \ \ \ \ \/__|__\/ / / / /
        \ \ \ \/___|___\/ / / /
         \ \ \/____|____\/ / /
          \ \/_____|_____\/ /
           \/______|______\/
           /       |       \

=head1 UNDER THE HOOD

To fill in the shape of the spider, we inserted whitespace,
semi-colons, sharps, and maybe the occasional C<{> C<}> pair
into the original code. Certain blocks of text, like
C<print>, C<while>, and C<y#_ # _#> are kept intact since
splitting them would cause the program to either fail to
compile or to behave differently. 

The ASCII Artinator tokenizes the code and
does its best to identify 

=over 4

=item 1. Character strings that must not be divided

These include alphanumeric literals, quoted strings,
and most regular expressions.

=item 2. Places in the code where it is OK to insert padding.

=item 3. Places in the ASCII artwork where there are multiple
consecutive darkspace characters

=back

The next step is to try to align the tokens from the code
with enough contiguous blocks of darkspace in the art. When
a token is misaligned, we attempt to align it by inserting
some padding at some point in the code before that token.

There are currently two ways that we pad the code. Each
time there is a need to pad the code, we randomly choose
a padding method and randomly choose an eligible position
for padding.

=over 4

=item 1. Inserting semi-colons at the beginning or end of a statement

In general, we can put as many semi-colons as we like at the beginning
or end of statements. The following lines of code should all do the
same thing:

    $a=$b+$c;$d=4
    $a=$b+$c;;;;;;$d=4;;;;;;
    ;;;;;;;;;$a=$b+$c;;;;;;;;$d=4;

=item 2. Putting braces around a variable name.

In general, we can replace C<$name> with C<${name}> and the code
will run the same.

=back

There are several other interesting ways to pad code (putting parentheses
around expressions, adding and or-ing zeros to expressions, using quoted
strings in a void context) that may be put to use in future versions
of this module.

When all tokens from the code are successfully aligned with the
blocks of darkspace from the artwork, we can paste the code on top
of the art and write the output file. 

Sometimes we insert too many characters without successfully
aligning the tokens and darkspace blocks (and actually in the
spider example, this happens about 90% of the time). If this
happens, we will start over and retry up to 100 times.

=head1 BEST PRACTICES

Certain coding practices will increase the chance that
C<Acme::AsciiArtinator> will be able to embed your code
in the artwork of your choice. In no particular order,
here are some suggestions:

=over 4

=item * Make sure the original code works

Make sure the code compiles and test it to see if it
works like you expect it to
before running the ASCII Artinator. It would be frustrating to
try to debug an artinated script only to later realize that
there was some bug in the original input.

=item * Get rid of comments

This module won't handle comments very well. There's no way
to stop the ASCII Artinator from splitting your comment across
two lines and breaking the code.

=item * Reduce whitespace

In addition to making the code longer and thus more difficult
to align, any whitespace in your code will be printed out as
space over a darkspace in the art and put a "hole" in your
picture. It would be nice if there was a way to align the
whitespace in the code with the whitespace in the art, but that
is probably something for a far future version.

=item * Avoid significant newlines

Newlines are stripped from the code before the code is tokenized.
If there are any significant newlines (I mean the literal 0x0a char.
It should still be OK to say  C<print"\n">), then the artinated
code will run differently.

=item * Consider workarounds for quoted strings

Quoted strings are parsed as a single token. Consider ways to break
them up so that can be split into multiple tokens. For example, instead
of saying C<$h="Hello, world!";>, we could actually say something like:

    &I;($c,$e)=qw(, !);$h=H.e.l.l.o.$c.$".W.o.r.l.d.$e;

The modified code is a lot longer, but this code can be split at any
point except in the middle of C<qw>, so it is much more flexible code
from the perspective of the Artinator.

=item * Perform some smart reordering

In the spider example, we see that the largest contiguous blocks of
darkspace are in the center of the spider, and at the beginning and
end of the spider art, there are many smaller blocks of darkspace.
In this case, code that has large tokens in the middle or near the
end of the code will be more flexible than code with large tokens in
the beginning of the code. So for example, we are better off
writing

    @o=(map ... );print@o

than

    print@o=(map ... )

even through the latter code is a little shorter.

=back

=head1 OPTIONS

The C<asciiartinate> method supports the following options:

=over 4

=item art_file => filename

=item art_string => string

=item art => string

Specifies the ASCII artwork that we'll try to embed code into.
At least one of C<art>, C<art_string>, C<art_file> must be
specified.

=item code_file => filename

=item code_string => string

=item code => string

Specifies the Perl code that we will try to embed into the
art. At least one of C<code>, C<code_string>, C<code_file>
must be specified.

=item output => filename

Specifies the output file for the embedded code. If omitted,
output is written to the file "ascii-art.pl" in the current
directory.

=item compile_check => 0 | 1

Runs the Perl interpreter with the C<-cw> flags on the
original code string and asserts that the code compiles.


=back

=head1 TODO

Lots of future enhancements are possible:

=over 4

=item * Use new ways of padding code

=back

=over 4

=item * Take big blocks of filler and fill them with something else. Random quoted strings.

=back

=over 4

=item * Try to align whitespace in the code with whitespace in the art.

=back

=over 4

=item * Read expected input(s) and output(s) to verify that the artinated code does the same
thing as the original code.

=back

=over 4

=item * Have a concept of "grayspace" in the artwork. These are positions where
we can put either whitespace or a character from the code, whichever makes it
easier to align the code.

=back

=over 4

=item * Optionally implement some "best practices" automatically to make the code
more flexible without changing its behavior.

=back

=head1 BUGS

Probably lots. 

=head1 SEE ALSO

If you liked this module, you might also get a kick out of L<Acme::EyeDrops>.

=head1 AUTHOR

Marty O'Brien, E<lt>mob@cpan.orgE<gt>

=head1 LICENSE AND COPYRIGHT

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.

=cut
