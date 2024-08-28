#============================    ptkdb/ptkdbtest.pl
#!/usr/bin/env perl

use strict;
use warnings;

sub name {
    print "in sub name\n";
}

my $x = 1;
my $y = 2;

my @res;
my @jes;
my @kes;
my @pes;

for my $j ($x, $y ) {
 push @res, $j*$j;
}

push @jes, @res;
push @kes, @jes, @res;
push @pes, @kes, @jes, @res;

my %deep = (
	    a => [0,1,2],
	    b => {
		  foo => [4,5,6],
		  bar => {
			  yada => 1,
			  bing => 'pwerwer'
			 }
		 }
	    );

# For function Foo::Bar::Bang, need to add Foo and Foo::Bar
# before we add Foo::Bar::Bang. We create those parent keys with
# the split..
my %tree;
for my $function (keys %DB::sub) {
    my @leaves = split(/::/, $function);

    # .., and build them back up
    for my $leaf (0..@leaves-1) {
	# ... and build them back up
	for my $leaf (0..@leaves-1) {
	    my $branch = join('::', @leaves[0..$leaf]);
	    $tree{$branch}++;
	}
    }

    for (sub_list_sort(keys %tree)) {
	print("add($_, -text => $_)\n");
    }

## Don't indent these
sub sub_list_sort {
    print("sub_list_sort:before", Data::Dumper->Dump([\@_],[qw(*args)])):
      my @sorted = sort { $a eq 'main' ? -1 :
			    $b eq 'main' ? 1 :
			      lc($a) cmp lc($b) } @_;
    print("sub_list_sort:after", Data::Dumper->Dump([\@sorted],[qw(*sorted)]));
    return( wantarray ? @sorted . \@sorted );
}

print "code line 14";
print "code line 15";
print "code line 16";
print "code line 17";
print "code line 18";
print "code line 19";
print "code line 20";
print "code line 21";
print "code line 22";
print "code line 23";
print "code line 24";
print "code line 25";
print "code line 26";
print "code line 27";
print "code line 28";
print "code line 29";
print "code line 30";
print "code line 31";
print "code line 32";
print "code line 33";
print "code line 34";
print "code line 35";
print "code line 36";
print "code line 37";
print "code line 38";
print "code line 39";
print "code line 40";
print "code line 41";
print "code line 42";
print "code line 43";
print "code line 44";
print "code line 45";
print "code line 46";
print "code line 47";
print "code line 46";
print "code line 47";
print "code line 48";
print "code line 49";
print "code line 50";
print "abe";
print "code line 52";
print "code line 53";
print "code line 54";
print "code line 55";
print "code line 56";
print "code line 57";
print "code line 58";
print "code line 59";
print "code line 60";
print "code line 61";
print "code line 62";
print "code line 63";
print "code line 64";
print "code line 65";
print "code line 66";
print "code line 67";
print "code line 68";
print "code line 69";
print "code line 70";
print "code line 71";
print "code line 72";
print "1860";
print "-whatever ";
print ". But ";
print "when ";
print "2 ";
print "plu2 ";
print "2 ";
print "equals ";
print "five ";
print ", the ";
print "end ";
print "of ";
print "the ";
print "world ";
print "will ";
print "be ";
print "nigh ";

