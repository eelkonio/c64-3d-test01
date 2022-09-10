#!perl

use Math::Trig;

my $newline=1;
my $ytablelow="ytablelow";
my $ytablehigh="ytablehigh";
for (my $y=0; $y<256; $y++) {
    if ($newline) {
	$ytablelow.="\n  !byte ";
	$ytablehigh.="\n  !byte ";
	$newline=0;
    }
    if ($y>=200) {
	$ytablelow.="255";
	$ytablehigh.="255";
    } else {
	my $yval=(int($y/8)*320);
	$yval+=($y % 8);
	$ytablelow.=($yval&255);
	$ytablehigh.=(int($yval/256));
    }
    
    if ($y>0 && $y%16 == 0) {
	$newline=1;
    } else {
	$newline=0;
	if ($y<256-1) {
	    $ytablelow.=", ";
	    $ytablehigh.=", ";
	}
    }
}
print "$ytablelow\n$ytablehigh\n";



$newline=1;
print "xtable";
for (my $x=0; $x<256; $x++) {
    if ($newline) {
	print "\n  !byte ";
	$newline=0;
    }
    my $xval=($x & 248);
    print $xval;

    if ($x>0 && $x%16 == 0) {
	$newline=1;
    } else {
	$newline=0;
	if ($x<256-1) {
	    print ", ";
	}
    }
}
print "\n";


$newline=1;
print "mask";
for (my $x=0; $x<256; $x++) {
    if ($newline) {
	print "\n  !byte ";
	$newline=0;
    }
    my $xval=2 ** (7 - ($x % 8));
    print $xval;

    if ($x>0 && $x%16 == 0) {
	$newline=1;
    } else {
	$newline=0;
	if ($x<256-1) {
	    print ", ";
	}
    }
}
print "\n";



$newline=1;
print "sin";
for (my $x=0; $x<256; $x++) {
    if ($newline) {
	print "\n  !byte ";
	$newline=0;
    }
    my $xval=int(sin(($x/256)*2*pi())*99+100);
    print $xval;

    if ($x>0 && $x%16 == 0) {
	$newline=1;
    } else {
	$newline=0;
	if ($x<256-1) {
	    print ", ";
	}
    }
}
print "\n";




