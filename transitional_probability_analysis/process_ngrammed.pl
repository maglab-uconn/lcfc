#!/usr/bin/perl -s


@patterns = split(/\,/, $pats);



while(<>){
    chomp;
    ($typ, $ord, $targ, $after, $cont, $jnk, $jnk, $jnk, $bpr, $jnk, $fpr) = split;
    next if($typ eq "COUNT");
    $forward{$targ."#".$cont} = $fpr; 
    $backward{$targ."#".$cont} = $bpr;
}

foreach $p (@patterns){
    $target = substr($p, -1);
    for($i = length($p) - 2; $i >= 0; $i--) {
	$context = substr($p, $i, -1);
	print "$target\tAFTER\t$context\tFRWRD\t".$forward{$target."#".$context}.
	    "\tBKWRD\t".$backward{$target."#".$context}."\n";
    }
}
    
