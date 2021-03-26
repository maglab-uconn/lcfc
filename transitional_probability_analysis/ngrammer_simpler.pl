#!/usr/bin/perl -s

# specify -csv at the command oine if you want CSV instead of TSV output

########################################################
# these options can be specified at the command line,
# e.g., "./ngrammer_simpler.pl -cmin=2"

# what is the smallest phoneme context to condition on?
if($cmin eq ""){ $cmin = 1; }
# what is the largst phoneme context to condition on?
if($cmax eq ""){ $cmax = 12; }

########################################################
# get lexicon
$item = 0;
while(<>){
    chomp;
    # parse CSV file, expecting orthography, then frequency,
    # then the phono string. Anything else is @jnk
    ($ortho, $freq, $string, @jnk) = split(/\,/, $_); 
    next if($string eq ""); # skip line if no phono string

    $strng_counter[$item] = $string;
    if($freq eq ""){ $frq[$item] = 1; } else { $frq[$item] = $freq;}
    push @strings, $string;
    $item++;
}
print STDERR "Read $#strings + 1 items from STDIN\n";

########################################################
# process the data
for($order = $cmin; $order <= $cmax; $order ++){
    %occs = ();
    $sumocc = 0;
    %contexts = ();
    %targets = ();
    %trg_total = ();
    %trg_con = ();
    %con_total = ();
    for($i = 0; $i < $item; $i++){
	$s = $strng_counter[$i];
	$f = $frq[$i];
	$o = $otho[$i];
	next if(length($s) < $order);
	$finalpos = length($s) - $order;
	for($pos = 0; $pos <= $finalpos; $pos++){
	    $first = $pos; $howmany = $order;
	    $ss = substr($s, $first, $howmany);
	    #		print STDERR "$s\t$order\t$first\t\"$ss\"\n";
	    $occs{$ss} += $f;
	    $sumocc += $f;
	    if(length($ss) > 1){
		$con = substr($ss, 0, -1);
		$trg = substr($ss, -1);
		$trg_total{$trg} += $f;
		$trg_con{$trg."#".$con} += $f;
		$con_total{$con} += $f;
	    }
	}
    }
    $occount = keys %occs; # how many
    foreach $anoc (sort(keys %occs)) {
	$prob = sprintf("%.8f", $occs{$anoc} / $sumocc); 
	print "COUNT\t$order\t$anoc\t$occs{$anoc}\t$prob\n";
    }
    
    if($order > 1){
	foreach $a_tc (sort(keys %trg_con)) {
	    ($t, $c) = split(/\#/, $a_tc);
	    # backward prob: p(context | following target)
	    $back_pr = sprintf("%.8f", $trg_con{$a_tc} / $trg_total{$t});
	    # forward prob: p(following target | context)
	    $for_pr = sprintf("%.8f", $trg_con{$a_tc} / $con_total{$c});
	    if($csv){ 
		print "COND,$order,$t,AFTER,$c,$trg_total{$t},$trg_con{$a_tc},BACKPR,$back_pr,FORPR,$for_pr\n";
	    } else { 
		print "COND\t$order\t$t\tAFTER\t$c\t$trg_total{$t}\t$trg_con{$a_tc}\tBACKPR\t$back_pr\tFORPR\t$for_pr\n";
	    }
	}
    }
}

