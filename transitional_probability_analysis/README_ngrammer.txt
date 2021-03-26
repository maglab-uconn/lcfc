This is a simple and poorly documented perl script to get unigram and
ngram probabilities from a lexicon file.

At the command line, just enter:

./ngrammer_simpler.pl < fkwp_pro_simple.csv > tp_out.txt

It reads in the .csv file and outputs that text file. The CSV file is
orthographic forms, frequencies, and phonological forms from Francis &
Kucera.

The output is tab-separated values by default. Turn on CSV output like
this:

./ngrammer_simpler.pl -csv < fkwp_pro_simple.csv > tp_out.csv

Let's look at the first 4 lines of TSV output:

> head -4 tp_out.txt
COUNT	1	A	70270	0.02787200
COUNT	1	C	16781	0.00665604
COUNT	1	D	76186	0.03021853
COUNT	1	E	71931	0.02853082

So /A/ (the vowel in "at") has a frequency-weighted occurrence of
70,270, which makes it account for nearly 3% of all frequency-weighted
phoneme occurrences in this lexicon.

If you just want to see all the COUNT lines, you could do this:

> grep ^COUNT tp_out.txt

Let's look at the last 4 lines, with this command:

> grep ^COUNT tp_out.txt | tail -4

COUNT	12	yumAnItErixn	3	0.00056211
COUNT	12	yumIlietIGli	1	0.00018737
COUNT	12	yunIdIrEkSxn	1	0.00018737
COUNT	12	zoIlEktrIsIt	1	0.00018737

Since cmax is set to 12, it goes out to finding 12-grams. There are
902 that occur in this lexicon. The examples you see here also suggest
caution about the F&K pronunciations. The 4 patterns here are
"humanitarian", "humiliatingly", "unidirection", and the last one is
part of the very arcane and rare word, "piezoelectricity:
(/pYizoIlEktrIsIti/). So in the dialect of the pronunciations, the /h/
at the beginings of words like "human" is not pronounced -- they start
with /y/. This should have very little impact overall.

The COUNT lines give us the relative frequencies of each 1-gram, then
each 2-gram, etc. There are minima and maxima specified in the perl
script (1 and 12), which you could change with flags like this:

./ngrammer_simpler.pl -cmin=2 -cmax=8 < fkwp_pro_simple.csv > tp_out.txt

The non-count lines are COND (conditional probability) lines. In this
simplified version, the script steps through each word starting from
the second phoneme and calculates the probability of that phoneme
given all preceding phonemes in the word. Here is one lines from
partway through the file. This refers to the probability of /t/
following /dIs/ (as in DISTinguish). The 4 indicates this is about a
4-gram, specifically, t AFTER dIs. The next number is how many
(frequency-weighted) times the target (t) occurs (98643) and the one
after that is the frequency-weighted number of times the context (dIs)
occurs (1118). BACKPR is the backwards probability of dIs occurring
before t (~1% of the time that /t/ occurs, it is proceeded by /dIs/),
while FORPR is the forward transitional probability (~30% of the time
that dIs occurs, t will follow).

COND	4	t	AFTER	dIs	98643	1118	BACKPR	0.01133380	FORPR	0.29647308


Finding all the relevant lines for a word is pretty 
tricky. Therefore, we've created a second helper script that will do
this for you. You give it a list of patterns (-pats, comma separated) 
and the tp_out.txt file you generated with n_grammer_simpler.pl,
and it pulls out the relevant lines. Here's an example where I specify
the patterns for DISTINGUISH and CAT:

> ./process_ngrammed.pl -pats=dIstIGgwIS,kAt < tp_out.txt
S	AFTER	I	FRWRD	0.03007531	BKWRD	0.21438008
S	AFTER	wI	FRWRD	0.09253731	BKWRD	0.01265771
S	AFTER	gwI	FRWRD	0.31228070	BKWRD	0.00432585
S	AFTER	GgwI	FRWRD	0.31228070	BKWRD	0.00516241
S	AFTER	IGgwI	FRWRD	0.66379310	BKWRD	0.00569022
S	AFTER	tIGgwI	FRWRD	1.00000000	BKWRD	0.00827157
S	AFTER	stIGgwI	FRWRD	1.00000000	BKWRD	0.01302215
S	AFTER	IstIGgwI	FRWRD	1.00000000	BKWRD	0.02482622
S	AFTER	dIstIGgwI	FRWRD	1.00000000	BKWRD	0.04626774
t	AFTER	A	FRWRD	0.04458744	BKWRD	0.02017911
t	AFTER	kA	FRWRD	0.08547009	BKWRD	0.00215385

This script takes the patterns, and starts with the final phoneme
conditioned on the immediately preceding phoneme, then the preceding 2
phonemes, etc. As such, it is specially tailored to the probabilities
in this paper. But if you wanted other details, you could use the
script as a template to find them.

-- Jim Magnuson, 2021.03.26
