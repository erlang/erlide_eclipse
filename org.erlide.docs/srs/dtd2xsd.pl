#! perl
#
# by Dan Connolly http://www.w3.org/People/Connolly/ connolly@w3.org
#    Bert Bos http://www.w3.org/People/Bos/ <bert@w3.org>
#    Yuichi Koike
#    Mary Holstege (holstege@mathling.com)
# initial hack by DC Apr 2000, based on dtd2bnf by BB Mar 1998;
# major revision to Apr 2000 make it actually usable by YK;
# tweaks by DC; major update Jan 2001 by MH
#
# see Log since then at end.
# $Id: dtd2xsd.pl,v 1.1 2006/10/13 11:52:07 vladdu Exp $

use strict;

# Handling command line argument
my $targetNS = "http://www.w3.org/namespace/";
my $prefix = "t";
my $alias = 0;
my $file = "";
my %SimpleTypes;
my @AttrGroupPatterns;
my @ModelGroupPatterns;
my @SubstitutionGroupPatterns;
my %SubstitutionGroup;

my %Mixed;
my %ModelGroup;
my $mapping_file;
my $pcdata_flag = 0;
my $pcdata_simpletype = "string";
my $debug = 0;

while ($#ARGV >= 0) {
	 my $para = shift(@ARGV);
	 if ($para eq "-ns") {
		  $targetNS = shift(@ARGV);
	 } elsif ($para eq "-prefix") {
		  $prefix = shift(@ARGV);
	 } elsif ($para eq "-alias") {
		  $alias = 1;
	 } elsif ($para eq "-pcdata") {
		  # Treat #PCDATA by itself as being string (or other simple type
		  # if so designated in the mapping file)
		  $pcdata_flag = 1;
	 } elsif ($para eq "-mapfile") {
		  $mapping_file = shift(@ARGV);
	 } elsif ($para eq "-simpletype") {
		  my($pat) = shift(@ARGV);
		  my($b) = shift(@ARGV);
		  $SimpleTypes{$pat} = $b;
	 } elsif ($para eq "-attrgroup") {
		  push(@AttrGroupPatterns, shift(@ARGV));
	 } elsif ($para eq "-modelgroup") {
		  push(@ModelGroupPatterns, shift(@ARGV));
	 } elsif ($para eq "-substgroup") {
		  push(@SubstitutionGroupPatterns, shift(@ARGV));
	 } elsif ($para eq "-debug") {
		  $debug = 1;
	 } else {
		  $file = $para;
	 }
}

# Alias dictionary: defaults
my %alias_dic;
$alias_dic{"URI"} = "uriReference";
$alias_dic{"LANG"} = "language";
$alias_dic{"NUMBER"} = "nonNegativeInteger";
$alias_dic{"Date"} = "date";
$alias_dic{"Boolean"} = "boolean";

if ( $mapping_file )
{
	 print STDERR "Open mapping $mapping_file ";
	 if ( !open( MAPPINGS, "<$mapping_file" ) )
	 {
		  print STDERR "unsuccessful.\n";
	 }
	 else {
		  print STDERR "successful.\n";
		  while ( <MAPPINGS> ) {
				chop;
				if ( /^alias\s+([^ \t]+)\s*=\s*([^ \t]+)\s*/i ) {
					 $alias_dic{$1} = $2;
				}
				elsif ( /^simpletype\s+([^ \t]+)\s*=\s*([^ \t]+)\s*/i ) {
					 $SimpleTypes{$1} = $2;
				}
				elsif ( /^attrgroup\s+([^ \t]+)\s*/i ) {
					 push( @AttrGroupPatterns, $1 );
				}
				elsif ( /^modelgroup\s+([^ \t]+)\s*/i ) {
					 push( @ModelGroupPatterns, $1 );
				}
				elsif ( /^substgroup\s+([^ \t]+)\s*/i ) {
					 push( @SubstitutionGroupPatterns, $1 );
				}
				elsif ( /^pcdata\s+([^ \t]+)\s*/i ) {
					 ## BUGLET: doesn't pay attention to prefix; just a special alias
					 $pcdata_simpletype = $1;
				}
		  }
	 }

	 foreach my $key (keys(%alias_dic)) 
	 {
		  print STDERR "Alias \%$key to $alias_dic{$key}\n"
		  }
}

# Variable declaration
my $linelen = 72;

my $PROG = substr($0, rindex($0, "/") + 1);
my $USAGE = "Usage: $PROG file\n";

my $str = "(?:\"([^\"]*)\"|\'([^\']*)\')";
my %pent;				# Parameter entities
my %attributes;			# Attribute lists
my @element;			# Elements in source order
my %model;				# Content models

# Main
$/ = undef;

# Open file, remove comment and include external entity
my $buf = openFile($file);

# Alias treatment
my $alias_ident = "_alias_";
if ($alias eq 1) {
	 foreach my $key (keys(%alias_dic)) {
		  my $aliaskey = sprintf("%s%s%s", $alias_ident, $key, $alias_ident);
		  $buf =~ s/\%$key;/$aliaskey/gsie;
	 }
}


# store all parameter entities
while ($buf =~ s/<!ENTITY\s+%\s+(\S+)\s+$str\s*>//sie) {
    my($n, $repltext) = ($1, $2.$3);
    my ($pat);

    next if $pent{$n}; # only the first declaration of an entity counts

    foreach $pat (keys %SimpleTypes){
		  if ($n =~ /^$pat$/){
				$buf .= " <!_DATATYPE $n $SimpleTypes{$pat} $repltext> ";
				$pent{$n} = "#DATATYPEREF $n";
				undef $n;
				last;
		  }
    }

    foreach $pat (@AttrGroupPatterns){
		  if ($n =~ /^$pat$/){
				$buf .= " <!_ATTRGROUP $n $repltext> ";
				$pent{$n} = "#ATTRGROUPREF $n";
				undef $n;
				last;
		  }
    }

    foreach $pat (@ModelGroupPatterns){
		  if ($n =~ /^$pat$/){
				$buf .= " <!_MODELGROUP $n $repltext> ";
				$pent{$n} = "#MODELGROUPREF $n";
				undef $n;
				last;
		  }
    }

    foreach $pat (@SubstitutionGroupPatterns){
		  if ($n =~ /^$pat$/){
				$buf .= " <!_SUBSTGROUP $n $repltext> ";
				$pent{$n} = "#SUBSTGROUPREF $n";
				undef $n;
				last;
		  }

    }

    $pent{$n}=$repltext if $n;
}

# remove all general entities
$buf =~ s/<!ENTITY\s+.*?>//gsie;

# loop until parameter entities fully expanded
my $i;
do {
	 # count # of substitutions
	 $i = 0;
	 # expand parameter entities
	 $buf =~ s/%([a-zA-Z0-9_\.-]+);?/$i++,$pent{$1}/gse;
} while ($i != 0);

# treat conditional sections
while($buf =~ s/<!\[\s*?INCLUDE\s*?\[(.*)\]\]>/\1/gsie) {};
while($buf =~ s/<!\[\s*?IGNORE\s*?\[.*\]\]>//gsie) {};

# store attribute lists
$buf =~ s/<!ATTLIST\s+(\S+)\s+(.*?)>/store_att($1, $2)/gsie;

# store content models
$buf =~ s/<!ELEMENT\s+(\S+)\s+(.+?)>/store_elt($1, $2)/gsie;

#print "<?xml version='1.0'?>\n";
print "<schema
  xmlns='http://www.w3.org/2000/10/XMLSchema'
  targetNamespace='$targetNS'
  xmlns:$prefix='$targetNS'>\n";

# find maximum length of non-terminals
#my $maxlen = max(map(length, @element)) + 4;

# write simple type declarations
$buf =~ s/<!_DATATYPE\s+(\S+)\s+(\S+)\s+(.+?)>/write_simpleType($1, $2, $3)/gsie;

# write attribute groups
$buf =~ s/<!_ATTRGROUP\s+(\S+)\s+(.+?)>/write_attrGroup($1, $2)/gsie;

# write model groups
$buf =~ s/<!_MODELGROUP\s+(\S+)\s+(.+?)>/write_modelGroup($1, $2)/gsie;

# write subsitution groups
$buf =~ s/<!_SUBSTGROUP\s+(\S+)\s+(.+?)>/write_substitutionGroup($1, $2)/gsie;


my($e);

# loop over elements, writing XML schema
foreach $e (@element) {
	 my $h = $model{$e};
	 my $h2 = $attributes{$e};
	 my @model = @$h;
	 my $isSimple = ($pcdata_flag eq 1) && ($model[1] eq '#PCDATA') &&
		  ( ($#model eq 2) || 
			 ( ($#model eq 3) && ($model[3] eq '*') ) );

	 my $substGroup = $SubstitutionGroup{$e};
	 if ( $substGroup )
	 {
		  $substGroup = " substitutionGroup='$substGroup'";
	 }

	 # print rule for element $e
	 if ( $isSimple && ! $h2 )
	 {
		  # Assume (#PCDATA) is string
		  print "\n <element name='$e' type='$pcdata_simpletype'$substGroup>\n";
	 }
	 else {
		  print "\n <element name='$e'$substGroup>\n";
	 }

	 if ( $isSimple )
	 {
		  # Assume (#PCDATA) is string
		  if ( $h2 ) 
		  {
				print "  <complexType>\n";
				print "  <simpleContent>\n";
				print "  <extension base='string'>\n";
		  }
	 }

	 else {
		  # print rule for $e's content model
		  print "  <complexType";
		  if ($model[0] eq 'EMPTY') {
				if (! $h2 ) {
					 print "/>\n";
				} else {
					 print ">\n";
				}
		  } 
		  elsif ( $model[0] eq 'ANY' )
		  {
				print ">\n";
				print "   <sequence>\n";
				print "   <any namespace='$targetNS'/>\n";
				print "   </sequence>\n";
		  }
		  else {
				if ( $debug eq 1 ) {
					 print STDERR "==mixed? @model\n"; #@@
				}
				if (&isMixed(@model)) {
					 print " mixed='true'>\n";
				}
				else {
					 print ">\n";
				}

				my @list = &makeChildList('', @model);
				&printChildList(3, @list);
		  }
	 }

	 # print rule for $e's attributes
	 if (! $h2) {
		  # nothing
	 } else {
		  &printAttrDecls(@$h2);
		  if ( $isSimple ) {
				print "   </extension>\n";
				print "   </simpleContent>\n";
		  }
	 }

	 if ( !$h2 && $isSimple ) {
		  # Do nothing
	 }
	 elsif ($h2 || $model[0] ne 'EMPTY') {
		  print "  </complexType>\n";
	 }

	 print " </element>\n";
}

print "</schema>\n";
exit;

sub printSpace
{
	 my ($num) = $_[0];
	 for (my $i=0; $i<$num; $i++) {
		  print " ";
	 }
}

sub printChildList
{
	 my ($num, @list) = @_;

	 my @currentTag = ();
	 for (my $i=0; $i<= $#list; $i++) {
		  my $n = $list[$i];

		  if ($n eq 0 || $n eq 1 || $n eq 2 || $n eq 3) {
				if ( ($pcdata_flag eq 0) && ($n eq 0 || $n eq 1) && $list[$i+1] eq 20)
				{
					 # The whole list is 0 20 or 1 20; i.e. (#PCDATA) or (#PCDATA)*. 
					 # Don't generate a sequence child; mixed handles all this.
				}
				else {
#            my $do_it_flag = 1;
					 if ( $currentTag[$#currentTag] eq "" && $n eq 0 )
					 {
						  push(@currentTag, "");
#  					 my $n_1 = $list[$i+1];
#  					 if ( $n_1 eq 10 || $n_1 eq 11 || $n_1 eq 12 || $n_1 eq 13 )
#  					 {
#  						  # do nothing: we have a phantom sequence wrapping a choice
#  						  # that we want to not want to appear. OTOH we want a top 
#  						  # level sequence in other cases.
#  						  $do_it_flag = 0;
#  					 }
					 }

#  				if ( $do_it_flag eq 1 )
#  {
					 printSpace($num); $num++;
					 print "<sequence";
					 if ($n eq 1) {
						  print " minOccurs='0' maxOccurs='unbounded'";
					 } elsif ($n eq 2) {
						  print " maxOccurs='unbounded'";
					 } elsif ($n eq 3) {
						  print " minOccurs='0' maxOccurs='1'";
					 }
					 print ">\n";
					 push(@currentTag, "sequence");
				}
#}
		  } elsif ($n eq 10 || $n eq 11 || $n eq 12 || $n eq 13) {
				printSpace($num); $num++;
				print "<choice";
				if ($n eq 11) {
					 print " minOccurs='0' maxOccurs='unbounded'";
				} elsif ($n eq 12) {
					 print " maxOccurs='unbounded'";
				} elsif ($n eq 13) {
					 print " minOccurs='0' maxOccurs='1'";
				}
				print ">\n";
				push(@currentTag, "choice");
		  } elsif ($n eq 20) {
				my $tag = pop(@currentTag);
				if ($tag ne "") {
					 $num--; printSpace($num);
					 print "</", $tag, ">\n";
				}
		  } else {
				printSpace($num);
				if ($n eq '#MODELGROUPREF') {
					 print "<group ref='$prefix:$list[++$i]'";
				}
				elsif ($n eq '#SUBSTGROUPREF') {
					 print "<element ref='$prefix:$list[++$i]'";
				} else {
					 print "<element ref='$prefix:$n'";
				}

				if ($currentTag[$#currentTag] ne "choice") {
					 if ($list[$i+1] eq "+") {
						  print " maxOccurs='unbounded'";
						  $i++;
					 } elsif ($list[$i+1] eq "?") {
						  print " minOccurs='0' maxOccurs='1'";
						  $i++;
					 } elsif ($list[$i+1] eq "*") {
						  print " minOccurs='0' maxOccurs='unbounded'";
						  $i++;
					 }
				}
				print "/>\n";
		  }
	 }
}

sub makeChildList {
	 my ($groupName, @model) = @_;
	 my @ret = ();
	 my @brace = ();
	 for (my $i=0; $i<=$#model; $i++) {
		  my $n = $model[$i];

		  if ($n eq "(") {
				push(@ret, 0);
				push(@brace, $#ret);
		  } elsif ($n eq ")") {
				if ($model[$i+1] eq "*") {
					 $ret[$brace[$#brace]] += 1;
					 $i++;
				} elsif ($model[$i+1] eq "+") {
					 $ret[$brace[$#brace]] += 2;
					 $i++;
				} elsif ($model[$i+1] eq "?") {
					 $ret[$brace[$#brace]] += 3;
					 $i++;
				}
				pop(@brace);
				push(@ret, 20);
		  } elsif ($n eq ",") {
				$ret[$brace[$#brace]] = 0;
		  } elsif ($n eq "|") {
				$ret[$brace[$#brace]] = 10;
		  } elsif ($n eq "#PCDATA") {
				if ($model[$i+1] eq "|") {
					 $i++;
				}
				if($groupName){
					 $Mixed{$groupName} = 1;
				}
		  } else {
				push(@ret, $n);
		  }
	 }

	 # "( ( a | b | c )* )" gets mapped to "0 10 a b c 20 20" which will generate
	 # a spurious sequence element. This is not too harmful when this is an
	 # element content model, but with model groups it is incorrect.
	 # In general we need to strip off 0 20 from the ends when it is redundant. 
	 # Redundant means: there is some other group that bounds the whole list. 
	 # Note that it gets a little tricky:
	 # ( (a|b),(c|d) ) gets mapped to "0 10 a b 20 10 c d 20 20". If one
	 # naively chops off the 0 and 20 on the groups that there is a 10 on one
	 # end and a 20 on the other, one loses the bounding sequence, which is 
	 # required in this case.
	 #
	 if ( $ret[0] eq 0 && $ret[$#ret] eq 20 && $ret[$#ret-1] eq 20 &&
			( $ret[1] eq 0 || $ret[1] eq 1 || $ret[1] eq 2 || $ret[1] eq 3 ||
			  $ret[1] eq 10 || $ret[1] eq 11 || $ret[1] eq 12 || $ret[1] eq 13 )
			)
	 {
		  # OK, it is possible that the 0 20 is redundant. Now scan for balance:
		  # All interim 20 between the proposed new start and the proposed new
		  # final one should be at level 1 or above. 
		  my $depth = 0;
		  my $redundant_paren = 1;  # Assume redundant until proved otherwise
		  for ( my $i = 1; $i <= $#ret-1; $i++ )
		  {
				if ( $ret[$i] eq 20 )
				{
					 $depth--;
					 if ( $i < $#ret-1 && $depth < 1 )
					 {
						  $redundant_paren = 0;
						  print STDERR "i=$i,depth=$depth\n";
					 }
				}
				elsif ( $ret[$i] eq 0 || 
						  $ret[$i] eq 1 || 
						  $ret[$i] eq 2 || 
						  $ret[$i] eq 3 ||
						  $ret[$i] eq 10 || 
						  $ret[$i] eq 11 || 
						  $ret[$i] eq 12 || 
						  $ret[$i] eq 13 
						  )
				{
					 $depth++;
				}
		  }  # for

		  if ( $redundant_paren eq 1 )
		  {
				print STDERR "Truncating @ret\n";
				@ret = @ret[1..$#ret-1];
		  }
	 }

	 if ( $debug eq 1 ) {
		  print STDERR "@model to @ret\n";
	 }
	 return @ret;
}


sub printAttrDecls{
    my @atts = @_;

    for (my $i = 0; $i <= $#atts; $i++) {
		  if ($atts[$i] eq '#ATTRGROUPREF'){
				print "   <attributeGroup ref='$prefix:$atts[$i+1]'/>\n";
				$i ++;
		  } else {
				# attribute name
				print "   <attribute name='$atts[$i]'";

				# attribute type
				my @enume;
				$i++;
				if ($atts[$i] eq "(") {
					 # like `attname ( yes | no ) #REQUIRED`
					 $i++;
					 while ($atts[$i] ne ")") {
						  if ($atts[$i] ne "|") {
								push(@enume, $atts[$i]);
						  }
						  $i++;
					 }
				} elsif ($atts[$i] eq '#DATATYPEREF'){
					 print " type='$prefix:$atts[++$i]'";
				} elsif ($alias eq 1 && $atts[$i] =~ s/$alias_ident//gsie) {
					 # alias special
					 print " type='$alias_dic{$atts[$i]}'";
				} elsif ($atts[$i] =~ /ID|IDREF|ENTITY|NOTATION|IDREFS|ENTITIES|NMTOKEN|NMTOKENS/) {
					 # common type for DTD and Schema
					 print " type='$atts[$i]'";
				} else {
					 # `attname CDATA #REQUIRED`
					 print " type='string'";
				}

				$i++;

				# #FIXED
				if($atts[$i] eq "#FIXED") {
					 $i++;
					 print " use='fixed' value='$atts[$i]'/>\n";
				} else {
					 # minOccurs
					 if ($atts[$i] eq "#REQUIRED") {
						  print " use='required'";
					 } elsif ($atts[$i] eq "#IMPLIED") {
						  print " use='optional'";
					 } else {
						  print " use='default' value='$atts[$i]'";
					 }

					 # enumerate
					 if ($#enume eq -1) {
						  print "/>\n";
					 } else {
						  print ">\n";
						  print "    <simpleType>\n";
						  print "     <restriction base='string'>\n";
						  &write_enum(@enume);
						  print "     </restriction>\n";
						  print "    </simpleType>\n";
						  print "   </attribute>\n";
					 }
				}
		  }
    }
}

sub write_enum{
    my(@enume) = @_;

    for (my $j = 0; $j <= $#enume; $j++) {
		  print "      <enumeration value='$enume[$j]'/>\n";
    }
}


# Parse a string into an array of "words".
# Words are whitespace-separated sequences of non-whitespace characters,
# or quoted strings ("" or ''), with the quotes removed.
# HACK: added () stuff for attlist stuff
# Parse words for attribute list
sub parsewords {
	 my $line = $_[0];
	 $line =~ s/(\(|\)|\|)/ $1 /g;
	 my @words = ();

	 while ($line ne '') {
		  if ($line =~ /^\s+/) {
				# Skip whitespace
		  } elsif ($line =~ /^\"((?:[^\"]|\\\")*)\"/) {
				push(@words, $1);
		  } elsif ($line =~ /^\'((?:[^\']|\\\')*)\'/) {
				push(@words, $1);
		  } elsif ($line =~ /^\S+/) {
				push(@words, $&);
		  } else {
				die "Cannot happen\n";
		  }
		  $line = $';
	 }
    return @words;
}

# Store content model, return empty string
sub store_elt
{
	 my ($name, $model) = @_;
	 $model =~ s/\s+/ /gso;
	 push(@element, $name);

	 my @words;
	 while ($model =~ s/^\s*(\(|\)|,|\+|\?|\||[\w_\.-]+|\#\w+|\*)//) {
		  push(@words, $1);
	 };
	 $model{$name} = [ @words ];
	 return '';
}


# Store attribute list, return empty string
sub store_att
{
	 my ($element, $atts) = @_;
	 my @words = parsewords($atts);
	 $attributes{$element} = [ @words ];
	 return '';
}

sub write_simpleType{
    my($n, $b, $stuff) = @_;
    my @words = parsewords($stuff);

    print "\n  <simpleType name='$n'>\n";
    print "   <restriction base='$b'>\n";
#    print STDERR "\n==stuff:\n$stuff \n\n===\n", join('|', @words);

    my $i = 0;
    my @enume;

    if ($words[$i] eq "(") {
		  $i++;
		  while ($words[$i] ne ")") {
				if ($words[$i] ne "|") {
					 push(@enume, $words[$i]);
				}
				$i++;
		  }
		  write_enum(@enume);
    }

	 print "   </restriction>\n";
    print "  </simpleType>\n";
}

sub write_attrGroup{
    my($n, $stuff) = @_;
    my @words = parsewords($stuff);

    print "\n  <attributeGroup name='$n'>\n";
#    print STDERR "\n==stuff:\n$stuff \n\n===\n", join('|', @words);
    printAttrDecls(@words);
    print "  </attributeGroup>\n";
}

sub write_modelGroup{
    my($n, $stuff) = @_;
    my @words = parsewords($stuff);

    print "\n  <group name='$n'>\n";
    print "<!-- $stuff -->\n";

    my @list = &makeChildList($n, '(', @words, ')');
    &printChildList(3, @list);

    $ModelGroup{$n} = \@list;

    print "  </group>\n";
}

sub write_substitutionGroup
{
    my($n, $stuff) = @_;
    my @words = parsewords($stuff);

    print "\n  <element name='$n' abstract='true'>\n";

    my @list = &makeChildList($n, '(', @words, ')');
	 for ( my $i = 0; $i < $#list; $i++ )
	 {
		  $SubstitutionGroup{ $list[$i] } = $n;
	 }

    print "  </element>\n";
}

sub isMixed{
    my(@model) = @_;
	 my $isSimple = ($pcdata_flag eq 1) && ($model[1] eq '#PCDATA') &&
		  ( ($#model eq 2) || 
			 ( ($#model eq 3) && ($model[3] eq '*') ) );

	 if ( $debug eq 1 ) {
		  print STDERR "++ mixed? @model\n"; #@@
	 }

	 if ( $isSimple )
	 {
		  if ( $debug eq 1 ) 
		  {
				print STDERR "++ no; simple type. @model\n"; #@@
		  }
		  return 0;
	 }

    my($i);

    for ($i = 0; $i <= $#model; $i++) {
		  if ( $model[$i] eq '#PCDATA' ||
				 ($model[$i] eq '#MODELGROUPREF' && $Mixed{$model[$i+1]}) ||
				 ($model[$i] eq '#SUBSTGROUPREF' && $Mixed{$model[$i+1]}) )
		  {
				if ( $debug eq 1 ) {
					 print STDERR "++ yes! $i @model\n"; #@@
				}
				return 1;
		  }
    }

	 if ( $debug eq 1 ) {
		  print STDERR "++ no. @model\n"; #@@
	 }

    return 0;
}

# Return maximum value of an array of numbers
sub max
{
	 my $max = $_[0];
	 foreach my $i (@_) {
		  if ($i > $max) {$max = $i;}
	 }
	 return $max;
}


# 1) Open file
# 2) Remove comment, processing instructions, and general entities
# 3) Include external parameter entities recursively
# 4) Return the contents of opened file
sub openFile {
	 my $file = $_[0];

	 my %extent;
	 my $bufbuf;
	 if ($file ne "") {
		  print STDERR "open $file ";
		  if(! open AAA, $file) {
				print STDERR " failed!!\n";
				return "";
		  }
		  print STDERR " successful\n";
		  $bufbuf = <AAA>;
	 } else {
		  print STDERR "open STDIN successful\n";
		  $bufbuf = <>;
	 }

	 # remove comments
	 $bufbuf =~ s/<!--.*?-->//gso;

	 # remove processing instructions
	 $bufbuf =~ s/<\?.*?>//gso;

	 # store external parameter entities
	 while ($bufbuf =~ s/<!ENTITY\s+%\s+(\S+)\s+PUBLIC\s+$str\s+$str.*?>//sie) {
		  $extent{$1} = $4.$5;
	 }
	 while ($bufbuf =~ s/<!ENTITY\s+%\s+(\S+)\s+SYSTEM\s+$str.*?>//sie) {
		  $extent{$1} = $2.$3;
	 }

	 # read external entity files
	 foreach my $key (keys(%extent)) {
		  $bufbuf =~ s/%$key;/openFile($extent{$key})/gsie;
	 }

	 return $bufbuf;
}

# $Log: dtd2xsd.pl,v $
# Revision 1.1  2006/10/13 11:52:07  vladdu
# added srs framework
#
# Revision 1.17  2001/01/19 05:59:12  connolly
# more changelog stuff; link to MH's announcement etc.
#
# Revision 1.16  2001/01/19 05:55:56  connolly
# added Log at end
#
# Changes: 2001/01/10
# Date:      Thu, 11 Jan 2001 14:51:44 -0800
# From:      Mary Holstege <holstege@mathling.com>
# To:        xml-dev@lists.xml.org
# Subject:   [ANN] Updated version of DTD to XML Schema tool
# http://lists.xml.org/archives/xml-dev/200101/msg00481.html
# http://www.mathling.com/xmlschema/
# Switch to CR syntax
# Support external mapping file for type aliases, simple types, model and
#    attribute groups
# Map ANY correctly to wildcard rather than element 'ANY'
# Support treating lead PCDATA as string or other aliased simple type instead
# of as mixed content (may be more appropriate for data-oriented DTDs)
#    e.g. <!ELEMENT title (#PCDATA)> => <element name="title" type="string"/>
# Support subsitution groups.
