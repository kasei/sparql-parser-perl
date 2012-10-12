package SPARQL::AST;

use 5.010;
use Moose;
# use SPARQL::AST::Constants;
use Scalar::Util qw(reftype);

has 'type' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
);

has 'value' => (
	is => 'ro',
);

has 'children' => (
	is => 'ro',
	isa => 'ArrayRef[SPARQL::AST]',
);


sub as_string {
	my $self		= shift;
	my $indent		= shift || 0;
	my $head		= '  ' x $indent;
	my $type		= $self->type;
	my $value		= $self->value;
	my $children	= $self->children || [];
	my $string		= '';
	if (defined($value)) {
		if (ref($value) and not(blessed($value)) and reftype($value) eq 'ARRAY') {
			$value	= join(' ', @$value);
		} elsif (blessed($value)) {
			$value	= $value->as_string;
		}
		$string	.= "${head}${type} ($value)\n";
	} else {
		$string	.= "${head}${type}\n";
	}
	
	foreach my $c (@$children) {
		$string	.= $c->as_string( $indent+1 );
	}
	
	return $string;
}


__PACKAGE__->meta->make_immutable;

