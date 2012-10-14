package SPARQL::Token;

use 5.010;
use MooseX::ArrayRef;
use SPARQL::Constants;
use RDF::Trine::Namespace qw(xsd);
use RDF::Trine;

has type => ( is => 'ro', );
has start_line => ( is => 'ro', );
has start_column => ( is => 'ro', );
has line => ( is => 'ro', );
has column => ( is => 'ro', );
has args => ( is => 'ro', );

sub value {
	my $self	= shift;
	my $args	= $self->args;
	return $args->[0];
}

# This constructor relies on the list of attributes not changing order!
sub fast_constructor {
	my $class = shift;
	bless \@_, $class;
}

sub is_term {
	my $self	= shift;
	my $type	= $self->type;
	return ($type == IRI or $type == PREFIXNAME);
}

sub as_node {
	my $self	= shift;
	my %args	= @_;
	my $map		= $args{namespaces};
	my $base	= $args{base};
	my $value	= $self->value;
	
	given ($self->type) {
		return RDF::Trine::Node::Literal->new($value, undef, $xsd->boolean) when (BOOLEAN);
		return RDF::Trine::Node::Literal->new($value, undef, $xsd->double) when (DOUBLE);
		return RDF::Trine::Node::Literal->new($value, undef, $xsd->decimal) when (DECIMAL);
		return RDF::Trine::Node::Literal->new($value, undef, $xsd->integer) when (INTEGER);
		return RDF::Trine::Node::Variable->new($value) when (VAR);
		return RDF::Trine::Node::Resource->new($value, $base) when (IRI);
		return RDF::Trine::Node::Resource->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil') when (NIL);
		when (KEYWORD) {
			if ($self->value eq 'A') {
				return RDF::Trine::Node::Resource->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
			}
		}
		when (PREFIXNAME) {
			my ($n,$l)	= @{ $self->args };
			my $ns	= $map->namespace_uri($n);
			unless ($ns) {
				die "No prefix defined for namespace '$n'";
			}
			
			if (length($l)) {
				return $ns->$l();
			} else {
				return RDF::Trine::Node::Resource->new($ns->uri->as_string);
			}
		}
	}
	
	use Data::Dumper;
	warn Dumper($self->args);
	Carp::confess "as_node is not implemented for " . decrypt_constant($self->type);
}

__PACKAGE__->meta->make_immutable;

1;
