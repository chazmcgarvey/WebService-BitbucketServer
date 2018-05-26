# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::AccessTokens::V1;
# ABSTRACT: Bindings for a Bitbucket Server REST API

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->access_tokens;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<AccessTokens::V1|https://developer.atlassian.com/static/rest/bitbucket-server/5.10.0/bitbucket-access-tokens-rest.html>.

Original API documentation created by and copyright Atlassian.

=cut

use warnings;
use strict;

our $VERSION = '9999.999'; # VERSION

use Moo;
use namespace::clean;

=head1 ATTRIBUTES

=head2 context

Get the instance of L<WebService::BitbucketServer> passed to L</new>.

=cut

has context => (
    is          => 'ro',
    isa         => sub { die 'Not a WebService::BitbucketServer' if !$_[0]->isa('WebService::BitbucketServer'); },
    required    => 1,
);

=head1 METHODS

=head2 new

    $api = WebService::BitbucketServer::AccessTokens::V1->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->access_tokens >>> instead.

=cut

sub _croak { require Carp; Carp::croak(@_) }

sub _get_url {
    my $url  = shift;
    my $args = shift || {};
    $url =~ s/\{([^:}]+)(?::\.\*)?\}/_get_path_parameter($1, $args)/eg;
    return $url;
}

sub _get_path_parameter {
    my $name = shift;
    my $args = shift || {};
    return delete $args->{$name} if defined $args->{$name};
    $name =~ s/([A-Z])/'_'.lc($1)/eg;
    return delete $args->{$name} if defined $args->{$name};
    _croak("Missing required parameter $name");
}

=head2 create_token

Create an access token for the user according to the given request

    PUT access-tokens/1.0/users/{userSlug}

Responses:

=over 4

=item * C<<< 200 >>> - accessToken, type: application/json

A response containing the raw access token and associated details

=item * C<<< 400 >>> - errors, type: application/json

One of the following error cases occurred (check the error message for more details):

=over 4

=item *

The request does not contain a token name

=item *

The request does not contain a list of permissions, or the list of permissions is empty

=item *

One of the provided permission levels are unknown

=item *

The user already has their maximum number of tokens

=back

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user is not permitted to create an access token on
behalf of this user or authentication failed

=back

=cut

sub create_token {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('access-tokens/1.0/users/{userSlug}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'PUT', url => $url, $data ? (data => $data) : ());
}

=head2 get_tokens

Get all access tokens associated with the given user

    GET access-tokens/1.0/users/{userSlug}

Responses:

=over 4

=item * C<<< 200 >>> - accessToken, type: application/json

A response containing a page of access tokens and associated details

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user is not permitted to get access tokens on
behalf of this user or authentication failed

=item * C<<< 404 >>> - errors, type: application/json

The specified user does not exist

=back

=cut

sub get_tokens {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('access-tokens/1.0/users/{userSlug}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 update_token

Modify an access token for the user according to the given request. Any fields not specified
will not be altered

    POST access-tokens/1.0/users/{userSlug}/{tokenId}

Parameters:

=over 4

=item * C<<< tokenId >>> - string, default: none

the ID of the token

=back

Responses:

=over 4

=item * C<<< 200 >>> - accessToken, type: application/json

A response containing the updated access token and associated details

=item * C<<< 400 >>> - errors, type: application/json

One of the provided permission levels are unknown

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user is not permitted to update an access token on
behalf of this user or authentication failed

=back

=cut

sub update_token {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('access-tokens/1.0/users/{userSlug}/{tokenId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head2 get_token

Get an access token for the user according to the given ID

    GET access-tokens/1.0/users/{userSlug}/{tokenId}

Parameters:

=over 4

=item * C<<< tokenId >>> - string, default: none

the ID of the token

=back

Responses:

=over 4

=item * C<<< 200 >>> - accessToken, type: application/json

A response containing the access token and associated details

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user is not permitted to get access tokens on
behalf of this user or authentication failed

=item * C<<< 404 >>> - errors, type: application/json

The specified user or token does not exist

=back

=cut

sub get_token {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('access-tokens/1.0/users/{userSlug}/{tokenId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 delete_token

Delete an access token for the user according to the given ID

    DELETE access-tokens/1.0/users/{userSlug}/{tokenId}

Parameters:

=over 4

=item * C<<< tokenId >>> - string, default: none

the ID of the token

=back

Responses:

=over 4

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user is not permitted to delete an access token on
behalf of this user or authentication failed

=item * C<<< 204 >>> - data, type: application/json

an empty response indicating that the token has been deleted

=item * C<<< 404 >>> - errors, type: application/json

The specified user or token does not exist

=back

=cut

sub delete_token {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('access-tokens/1.0/users/{userSlug}/{tokenId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}

=head1 SEE ALSO

=over 4

=item * L<WebService::BitbucketServer>

=item * L<https://developer.atlassian.com/bitbucket/server/docs/latest/>

=back

=cut

1;
