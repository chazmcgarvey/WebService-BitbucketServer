# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::RefRestriction::V2;
# ABSTRACT: Bindings for a Bitbucket Server REST API

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->ref_restriction;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<RefRestriction::V2|https://developer.atlassian.com/static/rest/bitbucket-server/5.5.0/bitbucket-ref-restriction-rest.html>.

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

    $api = WebService::BitbucketServer::RefRestriction::V2->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->ref_restriction >>> instead.

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

=head2 create_restrictions_for_repository

Allows creating multiple restrictions at once.

    POST branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions

Responses:

=over 4

=item * C<<< 200 >>> - restriction, type: application/json

Response contains the ref restrictions that were just created.

=item * C<<< 400 >>> - errors, type: application/json

The request has failed validation.

=item * C<<< 401 >>> - validation, type: application/json

The currently authenticated user has insufficient permissions to perform this operation.

=back

=cut

sub create_restrictions_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head2 get_restrictions_for_repository

Search for restrictions using the supplied parameters.

The authenticated user must have B<<< REPO_ADMIN >>> permission or higher to call this resource.
Only authenticated users may call this resource.

    GET branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions

Parameters:

=over 4

=item * C<<< type >>> - string, default: none

(optional) types of restrictions to filter on: one of 'read-only', 'no-deletes', 'fast-forward-only' or 'pull-request-only'.

=item * C<<< matcherType >>> - string, default: none

(optional) matcher type to filter on: one of 'BRANCH', 'PATTERN', 'MODEL_CATEGORY' or 'MODEL_BRANCH'.

=item * C<<< matcherId >>> - string, default: none

(optional) matcher id to filter on. Requires the matcherType parameter to be specified also.

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

=item * C<<< 404 >>> - not-found, type: application/json

The restriction could not be found.

=back

=cut

sub get_restrictions_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 get_restriction_for_repository

Returns a restriction as specified by a restriction id.

The authenticated user must have B<<< REPO_ADMIN >>> permission or higher
to call this resource.
Only authenticated users may call this resource.

    GET branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

=back

Responses:

=over 4

=item * C<<< 200 >>> - restriction, type: application/json

The restriction that was created

=item * C<<< 404 >>> - not-found, type: unknown

The restriction could not be found.

=back

=cut

sub get_restriction_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions/{id}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 delete_restriction_for_repository

Deletes a restriction as specified by a restriction id.

The authenticated user must have B<<< REPO_ADMIN >>> permission or higher
to call this resource.
Only authenticated users may call this resource.

    DELETE branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

=back

Responses:

=over 4

=item * C<<< 204 >>> - data, type: unknown

an empty response indicating that the restriction no longer exists on the repository

=back

=cut

sub delete_restriction_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/repos/{repositorySlug}/restrictions/{id}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}

=head2 create_restriction

Allows creating multiple restrictions at once.

    POST branch-permissions/2.0/projects/{projectKey}/restrictions

Responses:

=over 4

=item * C<<< 200 >>> - restriction, type: application/json

Response contains the ref restrictions that were just created.

=item * C<<< 400 >>> - errors, type: application/json

The request has failed validation.

=item * C<<< 401 >>> - validation, type: application/json

The currently authenticated user has insufficient permissions to perform this operation.

=back

=cut

sub create_restriction {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/restrictions', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head2 get_restrictions

Search for restrictions using the supplied parameters.

The authenticated user must have B<<< REPO_ADMIN >>> permission or higher to call this resource.
Only authenticated users may call this resource.

    GET branch-permissions/2.0/projects/{projectKey}/restrictions

Parameters:

=over 4

=item * C<<< type >>> - string, default: none

(optional) types of restrictions to filter on: one of 'read-only', 'no-deletes', 'fast-forward-only' or 'pull-request-only'.

=item * C<<< matcherType >>> - string, default: none

(optional) matcher type to filter on: one of 'BRANCH', 'PATTERN', 'MODEL_CATEGORY' or 'MODEL_BRANCH'.

=item * C<<< matcherId >>> - string, default: none

(optional) matcher id to filter on. Requires the matcherType parameter to be specified also.

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

=item * C<<< 404 >>> - not-found, type: application/json

The restriction could not be found.

=back

=cut

sub get_restrictions {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/restrictions', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 get_restriction

Returns a restriction as specified by a restriction id.

The authenticated user must have B<<< REPO_ADMIN >>> permission or higher
to call this resource.
Only authenticated users may call this resource.

    GET branch-permissions/2.0/projects/{projectKey}/restrictions/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

=back

Responses:

=over 4

=item * C<<< 200 >>> - restriction, type: application/json

The restriction that was created

=item * C<<< 404 >>> - not-found, type: unknown

The restriction could not be found.

=back

=cut

sub get_restriction {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/restrictions/{id}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 delete_restriction

Deletes a restriction as specified by a restriction id.

The authenticated user must have B<<< PROJECT_ADMIN >>> permission or higher
to call this resource.
Only authenticated users may call this resource.

    DELETE branch-permissions/2.0/projects/{projectKey}/restrictions/{id}

Parameters:

=over 4

=item * C<<< id >>> - int, default: none

=back

Responses:

=over 4

=item * C<<< 204 >>> - data, type: unknown

an empty response indicating that the restriction no longer exists on the project

=back

=cut

sub delete_restriction {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-permissions/2.0/projects/{projectKey}/restrictions/{id}', $args);
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
