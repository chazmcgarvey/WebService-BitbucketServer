# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::SSH::V1;
# ABSTRACT: Bindings for a Bitbucket Server REST API


use warnings;
use strict;

our $VERSION = '0.603'; # VERSION

use Moo;
use namespace::clean;


has context => (
    is          => 'ro',
    isa         => sub { die 'Not a WebService::BitbucketServer' if !$_[0]->isa('WebService::BitbucketServer'); },
    required    => 1,
);


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


sub get_keys_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub add_key_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}


sub get_key_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh/{keyId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub revoke_key_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh/{keyId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}


sub update_permission_for_repository {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh/{keyId}/permission/{permission}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'PUT', url => $url, $data ? (data => $data) : ());
}


sub get_keys_for_project {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/ssh', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub add_key_for_project {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/ssh', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}


sub get_key_for_project {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/ssh/{keyId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub revoke_key_for_project {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/ssh/{keyId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}


sub update_permission_for_project {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/projects/{projectKey}/ssh/{keyId}/permission/{permission}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'PUT', url => $url, $data ? (data => $data) : ());
}


sub revoke_key {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/ssh/{keyId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}


sub get_projects_for_key {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/ssh/{keyId}/projects', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub get_repositories_for_key {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('keys/1.0/ssh/{keyId}/repos', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub get_ssh_keys {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('ssh/1.0/keys', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


sub add_ssh_key {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('ssh/1.0/keys', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}


sub delete_ssh_keys {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('ssh/1.0/keys', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}


sub delete_ssh_key {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('ssh/1.0/keys/{keyId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}


sub get_ssh_settings {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('ssh/1.0/settings', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}


1;

__END__

=pod

=encoding UTF-8

=head1 NAME

WebService::BitbucketServer::SSH::V1 - Bindings for a Bitbucket Server REST API

=head1 VERSION

version 0.603

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->ssh;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<SSH::V1|https://developer.atlassian.com/static/rest/bitbucket-server/5.5.0/bitbucket-ssh-rest.html>.

Original API documentation created by and copyright Atlassian.

=head1 ATTRIBUTES

=head2 context

Get the instance of L<WebService::BitbucketServer> passed to L</new>.

=head1 METHODS

=head2 new

    $api = WebService::BitbucketServer::SSH::V1->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->ssh >>> instead.

=head2 get_keys_for_repository

Retrieves the access keys for the repository identified in the URL.

    GET keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh

Parameters:

=over 4

=item * C<<< filter >>> - string, default: none

if specified only SSH access keys with a label prefixed with the supplied string will be returned

=item * C<<< effective >>> - boolean, default: false

Controls whether SSH access keys configured at the project level should be included in the
results or not. When set to C<<< true >>> all keys that have I<<< access >>> to the repository
(including project level keys) are included in the results. When set to C<<< false >>>, only
access keys configured for the specified C<<< repository >>> are considered.
Default is C<<< false >>>.

=item * C<<< permission >>> - string, default: none

if specified only SSH access keys with at least the supplied permission will be returned
Default is C<<< Permission.REPO_READ >>>.

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

A single page of access keys for the repository.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to retrieve the
access keys for this repository

=item * C<<< 404 >>> - errors, type: application/json

The specified repository does not exist

=back

=head2 add_key_for_repository

Register a new SSH key and grants access to the repository identified in the URL.

    POST keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The newly created access key

=item * C<<< 400 >>> - errors, type: application/json

The current request contains invalid or missing values.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to add an access
key to the repository

=item * C<<< 404 >>> - errors, type: application/json

The specified repository does not exist

=back

=head2 get_key_for_repository

Retrieves the access keys for the repository identified in the URL.

    GET keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh/{keyId}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the identifier of the SSH key

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

A single page of access keys for the repository.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to retrieve the
access keys for this repository

=item * C<<< 404 >>> - errors, type: application/json

The specified repository does not exist

=back

=head2 revoke_key_for_repository

Remove an existing access key for the repository identified in the URL. If the same SSH key is used as an access
key for multiple projects or repositories, only the access to the repository identified in the URL will be
revoked.

    DELETE keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh/{keyId}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the identifier of the SSH key

=back

Responses:

=over 4

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to remove
access keys for this repository

=item * C<<< 204 >>> - data, type: application/json

The access key was deleted (or none was found matching the given id).

=back

=head2 update_permission_for_repository

Updates the permission granted to the specified SSH key to the project identified in the URL.

    PUT keys/1.0/projects/{projectKey}/repos/{repositorySlug}/ssh/{keyId}/permission/{permission}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the SSH key ID

=item * C<<< permission >>> - string, default: none

the new permission to be granted to the SSH key

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The newly created access key

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions on the project
to edit its access keys

=item * C<<< 404 >>> - errors, type: application/json

The specified project does not exist

=back

=head2 get_keys_for_project

Retrieves the access keys for the project identified in the URL.

    GET keys/1.0/projects/{projectKey}/ssh

Parameters:

=over 4

=item * C<<< filter >>> - string, default: none

if specified only SSH access keys with a label prefixed with the supplied string will be returned

=item * C<<< permission >>> - string, default: none

if specified only SSH access keys with at least the supplied permission will be returned
Default is {@link Permission#PROJECT_READ}.

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

A single page of access keys associated with the project.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to retrieve the
access keys for this project

=item * C<<< 404 >>> - errors, type: application/json

The specified project does not exist

=back

=head2 add_key_for_project

Register a new SSH key and grants access to the project identified in the URL.

    POST keys/1.0/projects/{projectKey}/ssh

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The newly created access key

=item * C<<< 400 >>> - errors, type: application/json

The current request contains invalid or missing values.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to add an access
key to the project.

=item * C<<< 404 >>> - errors, type: application/json

The specified project does not exist

=back

=head2 get_key_for_project

Retrieves the access keys for the project identified in the URL.

    GET keys/1.0/projects/{projectKey}/ssh/{keyId}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the identifier of the SSH key

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

A single page of access keys associated with the project.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to retrieve the
access keys for this project

=item * C<<< 404 >>> - errors, type: application/json

The specified project does not exist

=back

=head2 revoke_key_for_project

Remove an existing access key for the project identified in the URL. If the same SSH key is used as an access
key for multiple projects or repositories, only the access to the project identified in the URL will be
revoked.

    DELETE keys/1.0/projects/{projectKey}/ssh/{keyId}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the identifier of the SSH key

=back

Responses:

=over 4

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to remove
access keys for this project

=item * C<<< 204 >>> - data, type: application/json

The access key was deleted (or none was found matching the given id).

=back

=head2 update_permission_for_project

Updates the permission granted to the specified SSH key to the project identified in the URL.

    PUT keys/1.0/projects/{projectKey}/ssh/{keyId}/permission/{permission}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the SSH key ID

=item * C<<< permission >>> - string, default: none

the new permission to be granted to the SSH key

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The newly created access key

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions on the project
to edit its access keys

=item * C<<< 404 >>> - errors, type: application/json

The specified project does not exist

=back

=head2 revoke_key

Remove an existing access key for the projects and repositories in the submitted entity. If the same SSH key is
used as an access key for multiple projects or repositories not supplied, only the access to the projects
or repositories identified will be revoked.

    DELETE keys/1.0/ssh/{keyId}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the identifier of the SSH key

=back

Responses:

=over 4

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to remove
access keys for one or more of the specified projects or repositories

=item * C<<< 204 >>> - data, type: application/json

The access keys were deleted (or none was found matching the given id and
repositories or projects).

=item * C<<< 404 >>> - errors, type: application/json

On or more of the specified repositories or projects does not exist or the key
itself does not exist

=back

=head2 get_projects_for_key

Retrieves all project-related access keys for the SSH key with id C<<< keyId >>>. If the current user is not an
admin any of the projects the key provides access to, none are returned.

    GET keys/1.0/ssh/{keyId}/projects

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

the SSH key with ID C<<< keyId >>>.

=item * C<<< 404 >>> - errors, type: application/json

The specified key does not exist

=back

=head2 get_repositories_for_key

Retrieves all repository-related access keys for the SSH key with id C<<< keyId >>>. If the current user is not
an admin of any of the projects the key provides access to, none are returned.

    GET keys/1.0/ssh/{keyId}/repos

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

the SSH key with ID C<<< keyId >>>.

=item * C<<< 404 >>> - errors, type: application/json

The specified key does not exist

=back

=head2 get_ssh_keys

Retrieve a page of ssh keys.

    GET ssh/1.0/keys

Parameters:

=over 4

=item * C<<< user >>> - string, default: none

the username of the user to retrieve the keys for.
If no username is specified, the ssh keys will be
retrieved for the current authenticated user.

=back

Responses:

=over 4

=item * C<<< 200 >>> - page, type: application/json

A page of ssh keys.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions
to retrieve the ssh keys. This is only possible when a
B<<< user >>> is explicitly supplied.

=item * C<<< 404 >>> - errors, type: application/json

No user matches the supplied B<<< user >>>

=back

=head2 add_ssh_key

Add a new ssh key to a supplied user.

    POST ssh/1.0/keys

Parameters:

=over 4

=item * C<<< user >>> - string, default: none

the username of the user to add the ssh key for.
If no username is specified, the ssh key will
be added for the current authenticated user.

=back

Responses:

=over 4

=item * C<<< 201 >>> - sshKey, type: application/json

The newly created ssh key.

=item * C<<< 400 >>> - errors, type: application/json

The ssh key was not created because the key was not a valid
RSA/DSA/ECDSA/Ed25519 key of a supported length.

=item * C<<< 401 >>> - errors, type: application/json

Either there is no authenticated user or the currently authenticated user
has insufficient permissions to add an ssh key. The latter is only
possible when a B<<< user >>> is explicitly supplied.

=item * C<<< 404 >>> - errors, type: application/json

No user matches the supplied B<<< user >>>

=item * C<<< 409 >>> - errors, type: application/json

The ssh key already exists on the system.

=back

=head2 delete_ssh_keys

Delete all ssh keys for a supplied user.

    DELETE ssh/1.0/keys

Parameters:

=over 4

=item * C<<< user >>> - string, default: none

the username of the user to delete the keys for.
If no username is specified, the ssh keys will
be deleted for the current authenticated user.

=back

Responses:

=over 4

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions
to delete the ssh keys. This is only possible when a
B<<< user >>> is explicitly supplied.

=item * C<<< 204 >>> - data, type: unknown

The ssh keys matching the supplied B<<< user >>> were deleted.

=item * C<<< 404 >>> - errors, type: application/json

No user matches the supplied B<<< user >>>

=back

=head2 delete_ssh_key

Delete an ssh key.

    DELETE ssh/1.0/keys/{keyId}

Parameters:

=over 4

=item * C<<< keyId >>> - int, default: none

the id of the key to delete.

=back

Responses:

=over 4

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions
to delete the ssh key.

=item * C<<< 204 >>> - data, type: unknown

The ssh key matching the supplied B<<< id >>> was deleted or
did not exist.

=back

=head2 get_ssh_settings

    GET ssh/1.0/settings

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The ssh settings from upstream

=item * C<<< 401 >>> - errors, type: application/json

The request was not authenticated

=back

=head1 SEE ALSO

=over 4

=item * L<WebService::BitbucketServer>

=item * L<https://developer.atlassian.com/bitbucket/server/docs/latest/>

=back

=head1 BUGS

Please report any bugs or feature requests on the bugtracker website
L<https://github.com/chazmcgarvey/WebService-BitbucketServer/issues>

When submitting a bug or request, please include a test-file or a
patch to an existing test-file that illustrates the bug or desired
feature.

=head1 AUTHOR

Charles McGarvey <chazmcgarvey@brokenzipper.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2017 by Charles McGarvey.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
