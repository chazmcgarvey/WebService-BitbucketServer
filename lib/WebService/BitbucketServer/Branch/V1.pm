# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::Branch::V1;
# ABSTRACT: Bindings for a Bitbucket Server REST API

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->branch;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<Branch::V1|https://developer.atlassian.com/static/rest/bitbucket-server/5.10.0/bitbucket-branch-rest.html>.

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

    $api = WebService::BitbucketServer::Branch::V1->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->branch >>> instead.

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

=head2 create_branch

Creates a branch in the specified repository.

The authenticated user must have an effective B<<< REPO_WRITE >>> permission to call this resource. If
branch permissions are set up in the repository, the authenticated user must also have access to the branch name
that is to be created.

    POST branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branches

Responses:

=over 4

=item * C<<< 201 >>> - data, type: application/json

a JSON representation of the newly created branch

=item * C<<< 400 >>> - errors, type: application/json

the branch was not created because the request was invalid, e.g. the provided
ref name already existed in the repository, or was not a valid ref name in the
repository

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to create a
branch. This could be due to insufficient repository permissions, or lack of
branch permission for the provided ref name

=back

=cut

sub create_branch {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branches', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head2 delete_branch

Deletes a branch in the specified repository.

If the branch does not exist, this operation will not raise an error. In other words after calling this resource
and receiving a 204 response the branch provided in the request is guaranteed to not exist in the specified
repository any more, regardless of its existence beforehand.

The optional 'endPoint' parameter of the request may contain a commit ID that the provided ref name is
expected to point to. Should the ref point to a different commit ID, a 400 response will be returned with
appropriate error details.

The authenticated user must have an effective B<<< REPO_WRITE >>> permission to call this resource. If
branch permissions are set up in the repository, the authenticated user must also have access to the branch name
that is to be deleted.

    DELETE branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branches

Responses:

=over 4

=item * C<<< 400 >>> - errors, type: application/json

the branch was not deleted because the request was invalid, e.g. no ref name
to delete was provided, or the provided ref name points to the default branch
in the repository that cannot be deleted

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to delete a
branch. This could be due to insufficient repository permissions, or lack of
branch permission for the provided ref name.

=item * C<<< 204 >>> - data, type: unknown

an empty response indicating that the branch no longer exists in the repository

=back

=cut

sub delete_branch {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branches', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'DELETE', url => $url, $data ? (data => $data) : ());
}

=head2 find_branch_info_by_commit

Gets the branch information associated with a single commit from a given repository.

    GET branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branches/info/{commitId}

Parameters:

=over 4

=item * C<<< commitId >>> - string, default: none

full SHA1 of the commit (ex: C<<< e00cf62997a027bbf785614a93e2e55bb331d268 >>>)

=back

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

a page of branch refs associated with the commit

=item * C<<< 500 >>> - errors, type: application/json

The request has timed out processing the branch request

=back

=cut

sub find_branch_info_by_commit {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branches/info/{commitId}', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 get_branch_model

Get the {@link BranchModel} associated with the repository.

The authenticated user must have the B<<< REPO_READ >>> (or higher) permission for the specified
repository to call this resource.

    GET branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branchmodel

Responses:

=over 4

=item * C<<< 200 >>> - data, type: application/json

The branch model associated with the specified repository.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user does not have sufficient permission (REPO_READ)
to query the branch model of the repository.

=item * C<<< 409 >>> - errors, type: application/json

The specified repository is empty - the branch model cannot be constructed

=back

=cut

sub get_branch_model {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('branch-utils/1.0/projects/{projectKey}/repos/{repositorySlug}/branchmodel', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head1 SEE ALSO

=over 4

=item * L<WebService::BitbucketServer>

=item * L<https://developer.atlassian.com/bitbucket/server/docs/latest/>

=back

=cut

1;
