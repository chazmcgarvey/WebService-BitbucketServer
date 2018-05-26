# Generated by WebService::BitbucketServer::WADL - DO NOT EDIT!
package WebService::BitbucketServer::Git::V1;
# ABSTRACT: Bindings for a Bitbucket Server REST API

=head1 SYNOPSIS

    my $stash = WebService::BitbucketServer->new(
        base_url    => 'https://stash.example.com/',
        username    => 'bob',
        password    => 'secret',
    );
    my $api = $stash->git;

=head1 DESCRIPTION

This is a Bitbucket Server REST API for L<Git::V1|https://developer.atlassian.com/static/rest/bitbucket-server/5.10.0/bitbucket-git-rest.html>.

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

    $api = WebService::BitbucketServer::Git::V1->new(context => $webservice_bitbucketserver_obj);

Create a new API.

Normally you would use C<<< $webservice_bitbucketserver_obj->git >>> instead.

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

=head2 can_rebase

Checks preconditions to determine whether the pull request can be rebased.

Some of the preconditions are:

=over 4

=item *

The pull request is between Git repositories

=item *

The pull request is currently open

=item *

The pull request's "from" ref is a I<<< branch >>>

=over 4

=item *

In other words, the qualified ID for the "from" ref must start with C<<< refs/heads/ >>>

=item *

Tags, and other non-standard refs, cannot be rebased

=back

=item *

The current user has an e-mail address

=over 4

=item *

Pull requests cannot be rebased anonymously

=item *

C<<< git rebase >>> records the current user as the committer for the rebased commits, which
requires a name and e-mail address

=back

=item *

The current user has I<<< write >>> access to the "from" ref's repository

=over 4

=item *

Note that in order to I<<< view >>> a pull request a user is only required to have I<<< read >>>
access to the toRef's repository, so just because a user can I<<< see >>>
a pull request does not mean they can request a rebase

=back

=back

This list is not exhaustive, and the exact set of preconditions applied can be extended by third-party add-ons.

The authenticated user must have B<<< REPO_READ >>> permission for the repository that this pull request
targets to call this resource.

    GET git/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/rebase

Parameters:

=over 4

=item * C<<< pullRequestId >>> - long, default: none

the ID of the pull request within the repository

=back

Responses:

=over 4

=item * C<<< 200 >>> - pullRequest, type: application/json

The rebaseability status of the pull request.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to view the
specified pull request.

=item * C<<< 404 >>> - errors, type: application/json

The specified repository or pull request does not exist.

=back

=cut

sub can_rebase {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('git/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/rebase', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'GET', url => $url, $data ? (data => $data) : ());
}

=head2 rebase

Rebases the specified pull request, rewriting the incoming commits to start from the tip commit of the pull
request's target branch. I<<< This operation alters the pull request's source branch and cannot be undone. >>>

The authenticated user must have B<<< REPO_READ >>> permission for the repository that this pull request
targets I<<< and >>> B<<< REPO_WRITE >>> permission for the pull request's source repository to call this
resource.

    POST git/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/rebase

Parameters:

=over 4

=item * C<<< pullRequestId >>> - long, default: none

the ID of the pull request within the repository

=back

Responses:

=over 4

=item * C<<< 200 >>> - pullRequest, type: application/json

The merged pull request.

=item * C<<< 401 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to view the pull
request and/or to update its source branch.

=item * C<<< 404 >>> - errors, type: application/json

The specified repository or pull request does not exist.

=item * C<<< 409 >>> - errors, type: application/json

Any of the following error cases occurred (check the error message for more details):

=over 4

=item *

The rebase encountered conflicts.

=item *

The rebase discarded all of the incoming commits and would have left
the pull request empty

=item *

A C<<< PreRepositoryHook >>> vetoed the rebase.

=item *

The specified version is out of date.

=item *

The specified pull request is not open.

=back

=back

=cut

sub rebase {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('git/1.0/projects/{projectKey}/repos/{repositorySlug}/pull-requests/{pullRequestId}/rebase', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head2 create_tag

Creates a tag in the specified repository.

The authenticated user must have an effective B<<< REPO_WRITE >>> permission to call this resource.

'LIGHTWEIGHT' and 'ANNOTATED' are the two type of tags that can be created. The 'startPoint' can either be a ref
or a 'commit'.

    POST git/1.0/projects/{projectKey}/repos/{repositorySlug}/tags

Responses:

=over 4

=item * C<<< 201 >>> - data, type: application/json

a JSON representation of the newly created tag

=item * C<<< 400 >>> - errors, type: application/json

the tag was not created because the request was invalid, e.g. the provided
ref name already existed in the repository, or was not a valid ref name in the
repository, or the start point is invalid

=item * C<<< 403 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to create a
tag. This could be due to insufficient repository permissions.

=back

=cut

sub create_tag {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('git/1.0/projects/{projectKey}/repos/{repositorySlug}/tags', $args);
    my $data = (exists $args->{data} && $args->{data}) || (%$args && $args);
    $self->context->call(method => 'POST', url => $url, $data ? (data => $data) : ());
}

=head2 delete_tag

Deletes a tag in the specified repository.

The authenticated user must have an effective B<<< REPO_WRITE >>> permission to call this resource.

    DELETE git/1.0/projects/{projectKey}/repos/{repositorySlug}/tags/{name:.*}

Parameters:

=over 4

=item * C<<< name >>> - string, default: none

the name of the tag to be deleted

=back

Responses:

=over 4

=item * C<<< 400 >>> - errors, type: application/json

the tag was not deleted because repository is either empty,
or is not a git repository

=item * C<<< 204 >>> - data, type: unknown

an empty response indicating that the tag no longer exists in the repository

=item * C<<< 403 >>> - errors, type: application/json

The currently authenticated user has insufficient permissions to delete a
tag. This could be due to insufficient repository permissions.

=item * C<<< 404 >>> - errors, type: application/json

If the tag doesn't exist in the {@link Repository}

=back

=cut

sub delete_tag {
    my $self = shift;
    my $args = {@_ == 1 ? %{$_[0]} : @_};
    my $url  = _get_url('git/1.0/projects/{projectKey}/repos/{repositorySlug}/tags/{name:.*}', $args);
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
